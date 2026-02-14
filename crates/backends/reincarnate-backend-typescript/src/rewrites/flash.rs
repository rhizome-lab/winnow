//! Flash/AVM2-specific JsExpr → JsExpr rewrite pass.
//!
//! Runs AFTER mechanical `Expr → JsExpr` lowering. Resolves Flash SystemCall
//! nodes (scope lookups, super dispatch, object construction, etc.) into
//! native JavaScript constructs. All Flash-specific knowledge is confined here;
//! `lower.rs` is purely engine-agnostic.

use std::collections::{HashMap, HashSet};

use reincarnate_core::ir::{CastKind, Constant, Type, ValueId};

use crate::emit::{ClassRegistry, RefSets};
use crate::js_ast::{JsExpr, JsFunction, JsStmt};

// ---------------------------------------------------------------------------
// Flash-specific rewrite context
// ---------------------------------------------------------------------------

/// Context needed for Flash/AVM2 scope resolution and rewrite decisions.
pub struct FlashRewriteCtx {
    /// Qualified class name → sanitized short name.
    pub class_names: HashMap<String, String>,
    /// Short names of the current class and all its ancestors.
    pub ancestors: HashSet<String>,
    /// Instance method short names visible in the class hierarchy.
    pub method_names: HashSet<String>,
    /// Instance field short names visible in the class hierarchy.
    pub instance_fields: HashSet<String>,
    /// Whether we are inside a method (have a `this`).
    pub has_self: bool,
    /// Suppress `super()` calls (class has no real superclass).
    pub suppress_super: bool,
    /// Whether we are inside a cinit (class static initializer).
    pub is_cinit: bool,
    /// Static field short names declared on the current class.
    pub static_fields: HashSet<String>,
    /// Static method short name → owning class short name (across hierarchy).
    pub static_method_owners: HashMap<String, String>,
    /// Const field short name → owning class short name (across hierarchy).
    pub static_field_owners: HashMap<String, String>,
    /// Instance Const fields promoted to static readonly — `this.FIELD` → `ClassName.FIELD`.
    pub const_instance_fields: HashSet<String>,
    /// Short name of the current class (for `this.CONST` → `ClassName.CONST` rewrites).
    pub class_short_name: Option<String>,
    /// Instance/Free method names that need `as3Bind` wrapping when used outside callee position.
    pub bindable_methods: HashSet<String>,
    /// Pre-compiled closure bodies (short name → JsFunction), for inlining as arrow functions.
    pub closure_bodies: HashMap<String, JsFunction>,
    /// All known class short names (module classes + runtime type_definitions).
    /// Used to detect class coercions: `ClassName(obj)` → `asType(obj, ClassName)`.
    pub known_classes: HashSet<String>,
}

// ---------------------------------------------------------------------------
// Scope-lookup detection and resolution
// ---------------------------------------------------------------------------

enum ScopeResolution {
    /// The lookup matched an ancestor class.
    Ancestor(String),
    /// Generic scope lookup (class ref, global, etc.).
    ScopeLookup,
}

/// Check whether a JsExpr is a Flash scope-lookup SystemCall.
fn is_scope_lookup(expr: &JsExpr) -> bool {
    scope_lookup_args(expr).is_some()
}

/// Extract the args from a scope-lookup SystemCall, or None.
fn scope_lookup_args(expr: &JsExpr) -> Option<&[JsExpr]> {
    match expr {
        JsExpr::SystemCall {
            system,
            method,
            args,
        } if system == "Flash.Scope"
            && (method == "findPropStrict" || method == "findProperty") =>
        {
            Some(args)
        }
        _ => None,
    }
}

/// Extract the class name from a scope-lookup arg string constant.
///
/// For arg like `"classes:SomeClass::someField"`, returns `"SomeClass"`.
fn class_from_scope_arg(args: &[JsExpr]) -> Option<String> {
    let arg = args.first()?;
    if let JsExpr::Literal(Constant::String(s)) = arg {
        let prefix = s.rsplit_once("::")?.0;
        let class_name = prefix.rsplit_once(':')?.1;
        Some(class_name.to_string())
    } else {
        None
    }
}

fn resolve_scope_lookup(args: &[JsExpr], ctx: &FlashRewriteCtx) -> ScopeResolution {
    if let Some(class_name) = class_from_scope_arg(args) {
        if ctx.ancestors.contains(&class_name) {
            return ScopeResolution::Ancestor(class_name);
        }
    }
    ScopeResolution::ScopeLookup
}

// ---------------------------------------------------------------------------
// Scope-lookup resolution helpers
// ---------------------------------------------------------------------------

/// Resolve `Field { object: scope_lookup, field }`.
fn resolve_field(object: &JsExpr, field: &str, ctx: &FlashRewriteCtx) -> Option<JsExpr> {
    let args = scope_lookup_args(object)?;
    let effective = field.rsplit("::").next().unwrap_or(field);

    Some(match resolve_scope_lookup(args, ctx) {
        ScopeResolution::Ancestor(ref class_name) => {
            if ctx.is_cinit
                || ctx.instance_fields.contains(effective)
                || ctx.method_names.contains(effective)
            {
                JsExpr::Field {
                    object: Box::new(JsExpr::This),
                    field: effective.to_string(),
                }
            } else {
                JsExpr::Field {
                    object: Box::new(JsExpr::Var(class_name.clone())),
                    field: effective.to_string(),
                }
            }
        }
        ScopeResolution::ScopeLookup => {
            // Check if the full field (pre-namespace-strip) is a known class name.
            if let Some(short) = ctx.class_names.get(field) {
                return Some(JsExpr::Var(short.clone()));
            }
            if ctx.is_cinit && ctx.static_fields.contains(effective) {
                JsExpr::Field {
                    object: Box::new(JsExpr::This),
                    field: effective.to_string(),
                }
            } else if let Some(ref class_name) = ctx.class_short_name {
                if ctx.const_instance_fields.contains(effective) {
                    JsExpr::Field {
                        object: Box::new(JsExpr::Var(class_name.clone())),
                        field: effective.to_string(),
                    }
                } else if let Some(owner) = ctx.static_field_owners.get(effective) {
                    JsExpr::Field {
                        object: Box::new(JsExpr::Var(owner.clone())),
                        field: effective.to_string(),
                    }
                } else if ctx.has_self
                    && (ctx.instance_fields.contains(effective)
                        || ctx.method_names.contains(effective))
                {
                    JsExpr::Field {
                        object: Box::new(JsExpr::This),
                        field: effective.to_string(),
                    }
                } else {
                    JsExpr::Var(effective.to_string())
                }
            } else if ctx.has_self
                && (ctx.instance_fields.contains(effective)
                    || ctx.method_names.contains(effective))
            {
                JsExpr::Field {
                    object: Box::new(JsExpr::This),
                    field: effective.to_string(),
                }
            } else {
                JsExpr::Var(effective.to_string())
            }
        }
    })
}

/// Resolve a Call where the callee is `Field(scope_lookup, method)`.
fn resolve_scope_call(
    method: &str,
    scope_args: &[JsExpr],
    rest_args: Vec<JsExpr>,
    ctx: &FlashRewriteCtx,
) -> JsExpr {
    let effective = method.rsplit("::").next().unwrap_or(method);

    let callee = match resolve_scope_lookup(scope_args, ctx) {
        ScopeResolution::Ancestor(ref class_name) => {
            if ctx.is_cinit || ctx.method_names.contains(effective) {
                // Instance method (or cinit scope) → this.method
                JsExpr::Field {
                    object: Box::new(JsExpr::This),
                    field: effective.to_string(),
                }
            } else {
                // Static method on ancestor class → ClassName.method
                JsExpr::Field {
                    object: Box::new(JsExpr::Var(class_name.clone())),
                    field: effective.to_string(),
                }
            }
        }
        ScopeResolution::ScopeLookup => {
            // Non-ancestor: try to extract a class name for static dispatch.
            if let Some(class_name) = class_from_scope_arg(scope_args) {
                JsExpr::Field {
                    object: Box::new(JsExpr::Var(class_name)),
                    field: effective.to_string(),
                }
            } else if (ctx.has_self && ctx.method_names.contains(effective))
                || ctx.is_cinit
            {
                JsExpr::Field {
                    object: Box::new(JsExpr::This),
                    field: effective.to_string(),
                }
            } else if let Some(owner) = ctx.static_method_owners.get(effective) {
                // Static method found in ancestor hierarchy → OwnerClass.method
                JsExpr::Field {
                    object: Box::new(JsExpr::Var(owner.clone())),
                    field: effective.to_string(),
                }
            } else {
                JsExpr::Var(effective.to_string())
            }
        }
    };

    // Class coercion: ClassName(arg) → asType(arg, ClassName)
    // AS3 allows `ClassName(obj)` as a type coercion (returns obj if instance, null otherwise).
    // In JS, calling a class constructor without `new` throws — emit asType instead.
    if rest_args.len() == 1 {
        if let JsExpr::Var(ref name) = callee {
            if ctx.known_classes.contains(name.as_str()) {
                return JsExpr::Cast {
                    expr: Box::new(rest_args.into_iter().next().unwrap()),
                    ty: Type::Struct(name.clone()),
                    kind: CastKind::AsType,
                };
            }
        }
    }

    JsExpr::Call {
        callee: Box::new(callee),
        args: rest_args,
    }
}

// ---------------------------------------------------------------------------
// Top-level rewrite entry point
// ---------------------------------------------------------------------------

/// Rewrite a lowered JS function, resolving all Flash SystemCalls and
/// scope-lookup patterns.
pub fn rewrite_flash_function(mut func: JsFunction, ctx: &FlashRewriteCtx) -> JsFunction {
    func.body = rewrite_stmts(func.body, ctx);
    if ctx.has_self && !ctx.bindable_methods.is_empty() {
        bind_method_refs_stmts(&mut func.body, &ctx.bindable_methods);
    }
    func
}

/// Move the first `super()` call as early as possible in a constructor body,
/// without hoisting it above statements that define variables it depends on.
///
/// When `class_name` is provided, also rewrites `this.field` references in
/// `super()` arguments to `ClassName.prototype.field`, since ES6 forbids
/// accessing `this` before `super()` in derived class constructors.
pub fn hoist_super_call(body: &mut Vec<JsStmt>, class_name: Option<&str>) {
    let pos = body
        .iter()
        .position(|s| matches!(s, JsStmt::Expr(JsExpr::SuperCall(_))));
    let Some(i) = pos else { return };
    // Collect all variable names referenced by the super call's arguments.
    let mut needed = HashSet::new();
    if let JsStmt::Expr(JsExpr::SuperCall(args)) = &body[i] {
        for arg in args {
            collect_expr_vars(arg, &mut needed);
        }
    }
    // Find the latest statement before `i` that writes to any needed variable.
    let target = if needed.is_empty() {
        0
    } else {
        let mut last_dep: Option<usize> = None;
        for (j, s) in body.iter().enumerate().take(i) {
            if stmt_writes_any(s, &needed) {
                last_dep = Some(j);
            }
        }
        match last_dep {
            Some(j) => j + 1,
            None => 0,
        }
    };
    if target < i {
        let stmt = body.remove(i);
        body.insert(target, stmt);
    }

    // Rewrite `this.field` → `ClassName.prototype.field` in super() args.
    // ES6 forbids `this` before `super()` in derived class constructors, but
    // AVM2 allows it. Method references (the common case) live on the prototype.
    if let Some(cn) = class_name {
        let pos = body
            .iter()
            .position(|s| matches!(s, JsStmt::Expr(JsExpr::SuperCall(_))));
        if let Some(idx) = pos {
            if let JsStmt::Expr(JsExpr::SuperCall(args)) = &mut body[idx] {
                for arg in args.iter_mut() {
                    rewrite_this_to_prototype(arg, cn);
                }
            }
        }
    }
}

/// Collect all `Var` name references in a JS expression.
fn collect_expr_vars(expr: &JsExpr, out: &mut HashSet<String>) {
    match expr {
        JsExpr::Var(name) => {
            out.insert(name.clone());
        }
        JsExpr::Literal(_) | JsExpr::This | JsExpr::Activation | JsExpr::SuperGet(_) => {}
        JsExpr::Binary { lhs, rhs, .. }
        | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs }
        | JsExpr::LogicalAnd { lhs, rhs }
        | JsExpr::In {
            key: lhs,
            object: rhs,
        }
        | JsExpr::Delete {
            object: lhs,
            key: rhs,
        } => {
            collect_expr_vars(lhs, out);
            collect_expr_vars(rhs, out);
        }
        JsExpr::Unary { expr: e, .. }
        | JsExpr::Cast { expr: e, .. }
        | JsExpr::TypeCheck { expr: e, .. }
        | JsExpr::Not(e)
        | JsExpr::PostIncrement(e)
        | JsExpr::TypeOf(e)
        | JsExpr::GeneratorResume(e) => collect_expr_vars(e, out),
        JsExpr::Field { object, .. } => collect_expr_vars(object, out),
        JsExpr::Index { collection, index } => {
            collect_expr_vars(collection, out);
            collect_expr_vars(index, out);
        }
        JsExpr::Call { callee, args } | JsExpr::New { callee, args } => {
            collect_expr_vars(callee, out);
            for a in args {
                collect_expr_vars(a, out);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            collect_expr_vars(cond, out);
            collect_expr_vars(then_val, out);
            collect_expr_vars(else_val, out);
        }
        JsExpr::ArrayInit(elems) | JsExpr::TupleInit(elems) | JsExpr::SuperCall(elems) => {
            for e in elems {
                collect_expr_vars(e, out);
            }
        }
        JsExpr::ObjectInit(pairs) => {
            for (_, e) in pairs {
                collect_expr_vars(e, out);
            }
        }
        JsExpr::SuperMethodCall { args, .. }
        | JsExpr::GeneratorCreate { args, .. }
        | JsExpr::SystemCall { args, .. } => {
            for a in args {
                collect_expr_vars(a, out);
            }
        }
        JsExpr::SuperSet { value, .. } => collect_expr_vars(value, out),
        JsExpr::Yield(opt) => {
            if let Some(e) = opt {
                collect_expr_vars(e, out);
            }
        }
        JsExpr::ArrowFunction { body, .. } => {
            collect_stmts_vars(body, out);
        }
    }
}

/// Collect variable references from a list of statements.
fn collect_stmts_vars(stmts: &[JsStmt], out: &mut HashSet<String>) {
    for stmt in stmts {
        match stmt {
            JsStmt::VarDecl { init: Some(e), .. } | JsStmt::Expr(e) | JsStmt::Return(Some(e)) | JsStmt::Throw(e) => {
                collect_expr_vars(e, out);
            }
            JsStmt::Assign { target, value } | JsStmt::CompoundAssign { target, value, .. } => {
                collect_expr_vars(target, out);
                collect_expr_vars(value, out);
            }
            JsStmt::If { cond, then_body, else_body } => {
                collect_expr_vars(cond, out);
                collect_stmts_vars(then_body, out);
                collect_stmts_vars(else_body, out);
            }
            JsStmt::While { cond, body } => {
                collect_expr_vars(cond, out);
                collect_stmts_vars(body, out);
            }
            JsStmt::For { init, cond, update, body } => {
                collect_stmts_vars(init, out);
                collect_expr_vars(cond, out);
                collect_stmts_vars(update, out);
                collect_stmts_vars(body, out);
            }
            JsStmt::Loop { body } => collect_stmts_vars(body, out),
            JsStmt::ForOf { iterable, body, .. } => {
                collect_expr_vars(iterable, out);
                collect_stmts_vars(body, out);
            }
            JsStmt::Dispatch { blocks, .. } => {
                for (_, stmts) in blocks {
                    collect_stmts_vars(stmts, out);
                }
            }
            JsStmt::Switch { value, cases, default_body } => {
                collect_expr_vars(value, out);
                for (_, stmts) in cases {
                    collect_stmts_vars(stmts, out);
                }
                collect_stmts_vars(default_body, out);
            }
            JsStmt::VarDecl { init: None, .. } | JsStmt::Return(None) | JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => {}
        }
    }
}

/// Replace `this.field` with `ClassName.prototype.field` inside a `super()` argument.
/// AVM2 allows `this` before `super()`, but ES6 does not; method references live on
/// the prototype and are accessible without `this`.
fn rewrite_this_to_prototype(expr: &mut JsExpr, class_name: &str) {
    // Replace as3Bind(this, X) with a lazy arrow function:
    //   (...args: any[]): any => { return X.apply(this, args); }
    // Arrow functions capture `this` lexically but only evaluate it when called,
    // which is after super() completes — so this is safe in super() arguments.
    {
        let is_as3_bind = matches!(
            expr,
            JsExpr::Call { callee, args }
            if matches!(callee.as_ref(), JsExpr::Var(n) if n == "as3Bind")
                && args.len() == 2
                && matches!(&args[0], JsExpr::This)
        );
        if is_as3_bind {
            let dummy = JsExpr::Literal(Constant::Null);
            let old = std::mem::replace(expr, dummy);
            if let JsExpr::Call { mut args, .. } = old {
                let method_ref = args.swap_remove(1);
                // Extract method name for the type assertion before rewriting.
                let cast_as = if let JsExpr::Field { field, .. } = &method_ref {
                    Some(format!("{class_name}['{field}']"))
                } else {
                    None
                };
                let mut method_ref = method_ref;
                rewrite_this_to_prototype(&mut method_ref, class_name);
                *expr = JsExpr::ArrowFunction {
                    params: vec![("args".to_string(), Type::Array(Box::new(Type::Dynamic)))],
                    return_ty: Type::Unknown,
                    body: vec![JsStmt::Return(Some(JsExpr::Call {
                        callee: Box::new(JsExpr::Field {
                            object: Box::new(method_ref),
                            field: "apply".to_string(),
                        }),
                        args: vec![JsExpr::This, JsExpr::Var("args".to_string())],
                    }))],
                    has_rest_param: true,
                    cast_as,
                };
            }
            return;
        }
    }

    match expr {
        JsExpr::Field { object, .. } if matches!(object.as_ref(), JsExpr::This) => {
            *object = Box::new(JsExpr::Field {
                object: Box::new(JsExpr::Var(class_name.to_string())),
                field: "prototype".to_string(),
            });
        }
        // Recurse into subexpressions.
        JsExpr::Field { object, .. } => rewrite_this_to_prototype(object, class_name),
        JsExpr::Binary { lhs, rhs, .. }
        | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs }
        | JsExpr::LogicalAnd { lhs, rhs }
        | JsExpr::In {
            key: lhs,
            object: rhs,
        }
        | JsExpr::Delete {
            object: lhs,
            key: rhs,
        } => {
            rewrite_this_to_prototype(lhs, class_name);
            rewrite_this_to_prototype(rhs, class_name);
        }
        JsExpr::Unary { expr: e, .. }
        | JsExpr::Cast { expr: e, .. }
        | JsExpr::TypeCheck { expr: e, .. }
        | JsExpr::Not(e)
        | JsExpr::PostIncrement(e)
        | JsExpr::TypeOf(e)
        | JsExpr::GeneratorResume(e) => rewrite_this_to_prototype(e, class_name),
        JsExpr::Index { collection, index } => {
            rewrite_this_to_prototype(collection, class_name);
            rewrite_this_to_prototype(index, class_name);
        }
        JsExpr::Call { callee, args } | JsExpr::New { callee, args } => {
            rewrite_this_to_prototype(callee, class_name);
            for a in args {
                rewrite_this_to_prototype(a, class_name);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            rewrite_this_to_prototype(cond, class_name);
            rewrite_this_to_prototype(then_val, class_name);
            rewrite_this_to_prototype(else_val, class_name);
        }
        JsExpr::ArrayInit(elems) | JsExpr::TupleInit(elems) | JsExpr::SuperCall(elems) => {
            for e in elems {
                rewrite_this_to_prototype(e, class_name);
            }
        }
        JsExpr::ObjectInit(pairs) => {
            for (_, e) in pairs {
                rewrite_this_to_prototype(e, class_name);
            }
        }
        JsExpr::SuperMethodCall { args, .. }
        | JsExpr::GeneratorCreate { args, .. }
        | JsExpr::SystemCall { args, .. } => {
            for a in args {
                rewrite_this_to_prototype(a, class_name);
            }
        }
        JsExpr::SuperSet { value, .. } => rewrite_this_to_prototype(value, class_name),
        JsExpr::Yield(opt) => {
            if let Some(e) = opt {
                rewrite_this_to_prototype(e, class_name);
            }
        }
        // Arrow functions in super() args are extremely rare; skip recursion.
        JsExpr::ArrowFunction { .. } => {}
        // Leaves: no recursion needed.
        JsExpr::Literal(_)
        | JsExpr::Var(_)
        | JsExpr::This
        | JsExpr::Activation
        | JsExpr::SuperGet(_) => {}
    }
}


/// Check whether a statement declares or assigns any variable in `vars`.
/// Recurses into nested bodies (if/else, loops) to find assignments.
fn stmt_writes_any(stmt: &JsStmt, vars: &HashSet<String>) -> bool {
    match stmt {
        JsStmt::VarDecl { name, .. } => vars.contains(name.as_str()),
        JsStmt::Assign {
            target: JsExpr::Var(name),
            ..
        }
        | JsStmt::CompoundAssign {
            target: JsExpr::Var(name),
            ..
        } => vars.contains(name.as_str()),
        JsStmt::If {
            then_body,
            else_body,
            ..
        } => {
            then_body.iter().any(|s| stmt_writes_any(s, vars))
                || else_body.iter().any(|s| stmt_writes_any(s, vars))
        }
        JsStmt::While { body, .. }
        | JsStmt::Loop { body }
        | JsStmt::ForOf { body, .. } => body.iter().any(|s| stmt_writes_any(s, vars)),
        JsStmt::For {
            init,
            body,
            update,
            ..
        } => {
            init.iter().any(|s| stmt_writes_any(s, vars))
                || body.iter().any(|s| stmt_writes_any(s, vars))
                || update.iter().any(|s| stmt_writes_any(s, vars))
        }
        JsStmt::Dispatch { blocks, .. } => blocks
            .iter()
            .any(|(_, stmts)| stmts.iter().any(|s| stmt_writes_any(s, vars))),
        JsStmt::Switch { cases, default_body, .. } => {
            cases.iter().any(|(_, stmts)| stmts.iter().any(|s| stmt_writes_any(s, vars)))
                || default_body.iter().any(|s| stmt_writes_any(s, vars))
        }
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Statement rewriting
// ---------------------------------------------------------------------------

fn rewrite_stmts(stmts: Vec<JsStmt>, ctx: &FlashRewriteCtx) -> Vec<JsStmt> {
    stmts
        .into_iter()
        .filter_map(|s| rewrite_stmt(s, ctx))
        .collect()
}

/// Rewrite a single JS statement. Returns `None` to skip (e.g. suppressed super).
fn rewrite_stmt(stmt: JsStmt, ctx: &FlashRewriteCtx) -> Option<JsStmt> {
    // Check statement-level SystemCall patterns BEFORE recursing.
    if let JsStmt::Expr(JsExpr::SystemCall {
        ref system,
        ref method,
        ref args,
        ..
    }) = stmt
    {
        // constructSuper → super() or skip
        if system == "Flash.Class" && method == "constructSuper" {
            if ctx.suppress_super {
                return None;
            }
            let rewritten_args = rewrite_exprs(args.clone(), ctx);
            // Skip first arg (this), rest are constructor args
            return Some(JsStmt::Expr(JsExpr::SuperCall(
                rewritten_args.into_iter().skip(1).collect(),
            )));
        }

        // throw(x) → throw x;
        if system == "Flash.Exception" && method == "throw" && args.len() == 1 {
            let arg = rewrite_expr(args[0].clone(), ctx);
            return Some(JsStmt::Throw(arg));
        }

        // setSuper(this, "prop", value) → super.prop = value;
        if system == "Flash.Class" && method == "setSuper" && args.len() == 3 {
            if let JsExpr::Literal(Constant::String(ref name)) = args[1] {
                let short = name.rsplit("::").next().unwrap_or(name);
                let value = rewrite_expr(args[2].clone(), ctx);
                return Some(JsStmt::Assign {
                    target: JsExpr::SuperGet(short.to_string()),
                    value,
                });
            }
        }

        // findPropStrict/findProperty as standalone statement → skip
        if system == "Flash.Scope"
            && (method == "findPropStrict" || method == "findProperty")
        {
            return None;
        }
    }

    Some(match stmt {
        JsStmt::VarDecl {
            name,
            ty,
            init,
            mutable,
        } => JsStmt::VarDecl {
            name,
            ty,
            init: init.map(|e| rewrite_expr(e, ctx)),
            mutable,
        },

        JsStmt::Assign { target, value } => JsStmt::Assign {
            target: rewrite_expr(target, ctx),
            value: rewrite_expr(value, ctx),
        },

        JsStmt::CompoundAssign { target, op, value } => JsStmt::CompoundAssign {
            target: rewrite_expr(target, ctx),
            op,
            value: rewrite_expr(value, ctx),
        },

        JsStmt::Expr(expr) => {
            let rewritten = rewrite_expr(expr, ctx);
            // Skip empty var references (standalone scope lookups that resolved to nothing).
            if let JsExpr::Var(ref name) = rewritten {
                if name.is_empty() {
                    return None;
                }
            }
            JsStmt::Expr(rewritten)
        }

        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => JsStmt::If {
            cond: rewrite_expr(cond, ctx),
            then_body: rewrite_stmts(then_body, ctx),
            else_body: rewrite_stmts(else_body, ctx),
        },

        JsStmt::While { cond, body } => JsStmt::While {
            cond: rewrite_expr(cond, ctx),
            body: rewrite_stmts(body, ctx),
        },

        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => JsStmt::For {
            init: rewrite_stmts(init, ctx),
            cond: rewrite_expr(cond, ctx),
            update: rewrite_stmts(update, ctx),
            body: rewrite_stmts(body, ctx),
        },

        JsStmt::Loop { body } => JsStmt::Loop {
            body: rewrite_stmts(body, ctx),
        },

        JsStmt::ForOf {
            binding,
            declare,
            iterable,
            body,
        } => JsStmt::ForOf {
            binding,
            declare,
            iterable: rewrite_expr(iterable, ctx),
            body: rewrite_stmts(body, ctx),
        },

        JsStmt::Return(expr) => JsStmt::Return(expr.map(|e| rewrite_expr(e, ctx))),
        JsStmt::Throw(expr) => JsStmt::Throw(rewrite_expr(expr, ctx)),
        JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => stmt,

        JsStmt::Dispatch { blocks, entry } => JsStmt::Dispatch {
            blocks: blocks
                .into_iter()
                .map(|(idx, stmts)| (idx, rewrite_stmts(stmts, ctx)))
                .collect(),
            entry,
        },

        JsStmt::Switch { value, cases, default_body } => JsStmt::Switch {
            value: rewrite_expr(value, ctx),
            cases: cases
                .into_iter()
                .map(|(c, stmts)| (c, rewrite_stmts(stmts, ctx)))
                .collect(),
            default_body: rewrite_stmts(default_body, ctx),
        },
    })
}

// ---------------------------------------------------------------------------
// Expression rewriting
// ---------------------------------------------------------------------------

fn rewrite_exprs(exprs: Vec<JsExpr>, ctx: &FlashRewriteCtx) -> Vec<JsExpr> {
    exprs.into_iter().map(|e| rewrite_expr(e, ctx)).collect()
}

/// Rewrite a single JsExpr, resolving Flash SystemCalls and scope lookups.
///
/// Matches compound patterns (scope-lookup embedded in Field/Call/Binary)
/// TOP-DOWN before recursing into children.
fn rewrite_expr(expr: JsExpr, ctx: &FlashRewriteCtx) -> JsExpr {
    // --- Top-down compound pattern matching ---

    // Field { object: scope_lookup, field } → resolved field access
    if let JsExpr::Field {
        ref object,
        ref field,
    } = expr
    {
        if is_scope_lookup(object) {
            if let Some(resolved) = resolve_field(object, field, ctx) {
                return resolved;
            }
        }
    }

    // Call { callee: Field(scope_lookup, method), args } → resolved scope call
    if let JsExpr::Call {
        ref callee,
        ref args,
    } = expr
    {
        if let JsExpr::Field {
            ref object,
            ref field,
        } = **callee
        {
            if let Some(scope_args) = scope_lookup_args(object) {
                let rewritten_args = rewrite_exprs(args.clone(), ctx);
                return resolve_scope_call(field, scope_args, rewritten_args, ctx);
            }
        }
    }

    // Binary { lhs: scope_lookup, rhs } → strip scope lookup, return other side
    if let JsExpr::Binary {
        ref lhs, ref rhs, ..
    } = expr
    {
        if is_scope_lookup(lhs) {
            return rewrite_expr(rhs.as_ref().clone(), ctx);
        }
        if is_scope_lookup(rhs) {
            return rewrite_expr(lhs.as_ref().clone(), ctx);
        }
    }

    // --- SystemCall rewrites ---
    if let JsExpr::SystemCall {
        ref system,
        ref method,
        ref args,
    } = expr
    {
        if let Some(result) = rewrite_system_call(system, method, args, ctx) {
            return result;
        }
    }

    // --- Recurse into children ---
    match expr {
        JsExpr::Literal(_) | JsExpr::Var(_) | JsExpr::This | JsExpr::Activation => expr,

        JsExpr::Binary { op, lhs, rhs } => JsExpr::Binary {
            op,
            lhs: Box::new(rewrite_expr(*lhs, ctx)),
            rhs: Box::new(rewrite_expr(*rhs, ctx)),
        },

        JsExpr::Unary { op, expr: inner } => JsExpr::Unary {
            op,
            expr: Box::new(rewrite_expr(*inner, ctx)),
        },

        JsExpr::Cmp { kind, lhs, rhs } => JsExpr::Cmp {
            kind,
            lhs: Box::new(rewrite_expr(*lhs, ctx)),
            rhs: Box::new(rewrite_expr(*rhs, ctx)),
        },

        JsExpr::Field { object, field } => {
            let object = Box::new(rewrite_expr(*object, ctx));
            // Rewrite this.CONST → ClassName.CONST for promoted instance Const fields.
            if matches!(*object, JsExpr::This) {
                if let Some(ref class_name) = ctx.class_short_name {
                    if ctx.const_instance_fields.contains(&field) {
                        return JsExpr::Field {
                            object: Box::new(JsExpr::Var(class_name.clone())),
                            field,
                        };
                    }
                }
            }
            JsExpr::Field { object, field }
        }

        JsExpr::Index { collection, index } => JsExpr::Index {
            collection: Box::new(rewrite_expr(*collection, ctx)),
            index: Box::new(rewrite_expr(*index, ctx)),
        },

        JsExpr::Call { callee, args } => JsExpr::Call {
            callee: Box::new(rewrite_expr(*callee, ctx)),
            args: rewrite_exprs(args, ctx),
        },

        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => JsExpr::Ternary {
            cond: Box::new(rewrite_expr(*cond, ctx)),
            then_val: Box::new(rewrite_expr(*then_val, ctx)),
            else_val: Box::new(rewrite_expr(*else_val, ctx)),
        },

        JsExpr::LogicalOr { lhs, rhs } => JsExpr::LogicalOr {
            lhs: Box::new(rewrite_expr(*lhs, ctx)),
            rhs: Box::new(rewrite_expr(*rhs, ctx)),
        },

        JsExpr::LogicalAnd { lhs, rhs } => JsExpr::LogicalAnd {
            lhs: Box::new(rewrite_expr(*lhs, ctx)),
            rhs: Box::new(rewrite_expr(*rhs, ctx)),
        },

        JsExpr::Cast { expr: inner, ty, kind } => JsExpr::Cast {
            expr: Box::new(rewrite_expr(*inner, ctx)),
            ty,
            kind,
        },

        JsExpr::TypeCheck { expr: inner, ty } => JsExpr::TypeCheck {
            expr: Box::new(rewrite_expr(*inner, ctx)),
            ty,
        },

        JsExpr::ArrayInit(elems) => JsExpr::ArrayInit(rewrite_exprs(elems, ctx)),
        JsExpr::ObjectInit(pairs) => JsExpr::ObjectInit(
            pairs
                .into_iter()
                .map(|(k, v)| (k, rewrite_expr(v, ctx)))
                .collect(),
        ),
        JsExpr::TupleInit(elems) => JsExpr::TupleInit(rewrite_exprs(elems, ctx)),

        JsExpr::Not(inner) => JsExpr::Not(Box::new(rewrite_expr(*inner, ctx))),
        JsExpr::PostIncrement(inner) => {
            JsExpr::PostIncrement(Box::new(rewrite_expr(*inner, ctx)))
        }

        JsExpr::GeneratorCreate { func, args } => JsExpr::GeneratorCreate {
            func,
            args: rewrite_exprs(args, ctx),
        },
        JsExpr::GeneratorResume(inner) => {
            JsExpr::GeneratorResume(Box::new(rewrite_expr(*inner, ctx)))
        }
        JsExpr::Yield(v) => JsExpr::Yield(v.map(|e| Box::new(rewrite_expr(*e, ctx)))),

        JsExpr::New { callee, args } => JsExpr::New {
            callee: Box::new(rewrite_expr(*callee, ctx)),
            args: rewrite_exprs(args, ctx),
        },
        JsExpr::TypeOf(inner) => JsExpr::TypeOf(Box::new(rewrite_expr(*inner, ctx))),
        JsExpr::In { key, object } => JsExpr::In {
            key: Box::new(rewrite_expr(*key, ctx)),
            object: Box::new(rewrite_expr(*object, ctx)),
        },
        JsExpr::Delete { object, key } => JsExpr::Delete {
            object: Box::new(rewrite_expr(*object, ctx)),
            key: Box::new(rewrite_expr(*key, ctx)),
        },
        JsExpr::SuperCall(args) => JsExpr::SuperCall(rewrite_exprs(args, ctx)),
        JsExpr::SuperMethodCall { method, args } => JsExpr::SuperMethodCall {
            method,
            args: rewrite_exprs(args, ctx),
        },
        JsExpr::SuperGet(_) => expr,
        JsExpr::SuperSet { prop, value } => JsExpr::SuperSet {
            prop,
            value: Box::new(rewrite_expr(*value, ctx)),
        },

        JsExpr::SystemCall {
            system,
            method,
            args,
        } => JsExpr::SystemCall {
            system,
            method,
            args: rewrite_exprs(args, ctx),
        },

        JsExpr::ArrowFunction {
            params,
            return_ty,
            body,
            has_rest_param,
            cast_as,
        } => JsExpr::ArrowFunction {
            params,
            return_ty,
            body: rewrite_stmts(body, ctx),
            has_rest_param,
            cast_as,
        },
    }
}

// ---------------------------------------------------------------------------
// SystemCall expression rewrites
// ---------------------------------------------------------------------------

/// Rewrite a Flash SystemCall node in the JsExpr tree.
///
/// Returns `Some(JsExpr)` if the call was recognized, `None` for unmapped.
fn rewrite_system_call(
    system: &str,
    method: &str,
    args: &[JsExpr],
    ctx: &FlashRewriteCtx,
) -> Option<JsExpr> {
    // constructSuper → super() or void 0
    if system == "Flash.Class" && method == "constructSuper" {
        if ctx.suppress_super {
            return Some(JsExpr::Literal(Constant::Null));
        }
        let rewritten = rewrite_exprs(args.iter().skip(1).cloned().collect(), ctx);
        return Some(JsExpr::SuperCall(rewritten));
    }

    // newFunction → inline arrow function (or this.methodRef fallback)
    if system == "Flash.Object" && method == "newFunction" && args.len() == 1 {
        if let JsExpr::Literal(Constant::String(ref name)) = args[0] {
            let short = name.rsplit("::").next().unwrap_or(name);
            if let Some(closure_func) = ctx.closure_bodies.get(short).cloned() {
                let rewritten = rewrite_flash_function(closure_func, ctx);
                // Skip first param (activation scope object).
                let params = if rewritten.params.len() > 1 {
                    rewritten.params[1..].to_vec()
                } else {
                    vec![]
                };
                return Some(JsExpr::ArrowFunction {
                    params,
                    return_ty: rewritten.return_ty,
                    body: rewritten.body,
                    has_rest_param: rewritten.has_rest_param,
                    cast_as: None,
                });
            }
            // Fallback: non-compiled closure → this.$closureN
            return Some(JsExpr::Field {
                object: Box::new(JsExpr::This),
                field: short.to_string(),
            });
        }
    }

    // applyType(base, ...typeArgs) → Array
    // Vector is the only parameterized type in AS3; generics are erased in TS.
    if system == "Flash.Object" && method == "applyType" {
        return Some(JsExpr::Var("Array".to_string()));
    }

    // construct → new Ctor(args)  (but `new Object()` → `{}`)
    if system == "Flash.Object" && method == "construct" && !args.is_empty() {
        let callee = rewrite_expr(args[0].clone(), ctx);
        let rest = rewrite_exprs(args[1..].to_vec(), ctx);
        // `new Object()` with no constructor args → empty object literal
        if rest.is_empty() && matches!(&callee, JsExpr::Var(name) if name == "Object") {
            return Some(JsExpr::ObjectInit(Vec::new()));
        }
        return Some(JsExpr::New {
            callee: Box::new(callee),
            args: rest,
        });
    }

    // findPropStrict/findProperty → scope resolution
    if system == "Flash.Scope" && (method == "findPropStrict" || method == "findProperty") {
        return Some(match resolve_scope_lookup(args, ctx) {
            ScopeResolution::Ancestor(ref class_name) => {
                if ctx.is_cinit {
                    JsExpr::This
                } else {
                    JsExpr::Var(class_name.clone())
                }
            }
            ScopeResolution::ScopeLookup => {
                // Standalone scope lookup — emit empty var (will be filtered at stmt level).
                JsExpr::Var(String::new())
            }
        });
    }

    // newActivation → ({})
    if system == "Flash.Scope" && method == "newActivation" && args.is_empty() {
        return Some(JsExpr::Activation);
    }

    // typeOf → typeof expr
    if system == "Flash.Object" && method == "typeOf" && args.len() == 1 {
        return Some(JsExpr::TypeOf(Box::new(rewrite_expr(args[0].clone(), ctx))));
    }

    // hasProperty(obj, k) → k in obj
    if system == "Flash.Object" && method == "hasProperty" && args.len() == 2 {
        return Some(JsExpr::In {
            key: Box::new(rewrite_expr(args[1].clone(), ctx)),
            object: Box::new(rewrite_expr(args[0].clone(), ctx)),
        });
    }

    // deleteProperty(obj, k) → delete obj[k]
    if system == "Flash.Object" && method == "deleteProperty" && args.len() == 2 {
        return Some(JsExpr::Delete {
            object: Box::new(rewrite_expr(args[0].clone(), ctx)),
            key: Box::new(rewrite_expr(args[1].clone(), ctx)),
        });
    }

    // newObject(k1, v1, k2, v2, ...) → { k1: v1, k2: v2, ... }
    if system == "Flash.Object" && method == "newObject" {
        if args.is_empty() {
            return Some(JsExpr::ObjectInit(Vec::new()));
        }
        if args.len().is_multiple_of(2) {
            let mut keys: Vec<String> = Vec::new();
            let mut values: HashMap<String, JsExpr> = HashMap::new();
            for pair in args.chunks_exact(2) {
                let key = extract_object_key(&pair[0]);
                let val = rewrite_expr(pair[1].clone(), ctx);
                if !values.contains_key(&key) {
                    keys.push(key.clone());
                }
                values.insert(key, val);
            }
            let pairs: Vec<_> = keys
                .into_iter()
                .map(|k| {
                    let v = values.remove(&k).unwrap();
                    (k, v)
                })
                .collect();
            return Some(JsExpr::ObjectInit(pairs));
        }
    }

    // callSuper(this, "method", ...args) → super.method(args)
    if system == "Flash.Class" && method == "callSuper" && args.len() >= 2 {
        if let JsExpr::Literal(Constant::String(ref name)) = args[1] {
            let short = name.rsplit("::").next().unwrap_or(name);
            let rest = rewrite_exprs(args[2..].to_vec(), ctx);
            return Some(JsExpr::SuperMethodCall {
                method: short.to_string(),
                args: rest,
            });
        }
    }

    // getSuper(this, "prop") → super.prop
    if system == "Flash.Class" && method == "getSuper" && args.len() == 2 {
        if let JsExpr::Literal(Constant::String(ref name)) = args[1] {
            let short = name.rsplit("::").next().unwrap_or(name);
            return Some(JsExpr::SuperGet(short.to_string()));
        }
    }

    // setSuper(this, "prop", value) → (super.prop = value)
    if system == "Flash.Class" && method == "setSuper" && args.len() == 3 {
        if let JsExpr::Literal(Constant::String(ref name)) = args[1] {
            let short = name.rsplit("::").next().unwrap_or(name);
            return Some(JsExpr::SuperSet {
                prop: short.to_string(),
                value: Box::new(rewrite_expr(args[2].clone(), ctx)),
            });
        }
    }

    None
}

/// Extract an object-literal key string from a JsExpr key.
fn extract_object_key(expr: &JsExpr) -> String {
    match expr {
        JsExpr::Literal(Constant::String(s)) => s.clone(),
        _ => format!("{:?}", expr),
    }
}

// ---------------------------------------------------------------------------
// AS3 method closure auto-binding: as3Bind(this, this.method)
// ---------------------------------------------------------------------------

/// Post-rewrite pass: wrap `this.method` references (not in callee position)
/// with `as3Bind(this, this.method)` for identity-stable method closures.
fn bind_method_refs_stmts(stmts: &mut [JsStmt], bindable: &HashSet<String>) {
    for stmt in stmts.iter_mut() {
        bind_method_refs_stmt(stmt, bindable);
    }
}

fn bind_method_refs_stmt(stmt: &mut JsStmt, bindable: &HashSet<String>) {
    match stmt {
        JsStmt::VarDecl { init, .. } => {
            if let Some(e) = init {
                bind_method_refs_expr(e, bindable, false);
            }
        }
        JsStmt::Assign { target, value } => {
            bind_method_refs_expr(target, bindable, false);
            bind_method_refs_expr(value, bindable, false);
        }
        JsStmt::CompoundAssign { target, value, .. } => {
            bind_method_refs_expr(target, bindable, false);
            bind_method_refs_expr(value, bindable, false);
        }
        JsStmt::Expr(e) => bind_method_refs_expr(e, bindable, false),
        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            bind_method_refs_expr(cond, bindable, false);
            bind_method_refs_stmts(then_body, bindable);
            bind_method_refs_stmts(else_body, bindable);
        }
        JsStmt::While { cond, body } => {
            bind_method_refs_expr(cond, bindable, false);
            bind_method_refs_stmts(body, bindable);
        }
        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => {
            bind_method_refs_stmts(init, bindable);
            bind_method_refs_expr(cond, bindable, false);
            bind_method_refs_stmts(update, bindable);
            bind_method_refs_stmts(body, bindable);
        }
        JsStmt::Loop { body } => {
            bind_method_refs_stmts(body, bindable);
        }
        JsStmt::ForOf {
            iterable, body, ..
        } => {
            bind_method_refs_expr(iterable, bindable, false);
            bind_method_refs_stmts(body, bindable);
        }
        JsStmt::Return(Some(e)) | JsStmt::Throw(e) => {
            bind_method_refs_expr(e, bindable, false);
        }
        JsStmt::Dispatch { blocks, .. } => {
            for (_, stmts) in blocks.iter_mut() {
                bind_method_refs_stmts(stmts, bindable);
            }
        }
        JsStmt::Switch { value, cases, default_body } => {
            bind_method_refs_expr(value, bindable, false);
            for (_, stmts) in cases.iter_mut() {
                bind_method_refs_stmts(stmts, bindable);
            }
            bind_method_refs_stmts(default_body, bindable);
        }
        JsStmt::Return(None) | JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => {
        }
    }
}

/// Recursively bind method refs in an expression.
/// `in_callee` is true when this expression is the direct callee of a Call or New.
fn bind_method_refs_expr(expr: &mut JsExpr, bindable: &HashSet<String>, in_callee: bool) {
    // First, recurse into children with correct in_callee propagation.
    match expr {
        JsExpr::Call { callee, args } => {
            bind_method_refs_expr(callee, bindable, true);
            for a in args.iter_mut() {
                bind_method_refs_expr(a, bindable, false);
            }
        }
        JsExpr::New { callee, args } => {
            bind_method_refs_expr(callee, bindable, true);
            for a in args.iter_mut() {
                bind_method_refs_expr(a, bindable, false);
            }
        }
        JsExpr::Binary { lhs, rhs, .. }
        | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs }
        | JsExpr::LogicalAnd { lhs, rhs }
        | JsExpr::In {
            key: lhs,
            object: rhs,
        }
        | JsExpr::Delete {
            object: lhs,
            key: rhs,
        } => {
            bind_method_refs_expr(lhs, bindable, false);
            bind_method_refs_expr(rhs, bindable, false);
        }
        JsExpr::Unary { expr: e, .. }
        | JsExpr::Cast { expr: e, .. }
        | JsExpr::TypeCheck { expr: e, .. }
        | JsExpr::Not(e)
        | JsExpr::PostIncrement(e)
        | JsExpr::TypeOf(e)
        | JsExpr::GeneratorResume(e) => {
            bind_method_refs_expr(e, bindable, false);
        }
        JsExpr::Field { object, .. } => {
            bind_method_refs_expr(object, bindable, false);
        }
        JsExpr::Index { collection, index } => {
            bind_method_refs_expr(collection, bindable, false);
            bind_method_refs_expr(index, bindable, false);
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            bind_method_refs_expr(cond, bindable, false);
            bind_method_refs_expr(then_val, bindable, false);
            bind_method_refs_expr(else_val, bindable, false);
        }
        JsExpr::ArrayInit(elems) | JsExpr::TupleInit(elems) | JsExpr::SuperCall(elems) => {
            for e in elems.iter_mut() {
                bind_method_refs_expr(e, bindable, false);
            }
        }
        JsExpr::ObjectInit(pairs) => {
            for (_, e) in pairs.iter_mut() {
                bind_method_refs_expr(e, bindable, false);
            }
        }
        JsExpr::SuperMethodCall { args, .. }
        | JsExpr::GeneratorCreate { args, .. }
        | JsExpr::SystemCall { args, .. } => {
            for a in args.iter_mut() {
                bind_method_refs_expr(a, bindable, false);
            }
        }
        JsExpr::SuperSet { value, .. } => {
            bind_method_refs_expr(value, bindable, false);
        }
        JsExpr::Yield(opt) => {
            if let Some(e) = opt {
                bind_method_refs_expr(e, bindable, false);
            }
        }
        JsExpr::ArrowFunction { body, .. } => {
            bind_method_refs_stmts(body, bindable);
        }
        JsExpr::Literal(_)
        | JsExpr::Var(_)
        | JsExpr::This
        | JsExpr::Activation
        | JsExpr::SuperGet(_) => {}
    }

    // After recursing, check if this expr should be wrapped with as3Bind.
    if !in_callee {
        if let JsExpr::Field { object, field } = expr {
            if matches!(object.as_ref(), JsExpr::This) && bindable.contains(field.as_str()) {
                // Replace `this.method` with `as3Bind(this, this.method)`.
                let original = std::mem::replace(expr, JsExpr::This); // placeholder
                *expr = JsExpr::Call {
                    callee: Box::new(JsExpr::Var("as3Bind".to_string())),
                    args: vec![JsExpr::This, original],
                };
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Dead activation object elimination
// ---------------------------------------------------------------------------

/// Check whether `name` appears in an expression (read or write).
fn expr_references_var(expr: &JsExpr, name: &str) -> bool {
    match expr {
        JsExpr::Var(n) => n == name,
        JsExpr::Literal(_) | JsExpr::This | JsExpr::Activation | JsExpr::SuperGet(_) => false,
        JsExpr::Binary { lhs, rhs, .. }
        | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs }
        | JsExpr::LogicalAnd { lhs, rhs }
        | JsExpr::In { key: lhs, object: rhs }
        | JsExpr::Delete { object: lhs, key: rhs } => {
            expr_references_var(lhs, name) || expr_references_var(rhs, name)
        }
        JsExpr::Unary { expr: e, .. }
        | JsExpr::Cast { expr: e, .. }
        | JsExpr::TypeCheck { expr: e, .. }
        | JsExpr::Not(e)
        | JsExpr::PostIncrement(e)
        | JsExpr::TypeOf(e)
        | JsExpr::GeneratorResume(e) => expr_references_var(e, name),
        JsExpr::Field { object, .. } => expr_references_var(object, name),
        JsExpr::Index { collection, index } => {
            expr_references_var(collection, name) || expr_references_var(index, name)
        }
        JsExpr::Call { callee, args } | JsExpr::New { callee, args } => {
            expr_references_var(callee, name) || args.iter().any(|a| expr_references_var(a, name))
        }
        JsExpr::Ternary { cond, then_val, else_val } => {
            expr_references_var(cond, name)
                || expr_references_var(then_val, name)
                || expr_references_var(else_val, name)
        }
        JsExpr::ArrayInit(elems) | JsExpr::TupleInit(elems) | JsExpr::SuperCall(elems) => {
            elems.iter().any(|e| expr_references_var(e, name))
        }
        JsExpr::ObjectInit(pairs) => pairs.iter().any(|(_, e)| expr_references_var(e, name)),
        JsExpr::SuperMethodCall { args, .. }
        | JsExpr::GeneratorCreate { args, .. }
        | JsExpr::SystemCall { args, .. } => args.iter().any(|a| expr_references_var(a, name)),
        JsExpr::SuperSet { value, .. } => expr_references_var(value, name),
        JsExpr::Yield(opt) => opt.as_ref().is_some_and(|e| expr_references_var(e, name)),
        JsExpr::ArrowFunction { body, .. } => stmts_reference_var(body, name),
    }
}

/// Check whether any statement in a list references `name`.
fn stmts_reference_var(stmts: &[JsStmt], name: &str) -> bool {
    stmts.iter().any(|s| stmt_references_var(s, name))
}

/// Check whether a statement references `name`.
fn stmt_references_var(stmt: &JsStmt, name: &str) -> bool {
    match stmt {
        JsStmt::VarDecl { name: n, init, .. } => {
            n == name || init.as_ref().is_some_and(|e| expr_references_var(e, name))
        }
        JsStmt::Assign { target, value } | JsStmt::CompoundAssign { target, value, .. } => {
            expr_references_var(target, name) || expr_references_var(value, name)
        }
        JsStmt::Expr(e) | JsStmt::Return(Some(e)) | JsStmt::Throw(e) => {
            expr_references_var(e, name)
        }
        JsStmt::If { cond, then_body, else_body } => {
            expr_references_var(cond, name)
                || stmts_reference_var(then_body, name)
                || stmts_reference_var(else_body, name)
        }
        JsStmt::While { cond, body } => {
            expr_references_var(cond, name) || stmts_reference_var(body, name)
        }
        JsStmt::For { init, cond, update, body } => {
            stmts_reference_var(init, name)
                || expr_references_var(cond, name)
                || stmts_reference_var(update, name)
                || stmts_reference_var(body, name)
        }
        JsStmt::Loop { body } => stmts_reference_var(body, name),
        JsStmt::ForOf { iterable, body, .. } => {
            expr_references_var(iterable, name) || stmts_reference_var(body, name)
        }
        JsStmt::Dispatch { blocks, .. } => {
            blocks.iter().any(|(_, stmts)| stmts_reference_var(stmts, name))
        }
        JsStmt::Switch { value, cases, default_body } => {
            expr_references_var(value, name)
                || cases.iter().any(|(_, stmts)| stmts_reference_var(stmts, name))
                || stmts_reference_var(default_body, name)
        }
        JsStmt::Return(None) | JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => {
            false
        }
    }
}

/// Check whether a statement is a dead field-write to `act_name` (e.g. `act.field = value`).
fn is_dead_activation_field_write(stmt: &JsStmt, act_name: &str) -> bool {
    matches!(
        stmt,
        JsStmt::Assign {
            target: JsExpr::Field { object, .. },
            ..
        } if matches!(object.as_ref(), JsExpr::Var(n) if n == act_name)
    )
}

/// Eliminate dead activation objects after closure inlining.
///
/// After closures are inlined as arrow functions, activation objects like:
///   const curry$0 = ({});
///   curry$0.func = func;
///   curry$0.args = args;
/// become dead code (the arrow function captures variables lexically).
/// This pass removes the VarDecl and all field-write assigns if the activation
/// object has no other references.
pub fn eliminate_dead_activations(body: &mut Vec<JsStmt>) {
    // Find activation object names: VarDecl with Activation init.
    let act_names: Vec<String> = body
        .iter()
        .filter_map(|s| {
            if let JsStmt::VarDecl {
                name,
                init: Some(JsExpr::Activation),
                ..
            } = s
            {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();

    for act_name in &act_names {
        // Check that ALL remaining references to act_name are either:
        // 1. The VarDecl itself
        // 2. Field-write assigns (act_name.field = value)
        let all_dead = body.iter().all(|s| {
            // Skip the VarDecl itself
            if let JsStmt::VarDecl {
                name,
                init: Some(JsExpr::Activation),
                ..
            } = s
            {
                return name == act_name;
            }
            // Field-write: act_name.field = value (only check `act_name` appears as target object)
            if is_dead_activation_field_write(s, act_name) {
                // Check that the VALUE side doesn't reference act_name.
                if let JsStmt::Assign { value, .. } = s {
                    return !expr_references_var(value, act_name);
                }
            }
            // Any other statement: must not reference act_name at all.
            !stmt_references_var(s, act_name)
        });

        if all_dead {
            body.retain(|s| {
                // Remove the VarDecl
                if let JsStmt::VarDecl {
                    name,
                    init: Some(JsExpr::Activation),
                    ..
                } = s
                {
                    return name != act_name;
                }
                // Remove field-write assigns
                !is_dead_activation_field_write(s, act_name)
            });
        }
    }

    // Recurse into nested arrow function bodies.
    for stmt in body.iter_mut() {
        eliminate_dead_activations_in_stmt(stmt);
    }
}

/// Recurse into statement bodies to eliminate dead activations in nested scopes.
fn eliminate_dead_activations_in_stmt(stmt: &mut JsStmt) {
    match stmt {
        JsStmt::VarDecl { init: Some(e), .. } | JsStmt::Expr(e) | JsStmt::Return(Some(e)) | JsStmt::Throw(e) => {
            eliminate_dead_activations_in_expr(e);
        }
        JsStmt::Assign { target, value } | JsStmt::CompoundAssign { target, value, .. } => {
            eliminate_dead_activations_in_expr(target);
            eliminate_dead_activations_in_expr(value);
        }
        JsStmt::If { cond, then_body, else_body } => {
            eliminate_dead_activations_in_expr(cond);
            eliminate_dead_activations(then_body);
            eliminate_dead_activations(else_body);
        }
        JsStmt::While { cond, body } => {
            eliminate_dead_activations_in_expr(cond);
            eliminate_dead_activations(body);
        }
        JsStmt::For { init, cond, update, body } => {
            eliminate_dead_activations(init);
            eliminate_dead_activations_in_expr(cond);
            eliminate_dead_activations(update);
            eliminate_dead_activations(body);
        }
        JsStmt::Loop { body } => eliminate_dead_activations(body),
        JsStmt::ForOf { iterable, body, .. } => {
            eliminate_dead_activations_in_expr(iterable);
            eliminate_dead_activations(body);
        }
        JsStmt::Dispatch { blocks, .. } => {
            for (_, stmts) in blocks {
                eliminate_dead_activations(stmts);
            }
        }
        JsStmt::Switch { value, cases, default_body } => {
            eliminate_dead_activations_in_expr(value);
            for (_, stmts) in cases {
                eliminate_dead_activations(stmts);
            }
            eliminate_dead_activations(default_body);
        }
        JsStmt::VarDecl { init: None, .. } | JsStmt::Return(None) | JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => {}
    }
}

/// Recurse into expressions to find and clean arrow function bodies.
fn eliminate_dead_activations_in_expr(expr: &mut JsExpr) {
    match expr {
        JsExpr::ArrowFunction { body, .. } => {
            eliminate_dead_activations(body);
        }
        JsExpr::Binary { lhs, rhs, .. }
        | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs }
        | JsExpr::LogicalAnd { lhs, rhs }
        | JsExpr::In { key: lhs, object: rhs }
        | JsExpr::Delete { object: lhs, key: rhs } => {
            eliminate_dead_activations_in_expr(lhs);
            eliminate_dead_activations_in_expr(rhs);
        }
        JsExpr::Unary { expr: e, .. }
        | JsExpr::Cast { expr: e, .. }
        | JsExpr::TypeCheck { expr: e, .. }
        | JsExpr::Not(e)
        | JsExpr::PostIncrement(e)
        | JsExpr::TypeOf(e)
        | JsExpr::GeneratorResume(e) => eliminate_dead_activations_in_expr(e),
        JsExpr::Field { object, .. } => eliminate_dead_activations_in_expr(object),
        JsExpr::Index { collection, index } => {
            eliminate_dead_activations_in_expr(collection);
            eliminate_dead_activations_in_expr(index);
        }
        JsExpr::Call { callee, args } | JsExpr::New { callee, args } => {
            eliminate_dead_activations_in_expr(callee);
            for a in args { eliminate_dead_activations_in_expr(a); }
        }
        JsExpr::Ternary { cond, then_val, else_val } => {
            eliminate_dead_activations_in_expr(cond);
            eliminate_dead_activations_in_expr(then_val);
            eliminate_dead_activations_in_expr(else_val);
        }
        JsExpr::ArrayInit(elems) | JsExpr::TupleInit(elems) | JsExpr::SuperCall(elems) => {
            for e in elems { eliminate_dead_activations_in_expr(e); }
        }
        JsExpr::ObjectInit(pairs) => {
            for (_, e) in pairs { eliminate_dead_activations_in_expr(e); }
        }
        JsExpr::SuperMethodCall { args, .. }
        | JsExpr::GeneratorCreate { args, .. }
        | JsExpr::SystemCall { args, .. } => {
            for a in args { eliminate_dead_activations_in_expr(a); }
        }
        JsExpr::SuperSet { value, .. } => eliminate_dead_activations_in_expr(value),
        JsExpr::Yield(Some(e)) => eliminate_dead_activations_in_expr(e),
        JsExpr::Literal(_) | JsExpr::Var(_) | JsExpr::This | JsExpr::Activation
        | JsExpr::SuperGet(_) | JsExpr::Yield(None) => {}
    }
}

// ---------------------------------------------------------------------------
// Import extraction for Flash scope-lookup SystemCalls
// ---------------------------------------------------------------------------

/// Collect import references from Flash.Scope findPropStrict/findProperty calls.
///
/// Scope lookups may resolve to static methods/fields on ancestor classes, class
/// coercions, or module-level globals. This produces the value/type imports that
/// the emitter needs.
#[allow(clippy::too_many_arguments)]
pub(crate) fn collect_flash_scope_refs(
    args: &[ValueId],
    const_strings: &HashMap<ValueId, &str>,
    self_name: &str,
    registry: &ClassRegistry,
    static_method_owners: &HashMap<String, String>,
    static_field_owners: &HashMap<String, String>,
    global_names: &HashSet<String>,
    refs: &mut RefSets,
) {
    if let Some(&scope_str) = args.first().and_then(|v| const_strings.get(v)) {
        // Extract the bare name from the scope arg.
        let bare = scope_str.rsplit("::").next().unwrap_or(scope_str);
        if let Some(owner) = static_method_owners.get(bare) {
            if owner != self_name {
                if let Some(entry) = registry.lookup(owner) {
                    refs.value_refs.insert(entry.short_name.clone());
                }
            }
        }
        if let Some(owner) = static_field_owners.get(bare) {
            if owner != self_name {
                if let Some(entry) = registry.lookup(owner) {
                    refs.value_refs.insert(entry.short_name.clone());
                }
            }
        }
        // Class coercion: FindPropStrict("ClassName") + CallPropLex("ClassName", 1)
        // resolves to asType(obj, ClassName) — need the class as a value import.
        if bare != self_name {
            if let Some(entry) = registry.lookup(bare) {
                refs.value_refs.insert(entry.short_name.clone());
            }
        }
        // Module-level globals (package variables).
        if global_names.contains(bare) {
            refs.globals_used.insert(bare.to_string());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a minimal `FlashRewriteCtx` with all fields empty.
    fn empty_ctx() -> FlashRewriteCtx {
        FlashRewriteCtx {
            class_names: HashMap::new(),
            ancestors: HashSet::new(),
            method_names: HashSet::new(),
            instance_fields: HashSet::new(),
            has_self: false,
            suppress_super: false,
            is_cinit: false,
            static_fields: HashSet::new(),
            static_method_owners: HashMap::new(),
            static_field_owners: HashMap::new(),
            const_instance_fields: HashSet::new(),
            class_short_name: None,
            bindable_methods: HashSet::new(),
            closure_bodies: HashMap::new(),
            known_classes: HashSet::new(),
        }
    }

    fn scope_lookup(name: &str) -> JsExpr {
        JsExpr::SystemCall {
            system: "Flash.Scope".into(),
            method: "findPropStrict".into(),
            args: vec![JsExpr::Literal(Constant::String(name.into()))],
        }
    }

    fn body_stmt(name: &str) -> JsStmt {
        JsStmt::Expr(JsExpr::Call {
            callee: Box::new(JsExpr::Var(name.into())),
            args: vec![],
        })
    }

    // --- Scope resolution ---

    #[test]
    fn scope_lookup_ancestor_resolves_to_this_for_instance_field() {
        let mut ctx = empty_ctx();
        ctx.ancestors.insert("MyClass".into());
        ctx.instance_fields.insert("x".into());
        let result = resolve_field(&scope_lookup("classes:MyClass::x"), "x", &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(matches!(&expr, JsExpr::Field { object, field }
            if matches!(object.as_ref(), JsExpr::This) && field == "x"));
    }

    #[test]
    fn scope_lookup_ancestor_static_resolves_to_class_dot_field() {
        let mut ctx = empty_ctx();
        ctx.ancestors.insert("MyClass".into());
        let result = resolve_field(&scope_lookup("classes:MyClass::MAX"), "MAX", &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(matches!(&expr, JsExpr::Field { object, field }
            if matches!(object.as_ref(), JsExpr::Var(n) if n == "MyClass") && field == "MAX"));
    }

    #[test]
    fn scope_lookup_non_ancestor_resolves_to_bare_var() {
        let ctx = empty_ctx();
        let result = resolve_field(&scope_lookup("global::trace"), "trace", &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(matches!(&expr, JsExpr::Var(n) if n == "trace"));
    }

    #[test]
    fn scope_lookup_with_self_and_method_resolves_to_this() {
        let mut ctx = empty_ctx();
        ctx.has_self = true;
        ctx.method_names.insert("update".into());
        let result = resolve_field(&scope_lookup("global::update"), "update", &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(matches!(&expr, JsExpr::Field { object, field }
            if matches!(object.as_ref(), JsExpr::This) && field == "update"));
    }

    // --- SystemCall rewrites ---

    #[test]
    fn construct_super_becomes_super_call() {
        let ctx = empty_ctx();
        let args = vec![JsExpr::This, JsExpr::Literal(Constant::Int(42))];
        let result = rewrite_system_call("Flash.Class", "constructSuper", &args, &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(matches!(&expr, JsExpr::SuperCall(args) if args.len() == 1));
    }

    #[test]
    fn construct_super_suppressed_becomes_null() {
        let mut ctx = empty_ctx();
        ctx.suppress_super = true;
        let args = vec![JsExpr::This];
        let result = rewrite_system_call("Flash.Class", "constructSuper", &args, &ctx);
        assert!(matches!(result, Some(JsExpr::Literal(Constant::Null))));
    }

    #[test]
    fn construct_becomes_new() {
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::Var("MyClass".into()),
            JsExpr::Literal(Constant::Int(1)),
        ];
        let result = rewrite_system_call("Flash.Object", "construct", &args, &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(matches!(&expr, JsExpr::New { callee, args }
            if matches!(callee.as_ref(), JsExpr::Var(n) if n == "MyClass")
            && args.len() == 1));
    }

    #[test]
    fn construct_object_no_args_becomes_empty_object() {
        let ctx = empty_ctx();
        let args = vec![JsExpr::Var("Object".into())];
        let result = rewrite_system_call("Flash.Object", "construct", &args, &ctx);
        assert!(matches!(result, Some(JsExpr::ObjectInit(pairs)) if pairs.is_empty()));
    }

    #[test]
    fn typeof_rewrite() {
        let ctx = empty_ctx();
        let args = vec![JsExpr::Var("x".into())];
        let result = rewrite_system_call("Flash.Object", "typeOf", &args, &ctx);
        assert!(matches!(result, Some(JsExpr::TypeOf(_))));
    }

    #[test]
    fn has_property_becomes_in() {
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::Var("obj".into()),
            JsExpr::Literal(Constant::String("key".into())),
        ];
        let result = rewrite_system_call("Flash.Object", "hasProperty", &args, &ctx);
        assert!(matches!(result, Some(JsExpr::In { .. })));
    }

    #[test]
    fn delete_property_becomes_delete() {
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::Var("obj".into()),
            JsExpr::Literal(Constant::String("key".into())),
        ];
        let result = rewrite_system_call("Flash.Object", "deleteProperty", &args, &ctx);
        assert!(matches!(result, Some(JsExpr::Delete { .. })));
    }

    #[test]
    fn new_object_becomes_object_init() {
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::Literal(Constant::String("a".into())),
            JsExpr::Literal(Constant::Int(1)),
            JsExpr::Literal(Constant::String("b".into())),
            JsExpr::Literal(Constant::Int(2)),
        ];
        let result = rewrite_system_call("Flash.Object", "newObject", &args, &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        if let JsExpr::ObjectInit(pairs) = expr {
            assert_eq!(pairs.len(), 2);
            assert_eq!(pairs[0].0, "a");
            assert_eq!(pairs[1].0, "b");
        } else {
            panic!("expected ObjectInit, got {:?}", expr);
        }
    }

    #[test]
    fn call_super_becomes_super_method_call() {
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::This,
            JsExpr::Literal(Constant::String("ns::doStuff".into())),
            JsExpr::Literal(Constant::Int(42)),
        ];
        let result = rewrite_system_call("Flash.Class", "callSuper", &args, &ctx);
        assert!(result.is_some());
        if let Some(JsExpr::SuperMethodCall { method, args }) = result {
            assert_eq!(method, "doStuff");
            assert_eq!(args.len(), 1);
        } else {
            panic!("expected SuperMethodCall");
        }
    }

    #[test]
    fn get_super_becomes_super_get() {
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::This,
            JsExpr::Literal(Constant::String("value".into())),
        ];
        let result = rewrite_system_call("Flash.Class", "getSuper", &args, &ctx);
        assert!(matches!(result, Some(JsExpr::SuperGet(n)) if n == "value"));
    }

    #[test]
    fn apply_type_becomes_array() {
        let ctx = empty_ctx();
        let result = rewrite_system_call("Flash.Object", "applyType", &[], &ctx);
        assert!(matches!(result, Some(JsExpr::Var(n)) if n == "Array"));
    }

    #[test]
    fn class_coercion_in_scope_call() {
        let mut ctx = empty_ctx();
        ctx.known_classes.insert("Sprite".into());
        // When scope arg has no class prefix, callee resolves to bare Var("Sprite")
        // which triggers the class coercion path.
        let rewritten = resolve_scope_call(
            "Sprite",
            &[JsExpr::Literal(Constant::String("Sprite".into()))],
            vec![JsExpr::Var("obj".into())],
            &ctx,
        );
        assert!(matches!(&rewritten, JsExpr::Cast { kind: CastKind::AsType, .. }));
    }

    // --- hoist_super_call ---

    #[test]
    fn hoist_super_call_no_deps() {
        let mut body = vec![
            body_stmt("a"),
            body_stmt("b"),
            JsStmt::Expr(JsExpr::SuperCall(vec![])),
        ];
        hoist_super_call(&mut body, None);
        assert!(matches!(&body[0], JsStmt::Expr(JsExpr::SuperCall(_))));
    }

    #[test]
    fn hoist_super_call_with_dep() {
        let mut body = vec![
            JsStmt::VarDecl {
                name: "x".into(),
                ty: Some(Type::Int(32)),
                init: Some(JsExpr::Literal(Constant::Int(1))),
                mutable: false,
            },
            body_stmt("other"),
            JsStmt::Expr(JsExpr::SuperCall(vec![JsExpr::Var("x".into())])),
        ];
        hoist_super_call(&mut body, None);
        assert!(matches!(&body[1], JsStmt::Expr(JsExpr::SuperCall(_))));
    }

    #[test]
    fn hoist_super_call_rewrites_this_to_prototype() {
        let mut body = vec![JsStmt::Expr(JsExpr::SuperCall(vec![JsExpr::Field {
            object: Box::new(JsExpr::This),
            field: "handler".into(),
        }]))];
        hoist_super_call(&mut body, Some("MyClass"));
        if let JsStmt::Expr(JsExpr::SuperCall(args)) = &body[0] {
            assert!(matches!(&args[0], JsExpr::Field { object, field }
                if field == "handler"
                && matches!(object.as_ref(), JsExpr::Field { object: inner, field: proto }
                    if proto == "prototype"
                    && matches!(inner.as_ref(), JsExpr::Var(n) if n == "MyClass"))));
        } else {
            panic!("expected SuperCall");
        }
    }

    // --- Statement-level rewrites ---

    #[test]
    fn throw_statement_rewrite() {
        let ctx = empty_ctx();
        let stmt = JsStmt::Expr(JsExpr::SystemCall {
            system: "Flash.Exception".into(),
            method: "throw".into(),
            args: vec![JsExpr::Var("err".into())],
        });
        let result = rewrite_stmt(stmt, &ctx);
        assert!(matches!(result, Some(JsStmt::Throw(_))));
    }

    #[test]
    fn standalone_scope_lookup_suppressed() {
        let ctx = empty_ctx();
        let stmt = JsStmt::Expr(scope_lookup("classes:Foo::bar"));
        let result = rewrite_stmt(stmt, &ctx);
        assert!(result.is_none());
    }

    #[test]
    fn set_super_statement_rewrite() {
        let ctx = empty_ctx();
        let stmt = JsStmt::Expr(JsExpr::SystemCall {
            system: "Flash.Class".into(),
            method: "setSuper".into(),
            args: vec![
                JsExpr::This,
                JsExpr::Literal(Constant::String("value".into())),
                JsExpr::Literal(Constant::Int(42)),
            ],
        });
        let result = rewrite_stmt(stmt, &ctx);
        assert!(result.is_some());
        assert!(matches!(&result.unwrap(), JsStmt::Assign {
            target: JsExpr::SuperGet(prop), ..
        } if prop == "value"));
    }

    // --- eliminate_dead_activations ---

    #[test]
    fn dead_activation_removed() {
        let mut body = vec![
            JsStmt::VarDecl {
                name: "act$0".into(),
                ty: Some(Type::Dynamic),
                init: Some(JsExpr::Activation),
                mutable: false,
            },
            JsStmt::Assign {
                target: JsExpr::Field {
                    object: Box::new(JsExpr::Var("act$0".into())),
                    field: "x".into(),
                },
                value: JsExpr::Literal(Constant::Int(1)),
            },
            body_stmt("other"),
        ];
        eliminate_dead_activations(&mut body);
        assert_eq!(body.len(), 1);
    }

    #[test]
    fn live_activation_kept() {
        let mut body = vec![
            JsStmt::VarDecl {
                name: "act$0".into(),
                ty: Some(Type::Dynamic),
                init: Some(JsExpr::Activation),
                mutable: false,
            },
            JsStmt::Expr(JsExpr::Call {
                callee: Box::new(JsExpr::Var("doSomething".into())),
                args: vec![JsExpr::Var("act$0".into())],
            }),
        ];
        eliminate_dead_activations(&mut body);
        assert_eq!(body.len(), 2);
    }

    // --- as3Bind ---

    #[test]
    fn method_ref_in_non_callee_position_bound() {
        let bindable: HashSet<String> = ["update".to_string()].into();
        let mut expr = JsExpr::Field {
            object: Box::new(JsExpr::This),
            field: "update".into(),
        };
        bind_method_refs_expr(&mut expr, &bindable, false);
        assert!(matches!(&expr, JsExpr::Call { callee, args }
            if matches!(callee.as_ref(), JsExpr::Var(n) if n == "as3Bind")
            && args.len() == 2));
    }

    #[test]
    fn method_ref_in_callee_position_not_bound() {
        let bindable: HashSet<String> = ["update".to_string()].into();
        let mut expr = JsExpr::Field {
            object: Box::new(JsExpr::This),
            field: "update".into(),
        };
        bind_method_refs_expr(&mut expr, &bindable, true);
        assert!(matches!(&expr, JsExpr::Field { field, .. } if field == "update"));
    }

    // --- const instance field promotion ---

    #[test]
    fn const_instance_field_resolves_to_class_static() {
        let mut ctx = empty_ctx();
        ctx.has_self = true;
        ctx.class_short_name = Some("MyClass".into());
        ctx.const_instance_fields.insert("MAX_HP".into());

        let expr = JsExpr::Field {
            object: Box::new(JsExpr::This),
            field: "MAX_HP".into(),
        };
        let result = rewrite_expr(expr, &ctx);
        assert!(matches!(&result, JsExpr::Field { object, field }
            if matches!(object.as_ref(), JsExpr::Var(n) if n == "MyClass")
            && field == "MAX_HP"));
    }

    // --- Adversarial / edge cases ---

    #[test]
    fn scope_lookup_cinit_static_field_resolves_to_this() {
        // In cinit context, static fields resolve to this.field, not ClassName.field.
        // This is subtle: cinit runs as the class constructor, so `this` IS the class.
        let mut ctx = empty_ctx();
        ctx.is_cinit = true;
        ctx.static_fields.insert("INSTANCE_COUNT".into());
        let result = resolve_field(
            &scope_lookup("global::INSTANCE_COUNT"),
            "INSTANCE_COUNT",
            &ctx,
        );
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(
            matches!(&expr, JsExpr::Field { object, field }
                if matches!(object.as_ref(), JsExpr::This) && field == "INSTANCE_COUNT"),
            "cinit should resolve static field to this.field, got {:?}",
            expr
        );
    }

    #[test]
    fn scope_lookup_static_field_owner_from_ancestor() {
        // Static field owned by a different class in the hierarchy.
        let mut ctx = empty_ctx();
        ctx.class_short_name = Some("Child".into());
        ctx.static_field_owners
            .insert("MAX".into(), "Parent".into());
        let result = resolve_field(&scope_lookup("global::MAX"), "MAX", &ctx);
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(
            matches!(&expr, JsExpr::Field { object, field }
                if matches!(object.as_ref(), JsExpr::Var(n) if n == "Parent") && field == "MAX"),
            "should resolve to Parent.MAX, got {:?}",
            expr
        );
    }

    #[test]
    fn scope_lookup_namespace_stripped_from_field() {
        // Field name like "ns::myField" should strip the namespace prefix.
        let mut ctx = empty_ctx();
        ctx.has_self = true;
        ctx.instance_fields.insert("myField".into());
        let result = resolve_field(
            &scope_lookup("global::myField"),
            "ns::myField",
            &ctx,
        );
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(
            matches!(&expr, JsExpr::Field { object, field }
                if matches!(object.as_ref(), JsExpr::This) && field == "myField"),
            "should strip namespace and resolve to this.myField, got {:?}",
            expr
        );
    }

    #[test]
    fn scope_call_non_ancestor_with_class_prefix_dispatches_statically() {
        // Call to OtherClass.method — scope arg contains class name, not in ancestors.
        let ctx = empty_ctx();
        let rewritten = resolve_scope_call(
            "doStuff",
            &[JsExpr::Literal(Constant::String(
                "classes:OtherClass::doStuff".into(),
            ))],
            vec![JsExpr::Literal(Constant::Int(1))],
            &ctx,
        );
        // Should produce OtherClass.doStuff(1), NOT bare doStuff(1).
        assert!(
            matches!(&rewritten, JsExpr::Call { callee, .. }
                if matches!(callee.as_ref(), JsExpr::Field { object, field }
                    if matches!(object.as_ref(), JsExpr::Var(n) if n == "OtherClass")
                    && field == "doStuff")),
            "should dispatch to OtherClass.doStuff, got {:?}",
            rewritten
        );
    }

    #[test]
    fn class_coercion_not_triggered_for_multi_arg_call() {
        // ClassName(a, b) with 2 args should NOT be treated as coercion.
        let mut ctx = empty_ctx();
        ctx.known_classes.insert("Sprite".into());
        let rewritten = resolve_scope_call(
            "Sprite",
            &[JsExpr::Literal(Constant::String("Sprite".into()))],
            vec![
                JsExpr::Var("a".into()),
                JsExpr::Var("b".into()),
            ],
            &ctx,
        );
        // Two args → regular call, NOT a Cast.
        assert!(
            matches!(&rewritten, JsExpr::Call { .. }),
            "multi-arg call should not be coercion, got {:?}",
            rewritten
        );
        assert!(
            !matches!(&rewritten, JsExpr::Cast { .. }),
            "must not produce Cast for multi-arg"
        );
    }

    #[test]
    fn class_coercion_not_triggered_for_non_class_name() {
        // regularFunc(obj) where regularFunc is NOT in known_classes.
        let ctx = empty_ctx();
        let rewritten = resolve_scope_call(
            "regularFunc",
            &[JsExpr::Literal(Constant::String("regularFunc".into()))],
            vec![JsExpr::Var("obj".into())],
            &ctx,
        );
        assert!(
            matches!(&rewritten, JsExpr::Call { .. }),
            "non-class single-arg call should remain a Call, got {:?}",
            rewritten
        );
    }

    #[test]
    fn new_object_duplicate_keys_last_wins() {
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::Literal(Constant::String("x".into())),
            JsExpr::Literal(Constant::Int(1)),
            JsExpr::Literal(Constant::String("x".into())),
            JsExpr::Literal(Constant::Int(2)),
        ];
        let result = rewrite_system_call("Flash.Object", "newObject", &args, &ctx);
        assert!(result.is_some());
        if let Some(JsExpr::ObjectInit(pairs)) = result {
            // Duplicate key "x" — should keep only one entry, with the last value.
            assert_eq!(pairs.len(), 1, "duplicate key should deduplicate");
            assert_eq!(pairs[0].0, "x");
            assert!(
                matches!(&pairs[0].1, JsExpr::Literal(Constant::Int(2))),
                "last value should win, got {:?}",
                pairs[0].1
            );
        } else {
            panic!("expected ObjectInit");
        }
    }

    #[test]
    fn new_object_odd_arg_count_falls_through() {
        // Odd number of args (3) — not valid key/value pairs, should NOT produce ObjectInit.
        let ctx = empty_ctx();
        let args = vec![
            JsExpr::Literal(Constant::String("a".into())),
            JsExpr::Literal(Constant::Int(1)),
            JsExpr::Literal(Constant::String("orphan".into())),
        ];
        let result = rewrite_system_call("Flash.Object", "newObject", &args, &ctx);
        // Odd count → no rewrite (falls through to None).
        assert!(result.is_none(), "odd arg count should not produce ObjectInit");
    }

    #[test]
    fn construct_super_with_only_this_produces_empty_super() {
        // constructSuper(this) with no additional args → super()
        let ctx = empty_ctx();
        let args = vec![JsExpr::This];
        let result = rewrite_system_call("Flash.Class", "constructSuper", &args, &ctx);
        assert!(result.is_some());
        if let Some(JsExpr::SuperCall(args)) = result {
            assert_eq!(args.len(), 0, "super() should have no args");
        } else {
            panic!("expected SuperCall");
        }
    }

    #[test]
    fn hoist_super_already_at_position_zero_is_noop() {
        let mut body = vec![
            JsStmt::Expr(JsExpr::SuperCall(vec![])),
            body_stmt("a"),
            body_stmt("b"),
        ];
        hoist_super_call(&mut body, None);
        // Already at position 0 — should remain there.
        assert!(matches!(&body[0], JsStmt::Expr(JsExpr::SuperCall(_))));
        assert_eq!(body.len(), 3);
    }

    #[test]
    fn hoist_super_no_super_present_is_noop() {
        let mut body = vec![body_stmt("a"), body_stmt("b")];
        let orig_len = body.len();
        hoist_super_call(&mut body, None);
        assert_eq!(body.len(), orig_len);
    }

    #[test]
    fn hoist_super_multiple_deps_hoists_after_last() {
        // super(x, y) depends on both x and y. y is declared after x.
        // Should hoist to just after y's declaration.
        let mut body = vec![
            JsStmt::VarDecl {
                name: "x".into(),
                ty: Some(Type::Int(32)),
                init: Some(JsExpr::Literal(Constant::Int(1))),
                mutable: false,
            },
            body_stmt("unrelated"),
            JsStmt::VarDecl {
                name: "y".into(),
                ty: Some(Type::Int(32)),
                init: Some(JsExpr::Literal(Constant::Int(2))),
                mutable: false,
            },
            body_stmt("also_unrelated"),
            JsStmt::Expr(JsExpr::SuperCall(vec![
                JsExpr::Var("x".into()),
                JsExpr::Var("y".into()),
            ])),
        ];
        hoist_super_call(&mut body, None);
        // Should be at position 3 (after y's decl at position 2).
        assert!(
            matches!(&body[3], JsStmt::Expr(JsExpr::SuperCall(_))),
            "super should be at index 3, got {:?}",
            body[3]
        );
    }

    #[test]
    fn activation_with_self_referencing_value_kept() {
        // act$0.x = act$0 — the value references act$0, so it's not dead.
        let mut body = vec![
            JsStmt::VarDecl {
                name: "act$0".into(),
                ty: Some(Type::Dynamic),
                init: Some(JsExpr::Activation),
                mutable: false,
            },
            JsStmt::Assign {
                target: JsExpr::Field {
                    object: Box::new(JsExpr::Var("act$0".into())),
                    field: "self_ref".into(),
                },
                value: JsExpr::Var("act$0".into()), // value references act$0!
            },
        ];
        eliminate_dead_activations(&mut body);
        // Self-referencing value means it's NOT purely dead — should be kept.
        assert_eq!(body.len(), 2, "self-referencing activation should be kept");
    }

    #[test]
    fn method_bind_inside_call_arg_still_wraps() {
        // foo(this.update) — this.update is NOT in callee position (it's an arg).
        let bindable: HashSet<String> = ["update".to_string()].into();
        let mut expr = JsExpr::Call {
            callee: Box::new(JsExpr::Var("foo".into())),
            args: vec![JsExpr::Field {
                object: Box::new(JsExpr::This),
                field: "update".into(),
            }],
        };
        bind_method_refs_expr(&mut expr, &bindable, false);
        // The arg this.update should be wrapped with as3Bind.
        if let JsExpr::Call { args, .. } = &expr {
            assert!(
                matches!(&args[0], JsExpr::Call { callee, .. }
                    if matches!(callee.as_ref(), JsExpr::Var(n) if n == "as3Bind")),
                "arg should be wrapped with as3Bind, got {:?}",
                args[0]
            );
        } else {
            panic!("expected Call");
        }
    }

    #[test]
    fn method_bind_non_this_field_not_wrapped() {
        // obj.update (not this.update) — should NOT be wrapped.
        let bindable: HashSet<String> = ["update".to_string()].into();
        let mut expr = JsExpr::Field {
            object: Box::new(JsExpr::Var("obj".into())),
            field: "update".into(),
        };
        bind_method_refs_expr(&mut expr, &bindable, false);
        assert!(
            matches!(&expr, JsExpr::Field { object, .. }
                if matches!(object.as_ref(), JsExpr::Var(n) if n == "obj")),
            "non-this field should not be wrapped, got {:?}",
            expr
        );
    }

    #[test]
    fn const_field_not_promoted_without_class_name() {
        // If class_short_name is None, const instance fields should NOT be promoted.
        let mut ctx = empty_ctx();
        ctx.has_self = true;
        ctx.class_short_name = None; // no class name
        ctx.const_instance_fields.insert("MAX_HP".into());

        let expr = JsExpr::Field {
            object: Box::new(JsExpr::This),
            field: "MAX_HP".into(),
        };
        let result = rewrite_expr(expr, &ctx);
        // Without class_short_name, it should remain this.MAX_HP.
        assert!(
            matches!(&result, JsExpr::Field { object, .. }
                if matches!(object.as_ref(), JsExpr::This)),
            "without class name, should stay this.MAX_HP, got {:?}",
            result
        );
    }

    #[test]
    fn unknown_system_call_passes_through() {
        let ctx = empty_ctx();
        let args = vec![JsExpr::Literal(Constant::Int(1))];
        let result = rewrite_system_call("Unknown.System", "mystery", &args, &ctx);
        assert!(result.is_none(), "unknown system calls should return None");
    }

    #[test]
    fn scope_lookup_class_names_mapping_takes_priority() {
        // When a full qualified name matches class_names, it should return
        // the mapped short name, not try other resolution paths.
        let mut ctx = empty_ctx();
        ctx.class_names
            .insert("com.example::LongName".into(), "ShortName".into());
        let result = resolve_field(
            &scope_lookup("global::LongName"),
            "com.example::LongName",
            &ctx,
        );
        assert!(result.is_some());
        let expr = result.unwrap();
        assert!(
            matches!(&expr, JsExpr::Var(n) if n == "ShortName"),
            "should resolve to mapped short name, got {:?}",
            expr
        );
    }
}
