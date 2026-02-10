//! Flash/AVM2-specific JsExpr → JsExpr rewrite pass.
//!
//! Runs AFTER mechanical `Expr → JsExpr` lowering. Resolves Flash SystemCall
//! nodes (scope lookups, super dispatch, object construction, etc.) into
//! native JavaScript constructs. All Flash-specific knowledge is confined here;
//! `lower.rs` is purely engine-agnostic.

use std::collections::{HashMap, HashSet};

use reincarnate_core::ir::Constant;

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
    /// Method short names visible in the class hierarchy.
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
            if ctx.is_cinit || ctx.instance_fields.contains(effective) {
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
    let _ = scope_args; // Resolution is based on method name + hierarchy metadata
    let use_this = (ctx.has_self && ctx.method_names.contains(method)) || ctx.is_cinit;
    let callee = if use_this {
        JsExpr::Field {
            object: Box::new(JsExpr::This),
            field: method.to_string(),
        }
    } else {
        JsExpr::Var(method.to_string())
    };
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
    func
}

/// Move the first `super()` call to position 0 in a constructor body.
pub fn hoist_super_call(body: &mut Vec<JsStmt>) {
    let pos = body
        .iter()
        .position(|s| matches!(s, JsStmt::Expr(JsExpr::SuperCall(_))));
    if let Some(i) = pos {
        if i > 0 {
            let stmt = body.remove(i);
            body.insert(0, stmt);
        }
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
                let value = rewrite_expr(args[2].clone(), ctx);
                return Some(JsStmt::Assign {
                    target: JsExpr::SuperGet(name.clone()),
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

        JsExpr::Field { object, field } => JsExpr::Field {
            object: Box::new(rewrite_expr(*object, ctx)),
            field,
        },

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

        JsExpr::Cast { expr: inner, ty } => JsExpr::Cast {
            expr: Box::new(rewrite_expr(*inner, ctx)),
            ty,
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

    // newFunction → this.methodRef
    if system == "Flash.Object" && method == "newFunction" && args.len() == 1 {
        if let JsExpr::Literal(Constant::String(ref name)) = args[0] {
            let short = name.rsplit("::").next().unwrap_or(name);
            return Some(JsExpr::Field {
                object: Box::new(JsExpr::This),
                field: short.to_string(),
            });
        }
    }

    // construct → new Ctor(args)
    if system == "Flash.Object" && method == "construct" && !args.is_empty() {
        let callee = rewrite_expr(args[0].clone(), ctx);
        let rest = rewrite_exprs(args[1..].to_vec(), ctx);
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
            let rest = rewrite_exprs(args[2..].to_vec(), ctx);
            return Some(JsExpr::SuperMethodCall {
                method: name.clone(),
                args: rest,
            });
        }
    }

    // getSuper(this, "prop") → super.prop
    if system == "Flash.Class" && method == "getSuper" && args.len() == 2 {
        if let JsExpr::Literal(Constant::String(ref name)) = args[1] {
            return Some(JsExpr::SuperGet(name.clone()));
        }
    }

    // setSuper(this, "prop", value) → (super.prop = value)
    if system == "Flash.Class" && method == "setSuper" && args.len() == 3 {
        if let JsExpr::Literal(Constant::String(ref name)) = args[1] {
            return Some(JsExpr::SuperSet {
                prop: name.clone(),
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
