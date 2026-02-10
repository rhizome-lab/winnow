//! TypeScript AST printer.
//!
//! Converts `AstFunction` → TypeScript source. Handles all TS-specific
//! concerns: type mapping, class hierarchy resolution, scope-lookup
//! patterns, and identifier sanitization.

use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use reincarnate_core::ir::ast::{AstFunction, BinOp, Expr, Stmt, UnaryOp};
use reincarnate_core::ir::{CmpKind, Constant, MethodKind, Type, Visibility};

use crate::emit::sanitize_ident;
use crate::types::ts_type;

/// Context for printing a function/method body.
pub struct PrintCtx {
    /// Qualified class name → sanitized short name.
    pub class_names: HashMap<String, String>,
    /// Short names of the current class and all its ancestors.
    pub ancestors: HashSet<String>,
    /// Method short names visible in the class hierarchy.
    pub method_names: HashSet<String>,
    /// Instance field short names visible in the class hierarchy (from StructDef).
    pub instance_fields: HashSet<String>,
    /// Whether we are inside a method (have a `this`).
    pub has_self: bool,
    /// Name of the `self` parameter (e.g. "v0") — mapped to `this` during printing.
    pub self_param_name: Option<String>,
    /// Suppress `super()` calls (class has no real superclass, e.g. `extends Object`).
    pub suppress_super: bool,
    /// Whether we are inside a cinit (class static initializer).
    pub is_cinit: bool,
}

impl PrintCtx {
    pub fn for_function(class_names: &HashMap<String, String>) -> Self {
        Self {
            class_names: class_names.clone(),
            ancestors: HashSet::new(),
            method_names: HashSet::new(),
            instance_fields: HashSet::new(),
            has_self: false,
            self_param_name: None,
            suppress_super: false,
            is_cinit: false,
        }
    }

    pub fn for_method(
        class_names: &HashMap<String, String>,
        ancestors: &HashSet<String>,
        method_names: &HashSet<String>,
        instance_fields: &HashSet<String>,
    ) -> Self {
        Self {
            class_names: class_names.clone(),
            ancestors: ancestors.clone(),
            method_names: method_names.clone(),
            instance_fields: instance_fields.clone(),
            has_self: true,
            self_param_name: None,
            suppress_super: false,
            is_cinit: false,
        }
    }
}

/// Print a standalone function.
pub fn print_function(
    ast: &AstFunction,
    ctx: &PrintCtx,
    out: &mut String,
) {
    let vis = visibility_prefix(ast.visibility);
    let star = if ast.is_generator { "*" } else { "" };
    let params = print_params(&ast.params, &ast.body);
    let ret_ty = ts_type(&ast.return_ty);

    let _ = writeln!(
        out,
        "{vis}function{star} {}({params}): {ret_ty} {{",
        sanitize_ident(&ast.name),
    );

    print_stmts(&ast.body, ctx, out, "  ");

    let _ = writeln!(out, "}}\n");
}

/// Print a class method.
pub fn print_class_method(
    ast: &AstFunction,
    raw_name: &str,
    skip_self: bool,
    ctx: &PrintCtx,
    out: &mut String,
) {
    let params = if skip_self && !ast.params.is_empty() {
        &ast.params[1..]
    } else {
        &ast.params
    };
    let params_str = print_params_with_defaults(params, &ast.body);
    let ret_ty = ts_type(&ast.return_ty);
    let star = if ast.is_generator { "*" } else { "" };

    // cinit → static initializer block
    if raw_name == "cinit" && matches!(ast.method_kind, MethodKind::Static) {
        let _ = writeln!(out, "  static {{");
        let local_ctx = PrintCtx {
            class_names: ctx.class_names.clone(),
            ancestors: ctx.ancestors.clone(),
            method_names: ctx.method_names.clone(),
            instance_fields: ctx.instance_fields.clone(),
            has_self: true,
            self_param_name: ast.params.first().map(|(name, _)| name.clone()),
            suppress_super: ctx.suppress_super,
            is_cinit: true,
        };
        print_stmts(&ast.body, &local_ctx, out, "    ");
        let _ = writeln!(out, "  }}\n");
        return;
    }

    match ast.method_kind {
        MethodKind::Constructor => {
            let _ = writeln!(out, "  constructor({params_str}) {{");
        }
        MethodKind::Getter => {
            let name = raw_name.strip_prefix("get_").unwrap_or(raw_name);
            let _ = writeln!(out, "  get {name}(): {ret_ty} {{");
        }
        MethodKind::Setter => {
            let name = raw_name.strip_prefix("set_").unwrap_or(raw_name);
            let _ = writeln!(out, "  set {name}({params_str}) {{");
        }
        MethodKind::Static => {
            let _ = writeln!(
                out,
                "  static {star}{}({params_str}): {ret_ty} {{",
                sanitize_ident(raw_name),
            );
        }
        _ => {
            let _ = writeln!(
                out,
                "  {star}{}({params_str}): {ret_ty} {{",
                sanitize_ident(raw_name),
            );
        }
    }

    // Build a local context with self_param_name set for `this` substitution.
    let local_ctx = if skip_self && !ast.params.is_empty() {
        let mut lctx = PrintCtx {
            class_names: ctx.class_names.clone(),
            ancestors: ctx.ancestors.clone(),
            method_names: ctx.method_names.clone(),
            instance_fields: ctx.instance_fields.clone(),
            has_self: ctx.has_self,
            self_param_name: Some(ast.params[0].0.clone()),
            suppress_super: ctx.suppress_super,
            is_cinit: false,
        };
        // Ensure has_self is true when we have a self param.
        lctx.has_self = true;
        lctx
    } else {
        PrintCtx {
            class_names: ctx.class_names.clone(),
            ancestors: ctx.ancestors.clone(),
            method_names: ctx.method_names.clone(),
            instance_fields: ctx.instance_fields.clone(),
            has_self: ctx.has_self,
            self_param_name: ctx.self_param_name.clone(),
            suppress_super: ctx.suppress_super,
            is_cinit: false,
        }
    };

    let indent = if matches!(ast.method_kind, MethodKind::Free) {
        "  "
    } else {
        "    "
    };
    print_stmts(&ast.body, &local_ctx, out, indent);

    let _ = writeln!(out, "  }}");
}

fn print_params(params: &[(String, Type)], _body: &[Stmt]) -> String {
    params
        .iter()
        .map(|(name, ty)| format!("{}: {}", sanitize_ident(name), ts_type(ty)))
        .collect::<Vec<_>>()
        .join(", ")
}

fn print_params_with_defaults(params: &[(String, Type)], _body: &[Stmt]) -> String {
    params
        .iter()
        .map(|(name, ty)| format!("{}: {}", sanitize_ident(name), ts_type(ty)))
        .collect::<Vec<_>>()
        .join(", ")
}

// ---------------------------------------------------------------------------
// Statement printing
// ---------------------------------------------------------------------------

fn print_stmts(stmts: &[Stmt], ctx: &PrintCtx, out: &mut String, indent: &str) {
    for stmt in stmts {
        print_stmt(stmt, ctx, out, indent);
    }
}

fn print_stmt(stmt: &Stmt, ctx: &PrintCtx, out: &mut String, indent: &str) {
    match stmt {
        Stmt::VarDecl {
            name,
            ty,
            init,
            mutable,
        } => {
            let kw = if *mutable { "let" } else { "const" };
            let name_str = sanitize_ident(name);
            match (ty, init) {
                (Some(ty), Some(init)) => {
                    // Check if init is a cast to the same type — use annotation.
                    if let Expr::Cast { expr, ty: cast_ty } = init {
                        if cast_ty == ty {
                            let _ = writeln!(
                                out,
                                "{indent}{kw} {name_str}: {} = {};",
                                ts_type(ty),
                                print_expr(expr, ctx),
                            );
                            return;
                        }
                    }
                    let _ = writeln!(
                        out,
                        "{indent}{kw} {name_str}: {} = {};",
                        ts_type(ty),
                        print_expr(init, ctx),
                    );
                }
                (Some(ty), None) => {
                    let _ = writeln!(out, "{indent}{kw} {name_str}: {};", ts_type(ty));
                }
                (None, Some(init)) => {
                    // Check for cast — use type annotation instead of `as`.
                    if let Expr::Cast { expr, ty } = init {
                        let _ = writeln!(
                            out,
                            "{indent}{kw} {name_str}: {} = {};",
                            ts_type(ty),
                            print_expr(expr, ctx),
                        );
                    } else {
                        let _ = writeln!(
                            out,
                            "{indent}{kw} {name_str} = {};",
                            print_expr(init, ctx),
                        );
                    }
                }
                (None, None) => {
                    let _ = writeln!(out, "{indent}{kw} {name_str};");
                }
            }
        }

        Stmt::Assign { target, value } => {
            let _ = writeln!(
                out,
                "{indent}{} = {};",
                print_expr(target, ctx),
                print_expr(value, ctx),
            );
        }

        Stmt::CompoundAssign { target, op, value } => {
            let _ = writeln!(
                out,
                "{indent}{} {}= {};",
                print_expr(target, ctx),
                binop_str(*op),
                print_expr(value, ctx),
            );
        }

        Stmt::Expr(expr) => {
            // Handle constructSuper pattern.
            if let Expr::SystemCall {
                system,
                method,
                args,
            } = expr
            {
                if system == "Flash.Class" && method == "constructSuper" {
                    if ctx.suppress_super {
                        return;
                    }
                    let rest_args: Vec<_> = args[1..]
                        .iter()
                        .map(|a| print_expr(a, ctx))
                        .collect();
                    let _ = writeln!(out, "{indent}super({});", rest_args.join(", "));
                    return;
                }
            }
            let _ = writeln!(out, "{indent}{};", print_expr(expr, ctx));
        }

        Stmt::If {
            cond,
            then_body,
            else_body,
        } => {
            if then_body.is_empty() && else_body.is_empty() {
                return;
            }
            let inner = format!("{indent}  ");
            if else_body.is_empty() {
                let _ = writeln!(out, "{indent}if ({}) {{", print_expr(cond, ctx));
                print_stmts(then_body, ctx, out, &inner);
                let _ = writeln!(out, "{indent}}}");
            } else {
                let _ = writeln!(out, "{indent}if ({}) {{", print_expr(cond, ctx));
                print_stmts(then_body, ctx, out, &inner);
                let _ = writeln!(out, "{indent}}} else {{");
                print_stmts(else_body, ctx, out, &inner);
                let _ = writeln!(out, "{indent}}}");
            }
        }

        Stmt::While { cond, body } => {
            let _ = writeln!(out, "{indent}while ({}) {{", print_expr(cond, ctx));
            let inner = format!("{indent}  ");
            print_stmts(body, ctx, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            // For loops are emitted as a regular for loop structure.
            let inner = format!("{indent}  ");
            print_stmts(init, ctx, out, indent);
            let _ = writeln!(out, "{indent}while ({}) {{", print_expr(cond, ctx));
            print_stmts(body, ctx, out, &inner);
            print_stmts(update, ctx, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        Stmt::Loop { body } => {
            let _ = writeln!(out, "{indent}while (true) {{");
            let inner = format!("{indent}  ");
            print_stmts(body, ctx, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        Stmt::Return(expr) => {
            if let Some(e) = expr {
                let _ = writeln!(out, "{indent}return {};", print_expr(e, ctx));
            } else {
                let _ = writeln!(out, "{indent}return;");
            }
        }

        Stmt::Break => {
            let _ = writeln!(out, "{indent}break;");
        }

        Stmt::Continue => {
            let _ = writeln!(out, "{indent}continue;");
        }

        Stmt::LabeledBreak { depth } => {
            let _ = writeln!(out, "{indent}break L{depth};");
        }

        Stmt::Dispatch { blocks, entry } => {
            let _ = writeln!(out, "{indent}let $block = {entry};");
            let _ = writeln!(out, "{indent}while (true) {{");
            let _ = writeln!(out, "{indent}  switch ($block) {{");
            for (idx, block_stmts) in blocks {
                let _ = writeln!(out, "{indent}    case {idx}: {{");
                let case_indent = format!("{indent}      ");
                print_stmts(block_stmts, ctx, out, &case_indent);
                let _ = writeln!(out, "{indent}    }}");
            }
            let _ = writeln!(out, "{indent}  }}");
            let _ = writeln!(out, "{indent}}}");
        }
    }
}

// ---------------------------------------------------------------------------
// Expression printing
// ---------------------------------------------------------------------------

fn print_expr(expr: &Expr, ctx: &PrintCtx) -> String {
    match expr {
        Expr::Literal(c) => emit_constant(c),

        Expr::Var(name) => {
            if let Some(ref self_name) = ctx.self_param_name {
                if name == self_name {
                    return "this".into();
                }
            }
            sanitize_ident(name)
        }

        Expr::Binary { op, lhs, rhs } => {
            let op_str = binop_str(*op);
            // Handle scope-lookup stripping for binops.
            if is_scope_lookup(lhs) {
                return print_expr_operand(rhs, ctx);
            }
            if is_scope_lookup(rhs) {
                return print_expr_operand(lhs, ctx);
            }
            format!(
                "{} {op_str} {}",
                print_expr_operand(lhs, ctx),
                print_expr_operand(rhs, ctx),
            )
        }

        Expr::Unary { op, expr: inner } => {
            let op_str = match op {
                UnaryOp::Neg => "-",
                UnaryOp::BitNot => "~",
            };
            format!("{op_str}{}", print_expr_operand(inner, ctx))
        }

        Expr::Cmp { kind, lhs, rhs } => {
            let has_null = is_null_literal(lhs) || is_null_literal(rhs);
            let op_str = if has_null {
                match kind {
                    CmpKind::Eq => "==",
                    CmpKind::Ne => "!=",
                    _ => cmp_str(*kind),
                }
            } else {
                cmp_str(*kind)
            };
            format!(
                "{} {op_str} {}",
                print_expr_operand(lhs, ctx),
                print_expr_operand(rhs, ctx),
            )
        }

        Expr::Field { object, field } => {
            // Scope-lookup + GetField → resolve to `this.field` or bare name.
            if let Some(args) = scope_lookup_args(object) {
                let effective = field.rsplit("::").next().unwrap_or(field);
                match resolve_scope_lookup(args, ctx) {
                    ScopeResolution::Ancestor(ref class_name) => {
                        let safe = sanitize_ident(effective);
                        if ctx.is_cinit || ctx.instance_fields.contains(effective) {
                            // In cinit `this` is the class; for instance fields
                            // `this.field` is correct on the instance.
                            return format!("this.{safe}");
                        }
                        // Static members live on the class constructor, not instances.
                        let cls = sanitize_ident(class_name);
                        return format!("{cls}.{safe}");
                    }
                    ScopeResolution::ScopeLookup => {
                        // Check if the field is a known class name.
                        if let Some(short) = ctx.class_names.get(field) {
                            return short.clone();
                        }
                        if ctx.is_cinit {
                            let safe = sanitize_ident(effective);
                            return format!("this.{safe}");
                        }
                        return sanitize_ident(effective);
                    }
                }
            }
            // Normal field access.
            let effective_field = if field.contains("::") {
                field.rsplit("::").next().unwrap_or(field)
            } else {
                field
            };
            if is_valid_js_ident(effective_field) {
                format!("{}.{effective_field}", print_expr_operand(object, ctx))
            } else {
                format!(
                    "{}[\"{}\"]",
                    print_expr_operand(object, ctx),
                    escape_js_string(effective_field),
                )
            }
        }

        Expr::Index { collection, index } => {
            format!(
                "{}[{}]",
                print_expr_operand(collection, ctx),
                print_expr(index, ctx),
            )
        }

        Expr::Call { func: fname, args } => {
            print_call(fname, args, ctx)
        }

        Expr::CallIndirect { callee, args } => {
            let args_str: Vec<_> = args.iter().map(|a| print_expr(a, ctx)).collect();
            format!("{}({})", print_expr(callee, ctx), args_str.join(", "))
        }

        Expr::SystemCall {
            system,
            method,
            args,
        } => {
            print_system_call(system, method, args, ctx)
        }

        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            format!(
                "{} ? {} : {}",
                print_expr_operand(cond, ctx),
                print_expr_operand(then_val, ctx),
                print_expr_operand(else_val, ctx),
            )
        }

        Expr::LogicalOr { lhs, rhs } => {
            format!(
                "{} || {}",
                print_expr_operand(lhs, ctx),
                print_expr_operand(rhs, ctx),
            )
        }

        Expr::LogicalAnd { lhs, rhs } => {
            format!(
                "{} && {}",
                print_expr_operand(lhs, ctx),
                print_expr_operand(rhs, ctx),
            )
        }

        Expr::Cast { expr: inner, ty } => {
            format!("{} as {}", print_expr_operand(inner, ctx), ts_type(ty))
        }

        Expr::TypeCheck { expr: inner, ty } => {
            print_type_check(inner, ty, ctx)
        }

        Expr::ArrayInit(elems) => {
            let elems_str: Vec<_> = elems.iter().map(|e| print_expr(e, ctx)).collect();
            format!("[{}]", elems_str.join(", "))
        }

        Expr::StructInit { name: _, fields } => {
            let field_strs: Vec<_> = fields
                .iter()
                .map(|(name, val)| {
                    if is_valid_js_ident(name) {
                        format!("{name}: {}", print_expr(val, ctx))
                    } else {
                        format!("\"{}\": {}", escape_js_string(name), print_expr(val, ctx))
                    }
                })
                .collect();
            format!("{{ {} }}", field_strs.join(", "))
        }

        Expr::TupleInit(elems) => {
            let elems_str: Vec<_> = elems.iter().map(|e| print_expr(e, ctx)).collect();
            format!("[{}]", elems_str.join(", "))
        }

        Expr::GlobalRef(name) => sanitize_ident(name),

        Expr::CoroutineCreate { func: fname, args } => {
            let args_str: Vec<_> = args.iter().map(|a| print_expr(a, ctx)).collect();
            format!("{}({})", sanitize_ident(fname), args_str.join(", "))
        }

        Expr::CoroutineResume(inner) => {
            format!("{}.next()", print_expr(inner, ctx))
        }

        Expr::Yield(v) => {
            if let Some(inner) = v {
                format!("yield {}", print_expr(inner, ctx))
            } else {
                "yield".into()
            }
        }

        Expr::Not(inner) => {
            format!("!{}", print_expr_operand(inner, ctx))
        }

        Expr::PostIncrement(inner) => {
            format!("{}++", print_expr_operand(inner, ctx))
        }
    }
}

/// Print an expression as an operand (may need parenthesization).
fn print_expr_operand(expr: &Expr, ctx: &PrintCtx) -> String {
    if needs_parens(expr) {
        format!("({})", print_expr(expr, ctx))
    } else {
        print_expr(expr, ctx)
    }
}

/// Whether an expression needs parentheses when used as an operand.
fn needs_parens(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Binary { .. }
            | Expr::Cmp { .. }
            | Expr::Ternary { .. }
            | Expr::LogicalOr { .. }
            | Expr::LogicalAnd { .. }
            | Expr::Cast { .. }
            | Expr::Unary { .. }
            | Expr::Not(_)
    )
}

// ---------------------------------------------------------------------------
// Pattern-specific printers
// ---------------------------------------------------------------------------

/// Detect whether an expression is a scope-lookup (findPropStrict/findProperty).
fn is_scope_lookup(expr: &Expr) -> bool {
    scope_lookup_args(expr).is_some()
}

/// Extract the args from a scope-lookup SystemCall, or None if not a scope lookup.
fn scope_lookup_args(expr: &Expr) -> Option<&[Expr]> {
    match expr {
        Expr::SystemCall {
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

/// Resolve the class name from a scope-lookup arg string constant.
fn class_from_scope_arg(args: &[Expr]) -> Option<String> {
    let arg = args.first()?;
    if let Expr::Literal(Constant::String(s)) = arg {
        let prefix = s.rsplit_once("::")?.0;
        let class_name = prefix.rsplit_once(':')?.1;
        Some(class_name.to_string())
    } else {
        None
    }
}

/// Resolve a scope-lookup expression:
/// - If the lookup is for an ancestor class → "this"
/// - Otherwise → marks as a scope lookup (bare name resolution happens
///   at the consumption site: Field, Call, etc.)
fn resolve_scope_lookup(args: &[Expr], ctx: &PrintCtx) -> ScopeResolution {
    if ctx.has_self {
        if let Some(class_name) = class_from_scope_arg(args) {
            if ctx.ancestors.contains(&class_name) {
                return ScopeResolution::Ancestor(class_name);
            }
        }
    }
    ScopeResolution::ScopeLookup
}

enum ScopeResolution {
    /// The lookup matched an ancestor class — carry the short class name.
    Ancestor(String),
    ScopeLookup,
}

/// Print a Call expression with TS-specific rewrites.
fn print_call(fname: &str, args: &[Expr], ctx: &PrintCtx) -> String {
    if fname.contains("::") && !args.is_empty() {
        // Qualified name → method dispatch: receiver.method(rest_args)
        let method = fname.rsplit("::").next().unwrap_or(fname);
        let receiver = &args[0];
        let rest_args: Vec<_> = args[1..].iter().map(|a| print_expr(a, ctx)).collect();
        let rest_str = rest_args.join(", ");

        if is_scope_lookup(receiver) {
            let use_this =
                (ctx.has_self && ctx.method_names.contains(method)) || ctx.is_cinit;
            return if use_this {
                format!("this.{}({rest_str})", sanitize_ident(method))
            } else {
                format!("{}({rest_str})", sanitize_ident(method))
            };
        }
        return format!(
            "{}.{}({rest_str})",
            print_expr_operand(receiver, ctx),
            sanitize_ident(method),
        );
    }

    if !args.is_empty() && is_scope_lookup(&args[0]) {
        // Scope-lookup receiver — strip it.
        let rest_args: Vec<_> = args[1..].iter().map(|a| print_expr(a, ctx)).collect();
        let rest_str = rest_args.join(", ");
        let safe_name = sanitize_ident(fname);
        return if ctx.has_self && ctx.method_names.contains(fname) {
            format!("this.{safe_name}({rest_str})")
        } else {
            format!("{safe_name}({rest_str})")
        };
    }

    // Global dotted call (e.g. Math.max, Math.min) — not a method dispatch.
    if fname.contains('.') {
        let args_str: Vec<_> = args.iter().map(|a| print_expr(a, ctx)).collect();
        return format!("{fname}({})", args_str.join(", "));
    }

    if !args.is_empty() {
        // Unqualified call with receiver: args[0].method(args[1..])
        let receiver = &args[0];
        let rest_args: Vec<_> = args[1..].iter().map(|a| print_expr(a, ctx)).collect();
        let rest_str = rest_args.join(", ");
        format!(
            "{}.{}({rest_str})",
            print_expr_operand(receiver, ctx),
            sanitize_ident(fname),
        )
    } else {
        format!("{}()", sanitize_ident(fname))
    }
}

/// Print a SystemCall expression with TS-specific rewrites.
fn print_system_call(
    system: &str,
    method: &str,
    args: &[Expr],
    ctx: &PrintCtx,
) -> String {
    // constructSuper → super()
    if system == "Flash.Class" && method == "constructSuper" {
        if ctx.suppress_super {
            return "void 0".to_string();
        }
        let rest_args: Vec<_> = args[1..].iter().map(|a| print_expr(a, ctx)).collect();
        return format!("super({})", rest_args.join(", "));
    }

    // construct → new
    if system == "Flash.Object" && method == "construct" {
        if let Some((ctor, rest)) = args.split_first() {
            let rest_args: Vec<_> = rest.iter().map(|a| print_expr(a, ctx)).collect();
            return format!("new {}({})", print_expr(ctor, ctx), rest_args.join(", "));
        }
    }

    // findPropStrict/findProperty → resolve scope or emit `this`
    if system == "Flash.Scope" && (method == "findPropStrict" || method == "findProperty") {
        match resolve_scope_lookup(args, ctx) {
            ScopeResolution::Ancestor(ref class_name) => {
                if ctx.is_cinit {
                    return "this".into();
                }
                return sanitize_ident(class_name);
            }
            ScopeResolution::ScopeLookup => {
                // The scope lookup will be consumed by Field/Call site.
                // If it reaches here as a standalone expression, skip it.
                return String::new();
            }
        }
    }

    // Default system call.
    let args_str: Vec<_> = args.iter().map(|a| print_expr(a, ctx)).collect();
    let sys_ident = sanitize_ident(system);
    let safe_method = if is_valid_js_ident(method) {
        format!(".{method}")
    } else {
        format!("[\"{}\"]", escape_js_string(method))
    };
    format!("{sys_ident}{safe_method}({})", args_str.join(", "))
}

/// Print a TypeCheck expression.
fn print_type_check(expr: &Expr, ty: &Type, ctx: &PrintCtx) -> String {
    let operand = print_expr_operand(expr, ctx);
    match ty {
        Type::Bool => format!("typeof {operand} === \"boolean\""),
        Type::Int(_) | Type::UInt(_) | Type::Float(_) => {
            format!("typeof {operand} === \"number\"")
        }
        Type::String => format!("typeof {operand} === \"string\""),
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            format!("{operand} instanceof {}", sanitize_ident(short))
        }
        Type::Union(types) => {
            let checks: Vec<_> = types
                .iter()
                .map(|t| print_type_check(expr, t, ctx))
                .collect();
            format!("({})", checks.join(" || "))
        }
        _ => format!("typeof {operand} === \"object\""),
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn is_null_literal(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Constant::Null))
}

fn binop_str(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Rem => "%",
        BinOp::BitAnd => "&",
        BinOp::BitOr => "|",
        BinOp::BitXor => "^",
        BinOp::Shl => "<<",
        BinOp::Shr => ">>",
    }
}

fn cmp_str(kind: CmpKind) -> &'static str {
    match kind {
        CmpKind::Eq => "===",
        CmpKind::Ne => "!==",
        CmpKind::Lt => "<",
        CmpKind::Le => "<=",
        CmpKind::Gt => ">",
        CmpKind::Ge => ">=",
    }
}

fn is_valid_js_ident(name: &str) -> bool {
    !name.is_empty()
        && !name.starts_with(|c: char| c.is_ascii_digit())
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
}

fn emit_constant(c: &Constant) -> String {
    match c {
        Constant::Null => "null".into(),
        Constant::Bool(b) => b.to_string(),
        Constant::Int(n) => n.to_string(),
        Constant::UInt(n) => n.to_string(),
        Constant::Float(f) => format_float(*f),
        Constant::String(s) => format!("\"{}\"", escape_js_string(s)),
    }
}

fn format_float(f: f64) -> String {
    if f.fract() == 0.0 && f.is_finite() {
        format!("{f:.1}")
    } else {
        format!("{f}")
    }
}

fn escape_js_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out
}

fn visibility_prefix(vis: Visibility) -> &'static str {
    match vis {
        Visibility::Public => "export ",
        Visibility::Private | Visibility::Protected => "",
    }
}
