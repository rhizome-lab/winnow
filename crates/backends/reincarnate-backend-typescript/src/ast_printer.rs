//! TypeScript AST printer.
//!
//! Prints `JsFunction` → TypeScript source. Handles all TS-specific formatting:
//! type annotations, identifier sanitization, precedence-based parenthesization.
//! Contains zero engine knowledge — all SystemCall resolution happened during
//! the `lower` pass.

use std::fmt::Write;

use reincarnate_core::ir::ast::BinOp;
use reincarnate_core::ir::{CastKind, CmpKind, Constant, MethodKind, Type, UnaryOp, Visibility};

use crate::emit::sanitize_ident;
use crate::js_ast::{JsExpr, JsFunction, JsStmt};
use crate::types::ts_type;

// ---------------------------------------------------------------------------
// Function / method printing
// ---------------------------------------------------------------------------

/// Print a standalone function.
pub fn print_function(js: &JsFunction, preamble: Option<&str>, out: &mut String) {
    let vis = visibility_prefix(js.visibility);
    let star = if js.is_generator { "*" } else { "" };
    let params = print_params(&js.params, js.has_rest_param, false);
    let ret_ty = ts_type(&js.return_ty);

    let _ = writeln!(
        out,
        "{vis}function{star} {}({params}): {ret_ty} {{",
        sanitize_ident(&js.name),
    );

    if let Some(pre) = preamble {
        let _ = writeln!(out, "  {pre}");
    }

    print_stmts(&js.body, out, "  ");

    let _ = writeln!(out, "}}\n");
}

/// Print a class method.
pub fn print_class_method(
    js: &JsFunction,
    raw_name: &str,
    skip_self: bool,
    preamble: Option<&str>,
    out: &mut String,
) {
    let params = if skip_self && !js.params.is_empty() {
        &js.params[1..]
    } else {
        &js.params
    };
    let params_str = print_params(params, js.has_rest_param, false);
    let ret_ty = ts_type(&js.return_ty);
    let star = if js.is_generator { "*" } else { "" };

    // cinit → static initializer block
    if raw_name == "cinit" && matches!(js.method_kind, MethodKind::Static) {
        let _ = writeln!(out, "  static {{");
        print_stmts(&js.body, out, "    ");
        let _ = writeln!(out, "  }}\n");
        return;
    }

    match js.method_kind {
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

    let indent = if matches!(js.method_kind, MethodKind::Free) {
        "  "
    } else {
        "    "
    };
    if let Some(pre) = preamble {
        let _ = writeln!(out, "{indent}{pre}");
    }
    print_stmts(&js.body, out, indent);

    let _ = writeln!(out, "  }}");
}

fn print_params(params: &[(String, Type)], has_rest_param: bool, infer_dynamic: bool) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, (name, ty))| {
            let prefix = if has_rest_param && i == params.len() - 1 {
                "..."
            } else {
                ""
            };
            // When infer_dynamic is set and the type is Dynamic, omit `: any` so
            // TypeScript can contextually infer the type from the call site.
            if infer_dynamic && matches!(ty, Type::Dynamic) {
                format!("{prefix}{}", sanitize_ident(name))
            } else {
                format!("{prefix}{}: {}", sanitize_ident(name), ts_type(ty))
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

// ---------------------------------------------------------------------------
// Statement printing
// ---------------------------------------------------------------------------

fn print_stmts(stmts: &[JsStmt], out: &mut String, indent: &str) {
    for stmt in stmts {
        print_stmt(stmt, out, indent);
    }
}

fn print_stmt(stmt: &JsStmt, out: &mut String, indent: &str) {
    match stmt {
        JsStmt::VarDecl {
            name,
            ty,
            init,
            mutable,
        } => {
            let kw = if *mutable { "let" } else { "const" };
            let name_str = sanitize_ident(name);
            match (ty, init) {
                (Some(ty), Some(init)) => {
                    // Cast to the same type: strip TS assertion forms and use type
                    // annotation. Keep function-call forms (asType, Number, int, etc.).
                    if let JsExpr::Cast { expr, ty: cast_ty, kind } = init {
                        let is_ts_assertion = match kind {
                            CastKind::AsType => !matches!(ty, Type::Struct(_) | Type::Enum(_)),
                            CastKind::Coerce => matches!(ty, Type::Struct(_) | Type::Enum(_) | Type::Dynamic),
                        };
                        if cast_ty == ty && is_ts_assertion {
                            let _ = writeln!(
                                out,
                                "{indent}{kw} {name_str}: {} = {};",
                                ts_type(ty),
                                print_expr(expr),
                            );
                            return;
                        }
                    }
                    let _ = writeln!(
                        out,
                        "{indent}{kw} {name_str}: {} = {};",
                        ts_type(ty),
                        print_expr(init),
                    );
                }
                (Some(ty), None) => {
                    let _ = writeln!(out, "{indent}{kw} {name_str}: {};", ts_type(ty));
                }
                (None, Some(init)) => {
                    // Cast → determine if the cast form is a TS assertion (strippable
                    // to type annotation) or a runtime call (must keep in expr).
                    if let JsExpr::Cast { expr, ty, kind } = init {
                        let is_ts_assertion = match kind {
                            CastKind::AsType => !matches!(ty, Type::Struct(_) | Type::Enum(_)),
                            CastKind::Coerce => matches!(ty, Type::Struct(_) | Type::Enum(_) | Type::Dynamic),
                        };
                        if is_ts_assertion {
                            // Strip TS assertion, use type annotation + inner expr.
                            let _ = writeln!(
                                out,
                                "{indent}{kw} {name_str}: {} = {};",
                                ts_type(ty),
                                print_expr(expr),
                            );
                        } else {
                            // Keep the runtime call (asType/Number/int/etc.), add type annotation.
                            let _ = writeln!(
                                out,
                                "{indent}{kw} {name_str}: {} = {};",
                                ts_type(ty),
                                print_expr(init),
                            );
                        }
                    } else {
                        // Empty array `[]` or empty object `{}` with no explicit type
                        // annotation would cause TypeScript to infer `any[]` (TS7034) or
                        // `{}` with no index signature (TS7053 when string-indexed later).
                        // Annotate conservatively so the types are explicit.
                        let annotation = match init {
                            JsExpr::ArrayInit(elems) if elems.is_empty() => {
                                Some("unknown[]")
                            }
                            // Any object literal without an explicit type annotation is
                            // treated as a dynamic map. TypeScript would otherwise infer
                            // a narrow structural type (`{}` or `{ k: T; ... }`) with no
                            // index signature, causing TS7053 when the object is later
                            // accessed with a dynamic or `any`-typed key.
                            JsExpr::ObjectInit(_) => Some("Record<string, unknown>"),
                            _ => None,
                        };
                        if let Some(ann) = annotation {
                            let _ = writeln!(
                                out,
                                "{indent}{kw} {name_str}: {ann} = {};",
                                print_expr(init),
                            );
                        } else {
                            let _ = writeln!(
                                out,
                                "{indent}{kw} {name_str} = {};",
                                print_expr(init),
                            );
                        }
                    }
                }
                (None, None) => {
                    let _ = writeln!(out, "{indent}{kw} {name_str};");
                }
            }
        }

        JsStmt::Assign { target, value } => {
            let tgt = print_expr(target);
            // Any object literal assigned to an untyped variable causes TypeScript to
            // infer a narrow structural type (`{}` or `{ k: T; ... }`), which has no
            // index signature. Subsequent dynamic-key access then fails with TS7053.
            // Cast to Record<string, unknown> to match the VarDecl treatment.
            let val = if matches!(value, JsExpr::ObjectInit(_)) {
                format!("{} as Record<string, unknown>", print_expr(value))
            } else {
                print_expr(value)
            };
            if tgt.starts_with('{') {
                let _ = writeln!(out, "{indent}({tgt}) = {val};");
            } else {
                let _ = writeln!(out, "{indent}{tgt} = {val};");
            }
        }

        JsStmt::CompoundAssign { target, op, value } => {
            let tgt = print_expr(target);
            let val = print_expr(value);
            let op_str = binop_str(*op);
            if tgt.starts_with('{') {
                let _ = writeln!(out, "{indent}({tgt}) {op_str}= {val};");
            } else {
                let _ = writeln!(out, "{indent}{tgt} {op_str}= {val};");
            }
        }

        JsStmt::Expr(expr) => {
            let s = print_expr(expr);
            if s.starts_with('{') {
                let _ = writeln!(out, "{indent}({s});");
            } else {
                let _ = writeln!(out, "{indent}{s};");
            }
        }

        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            if then_body.is_empty() && else_body.is_empty() {
                return;
            }
            let inner = format!("{indent}  ");
            if else_body.is_empty() {
                let _ = writeln!(out, "{indent}if ({}) {{", print_expr(cond));
                print_stmts(then_body, out, &inner);
                let _ = writeln!(out, "{indent}}}");
            } else {
                let _ = writeln!(out, "{indent}if ({}) {{", print_expr(cond));
                print_stmts(then_body, out, &inner);
                let _ = writeln!(out, "{indent}}} else {{");
                print_stmts(else_body, out, &inner);
                let _ = writeln!(out, "{indent}}}");
            }
        }

        JsStmt::While { cond, body } => {
            let _ = writeln!(out, "{indent}while ({}) {{", print_expr(cond));
            let inner = format!("{indent}  ");
            print_stmts(body, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => {
            let inner = format!("{indent}  ");
            // Try to emit as proper `for (init; cond; update)` syntax when
            // init is a single VarDecl and update is a single statement.
            if init.len() == 1 && update.len() == 1 {
                if let (Some(init_str), Some(update_str)) =
                    (print_for_init(&init[0]), print_for_update(&update[0]))
                {
                    let _ = writeln!(
                        out,
                        "{indent}for ({init_str}; {}; {update_str}) {{",
                        print_expr(cond),
                    );
                    print_stmts(body, out, &inner);
                    let _ = writeln!(out, "{indent}}}");
                    return;
                }
            }
            // Fallback: emit as `init; while (cond) { body; update; }`.
            print_stmts(init, out, indent);
            let _ = writeln!(out, "{indent}while ({}) {{", print_expr(cond));
            print_stmts(body, out, &inner);
            print_stmts(update, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        JsStmt::Loop { body } => {
            let _ = writeln!(out, "{indent}while (true) {{");
            let inner = format!("{indent}  ");
            print_stmts(body, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        JsStmt::ForOf {
            binding,
            declare,
            iterable,
            body,
        } => {
            let inner = format!("{indent}  ");
            let decl = if *declare { "const " } else { "" };
            let _ = writeln!(
                out,
                "{indent}for ({decl}{} of {}) {{",
                sanitize_ident(binding),
                print_expr(iterable),
            );
            print_stmts(body, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        JsStmt::Return(expr) => {
            if let Some(e) = expr {
                let _ = writeln!(out, "{indent}return {};", print_expr(e));
            } else {
                let _ = writeln!(out, "{indent}return;");
            }
        }

        JsStmt::Break => {
            let _ = writeln!(out, "{indent}break;");
        }

        JsStmt::Continue => {
            let _ = writeln!(out, "{indent}continue;");
        }

        JsStmt::LabeledBreak { depth } => {
            let _ = writeln!(out, "{indent}break L{depth};");
        }

        JsStmt::Dispatch { blocks, entry } => {
            // A single-block dispatch is a degenerate case: the while/switch wrapper
            // would create an infinite loop with no exit, making subsequent code
            // unreachable (TS7027). Inline the block body directly.
            if blocks.len() == 1 {
                let (_, block_stmts) = &blocks[0];
                print_stmts(block_stmts, out, indent);
            } else {
                let _ = writeln!(out, "{indent}let $block = {entry};");
                let _ = writeln!(out, "{indent}while (true) {{");
                let _ = writeln!(out, "{indent}  switch ($block) {{");
                for (idx, block_stmts) in blocks {
                    let _ = writeln!(out, "{indent}    case {idx}: {{");
                    let case_indent = format!("{indent}      ");
                    print_stmts(block_stmts, out, &case_indent);
                    let _ = writeln!(out, "{indent}    }}");
                }
                let _ = writeln!(out, "{indent}  }}");
                let _ = writeln!(out, "{indent}}}");
            }
        }

        JsStmt::Switch {
            value,
            cases,
            default_body,
        } => {
            let _ = writeln!(out, "{indent}switch ({} as any) {{", print_expr(value));
            let case_indent = format!("{indent}  ");
            for (constant, case_stmts) in cases {
                let _ = writeln!(
                    out,
                    "{indent}  case {}:",
                    emit_constant(constant)
                );
                if case_stmts.is_empty() {
                    // Fall-through: no body, no break.
                } else {
                    print_stmts(case_stmts, out, &case_indent);
                    let _ = writeln!(out, "{indent}    break;");
                }
            }
            if !default_body.is_empty() {
                let _ = writeln!(out, "{indent}  default:");
                print_stmts(default_body, out, &case_indent);
            }
            let _ = writeln!(out, "{indent}}}");
        }

        // --- JS-specific statements ---
        JsStmt::Throw(expr) => {
            let _ = writeln!(out, "{indent}throw {};", print_expr(expr));
        }
    }
}

// ---------------------------------------------------------------------------
// For-loop header helpers
// ---------------------------------------------------------------------------

/// Print a single init statement for a `for` header (no trailing semicolon).
fn print_for_init(stmt: &JsStmt) -> Option<String> {
    match stmt {
        JsStmt::VarDecl {
            name,
            ty,
            init: Some(init),
            mutable,
        } => {
            let kw = if *mutable { "let" } else { "const" };
            let name_str = sanitize_ident(name);
            match ty {
                Some(ty) => Some(format!("{kw} {name_str}: {} = {}", ts_type(ty), print_expr(init))),
                None => Some(format!("{kw} {name_str} = {}", print_expr(init))),
            }
        }
        JsStmt::Assign { target, value } => {
            Some(format!("{} = {}", print_expr(target), print_expr(value)))
        }
        _ => None,
    }
}

/// Print a single update statement for a `for` header (no trailing semicolon).
fn print_for_update(stmt: &JsStmt) -> Option<String> {
    match stmt {
        JsStmt::CompoundAssign { target, op, value } => Some(format!(
            "{} {}= {}",
            print_expr(target),
            binop_str(*op),
            print_expr(value),
        )),
        JsStmt::Assign { target, value } => {
            Some(format!("{} = {}", print_expr(target), print_expr(value)))
        }
        JsStmt::Expr(expr) => Some(print_expr(expr)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Expression printing
// ---------------------------------------------------------------------------

fn print_expr(expr: &JsExpr) -> String {
    match expr {
        JsExpr::Literal(c) => emit_constant(c),

        JsExpr::Var(name) => sanitize_ident(name),

        JsExpr::This => "this".into(),

        JsExpr::Binary { op, lhs, rhs } => {
            format!(
                "{} {} {}",
                print_expr_operand(lhs),
                binop_str(*op),
                print_expr_operand(rhs),
            )
        }

        JsExpr::Unary { op, expr: inner } => {
            let op_str = match op {
                UnaryOp::Neg => "-",
                UnaryOp::BitNot => "~",
            };
            format!("{op_str}{}", print_expr_operand(inner))
        }

        JsExpr::Cmp { kind, lhs, rhs } => {
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
                print_expr_operand(lhs),
                print_expr_operand(rhs),
            )
        }

        JsExpr::Field { object, field } => {
            if is_valid_js_ident(field) {
                format!("{}.{field}", print_expr_operand(object))
            } else {
                format!(
                    "{}[\"{}\"]",
                    print_expr_operand(object),
                    escape_js_string(field),
                )
            }
        }

        JsExpr::Index { collection, index } => {
            // An inline object literal used as a map (e.g. `{ k: v }[key]`) has no index
            // signature, causing TS7053 when the key is `any`-typed. Cast to Record so
            // the lookup is well-typed.
            let coll_str = if matches!(collection.as_ref(), JsExpr::ObjectInit(_)) {
                format!("({} as Record<string, unknown>)", print_expr(collection))
            } else {
                print_expr_operand(collection)
            };
            format!("{coll_str}[{}]", print_expr(index))
        }

        JsExpr::Call { callee, args } => {
            let args_str: Vec<_> = args.iter().map(print_expr).collect();
            format!(
                "{}({})",
                print_expr_operand(callee),
                args_str.join(", "),
            )
        }

        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            format!(
                "{} ? {} : {}",
                print_expr_operand(cond),
                print_expr_operand(then_val),
                print_expr_operand(else_val),
            )
        }

        JsExpr::LogicalOr { lhs, rhs } => {
            format!(
                "{} || {}",
                print_expr_operand(lhs),
                print_expr_operand(rhs),
            )
        }

        JsExpr::LogicalAnd { lhs, rhs } => {
            format!(
                "{} && {}",
                print_expr_operand(lhs),
                print_expr_operand(rhs),
            )
        }

        JsExpr::Cast { expr: inner, ty, kind } => {
            match (kind, ty) {
                // AsType + Struct/Enum → asType(x, Foo) (runtime null-on-failure).
                (CastKind::AsType, Type::Struct(name) | Type::Enum(name)) => {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    format!("asType({}, {})", print_expr(inner), sanitize_ident(short))
                }
                // Coerce + Struct/Enum → TS assertion (compiler-guaranteed).
                (CastKind::Coerce, Type::Struct(name) | Type::Enum(name)) => {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    format!("{} as {}", print_expr_operand(inner), sanitize_ident(short))
                }
                // Coerce + Float → Number(x).
                (CastKind::Coerce, Type::Float(_)) => {
                    format!("Number({})", print_expr(inner))
                }
                // Coerce + Int(32) → int(x).
                (CastKind::Coerce, Type::Int(32)) => {
                    format!("int({})", print_expr(inner))
                }
                // Coerce + UInt(32) → uint(x).
                (CastKind::Coerce, Type::UInt(32)) => {
                    format!("uint({})", print_expr(inner))
                }
                // Coerce + String → String(x).
                (CastKind::Coerce, Type::String) => {
                    format!("String({})", print_expr(inner))
                }
                // Coerce + Bool → Boolean(x).
                (CastKind::Coerce, Type::Bool) => {
                    format!("Boolean({})", print_expr(inner))
                }
                // Coerce + Dynamic/other → passthrough (no-op).
                (CastKind::Coerce, _) => print_expr(inner),
                // AsType + Dynamic/other → passthrough (no-op).
                (CastKind::AsType, Type::Dynamic) => print_expr(inner),
                // AsType + Primitive → TS type assertion.
                (CastKind::AsType, _) => {
                    format!("{} as {}", print_expr_operand(inner), ts_type(ty))
                }
            }
        }

        JsExpr::TypeCheck { expr: inner, ty } => print_type_check(inner, ty),

        JsExpr::ArrayInit(elems) => {
            let elems_str: Vec<_> = elems.iter().map(print_expr).collect();
            format!("[{}]", elems_str.join(", "))
        }

        JsExpr::ObjectInit(pairs) => {
            if pairs.is_empty() {
                return "{}".to_string();
            }
            let field_strs: Vec<_> = pairs
                .iter()
                .map(|(name, val)| {
                    if name == "..." {
                        // Spread entry: emit `...expr`
                        format!("...{}", print_expr_operand(val))
                    } else if is_valid_js_ident(name) {
                        format!("{name}: {}", print_expr(val))
                    } else {
                        format!("\"{}\": {}", escape_js_string(name), print_expr(val))
                    }
                })
                .collect();
            format!("{{ {} }}", field_strs.join(", "))
        }

        JsExpr::TupleInit(elems) => {
            let elems_str: Vec<_> = elems.iter().map(print_expr).collect();
            format!("[{}]", elems_str.join(", "))
        }

        JsExpr::Not(inner) => {
            format!("!{}", print_expr_operand(inner))
        }

        JsExpr::PostIncrement(inner) => {
            format!("{}++", print_expr_operand(inner))
        }

        JsExpr::Spread(inner) => {
            format!("...{}", print_expr_operand(inner))
        }

        JsExpr::GeneratorCreate { func: fname, args } => {
            let args_str: Vec<_> = args.iter().map(print_expr).collect();
            format!("{}({})", sanitize_ident(fname), args_str.join(", "))
        }

        JsExpr::GeneratorResume(inner) => {
            format!("{}.next()", print_expr(inner))
        }

        JsExpr::Yield(v) => {
            if let Some(inner) = v {
                format!("yield {}", print_expr(inner))
            } else {
                "yield".into()
            }
        }

        // --- JS-specific constructs ---
        JsExpr::New { callee, args } => {
            let args_str: Vec<_> = args.iter().map(print_expr).collect();
            // Parenthesize call-expression callees: `new (f(a,b))()` not `new f(a,b)()`
            // because `new f(a,b)()` parses as `(new f(a,b))()` in JS.
            let callee_str = match callee.as_ref() {
                JsExpr::Call { .. } | JsExpr::SystemCall { .. } => {
                    format!("({})", print_expr(callee))
                }
                _ => print_expr(callee),
            };
            format!("new {}({})", callee_str, args_str.join(", "))
        }

        JsExpr::TypeOf(inner) => {
            format!("typeof {}", print_expr_operand(inner))
        }

        JsExpr::In { key, object } => {
            format!(
                "{} in {}",
                print_expr_operand(key),
                print_expr_operand(object),
            )
        }

        JsExpr::Delete { object, key } => {
            format!("delete {}[{}]", print_expr_operand(object), print_expr(key))
        }

        JsExpr::SuperCall(args) => {
            let args_str: Vec<_> = args.iter().map(print_expr).collect();
            format!("super({})", args_str.join(", "))
        }

        JsExpr::SuperMethodCall { method, args } => {
            let args_str: Vec<_> = args.iter().map(print_expr).collect();
            format!("super.{}({})", sanitize_ident(method), args_str.join(", "))
        }

        JsExpr::SuperGet(prop) => {
            format!("super.{}", sanitize_ident(prop))
        }

        JsExpr::SuperSet { prop, value } => {
            format!(
                "(super.{} = {})",
                sanitize_ident(prop),
                print_expr(value),
            )
        }

        JsExpr::Activation => "({})".to_string(),

        JsExpr::ArrowFunction {
            params,
            return_ty,
            body,
            has_rest_param,
            cast_as,
            infer_param_types,
        } => {
            let params_str = print_params(params, *has_rest_param, *infer_param_types);
            let ret_ty = ts_type(return_ty);
            let mut out = format!("({params_str}): {ret_ty} => {{\n");
            print_stmts(body, &mut out, "  ");
            out.push('}');
            if let Some(cast) = cast_as {
                out = format!("({out}) as {cast}");
            }
            out
        }

        // --- Fallback: unmapped system call ---
        JsExpr::SystemCall {
            system,
            method,
            args,
        } => {
            let args_str: Vec<_> = args.iter().map(print_expr).collect();
            let sys_ident = sanitize_ident(system);
            let safe_method = if is_valid_js_ident(method) {
                format!(".{method}")
            } else {
                format!("[\"{}\"]", escape_js_string(method))
            };
            format!("{sys_ident}{safe_method}({})", args_str.join(", "))
        }
    }
}

/// Print an expression as an operand (may need parenthesization).
fn print_expr_operand(expr: &JsExpr) -> String {
    if needs_parens(expr) {
        format!("({})", print_expr(expr))
    } else {
        print_expr(expr)
    }
}

/// Whether an expression needs parentheses when used as an operand.
fn needs_parens(expr: &JsExpr) -> bool {
    match expr {
        // Function-call forms (asType, Number, int, etc.) don't need parens.
        // Only `x as T` forms need them.
        JsExpr::Cast { ty, kind, .. } => match (kind, ty) {
            (CastKind::AsType, Type::Struct(_) | Type::Enum(_)) => false,
            (CastKind::Coerce, Type::Struct(_) | Type::Enum(_)) => true,  // `x as Foo`
            (CastKind::Coerce, Type::Float(_) | Type::Int(32) | Type::UInt(32) | Type::String | Type::Bool) => false,
            (CastKind::Coerce, _) => false,  // passthrough
            (CastKind::AsType, Type::Dynamic) => false,  // passthrough
            (CastKind::AsType, _) => true,  // `x as T`
        },
        JsExpr::Binary { .. }
            | JsExpr::Cmp { .. }
            | JsExpr::Ternary { .. }
            | JsExpr::LogicalOr { .. }
            | JsExpr::LogicalAnd { .. }
            | JsExpr::Unary { .. }
            | JsExpr::Not(_)
            | JsExpr::In { .. }
            | JsExpr::SuperSet { .. }
            | JsExpr::ArrowFunction { .. } => true,
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Type check printing
// ---------------------------------------------------------------------------

fn print_type_check(expr: &JsExpr, ty: &Type) -> String {
    let operand = print_expr_operand(expr);
    match ty {
        Type::Bool => format!("typeof {operand} === \"boolean\""),
        Type::Int(_) | Type::UInt(_) | Type::Float(_) => {
            format!("typeof {operand} === \"number\"")
        }
        Type::String => format!("typeof {operand} === \"string\""),
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            // Use isType() instead of instanceof — works for both classes and interfaces.
            format!("isType({}, {})", print_expr(expr), sanitize_ident(short))
        }
        Type::Union(types) => {
            let checks: Vec<_> = types
                .iter()
                .map(|t| print_type_check(expr, t))
                .collect();
            format!("({})", checks.join(" || "))
        }
        _ => format!("typeof {operand} === \"object\""),
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn is_null_literal(expr: &JsExpr) -> bool {
    matches!(expr, JsExpr::Literal(Constant::Null))
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
        CmpKind::LooseEq => "==",
        CmpKind::LooseNe => "!=",
    }
}

pub fn is_valid_js_ident(name: &str) -> bool {
    !name.is_empty()
        && !name.starts_with(|c: char| c.is_ascii_digit())
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
}

pub(crate) fn emit_constant(c: &Constant) -> String {
    match c {
        Constant::Null => "null".into(),
        Constant::Bool(b) => b.to_string(),
        Constant::Int(n) => n.to_string(),
        Constant::UInt(n) => n.to_string(),
        Constant::Float(f) => format_float(*f),
        Constant::String(s) => {
            if s.contains('\n') {
                format!("`{}`", escape_js_template(s))
            } else {
                format!("\"{}\"", escape_js_string(s))
            }
        }
    }
}

fn format_float(f: f64) -> String {
    if f.fract() == 0.0 && f.is_finite() {
        format!("{f:.1}")
    } else {
        format!("{f}")
    }
}

pub fn escape_js_string(s: &str) -> String {
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

/// Escape a string for use inside a JS template literal (backtick-quoted).
/// Newlines are preserved literally; backticks and `${` are escaped.
fn escape_js_template(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '`' => out.push_str("\\`"),
            '$' if chars.peek() == Some(&'{') => {
                chars.next();
                out.push_str("\\${");
            }
            '\r' => {
                // Normalize \r\n to \n, drop bare \r
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }
                out.push('\n');
            }
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
