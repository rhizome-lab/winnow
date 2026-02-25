//! GameMaker-specific JsExpr → JsExpr rewrite pass.
//!
//! Resolves GameMaker SystemCall nodes into native JavaScript constructs.
//! Much simpler than Flash — only 8 SystemCall patterns to handle.

use std::collections::{BTreeMap, HashMap};

use reincarnate_core::ir::value::Constant;
use reincarnate_core::ir::{CmpKind, ExternalImport, Type, ValueId};

use crate::emit::{ClassRegistry, RefSets};
use crate::js_ast::{JsExpr, JsFunction, JsStmt};

/// Build `ObjName.instances[0]!` — non-null asserted singleton instance access.
fn instances_0(obj_name: String) -> JsExpr {
    JsExpr::NonNull(Box::new(JsExpr::Index {
        collection: Box::new(JsExpr::Field {
            object: Box::new(JsExpr::Var(obj_name)),
            field: "instances".into(),
        }),
        index: Box::new(JsExpr::Literal(Constant::Int(0))),
    }))
}

/// Returns the stateful runtime names that a direct `Op::Call` rewrite will
/// introduce, if any.  Used by import generation to ensure these names are
/// destructured from `this._rt` before the rewrite pass runs.
///
/// For example, `@@Other@@()` rewrites to `other` (a destructured field), and
/// `@@Global@@()` rewrites to `global`.  Without this hook the scanner would
/// only see `"@@Other@@"` / `"@@Global@@"` which don't map to any module entry.
pub fn rewrite_introduced_direct_calls(func_name: &str) -> &'static [&'static str] {
    match func_name {
        "@@Global@@" => &["global"],
        "@@Other@@" => &["other"],
        _ => &[],
    }
}

/// Returns the bare function names that a SystemCall rewrite will introduce,
/// if any.  Used by import generation to emit the correct imports before
/// the rewrite pass runs.
pub fn rewrite_introduced_calls(system: &str, method: &str) -> &'static [&'static str] {
    match (system, method) {
        ("GameMaker.Global", "set") | ("GameMaker.Global", "get") => &["global"],
        ("GameMaker.Instance", "getOn") => &["getInstanceField"],
        ("GameMaker.Instance", "setOn") => &["setInstanceField", "setInstanceFieldIndex"],
        ("GameMaker.Instance", "getAll") => &["getAllField"],
        ("GameMaker.Instance", "setAll") => &["setAllField"],
        ("GameMaker.Instance", "setField") => &["setInstanceField"],
        ("GameMaker.Instance", "withInstances") => &["withInstances"],
        // getOther/setOther rewrite to expressions using `other` (the collision partner).
        ("GameMaker.Instance", "getOther") => &["other"],
        ("GameMaker.Instance", "setOther") => &["other", "setOtherField"],
        _ => &[],
    }
}

/// Rewrite a function's body, resolving GameMaker SystemCalls.
///
/// `event_name` is the method name of the enclosing event handler (e.g.
/// `"create"`, `"step"`, `"draw"`), used to rewrite `event_inherited()` to
/// `super.eventName()`.  Pass `None` for free functions.
pub fn rewrite_gamemaker_function(
    mut func: JsFunction,
    sprite_names: &[String],
    closure_bodies: &HashMap<String, JsFunction>,
    event_name: Option<&str>,
) -> JsFunction {
    rewrite_stmts(&mut func.body, sprite_names, closure_bodies, event_name);
    func
}

fn rewrite_stmts(
    stmts: &mut Vec<JsStmt>,
    sprite_names: &[String],
    closure_bodies: &HashMap<String, JsFunction>,
    event_name: Option<&str>,
) {
    for stmt in stmts.iter_mut() {
        rewrite_stmt(stmt, sprite_names, closure_bodies, event_name);
    }
    // Remove no-op expression statements produced by GML internal function rewrites:
    //   @@Global@@() → global; (standalone call is a no-op)
    //   @@Other@@()  → other;  (standalone call is a no-op)
    stmts.retain(|s| !is_gml_noop_stmt(s));
}

/// Returns true for expression-statement no-ops introduced by rewriting GML
/// internal built-in calls (`@@Global@@`, `@@Other@@`).
fn is_gml_noop_stmt(stmt: &JsStmt) -> bool {
    matches!(stmt, JsStmt::Expr(JsExpr::Var(v)) if v == "global" || v == "other")
}

fn rewrite_stmt(
    stmt: &mut JsStmt,
    sprite_names: &[String],
    closure_bodies: &HashMap<String, JsFunction>,
    event_name: Option<&str>,
) {
    match stmt {
        JsStmt::VarDecl { init, .. } => {
            if let Some(expr) = init {
                rewrite_expr(expr, sprite_names, closure_bodies, event_name);
            }
        }
        JsStmt::Assign { target, value } => {
            rewrite_expr(target, sprite_names, closure_bodies, event_name);
            rewrite_expr(value, sprite_names, closure_bodies, event_name);
            try_resolve_sprite_assign(target, value, sprite_names);
        }
        JsStmt::CompoundAssign { target, value, .. } => {
            rewrite_expr(target, sprite_names, closure_bodies, event_name);
            rewrite_expr(value, sprite_names, closure_bodies, event_name);
            try_resolve_sprite_assign(target, value, sprite_names);
        }
        JsStmt::Expr(e) => {
            // Intercept global set with constant key → global.field = val
            if let JsExpr::SystemCall {
                system,
                method,
                args,
            } = e
            {
                if system == "GameMaker.Global"
                    && method == "set"
                    && args.len() == 2
                    && matches!(&args[0], JsExpr::Literal(Constant::String(_)))
                {
                    let mut args = std::mem::take(args);
                    let mut val = args.pop().unwrap();
                    let name_expr = args.pop().unwrap();
                    let JsExpr::Literal(Constant::String(field_name)) = name_expr else {
                        unreachable!()
                    };
                    rewrite_expr(&mut val, sprite_names, closure_bodies, event_name);
                    *stmt = JsStmt::Assign {
                        target: JsExpr::Field {
                            object: Box::new(JsExpr::Var("global".into())),
                            field: field_name,
                        },
                        value: val,
                    };
                    return;
                }
            }
            // Intercept setOn with named object at statement level → direct assignment.
            if let JsExpr::SystemCall {
                system,
                method,
                args,
            } = e
            {
                if system == "GameMaker.Instance"
                    && method == "setOn"
                    && matches!(&args[0], JsExpr::Literal(Constant::String(_)))
                    && matches!(&args[1], JsExpr::Literal(Constant::String(_)))
                {
                    match args.len() {
                        // setOn(obj, field, value) → Obj.instances[0].field = value
                        3 => {
                            let mut args = std::mem::take(args);
                            let val = args.pop().unwrap();
                            let field = args.pop().unwrap();
                            let obj_name_expr = args.pop().unwrap();
                            let JsExpr::Literal(Constant::String(obj_name)) = obj_name_expr
                            else {
                                unreachable!()
                            };
                            let JsExpr::Literal(Constant::String(field_name)) = field else {
                                unreachable!()
                            };
                            let target = JsExpr::Field {
                                object: Box::new(instances_0(obj_name)),
                                field: field_name,
                            };
                            let mut val = val;
                            rewrite_expr(&mut val, sprite_names, closure_bodies, event_name);
                            *stmt = JsStmt::Assign { target, value: val };
                            return;
                        }
                        // setOn(obj, field, index, value) → Obj.instances[0]!.field[index] = value
                        4 => {
                            let mut args = std::mem::take(args);
                            let val = args.pop().unwrap();
                            let mut index_expr = args.pop().unwrap();
                            let field = args.pop().unwrap();
                            let obj_name_expr = args.pop().unwrap();
                            let JsExpr::Literal(Constant::String(obj_name)) = obj_name_expr
                            else {
                                unreachable!()
                            };
                            let JsExpr::Literal(Constant::String(field_name)) = field else {
                                unreachable!()
                            };
                            rewrite_expr(&mut index_expr, sprite_names, closure_bodies, event_name);
                            let target = JsExpr::Index {
                                collection: Box::new(JsExpr::Field {
                                    object: Box::new(instances_0(obj_name)),
                                    field: field_name,
                                }),
                                index: Box::new(index_expr),
                            };
                            let mut val = val;
                            rewrite_expr(&mut val, sprite_names, closure_bodies, event_name);
                            *stmt = JsStmt::Assign { target, value: val };
                            return;
                        }
                        _ => {}
                    }
                }
            }
            rewrite_expr(e, sprite_names, closure_bodies, event_name);
        }
        JsStmt::Return(Some(e)) => rewrite_expr(e, sprite_names, closure_bodies, event_name),
        JsStmt::Return(None) => {}
        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            rewrite_expr(cond, sprite_names, closure_bodies, event_name);
            rewrite_stmts(then_body, sprite_names, closure_bodies, event_name);
            rewrite_stmts(else_body, sprite_names, closure_bodies, event_name);
        }
        JsStmt::While { cond, body } => {
            rewrite_expr(cond, sprite_names, closure_bodies, event_name);
            rewrite_stmts(body, sprite_names, closure_bodies, event_name);
        }
        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => {
            rewrite_stmts(init, sprite_names, closure_bodies, event_name);
            rewrite_expr(cond, sprite_names, closure_bodies, event_name);
            rewrite_stmts(update, sprite_names, closure_bodies, event_name);
            rewrite_stmts(body, sprite_names, closure_bodies, event_name);
        }
        JsStmt::Loop { body } => {
            rewrite_stmts(body, sprite_names, closure_bodies, event_name);
        }
        JsStmt::ForOf { iterable, body, .. } => {
            rewrite_expr(iterable, sprite_names, closure_bodies, event_name);
            rewrite_stmts(body, sprite_names, closure_bodies, event_name);
        }
        JsStmt::Throw(e) => rewrite_expr(e, sprite_names, closure_bodies, event_name),
        JsStmt::Dispatch { blocks, .. } => {
            for (_, stmts) in blocks {
                rewrite_stmts(stmts, sprite_names, closure_bodies, event_name);
            }
        }
        JsStmt::Switch {
            value,
            cases,
            default_body,
        } => {
            rewrite_expr(value, sprite_names, closure_bodies, event_name);
            for (_, stmts) in cases {
                rewrite_stmts(stmts, sprite_names, closure_bodies, event_name);
            }
            rewrite_stmts(default_body, sprite_names, closure_bodies, event_name);
        }
        JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => {}
    }
}

/// If `target` is a `Field { field: "sprite_index", .. }` and `value` is a
/// `Literal(Int(idx))` within range, replace the value with `Sprites.Name`.
fn try_resolve_sprite_assign(target: &JsExpr, value: &mut JsExpr, sprite_names: &[String]) {
    if sprite_names.is_empty() {
        return;
    }
    let is_sprite_field = matches!(target, JsExpr::Field { field, .. } if field == "sprite_index");
    if !is_sprite_field {
        return;
    }
    if let JsExpr::Literal(Constant::Int(idx)) = value {
        let idx = *idx as usize;
        if let Some(name) = sprite_names.get(idx) {
            let sprites = Box::new(JsExpr::Var("Sprites".into()));
            *value = if crate::ast_printer::is_valid_js_ident(name) {
                JsExpr::Field { object: sprites, field: name.clone() }
            } else {
                JsExpr::Index {
                    collection: sprites,
                    index: Box::new(JsExpr::Literal(Constant::String(name.clone()))),
                }
            };
        }
    }
}

fn rewrite_expr(
    expr: &mut JsExpr,
    sprite_names: &[String],
    closure_bodies: &HashMap<String, JsFunction>,
    event_name: Option<&str>,
) {
    // Early-exit: rewrite GML internal built-in calls before child recursion.
    // These replace the whole expression so we handle arg recursion manually.
    //
    // Note: JsExpr::Var stores the original GML name with `@@` delimiters.
    // `sanitize_ident` is only applied at print time (ast_printer.rs), so we
    // match against the raw form here.
    if let JsExpr::Call { callee, args } = expr {
        if let JsExpr::Var(name) = callee.as_ref() {
            match name.as_str() {
                // @@NewGMLArray@@(v0, v1, ...) → [v0, v1, ...]
                "@@NewGMLArray@@" => {
                    for arg in args.iter_mut() {
                        rewrite_expr(arg, sprite_names, closure_bodies, event_name);
                    }
                    *expr = JsExpr::ArrayInit(std::mem::take(args));
                    return;
                }
                // @@NewGMLObject@@() → {}
                "@@NewGMLObject@@" => {
                    *expr = JsExpr::ObjectInit(vec![]);
                    return;
                }
                // @@Global@@() → global
                "@@Global@@" if args.is_empty() => {
                    *expr = JsExpr::Var("global".into());
                    return;
                }
                // @@Other@@() → other
                "@@Other@@" if args.is_empty() => {
                    *expr = JsExpr::Var("other".into());
                    return;
                }
                // event_inherited() → super.eventName()
                "event_inherited" => {
                    if let Some(method) = event_name {
                        *expr = JsExpr::SuperMethodCall {
                            method: method.to_string(),
                            args: vec![],
                        };
                        return;
                    }
                }
                // array_length(arr) → arr.length
                "array_length" if args.len() == 1 => {
                    let mut arr = args.remove(0);
                    rewrite_expr(&mut arr, sprite_names, closure_bodies, event_name);
                    *expr = JsExpr::Field {
                        object: Box::new(arr),
                        field: "length".into(),
                    };
                    return;
                }
                // array_push(arr, v0, v1, ...) → arr.push(v0, v1, ...)
                "array_push" if !args.is_empty() => {
                    for arg in args.iter_mut() {
                        rewrite_expr(arg, sprite_names, closure_bodies, event_name);
                    }
                    let mut args = std::mem::take(args);
                    let arr = args.remove(0);
                    *expr = JsExpr::Call {
                        callee: Box::new(JsExpr::Field {
                            object: Box::new(arr),
                            field: "push".into(),
                        }),
                        args,
                    };
                    return;
                }
                // show_debug_message(msg) → console.log(msg)
                "show_debug_message" => {
                    for arg in args.iter_mut() {
                        rewrite_expr(arg, sprite_names, closure_bodies, event_name);
                    }
                    *expr = JsExpr::Call {
                        callee: Box::new(JsExpr::Field {
                            object: Box::new(JsExpr::Var("console".into())),
                            field: "log".into(),
                        }),
                        args: std::mem::take(args),
                    };
                    return;
                }
                // is_struct(val) → (typeof (val) === "object" && (val) !== null)
                "is_struct" if args.len() == 1 => {
                    let mut val = args.remove(0);
                    rewrite_expr(&mut val, sprite_names, closure_bodies, event_name);
                    let val_box = Box::new(val);
                    *expr = JsExpr::LogicalAnd {
                        lhs: Box::new(JsExpr::Cmp {
                            kind: CmpKind::Eq,
                            lhs: Box::new(JsExpr::TypeOf(val_box.clone())),
                            rhs: Box::new(JsExpr::Literal(
                                Constant::String("object".into()),
                            )),
                        }),
                        rhs: Box::new(JsExpr::Cmp {
                            kind: CmpKind::Ne,
                            lhs: val_box,
                            rhs: Box::new(JsExpr::Literal(Constant::Null)),
                        }),
                    };
                    return;
                }
                _ => {}
            }
        }
    }
    // Rewrite bare variable references for GML built-ins used as values
    // (not in function-call position, e.g. `this.altDestinationObject = @@Global@@`).
    if let JsExpr::Var(name) = expr {
        match name.as_str() {
            "@@Global@@" => {
                *expr = JsExpr::Var("global".into());
                return;
            }
            "@@Other@@" => {
                *expr = JsExpr::Var("other".into());
                return;
            }
            _ => {}
        }
    }

    // Normal path: recurse into children first, then attempt SystemCall resolution.
    rewrite_expr_children(expr, sprite_names, closure_bodies, event_name);

    // Then, attempt to resolve SystemCall patterns.
    let replacement = match expr {
        JsExpr::SystemCall {
            system,
            method,
            args,
        } => try_rewrite_system_call(system, method, args, closure_bodies),
        _ => None,
    };

    if let Some(new_expr) = replacement {
        *expr = new_expr;
        // Re-recurse into the replacement's children so that SystemCalls
        // inside an inlined closure body (ArrowFunction) are also rewritten.
        rewrite_expr_children(expr, sprite_names, closure_bodies, event_name);
    }
}

fn rewrite_expr_children(
    expr: &mut JsExpr,
    sprite_names: &[String],
    closure_bodies: &HashMap<String, JsFunction>,
    event_name: Option<&str>,
) {
    match expr {
        JsExpr::Binary { lhs, rhs, .. } | JsExpr::Cmp { lhs, rhs, .. } => {
            rewrite_expr(lhs, sprite_names, closure_bodies, event_name);
            rewrite_expr(rhs, sprite_names, closure_bodies, event_name);
        }
        JsExpr::LogicalOr { lhs, rhs } | JsExpr::LogicalAnd { lhs, rhs } => {
            rewrite_expr(lhs, sprite_names, closure_bodies, event_name);
            rewrite_expr(rhs, sprite_names, closure_bodies, event_name);
        }
        JsExpr::Unary { expr: inner, .. } => rewrite_expr(inner, sprite_names, closure_bodies, event_name),
        JsExpr::Not(inner) | JsExpr::PostIncrement(inner) | JsExpr::Spread(inner) => rewrite_expr(inner, sprite_names, closure_bodies, event_name),
        JsExpr::Field { object, .. } => rewrite_expr(object, sprite_names, closure_bodies, event_name),
        JsExpr::Index { collection, index } => {
            rewrite_expr(collection, sprite_names, closure_bodies, event_name);
            rewrite_expr(index, sprite_names, closure_bodies, event_name);
        }
        JsExpr::Call { callee, args } => {
            rewrite_expr(callee, sprite_names, closure_bodies, event_name);
            for arg in args {
                rewrite_expr(arg, sprite_names, closure_bodies, event_name);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            rewrite_expr(cond, sprite_names, closure_bodies, event_name);
            rewrite_expr(then_val, sprite_names, closure_bodies, event_name);
            rewrite_expr(else_val, sprite_names, closure_bodies, event_name);
        }
        JsExpr::ArrayInit(items) | JsExpr::TupleInit(items) => {
            for item in items {
                rewrite_expr(item, sprite_names, closure_bodies, event_name);
            }
        }
        JsExpr::ObjectInit(fields) => {
            for (_, val) in fields {
                rewrite_expr(val, sprite_names, closure_bodies, event_name);
            }
        }
        JsExpr::New { callee, args } => {
            rewrite_expr(callee, sprite_names, closure_bodies, event_name);
            for arg in args {
                rewrite_expr(arg, sprite_names, closure_bodies, event_name);
            }
        }
        JsExpr::TypeOf(inner) => rewrite_expr(inner, sprite_names, closure_bodies, event_name),
        JsExpr::In { key, object } => {
            rewrite_expr(key, sprite_names, closure_bodies, event_name);
            rewrite_expr(object, sprite_names, closure_bodies, event_name);
        }
        JsExpr::Delete { object, key } => {
            rewrite_expr(object, sprite_names, closure_bodies, event_name);
            rewrite_expr(key, sprite_names, closure_bodies, event_name);
        }
        JsExpr::Cast { expr: inner, .. } | JsExpr::TypeCheck { expr: inner, .. } => {
            rewrite_expr(inner, sprite_names, closure_bodies, event_name)
        }
        // Arrow functions are closures: `event_inherited` inside a closure is
        // not meaningful (closures don't have a "current event"), so pass None.
        JsExpr::ArrowFunction { body, .. } => rewrite_stmts(body, sprite_names, closure_bodies, None),
        JsExpr::SuperCall(args) | JsExpr::SuperMethodCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg, sprite_names, closure_bodies, event_name);
            }
        }
        JsExpr::SuperGet(_) => {}
        JsExpr::SuperSet { value, .. } => rewrite_expr(value, sprite_names, closure_bodies, event_name),
        JsExpr::GeneratorCreate { args, .. } => {
            for arg in args {
                rewrite_expr(arg, sprite_names, closure_bodies, event_name);
            }
        }
        JsExpr::GeneratorResume(inner) => rewrite_expr(inner, sprite_names, closure_bodies, event_name),
        JsExpr::Yield(inner) => {
            if let Some(e) = inner {
                rewrite_expr(e, sprite_names, closure_bodies, event_name);
            }
        }
        JsExpr::NonNull(inner) => rewrite_expr(inner, sprite_names, closure_bodies, event_name),
        JsExpr::Activation => {}
        JsExpr::SystemCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg, sprite_names, closure_bodies, event_name);
            }
        }
        // Leaf nodes — nothing to recurse into.
        JsExpr::Literal(_) | JsExpr::Var(_) | JsExpr::This => {}
    }
}

/// Try to rewrite a GameMaker SystemCall into a native JS expression.
fn try_rewrite_system_call(
    system: &str,
    method: &str,
    args: &mut Vec<JsExpr>,
    closure_bodies: &HashMap<String, JsFunction>,
) -> Option<JsExpr> {
    match (system, method) {
        // SugarCube.Engine.closure(name[, cap0, cap1, ...]) → arrow function or IIFE
        //
        // This is the IR encoding emitted by lower.rs for Op::MakeClosure.
        // For GML with-bodies: no-capture case produces a plain arrow function;
        // the capture case produces ((cap0, ...) => (_self) => body)(cap_val0, ...).
        ("SugarCube.Engine", "closure") if !args.is_empty() => {
            let JsExpr::Literal(Constant::String(ref name)) = args[0] else {
                return None;
            };
            let short_name = name.rsplit("::").next().unwrap_or(name.as_str());
            let closure_func = closure_bodies.get(short_name).cloned()?;
            let n_cap = closure_func.num_capture_params;
            let n_total = closure_func.params.len();
            let n_reg = n_total.saturating_sub(n_cap);
            if n_cap == 0 || args.len() == 1 {
                // No captures: plain arrow function.
                return Some(JsExpr::ArrowFunction {
                    params: closure_func.params,
                    return_ty: closure_func.return_ty,
                    body: closure_func.body,
                    has_rest_param: closure_func.has_rest_param,
                    cast_as: None,
                    infer_param_types: false,
                });
            }
            // IIFE for captures: ((cap0, ...) => (_self) => body)(cap_val0, ...)
            let mut all_params = closure_func.params;
            let cap_params: Vec<(String, Type)> = all_params
                .split_off(n_reg)
                .into_iter()
                .map(|(n, _)| (n, Type::Dynamic))
                .collect();
            let reg_params = all_params;
            let cap_vals: Vec<JsExpr> = args.drain(1..).collect();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::ArrowFunction {
                    params: cap_params,
                    return_ty: Type::Dynamic,
                    body: vec![crate::js_ast::JsStmt::Return(Some(JsExpr::ArrowFunction {
                        params: reg_params,
                        return_ty: closure_func.return_ty,
                        body: closure_func.body,
                        has_rest_param: closure_func.has_rest_param,
                        cast_as: None,
                        infer_param_types: false,
                    }))],
                    has_rest_param: false,
                    cast_as: None,
                    infer_param_types: false,
                }),
                args: cap_vals,
            })
        }
        // GameMaker.Instance.withInstances(target, closure) → withInstances(target, closure)
        //
        // At this point the closure arg has already been rewritten (children-first)
        // from SugarCube.Engine.closure(...) → ArrowFunction.
        ("GameMaker.Instance", "withInstances") if args.len() == 2 => {
            let callback = args.pop().unwrap();
            let target = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("withInstances".into())),
                args: vec![target, callback],
            })
        }
        // GameMaker.Global.set(name, val) → variable_global_set(name, val)
        // Constant-key sets are intercepted at statement level (→ global.name = val).
        // This handles the expression-position fallback and dynamic keys.
        ("GameMaker.Global", "set") if args.len() == 2 => {
            let val = args.pop().unwrap();
            let name = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("variable_global_set".into())),
                args: vec![name, val],
            })
        }
        // GameMaker.Global.get(name) → global.name (constant key)
        // or variable_global_get(name) (dynamic key fallback)
        ("GameMaker.Global", "get") if args.len() == 1 => {
            let name = args.pop().unwrap();
            if let JsExpr::Literal(Constant::String(field_name)) = name {
                Some(JsExpr::Field {
                    object: Box::new(JsExpr::Var("global".into())),
                    field: field_name,
                })
            } else {
                Some(JsExpr::Call {
                    callee: Box::new(JsExpr::Var("variable_global_get".into())),
                    args: vec![name],
                })
            }
        }
        // GameMaker.Instance.getOn(objName, field) → ObjName.instances[0]!.field
        // GameMaker.Instance.getOn(objId, field)   → getInstanceField(objId, field)
        ("GameMaker.Instance", "getOn") if args.len() == 2 => {
            let field = args.pop().unwrap();
            let obj_id = args.pop().unwrap();
            if let JsExpr::Literal(Constant::String(ref obj_name)) = obj_id {
                if let JsExpr::Literal(Constant::String(ref field_name)) = field {
                    Some(JsExpr::Field {
                        object: Box::new(instances_0(obj_name.clone())),
                        field: field_name.clone(),
                    })
                } else {
                    Some(JsExpr::Index {
                        collection: Box::new(instances_0(obj_name.clone())),
                        index: Box::new(field),
                    })
                }
            } else {
                Some(JsExpr::Call {
                    callee: Box::new(JsExpr::Var("getInstanceField".into())),
                    args: vec![obj_id, field],
                })
            }
        }
        // GameMaker.Instance.getOn(objName, field, index) → ObjName.instances[0]!.field[index]
        // GameMaker.Instance.getOn(objId, field, index)   → getInstanceField(objId, field)[index]
        ("GameMaker.Instance", "getOn") if args.len() == 3 => {
            let index = args.pop().unwrap();
            let field = args.pop().unwrap();
            let obj_id = args.pop().unwrap();
            let base = if let JsExpr::Literal(Constant::String(ref obj_name)) = obj_id {
                if let JsExpr::Literal(Constant::String(ref field_name)) = field {
                    JsExpr::Field {
                        object: Box::new(instances_0(obj_name.clone())),
                        field: field_name.clone(),
                    }
                } else {
                    JsExpr::Index {
                        collection: Box::new(instances_0(obj_name.clone())),
                        index: Box::new(field),
                    }
                }
            } else {
                JsExpr::Call {
                    callee: Box::new(JsExpr::Var("getInstanceField".into())),
                    args: vec![obj_id, field],
                }
            };
            Some(JsExpr::Index {
                collection: Box::new(base),
                index: Box::new(index),
            })
        }
        // GameMaker.Instance.setOn(objId, field, val) → setInstanceField(objId, field, val)
        // Named object+field case is handled at statement level (→ assignment).
        ("GameMaker.Instance", "setOn") if args.len() == 3 => {
            let val = args.pop().unwrap();
            let field = args.pop().unwrap();
            let obj_id = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("setInstanceField".into())),
                args: vec![obj_id, field, val],
            })
        }
        // GameMaker.Instance.setOn(objId, field, index, val) → setInstanceFieldIndex(objId, field, index, val)
        // Named object+field case is handled at statement level (→ indexed assignment).
        ("GameMaker.Instance", "setOn") if args.len() == 4 => {
            let val = args.pop().unwrap();
            let index = args.pop().unwrap();
            let field = args.pop().unwrap();
            let obj_id = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("setInstanceFieldIndex".into())),
                args: vec![obj_id, field, index, val],
            })
        }
        // GameMaker.Instance.getOther(field) → other[field]
        ("GameMaker.Instance", "getOther") if args.len() == 1 => {
            let field = args.pop().unwrap();
            Some(JsExpr::Index {
                collection: Box::new(JsExpr::Var("other".into())),
                index: Box::new(field),
            })
        }
        // GameMaker.Instance.setOther(field, val) → setOtherField(other, field, val)
        ("GameMaker.Instance", "setOther") if args.len() == 2 => {
            let val = args.pop().unwrap();
            let field = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("setOtherField".into())),
                args: vec![JsExpr::Var("other".into()), field, val],
            })
        }
        // GameMaker.Instance.getAll(field) → getAllField(field)
        ("GameMaker.Instance", "getAll") if args.len() == 1 => {
            let field = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("getAllField".into())),
                args: vec![field],
            })
        }
        // GameMaker.Instance.setAll(field, val) → setAllField(field, val)
        ("GameMaker.Instance", "setAll") if args.len() == 2 => {
            let val = args.pop().unwrap();
            let field = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("setAllField".into())),
                args: vec![field, val],
            })
        }
        // GameMaker.Instance.getField(target, field) → target[field]
        ("GameMaker.Instance", "getField") if args.len() == 2 => {
            let field = args.pop().unwrap();
            let target = args.pop().unwrap();
            Some(JsExpr::Index {
                collection: Box::new(target),
                index: Box::new(field),
            })
        }
        // GameMaker.Instance.setField(target, field, val) → setInstanceField(target, field, val)
        ("GameMaker.Instance", "setField") if args.len() == 3 => {
            let val = args.pop().unwrap();
            let field = args.pop().unwrap();
            let target = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("setInstanceField".into())),
                args: vec![target, field, val],
            })
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Import extraction for GameMaker instance SystemCalls
// ---------------------------------------------------------------------------

/// Collect import references from GameMaker.Instance getOn/setOn calls.
///
/// When the first argument is a const string naming an object class, the rewrite
/// pass produces `ObjName.instances[0].field` — so the object class needs a
/// value import.
pub(crate) fn collect_gamemaker_instance_refs(
    args: &[ValueId],
    const_strings: &HashMap<ValueId, &str>,
    self_name: &str,
    registry: &ClassRegistry,
    external_imports: &BTreeMap<String, ExternalImport>,
    refs: &mut RefSets,
) {
    if let Some(&obj_name) = args.first().and_then(|v| const_strings.get(v)) {
        if obj_name != self_name {
            if let Some(entry) = registry.lookup(obj_name) {
                refs.value_refs.insert(entry.short_name.clone());
            } else if external_imports.contains_key(obj_name) {
                refs.ext_value_refs.insert(obj_name.to_string());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rewrite_introduced_calls_maps_with_instances() {
        assert_eq!(
            rewrite_introduced_calls("GameMaker.Instance", "withInstances"),
            &["withInstances"]
        );
    }

    #[test]
    fn rewrite_introduced_calls_unknown_returns_empty() {
        assert_eq!(
            rewrite_introduced_calls("GameMaker.Instance", "withEnd"),
            &[] as &[&str]
        );
        assert_eq!(
            rewrite_introduced_calls("Unknown.System", "whatever"),
            &[] as &[&str]
        );
    }
}
