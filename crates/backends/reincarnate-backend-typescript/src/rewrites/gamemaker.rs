//! GameMaker-specific JsExpr → JsExpr rewrite pass.
//!
//! Resolves GameMaker SystemCall nodes into native JavaScript constructs.
//! Much simpler than Flash — only 8 SystemCall patterns to handle.

use reincarnate_core::ir::value::Constant;
use reincarnate_core::ir::Type;

use crate::js_ast::{JsExpr, JsFunction, JsStmt};

/// Rewrite a function's body, resolving GameMaker SystemCalls.
pub fn rewrite_gamemaker_function(mut func: JsFunction) -> JsFunction {
    rewrite_stmts(&mut func.body);
    func
}

fn rewrite_stmts(stmts: &mut Vec<JsStmt>) {
    for stmt in stmts.iter_mut() {
        rewrite_stmt(stmt);
    }
    collapse_with_blocks(stmts);
}

/// Detect `withBegin(target) ... withEnd()` bracket patterns and collapse them
/// into `withInstances(target, () => { ...body... })`.
fn collapse_with_blocks(stmts: &mut Vec<JsStmt>) {
    let mut i = 0;
    while i < stmts.len() {
        // Look for Expr(SystemCall("GameMaker.Instance", "withBegin", [target])).
        let is_with_begin = matches!(
            &stmts[i],
            JsStmt::Expr(JsExpr::SystemCall { system, method, args })
            if system == "GameMaker.Instance" && method == "withBegin" && args.len() == 1
        );
        if !is_with_begin {
            i += 1;
            continue;
        }

        // Find the matching withEnd at the same nesting level.
        let end_idx = stmts[i + 1..].iter().position(|s| {
            matches!(
                s,
                JsStmt::Expr(JsExpr::SystemCall { system, method, .. })
                if system == "GameMaker.Instance" && method == "withEnd"
            )
        });
        let Some(end_offset) = end_idx else {
            i += 1;
            continue;
        };
        let end_idx = i + 1 + end_offset;

        // Extract the target from withBegin.
        let JsStmt::Expr(JsExpr::SystemCall { args, .. }) = &mut stmts[i] else {
            unreachable!()
        };
        let target = args.pop().unwrap();

        // Drain the body statements between withBegin and withEnd.
        let body: Vec<JsStmt> = stmts.drain(i + 1..end_idx).collect();

        // Remove the withEnd statement (now at index i + 1 after drain).
        stmts.remove(i + 1);

        // Replace withBegin with withInstances(target, () => { body }).
        stmts[i] = JsStmt::Expr(JsExpr::SystemCall {
            system: "GameMaker.Instance".into(),
            method: "withInstances".into(),
            args: vec![
                target,
                JsExpr::ArrowFunction {
                    params: vec![],
                    return_ty: Type::Void,
                    body,
                    has_rest_param: false,
                    cast_as: None,
                },
            ],
        });

        i += 1;
    }
}

fn rewrite_stmt(stmt: &mut JsStmt) {
    match stmt {
        JsStmt::VarDecl { init, .. } => {
            if let Some(expr) = init {
                rewrite_expr(expr);
            }
        }
        JsStmt::Assign { target, value } => {
            rewrite_expr(target);
            rewrite_expr(value);
        }
        JsStmt::CompoundAssign { target, value, .. } => {
            rewrite_expr(target);
            rewrite_expr(value);
        }
        JsStmt::Expr(e) => {
            // Intercept setOn with named object at statement level → direct assignment.
            if let JsExpr::SystemCall {
                system,
                method,
                args,
            } = e
            {
                if system == "GameMaker.Instance"
                    && method == "setOn"
                    && args.len() == 3
                    && matches!(&args[0], JsExpr::Literal(Constant::String(_)))
                    && matches!(&args[1], JsExpr::Literal(Constant::String(_)))
                {
                    let mut args = std::mem::take(args);
                    let val = args.pop().unwrap();
                    let field = args.pop().unwrap();
                    let obj_name_expr = args.pop().unwrap();
                    let JsExpr::Literal(Constant::String(obj_name)) = obj_name_expr else {
                        unreachable!()
                    };
                    let JsExpr::Literal(Constant::String(field_name)) = field else {
                        unreachable!()
                    };
                    let target = JsExpr::Field {
                        object: Box::new(JsExpr::Index {
                            collection: Box::new(JsExpr::Field {
                                object: Box::new(JsExpr::Var(obj_name)),
                                field: "instances".into(),
                            }),
                            index: Box::new(JsExpr::Literal(Constant::Int(0))),
                        }),
                        field: field_name,
                    };
                    let mut val = val;
                    rewrite_expr(&mut val);
                    *stmt = JsStmt::Assign { target, value: val };
                    return;
                }
            }
            rewrite_expr(e);
        }
        JsStmt::Return(Some(e)) => rewrite_expr(e),
        JsStmt::Return(None) => {}
        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            rewrite_expr(cond);
            rewrite_stmts(then_body);
            rewrite_stmts(else_body);
        }
        JsStmt::While { cond, body } => {
            rewrite_expr(cond);
            rewrite_stmts(body);
        }
        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => {
            rewrite_stmts(init);
            rewrite_expr(cond);
            rewrite_stmts(update);
            rewrite_stmts(body);
        }
        JsStmt::Loop { body } => {
            rewrite_stmts(body);
        }
        JsStmt::ForOf { iterable, body, .. } => {
            rewrite_expr(iterable);
            rewrite_stmts(body);
        }
        JsStmt::Throw(e) => rewrite_expr(e),
        JsStmt::Dispatch { blocks, .. } => {
            for (_, stmts) in blocks {
                rewrite_stmts(stmts);
            }
        }
        JsStmt::Switch {
            value,
            cases,
            default_body,
        } => {
            rewrite_expr(value);
            for (_, stmts) in cases {
                rewrite_stmts(stmts);
            }
            rewrite_stmts(default_body);
        }
        JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => {}
    }
}

fn rewrite_expr(expr: &mut JsExpr) {
    // First, recurse into children.
    rewrite_expr_children(expr);

    // Then, attempt to resolve SystemCall patterns.
    let replacement = match expr {
        JsExpr::SystemCall {
            system,
            method,
            args,
        } => try_rewrite_system_call(system, method, args),
        _ => None,
    };

    if let Some(new_expr) = replacement {
        *expr = new_expr;
    }
}

fn rewrite_expr_children(expr: &mut JsExpr) {
    match expr {
        JsExpr::Binary { lhs, rhs, .. } | JsExpr::Cmp { lhs, rhs, .. } => {
            rewrite_expr(lhs);
            rewrite_expr(rhs);
        }
        JsExpr::LogicalOr { lhs, rhs } | JsExpr::LogicalAnd { lhs, rhs } => {
            rewrite_expr(lhs);
            rewrite_expr(rhs);
        }
        JsExpr::Unary { expr: inner, .. } => rewrite_expr(inner),
        JsExpr::Not(inner) | JsExpr::PostIncrement(inner) => rewrite_expr(inner),
        JsExpr::Field { object, .. } => rewrite_expr(object),
        JsExpr::Index { collection, index } => {
            rewrite_expr(collection);
            rewrite_expr(index);
        }
        JsExpr::Call { callee, args } => {
            rewrite_expr(callee);
            for arg in args {
                rewrite_expr(arg);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            rewrite_expr(cond);
            rewrite_expr(then_val);
            rewrite_expr(else_val);
        }
        JsExpr::ArrayInit(items) | JsExpr::TupleInit(items) => {
            for item in items {
                rewrite_expr(item);
            }
        }
        JsExpr::ObjectInit(fields) => {
            for (_, val) in fields {
                rewrite_expr(val);
            }
        }
        JsExpr::New { callee, args } => {
            rewrite_expr(callee);
            for arg in args {
                rewrite_expr(arg);
            }
        }
        JsExpr::TypeOf(inner) => rewrite_expr(inner),
        JsExpr::In { key, object } => {
            rewrite_expr(key);
            rewrite_expr(object);
        }
        JsExpr::Delete { object, key } => {
            rewrite_expr(object);
            rewrite_expr(key);
        }
        JsExpr::Cast { expr: inner, .. } | JsExpr::TypeCheck { expr: inner, .. } => {
            rewrite_expr(inner)
        }
        JsExpr::ArrowFunction { body, .. } => rewrite_stmts(body),
        JsExpr::SuperCall(args) | JsExpr::SuperMethodCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg);
            }
        }
        JsExpr::SuperGet(_) => {}
        JsExpr::SuperSet { value, .. } => rewrite_expr(value),
        JsExpr::GeneratorCreate { args, .. } => {
            for arg in args {
                rewrite_expr(arg);
            }
        }
        JsExpr::GeneratorResume(inner) => rewrite_expr(inner),
        JsExpr::Yield(inner) => {
            if let Some(e) = inner {
                rewrite_expr(e);
            }
        }
        JsExpr::Activation => {}
        JsExpr::SystemCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg);
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
) -> Option<JsExpr> {
    match (system, method) {
        // GameMaker.Global.set(name, val) → variable_global_set(name, val)
        ("GameMaker.Global", "set") if args.len() == 2 => {
            let val = args.pop().unwrap();
            let name = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("variable_global_set".into())),
                args: vec![name, val],
            })
        }
        // GameMaker.Global.get(name) → variable_global_get(name)
        ("GameMaker.Global", "get") if args.len() == 1 => {
            let name = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("variable_global_get".into())),
                args: vec![name],
            })
        }
        // GameMaker.Instance.getOn(objName, field) → ObjName.instances[0].field
        // GameMaker.Instance.getOn(objId, field)   → getInstanceField(objId, field)
        ("GameMaker.Instance", "getOn") if args.len() == 2 => {
            let field = args.pop().unwrap();
            let obj_id = args.pop().unwrap();
            if let JsExpr::Literal(Constant::String(ref obj_name)) = obj_id {
                if let JsExpr::Literal(Constant::String(ref field_name)) = field {
                    Some(JsExpr::Field {
                        object: Box::new(JsExpr::Index {
                            collection: Box::new(JsExpr::Field {
                                object: Box::new(JsExpr::Var(obj_name.clone())),
                                field: "instances".into(),
                            }),
                            index: Box::new(JsExpr::Literal(Constant::Int(0))),
                        }),
                        field: field_name.clone(),
                    })
                } else {
                    Some(JsExpr::Index {
                        collection: Box::new(JsExpr::Index {
                            collection: Box::new(JsExpr::Field {
                                object: Box::new(JsExpr::Var(obj_name.clone())),
                                field: "instances".into(),
                            }),
                            index: Box::new(JsExpr::Literal(Constant::Int(0))),
                        }),
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
