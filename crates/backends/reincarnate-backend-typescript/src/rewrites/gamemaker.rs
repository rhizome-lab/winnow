//! GameMaker-specific JsExpr → JsExpr rewrite pass.
//!
//! Resolves GameMaker SystemCall nodes into native JavaScript constructs.
//! Much simpler than Flash — only 8 SystemCall patterns to handle.

use std::collections::{BTreeMap, HashMap};

use reincarnate_core::ir::value::Constant;
use reincarnate_core::ir::{ExternalImport, Type, ValueId};

use crate::emit::{ClassRegistry, RefSets};
use crate::js_ast::{JsExpr, JsFunction, JsStmt};

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
        ("GameMaker.Instance", "withBegin") => &["withInstances"],
        ("GameMaker.Instance", "withInstances") => &["withInstances"],
        _ => &[],
    }
}

/// Rewrite a function's body, resolving GameMaker SystemCalls.
pub fn rewrite_gamemaker_function(mut func: JsFunction, sprite_names: &[String]) -> JsFunction {
    rewrite_stmts(&mut func.body, sprite_names);
    func
}

fn rewrite_stmts(stmts: &mut Vec<JsStmt>, sprite_names: &[String]) {
    for stmt in stmts.iter_mut() {
        rewrite_stmt(stmt, sprite_names);
    }
    collapse_with_blocks(stmts);
}

/// Detect `withBegin(target) ... withEnd()` bracket patterns and collapse them
/// into `withInstances(target, () => { ...body... })`.
///
/// Two patterns are handled:
///
/// 1. **Flat**: `withBegin(target); ...body...; withEnd();` — all at the same
///    nesting level.
///
/// 2. **Loop-wrapped** (produced by the structurizer from PushEnv/PopEnv
///    back-edges): `withBegin(target); loop { ...body...; withEnd(); continue; }`
///    — the `withEnd` is inside the loop body, not at the outer level.
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

        // --- Pattern 2: loop-wrapped ---
        // withBegin(target); Loop { ...body...; withEnd(); continue; }
        if i + 1 < stmts.len() {
            let is_loop = matches!(&stmts[i + 1], JsStmt::Loop { .. } | JsStmt::While { .. });
            if is_loop {
                if let Some(body) = try_extract_with_loop_body(&mut stmts[i + 1]) {
                    // Extract the target from withBegin.
                    let JsStmt::Expr(JsExpr::SystemCall { args, .. }) = &mut stmts[i] else {
                        unreachable!()
                    };
                    let target = args.pop().unwrap();

                    // Remove the loop statement.
                    stmts.remove(i + 1);

                    // Replace withBegin with withInstances(target, () => { body }).
                    stmts[i] = make_with_instances(target, body);
                    i += 1;
                    continue;
                }
            }
        }

        // --- Pattern 1: flat ---
        // withBegin(target); ...body...; withEnd();
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
        stmts[i] = make_with_instances(target, body);

        i += 1;
    }
}

/// Try to extract the with-body from a loop statement that wraps a with-block.
///
/// Recognizes loops whose body ends with `withEnd(); continue;` or just
/// `withEnd();`. Returns the body with those trailing statements stripped,
/// or `None` if the pattern doesn't match.
fn try_extract_with_loop_body(loop_stmt: &mut JsStmt) -> Option<Vec<JsStmt>> {
    let body = match loop_stmt {
        JsStmt::Loop { body } => body,
        JsStmt::While { body, .. } => body,
        _ => return None,
    };

    // Find withEnd() — typically second-to-last (before continue) or last.
    let end_pos = body.iter().rposition(|s| {
        matches!(
            s,
            JsStmt::Expr(JsExpr::SystemCall { system, method, .. })
            if system == "GameMaker.Instance" && method == "withEnd"
        )
    })?;

    // Everything after withEnd should be only Continue/Break (structurizer artifacts).
    let all_tail_ok = body[end_pos + 1..]
        .iter()
        .all(|s| matches!(s, JsStmt::Continue | JsStmt::Break));
    if !all_tail_ok {
        return None;
    }

    // Truncate: remove withEnd and everything after it.
    body.truncate(end_pos);
    Some(std::mem::take(body))
}

/// Build a `withInstances(target, () => { body })` statement.
fn make_with_instances(target: JsExpr, body: Vec<JsStmt>) -> JsStmt {
    JsStmt::Expr(JsExpr::Call {
        callee: Box::new(JsExpr::Var("withInstances".into())),
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
    })
}

fn rewrite_stmt(stmt: &mut JsStmt, sprite_names: &[String]) {
    match stmt {
        JsStmt::VarDecl { init, .. } => {
            if let Some(expr) = init {
                rewrite_expr(expr, sprite_names);
            }
        }
        JsStmt::Assign { target, value } => {
            rewrite_expr(target, sprite_names);
            rewrite_expr(value, sprite_names);
            try_resolve_sprite_assign(target, value, sprite_names);
        }
        JsStmt::CompoundAssign { target, value, .. } => {
            rewrite_expr(target, sprite_names);
            rewrite_expr(value, sprite_names);
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
                    rewrite_expr(&mut val, sprite_names);
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
                            rewrite_expr(&mut val, sprite_names);
                            *stmt = JsStmt::Assign { target, value: val };
                            return;
                        }
                        // setOn(obj, field, index, value) → Obj.instances[0].field[index] = value
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
                            rewrite_expr(&mut index_expr, sprite_names);
                            let target = JsExpr::Index {
                                collection: Box::new(JsExpr::Field {
                                    object: Box::new(JsExpr::Index {
                                        collection: Box::new(JsExpr::Field {
                                            object: Box::new(JsExpr::Var(obj_name)),
                                            field: "instances".into(),
                                        }),
                                        index: Box::new(JsExpr::Literal(Constant::Int(0))),
                                    }),
                                    field: field_name,
                                }),
                                index: Box::new(index_expr),
                            };
                            let mut val = val;
                            rewrite_expr(&mut val, sprite_names);
                            *stmt = JsStmt::Assign { target, value: val };
                            return;
                        }
                        _ => {}
                    }
                }
            }
            rewrite_expr(e, sprite_names);
        }
        JsStmt::Return(Some(e)) => rewrite_expr(e, sprite_names),
        JsStmt::Return(None) => {}
        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            rewrite_expr(cond, sprite_names);
            rewrite_stmts(then_body, sprite_names);
            rewrite_stmts(else_body, sprite_names);
        }
        JsStmt::While { cond, body } => {
            rewrite_expr(cond, sprite_names);
            rewrite_stmts(body, sprite_names);
        }
        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => {
            rewrite_stmts(init, sprite_names);
            rewrite_expr(cond, sprite_names);
            rewrite_stmts(update, sprite_names);
            rewrite_stmts(body, sprite_names);
        }
        JsStmt::Loop { body } => {
            rewrite_stmts(body, sprite_names);
        }
        JsStmt::ForOf { iterable, body, .. } => {
            rewrite_expr(iterable, sprite_names);
            rewrite_stmts(body, sprite_names);
        }
        JsStmt::Throw(e) => rewrite_expr(e, sprite_names),
        JsStmt::Dispatch { blocks, .. } => {
            for (_, stmts) in blocks {
                rewrite_stmts(stmts, sprite_names);
            }
        }
        JsStmt::Switch {
            value,
            cases,
            default_body,
        } => {
            rewrite_expr(value, sprite_names);
            for (_, stmts) in cases {
                rewrite_stmts(stmts, sprite_names);
            }
            rewrite_stmts(default_body, sprite_names);
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
            *value = JsExpr::Field {
                object: Box::new(JsExpr::Var("Sprites".into())),
                field: name.clone(),
            };
        }
    }
}

fn rewrite_expr(expr: &mut JsExpr, sprite_names: &[String]) {
    // First, recurse into children.
    rewrite_expr_children(expr, sprite_names);

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

fn rewrite_expr_children(expr: &mut JsExpr, sprite_names: &[String]) {
    match expr {
        JsExpr::Binary { lhs, rhs, .. } | JsExpr::Cmp { lhs, rhs, .. } => {
            rewrite_expr(lhs, sprite_names);
            rewrite_expr(rhs, sprite_names);
        }
        JsExpr::LogicalOr { lhs, rhs } | JsExpr::LogicalAnd { lhs, rhs } => {
            rewrite_expr(lhs, sprite_names);
            rewrite_expr(rhs, sprite_names);
        }
        JsExpr::Unary { expr: inner, .. } => rewrite_expr(inner, sprite_names),
        JsExpr::Not(inner) | JsExpr::PostIncrement(inner) => rewrite_expr(inner, sprite_names),
        JsExpr::Field { object, .. } => rewrite_expr(object, sprite_names),
        JsExpr::Index { collection, index } => {
            rewrite_expr(collection, sprite_names);
            rewrite_expr(index, sprite_names);
        }
        JsExpr::Call { callee, args } => {
            rewrite_expr(callee, sprite_names);
            for arg in args {
                rewrite_expr(arg, sprite_names);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            rewrite_expr(cond, sprite_names);
            rewrite_expr(then_val, sprite_names);
            rewrite_expr(else_val, sprite_names);
        }
        JsExpr::ArrayInit(items) | JsExpr::TupleInit(items) => {
            for item in items {
                rewrite_expr(item, sprite_names);
            }
        }
        JsExpr::ObjectInit(fields) => {
            for (_, val) in fields {
                rewrite_expr(val, sprite_names);
            }
        }
        JsExpr::New { callee, args } => {
            rewrite_expr(callee, sprite_names);
            for arg in args {
                rewrite_expr(arg, sprite_names);
            }
        }
        JsExpr::TypeOf(inner) => rewrite_expr(inner, sprite_names),
        JsExpr::In { key, object } => {
            rewrite_expr(key, sprite_names);
            rewrite_expr(object, sprite_names);
        }
        JsExpr::Delete { object, key } => {
            rewrite_expr(object, sprite_names);
            rewrite_expr(key, sprite_names);
        }
        JsExpr::Cast { expr: inner, .. } | JsExpr::TypeCheck { expr: inner, .. } => {
            rewrite_expr(inner, sprite_names)
        }
        JsExpr::ArrowFunction { body, .. } => rewrite_stmts(body, sprite_names),
        JsExpr::SuperCall(args) | JsExpr::SuperMethodCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg, sprite_names);
            }
        }
        JsExpr::SuperGet(_) => {}
        JsExpr::SuperSet { value, .. } => rewrite_expr(value, sprite_names),
        JsExpr::GeneratorCreate { args, .. } => {
            for arg in args {
                rewrite_expr(arg, sprite_names);
            }
        }
        JsExpr::GeneratorResume(inner) => rewrite_expr(inner, sprite_names),
        JsExpr::Yield(inner) => {
            if let Some(e) = inner {
                rewrite_expr(e, sprite_names);
            }
        }
        JsExpr::Activation => {}
        JsExpr::SystemCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg, sprite_names);
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
        // GameMaker.Instance.getOn(objName, field, index) → ObjName.instances[0].field[index]
        // GameMaker.Instance.getOn(objId, field, index)   → getInstanceField(objId, field)[index]
        ("GameMaker.Instance", "getOn") if args.len() == 3 => {
            let index = args.pop().unwrap();
            let field = args.pop().unwrap();
            let obj_id = args.pop().unwrap();
            let base = if let JsExpr::Literal(Constant::String(ref obj_name)) = obj_id {
                if let JsExpr::Literal(Constant::String(ref field_name)) = field {
                    JsExpr::Field {
                        object: Box::new(JsExpr::Index {
                            collection: Box::new(JsExpr::Field {
                                object: Box::new(JsExpr::Var(obj_name.clone())),
                                field: "instances".into(),
                            }),
                            index: Box::new(JsExpr::Literal(Constant::Int(0))),
                        }),
                        field: field_name.clone(),
                    }
                } else {
                    JsExpr::Index {
                        collection: Box::new(JsExpr::Index {
                            collection: Box::new(JsExpr::Field {
                                object: Box::new(JsExpr::Var(obj_name.clone())),
                                field: "instances".into(),
                            }),
                            index: Box::new(JsExpr::Literal(Constant::Int(0))),
                        }),
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
    use reincarnate_core::ir::value::Constant;

    fn gm_instance_call(method: &str, args: Vec<JsExpr>) -> JsExpr {
        JsExpr::SystemCall {
            system: "GameMaker.Instance".into(),
            method: method.into(),
            args,
        }
    }

    fn with_begin(target: i64) -> JsStmt {
        JsStmt::Expr(gm_instance_call(
            "withBegin",
            vec![JsExpr::Literal(Constant::Int(target))],
        ))
    }

    fn with_end() -> JsStmt {
        JsStmt::Expr(gm_instance_call("withEnd", vec![]))
    }

    fn body_stmt(name: &str) -> JsStmt {
        JsStmt::Expr(JsExpr::Call {
            callee: Box::new(JsExpr::Var(name.into())),
            args: vec![],
        })
    }

    /// Assert that `stmts[idx]` is `withInstances(expected_target, () => { ... })`
    /// and return a reference to the callback body.
    fn assert_with_instances(stmts: &[JsStmt], idx: usize, expected_target: i64) -> &Vec<JsStmt> {
        let JsStmt::Expr(JsExpr::Call { callee, args }) = &stmts[idx] else {
            panic!("expected Call at index {idx}, got {:?}", stmts[idx]);
        };
        let JsExpr::Var(name) = callee.as_ref() else {
            panic!("expected Var callee, got {:?}", callee);
        };
        assert_eq!(name, "withInstances");
        assert_eq!(args.len(), 2);
        let JsExpr::Literal(Constant::Int(target)) = &args[0] else {
            panic!("expected Int literal target, got {:?}", args[0]);
        };
        assert_eq!(*target, expected_target);
        let JsExpr::ArrowFunction { body, .. } = &args[1] else {
            panic!("expected ArrowFunction, got {:?}", args[1]);
        };
        body
    }

    #[test]
    fn collapse_flat_pattern() {
        let mut stmts = vec![with_begin(5), body_stmt("foo"), body_stmt("bar"), with_end()];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 1);
        let body = assert_with_instances(&stmts, 0, 5);
        assert_eq!(body.len(), 2);
    }

    #[test]
    fn collapse_loop_wrapped_pattern() {
        let mut stmts = vec![
            with_begin(3),
            JsStmt::Loop {
                body: vec![body_stmt("foo"), with_end(), JsStmt::Continue],
            },
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 1);
        let body = assert_with_instances(&stmts, 0, 3);
        assert_eq!(body.len(), 1);
    }

    #[test]
    fn collapse_loop_wrapped_no_trailing_continue() {
        let mut stmts = vec![
            with_begin(7),
            JsStmt::Loop {
                body: vec![body_stmt("foo"), with_end()],
            },
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 1);
        let body = assert_with_instances(&stmts, 0, 7);
        assert_eq!(body.len(), 1);
    }

    #[test]
    fn collapse_while_wrapped_pattern() {
        let mut stmts = vec![
            with_begin(10),
            JsStmt::While {
                cond: JsExpr::Literal(Constant::Bool(true)),
                body: vec![body_stmt("foo"), with_end(), JsStmt::Continue],
            },
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 1);
        assert_with_instances(&stmts, 0, 10);
    }

    #[test]
    fn collapse_preserves_surrounding_stmts() {
        let mut stmts = vec![
            body_stmt("before"),
            with_begin(1),
            JsStmt::Loop {
                body: vec![body_stmt("foo"), with_end(), JsStmt::Continue],
            },
            body_stmt("after"),
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 3);
        assert!(matches!(&stmts[0], JsStmt::Expr(JsExpr::Call { .. })));
        assert_with_instances(&stmts, 1, 1);
        assert!(matches!(&stmts[2], JsStmt::Expr(JsExpr::Call { .. })));
    }

    #[test]
    fn collapse_multiple_consecutive() {
        let mut stmts = vec![
            with_begin(1),
            JsStmt::Loop {
                body: vec![body_stmt("a"), with_end(), JsStmt::Continue],
            },
            with_begin(2),
            JsStmt::Loop {
                body: vec![body_stmt("b"), with_end(), JsStmt::Continue],
            },
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 2);
        assert_with_instances(&stmts, 0, 1);
        assert_with_instances(&stmts, 1, 2);
    }

    #[test]
    fn collapse_loop_with_multi_stmt_body() {
        let mut stmts = vec![
            with_begin(16),
            JsStmt::Loop {
                body: vec![
                    body_stmt("a"),
                    body_stmt("b"),
                    body_stmt("c"),
                    with_end(),
                    JsStmt::Continue,
                ],
            },
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 1);
        let body = assert_with_instances(&stmts, 0, 16);
        assert_eq!(body.len(), 3);
    }

    #[test]
    fn no_collapse_when_loop_has_no_with_end() {
        let mut stmts = vec![
            with_begin(5),
            JsStmt::Loop {
                body: vec![body_stmt("foo"), JsStmt::Continue],
            },
        ];
        collapse_with_blocks(&mut stmts);

        // Nothing collapsed — both statements remain.
        assert_eq!(stmts.len(), 2);
    }

    // --- Adversarial / edge cases ---

    #[test]
    fn nested_with_blocks_outer_collapses_inner_untouched_by_collapse_alone() {
        // collapse_with_blocks only operates on a single Vec level.
        // Nested with-blocks require rewrite_stmts (which recurses first).
        // This test documents that collapse_with_blocks alone collapses the
        // outer pair but leaves the inner pair for a recursive caller.
        let mut stmts = vec![
            with_begin(1),
            JsStmt::Loop {
                body: vec![
                    with_begin(2),
                    JsStmt::Loop {
                        body: vec![body_stmt("inner"), with_end(), JsStmt::Continue],
                    },
                    with_end(),
                    JsStmt::Continue,
                ],
            },
        ];
        collapse_with_blocks(&mut stmts);

        // Outer collapses: withBegin(1) + Loop → withInstances(1, callback)
        assert_eq!(stmts.len(), 1);
        let outer_body = assert_with_instances(&stmts, 0, 1);
        // Inner withBegin(2) + Loop is NOT collapsed (collapse doesn't recurse).
        // The body is [withBegin(2), Loop{...}] (2 items, not 1).
        assert_eq!(outer_body.len(), 2);
    }

    #[test]
    fn nested_with_blocks_both_collapse_via_rewrite_stmts() {
        // rewrite_stmts recurses into loop bodies before calling collapse,
        // so nested with-blocks are handled bottom-up.
        let mut stmts = vec![
            with_begin(1),
            JsStmt::Loop {
                body: vec![
                    with_begin(2),
                    JsStmt::Loop {
                        body: vec![body_stmt("inner"), with_end(), JsStmt::Continue],
                    },
                    with_end(),
                    JsStmt::Continue,
                ],
            },
        ];
        rewrite_stmts(&mut stmts, &[]);

        assert_eq!(stmts.len(), 1);
        let outer_body = assert_with_instances(&stmts, 0, 1);
        assert_eq!(outer_body.len(), 1);
        assert_with_instances(outer_body, 0, 2);
    }

    #[test]
    fn with_begin_as_last_statement_no_panic() {
        // withBegin(5) at the end with nothing after it — must not panic or collapse.
        let mut stmts = vec![body_stmt("setup"), with_begin(5)];
        collapse_with_blocks(&mut stmts);

        // Nothing to collapse — both statements remain.
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn with_end_in_middle_of_loop_body_still_collapses() {
        // withBegin(3); loop { withEnd(); body_after; continue }
        // withEnd is NOT at the end — body_after comes after it. Should NOT collapse
        // because body_after is a real statement, not continue/break.
        let mut stmts = vec![
            with_begin(3),
            JsStmt::Loop {
                body: vec![with_end(), body_stmt("after_end"), JsStmt::Continue],
            },
        ];
        collapse_with_blocks(&mut stmts);

        // Should NOT collapse: "after_end" is between withEnd and continue,
        // and it's not a break/continue. The tail check should reject this.
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn loop_with_real_stmts_after_with_end_not_collapsed() {
        // The key adversarial case: withEnd() is followed by a real statement
        // (not just continue/break) in the loop. This means the loop isn't just
        // a with-iteration wrapper — there's real post-with logic.
        let mut stmts = vec![
            with_begin(5),
            JsStmt::Loop {
                body: vec![
                    body_stmt("body"),
                    with_end(),
                    body_stmt("cleanup"),  // not continue/break
                ],
            },
        ];
        collapse_with_blocks(&mut stmts);

        // Must NOT collapse — "cleanup" after withEnd means this isn't a pure with-loop.
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn empty_loop_body_with_just_with_end() {
        // withBegin(1); loop { withEnd(); continue }
        // Empty with-body (no statements between begin and end).
        let mut stmts = vec![
            with_begin(1),
            JsStmt::Loop {
                body: vec![with_end(), JsStmt::Continue],
            },
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 1);
        let body = assert_with_instances(&stmts, 0, 1);
        assert_eq!(body.len(), 0); // empty callback body
    }

    #[test]
    fn with_begin_followed_by_non_loop_non_end_no_collapse() {
        // withBegin(1); foo(); bar();
        // No withEnd anywhere — should leave everything untouched.
        let mut stmts = vec![with_begin(1), body_stmt("foo"), body_stmt("bar")];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 3);
    }

    #[test]
    fn with_begin_followed_by_if_containing_with_end_no_collapse() {
        // This is the ORIGINAL BUG pattern but without a loop wrapper.
        // withBegin(3); if (...) { withEnd(); }
        // withEnd is inside the if-body, NOT at the outer level.
        // The flat scan should NOT find it (it only scans the same Vec).
        // The loop check should NOT match (it's an If, not a Loop).
        let mut stmts = vec![
            with_begin(3),
            JsStmt::If {
                cond: JsExpr::Literal(Constant::Bool(true)),
                then_body: vec![with_end()],
                else_body: vec![],
            },
        ];
        collapse_with_blocks(&mut stmts);

        // Must NOT collapse — withEnd is nested inside an If, not at the same level.
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn with_end_before_with_begin_not_confused() {
        // Reversed order: withEnd(); withBegin(1); loop { ...; withEnd(); continue }
        // The leading withEnd should be ignored, and only the withBegin/loop pair collapses.
        let mut stmts = vec![
            with_end(), // stray, should be left alone
            with_begin(1),
            JsStmt::Loop {
                body: vec![body_stmt("x"), with_end(), JsStmt::Continue],
            },
        ];
        collapse_with_blocks(&mut stmts);

        assert_eq!(stmts.len(), 2); // stray withEnd + collapsed withInstances
        // First statement is the stray withEnd (unchanged)
        assert!(matches!(
            &stmts[0],
            JsStmt::Expr(JsExpr::SystemCall { method, .. }) if method == "withEnd"
        ));
        assert_with_instances(&stmts, 1, 1);
    }

    #[test]
    fn flat_collapse_takes_first_with_end_not_last() {
        // withBegin(1); a(); withEnd(); b(); withEnd();
        // Flat pattern should match the FIRST withEnd, not the second.
        // Body should be just [a()], not [a(), withEnd(), b()].
        let mut stmts = vec![
            with_begin(1),
            body_stmt("a"),
            with_end(),
            body_stmt("b"),
            with_end(),
        ];
        collapse_with_blocks(&mut stmts);

        // First withBegin/withEnd pair collapses with body [a()].
        // Remaining: withInstances(1, () => { a() }), b(), withEnd()
        assert_eq!(stmts.len(), 3);
        let body = assert_with_instances(&stmts, 0, 1);
        assert_eq!(body.len(), 1);
    }

    #[test]
    fn rewrite_introduced_calls_maps_with_begin() {
        assert_eq!(
            rewrite_introduced_calls("GameMaker.Instance", "withBegin"),
            &["withInstances"]
        );
    }

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
