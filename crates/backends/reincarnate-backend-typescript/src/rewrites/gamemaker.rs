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
