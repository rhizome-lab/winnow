//! Twine/SugarCube-specific JsExpr → JsExpr rewrite pass.
//!
//! Resolves `SugarCube.Engine.*` SystemCall nodes that map to native JavaScript
//! constructs (`new`, `typeof`, `delete`, `in`, `**`, etc.). All other
//! SugarCube SystemCalls pass through to runtime modules via the printer's
//! `SystemCall` fallback + auto-import machinery.

use reincarnate_core::ir::value::Constant;
use reincarnate_core::ir::CmpKind;

use crate::js_ast::{JsExpr, JsFunction, JsStmt};

/// Returns the bare function names that a SystemCall rewrite will introduce,
/// if any. Used by import generation to emit the correct imports before
/// the rewrite pass runs.
pub fn rewrite_introduced_calls(_system: &str, _method: &str) -> &'static [&'static str] {
    // Twine rewrites produce only built-in JS constructs (new, typeof, etc.)
    // and calls to Math.pow / String — none of which need function-module imports.
    &[]
}

/// Rewrite a function's body, resolving SugarCube.Engine SystemCalls that map
/// to native JS constructs.
pub fn rewrite_twine_function(mut func: JsFunction) -> JsFunction {
    rewrite_stmts(&mut func.body);
    func
}

fn rewrite_stmts(stmts: &mut [JsStmt]) {
    for stmt in stmts.iter_mut() {
        rewrite_stmt(stmt);
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
        JsStmt::Expr(e) => rewrite_expr(e),
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
        JsStmt::Loop { body } => rewrite_stmts(body),
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
    // Recurse into children first.
    rewrite_expr_children(expr);

    // Then attempt to resolve SystemCall patterns.
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
            rewrite_expr(inner);
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
        JsExpr::Literal(_) | JsExpr::Var(_) | JsExpr::This => {}
    }
}

/// Try to rewrite a SugarCube SystemCall into a native JS expression.
///
/// Only rewrites `SugarCube.Engine.*` calls that map directly to JS constructs.
/// Everything else (State, Output, Navigation, Audio, DOM, Input, Widget, and
/// remaining Engine methods like iterate/eval/clone) returns `None` and passes
/// through to the runtime modules via the printer's SystemCall fallback.
fn try_rewrite_system_call(
    system: &str,
    method: &str,
    args: &mut Vec<JsExpr>,
) -> Option<JsExpr> {
    if system != "SugarCube.Engine" {
        return None;
    }

    match method {
        // new(callee, ...args) → new callee(...args)
        "new" if !args.is_empty() => {
            let mut a = std::mem::take(args);
            let callee = a.remove(0);
            Some(JsExpr::New {
                callee: Box::new(callee),
                args: a,
            })
        }

        // typeof(v) → typeof v
        "typeof" if args.len() == 1 => {
            let v = args.pop().unwrap();
            Some(JsExpr::TypeOf(Box::new(v)))
        }

        // delete(expr) → delete expr
        // The frontend emits delete with a single expression argument.
        // If it's a field access, split into object + key for Delete node.
        "delete" if args.len() == 1 => {
            let v = args.pop().unwrap();
            match v {
                JsExpr::Field { object, field } => Some(JsExpr::Delete {
                    object,
                    key: Box::new(JsExpr::Literal(Constant::String(field))),
                }),
                JsExpr::Index { collection, index } => Some(JsExpr::Delete {
                    object: collection,
                    key: index,
                }),
                // Fallback: delete v (not a standard pattern but preserve it)
                other => Some(JsExpr::Delete {
                    object: Box::new(other),
                    key: Box::new(JsExpr::Literal(Constant::String("__delete__".into()))),
                }),
            }
        }

        // in(key, obj) → key in obj
        "in" if args.len() == 2 => {
            let obj = args.pop().unwrap();
            let key = args.pop().unwrap();
            Some(JsExpr::In {
                key: Box::new(key),
                object: Box::new(obj),
            })
        }

        // pow(a, b) → Math.pow(a, b)
        "pow" if args.len() == 2 => {
            let b = args.pop().unwrap();
            let a = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Field {
                    object: Box::new(JsExpr::Var("Math".into())),
                    field: "pow".into(),
                }),
                args: vec![a, b],
            })
        }

        // def(v) → v != null (loose inequality)
        "def" if args.len() == 1 => {
            let v = args.pop().unwrap();
            Some(JsExpr::Cmp {
                kind: CmpKind::Ne,
                lhs: Box::new(v),
                rhs: Box::new(JsExpr::Literal(Constant::Null)),
            })
        }

        // ndef(v) → v == null (loose equality)
        "ndef" if args.len() == 1 => {
            let v = args.pop().unwrap();
            Some(JsExpr::Cmp {
                kind: CmpKind::Eq,
                lhs: Box::new(v),
                rhs: Box::new(JsExpr::Literal(Constant::Null)),
            })
        }

        // is_nullish(v) → v == null (loose equality, matches null and undefined)
        "is_nullish" if args.len() == 1 => {
            let v = args.pop().unwrap();
            Some(JsExpr::Cmp {
                kind: CmpKind::Eq,
                lhs: Box::new(v),
                rhs: Box::new(JsExpr::Literal(Constant::Null)),
            })
        }

        // to_string(v) → String(v)
        "to_string" if args.len() == 1 => {
            let v = args.pop().unwrap();
            Some(JsExpr::Call {
                callee: Box::new(JsExpr::Var("String".into())),
                args: vec![v],
            })
        }

        // Everything else (clone, resolve, iterate, iterator_*, eval, arrow,
        // error, done_*, break, continue, ushr, instanceof) passes through
        // to the runtime via the SystemCall fallback in the printer.
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use reincarnate_core::ir::{MethodKind, Type, Visibility};

    /// Helper to build a JsFunction with a single expression statement.
    fn func_with_expr(expr: JsExpr) -> JsFunction {
        JsFunction {
            name: "test".into(),
            params: vec![],
            return_ty: Type::Void,
            body: vec![JsStmt::Expr(expr)],
            is_generator: false,
            visibility: Visibility::Public,
            method_kind: MethodKind::Free,
            has_rest_param: false,
        }
    }

    fn extract_expr(func: &JsFunction) -> &JsExpr {
        match &func.body[0] {
            JsStmt::Expr(e) => e,
            _ => panic!("expected Expr statement"),
        }
    }

    #[test]
    fn rewrite_new() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "new".into(),
            args: vec![
                JsExpr::Var("Date".into()),
                JsExpr::Literal(Constant::Int(2025)),
            ],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        assert!(matches!(extract_expr(&func), JsExpr::New { .. }));
    }

    #[test]
    fn rewrite_typeof() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "typeof".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        assert!(matches!(extract_expr(&func), JsExpr::TypeOf(_)));
    }

    #[test]
    fn rewrite_def_ndef() {
        let def = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "def".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = rewrite_twine_function(func_with_expr(def));
        assert!(matches!(
            extract_expr(&func),
            JsExpr::Cmp {
                kind: CmpKind::Ne,
                ..
            }
        ));

        let ndef = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "ndef".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = rewrite_twine_function(func_with_expr(ndef));
        assert!(matches!(
            extract_expr(&func),
            JsExpr::Cmp {
                kind: CmpKind::Eq,
                ..
            }
        ));
    }

    #[test]
    fn rewrite_pow() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "pow".into(),
            args: vec![JsExpr::Var("a".into()), JsExpr::Var("b".into())],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        assert!(matches!(extract_expr(&func), JsExpr::Call { .. }));
    }

    #[test]
    fn rewrite_in() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "in".into(),
            args: vec![
                JsExpr::Literal(Constant::String("key".into())),
                JsExpr::Var("obj".into()),
            ],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        assert!(matches!(extract_expr(&func), JsExpr::In { .. }));
    }

    #[test]
    fn passthrough_state_get() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.State".into(),
            method: "get".into(),
            args: vec![JsExpr::Literal(Constant::String("name".into()))],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        // Should remain as SystemCall — not rewritten
        assert!(matches!(extract_expr(&func), JsExpr::SystemCall { .. }));
    }

    #[test]
    fn passthrough_engine_eval() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "eval".into(),
            args: vec![JsExpr::Literal(Constant::String("code".into()))],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        // eval should pass through to runtime
        assert!(matches!(extract_expr(&func), JsExpr::SystemCall { .. }));
    }

    #[test]
    fn rewrite_to_string() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "to_string".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                assert!(matches!(callee.as_ref(), JsExpr::Var(n) if n == "String"));
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_delete_field() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "delete".into(),
            args: vec![JsExpr::Field {
                object: Box::new(JsExpr::Var("obj".into())),
                field: "prop".into(),
            }],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        assert!(matches!(extract_expr(&func), JsExpr::Delete { .. }));
    }

    #[test]
    fn rewrite_is_nullish() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "is_nullish".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = rewrite_twine_function(func_with_expr(expr));
        assert!(matches!(
            extract_expr(&func),
            JsExpr::Cmp {
                kind: CmpKind::Eq,
                ..
            }
        ));
    }
}
