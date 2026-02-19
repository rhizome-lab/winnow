//! SugarCube-specific JsExpr → JsExpr rewrites.
//!
//! Converts `SugarCube.Engine.*` SystemCall nodes into native JavaScript
//! constructs: `new`, `typeof`, `delete`, `in`, `Math.pow`, `String()`,
//! null-checks (`def`/`ndef`/`is_nullish`), and closure inlining.

use std::collections::HashMap;

use reincarnate_core::ir::value::Constant;
use reincarnate_core::ir::CmpKind;

use crate::js_ast::{JsExpr, JsFunction};

/// Try to rewrite a `SugarCube.Engine.*` SystemCall.
pub(super) fn try_rewrite(
    method: &str,
    args: &mut Vec<JsExpr>,
    closures: &HashMap<String, JsFunction>,
) -> Option<JsExpr> {
    match method {
        // closure(name) → inline arrow function from pre-compiled closure body
        "closure" if args.len() == 1 => {
            if let JsExpr::Literal(Constant::String(ref name)) = args[0] {
                if let Some(closure_func) = closures.get(name.as_str()).cloned() {
                    let rewritten =
                        super::rewrite_twine_function(closure_func, closures);
                    return Some(JsExpr::ArrowFunction {
                        params: rewritten.params,
                        return_ty: rewritten.return_ty,
                        body: rewritten.body,
                        has_rest_param: rewritten.has_rest_param,
                        cast_as: None,
                        infer_param_types: false,
                    });
                }
            }
            None
        }
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
    use crate::js_ast::{JsExpr, JsFunction, JsStmt};
    use reincarnate_core::ir::value::Constant;
    use reincarnate_core::ir::{MethodKind, Type, Visibility};

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

    fn no_closures() -> HashMap<String, JsFunction> {
        HashMap::new()
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
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert!(matches!(extract_expr(&func), JsExpr::New { .. }));
    }

    #[test]
    fn rewrite_typeof() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "typeof".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert!(matches!(extract_expr(&func), JsExpr::TypeOf(_)));
    }

    #[test]
    fn rewrite_def_ndef() {
        let def = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "def".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = super::super::rewrite_twine_function(func_with_expr(def), &no_closures());
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
        let func = super::super::rewrite_twine_function(func_with_expr(ndef), &no_closures());
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
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
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
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert!(matches!(extract_expr(&func), JsExpr::In { .. }));
    }

    #[test]
    fn passthrough_state_get() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.State".into(),
            method: "get".into(),
            args: vec![JsExpr::Literal(Constant::String("name".into()))],
        };
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert!(matches!(extract_expr(&func), JsExpr::SystemCall { .. }));
    }

    #[test]
    fn passthrough_engine_eval() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "eval".into(),
            args: vec![JsExpr::Literal(Constant::String("code".into()))],
        };
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert!(matches!(extract_expr(&func), JsExpr::SystemCall { .. }));
    }

    #[test]
    fn rewrite_to_string() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "to_string".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
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
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert!(matches!(extract_expr(&func), JsExpr::Delete { .. }));
    }

    #[test]
    fn rewrite_is_nullish() {
        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "is_nullish".into(),
            args: vec![JsExpr::Var("x".into())],
        };
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert!(matches!(
            extract_expr(&func),
            JsExpr::Cmp {
                kind: CmpKind::Eq,
                ..
            }
        ));
    }

    #[test]
    fn rewrite_closure_inline() {
        let mut closures = HashMap::new();
        closures.insert(
            "test_arrow_0".to_string(),
            JsFunction {
                name: "test_arrow_0".into(),
                params: vec![("x".into(), Type::Dynamic)],
                return_ty: Type::Dynamic,
                body: vec![JsStmt::Return(Some(JsExpr::Var("x".into())))],
                is_generator: false,
                visibility: Visibility::Private,
                method_kind: MethodKind::Closure,
                has_rest_param: false,
            },
        );

        let expr = JsExpr::SystemCall {
            system: "SugarCube.Engine".into(),
            method: "closure".into(),
            args: vec![JsExpr::Literal(Constant::String("test_arrow_0".into()))],
        };
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &closures);
        match extract_expr(&func) {
            JsExpr::ArrowFunction { params, body, .. } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].0, "x");
                assert_eq!(body.len(), 1);
            }
            other => panic!("expected ArrowFunction, got {other:?}"),
        }
    }
}
