//! Harlowe.Engine JsExpr → JsExpr rewrites.
//!
//! Eliminates megamorphic dispatch in `math()`, `color_op()`, and
//! `collection_op()` by resolving the dispatch string at compile time:
//!
//! - `math("round", x)` → `Math.round(x)` (JS built-in)
//! - `not(x)` → `!x` (JS operator)
//! - `color_op("rgb", r, g, b)` → `Colors.rgb(r, g, b)` (namespace object)
//! - `collection_op("sorted", ...)` → `Collections.sorted(...)` (namespace object)

use crate::js_ast::JsExpr;
use reincarnate_core::ir::value::Constant;

/// Returns the bare function/namespace names that a `Harlowe.Engine` rewrite
/// will introduce, if any. Used by import generation to emit correct imports.
pub(super) fn rewrite_introduced_calls(method: &str) -> &'static [&'static str] {
    match method {
        // math → Math.* (global), not → !x (operator), lerp → stays as SystemCall
        "math" | "not" | "lerp" => &[],
        "color_op" => &["Colors"],
        "collection_op" => &["Collections"],
        _ => &[],
    }
}

/// Try to rewrite a `Harlowe.Engine.*` SystemCall.
pub(super) fn try_rewrite(method: &str, args: &mut Vec<JsExpr>) -> Option<JsExpr> {
    match method {
        "not" => try_rewrite_not(args),
        "math" => try_rewrite_math(args),
        "color_op" => try_rewrite_color_op(args),
        "collection_op" => try_rewrite_collection_op(args),
        _ => None,
    }
}

/// `not(x)` → `!x`
fn try_rewrite_not(args: &mut Vec<JsExpr>) -> Option<JsExpr> {
    if args.len() == 1 {
        Some(JsExpr::Not(Box::new(args.pop().unwrap())))
    } else {
        None
    }
}

/// Extract the first argument as a string literal, returning the string
/// and the remaining args.
fn extract_dispatch_name(args: &mut Vec<JsExpr>) -> Option<String> {
    if args.is_empty() {
        return None;
    }
    match &args[0] {
        JsExpr::Literal(Constant::String(s)) => {
            let name = s.clone();
            args.remove(0);
            Some(name)
        }
        _ => None,
    }
}

/// Build `Math.{method}(args...)`.
fn math_call(method: &str, args: Vec<JsExpr>) -> JsExpr {
    JsExpr::Call {
        callee: Box::new(JsExpr::Field {
            object: Box::new(JsExpr::Var("Math".into())),
            field: method.into(),
        }),
        args,
    }
}

/// `math("round", x)` → `Math.round(x)`, etc.
fn try_rewrite_math(args: &mut Vec<JsExpr>) -> Option<JsExpr> {
    let name = extract_dispatch_name(args)?;
    let remaining = std::mem::take(args);

    match name.as_str() {
        // Single-arg Math methods
        "round" | "floor" | "ceil" | "abs" | "sqrt" | "sin" | "cos" | "tan" | "log" | "sign" => {
            Some(math_call(&name, remaining))
        }
        // Two-arg: Math.pow(a, b)
        "pow" => Some(math_call("pow", remaining)),
        // Variadic: Math.min(...) / Math.max(...)
        "min" | "max" => Some(math_call(&name, remaining)),
        // clamp(x, lo, hi) → Math.min(Math.max(x, lo), hi)
        "clamp" if remaining.len() == 3 => {
            let mut it = remaining.into_iter();
            let x = it.next().unwrap();
            let lo = it.next().unwrap();
            let hi = it.next().unwrap();
            let inner = math_call("max", vec![x, lo]);
            Some(math_call("min", vec![inner, hi]))
        }
        // lerp → leave as SystemCall (can't inline without duplicating `a`)
        "lerp" => {
            // Re-insert the name as first arg and return None so the
            // SystemCall is preserved as-is (HarloweEngine.lerp fallback).
            let mut restored = vec![JsExpr::Literal(Constant::String(name))];
            restored.extend(remaining);
            *args = restored;
            None
        }
        // Unknown → restore args and return None
        _ => {
            let mut restored = vec![JsExpr::Literal(Constant::String(name))];
            restored.extend(remaining);
            *args = restored;
            None
        }
    }
}

/// `color_op("rgb", r, g, b)` → `Colors.rgb(r, g, b)`
fn try_rewrite_color_op(args: &mut Vec<JsExpr>) -> Option<JsExpr> {
    let name = extract_dispatch_name(args)?;
    let remaining = std::mem::take(args);
    Some(JsExpr::Call {
        callee: Box::new(JsExpr::Field {
            object: Box::new(JsExpr::Var("Colors".into())),
            field: name,
        }),
        args: remaining,
    })
}

/// Map kebab-case Harlowe names to camelCase JS identifiers.
fn kebab_to_camel(name: &str) -> String {
    match name {
        "some-pass" => "somePass".into(),
        "all-pass" => "allPass".into(),
        "none-pass" => "nonePass".into(),
        _ => name.to_string(),
    }
}

/// `collection_op("sorted", ...)` → `Collections.sorted(...)`
fn try_rewrite_collection_op(args: &mut Vec<JsExpr>) -> Option<JsExpr> {
    let name = extract_dispatch_name(args)?;
    let remaining = std::mem::take(args);
    let field = kebab_to_camel(&name);
    Some(JsExpr::Call {
        callee: Box::new(JsExpr::Field {
            object: Box::new(JsExpr::Var("Collections".into())),
            field,
        }),
        args: remaining,
    })
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

    fn no_closures() -> std::collections::HashMap<String, JsFunction> {
        std::collections::HashMap::new()
    }

    fn extract_expr(func: &JsFunction) -> &JsExpr {
        match &func.body[0] {
            JsStmt::Expr(e) => e,
            _ => panic!("expected Expr statement"),
        }
    }

    fn engine_call(method: &str, args: Vec<JsExpr>) -> JsExpr {
        JsExpr::SystemCall {
            system: "Harlowe.Engine".into(),
            method: method.into(),
            args,
        }
    }

    fn str_lit(s: &str) -> JsExpr {
        JsExpr::Literal(Constant::String(s.into()))
    }

    fn var(name: &str) -> JsExpr {
        JsExpr::Var(name.into())
    }

    // --- Phase 1: not + math ---

    #[test]
    fn rewrite_not() {
        let expr = engine_call("not", vec![var("x")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Not(inner) => {
                assert!(matches!(inner.as_ref(), JsExpr::Var(n) if n == "x"));
            }
            other => panic!("expected Not, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_math_round() {
        let expr = engine_call("math", vec![str_lit("round"), var("x")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(matches!(object.as_ref(), JsExpr::Var(n) if n == "Math"));
                        assert_eq!(field, "round");
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), 1);
                assert!(matches!(&args[0], JsExpr::Var(n) if n == "x"));
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_math_min() {
        let expr = engine_call("math", vec![str_lit("min"), var("a"), var("b")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(matches!(object.as_ref(), JsExpr::Var(n) if n == "Math"));
                        assert_eq!(field, "min");
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_math_clamp() {
        let expr = engine_call(
            "math",
            vec![str_lit("clamp"), var("x"), var("lo"), var("hi")],
        );
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        // clamp(x, lo, hi) → Math.min(Math.max(x, lo), hi)
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                // Outer: Math.min(inner, hi)
                match callee.as_ref() {
                    JsExpr::Field { field, .. } => assert_eq!(field, "min"),
                    other => panic!("expected Field(min), got {other:?}"),
                }
                assert_eq!(args.len(), 2);
                // Inner: Math.max(x, lo)
                match &args[0] {
                    JsExpr::Call { callee, args } => {
                        match callee.as_ref() {
                            JsExpr::Field { field, .. } => assert_eq!(field, "max"),
                            other => panic!("expected Field(max), got {other:?}"),
                        }
                        assert_eq!(args.len(), 2);
                    }
                    other => panic!("expected inner Call, got {other:?}"),
                }
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_math_lerp_passthrough() {
        let expr = engine_call(
            "math",
            vec![str_lit("lerp"), var("a"), var("b"), var("t")],
        );
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        // lerp should NOT be rewritten — stays as SystemCall
        match extract_expr(&func) {
            JsExpr::SystemCall {
                system, method, ..
            } => {
                assert_eq!(system, "Harlowe.Engine");
                assert_eq!(method, "math");
            }
            other => panic!("expected SystemCall passthrough, got {other:?}"),
        }
    }

    #[test]
    fn passthrough_unknown_math() {
        let expr = engine_call("math", vec![str_lit("??"), var("x")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        // Unknown math function should NOT be rewritten
        match extract_expr(&func) {
            JsExpr::SystemCall {
                system, method, ..
            } => {
                assert_eq!(system, "Harlowe.Engine");
                assert_eq!(method, "math");
            }
            other => panic!("expected SystemCall passthrough, got {other:?}"),
        }
    }

    // --- Phase 2: color_op ---

    #[test]
    fn rewrite_color_op_rgb() {
        let expr = engine_call(
            "color_op",
            vec![str_lit("rgb"), var("r"), var("g"), var("b")],
        );
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(matches!(object.as_ref(), JsExpr::Var(n) if n == "Colors"));
                        assert_eq!(field, "rgb");
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), 3);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    // --- Phase 3: collection_op ---

    #[test]
    fn rewrite_collection_op_sorted() {
        let expr = engine_call("collection_op", vec![str_lit("sorted"), var("a"), var("b")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(
                            matches!(object.as_ref(), JsExpr::Var(n) if n == "Collections")
                        );
                        assert_eq!(field, "sorted");
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_collection_op_kebab_case() {
        let expr = engine_call(
            "collection_op",
            vec![str_lit("some-pass"), var("pred"), var("a")],
        );
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(
                            matches!(object.as_ref(), JsExpr::Var(n) if n == "Collections")
                        );
                        assert_eq!(field, "somePass");
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn introduced_calls_engine() {
        assert!(rewrite_introduced_calls("math").is_empty());
        assert!(rewrite_introduced_calls("not").is_empty());
        assert!(rewrite_introduced_calls("lerp").is_empty());
        assert_eq!(rewrite_introduced_calls("color_op"), &["Colors"]);
        assert_eq!(rewrite_introduced_calls("collection_op"), &["Collections"]);
        assert!(rewrite_introduced_calls("plus").is_empty());
    }
}
