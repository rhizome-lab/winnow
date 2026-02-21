//! Harlowe.Engine JsExpr → JsExpr rewrites.
//!
//! Eliminates megamorphic dispatch in `math()`, `color_op()`, and
//! `collection_op()` by resolving the dispatch string at compile time:
//!
//! - `math("round", x)` → `Math.round(x)` (JS built-in)
//! - `not(x)` → `!x` (JS operator)
//! - `color_op("rgb", r, g, b)` → `Colors.rgb(r, g, b)` (namespace object)
//! - `collection_op("sorted", ...)` → `Collections.sorted(...)` (namespace object)
//! - `collection_op("find", pred_ref, ...)` → `Collections.find((x) => ..., ...)` (inlined closure)

use std::collections::HashMap;

use crate::js_ast::{JsExpr, JsFunction};
use reincarnate_core::ir::value::Constant;

/// Returns the bare function/namespace names that a `Harlowe.Engine` rewrite
/// will introduce, if any. Used by import generation to emit correct imports.
pub(super) fn rewrite_introduced_calls(method: &str) -> &'static [&'static str] {
    match method {
        // math → Math.* (global), not → !x (operator), lerp → stays as SystemCall
        "math" | "not" | "lerp" => &[],
        "color_op" => &["Colors"],
        "collection_op" => &["Collections"],
        "str_op" => &["StringOps"],
        _ => &[],
    }
}

/// Try to rewrite a `Harlowe.Engine.*` SystemCall.
pub(super) fn try_rewrite(
    method: &str,
    args: &mut Vec<JsExpr>,
    closures: &HashMap<String, JsFunction>,
) -> Option<JsExpr> {
    match method {
        "not" => try_rewrite_not(args),
        "math" => try_rewrite_math(args),
        "color_op" => try_rewrite_color_op(args),
        "collection_op" => try_rewrite_collection_op(args, closures),
        "str_op" => try_rewrite_str_op(args),
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
        "round" | "floor" | "ceil" | "abs" | "sqrt" | "sin" | "cos" | "tan"
        | "log" | "log10" | "log2" | "exp" | "sign" | "trunc" => {
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
/// `collection_op("find", pred_ref, ...)` → `Collections.find((x) => ..., ...)`
///
/// When the predicate argument is a `Var` referencing a closure in the
/// `closures` map, it is inlined as an arrow function at the call site.
/// This produces readable output and enables TypeScript contextual inference.
fn try_rewrite_collection_op(
    args: &mut Vec<JsExpr>,
    closures: &HashMap<String, JsFunction>,
) -> Option<JsExpr> {
    let name = extract_dispatch_name(args)?;
    let mut remaining = std::mem::take(args);
    let field = kebab_to_camel(&name);

    // For predicate ops, try to inline a closure reference as an arrow function.
    if is_predicate_op(&name) && !remaining.is_empty() {
        if let Some(arrow) = try_inline_closure(&remaining[0], closures) {
            remaining[0] = arrow;
        } else if let JsExpr::ArrowFunction { infer_param_types, .. } = &mut remaining[0] {
            // Already-inlined arrow (from MakeClosure → closure rewrite with zero captures):
            // enable contextual type inference so TypeScript omits `: any` on params.
            *infer_param_types = true;
        }
        // IIFE (captures present): TypeScript can still infer the inner arrow's param
        // types from context, so no annotation fixup is needed.
    }

    Some(JsExpr::Call {
        callee: Box::new(JsExpr::Field {
            object: Box::new(JsExpr::Var("Collections".into())),
            field,
        }),
        args: remaining,
    })
}

/// Map Harlowe str_op dispatch names to `StringOps` camelCase method names.
fn str_op_to_method(name: &str) -> String {
    match name {
        "str-reversed" | "string-reversed" => "strReversed".into(),
        "str-nth" | "string-nth" => "strNth".into(),
        "str-repeated" | "string-repeated" => "strRepeated".into(),
        "str-find" | "string-find" => "strFind".into(),
        "str-replaced" | "string-replaced" | "replaced" => "strReplaced".into(),
        "digit-format" => "digitFormat".into(),
        // upperfirst, lowerfirst, trimmed, words, plural are already valid JS identifiers
        _ => name.to_string(),
    }
}

/// `str_op("trimmed", s)` → `StringOps.trimmed(s)`, etc.
///
/// Special case: `str_op("str-nth", N)` with only 1 arg (no string) is the Harlowe
/// "string-nth value" used as a property accessor: `$arr's (str-nth: N)`.
/// In that context the accessor is just the 1-based index N itself — `get_property`
/// and `set_property` already handle numeric keys with 1-based indexing.
fn try_rewrite_str_op(args: &mut Vec<JsExpr>) -> Option<JsExpr> {
    let name = extract_dispatch_name(args)?;
    let remaining = std::mem::take(args);

    // Partial application: (str-nth: N) without the string — emit just N.
    if matches!(name.as_str(), "str-nth" | "string-nth") && remaining.len() == 1 {
        return Some(remaining.into_iter().next().unwrap());
    }

    let method = str_op_to_method(&name);
    Some(JsExpr::Call {
        callee: Box::new(JsExpr::Field {
            object: Box::new(JsExpr::Var("StringOps".into())),
            field: method,
        }),
        args: remaining,
    })
}

/// Returns true for collection ops whose first arg may be a predicate/transform lambda.
/// `sorted` is included because Harlowe allows an optional `via` lambda as first arg.
fn is_predicate_op(name: &str) -> bool {
    matches!(
        name,
        "find" | "some-pass" | "all-pass" | "none-pass" | "count" | "altered" | "sorted"
    )
}

/// If `expr` is a `Var` referencing a closure in `closures`, return an
/// inlined `ArrowFunction` with `infer_param_types: true`.
fn try_inline_closure(
    expr: &JsExpr,
    closures: &HashMap<String, JsFunction>,
) -> Option<JsExpr> {
    let name = match expr {
        JsExpr::Var(n) => n,
        _ => return None,
    };
    let closure = closures.get(name.as_str())?;
    // Inline: use the closure's params/return_ty/body as an arrow function.
    // infer_param_types: true lets TypeScript infer Dynamic params contextually.
    Some(JsExpr::ArrowFunction {
        params: closure.params.clone(),
        return_ty: closure.return_ty.clone(),
        body: closure.body.clone(),
        has_rest_param: closure.has_rest_param,
        cast_as: None,
        infer_param_types: true,
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
            num_capture_params: 0,
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
    fn rewrite_math_trunc() {
        let expr = engine_call("math", vec![str_lit("trunc"), var("x")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(matches!(object.as_ref(), JsExpr::Var(n) if n == "Math"));
                        assert_eq!(field, "trunc");
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    // --- Phase 4: str_op ---

    fn assert_stringops_call(func: &JsFunction, expected_method: &str, expected_arg_count: usize) {
        match extract_expr(func) {
            JsExpr::Call { callee, args } => {
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(matches!(object.as_ref(), JsExpr::Var(n) if n == "StringOps"),
                            "expected StringOps, got {object:?}");
                        assert_eq!(field, expected_method);
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), expected_arg_count);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_str_op_upperfirst() {
        let expr = engine_call("str_op", vec![str_lit("upperfirst"), var("s")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert_stringops_call(&func, "upperfirst", 1);
    }

    #[test]
    fn rewrite_str_op_trimmed() {
        let expr = engine_call("str_op", vec![str_lit("trimmed"), var("s")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert_stringops_call(&func, "trimmed", 1);
    }

    #[test]
    fn rewrite_str_op_str_reversed() {
        let expr = engine_call("str_op", vec![str_lit("str-reversed"), var("s")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert_stringops_call(&func, "strReversed", 1);
    }

    #[test]
    fn rewrite_str_op_string_reversed_alias() {
        let expr = engine_call("str_op", vec![str_lit("string-reversed"), var("s")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert_stringops_call(&func, "strReversed", 1);
    }

    #[test]
    fn rewrite_str_op_digit_format() {
        let expr = engine_call("str_op", vec![str_lit("digit-format"), var("fmt"), var("n")]);
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        assert_stringops_call(&func, "digitFormat", 2);
    }

    #[test]
    fn introduced_calls_engine() {
        assert!(rewrite_introduced_calls("math").is_empty());
        assert!(rewrite_introduced_calls("not").is_empty());
        assert!(rewrite_introduced_calls("lerp").is_empty());
        assert_eq!(rewrite_introduced_calls("color_op"), &["Colors"]);
        assert_eq!(rewrite_introduced_calls("collection_op"), &["Collections"]);
        assert_eq!(rewrite_introduced_calls("str_op"), &["StringOps"]);
        assert!(rewrite_introduced_calls("plus").is_empty());
    }

    fn make_pred_closure(name: &str) -> JsFunction {
        use crate::js_ast::JsStmt;
        use reincarnate_core::ir::MethodKind;
        JsFunction {
            name: name.into(),
            params: vec![("_x".into(), Type::Dynamic)],
            return_ty: Type::Bool,
            body: vec![JsStmt::Return(Some(JsExpr::Literal(Constant::Bool(true))))],
            is_generator: false,
            visibility: Visibility::Public,
            method_kind: MethodKind::Closure,
            has_rest_param: false,
            num_capture_params: 0,
        }
    }

    #[test]
    fn rewrite_collection_op_inlines_closure() {
        // collection_op("find", pred_lambda_ref, arr) where pred_lambda_ref is in closures
        // → Collections.find((x) => { return true; }, arr) with infer_param_types: true
        let pred_name = "passage_test_lambda_1";
        let mut closures = std::collections::HashMap::new();
        closures.insert(pred_name.to_string(), make_pred_closure(pred_name));

        let expr = engine_call(
            "collection_op",
            vec![str_lit("find"), var(pred_name), var("arr")],
        );
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &closures);
        match extract_expr(&func) {
            JsExpr::Call { callee, args } => {
                // Callee is Collections.find
                match callee.as_ref() {
                    JsExpr::Field { object, field } => {
                        assert!(matches!(object.as_ref(), JsExpr::Var(n) if n == "Collections"));
                        assert_eq!(field, "find");
                    }
                    other => panic!("expected Field, got {other:?}"),
                }
                assert_eq!(args.len(), 2, "should be (arrow_fn, arr)");
                // First arg should now be an inlined arrow function
                match &args[0] {
                    JsExpr::ArrowFunction {
                        params,
                        return_ty,
                        infer_param_types,
                        ..
                    } => {
                        assert_eq!(params.len(), 1);
                        assert!(
                            matches!(&params[0].1, Type::Dynamic),
                            "param should be Dynamic (inferred by TS)"
                        );
                        assert_eq!(*return_ty, Type::Bool);
                        assert!(infer_param_types, "should omit `: any` for TS inference");
                    }
                    other => panic!("expected ArrowFunction, got {other:?}"),
                }
                // Second arg is arr
                assert!(matches!(&args[1], JsExpr::Var(n) if n == "arr"));
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn rewrite_collection_op_no_inline_for_non_closure_var() {
        // When pred arg is a Var NOT in closures, pass it through unchanged.
        let expr = engine_call(
            "collection_op",
            vec![str_lit("find"), var("myPred"), var("arr")],
        );
        let func = super::super::rewrite_twine_function(func_with_expr(expr), &no_closures());
        match extract_expr(&func) {
            JsExpr::Call { args, .. } => {
                assert!(matches!(&args[0], JsExpr::Var(n) if n == "myPred"));
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }
}
