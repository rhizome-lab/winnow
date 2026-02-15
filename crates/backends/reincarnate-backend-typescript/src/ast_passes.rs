//! Engine-agnostic AST passes for the TypeScript backend.
//!
//! These run after engine-specific rewrites and before printing.

use std::collections::HashMap;

use reincarnate_core::ir::inst::CmpKind;
use reincarnate_core::ir::value::Constant;
use reincarnate_core::ir::{CastKind, Type};

use crate::js_ast::{JsExpr, JsFunction, JsStmt};
use crate::types::ts_type;

/// Recover `switch` statements from if-chains where every condition compares
/// the same expression against a distinct constant.
///
/// Handles two patterns:
/// - **Sequential if**: consecutive `if (EXPR === C) { ... }` with empty else
/// - **Nested if-else-if**: `if (EXPR === C1) { ... } else if (EXPR === C2) { ... } else { ... }`
///
/// NOTE: This transformation may not be semantics-preserving if the discriminant
/// expression has side effects, since `switch` evaluates it once while the
/// original if-chain evaluates it N times. We only apply this when
/// `is_stable_expr` returns true, but that check is conservative and syntactic
/// — it cannot rule out all side effects (e.g. getters on fields).
pub fn recover_switch_statements(body: &mut Vec<JsStmt>) {
    // Try the nested if-else-if pattern FIRST (outside-in), before recursing
    // into children. Otherwise inner if-else chains get converted to Switch
    // nodes and the outer chain no longer matches the if-else-if pattern.
    for stmt in body.iter_mut() {
        try_recover_nested_if_else(stmt);
    }

    // Then recurse into all nested bodies.
    for stmt in body.iter_mut() {
        recurse_into_stmt(stmt);
    }

    // Try the sequential-if pattern on runs of consecutive statements.
    try_recover_sequential_ifs(body);
}

/// Recurse into all sub-bodies of a statement.
fn recurse_into_stmt(stmt: &mut JsStmt) {
    match stmt {
        JsStmt::If {
            then_body,
            else_body,
            ..
        } => {
            recover_switch_statements(then_body);
            recover_switch_statements(else_body);
        }
        JsStmt::While { body, .. }
        | JsStmt::Loop { body }
        | JsStmt::ForOf { body, .. } => {
            recover_switch_statements(body);
        }
        JsStmt::For {
            init,
            body,
            update,
            ..
        } => {
            recover_switch_statements(init);
            recover_switch_statements(body);
            recover_switch_statements(update);
        }
        JsStmt::Switch {
            cases,
            default_body,
            ..
        } => {
            for (_, case_body) in cases {
                recover_switch_statements(case_body);
            }
            recover_switch_statements(default_body);
        }
        JsStmt::Dispatch { blocks, .. } => {
            for (_, block_body) in blocks {
                recover_switch_statements(block_body);
            }
        }
        // Recurse into expressions that contain statement bodies (e.g. arrow fns).
        JsStmt::VarDecl { init: Some(e), .. }
        | JsStmt::Assign { value: e, .. }
        | JsStmt::Expr(e)
        | JsStmt::Return(Some(e))
        | JsStmt::Throw(e) => {
            recurse_into_expr(e);
        }
        JsStmt::CompoundAssign { value, .. } => {
            recurse_into_expr(value);
        }
        _ => {}
    }
}

/// Recurse into an expression to find nested statement bodies (arrow functions).
fn recurse_into_expr(expr: &mut JsExpr) {
    match expr {
        JsExpr::ArrowFunction { body, .. } => {
            recover_switch_statements(body);
        }
        JsExpr::Binary { lhs, rhs, .. }
        | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs }
        | JsExpr::LogicalAnd { lhs, rhs } => {
            recurse_into_expr(lhs);
            recurse_into_expr(rhs);
        }
        JsExpr::Field { object, .. } => recurse_into_expr(object),
        JsExpr::Index { collection, index } => {
            recurse_into_expr(collection);
            recurse_into_expr(index);
        }
        JsExpr::Call { callee, args } => {
            recurse_into_expr(callee);
            for arg in args {
                recurse_into_expr(arg);
            }
        }
        JsExpr::New { callee, args } => {
            recurse_into_expr(callee);
            for arg in args {
                recurse_into_expr(arg);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            recurse_into_expr(cond);
            recurse_into_expr(then_val);
            recurse_into_expr(else_val);
        }
        JsExpr::Unary { expr, .. }
        | JsExpr::Cast { expr, .. }
        | JsExpr::TypeCheck { expr, .. }
        | JsExpr::Not(expr)
        | JsExpr::PostIncrement(expr)
        | JsExpr::TypeOf(expr)
        | JsExpr::GeneratorResume(expr) => {
            recurse_into_expr(expr);
        }
        JsExpr::Yield(Some(expr)) => recurse_into_expr(expr),
        JsExpr::ArrayInit(elems) | JsExpr::TupleInit(elems) => {
            for e in elems {
                recurse_into_expr(e);
            }
        }
        JsExpr::ObjectInit(fields) => {
            for (_, e) in fields {
                recurse_into_expr(e);
            }
        }
        JsExpr::SystemCall { args, .. }
        | JsExpr::SuperCall(args)
        | JsExpr::GeneratorCreate { args, .. } => {
            for arg in args {
                recurse_into_expr(arg);
            }
        }
        JsExpr::SuperMethodCall { args, .. } => {
            for arg in args {
                recurse_into_expr(arg);
            }
        }
        JsExpr::In { key, object } => {
            recurse_into_expr(key);
            recurse_into_expr(object);
        }
        JsExpr::Delete { object, key } => {
            recurse_into_expr(object);
            recurse_into_expr(key);
        }
        JsExpr::SuperSet { value, .. } => recurse_into_expr(value),
        _ => {}
    }
}

/// Try to recover a switch from a nested if-else-if chain rooted at `stmt`.
///
/// Pattern: `if (EXPR === C1) { body1 } else if (EXPR === C2) { body2 } else { default }`
fn try_recover_nested_if_else(stmt: &mut JsStmt) {
    let mut cases: Vec<(Constant, Vec<JsStmt>)> = Vec::new();
    let mut discriminant: Option<&JsExpr> = None;

    // Walk the if-else-if chain without consuming the statement.
    let mut current = &*stmt;
    let default_body;
    loop {
        if let JsStmt::If {
            cond,
            then_body,
            else_body,
        } = current
        {
            if let Some((disc, constant)) = extract_eq_constant(cond) {
                match &discriminant {
                    None => {
                        if !is_stable_expr(disc) {
                            return;
                        }
                        discriminant = Some(disc);
                    }
                    Some(prev) => {
                        if !exprs_structurally_equal(prev, disc) {
                            return;
                        }
                    }
                }
                cases.push((constant.clone(), then_body.clone()));

                // Continue down the else chain.
                if else_body.len() == 1 {
                    current = &else_body[0];
                    continue;
                } else {
                    // else_body is the default (possibly empty).
                    default_body = else_body.clone();
                    break;
                }
            } else {
                return;
            }
        } else {
            return;
        }
    }

    if cases.len() < 2 {
        return;
    }

    if !all_constants_distinct(&cases) {
        return;
    }

    let disc = discriminant.unwrap().clone();
    *stmt = JsStmt::Switch {
        value: disc,
        cases,
        default_body,
    };
}

/// Try to recover switches from runs of consecutive `if` statements with empty
/// else bodies that all compare the same expression against distinct constants.
fn try_recover_sequential_ifs(body: &mut Vec<JsStmt>) {
    let mut i = 0;
    while i < body.len() {
        // Find the start of a potential run.
        let run_start = i;
        let mut cases: Vec<(Constant, Vec<JsStmt>)> = Vec::new();
        let mut discriminant: Option<&JsExpr> = None;

        while i < body.len() {
            if let JsStmt::If {
                cond,
                then_body,
                else_body,
            } = &body[i]
            {
                if !else_body.is_empty() {
                    break;
                }
                if let Some((disc, constant)) = extract_eq_constant(cond) {
                    match &discriminant {
                        None => {
                            if !is_stable_expr(disc) {
                                break;
                            }
                            discriminant = Some(disc);
                        }
                        Some(prev) => {
                            if !exprs_structurally_equal(prev, disc) {
                                break;
                            }
                        }
                    }
                    cases.push((constant.clone(), then_body.clone()));
                    i += 1;
                    continue;
                }
            }
            break;
        }

        if cases.len() >= 2 {
            let disc = discriminant.unwrap().clone();
            let switch_stmt = JsStmt::Switch {
                value: disc,
                cases,
                default_body: vec![],
            };
            // Replace the run [run_start..i) with the single switch.
            body.splice(run_start..i, std::iter::once(switch_stmt));
            // After splice, the switch is at run_start; advance past it.
            i = run_start + 1;
        } else {
            // No run found starting at run_start; advance.
            i = if i == run_start { run_start + 1 } else { i };
        }
    }
}

/// If `cond` is `EXPR === CONST` (or `CONST === EXPR`), return `(expr, constant)`.
fn extract_eq_constant(cond: &JsExpr) -> Option<(&JsExpr, &Constant)> {
    if let JsExpr::Cmp {
        kind: CmpKind::Eq,
        lhs,
        rhs,
    } = cond
    {
        if let JsExpr::Literal(c) = rhs.as_ref() {
            return Some((lhs.as_ref(), c));
        }
        if let JsExpr::Literal(c) = lhs.as_ref() {
            return Some((rhs.as_ref(), c));
        }
    }
    None
}

/// Conservative check: returns true only for expressions that are clearly
/// free of side effects. This is a syntactic check and cannot rule out all
/// side effects (e.g. property getters).
fn is_stable_expr(expr: &JsExpr) -> bool {
    match expr {
        JsExpr::Var(_) | JsExpr::This | JsExpr::Literal(_) => true,
        JsExpr::Field { object, .. } => is_stable_expr(object),
        JsExpr::Index { collection, index } => {
            is_stable_expr(collection) && is_stable_expr(index)
        }
        _ => false,
    }
}

/// Recursive structural equality for `JsExpr`.
///
/// Conservative: returns false for any variant pair we don't explicitly handle,
/// which prevents incorrect switch recovery rather than risking a wrong match.
fn exprs_structurally_equal(a: &JsExpr, b: &JsExpr) -> bool {
    match (a, b) {
        (JsExpr::Var(x), JsExpr::Var(y)) => x == y,
        (JsExpr::This, JsExpr::This) => true,
        (JsExpr::Literal(x), JsExpr::Literal(y)) => x == y,
        (
            JsExpr::Field {
                object: o1,
                field: f1,
            },
            JsExpr::Field {
                object: o2,
                field: f2,
            },
        ) => f1 == f2 && exprs_structurally_equal(o1, o2),
        (
            JsExpr::Index {
                collection: c1,
                index: i1,
            },
            JsExpr::Index {
                collection: c2,
                index: i2,
            },
        ) => exprs_structurally_equal(c1, c2) && exprs_structurally_equal(i1, i2),
        (
            JsExpr::Cmp {
                kind: k1,
                lhs: l1,
                rhs: r1,
            },
            JsExpr::Cmp {
                kind: k2,
                lhs: l2,
                rhs: r2,
            },
        ) => k1 == k2 && exprs_structurally_equal(l1, l2) && exprs_structurally_equal(r1, r2),
        (
            JsExpr::Binary {
                op: o1,
                lhs: l1,
                rhs: r1,
            },
            JsExpr::Binary {
                op: o2,
                lhs: l2,
                rhs: r2,
            },
        ) => o1 == o2 && exprs_structurally_equal(l1, l2) && exprs_structurally_equal(r1, r2),
        (
            JsExpr::Unary { op: o1, expr: e1 },
            JsExpr::Unary { op: o2, expr: e2 },
        ) => o1 == o2 && exprs_structurally_equal(e1, e2),
        (JsExpr::Not(e1), JsExpr::Not(e2)) => exprs_structurally_equal(e1, e2),
        _ => false,
    }
}

/// Check that all case constants in the list are distinct.
fn all_constants_distinct(cases: &[(Constant, Vec<JsStmt>)]) -> bool {
    for i in 0..cases.len() {
        for j in (i + 1)..cases.len() {
            if cases[i].0 == cases[j].0 {
                return false;
            }
        }
    }
    true
}

// ---------------------------------------------------------------------------
// Redundant AsType cast elimination
// ---------------------------------------------------------------------------

/// Strip `x as T` casts where the variable `x` is already declared with type
/// `T`. These arise because AVM2 has explicit coerce/astype opcodes that
/// survive the IR `red_cast_elim` pass (which only checks IR-level types, not
/// the emitter's declaration types).
pub fn strip_redundant_casts(func: &mut JsFunction) {
    let mut var_types: HashMap<String, Type> = HashMap::new();
    // Collect param types.
    for (name, ty) in &func.params {
        if *ty != Type::Dynamic {
            var_types.insert(name.clone(), ty.clone());
        }
    }
    // Collect local variable types from declarations.
    collect_var_types(&func.body, &mut var_types);
    // Debug: count what we found
    // Strip redundant casts.
    strip_casts_in_body(&mut func.body, &var_types);
}

fn collect_var_types(body: &[JsStmt], var_types: &mut HashMap<String, Type>) {
    for stmt in body {
        match stmt {
            JsStmt::VarDecl {
                name,
                ty: Some(ty),
                ..
            } if *ty != Type::Dynamic => {
                var_types.insert(name.clone(), ty.clone());
            }
            // When ty is None but init is a Cast, the printer uses the cast
            // type as the annotation. Collect that type too.
            JsStmt::VarDecl {
                name,
                ty: None,
                init:
                    Some(JsExpr::Cast {
                        ty: cast_ty,
                        kind: CastKind::AsType,
                        ..
                    }),
                ..
            } if *cast_ty != Type::Dynamic
                && !matches!(cast_ty, Type::Struct(_) | Type::Enum(_)) =>
            {
                var_types.insert(name.clone(), cast_ty.clone());
            }
            JsStmt::If {
                then_body,
                else_body,
                ..
            } => {
                collect_var_types(then_body, var_types);
                collect_var_types(else_body, var_types);
            }
            JsStmt::While { body, .. }
            | JsStmt::Loop { body }
            | JsStmt::ForOf { body, .. } => {
                collect_var_types(body, var_types);
            }
            JsStmt::For {
                init,
                body,
                update,
                ..
            } => {
                collect_var_types(init, var_types);
                collect_var_types(body, var_types);
                collect_var_types(update, var_types);
            }
            JsStmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    collect_var_types(case_body, var_types);
                }
                collect_var_types(default_body, var_types);
            }
            JsStmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    collect_var_types(block_body, var_types);
                }
            }
            _ => {}
        }
    }
}

/// Whether two types map to the same TypeScript type (e.g. Int(32),
/// Float(64), and Union([Int(64), Int(32)]) all map to `number`).
fn same_ts_type(a: &Type, b: &Type) -> bool {
    if a == b {
        return true;
    }
    ts_type(a) == ts_type(b)
}

fn strip_casts_in_body(body: &mut [JsStmt], var_types: &HashMap<String, Type>) {
    for stmt in body.iter_mut() {
        strip_casts_in_stmt(stmt, var_types);
    }
}

fn strip_casts_in_stmt(stmt: &mut JsStmt, var_types: &HashMap<String, Type>) {
    match stmt {
        JsStmt::VarDecl {
            init: Some(expr), ..
        }
        | JsStmt::Expr(expr)
        | JsStmt::Throw(expr) => {
            strip_casts_in_expr(expr, var_types);
        }
        JsStmt::Assign { target, value } => {
            strip_casts_in_expr(target, var_types);
            strip_casts_in_expr(value, var_types);
        }
        JsStmt::CompoundAssign { target, value, .. } => {
            strip_casts_in_expr(target, var_types);
            strip_casts_in_expr(value, var_types);
        }
        JsStmt::Return(Some(expr)) => {
            strip_casts_in_expr(expr, var_types);
        }
        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            strip_casts_in_expr(cond, var_types);
            strip_casts_in_body(then_body, var_types);
            strip_casts_in_body(else_body, var_types);
        }
        JsStmt::While { cond, body } => {
            strip_casts_in_expr(cond, var_types);
            strip_casts_in_body(body, var_types);
        }
        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => {
            strip_casts_in_body(init, var_types);
            strip_casts_in_expr(cond, var_types);
            strip_casts_in_body(update, var_types);
            strip_casts_in_body(body, var_types);
        }
        JsStmt::Loop { body } | JsStmt::ForOf { body, .. } => {
            strip_casts_in_body(body, var_types);
        }
        JsStmt::Switch {
            value,
            cases,
            default_body,
        } => {
            strip_casts_in_expr(value, var_types);
            for (_, case_body) in cases.iter_mut() {
                strip_casts_in_body(case_body, var_types);
            }
            strip_casts_in_body(default_body, var_types);
        }
        JsStmt::Dispatch { blocks, .. } => {
            for (_, block_body) in blocks.iter_mut() {
                strip_casts_in_body(block_body, var_types);
            }
        }
        _ => {}
    }
}

fn strip_casts_in_expr(expr: &mut JsExpr, var_types: &HashMap<String, Type>) {
    // First, check if this expr is a strippable cast.
    let should_strip = if let JsExpr::Cast {
        expr: inner,
        ty: cast_ty,
        kind,
    } = &*expr
    {
        if *kind == CastKind::AsType {
            is_cast_redundant(inner, cast_ty, var_types)
        } else {
            false
        }
    } else {
        false
    };

    if should_strip {
        // Unwrap the Cast to its inner expression.
        let inner = match std::mem::replace(expr, JsExpr::This) {
            JsExpr::Cast { expr: inner, .. } => *inner,
            _ => unreachable!(),
        };
        *expr = inner;
    }

    // Recurse into sub-expressions.
    match expr {
        JsExpr::Binary { lhs, rhs, .. }
        | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs }
        | JsExpr::LogicalAnd { lhs, rhs } => {
            strip_casts_in_expr(lhs, var_types);
            strip_casts_in_expr(rhs, var_types);
        }
        JsExpr::In { key, object } | JsExpr::Delete { object, key } => {
            strip_casts_in_expr(key, var_types);
            strip_casts_in_expr(object, var_types);
        }
        JsExpr::Unary { expr: inner, .. }
        | JsExpr::Not(inner)
        | JsExpr::PostIncrement(inner)
        | JsExpr::TypeOf(inner)
        | JsExpr::GeneratorResume(inner)
        | JsExpr::Cast { expr: inner, .. }
        | JsExpr::TypeCheck { expr: inner, .. } => {
            strip_casts_in_expr(inner, var_types);
        }
        JsExpr::Field { object, .. } => {
            strip_casts_in_expr(object, var_types);
        }
        JsExpr::Index {
            collection, index, ..
        } => {
            strip_casts_in_expr(collection, var_types);
            strip_casts_in_expr(index, var_types);
        }
        JsExpr::Call { callee, args } | JsExpr::New { callee, args } => {
            strip_casts_in_expr(callee, var_types);
            for arg in args.iter_mut() {
                strip_casts_in_expr(arg, var_types);
            }
        }
        JsExpr::SystemCall { args, .. } | JsExpr::GeneratorCreate { args, .. } => {
            for arg in args.iter_mut() {
                strip_casts_in_expr(arg, var_types);
            }
        }
        JsExpr::SuperCall(args)
        | JsExpr::SuperMethodCall { args, .. }
        | JsExpr::ArrayInit(args)
        | JsExpr::TupleInit(args) => {
            for arg in args.iter_mut() {
                strip_casts_in_expr(arg, var_types);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            strip_casts_in_expr(cond, var_types);
            strip_casts_in_expr(then_val, var_types);
            strip_casts_in_expr(else_val, var_types);
        }
        JsExpr::ObjectInit(pairs) => {
            for (_, val) in pairs.iter_mut() {
                strip_casts_in_expr(val, var_types);
            }
        }
        JsExpr::SuperSet { value, .. } => {
            strip_casts_in_expr(value, var_types);
        }
        JsExpr::Yield(Some(inner)) => {
            strip_casts_in_expr(inner, var_types);
        }
        JsExpr::ArrowFunction { body, .. } => {
            strip_casts_in_body(body, var_types);
        }
        _ => {}
    }
}

/// Check if a Cast is redundant because the inner expression's type already
/// matches the cast target.
fn is_cast_redundant(
    inner: &JsExpr,
    cast_ty: &Type,
    var_types: &HashMap<String, Type>,
) -> bool {
    // Only strip TS assertion forms (not runtime calls like asType, Number).
    if matches!(cast_ty, Type::Struct(_) | Type::Enum(_)) {
        return false;
    }
    if let Some(expr_ty) = infer_expr_type(inner, var_types) {
        same_ts_type(&expr_ty, cast_ty)
    } else {
        false
    }
}

/// Infer the TypeScript type of an expression from its structure.
fn infer_expr_type(expr: &JsExpr, var_types: &HashMap<String, Type>) -> Option<Type> {
    match expr {
        JsExpr::Var(name) => var_types.get(name).cloned(),
        JsExpr::Literal(c) => match c {
            Constant::Int(_) | Constant::UInt(_) | Constant::Float(_) => {
                Some(Type::Float(64))
            }
            Constant::String(_) => Some(Type::String),
            Constant::Bool(_) => Some(Type::Bool),
            Constant::Null => None,
        },
        JsExpr::Binary { .. } | JsExpr::Unary { .. } => Some(Type::Float(64)),
        JsExpr::Cmp { .. } | JsExpr::Not(_) | JsExpr::TypeCheck { .. } => Some(Type::Bool),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::js_ast::{JsExpr, JsStmt};
    use reincarnate_core::ir::inst::CmpKind;
    use reincarnate_core::ir::value::Constant;

    fn var(name: &str) -> JsExpr {
        JsExpr::Var(name.to_string())
    }

    fn int_lit(n: i64) -> JsExpr {
        JsExpr::Literal(Constant::Int(n))
    }

    fn eq(lhs: JsExpr, rhs: JsExpr) -> JsExpr {
        JsExpr::Cmp {
            kind: CmpKind::Eq,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    #[test]
    fn switch_recovery_nested_if_else_chain() {
        // if (x === 3) { A } else if (x === 2) { B } else if (x === 1) { C }
        // Should produce a single 3-case switch, not if { } else { switch { } }.
        let mut body = vec![JsStmt::If {
            cond: eq(var("x"), int_lit(3)),
            then_body: vec![JsStmt::Expr(var("A"))],
            else_body: vec![JsStmt::If {
                cond: eq(var("x"), int_lit(2)),
                then_body: vec![JsStmt::Expr(var("B"))],
                else_body: vec![JsStmt::If {
                    cond: eq(var("x"), int_lit(1)),
                    then_body: vec![JsStmt::Expr(var("C"))],
                    else_body: vec![],
                }],
            }],
        }];

        recover_switch_statements(&mut body);

        assert_eq!(body.len(), 1, "Expected single statement, got: {body:?}");
        match &body[0] {
            JsStmt::Switch { cases, .. } => {
                assert_eq!(
                    cases.len(),
                    3,
                    "Expected 3-case switch, got {}-case: {cases:?}",
                    cases.len()
                );
            }
            other => panic!("Expected Switch, got: {other:?}"),
        }
    }
}
