//! AST-to-AST rewrite passes.
//!
//! These run after Shape→AST lowering to detect and simplify patterns that
//! are easier to match on the high-level AST than during lowering.

use super::ast::{Expr, Stmt};
use super::inst::CmpKind;

// ---------------------------------------------------------------------------
// Ternary rewrite
// ---------------------------------------------------------------------------

/// Rewrite single-assign if/else to ternary expressions.
///
/// Matches:
/// ```text
/// if (cond) { x = a; } else { x = b; }
/// ```
/// and rewrites to:
/// ```text
/// x = cond ? a : b;
/// ```
///
/// Recurses into all nested statement bodies.
pub fn rewrite_ternary(body: &mut [Stmt]) {
    for stmt in body.iter_mut() {
        // First recurse into nested bodies.
        recurse_into_stmt(stmt, rewrite_ternary);

        // Then try to rewrite this statement.
        let replacement = match stmt {
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => match_ternary(cond, then_body, else_body),
            _ => None,
        };

        if let Some(new_stmt) = replacement {
            *stmt = new_stmt;
        }
    }
}

/// Check whether an if/else matches the single-assign ternary pattern.
fn match_ternary(cond: &Expr, then_body: &[Stmt], else_body: &[Stmt]) -> Option<Stmt> {
    if then_body.len() != 1 || else_body.len() != 1 {
        return None;
    }

    let (then_target, then_value) = match &then_body[0] {
        Stmt::Assign { target, value } => (target, value),
        _ => return None,
    };
    let (else_target, else_value) = match &else_body[0] {
        Stmt::Assign { target, value } => (target, value),
        _ => return None,
    };

    if then_target != else_target {
        return None;
    }

    Some(Stmt::Assign {
        target: then_target.clone(),
        value: Expr::Ternary {
            cond: Box::new(cond.clone()),
            then_val: Box::new(then_value.clone()),
            else_val: Box::new(else_value.clone()),
        },
    })
}

// ---------------------------------------------------------------------------
// Math.max / Math.min rewrite
// ---------------------------------------------------------------------------

/// Rewrite comparison+ternary patterns to `Math.max` / `Math.min`.
///
/// Matches:
/// ```text
/// x = (a >= b) ? a : b   →  x = Math.max(a, b)
/// x = (a >= b) ? b : a   →  x = Math.min(a, b)
/// x = (a <= b) ? a : b   →  x = Math.min(a, b)
/// x = (a <= b) ? b : a   →  x = Math.max(a, b)
/// ```
/// (and similarly for `>` / `<`)
///
/// Must run **after** `rewrite_ternary`. Recurses into all nested statement
/// bodies.
pub fn rewrite_minmax(body: &mut [Stmt]) {
    for stmt in body.iter_mut() {
        recurse_into_stmt(stmt, rewrite_minmax);

        let replacement = match stmt {
            Stmt::Assign { target, value } => match_minmax(target, value),
            _ => None,
        };

        if let Some(new_stmt) = replacement {
            *stmt = new_stmt;
        }
    }
}

/// Check whether an assign of a ternary matches a Math.max/min pattern.
fn match_minmax(target: &Expr, value: &Expr) -> Option<Stmt> {
    let (cond, then_val, else_val) = match value {
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => (cond.as_ref(), then_val.as_ref(), else_val.as_ref()),
        _ => return None,
    };

    let (kind, cmp_lhs, cmp_rhs) = match cond {
        Expr::Cmp { kind, lhs, rhs } => (*kind, lhs.as_ref(), rhs.as_ref()),
        _ => return None,
    };

    let func_name = match kind {
        CmpKind::Ge | CmpKind::Gt => {
            if then_val == cmp_lhs && else_val == cmp_rhs {
                "Math.max"
            } else if then_val == cmp_rhs && else_val == cmp_lhs {
                "Math.min"
            } else {
                return None;
            }
        }
        CmpKind::Le | CmpKind::Lt => {
            if then_val == cmp_lhs && else_val == cmp_rhs {
                "Math.min"
            } else if then_val == cmp_rhs && else_val == cmp_lhs {
                "Math.max"
            } else {
                return None;
            }
        }
        _ => return None,
    };

    Some(Stmt::Assign {
        target: target.clone(),
        value: Expr::Call {
            func: func_name.to_string(),
            args: vec![then_val.clone(), else_val.clone()],
        },
    })
}

// ---------------------------------------------------------------------------
// Compound assignment rewrite
// ---------------------------------------------------------------------------

/// Rewrite `target = target op value` to `target op= value`.
///
/// Matches:
/// ```text
/// x = x + 1   →  x += 1
/// a.b = a.b - c  →  a.b -= c
/// ```
///
/// Only matches when the left operand of the binary expression equals the
/// assignment target (not the right operand), preserving operand order for
/// non-commutative operators (Sub, Div, Rem, Shl, Shr).
///
/// Recurses into all nested statement bodies.
pub fn rewrite_compound_assign(body: &mut [Stmt]) {
    for stmt in body.iter_mut() {
        recurse_into_stmt(stmt, rewrite_compound_assign);

        let replacement = match stmt {
            Stmt::Assign { target, value } => match_compound_assign(target, value),
            _ => None,
        };

        if let Some(new_stmt) = replacement {
            *stmt = new_stmt;
        }
    }
}

/// Check whether an assignment matches the compound assignment pattern.
fn match_compound_assign(target: &Expr, value: &Expr) -> Option<Stmt> {
    let (op, lhs, rhs) = match value {
        Expr::Binary { op, lhs, rhs } => (*op, lhs.as_ref(), rhs.as_ref()),
        _ => return None,
    };

    if lhs != target {
        return None;
    }

    Some(Stmt::CompoundAssign {
        target: target.clone(),
        op,
        value: rhs.clone(),
    })
}

// ---------------------------------------------------------------------------
// Declaration/init merging
// ---------------------------------------------------------------------------

/// Merge uninitialized `let x: T;` declarations with their first assignment.
///
/// Rewrites:
/// ```text
/// let x: number;
/// ...            // no references to x
/// x = expr;
/// ```
/// into:
/// ```text
/// ...
/// let x: number = expr;
/// ```
///
/// The merged declaration is placed at the assignment's original position
/// (not at the top) to preserve correct evaluation order when the init
/// expression references other variables.
///
/// Only operates on top-level statements — does not recurse into nested bodies
/// (block-param declarations only appear at the function top level).
pub fn merge_decl_init(body: &mut Vec<Stmt>) {
    loop {
        if !try_merge_one_decl(body) {
            break;
        }
    }
}

/// Try to merge a single uninit VarDecl with its first assignment.
/// Returns `true` if a merge was performed.
fn try_merge_one_decl(body: &mut Vec<Stmt>) -> bool {
    for i in 0..body.len() {
        let (name, ty) = match &body[i] {
            Stmt::VarDecl {
                name,
                ty,
                init: None,
                mutable: true,
            } => (name.clone(), ty.clone()),
            _ => continue,
        };

        // Find the first top-level statement after the decl that references this var.
        for j in (i + 1)..body.len() {
            if !stmt_references_var(&body[j], &name) {
                continue;
            }

            // First reference found. Is it a plain `name = value;`?
            let is_plain_assign = matches!(
                &body[j],
                Stmt::Assign { target: Expr::Var(tname), value }
                    if tname == &name && !expr_references_var(value, &name)
            );

            if !is_plain_assign {
                break; // first reference isn't a mergeable assign
            }

            // Safe to merge. Remove the uninit decl at i.
            body.remove(i);
            // The assign shifted left by 1.
            let assign_idx = j - 1;
            // Extract the value and replace with an initialized VarDecl.
            let value = match std::mem::replace(&mut body[assign_idx], Stmt::Break) {
                Stmt::Assign { value, .. } => value,
                _ => unreachable!(),
            };
            body[assign_idx] = Stmt::VarDecl {
                name,
                ty,
                init: Some(value),
                mutable: true,
            };
            return true;
        }
    }
    false
}

// ---------------------------------------------------------------------------
// Variable reference checking
// ---------------------------------------------------------------------------

/// Whether a statement references a named variable (in any position).
fn stmt_references_var(stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::VarDecl {
            name: n,
            init,
            ..
        } => n == name || init.as_ref().is_some_and(|e| expr_references_var(e, name)),

        Stmt::Assign { target, value } => {
            expr_references_var(target, name) || expr_references_var(value, name)
        }

        Stmt::CompoundAssign { target, value, .. } => {
            expr_references_var(target, name) || expr_references_var(value, name)
        }

        Stmt::Expr(e) => expr_references_var(e, name),

        Stmt::If {
            cond,
            then_body,
            else_body,
        } => {
            expr_references_var(cond, name)
                || then_body.iter().any(|s| stmt_references_var(s, name))
                || else_body.iter().any(|s| stmt_references_var(s, name))
        }

        Stmt::While { cond, body } => {
            expr_references_var(cond, name) || body.iter().any(|s| stmt_references_var(s, name))
        }

        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            init.iter().any(|s| stmt_references_var(s, name))
                || expr_references_var(cond, name)
                || update.iter().any(|s| stmt_references_var(s, name))
                || body.iter().any(|s| stmt_references_var(s, name))
        }

        Stmt::Loop { body } => body.iter().any(|s| stmt_references_var(s, name)),

        Stmt::Return(e) => e.as_ref().is_some_and(|e| expr_references_var(e, name)),

        Stmt::Dispatch { blocks, .. } => blocks
            .iter()
            .any(|(_, stmts)| stmts.iter().any(|s| stmt_references_var(s, name))),

        Stmt::Break | Stmt::Continue | Stmt::LabeledBreak { .. } => false,
    }
}

/// Whether an expression references a named variable.
fn expr_references_var(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::Var(n) => n == name,
        Expr::Literal(_) | Expr::GlobalRef(_) => false,
        Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
            expr_references_var(lhs, name) || expr_references_var(rhs, name)
        }
        Expr::LogicalOr { lhs, rhs } | Expr::LogicalAnd { lhs, rhs } => {
            expr_references_var(lhs, name) || expr_references_var(rhs, name)
        }
        Expr::Unary { expr: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::TypeCheck { expr: inner, .. }
        | Expr::Not(inner)
        | Expr::CoroutineResume(inner) => expr_references_var(inner, name),
        Expr::Field { object, .. } => expr_references_var(object, name),
        Expr::Index { collection, index } => {
            expr_references_var(collection, name) || expr_references_var(index, name)
        }
        Expr::Call { args, .. } | Expr::CoroutineCreate { args, .. } => {
            args.iter().any(|a| expr_references_var(a, name))
        }
        Expr::CallIndirect { callee, args } => {
            expr_references_var(callee, name) || args.iter().any(|a| expr_references_var(a, name))
        }
        Expr::SystemCall { args, .. } => args.iter().any(|a| expr_references_var(a, name)),
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            expr_references_var(cond, name)
                || expr_references_var(then_val, name)
                || expr_references_var(else_val, name)
        }
        Expr::ArrayInit(elems) | Expr::TupleInit(elems) => {
            elems.iter().any(|e| expr_references_var(e, name))
        }
        Expr::StructInit { fields, .. } => {
            fields.iter().any(|(_, v)| expr_references_var(v, name))
        }
        Expr::Yield(v) => v.as_ref().is_some_and(|e| expr_references_var(e, name)),
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Recurse a rewrite pass into all nested statement bodies.
fn recurse_into_stmt(stmt: &mut Stmt, pass: fn(&mut [Stmt])) {
    match stmt {
        Stmt::If {
            then_body,
            else_body,
            ..
        } => {
            pass(then_body);
            pass(else_body);
        }
        Stmt::While { body, .. } => {
            pass(body);
        }
        Stmt::For {
            init,
            update,
            body,
            ..
        } => {
            pass(init);
            pass(update);
            pass(body);
        }
        Stmt::Loop { body } => {
            pass(body);
        }
        Stmt::Dispatch { blocks, .. } => {
            for (_, block_body) in blocks {
                pass(block_body);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{BinOp, Expr, Stmt};
    use crate::ir::inst::CmpKind;
    use crate::ir::value::Constant;

    fn var(name: &str) -> Expr {
        Expr::Var(name.to_string())
    }

    fn int(n: i64) -> Expr {
        Expr::Literal(Constant::Int(n))
    }

    fn assign(target: Expr, value: Expr) -> Stmt {
        Stmt::Assign { target, value }
    }

    #[test]
    fn ternary_rewrite_basic() {
        let mut body = vec![Stmt::If {
            cond: var("c"),
            then_body: vec![assign(var("x"), int(1))],
            else_body: vec![assign(var("x"), int(2))],
        }];

        rewrite_ternary(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::Assign { target, value } => {
                assert_eq!(*target, var("x"));
                match value {
                    Expr::Ternary {
                        cond,
                        then_val,
                        else_val,
                    } => {
                        assert_eq!(**cond, var("c"));
                        assert_eq!(**then_val, int(1));
                        assert_eq!(**else_val, int(2));
                    }
                    other => panic!("Expected Ternary, got: {other:?}"),
                }
            }
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn ternary_no_rewrite_different_targets() {
        let mut body = vec![Stmt::If {
            cond: var("c"),
            then_body: vec![assign(var("x"), int(1))],
            else_body: vec![assign(var("y"), int(2))],
        }];

        rewrite_ternary(&mut body);

        // Should remain an if/else.
        assert!(matches!(&body[0], Stmt::If { .. }));
    }

    #[test]
    fn ternary_no_rewrite_multi_stmt() {
        let mut body = vec![Stmt::If {
            cond: var("c"),
            then_body: vec![assign(var("x"), int(1)), assign(var("y"), int(2))],
            else_body: vec![assign(var("x"), int(3))],
        }];

        rewrite_ternary(&mut body);

        assert!(matches!(&body[0], Stmt::If { .. }));
    }

    #[test]
    fn ternary_recurses_into_nested() {
        let inner_if = Stmt::If {
            cond: var("c"),
            then_body: vec![assign(var("x"), int(1))],
            else_body: vec![assign(var("x"), int(2))],
        };
        let mut body = vec![Stmt::While {
            cond: var("true"),
            body: vec![inner_if],
        }];

        rewrite_ternary(&mut body);

        match &body[0] {
            Stmt::While { body, .. } => match &body[0] {
                Stmt::Assign { value, .. } => {
                    assert!(matches!(value, Expr::Ternary { .. }));
                }
                other => panic!("Expected Assign, got: {other:?}"),
            },
            other => panic!("Expected While, got: {other:?}"),
        }
    }

    #[test]
    fn minmax_rewrite_ge_max() {
        // x = (a >= b) ? a : b  →  x = Math.max(a, b)
        let mut body = vec![assign(
            var("x"),
            Expr::Ternary {
                cond: Box::new(Expr::Cmp {
                    kind: CmpKind::Ge,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                }),
                then_val: Box::new(var("a")),
                else_val: Box::new(var("b")),
            },
        )];

        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => match value {
                Expr::Call { func, args } => {
                    assert_eq!(func, "Math.max");
                    assert_eq!(args.len(), 2);
                    assert_eq!(args[0], var("a"));
                    assert_eq!(args[1], var("b"));
                }
                other => panic!("Expected Call, got: {other:?}"),
            },
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn minmax_rewrite_ge_min() {
        // x = (a >= b) ? b : a  →  x = Math.min(b, a)
        let mut body = vec![assign(
            var("x"),
            Expr::Ternary {
                cond: Box::new(Expr::Cmp {
                    kind: CmpKind::Ge,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                }),
                then_val: Box::new(var("b")),
                else_val: Box::new(var("a")),
            },
        )];

        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => match value {
                Expr::Call { func, args } => {
                    assert_eq!(func, "Math.min");
                    assert_eq!(args.len(), 2);
                    assert_eq!(args[0], var("b"));
                    assert_eq!(args[1], var("a"));
                }
                other => panic!("Expected Call, got: {other:?}"),
            },
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn minmax_rewrite_le_min() {
        // x = (a <= b) ? a : b  →  x = Math.min(a, b)
        let mut body = vec![assign(
            var("x"),
            Expr::Ternary {
                cond: Box::new(Expr::Cmp {
                    kind: CmpKind::Le,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                }),
                then_val: Box::new(var("a")),
                else_val: Box::new(var("b")),
            },
        )];

        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => match value {
                Expr::Call { func, .. } => assert_eq!(func, "Math.min"),
                other => panic!("Expected Call, got: {other:?}"),
            },
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn minmax_rewrite_le_max() {
        // x = (a <= b) ? b : a  →  x = Math.max(b, a)
        let mut body = vec![assign(
            var("x"),
            Expr::Ternary {
                cond: Box::new(Expr::Cmp {
                    kind: CmpKind::Le,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                }),
                then_val: Box::new(var("b")),
                else_val: Box::new(var("a")),
            },
        )];

        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => match value {
                Expr::Call { func, .. } => assert_eq!(func, "Math.max"),
                other => panic!("Expected Call, got: {other:?}"),
            },
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn minmax_no_rewrite_mismatched_operands() {
        // x = (a >= b) ? c : d — operands don't match, no rewrite
        let mut body = vec![assign(
            var("x"),
            Expr::Ternary {
                cond: Box::new(Expr::Cmp {
                    kind: CmpKind::Ge,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                }),
                then_val: Box::new(var("c")),
                else_val: Box::new(var("d")),
            },
        )];

        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => {
                assert!(matches!(value, Expr::Ternary { .. }));
            }
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn minmax_no_rewrite_eq() {
        // x = (a == b) ? a : b — Eq is not a minmax comparison
        let mut body = vec![assign(
            var("x"),
            Expr::Ternary {
                cond: Box::new(Expr::Cmp {
                    kind: CmpKind::Eq,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                }),
                then_val: Box::new(var("a")),
                else_val: Box::new(var("b")),
            },
        )];

        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => {
                assert!(matches!(value, Expr::Ternary { .. }));
            }
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn combined_ternary_then_minmax() {
        // Full pipeline: if (a >= b) { x = a } else { x = b }
        // → ternary: x = (a >= b) ? a : b
        // → minmax: x = Math.max(a, b)
        let mut body = vec![Stmt::If {
            cond: Expr::Cmp {
                kind: CmpKind::Ge,
                lhs: Box::new(var("a")),
                rhs: Box::new(var("b")),
            },
            then_body: vec![assign(var("x"), var("a"))],
            else_body: vec![assign(var("x"), var("b"))],
        }];

        rewrite_ternary(&mut body);
        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => match value {
                Expr::Call { func, args } => {
                    assert_eq!(func, "Math.max");
                    assert_eq!(args[0], var("a"));
                    assert_eq!(args[1], var("b"));
                }
                other => panic!("Expected Call, got: {other:?}"),
            },
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn minmax_with_expressions() {
        // x = (a + 1 >= b * 2) ? (a + 1) : (b * 2) → Math.max(a + 1, b * 2)
        let a_plus_1 = Expr::Binary {
            op: BinOp::Add,
            lhs: Box::new(var("a")),
            rhs: Box::new(int(1)),
        };
        let b_times_2 = Expr::Binary {
            op: BinOp::Mul,
            lhs: Box::new(var("b")),
            rhs: Box::new(int(2)),
        };

        let mut body = vec![assign(
            var("x"),
            Expr::Ternary {
                cond: Box::new(Expr::Cmp {
                    kind: CmpKind::Ge,
                    lhs: Box::new(a_plus_1.clone()),
                    rhs: Box::new(b_times_2.clone()),
                }),
                then_val: Box::new(a_plus_1),
                else_val: Box::new(b_times_2),
            },
        )];

        rewrite_minmax(&mut body);

        match &body[0] {
            Stmt::Assign { value, .. } => match value {
                Expr::Call { func, .. } => assert_eq!(func, "Math.max"),
                other => panic!("Expected Call, got: {other:?}"),
            },
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    // -----------------------------------------------------------------------
    // Compound assignment tests
    // -----------------------------------------------------------------------

    #[test]
    fn compound_assign_basic_sub() {
        // HP = HP - damage  →  HP -= damage
        let mut body = vec![assign(
            var("HP"),
            Expr::Binary {
                op: BinOp::Sub,
                lhs: Box::new(var("HP")),
                rhs: Box::new(var("damage")),
            },
        )];

        rewrite_compound_assign(&mut body);

        match &body[0] {
            Stmt::CompoundAssign { target, op, value } => {
                assert_eq!(*target, var("HP"));
                assert_eq!(*op, BinOp::Sub);
                assert_eq!(*value, var("damage"));
            }
            other => panic!("Expected CompoundAssign, got: {other:?}"),
        }
    }

    #[test]
    fn compound_assign_add() {
        // x = x + 1  →  x += 1
        let mut body = vec![assign(
            var("x"),
            Expr::Binary {
                op: BinOp::Add,
                lhs: Box::new(var("x")),
                rhs: Box::new(int(1)),
            },
        )];

        rewrite_compound_assign(&mut body);

        match &body[0] {
            Stmt::CompoundAssign { target, op, value } => {
                assert_eq!(*target, var("x"));
                assert_eq!(*op, BinOp::Add);
                assert_eq!(*value, int(1));
            }
            other => panic!("Expected CompoundAssign, got: {other:?}"),
        }
    }

    #[test]
    fn compound_assign_no_rewrite_rhs_match() {
        // x = y + x — rhs matches target but lhs doesn't, no rewrite
        let mut body = vec![assign(
            var("x"),
            Expr::Binary {
                op: BinOp::Add,
                lhs: Box::new(var("y")),
                rhs: Box::new(var("x")),
            },
        )];

        rewrite_compound_assign(&mut body);

        assert!(matches!(&body[0], Stmt::Assign { .. }));
    }

    #[test]
    fn compound_assign_no_rewrite_different_target() {
        // x = y - z — no match at all
        let mut body = vec![assign(
            var("x"),
            Expr::Binary {
                op: BinOp::Sub,
                lhs: Box::new(var("y")),
                rhs: Box::new(var("z")),
            },
        )];

        rewrite_compound_assign(&mut body);

        assert!(matches!(&body[0], Stmt::Assign { .. }));
    }

    #[test]
    fn compound_assign_field_access() {
        // this.HP = this.HP * 2  →  this.HP *= 2
        let field = Expr::Field {
            object: Box::new(var("this")),
            field: "HP".to_string(),
        };
        let mut body = vec![assign(
            field.clone(),
            Expr::Binary {
                op: BinOp::Mul,
                lhs: Box::new(field.clone()),
                rhs: Box::new(int(2)),
            },
        )];

        rewrite_compound_assign(&mut body);

        match &body[0] {
            Stmt::CompoundAssign { target, op, value } => {
                assert_eq!(*target, field);
                assert_eq!(*op, BinOp::Mul);
                assert_eq!(*value, int(2));
            }
            other => panic!("Expected CompoundAssign, got: {other:?}"),
        }
    }

    #[test]
    fn compound_assign_recurses_into_nested() {
        let inner = assign(
            var("x"),
            Expr::Binary {
                op: BinOp::Add,
                lhs: Box::new(var("x")),
                rhs: Box::new(int(1)),
            },
        );
        let mut body = vec![Stmt::While {
            cond: var("true"),
            body: vec![inner],
        }];

        rewrite_compound_assign(&mut body);

        match &body[0] {
            Stmt::While { body, .. } => {
                assert!(matches!(&body[0], Stmt::CompoundAssign { .. }));
            }
            other => panic!("Expected While, got: {other:?}"),
        }
    }

    #[test]
    fn compound_assign_bitwise_ops() {
        // x = x | mask  →  x |= mask
        let mut body = vec![assign(
            var("x"),
            Expr::Binary {
                op: BinOp::BitOr,
                lhs: Box::new(var("x")),
                rhs: Box::new(var("mask")),
            },
        )];

        rewrite_compound_assign(&mut body);

        match &body[0] {
            Stmt::CompoundAssign { op, .. } => {
                assert_eq!(*op, BinOp::BitOr);
            }
            other => panic!("Expected CompoundAssign, got: {other:?}"),
        }
    }

    // -----------------------------------------------------------------------
    // Decl/init merge tests
    // -----------------------------------------------------------------------

    fn uninit_decl(name: &str) -> Stmt {
        Stmt::VarDecl {
            name: name.to_string(),
            ty: Some(crate::ir::ty::Type::Int(64)),
            init: None,
            mutable: true,
        }
    }

    #[test]
    fn merge_decl_basic() {
        // let x; x = 5;  →  let x = 5;
        let mut body = vec![uninit_decl("x"), assign(var("x"), int(5))];

        merge_decl_init(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::VarDecl {
                name, init, mutable, ..
            } => {
                assert_eq!(name, "x");
                assert_eq!(*init, Some(int(5)));
                assert!(*mutable);
            }
            other => panic!("Expected VarDecl, got: {other:?}"),
        }
    }

    #[test]
    fn merge_decl_with_gap() {
        // let x; const y = 10; x = 5;  →  const y = 10; let x = 5;
        let y_decl = Stmt::VarDecl {
            name: "y".to_string(),
            ty: None,
            init: Some(int(10)),
            mutable: false,
        };
        let mut body = vec![uninit_decl("x"), y_decl, assign(var("x"), int(5))];

        merge_decl_init(&mut body);

        assert_eq!(body.len(), 2);
        // y decl is first (x's uninit decl was removed from index 0).
        assert!(matches!(&body[0], Stmt::VarDecl { name, .. } if name == "y"));
        // x decl is at index 1 (replaced the assign).
        match &body[1] {
            Stmt::VarDecl { name, init, .. } => {
                assert_eq!(name, "x");
                assert_eq!(*init, Some(int(5)));
            }
            other => panic!("Expected VarDecl, got: {other:?}"),
        }
    }

    #[test]
    fn merge_decl_no_merge_first_ref_is_read() {
        // let x; y = x + 1; x = 5;  →  no merge (x read before assigned)
        let mut body = vec![
            uninit_decl("x"),
            assign(
                var("y"),
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("x")),
                    rhs: Box::new(int(1)),
                },
            ),
            assign(var("x"), int(5)),
        ];

        merge_decl_init(&mut body);

        // Should remain unchanged — first reference to x is a read in y's assign.
        assert_eq!(body.len(), 3);
        assert!(matches!(&body[0], Stmt::VarDecl { init: None, .. }));
    }

    #[test]
    fn merge_decl_no_merge_self_reference() {
        // let x; x = x + 1;  →  no merge (value references x)
        let mut body = vec![
            uninit_decl("x"),
            assign(
                var("x"),
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("x")),
                    rhs: Box::new(int(1)),
                },
            ),
        ];

        merge_decl_init(&mut body);

        assert_eq!(body.len(), 2);
        assert!(matches!(&body[0], Stmt::VarDecl { init: None, .. }));
    }

    #[test]
    fn merge_decl_no_merge_inside_if() {
        // let x; if (c) { x = 1; } else { x = 2; }  →  no merge (in nested body)
        let mut body = vec![
            uninit_decl("x"),
            Stmt::If {
                cond: var("c"),
                then_body: vec![assign(var("x"), int(1))],
                else_body: vec![assign(var("x"), int(2))],
            },
        ];

        merge_decl_init(&mut body);

        // First ref is the If statement (which references x), not a plain Assign.
        assert_eq!(body.len(), 2);
        assert!(matches!(&body[0], Stmt::VarDecl { init: None, .. }));
    }

    #[test]
    fn merge_decl_ternary() {
        // let x; x = c ? a : b;  →  let x = c ? a : b;
        let mut body = vec![
            uninit_decl("x"),
            assign(
                var("x"),
                Expr::Ternary {
                    cond: Box::new(var("c")),
                    then_val: Box::new(var("a")),
                    else_val: Box::new(var("b")),
                },
            ),
        ];

        merge_decl_init(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::VarDecl { name, init, .. } => {
                assert_eq!(name, "x");
                assert!(matches!(init, Some(Expr::Ternary { .. })));
            }
            other => panic!("Expected VarDecl, got: {other:?}"),
        }
    }

    #[test]
    fn merge_decl_preserves_order() {
        // let x; let y; y = 5; x = y + 1;
        // → let y = 5; let x = y + 1;
        // (y merged first since it appears first with a mergeable assign)
        let mut body = vec![
            uninit_decl("x"),
            uninit_decl("y"),
            assign(var("y"), int(5)),
            assign(
                var("x"),
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("y")),
                    rhs: Box::new(int(1)),
                },
            ),
        ];

        merge_decl_init(&mut body);

        assert_eq!(body.len(), 2);
        match &body[0] {
            Stmt::VarDecl { name, init, .. } => {
                assert_eq!(name, "y");
                assert_eq!(*init, Some(int(5)));
            }
            other => panic!("Expected VarDecl for y, got: {other:?}"),
        }
        match &body[1] {
            Stmt::VarDecl { name, init, .. } => {
                assert_eq!(name, "x");
                assert!(init.is_some());
            }
            other => panic!("Expected VarDecl for x, got: {other:?}"),
        }
    }
}
