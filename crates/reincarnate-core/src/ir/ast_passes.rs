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
}
