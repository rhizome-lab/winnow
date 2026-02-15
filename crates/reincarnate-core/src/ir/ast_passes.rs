//! AST-to-AST rewrite passes.
//!
//! These run after Shape→AST lowering to detect and simplify patterns that
//! are easier to match on the high-level AST than during lowering.

use super::ast::{BinOp, Expr, Stmt};
use super::inst::{CastKind, CmpKind};
use super::value::Constant;

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

    // Skip identity-branch ternaries where one side is just the target variable
    // (e.g. `x = cond ? x : y` → better as `if (!cond) { x = y; }`).
    if then_value == then_target || else_value == then_target {
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
// Single-use const folding
// ---------------------------------------------------------------------------

/// Fold single-use `const x = expr; ... use(x) ...` into `... use(expr) ...`.
///
/// This is AST-level copy propagation: if an immutable variable is assigned
/// once and referenced exactly once, substitute the init expression at the
/// use site and remove the declaration.
///
/// Runs iteratively until fixpoint. Recurses into nested bodies.
///
/// Safety rules:
/// - Adjacent use (next statement): always fold (pure or impure init).
/// - Non-adjacent use: only fold when all intervening statements are free
///   of side effects (pure VarDecls, uninit decls).
pub fn fold_single_use_consts(body: &mut Vec<Stmt>) {
    // Fold at this level first.
    loop {
        if !try_fold_one_const(body) {
            break;
        }
    }
    // Then recurse into nested bodies.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                fold_single_use_consts(then_body);
                fold_single_use_consts(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                fold_single_use_consts(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                fold_single_use_consts(init);
                fold_single_use_consts(update);
                fold_single_use_consts(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    fold_single_use_consts(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    fold_single_use_consts(case_body);
                }
                fold_single_use_consts(default_body);
            }
            _ => {}
        }
    }
}

/// Try to fold a single const. Returns `true` if a fold was performed.
fn try_fold_one_const(body: &mut Vec<Stmt>) -> bool {
    for i in 0..body.len() {
        // Dead uninit decl: `let vN: T;` with no remaining references.
        if let Stmt::VarDecl {
            name,
            init: None, ..
        } = &body[i]
        {
            let refs: usize = body[i + 1..]
                .iter()
                .map(|s| count_var_refs_in_stmt(s, name))
                .sum();
            if refs == 0 {
                let name = name.clone();
                body.remove(i);
                remove_dead_assigns(body, &name);
                return true;
            }
        }

        let name = match &body[i] {
            Stmt::VarDecl {
                name,
                init: Some(_),
                ..
            } => name.clone(),
            _ => continue,
        };

        // Count ALL references in the remaining body.
        let total_refs: usize = body[i + 1..]
            .iter()
            .map(|s| count_var_refs_in_stmt(s, &name))
            .sum();

        // Dead declaration: zero reads after the decl.
        if total_refs == 0 {
            let init = match &body[i] {
                Stmt::VarDecl {
                    init: Some(init), ..
                } => init,
                _ => unreachable!(),
            };
            if expr_has_side_effects(init) {
                // Preserve side effects: convert to expression statement.
                let init = match body.remove(i) {
                    Stmt::VarDecl {
                        init: Some(expr), ..
                    } => expr,
                    _ => unreachable!(),
                };
                body.insert(i, Stmt::Expr(init));
            } else {
                body.remove(i);
            }
            remove_dead_assigns(body, &name);
            return true;
        }

        if total_refs != 1 {
            continue;
        }

        // Don't fold if the variable is reassigned in the remaining body.
        // Loop-carried variables (e.g., repeat counters) have 1 read + 1 write
        // per iteration — the init value becomes stale after the back-edge write.
        if var_is_reassigned(&body[i + 1..], &name) {
            continue;
        }

        // Find the statement containing the single use (read, not bare write).
        // Must use count_var_refs_in_stmt (which skips bare Var writes) rather
        // than stmt_references_var (which counts writes too) — otherwise we'd
        // target a write-only statement, the substitute would find nothing to
        // replace, and the init expression would be silently dropped.
        let use_idx = (i + 1..body.len())
            .find(|&j| count_var_refs_in_stmt(&body[j], &name) > 0)
            .unwrap();

        let adjacent = use_idx == i + 1;

        if !adjacent {
            // Non-adjacent: only fold if all intervening statements are SE-free,
            // OR if the init is a stable path expression (Var/Field chain) and
            // no intervening statement reassigns a prefix of that path.
            let all_intervening_pure = body[i + 1..use_idx]
                .iter()
                .all(|s| !stmt_has_side_effects(s));
            if !all_intervening_pure {
                let init = match &body[i] {
                    Stmt::VarDecl {
                        init: Some(init), ..
                    } => init,
                    _ => unreachable!(),
                };
                // Stable path (Var/Field chain): sink past non-overlapping
                // field assignments.
                let can_sink_path = is_stable_path(init)
                    && body[i + 1..use_idx]
                        .iter()
                        .all(|s| !stmt_assigns_to_prefix_of(s, init));
                // Operand-stack temporary: if all intervening statements only
                // assign to local variables that the init doesn't reference,
                // the const was an AVM2 operand-stack artifact. Sink it to
                // reconstruct original source order.
                let can_sink_past_locals = !can_sink_path
                    && body[i + 1..use_idx].iter().all(|s| match s {
                        Stmt::Assign {
                            target: Expr::Var(t),
                            ..
                        } => !expr_references_var(init, t),
                        Stmt::VarDecl { name: n, .. } => !expr_references_var(init, n),
                        Stmt::Expr(_) => true,
                        _ => false,
                    });
                if !can_sink_path && !can_sink_past_locals {
                    continue;
                }
            }
        }

        // Extract init and substitute at the use site.
        let init_expr = match body.remove(i) {
            Stmt::VarDecl {
                init: Some(expr), ..
            } => expr,
            _ => unreachable!(),
        };

        // use_idx shifted left by 1 after removal.
        let mut replacement = Some(init_expr);
        substitute_var_in_stmt(&mut body[use_idx - 1], &name, &mut replacement);
        return true;
    }
    false
}

/// Whether a statement could have observable side effects.
fn stmt_has_side_effects(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::VarDecl {
            init: Some(init), ..
        } => expr_has_side_effects(init),
        Stmt::VarDecl { init: None, .. } => false,
        // Assigns, calls, control flow — conservatively side-effecting.
        _ => true,
    }
}

/// Whether an expression could have observable side effects (calls).
fn expr_has_side_effects(expr: &Expr) -> bool {
    match expr {
        Expr::Call { .. }
        | Expr::CallIndirect { .. }
        | Expr::SystemCall { .. }
        | Expr::MethodCall { .. }
        | Expr::CoroutineCreate { .. }
        | Expr::CoroutineResume(_)
        | Expr::Yield(_)
        | Expr::PostIncrement(_) => true,
        Expr::Literal(_) | Expr::Var(_) | Expr::GlobalRef(_) => false,
        Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
            expr_has_side_effects(lhs) || expr_has_side_effects(rhs)
        }
        Expr::LogicalOr { lhs, rhs } | Expr::LogicalAnd { lhs, rhs } => {
            expr_has_side_effects(lhs) || expr_has_side_effects(rhs)
        }
        Expr::Unary { expr: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::TypeCheck { expr: inner, .. }
        | Expr::Not(inner) => expr_has_side_effects(inner),
        Expr::Field { object, .. } => expr_has_side_effects(object),
        Expr::Index { collection, index } => {
            expr_has_side_effects(collection) || expr_has_side_effects(index)
        }
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            expr_has_side_effects(cond)
                || expr_has_side_effects(then_val)
                || expr_has_side_effects(else_val)
        }
        Expr::ArrayInit(elems) | Expr::TupleInit(elems) => {
            elems.iter().any(expr_has_side_effects)
        }
        Expr::StructInit { fields, .. } => {
            fields.iter().any(|(_, v)| expr_has_side_effects(v))  // closure needed: tuple destructure
        }
    }
}

/// Remove all bare assignments to `name` from `body` (recursing into nested scopes).
///
/// Called after removing a dead uninit decl with 0 read refs — all assignments to
/// that variable are dead writes. Side-effecting RHS values are preserved as
/// expression statements.
fn remove_dead_assigns(body: &mut Vec<Stmt>, name: &str) {
    let mut i = 0;
    while i < body.len() {
        // Recurse into nested bodies first.
        match &mut body[i] {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                remove_dead_assigns(then_body, name);
                remove_dead_assigns(else_body, name);
            }
            Stmt::While { body: inner, .. }
            | Stmt::Loop { body: inner }
            | Stmt::ForOf { body: inner, .. } => {
                remove_dead_assigns(inner, name);
            }
            Stmt::For {
                init,
                update,
                body: inner,
                ..
            } => {
                remove_dead_assigns(init, name);
                remove_dead_assigns(update, name);
                remove_dead_assigns(inner, name);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    remove_dead_assigns(block_body, name);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    remove_dead_assigns(case_body, name);
                }
                remove_dead_assigns(default_body, name);
            }
            _ => {}
        }
        // Check if this statement is a bare assign to the dead variable.
        let is_dead_assign = matches!(
            &body[i],
            Stmt::Assign { target: Expr::Var(n), .. } if n == name
        );
        if is_dead_assign {
            let stmt = body.remove(i);
            if let Stmt::Assign { value, .. } = stmt {
                if expr_has_side_effects(&value) {
                    body.insert(i, Stmt::Expr(value));
                    i += 1;
                }
            }
        } else {
            i += 1;
        }
    }
}

/// Whether an expression is a stable path (Var or Var.field.field... chain).
///
/// Stable paths can be safely re-evaluated past field assignments to different
/// targets because field writes don't change object identity.
fn is_stable_path(expr: &Expr) -> bool {
    match expr {
        Expr::Var(_) => true,
        Expr::Field { object, .. } => is_stable_path(object),
        _ => false,
    }
}

/// Whether a statement assigns to a prefix of the given path expression.
///
/// `this.foo` is a prefix of `this.foo` and `this.foo.bar`, but not of
/// `this.baz` or `this.foo_other`. A prefix assignment would change what
/// the path evaluates to, making it unsafe to sink past.
fn stmt_assigns_to_prefix_of(stmt: &Stmt, path: &Expr) -> bool {
    match stmt {
        Stmt::Assign { target, .. } | Stmt::CompoundAssign { target, .. } => {
            expr_is_prefix_of(target, path)
        }
        // Other statement types (calls, control flow) are conservatively unsafe.
        _ => stmt_has_side_effects(stmt),
    }
}

/// Whether `prefix` is a path prefix of `path`.
///
/// `this.foo` is a prefix of `this.foo` and `this.foo.bar`.
/// `this.foo.bar` is NOT a prefix of `this.foo` (deeper path doesn't
/// invalidate a shallower read).
fn expr_is_prefix_of(prefix: &Expr, path: &Expr) -> bool {
    if prefix == path {
        return true;
    }
    // Walk up the path chain — if any ancestor matches the prefix, it's a hit.
    match path {
        Expr::Field { object, .. } => expr_is_prefix_of(prefix, object),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Variable reference counting and substitution
// ---------------------------------------------------------------------------

/// Count occurrences of `Var(name)` in an expression.
fn count_var_refs_in_expr(expr: &Expr, name: &str) -> usize {
    match expr {
        Expr::Var(n) => usize::from(n == name),
        Expr::Literal(_) | Expr::GlobalRef(_) => 0,
        Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
            count_var_refs_in_expr(lhs, name) + count_var_refs_in_expr(rhs, name)
        }
        Expr::LogicalOr { lhs, rhs } | Expr::LogicalAnd { lhs, rhs } => {
            count_var_refs_in_expr(lhs, name) + count_var_refs_in_expr(rhs, name)
        }
        Expr::Unary { expr: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::TypeCheck { expr: inner, .. }
        | Expr::Not(inner)
        | Expr::CoroutineResume(inner)
        | Expr::PostIncrement(inner) => count_var_refs_in_expr(inner, name),
        Expr::Field { object, .. } => count_var_refs_in_expr(object, name),
        Expr::Index { collection, index } => {
            count_var_refs_in_expr(collection, name) + count_var_refs_in_expr(index, name)
        }
        Expr::Call { args, .. } | Expr::CoroutineCreate { args, .. } => {
            args.iter().map(|a| count_var_refs_in_expr(a, name)).sum()
        }
        Expr::CallIndirect { callee, args } => {
            count_var_refs_in_expr(callee, name)
                + args.iter().map(|a| count_var_refs_in_expr(a, name)).sum::<usize>()
        }
        Expr::SystemCall { args, .. } => {
            args.iter().map(|a| count_var_refs_in_expr(a, name)).sum()
        }
        Expr::MethodCall {
            receiver, args, ..
        } => {
            count_var_refs_in_expr(receiver, name)
                + args.iter().map(|a| count_var_refs_in_expr(a, name)).sum::<usize>()
        }
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            count_var_refs_in_expr(cond, name)
                + count_var_refs_in_expr(then_val, name)
                + count_var_refs_in_expr(else_val, name)
        }
        Expr::ArrayInit(elems) | Expr::TupleInit(elems) => {
            elems.iter().map(|e| count_var_refs_in_expr(e, name)).sum()
        }
        Expr::StructInit { fields, .. } => fields
            .iter()
            .map(|(_, v)| count_var_refs_in_expr(v, name))
            .sum(),
        Expr::Yield(v) => v.as_ref().map_or(0, |e| count_var_refs_in_expr(e, name)),
    }
}

/// Check whether `name` is reassigned anywhere in `stmts` (including nested bodies).
/// This catches `Assign { target: Var(name), .. }` and `CompoundAssign { target: Var(name), .. }`.
fn var_is_reassigned(stmts: &[Stmt], name: &str) -> bool {
    stmts.iter().any(|s| stmt_reassigns_var(s, name))
}

fn stmt_reassigns_var(stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::Assign {
            target: Expr::Var(v),
            ..
        } if v == name => true,
        Stmt::CompoundAssign {
            target: Expr::Var(v),
            ..
        } if v == name => true,
        // Recurse into nested bodies.
        Stmt::If {
            then_body,
            else_body,
            ..
        } => var_is_reassigned(then_body, name) || var_is_reassigned(else_body, name),
        Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
            var_is_reassigned(body, name)
        }
        Stmt::For {
            init,
            update,
            body,
            ..
        } => {
            var_is_reassigned(init, name)
                || var_is_reassigned(update, name)
                || var_is_reassigned(body, name)
        }
        Stmt::Switch {
            cases,
            default_body,
            ..
        } => {
            cases.iter().any(|(_, b)| var_is_reassigned(b, name))
                || var_is_reassigned(default_body, name)
        }
        Stmt::Dispatch { blocks, .. } => {
            blocks.iter().any(|(_, b)| var_is_reassigned(b, name))
        }
        _ => false,
    }
}

/// Count occurrences of `Var(name)` in a statement (recursing into nested bodies).
fn count_var_refs_in_stmt(stmt: &Stmt, name: &str) -> usize {
    match stmt {
        Stmt::VarDecl { name: n, init, .. } => {
            usize::from(n == name) + init.as_ref().map_or(0, |e| count_var_refs_in_expr(e, name))
        }
        Stmt::Assign { target, value } => {
            // A bare Var target is a write, not a read — don't count it.
            // Complex targets (Field, Index) contain reads of sub-expressions.
            let target_refs = if matches!(target, Expr::Var(v) if v == name) {
                0
            } else {
                count_var_refs_in_expr(target, name)
            };
            target_refs + count_var_refs_in_expr(value, name)
        }
        Stmt::CompoundAssign { target, value, .. } => {
            // CompoundAssign reads AND writes the target, so always count it.
            count_var_refs_in_expr(target, name) + count_var_refs_in_expr(value, name)
        }
        Stmt::Expr(e) => count_var_refs_in_expr(e, name),
        Stmt::If {
            cond,
            then_body,
            else_body,
        } => {
            count_var_refs_in_expr(cond, name)
                + then_body
                    .iter()
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
                + else_body
                    .iter()
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
        }
        Stmt::While { cond, body } => {
            count_var_refs_in_expr(cond, name)
                + body
                    .iter()
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
        }
        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            init.iter()
                .map(|s| count_var_refs_in_stmt(s, name))
                .sum::<usize>()
                + count_var_refs_in_expr(cond, name)
                + update
                    .iter()
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
                + body
                    .iter()
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
        }
        Stmt::Loop { body } => body
            .iter()
            .map(|s| count_var_refs_in_stmt(s, name))
            .sum(),
        Stmt::ForOf {
            binding,
            iterable,
            body,
            ..
        } => {
            usize::from(binding == name)
                + count_var_refs_in_expr(iterable, name)
                + body
                    .iter()
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
        }
        Stmt::Return(e) => e.as_ref().map_or(0, |e| count_var_refs_in_expr(e, name)),
        Stmt::Dispatch { blocks, .. } => blocks
            .iter()
            .flat_map(|(_, stmts)| stmts.iter())
            .map(|s| count_var_refs_in_stmt(s, name))
            .sum(),
        Stmt::Switch {
            value,
            cases,
            default_body,
        } => {
            count_var_refs_in_expr(value, name)
                + cases
                    .iter()
                    .flat_map(|(_, stmts)| stmts.iter())
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
                + default_body
                    .iter()
                    .map(|s| count_var_refs_in_stmt(s, name))
                    .sum::<usize>()
        }
        Stmt::Break | Stmt::Continue | Stmt::LabeledBreak { .. } => 0,
    }
}

/// Replace the first `Var(name)` with `replacement` in an expression.
/// Returns `true` if the substitution was performed.
fn substitute_var_in_expr(
    expr: &mut Expr,
    name: &str,
    replacement: &mut Option<Expr>,
) -> bool {
    if replacement.is_none() {
        return false;
    }

    if let Expr::Var(n) = expr {
        if n.as_str() == name {
            *expr = replacement.take().unwrap();
            return true;
        }
        return false;
    }

    match expr {
        Expr::Literal(_) | Expr::GlobalRef(_) | Expr::Var(_) => false,
        Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
            substitute_var_in_expr(lhs, name, replacement)
                || substitute_var_in_expr(rhs, name, replacement)
        }
        Expr::LogicalOr { lhs, rhs } | Expr::LogicalAnd { lhs, rhs } => {
            substitute_var_in_expr(lhs, name, replacement)
                || substitute_var_in_expr(rhs, name, replacement)
        }
        Expr::Unary { expr: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::TypeCheck { expr: inner, .. }
        | Expr::Not(inner)
        | Expr::CoroutineResume(inner)
        | Expr::PostIncrement(inner) => substitute_var_in_expr(inner, name, replacement),
        Expr::Field { object, .. } => substitute_var_in_expr(object, name, replacement),
        Expr::Index { collection, index } => {
            substitute_var_in_expr(collection, name, replacement)
                || substitute_var_in_expr(index, name, replacement)
        }
        Expr::Call { args, .. } | Expr::CoroutineCreate { args, .. } => args
            .iter_mut()
            .any(|a| substitute_var_in_expr(a, name, replacement)),
        Expr::CallIndirect { callee, args } => {
            substitute_var_in_expr(callee, name, replacement)
                || args
                    .iter_mut()
                    .any(|a| substitute_var_in_expr(a, name, replacement))
        }
        Expr::SystemCall { args, .. } => args
            .iter_mut()
            .any(|a| substitute_var_in_expr(a, name, replacement)),
        Expr::MethodCall {
            receiver, args, ..
        } => {
            substitute_var_in_expr(receiver, name, replacement)
                || args
                    .iter_mut()
                    .any(|a| substitute_var_in_expr(a, name, replacement))
        }
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            substitute_var_in_expr(cond, name, replacement)
                || substitute_var_in_expr(then_val, name, replacement)
                || substitute_var_in_expr(else_val, name, replacement)
        }
        Expr::ArrayInit(elems) | Expr::TupleInit(elems) => elems
            .iter_mut()
            .any(|e| substitute_var_in_expr(e, name, replacement)),
        Expr::StructInit { fields, .. } => fields
            .iter_mut()
            .any(|(_, v)| substitute_var_in_expr(v, name, replacement)),
        Expr::Yield(v) => v
            .as_mut()
            .is_some_and(|e| substitute_var_in_expr(e, name, replacement)),
    }
}

/// Replace the first `Var(name)` with `replacement` in a statement.
/// Returns `true` if the substitution was performed.
fn substitute_var_in_stmt(
    stmt: &mut Stmt,
    name: &str,
    replacement: &mut Option<Expr>,
) -> bool {
    if replacement.is_none() {
        return false;
    }

    match stmt {
        Stmt::VarDecl { init, .. } => init
            .as_mut()
            .is_some_and(|e| substitute_var_in_expr(e, name, replacement)),
        Stmt::Assign { target, value } => {
            // A bare Var target is a write — don't substitute into it.
            // Complex targets (Field, Index) contain reads that can be substituted.
            let target_sub = if matches!(target, Expr::Var(v) if v == name) {
                false
            } else {
                substitute_var_in_expr(target, name, replacement)
            };
            target_sub || substitute_var_in_expr(value, name, replacement)
        }
        Stmt::CompoundAssign { target, value, .. } => {
            // CompoundAssign reads AND writes the target, so always substitute.
            substitute_var_in_expr(target, name, replacement)
                || substitute_var_in_expr(value, name, replacement)
        }
        Stmt::Expr(e) => substitute_var_in_expr(e, name, replacement),
        Stmt::If {
            cond,
            then_body,
            else_body,
        } => {
            substitute_var_in_expr(cond, name, replacement)
                || then_body
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
                || else_body
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
        }
        Stmt::While { cond, body } => {
            substitute_var_in_expr(cond, name, replacement)
                || body
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
        }
        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            init.iter_mut()
                .any(|s| substitute_var_in_stmt(s, name, replacement))
                || substitute_var_in_expr(cond, name, replacement)
                || update
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
                || body
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
        }
        Stmt::Loop { body } => body
            .iter_mut()
            .any(|s| substitute_var_in_stmt(s, name, replacement)),
        Stmt::ForOf { iterable, body, .. } => {
            substitute_var_in_expr(iterable, name, replacement)
                || body
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
        }
        Stmt::Return(e) => e
            .as_mut()
            .is_some_and(|e| substitute_var_in_expr(e, name, replacement)),
        Stmt::Dispatch { blocks, .. } => blocks
            .iter_mut()
            .any(|(_, stmts)| {
                stmts
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
            }),
        Stmt::Switch {
            value,
            cases,
            default_body,
        } => {
            substitute_var_in_expr(value, name, replacement)
                || cases.iter_mut().any(|(_, stmts)| {
                    stmts
                        .iter_mut()
                        .any(|s| substitute_var_in_stmt(s, name, replacement))
                })
                || default_body
                    .iter_mut()
                    .any(|s| substitute_var_in_stmt(s, name, replacement))
        }
        Stmt::Break | Stmt::Continue | Stmt::LabeledBreak { .. } => false,
    }
}

// ---------------------------------------------------------------------------
// Forward substitution
// ---------------------------------------------------------------------------

/// Forward-substitute single-use assigns into adjacent use sites.
///
/// For each `x = E;` at position i, if `x` appears exactly once in the
/// remaining body and that use is at position i+1 (adjacent), substitute E
/// directly into the use site and remove the assign.  Also removes dead
/// assigns (zero refs remaining) — keeping E as a bare expression statement
/// if it has side effects.
///
/// Adjacent substitution preserves evaluation order regardless of side effects,
/// because no code executes between the assign and the use.
///
/// Recurses into nested bodies.
pub fn forward_substitute(body: &mut Vec<Stmt>) {
    // Substitute at this level first.
    loop {
        if !try_forward_substitute_one(body) {
            break;
        }
    }
    // Then recurse into nested bodies.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                forward_substitute(then_body);
                forward_substitute(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                forward_substitute(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                forward_substitute(init);
                forward_substitute(update);
                forward_substitute(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    forward_substitute(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    forward_substitute(case_body);
                }
                forward_substitute(default_body);
            }
            _ => {}
        }
    }
}

/// Try to forward-substitute one assign. Returns `true` if a substitution
/// was performed.
fn try_forward_substitute_one(body: &mut Vec<Stmt>) -> bool {
    for i in 0..body.len() {
        // Match: x = E; where x is a Var
        let name = match &body[i] {
            Stmt::Assign {
                target: Expr::Var(name),
                ..
            } => name.clone(),
            _ => continue,
        };

        // Don't substitute assignments to outer-scope variables — removing
        // the assignment would lose the update visible to outer scopes.
        let is_local = body
            .iter()
            .any(|s| matches!(s, Stmt::VarDecl { name: n, .. } if n == &name));
        if !is_local {
            continue;
        }

        // Count ALL references in the remaining body at this scope level.
        let total_refs: usize = body[i + 1..]
            .iter()
            .map(|s| count_var_refs_in_stmt(s, &name))
            .sum();

        if total_refs != 1 {
            continue;
        }

        // Don't substitute if the variable is reassigned in the remaining body.
        // Loop-carried variables (e.g., repeat counters) have 1 read + 1 write
        // per iteration — substituting the init value makes the loop infinite.
        if var_is_reassigned(&body[i + 1..], &name) {
            continue;
        }

        // Adjacent check: the single use (read) must be at position i+1.
        // Use count_var_refs_in_stmt (reads only) — not stmt_references_var
        // which also counts writes. If the adjacent stmt only *writes* the var
        // (e.g., nested assign target), we'd remove the assignment and the
        // substitute would find no read to replace, silently dropping the value.
        if i + 1 >= body.len() || count_var_refs_in_stmt(&body[i + 1], &name) == 0 {
            continue;
        }

        // Extract value and substitute at the use site.
        let value = match body.remove(i) {
            Stmt::Assign { value, .. } => value,
            _ => unreachable!(),
        };

        let mut replacement = Some(value);
        substitute_var_in_stmt(&mut body[i], &name, &mut replacement);
        return true;
    }
    false
}

// ---------------------------------------------------------------------------
// For-each (hasNext2) → for-of rewrite
// ---------------------------------------------------------------------------

/// Rewrite `while (true) { hasNext2 boilerplate ... }` loops into `for (const x of ...)`.
///
/// Detects the pattern emitted by Flash's HasNext2 opcode:
/// ```text
/// while (true) {
///   const vN = Flash_Iterator.hasNext2(obj, idx);
///   obj = vN[0];
///   idx = vN[1];
///   if (!vN[2]) break;
///   ... Flash_Iterator.nextValue(obj, idx) ... OR ... Flash_Iterator.nextName(obj, idx) ...
/// }
/// ```
/// and rewrites to:
/// ```text
/// for (const binding of Object.values(obj)) { ... }
/// ```
///
/// Recurses into all nested statement bodies.
pub fn rewrite_foreach_loops(body: &mut [Stmt]) {
    // Rewrite at this level first (bottom-up: recurse into children afterward
    // since the transform replaces the Loop with ForOf).
    let mut i = 0;
    while i < body.len() {
        if let Some(for_of) = try_rewrite_foreach(&body[i]) {
            body[i] = for_of;
        }
        i += 1;
    }
    // Recurse into nested bodies.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                rewrite_foreach_loops(then_body);
                rewrite_foreach_loops(else_body);
            }
            Stmt::While { body, .. }
            | Stmt::Loop { body }
            | Stmt::ForOf { body, .. } => {
                rewrite_foreach_loops(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                rewrite_foreach_loops(init);
                rewrite_foreach_loops(update);
                rewrite_foreach_loops(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    rewrite_foreach_loops(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    rewrite_foreach_loops(case_body);
                }
                rewrite_foreach_loops(default_body);
            }
            _ => {}
        }
    }
}

/// Try to match and rewrite a single `Stmt::Loop` as a for-of.
fn try_rewrite_foreach(stmt: &Stmt) -> Option<Stmt> {
    let loop_body = match stmt {
        Stmt::Loop { body } => body,
        _ => return None,
    };

    // Find the hasNext2 VarDecl. It can be at position 0 (no preceding decls)
    // or after some VarDecl/Assign statements that declare the iterator variables.
    let hn2_idx = loop_body.iter().position(|s| {
        matches!(
            s,
            Stmt::VarDecl {
                init: Some(Expr::SystemCall { system, method, .. }),
                ..
            } if system == "Flash.Iterator" && method == "hasNext2"
        )
    })?;

    // Need at least 4 statements from hn2_idx onward: hasNext2, assign[0], assign[1], if-break
    if loop_body.len() < hn2_idx + 5 {
        return None;
    }

    // [hn2_idx] VarDecl { name: tmp, init: SystemCall("Flash.Iterator", "hasNext2", [obj, idx]) }
    let (tmp_name, obj_expr) = match &loop_body[hn2_idx] {
        Stmt::VarDecl {
            name,
            init: Some(Expr::SystemCall { args, .. }),
            ..
        } if args.len() == 2 => (name.as_str(), &args[0]),
        _ => return None,
    };

    // [hn2_idx+1] Assign { target: Var(obj_name), value: Index(Var(tmp), 0) }
    let obj_name = match_index_assign(&loop_body[hn2_idx + 1], tmp_name, 0)?;

    // [hn2_idx+2] Assign { target: Var(idx_name), value: Index(Var(tmp), 1) }
    let idx_name = match_index_assign(&loop_body[hn2_idx + 2], tmp_name, 1)?;

    // [hn2_idx+3] If { cond: Not(Index(Var(tmp), 2)), then: [Break], else: [] }
    if !matches!(
        &loop_body[hn2_idx + 3],
        Stmt::If {
            cond: Expr::Not(inner),
            then_body,
            else_body,
        } if else_body.is_empty()
            && then_body.len() == 1
            && matches!(&then_body[0], Stmt::Break)
            && matches!(
                inner.as_ref(),
                Expr::Index { collection, index }
                    if matches!(collection.as_ref(), Expr::Var(v) if v == tmp_name)
                    && matches!(index.as_ref(), Expr::Literal(Constant::Int(2)))
            )
    ) {
        return None;
    }

    // Remaining body after the header (4 stmts starting at hn2_idx).
    let remaining = &loop_body[hn2_idx + 4..];

    // Find the first nextValue or nextName call in the remaining body.
    let (next_method, next_stmt_idx) = find_next_call(remaining, &obj_name, &idx_name)?;

    // Determine the iterable wrapper.
    let wrapper = match next_method {
        "nextValue" => "Object.values",
        "nextName" => "Object.keys",
        _ => return None,
    };

    let iterable = Expr::Call {
        func: wrapper.to_string(),
        args: vec![obj_expr.clone()],
    };

    // Build the new body by cloning remaining stmts and extracting the binding.
    let mut new_body: Vec<Stmt> = remaining.to_vec();

    // Extract binding from the statement containing the next call.
    let (binding, declare) = extract_binding_and_replace(&mut new_body, next_stmt_idx, &obj_name, &idx_name)?;

    Some(Stmt::ForOf {
        binding,
        declare,
        iterable,
        body: new_body,
    })
}

/// Match `target = tmp[index_val]` and return the target variable name.
fn match_index_assign(stmt: &Stmt, tmp_name: &str, index_val: i64) -> Option<String> {
    match stmt {
        Stmt::Assign {
            target: Expr::Var(name),
            value: Expr::Index { collection, index },
        } => {
            if !matches!(collection.as_ref(), Expr::Var(v) if v == tmp_name) {
                return None;
            }
            if !matches!(index.as_ref(), Expr::Literal(Constant::Int(i)) if *i == index_val) {
                return None;
            }
            Some(name.clone())
        }
        _ => None,
    }
}

/// Find the first statement in `body` containing a `Flash_Iterator.nextValue` or `nextName`
/// call with the expected args. Returns the method name and index.
fn find_next_call<'a>(body: &'a [Stmt], obj_name: &str, idx_name: &str) -> Option<(&'a str, usize)> {
    for (i, stmt) in body.iter().enumerate() {
        if let Some(method) = stmt_contains_next_call(stmt, obj_name, idx_name) {
            return Some((method, i));
        }
    }
    None
}

/// Check if a statement contains a Flash_Iterator.nextValue/nextName call.
/// Returns the method name if found.
fn stmt_contains_next_call<'a>(stmt: &'a Stmt, obj_name: &str, idx_name: &str) -> Option<&'a str> {
    match stmt {
        Stmt::VarDecl { init: Some(e), .. } => expr_contains_next_call(e, obj_name, idx_name),
        Stmt::Assign { value, .. } => expr_contains_next_call(value, obj_name, idx_name),
        Stmt::Expr(e) => expr_contains_next_call(e, obj_name, idx_name),
        Stmt::If { cond, .. } => expr_contains_next_call(cond, obj_name, idx_name),
        _ => None,
    }
}

/// Check if an expression IS or CONTAINS a Flash_Iterator.nextValue/nextName call.
fn expr_contains_next_call<'a>(expr: &'a Expr, obj_name: &str, idx_name: &str) -> Option<&'a str> {
    match expr {
        Expr::SystemCall {
            system,
            method,
            args,
        } if system == "Flash.Iterator"
            && (method == "nextValue" || method == "nextName")
            && args.len() == 2
            && matches!(&args[0], Expr::Var(v) if v == obj_name)
            && matches!(&args[1], Expr::Var(v) if v == idx_name) =>
        {
            Some(method.as_str())
        }
        Expr::Cast { expr, .. } => expr_contains_next_call(expr, obj_name, idx_name),
        Expr::Call { args, .. } | Expr::SystemCall { args, .. } => {
            args.iter().find_map(|a| expr_contains_next_call(a, obj_name, idx_name))
        }
        Expr::CallIndirect { callee, args } => {
            expr_contains_next_call(callee, obj_name, idx_name)
                .or_else(|| args.iter().find_map(|a| expr_contains_next_call(a, obj_name, idx_name)))
        }
        Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
            expr_contains_next_call(lhs, obj_name, idx_name)
                .or_else(|| expr_contains_next_call(rhs, obj_name, idx_name))
        }
        Expr::Unary { expr, .. } | Expr::Not(expr) => {
            expr_contains_next_call(expr, obj_name, idx_name)
        }
        Expr::Index { collection, index } => {
            expr_contains_next_call(collection, obj_name, idx_name)
                .or_else(|| expr_contains_next_call(index, obj_name, idx_name))
        }
        Expr::Field { object, .. } => expr_contains_next_call(object, obj_name, idx_name),
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            expr_contains_next_call(cond, obj_name, idx_name)
                .or_else(|| expr_contains_next_call(then_val, obj_name, idx_name))
                .or_else(|| expr_contains_next_call(else_val, obj_name, idx_name))
        }
        Expr::ArrayInit(elems) => {
            elems.iter().find_map(|e| expr_contains_next_call(e, obj_name, idx_name))
        }
        Expr::TypeCheck { expr, .. } => expr_contains_next_call(expr, obj_name, idx_name),
        _ => None,
    }
}

/// Extract the binding from the next-call statement and replace the call with `Var(binding)`.
///
/// Returns `(binding_name, declare)` where `declare` is true if the for-of should
/// declare the variable with `const`.
fn extract_binding_and_replace(
    body: &mut Vec<Stmt>,
    idx: usize,
    obj_name: &str,
    idx_name: &str,
) -> Option<(String, bool)> {
    // Case 1: VarDecl { name, init: nextCall | Cast(nextCall) }
    if let Stmt::VarDecl {
        name,
        init: Some(init),
        ..
    } = &body[idx]
    {
        let is_direct = is_next_call(init, obj_name, idx_name);
        let is_cast_wrapped = matches!(init, Expr::Cast { expr, .. } if is_next_call(expr, obj_name, idx_name));
        if is_direct || is_cast_wrapped {
            let binding = name.clone();
            body.remove(idx);
            return Some((binding, true));
        }
    }

    // Case 2: Assign { target: Var(name), value: nextCall | Cast(nextCall) }
    if let Stmt::Assign {
        target: Expr::Var(name),
        value,
    } = &body[idx]
    {
        let is_direct = is_next_call(value, obj_name, idx_name);
        let is_cast_wrapped = matches!(value, Expr::Cast { expr, .. } if is_next_call(expr, obj_name, idx_name));
        if is_direct || is_cast_wrapped {
            let binding = name.clone();
            body.remove(idx);
            return Some((binding, false));
        }
    }

    // Case 3: The nextCall is embedded deeper. Replace it in-place with a
    // synthetic `$item` variable and use that as the for-of binding.
    let binding = "$item".to_string();
    let replaced = replace_next_call_in_stmt(&mut body[idx], obj_name, idx_name, &binding);
    if replaced {
        Some((binding, true))
    } else {
        None
    }
}

/// Check if an expression is exactly a Flash_Iterator.nextValue/nextName call.
fn is_next_call(expr: &Expr, obj_name: &str, idx_name: &str) -> bool {
    matches!(
        expr,
        Expr::SystemCall { system, method, args }
            if system == "Flash.Iterator"
            && (method == "nextValue" || method == "nextName")
            && args.len() == 2
            && matches!(&args[0], Expr::Var(v) if v == obj_name)
            && matches!(&args[1], Expr::Var(v) if v == idx_name)
    )
}

/// Replace the first occurrence of nextValue/nextName call in a statement
/// with `Var(binding)`. Returns true if replacement was performed.
fn replace_next_call_in_stmt(stmt: &mut Stmt, obj_name: &str, idx_name: &str, binding: &str) -> bool {
    match stmt {
        Stmt::VarDecl { init: Some(e), .. } => replace_next_call_in_expr(e, obj_name, idx_name, binding),
        Stmt::Assign { value, .. } => replace_next_call_in_expr(value, obj_name, idx_name, binding),
        Stmt::Expr(e) => replace_next_call_in_expr(e, obj_name, idx_name, binding),
        Stmt::CompoundAssign { value, .. } => replace_next_call_in_expr(value, obj_name, idx_name, binding),
        Stmt::If { cond, .. } => replace_next_call_in_expr(cond, obj_name, idx_name, binding),
        _ => false,
    }
}

/// Replace the first occurrence of a Flash_Iterator next call with `Var(binding)`.
/// Handles Cast wrappers: `Cast(nextCall)` → `Var(binding)` (strip the cast).
fn replace_next_call_in_expr(expr: &mut Expr, obj_name: &str, idx_name: &str, binding: &str) -> bool {
    // Check if this expression is the target (possibly wrapped in Cast).
    if is_next_call(expr, obj_name, idx_name) {
        *expr = Expr::Var(binding.to_string());
        return true;
    }
    if let Expr::Cast { expr: inner, .. } = expr {
        if is_next_call(inner, obj_name, idx_name) {
            *expr = Expr::Var(binding.to_string());
            return true;
        }
    }

    // Recurse into sub-expressions.
    match expr {
        Expr::Cast { expr, .. } => replace_next_call_in_expr(expr, obj_name, idx_name, binding),
        Expr::Call { args, .. } | Expr::SystemCall { args, .. } => {
            args.iter_mut().any(|a| replace_next_call_in_expr(a, obj_name, idx_name, binding))
        }
        Expr::CallIndirect { callee, args } => {
            replace_next_call_in_expr(callee, obj_name, idx_name, binding)
                || args.iter_mut().any(|a| replace_next_call_in_expr(a, obj_name, idx_name, binding))
        }
        Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
            replace_next_call_in_expr(lhs, obj_name, idx_name, binding)
                || replace_next_call_in_expr(rhs, obj_name, idx_name, binding)
        }
        Expr::Unary { expr, .. } | Expr::Not(expr) => {
            replace_next_call_in_expr(expr, obj_name, idx_name, binding)
        }
        Expr::Index { collection, index } => {
            replace_next_call_in_expr(collection, obj_name, idx_name, binding)
                || replace_next_call_in_expr(index, obj_name, idx_name, binding)
        }
        Expr::Field { object, .. } => replace_next_call_in_expr(object, obj_name, idx_name, binding),
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            replace_next_call_in_expr(cond, obj_name, idx_name, binding)
                || replace_next_call_in_expr(then_val, obj_name, idx_name, binding)
                || replace_next_call_in_expr(else_val, obj_name, idx_name, binding)
        }
        Expr::ArrayInit(elems) => {
            elems.iter_mut().any(|e| replace_next_call_in_expr(e, obj_name, idx_name, binding))
        }
        Expr::TypeCheck { expr, .. } => replace_next_call_in_expr(expr, obj_name, idx_name, binding),
        Expr::LogicalOr { lhs, rhs } | Expr::LogicalAnd { lhs, rhs } => {
            replace_next_call_in_expr(lhs, obj_name, idx_name, binding)
                || replace_next_call_in_expr(rhs, obj_name, idx_name, binding)
        }
        _ => false,
    }
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

/// Strip `AsType` casts to get the underlying expression.
/// `AsType` is a pure type assertion with no runtime effect, so it's safe
/// to look through for structural comparison.
fn strip_as_type(expr: &Expr) -> &Expr {
    match expr {
        Expr::Cast {
            expr: inner,
            kind: CastKind::AsType,
            ..
        } => strip_as_type(inner),
        _ => expr,
    }
}

/// Check whether an assignment matches the compound assignment pattern.
fn match_compound_assign(target: &Expr, value: &Expr) -> Option<Stmt> {
    let (op, lhs, rhs) = match value {
        Expr::Binary { op, lhs, rhs } => (*op, lhs.as_ref(), rhs.as_ref()),
        _ => return None,
    };

    // Look through AsType casts when comparing lhs to target.
    if strip_as_type(lhs) != target {
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
/// Recurses into nested bodies after merging at the current level.
pub fn merge_decl_init(body: &mut Vec<Stmt>) {
    // Merge at this level first.
    loop {
        if !try_merge_one_decl(body) {
            break;
        }
    }
    // Then recurse into nested bodies.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                merge_decl_init(then_body);
                merge_decl_init(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                merge_decl_init(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                merge_decl_init(init);
                merge_decl_init(update);
                merge_decl_init(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    merge_decl_init(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    merge_decl_init(case_body);
                }
                merge_decl_init(default_body);
            }
            _ => {}
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

        Stmt::ForOf {
            binding,
            iterable,
            body,
            ..
        } => {
            binding == name
                || expr_references_var(iterable, name)
                || body.iter().any(|s| stmt_references_var(s, name))
        }

        Stmt::Return(e) => e.as_ref().is_some_and(|e| expr_references_var(e, name)),

        Stmt::Dispatch { blocks, .. } => blocks
            .iter()
            .any(|(_, stmts)| stmts.iter().any(|s| stmt_references_var(s, name))),

        Stmt::Switch {
            value,
            cases,
            default_body,
        } => {
            expr_references_var(value, name)
                || cases
                    .iter()
                    .any(|(_, stmts)| stmts.iter().any(|s| stmt_references_var(s, name)))
                || default_body.iter().any(|s| stmt_references_var(s, name))
        }

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
        | Expr::CoroutineResume(inner)
        | Expr::PostIncrement(inner) => expr_references_var(inner, name),
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
        Expr::MethodCall {
            receiver, args, ..
        } => {
            expr_references_var(receiver, name)
                || args.iter().any(|a| expr_references_var(a, name))
        }
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
// Scope narrowing
// ---------------------------------------------------------------------------

/// Push uninitialized `let` declarations into the innermost scope that uses them.
///
/// When a `let vN: T;` at the current scope is referenced only inside a single
/// child scope body (one if-branch, one loop body, etc.), move the declaration
/// into that child body. This enables `merge_decl_init` and `fold_single_use_consts`
/// to operate on the declaration once it shares scope with its assignment/use.
///
/// Recurses into nested scopes after narrowing.
pub fn narrow_var_scope(body: &mut Vec<Stmt>) {
    // Narrow at this level first.
    loop {
        if !try_narrow_one(body) {
            break;
        }
    }
    // Then recurse into nested bodies.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                narrow_var_scope(then_body);
                narrow_var_scope(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                narrow_var_scope(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                narrow_var_scope(init);
                narrow_var_scope(update);
                narrow_var_scope(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    narrow_var_scope(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    narrow_var_scope(case_body);
                }
                narrow_var_scope(default_body);
            }
            _ => {}
        }
    }
}

/// Try to narrow a single uninit VarDecl into a child scope.
/// Returns `true` if a narrowing was performed.
fn try_narrow_one(body: &mut Vec<Stmt>) -> bool {
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

        // Find which statements after the decl reference this variable.
        let ref_indices: Vec<usize> = (i + 1..body.len())
            .filter(|&j| stmt_references_var(&body[j], &name))
            .collect();

        if ref_indices.len() != 1 {
            continue;
        }

        let j = ref_indices[0];

        // The single referencing statement must have child bodies we can push into.
        // Find which single child body contains ALL references.
        if let Some(target_body) = find_unique_child_body(&mut body[j], &name) {
            // Insert the uninit decl at the top of that child body.
            target_body.insert(
                0,
                Stmt::VarDecl {
                    name,
                    ty,
                    init: None,
                    mutable: true,
                },
            );
            // Remove the original decl.
            body.remove(i);
            return true;
        }
    }
    false
}

/// If ALL references to `name` in `stmt` are inside exactly one child body,
/// return a mutable reference to that body. Otherwise return `None`.
fn find_unique_child_body<'a>(stmt: &'a mut Stmt, name: &str) -> Option<&'a mut Vec<Stmt>> {
    match stmt {
        Stmt::If {
            cond,
            then_body,
            else_body,
        } => {
            // References in the condition are not inside a child body.
            if expr_references_var(cond, name) {
                return None;
            }
            let in_then = then_body.iter().any(|s| stmt_references_var(s, name));
            let in_else = else_body.iter().any(|s| stmt_references_var(s, name));
            match (in_then, in_else) {
                (true, false) => Some(then_body),
                (false, true) => Some(else_body),
                _ => None, // both or neither
            }
        }
        Stmt::While { cond, body } => {
            if expr_references_var(cond, name) {
                return None;
            }
            if body.iter().any(|s| stmt_references_var(s, name)) {
                Some(body)
            } else {
                None
            }
        }
        Stmt::Loop { body } => {
            if body.iter().any(|s| stmt_references_var(s, name)) {
                Some(body)
            } else {
                None
            }
        }
        Stmt::ForOf {
            binding,
            iterable,
            body,
            ..
        } => {
            if binding == name || expr_references_var(iterable, name) {
                return None;
            }
            if body.iter().any(|s| stmt_references_var(s, name)) {
                Some(body)
            } else {
                None
            }
        }
        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            if expr_references_var(cond, name) {
                return None;
            }
            let in_init = init.iter().any(|s| stmt_references_var(s, name));
            let in_update = update.iter().any(|s| stmt_references_var(s, name));
            let in_body = body.iter().any(|s| stmt_references_var(s, name));
            let count = usize::from(in_init) + usize::from(in_update) + usize::from(in_body);
            if count != 1 {
                return None;
            }
            if in_init {
                Some(init)
            } else if in_body {
                Some(body)
            } else {
                Some(update)
            }
        }
        Stmt::Dispatch { blocks, .. } => {
            let mut found = None;
            for (idx, (_, block_body)) in blocks.iter().enumerate() {
                if block_body.iter().any(|s| stmt_references_var(s, name)) {
                    if found.is_some() {
                        return None; // multiple blocks reference it
                    }
                    found = Some(idx);
                }
            }
            found.map(|idx| &mut blocks[idx].1)
        }
        Stmt::Switch {
            cases,
            default_body,
            ..
        } => {
            let mut found_case = None;
            let in_default = default_body.iter().any(|s| stmt_references_var(s, name));
            for (idx, (_, case_body)) in cases.iter().enumerate() {
                if case_body.iter().any(|s| stmt_references_var(s, name)) {
                    if found_case.is_some() || in_default {
                        return None; // multiple branches reference it
                    }
                    found_case = Some(idx);
                }
            }
            if let Some(idx) = found_case {
                if in_default {
                    return None;
                }
                Some(&mut cases[idx].1)
            } else if in_default {
                Some(default_body)
            } else {
                None
            }
        }
        // Not a compound statement — can't narrow into it.
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Forwarding stub elimination
// ---------------------------------------------------------------------------

/// Remove `let vN: T; vM = vN;` stubs where `vN` is uninit and has no other refs.
///
/// These are structurizer artifacts from empty else-branches: an uninit phi
/// variable is immediately forwarded to another phi. Both the decl and the
/// forwarding assign are meaningless. Recurses into nested bodies.
pub fn eliminate_forwarding_stubs(body: &mut Vec<Stmt>) {
    loop {
        if !try_eliminate_one_stub(body) {
            break;
        }
    }
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                eliminate_forwarding_stubs(then_body);
                eliminate_forwarding_stubs(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                eliminate_forwarding_stubs(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                eliminate_forwarding_stubs(init);
                eliminate_forwarding_stubs(update);
                eliminate_forwarding_stubs(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    eliminate_forwarding_stubs(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    eliminate_forwarding_stubs(case_body);
                }
                eliminate_forwarding_stubs(default_body);
            }
            _ => {}
        }
    }
}

fn try_eliminate_one_stub(body: &mut Vec<Stmt>) -> bool {
    for i in 0..body.len().saturating_sub(1) {
        // Match: let vN: T; (uninit, mutable)
        let name = match &body[i] {
            Stmt::VarDecl {
                name,
                init: None,
                mutable: true,
                ..
            } => name.clone(),
            _ => continue,
        };

        // Next statement must be: vM = vN;
        let is_forwarding = matches!(
            &body[i + 1],
            Stmt::Assign { value: Expr::Var(v), .. } if v == &name
        );
        if !is_forwarding {
            continue;
        }

        // vN must have no other references in the body (only the forwarding assign).
        let other_refs: usize = body[i + 2..]
            .iter()
            .map(|s| count_var_refs_in_stmt(s, &name))
            .sum();
        if other_refs != 0 {
            continue;
        }

        // Remove both the decl and the forwarding assign.
        body.remove(i + 1);
        body.remove(i);
        return true;
    }
    false
}

// ---------------------------------------------------------------------------
// Self-assignment elimination
// ---------------------------------------------------------------------------

/// Remove no-op self-assignments (`x = x;`) produced by out-of-SSA coalescing.
///
/// When multiple SSA values share a name, pass-through branches emit `x = x`
/// which is a no-op. This runs BEFORE ternary detection so that self-assigns
/// don't bloat if/else branches and prevent ternary pattern matching.
pub fn eliminate_self_assigns(body: &mut Vec<Stmt>) {
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                eliminate_self_assigns(then_body);
                eliminate_self_assigns(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                eliminate_self_assigns(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                eliminate_self_assigns(init);
                eliminate_self_assigns(update);
                eliminate_self_assigns(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    eliminate_self_assigns(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    eliminate_self_assigns(case_body);
                }
                eliminate_self_assigns(default_body);
            }
            _ => {}
        }
    }
    body.retain(|stmt| !is_self_assign(stmt));

    // Fix empty-then-body if/else by flipping condition.
    // After self-assign removal, some if-bodies become empty while their
    // else-body is non-empty: `if (c) {} else { body }` → `if (!c) { body }`.
    for stmt in body.iter_mut() {
        if let Stmt::If {
            cond,
            then_body,
            else_body,
        } = stmt
        {
            if then_body.is_empty() && !else_body.is_empty() {
                // Replace cond with a placeholder, negate it, and put it back.
                let old_cond = std::mem::replace(cond, Expr::Literal(Constant::Bool(false)));
                *cond = negate_expr(old_cond);
                std::mem::swap(then_body, else_body);
            }
        }
    }

    // Remove fully-empty if/else statements (both branches empty after cleanup).
    body.retain(|stmt| {
        !matches!(stmt, Stmt::If { then_body, else_body, .. }
            if then_body.is_empty() && else_body.is_empty())
    });
}

fn is_self_assign(stmt: &Stmt) -> bool {
    matches!(stmt, Stmt::Assign { target: Expr::Var(t), value: Expr::Var(v) } if t == v)
}

/// Remove consecutive duplicate assignments (`x = a; x = a;` → `x = a;`).
///
/// Structurizer duplicate edges emit the same phi-assignment in both arms of a
/// diamond that converge to the same merge block. After ternary/other rewrites
/// these can end up adjacent. Recurses into nested bodies.
pub fn eliminate_duplicate_assigns(body: &mut Vec<Stmt>) {
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                eliminate_duplicate_assigns(then_body);
                eliminate_duplicate_assigns(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                eliminate_duplicate_assigns(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                eliminate_duplicate_assigns(init);
                eliminate_duplicate_assigns(update);
                eliminate_duplicate_assigns(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    eliminate_duplicate_assigns(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    eliminate_duplicate_assigns(case_body);
                }
                eliminate_duplicate_assigns(default_body);
            }
            _ => {}
        }
    }

    // Remove consecutive duplicate assigns.
    let mut i = 0;
    while i + 1 < body.len() {
        let is_dup = match (&body[i], &body[i + 1]) {
            (
                Stmt::Assign {
                    target: t1,
                    value: v1,
                },
                Stmt::Assign {
                    target: t2,
                    value: v2,
                },
            ) => t1 == t2 && v1 == v2,
            _ => false,
        };
        if is_dup {
            body.remove(i + 1);
        } else {
            i += 1;
        }
    }
}

/// Check whether every path through a body exits unconditionally
/// (return, break, continue, or labeled break).
fn body_always_exits(body: &[Stmt]) -> bool {
    match body.last() {
        Some(Stmt::Return(_) | Stmt::Break | Stmt::Continue | Stmt::LabeledBreak { .. }) => true,
        Some(Stmt::If {
            then_body,
            else_body,
            ..
        }) => !else_body.is_empty() && body_always_exits(then_body) && body_always_exits(else_body),
        _ => false,
    }
}

/// Absorb split-path phi conditions into their assigning branch.
///
/// Matches:
/// ```text
/// let vN: T;
/// ...
/// if (C) { ...; vN = E; } else { B; }
/// if (vN) { D; }
/// ```
/// and rewrites to:
/// ```text
/// ...
/// if (C) { ...; if (E) { D; } } else { B; }
/// ```
///
/// When the `if (vN)` has an else body and the then-body always exits,
/// the else body is pulled out as the continuation after the outer if.
///
/// This eliminates split-path phi booleans that are assigned in one
/// branch and left undefined (= false) on fallthrough paths.
pub fn absorb_phi_condition(body: &mut Vec<Stmt>) {
    loop {
        if !try_absorb_phi_condition(body) {
            break;
        }
    }
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                absorb_phi_condition(then_body);
                absorb_phi_condition(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                absorb_phi_condition(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                absorb_phi_condition(init);
                absorb_phi_condition(update);
                absorb_phi_condition(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    absorb_phi_condition(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    absorb_phi_condition(case_body);
                }
                absorb_phi_condition(default_body);
            }
            _ => {}
        }
    }
}

fn try_absorb_phi_condition(body: &mut Vec<Stmt>) -> bool {
    for i in 0..body.len().saturating_sub(1) {
        // Match: if (C) { ...; vN = E; } else { B; }
        let var_name = match &body[i] {
            Stmt::If { then_body, .. } => match then_body.last() {
                Some(Stmt::Assign {
                    target: Expr::Var(name),
                    ..
                }) => name.clone(),
                _ => continue,
            },
            _ => continue,
        };

        // Must have a corresponding uninit decl earlier in this scope.
        let decl_idx = (0..i).rev().find(|&j| {
            matches!(
                &body[j],
                Stmt::VarDecl { name, init: None, .. } if name == &var_name
            )
        });
        let Some(decl_idx) = decl_idx else {
            continue;
        };

        // vN must not appear between the decl and the outer if (ensures it's
        // a dedicated phi variable, not a general-purpose variable with earlier
        // assignments like `dodged = 1.0; ... if (x) { dodged = 4.0; }`).
        let refs_before: usize = body[decl_idx + 1..i]
            .iter()
            .map(|s| count_var_refs_in_stmt(s, &var_name))
            .sum();
        if refs_before > 0 {
            continue;
        }

        // vN must not appear in the else_body at all (reads or writes).
        // Use stmt_references_var which checks both, unlike
        // count_var_refs_in_stmt which only counts reads.
        let else_has_refs = match &body[i] {
            Stmt::If { else_body, .. } => else_body
                .iter()
                .any(|s| stmt_references_var(s, &var_name)),
            _ => unreachable!(),
        };
        if else_has_refs {
            continue;
        }

        // Next statement must be if (expr_with_vN) { D } [else { D2 }].
        let (use_cond, use_then, use_else) = match &body[i + 1] {
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => (cond, then_body, else_body),
            _ => continue,
        };

        // use_cond must reference vN exactly once.
        if count_var_refs_in_expr(use_cond, &var_name) != 1 {
            continue;
        }

        // No refs to vN anywhere after body[i+1].
        let refs_after: usize = body[i + 2..]
            .iter()
            .map(|s| count_var_refs_in_stmt(s, &var_name))
            .sum();
        if refs_after != 0 {
            continue;
        }

        // Case A: use_else is empty — simple absorption.
        // Case B: use_else non-empty, use_then always exits — pull else out.
        // Case C: use_else non-empty, neither exits — duplicate else into outer else.

        // Clone values before mutating.
        let assign_value = match &body[i] {
            Stmt::If { then_body, .. } => match then_body.last() {
                Some(Stmt::Assign { value, .. }) => value.clone(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        let mut new_cond = use_cond.clone();
        let use_then = use_then.clone();
        let use_else = use_else.clone();

        // Substitute E for vN in the condition.
        let mut replacement = Some(assign_value);
        substitute_var_in_expr(&mut new_cond, &var_name, &mut replacement);

        let is_case_c = !use_else.is_empty() && !body_always_exits(&use_then);

        // Build the merged if.
        // Case A/B: no else on the merged if (Case B's else becomes continuation).
        // Case C: keep the full else — the then-branch gets `if (E) { D } else { F }`.
        let merged_if = Stmt::If {
            cond: new_cond,
            then_body: use_then,
            else_body: if is_case_c { use_else.clone() } else { vec![] },
        };

        // Modify outer if's then_body: remove vN = E, append merged_if.
        if let Stmt::If {
            then_body,
            else_body,
            ..
        } = &mut body[i]
        {
            then_body.pop();
            then_body.push(merged_if);

            // Case C: append use_else to outer else_body too — when vN is
            // unassigned (falsy/undefined), the use-site if always takes
            // the else path.
            if is_case_c {
                else_body.extend(use_else.clone());
            }
        }

        // Remove the if(vN) statement.
        body.remove(i + 1);

        // Case B: insert use_else as continuation after body[i].
        if !use_else.is_empty() && !is_case_c {
            for (j, stmt) in use_else.into_iter().enumerate() {
                body.insert(i + 1 + j, stmt);
            }
        }

        // Remove the uninit decl.
        body.remove(decl_idx);

        return true;
    }
    false
}

/// Negate an expression, folding into comparisons when possible.
fn negate_expr(expr: Expr) -> Expr {
    match expr {
        Expr::Not(inner) => *inner,
        Expr::Cmp { kind, lhs, rhs } => Expr::Cmp {
            kind: kind.inverse(),
            lhs,
            rhs,
        },
        other => Expr::Not(Box::new(other)),
    }
}

// ---------------------------------------------------------------------------
// Post-increment rewrite
// ---------------------------------------------------------------------------

/// Rewrite read-modify-write patterns to post-increment.
///
/// Matches:
/// ```text
/// const vN = TARGET;
/// TARGET = (vN as number) + 1;  // or: TARGET = vN + 1
/// ... vN ...                     // exactly one remaining use
/// ```
/// and rewrites to:
/// ```text
/// ... TARGET++ ...
/// ```
///
/// Recurses into all nested statement bodies.
pub fn rewrite_post_increment(body: &mut Vec<Stmt>) {
    // Rewrite at this level.
    loop {
        if !try_rewrite_one_post_increment(body) {
            break;
        }
    }
    // Recurse into nested bodies.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                rewrite_post_increment(then_body);
                rewrite_post_increment(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                rewrite_post_increment(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                rewrite_post_increment(init);
                rewrite_post_increment(update);
                rewrite_post_increment(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    rewrite_post_increment(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    rewrite_post_increment(case_body);
                }
                rewrite_post_increment(default_body);
            }
            _ => {}
        }
    }
}

/// Try to find and rewrite one post-increment pattern. Returns true if a
/// rewrite was performed.
fn try_rewrite_one_post_increment(body: &mut Vec<Stmt>) -> bool {
    for i in 0..body.len().saturating_sub(1) {
        // Match: const vN = TARGET;
        let (var_name, target) = match &body[i] {
            Stmt::VarDecl {
                name,
                init: Some(init),
                mutable: false,
                ..
            } => (name.clone(), init.clone()),
            _ => continue,
        };

        // Match: TARGET = (vN [as number]) + 1;
        let is_increment = match &body[i + 1] {
            Stmt::Assign { target: tgt, value } => {
                tgt == &target && is_var_plus_one(value, &var_name)
            }
            _ => false,
        };
        if !is_increment {
            continue;
        }

        // Count remaining uses of vN in body[i+2..].
        let remaining_refs: usize = body[i + 2..]
            .iter()
            .map(|s| count_var_refs_in_stmt(s, &var_name))
            .sum();

        if remaining_refs == 1 {
            // Substitute the single remaining use with TARGET++.
            let inc_expr = Expr::PostIncrement(Box::new(target));
            let mut replacement = Some(inc_expr);
            for s in &mut body[i + 2..] {
                if substitute_var_in_stmt(s, &var_name, &mut replacement) {
                    break;
                }
            }
            // Remove the const and the increment assignment.
            body.remove(i + 1);
            body.remove(i);
            return true;
        } else if remaining_refs == 0 {
            // No remaining reads — emit TARGET++ as a bare expression statement.
            let inc_expr = Expr::PostIncrement(Box::new(target));
            body.remove(i + 1);
            body[i] = Stmt::Expr(inc_expr);
            return true;
        }
    }
    false
}

/// Check whether `expr` is `vN + 1` or `(vN as T) + 1`.
fn is_var_plus_one(expr: &Expr, var_name: &str) -> bool {
    if let Expr::Binary {
        op: BinOp::Add,
        lhs,
        rhs,
    } = expr
    {
        let is_one = match rhs.as_ref() {
            Expr::Literal(Constant::Int(1)) => true,
            Expr::Literal(Constant::Float(f)) => *f == 1.0,
            _ => false,
        };
        if !is_one {
            return false;
        }
        // lhs is either Var(vN) or Cast { expr: Var(vN), .. }
        match lhs.as_ref() {
            Expr::Var(n) => n == var_name,
            Expr::Cast { expr: inner, .. } => matches!(inner.as_ref(), Expr::Var(n) if n == var_name),
            _ => false,
        }
    } else {
        false
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Count total statements recursively (used as fixpoint termination check).
pub fn count_stmts(body: &[Stmt]) -> usize {
    body.iter()
        .map(|s| match s {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => 1 + count_stmts(then_body) + count_stmts(else_body),
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => 1 + count_stmts(body),
            Stmt::For {
                init,
                update,
                body,
                ..
            } => 1 + count_stmts(init) + count_stmts(update) + count_stmts(body),
            Stmt::Dispatch { blocks, .. } => {
                1 + blocks.iter().map(|(_, b)| count_stmts(b)).sum::<usize>()
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                1 + cases.iter().map(|(_, b)| count_stmts(b)).sum::<usize>()
                    + count_stmts(default_body)
            }
            _ => 1,
        })
        .sum()
}

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
        Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
            pass(body);
        }
        Stmt::Dispatch { blocks, .. } => {
            for (_, block_body) in blocks {
                pass(block_body);
            }
        }
        Stmt::Switch {
            cases,
            default_body,
            ..
        } => {
            for (_, case_body) in cases {
                pass(case_body);
            }
            pass(default_body);
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Invert empty-then if-blocks
// ---------------------------------------------------------------------------

/// Invert `if (x) {} else { ... }` to `if (!x) { ... }`.
///
/// When the then-body is empty and the else-body is non-empty, negates the
/// condition and swaps the bodies. Recurses into all nested statement bodies.
pub fn invert_empty_then(body: &mut [Stmt]) {
    for stmt in body.iter_mut() {
        // Recurse first (children before parent).
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                invert_empty_then(then_body);
                invert_empty_then(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                invert_empty_then(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                invert_empty_then(init);
                invert_empty_then(update);
                invert_empty_then(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    invert_empty_then(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    invert_empty_then(case_body);
                }
                invert_empty_then(default_body);
            }
            _ => {}
        }

        // Then try to invert this statement.
        if let Stmt::If {
            cond,
            then_body,
            else_body,
        } = stmt
        {
            if then_body.is_empty() && !else_body.is_empty() {
                let old_cond = std::mem::replace(cond, Expr::Literal(Constant::Null));
                *cond = negate_expr(old_cond);
                *then_body = std::mem::take(else_body);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Eliminate unreachable code after exits
// ---------------------------------------------------------------------------

/// Remove statements after unconditional exits (return, break, continue,
/// or if/else where both branches always exit).
///
/// Recurses into nested bodies first (children before parent), then scans
/// the current body and truncates after the first unconditionally-exiting
/// statement.
pub fn eliminate_unreachable_after_exit(body: &mut Vec<Stmt>) {
    // Recurse into children first.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                eliminate_unreachable_after_exit(then_body);
                eliminate_unreachable_after_exit(else_body);
            }
            Stmt::While { body, .. } | Stmt::Loop { body } | Stmt::ForOf { body, .. } => {
                eliminate_unreachable_after_exit(body);
            }
            Stmt::For {
                init,
                update,
                body,
                ..
            } => {
                eliminate_unreachable_after_exit(init);
                eliminate_unreachable_after_exit(update);
                eliminate_unreachable_after_exit(body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    eliminate_unreachable_after_exit(block_body);
                }
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    eliminate_unreachable_after_exit(case_body);
                }
                eliminate_unreachable_after_exit(default_body);
            }
            _ => {}
        }
    }

    // Scan for unconditional exits and truncate.
    for i in 0..body.len() {
        let exits = match &body[i] {
            Stmt::Return(_) | Stmt::Break | Stmt::Continue | Stmt::LabeledBreak { .. } => true,
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                !else_body.is_empty()
                    && body_always_exits(then_body)
                    && body_always_exits(else_body)
            }
            _ => false,
        };
        if exits && i + 1 < body.len() {
            body.truncate(i + 1);
            break;
        }
    }
}

// ---------------------------------------------------------------------------
// Simplify ternary to logical operators
// ---------------------------------------------------------------------------

/// Simplify `cond ? then_val : cond` → `cond && then_val`,
/// and `cond ? cond : else_val` → `cond || else_val`.
///
/// Recurses bottom-up into all sub-expressions and then into nested
/// statement bodies.
pub fn simplify_ternary_to_logical(body: &mut [Stmt]) {
    for stmt in body.iter_mut() {
        simplify_ternary_in_stmt(stmt);
    }
}

fn simplify_ternary_in_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::VarDecl { init, .. } => {
            if let Some(e) = init {
                simplify_ternary_in_expr(e);
            }
        }
        Stmt::Assign { target, value } => {
            simplify_ternary_in_expr(target);
            simplify_ternary_in_expr(value);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            simplify_ternary_in_expr(target);
            simplify_ternary_in_expr(value);
        }
        Stmt::Expr(e) => {
            simplify_ternary_in_expr(e);
        }
        Stmt::If {
            cond,
            then_body,
            else_body,
        } => {
            simplify_ternary_in_expr(cond);
            simplify_ternary_to_logical(then_body);
            simplify_ternary_to_logical(else_body);
        }
        Stmt::While { cond, body } => {
            simplify_ternary_in_expr(cond);
            simplify_ternary_to_logical(body);
        }
        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            simplify_ternary_to_logical(init);
            simplify_ternary_in_expr(cond);
            simplify_ternary_to_logical(update);
            simplify_ternary_to_logical(body);
        }
        Stmt::Loop { body } => {
            simplify_ternary_to_logical(body);
        }
        Stmt::ForOf { iterable, body, .. } => {
            simplify_ternary_in_expr(iterable);
            simplify_ternary_to_logical(body);
        }
        Stmt::Return(Some(e)) => {
            simplify_ternary_in_expr(e);
        }
        Stmt::Dispatch { blocks, .. } => {
            for (_, block_body) in blocks {
                simplify_ternary_to_logical(block_body);
            }
        }
        Stmt::Switch {
            value,
            cases,
            default_body,
        } => {
            simplify_ternary_in_expr(value);
            for (_, case_body) in cases {
                simplify_ternary_to_logical(case_body);
            }
            simplify_ternary_to_logical(default_body);
        }
        Stmt::Return(None) | Stmt::Break | Stmt::Continue | Stmt::LabeledBreak { .. } => {}
    }
}

fn simplify_ternary_in_expr(expr: &mut Expr) {
    // Recurse into sub-expressions first (bottom-up).
    match expr {
        Expr::Literal(_) | Expr::Var(_) | Expr::GlobalRef(_) => {}
        Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
            simplify_ternary_in_expr(lhs);
            simplify_ternary_in_expr(rhs);
        }
        Expr::LogicalOr { lhs, rhs } | Expr::LogicalAnd { lhs, rhs } => {
            simplify_ternary_in_expr(lhs);
            simplify_ternary_in_expr(rhs);
        }
        Expr::Unary { expr: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::TypeCheck { expr: inner, .. }
        | Expr::Not(inner)
        | Expr::CoroutineResume(inner)
        | Expr::PostIncrement(inner) => {
            simplify_ternary_in_expr(inner);
        }
        Expr::Field { object, .. } => {
            simplify_ternary_in_expr(object);
        }
        Expr::Index { collection, index } => {
            simplify_ternary_in_expr(collection);
            simplify_ternary_in_expr(index);
        }
        Expr::Call { args, .. } | Expr::CoroutineCreate { args, .. } => {
            for a in args {
                simplify_ternary_in_expr(a);
            }
        }
        Expr::CallIndirect { callee, args } => {
            simplify_ternary_in_expr(callee);
            for a in args {
                simplify_ternary_in_expr(a);
            }
        }
        Expr::SystemCall { args, .. } => {
            for a in args {
                simplify_ternary_in_expr(a);
            }
        }
        Expr::MethodCall {
            receiver, args, ..
        } => {
            simplify_ternary_in_expr(receiver);
            for a in args {
                simplify_ternary_in_expr(a);
            }
        }
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            simplify_ternary_in_expr(cond);
            simplify_ternary_in_expr(then_val);
            simplify_ternary_in_expr(else_val);
        }
        Expr::ArrayInit(elems) | Expr::TupleInit(elems) => {
            for e in elems {
                simplify_ternary_in_expr(e);
            }
        }
        Expr::StructInit { fields, .. } => {
            for (_, v) in fields {
                simplify_ternary_in_expr(v);
            }
        }
        Expr::Yield(v) => {
            if let Some(e) = v {
                simplify_ternary_in_expr(e);
            }
        }
    }

    // After recursion, check if this is a simplifiable ternary.
    if let Expr::Ternary { cond, then_val, else_val } = expr {
        if **cond == **else_val {
            // cond ? then_val : cond → cond && then_val
            let dummy = Expr::Literal(Constant::Null);
            let old = std::mem::replace(expr, dummy);
            if let Expr::Ternary { cond, then_val, .. } = old {
                *expr = Expr::LogicalAnd {
                    lhs: cond,
                    rhs: then_val,
                };
            }
        } else if **cond == **then_val {
            // cond ? cond : else_val → cond || else_val
            let dummy = Expr::Literal(Constant::Null);
            let old = std::mem::replace(expr, dummy);
            if let Expr::Ternary { cond, else_val, .. } = old {
                *expr = Expr::LogicalOr {
                    lhs: cond,
                    rhs: else_val,
                };
            }
        }
    }
}

// ---------------------------------------------------------------------------
// While → For loop promotion
// ---------------------------------------------------------------------------

/// Promote `let i = init; while (cond) { body; i += step; }` to
/// `for (let i = init; cond; i += step) { body }`.
///
/// Detects two increment patterns:
/// - **Tail increment**: last statement of the while body is `i += step`
/// - **Else-continue increment**: body is `if (x) { ... } else { i += step; continue; }`,
///   where the else branch is just the increment + continue
pub fn promote_while_to_for(body: &mut Vec<Stmt>) {
    // Recurse into nested structures first.
    for stmt in body.iter_mut() {
        match stmt {
            Stmt::If {
                then_body,
                else_body,
                ..
            } => {
                promote_while_to_for(then_body);
                promote_while_to_for(else_body);
            }
            Stmt::While { body: wb, .. }
            | Stmt::Loop { body: wb }
            | Stmt::For { body: wb, .. }
            | Stmt::ForOf { body: wb, .. } => {
                promote_while_to_for(wb);
            }
            Stmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_body) in cases {
                    promote_while_to_for(case_body);
                }
                promote_while_to_for(default_body);
            }
            Stmt::Dispatch { blocks, .. } => {
                for (_, block_body) in blocks {
                    promote_while_to_for(block_body);
                }
            }
            _ => {}
        }
    }

    // Scan for While loops and look backwards for a matching init statement.
    let mut i = 0;
    while i < body.len() {
        if !matches!(&body[i], Stmt::While { .. }) {
            i += 1;
            continue;
        }

        // Extract condition variable from the While's cond.
        let while_cond_var = if let Stmt::While { cond, .. } = &body[i] {
            extract_cmp_var(cond)
        } else {
            None
        };

        let Some(var_name) = while_cond_var else {
            i += 1;
            continue;
        };

        // Look backwards for a matching init (VarDecl or Assign to var_name).
        // Skip over statements that don't reference var_name.
        let mut init_idx = None;
        if i > 0 {
            for j in (0..i).rev() {
                let is_init = match &body[j] {
                    Stmt::VarDecl {
                        name,
                        init: Some(_),
                        mutable: true,
                        ..
                    } => name == &var_name,
                    Stmt::Assign {
                        target: Expr::Var(name),
                        ..
                    } => name == &var_name,
                    _ => false,
                };
                if is_init {
                    init_idx = Some(j);
                    break;
                }
                // Stop searching if an intervening statement references var_name.
                if stmt_references(&body[j], &var_name) {
                    break;
                }
            }
        }

        let Some(init_j) = init_idx else {
            i += 1;
            continue;
        };

        // Try to promote the pair.
        // Move the init statement to be adjacent to the while, then promote.
        let mut init_stmt = body.remove(init_j);
        // After removal, the while is now at i-1.
        let while_idx = i - 1;
        if let Some(promoted) = try_promote_while(&var_name, &mut init_stmt, &mut body[while_idx])
        {
            body[while_idx] = promoted;
            // Don't increment — recheck from the same position.
            i = while_idx;
            continue;
        } else {
            // Put the init back where it was.
            body.insert(init_j, init_stmt);
        }
        i += 1;
    }
}

/// Extract the variable name from a comparison expression like `i < N` or `i >= N`.
fn extract_cmp_var(expr: &Expr) -> Option<String> {
    if let Expr::Cmp { lhs, rhs, .. } = expr {
        if let Expr::Var(name) = lhs.as_ref() {
            return Some(name.clone());
        }
        if let Expr::Var(name) = rhs.as_ref() {
            return Some(name.clone());
        }
    }
    None
}

/// Check if a statement references the named variable (reads or writes).
fn stmt_references(stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::VarDecl {
            name: decl_name,
            init,
            ..
        } => {
            decl_name == name
                || init
                    .as_ref()
                    .is_some_and(|e| expr_references(e, name))
        }
        Stmt::Assign { target, value } => {
            expr_references(target, name) || expr_references(value, name)
        }
        Stmt::CompoundAssign { target, value, .. } => {
            expr_references(target, name) || expr_references(value, name)
        }
        Stmt::Expr(e) => expr_references(e, name),
        _ => true, // Conservatively assume anything else references the var.
    }
}

/// Check if `cond` references the named variable.
fn expr_references(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::Var(n) => n == name,
        Expr::Literal(_) | Expr::GlobalRef(_) => false,
        Expr::Binary { lhs, rhs, .. }
        | Expr::Cmp { lhs, rhs, .. }
        | Expr::LogicalOr { lhs, rhs }
        | Expr::LogicalAnd { lhs, rhs } => {
            expr_references(lhs, name) || expr_references(rhs, name)
        }
        Expr::Unary { expr: inner, .. }
        | Expr::Cast { expr: inner, .. }
        | Expr::TypeCheck { expr: inner, .. }
        | Expr::Not(inner)
        | Expr::CoroutineResume(inner)
        | Expr::PostIncrement(inner) => expr_references(inner, name),
        Expr::Field { object, .. } => expr_references(object, name),
        Expr::Index { collection, index } => {
            expr_references(collection, name) || expr_references(index, name)
        }
        Expr::Call { args, .. }
        | Expr::CoroutineCreate { args, .. }
        | Expr::SystemCall { args, .. } => args.iter().any(|a| expr_references(a, name)),
        Expr::CallIndirect { callee, args } => {
            expr_references(callee, name) || args.iter().any(|a| expr_references(a, name))
        }
        Expr::MethodCall {
            receiver, args, ..
        } => expr_references(receiver, name) || args.iter().any(|a| expr_references(a, name)),
        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            expr_references(cond, name)
                || expr_references(then_val, name)
                || expr_references(else_val, name)
        }
        Expr::ArrayInit(elems) | Expr::TupleInit(elems) => {
            elems.iter().any(|e| expr_references(e, name))
        }
        Expr::StructInit { fields, .. } => {
            fields.iter().any(|(_, v)| expr_references(v, name))
        }
        Expr::Yield(v) => v.as_ref().is_some_and(|e| expr_references(e, name)),
    }
}

/// Check if a statement is an increment/update of the named variable.
/// Returns true for `name += expr`, `name -= expr`, `name = name + expr`, etc.
/// Looks through `AsType` casts on the binary LHS (e.g. `name = (name as int) + 1`).
fn is_var_update(stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::CompoundAssign { target, .. } => matches!(target, Expr::Var(n) if n == name),
        Stmt::Assign { target, value } => {
            matches!(target, Expr::Var(n) if n == name)
                && matches!(
                    value,
                    Expr::Binary { lhs, .. } if matches!(strip_as_type(lhs.as_ref()), Expr::Var(n) if n == name)
                )
        }
        _ => false,
    }
}

/// Try to promote a `VarDecl/Assign; While` pair into a `For` loop.
fn try_promote_while(var_name: &str, init_stmt: &mut Stmt, while_stmt: &mut Stmt) -> Option<Stmt> {
    let Stmt::While { cond, body } = while_stmt else {
        return None;
    };

    // Condition must reference the loop variable.
    if !expr_references(cond, var_name) {
        return None;
    }

    // Extract the init statement (works for both VarDecl and Assign).
    let extract_init = |init_stmt: &mut Stmt| -> Stmt {
        std::mem::replace(init_stmt, Stmt::Expr(Expr::Literal(Constant::Null)))
    };

    // Pattern 1: last statement of body is `var_name += step` (tail increment).
    if body.len() >= 2 && is_var_update(body.last().unwrap(), var_name) {
        let update_stmt = body.pop().unwrap();
        let init = extract_init(init_stmt);
        let cond = std::mem::replace(cond, Expr::Literal(Constant::Null));
        let body = std::mem::take(body);
        return Some(Stmt::For {
            init: vec![init],
            cond,
            update: vec![update_stmt],
            body,
        });
    }

    // Pattern 2: body is a single `if (x) { ... } else { ...; var_name += step; continue; }`
    if body.len() == 1 {
        if let Stmt::If { else_body, .. } = &body[0] {
            // Else branch must end with `[..., increment, continue]`
            if else_body.len() >= 2
                && matches!(else_body.last(), Some(Stmt::Continue))
                && is_var_update(&else_body[else_body.len() - 2], var_name)
            {
                // Extract the if statement mutably.
                let Stmt::If {
                    cond: if_cond,
                    then_body: if_then,
                    else_body: if_else,
                } = std::mem::replace(
                    &mut body[0],
                    Stmt::Expr(Expr::Literal(Constant::Null)),
                )
                else {
                    unreachable!()
                };

                let mut if_else = if_else;
                if_else.pop(); // Remove Continue.
                let update_stmt = if_else.pop().unwrap(); // Remove increment.

                // If else body still has stmts, keep the if/else; otherwise remove else.
                let new_body = if if_else.is_empty() {
                    vec![Stmt::If {
                        cond: if_cond,
                        then_body: if_then,
                        else_body: vec![],
                    }]
                } else {
                    vec![Stmt::If {
                        cond: if_cond,
                        then_body: if_then,
                        else_body: if_else,
                    }]
                };

                let init = extract_init(init_stmt);
                let loop_cond = std::mem::replace(cond, Expr::Literal(Constant::Null));
                return Some(Stmt::For {
                    init: vec![init],
                    cond: loop_cond,
                    update: vec![update_stmt],
                    body: new_body,
                });
            }
        }
    }

    None
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

    // -----------------------------------------------------------------------
    // Single-use const folding tests
    // -----------------------------------------------------------------------

    fn const_decl(name: &str, init: Expr) -> Stmt {
        Stmt::VarDecl {
            name: name.to_string(),
            ty: None,
            init: Some(init),
            mutable: false,
        }
    }

    #[test]
    fn fold_adjacent_pure() {
        // const v17 = a && b; x = v17 ? 1 : 2;
        // → x = (a && b) ? 1 : 2;
        let logical_and = Expr::LogicalAnd {
            lhs: Box::new(var("a")),
            rhs: Box::new(var("b")),
        };
        let mut body = vec![
            const_decl("v17", logical_and.clone()),
            assign(
                var("x"),
                Expr::Ternary {
                    cond: Box::new(var("v17")),
                    then_val: Box::new(int(1)),
                    else_val: Box::new(int(2)),
                },
            ),
        ];

        fold_single_use_consts(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::Assign { value, .. } => match value {
                Expr::Ternary { cond, .. } => {
                    assert!(matches!(cond.as_ref(), Expr::LogicalAnd { .. }));
                }
                other => panic!("Expected Ternary, got: {other:?}"),
            },
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn fold_adjacent_impure() {
        // const v = call(); if (v) { ... }
        // → if (call()) { ... }
        let call = Expr::Call {
            func: "f".to_string(),
            args: vec![],
        };
        let mut body = vec![
            const_decl("v", call),
            Stmt::If {
                cond: var("v"),
                then_body: vec![assign(var("x"), int(1))],
                else_body: vec![],
            },
        ];

        fold_single_use_consts(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::If { cond, .. } => {
                assert!(matches!(cond, Expr::Call { .. }));
            }
            other => panic!("Expected If, got: {other:?}"),
        }
    }

    #[test]
    fn fold_no_fold_multi_use() {
        // const v = a + b; x = v; y = v;
        // → no fold (v used twice)
        let mut body = vec![
            const_decl(
                "v",
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                },
            ),
            assign(var("x"), var("v")),
            assign(var("y"), var("v")),
        ];

        fold_single_use_consts(&mut body);

        assert_eq!(body.len(), 3);
        assert!(matches!(&body[0], Stmt::VarDecl { .. }));
    }

    #[test]
    fn fold_cascading() {
        // const a = 1; const b = a + 2; x = b;
        // → const a = 1; x = a + 2;  (fold b)
        // → x = 1 + 2;               (fold a)
        let mut body = vec![
            const_decl("a", int(1)),
            const_decl(
                "b",
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(int(2)),
                },
            ),
            assign(var("x"), var("b")),
        ];

        fold_single_use_consts(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::Assign { value, .. } => {
                assert!(matches!(value, Expr::Binary { .. }));
            }
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    #[test]
    fn fold_non_adjacent_pure_across_pure_decls() {
        // const a = x + 1; const b = y + 2; z = a + b;
        // b is adjacent to use → fold first. Then a becomes adjacent → fold.
        let mut body = vec![
            const_decl(
                "a",
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("x")),
                    rhs: Box::new(int(1)),
                },
            ),
            const_decl(
                "b",
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("y")),
                    rhs: Box::new(int(2)),
                },
            ),
            assign(
                var("z"),
                Expr::Binary {
                    op: BinOp::Add,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                },
            ),
        ];

        fold_single_use_consts(&mut body);

        // Both a and b folded into the assignment.
        assert_eq!(body.len(), 1);
    }

    #[test]
    fn fold_single_use_mutable() {
        // let v = 1; x = v; — single-use mutable, fold it
        let mut body = vec![
            Stmt::VarDecl {
                name: "v".to_string(),
                ty: None,
                init: Some(int(1)),
                mutable: true,
            },
            assign(var("x"), var("v")),
        ];

        fold_single_use_consts(&mut body);

        assert_eq!(body.len(), 1);
        assert!(matches!(&body[0], Stmt::Assign { value, .. } if *value == int(1)));
    }

    #[test]
    fn fold_hp_pattern() {
        // const v35 = HP; HP = v35 - v11;
        // → HP = HP - v11;
        let mut body = vec![
            const_decl("v35", var("HP")),
            assign(
                var("HP"),
                Expr::Binary {
                    op: BinOp::Sub,
                    lhs: Box::new(var("v35")),
                    rhs: Box::new(var("v11")),
                },
            ),
        ];

        fold_single_use_consts(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::Assign { target, value } => {
                assert_eq!(*target, var("HP"));
                // value should be HP - v11 (v35 replaced with HP)
                match value {
                    Expr::Binary { lhs, rhs, .. } => {
                        assert_eq!(**lhs, var("HP"));
                        assert_eq!(**rhs, var("v11"));
                    }
                    other => panic!("Expected Binary, got: {other:?}"),
                }
            }
            other => panic!("Expected Assign, got: {other:?}"),
        }
    }

    // -----------------------------------------------------------------------
    // Regression tests for bug fixes
    // -----------------------------------------------------------------------

    // Regression: d73d9c7 — fold_single_use_consts must use read-only ref count
    // when finding fold targets. A bare `x = 5` is a write to x, not a read,
    // so the fold must skip it and find the real read site.
    // Updated: the fold is now blocked entirely because x is reassigned,
    // which means folding the init value past the reassignment would be
    // semantically incorrect (the read sees the reassigned value, not the init).
    #[test]
    fn fold_const_skips_bare_write() {
        // const x = a; x = 5; y = x;
        // x is reassigned (`x = 5`) so folding `a` past the reassignment is wrong.
        // The fold must be blocked — all three statements are preserved.
        let mut body = vec![
            const_decl("x", var("a")),
            assign(var("x"), int(5)),
            assign(var("y"), var("x")),
        ];

        fold_single_use_consts(&mut body);

        // x is reassigned, so the fold is blocked. The decl is preserved.
        assert_eq!(body.len(), 3, "All three statements should be preserved: {body:?}");
        assert!(
            matches!(&body[0], Stmt::VarDecl { name, init: Some(_), .. } if name == "x"),
            "VarDecl for x should be preserved: {body:?}"
        );
    }

    // Regression: 2626123 — dead decl removal must also remove orphaned assigns
    // to the same variable (bare `x = ...` with no reads).
    #[test]
    fn dead_decl_removes_orphaned_assigns() {
        // let x; x = 0; (no reads of x)
        // Both the uninit decl and the assign should be removed.
        let mut body = vec![uninit_decl("x"), assign(var("x"), int(0))];

        fold_single_use_consts(&mut body);

        assert!(
            body.is_empty(),
            "Expected empty body after dead decl + assign removal, got: {body:?}"
        );
    }

    // Regression: 88a9b23 — forward_substitute must not remove assigns to
    // variables declared in an outer scope.
    #[test]
    fn forward_sub_preserves_outer_scope_assign() {
        // x = expr; y = x;  (x has no VarDecl in this scope — it's from outer)
        // forward_substitute should inline x into y but NOT remove the assign
        // because x is visible to outer scopes.
        let call = Expr::Call {
            func: "f".to_string(),
            args: vec![],
        };
        let mut body = vec![assign(var("x"), call.clone()), assign(var("y"), var("x"))];

        forward_substitute(&mut body);

        // x = f() must be preserved (outer scope needs it).
        assert!(
            body.iter()
                .any(|s| matches!(s, Stmt::Assign { target, .. } if *target == var("x"))),
            "Expected outer-scope assign to x to be preserved: {body:?}"
        );
    }

    // Regression: f76cbae — forward_substitute must not replace the target of
    // an assignment (write position), only reads.
    #[test]
    fn forward_sub_no_replace_assign_target() {
        // const v = f(); v = 5;
        // The `v` in `v = 5` is a write target. forward_substitute must not
        // replace it with `f()` (which would produce `f() = 5` — nonsensical).
        let call = Expr::Call {
            func: "f".to_string(),
            args: vec![],
        };
        let mut body = vec![
            const_decl("v", call),
            assign(var("v"), int(5)),
        ];

        forward_substitute(&mut body);

        // The assign target must still be var("v"), not the call expression.
        let has_var_target = body.iter().any(|s| match s {
            Stmt::Assign { target, value } => *target == var("v") && *value == int(5),
            _ => false,
        });
        assert!(
            has_var_target,
            "Expected `v = 5` with Var target to be preserved: {body:?}"
        );
    }

    // Regression: 2a0a15a — eliminate_duplicate_assigns removes consecutive
    // identical assignments (structurizer artifacts from duplicate edges).
    #[test]
    fn eliminate_duplicate_assigns_basic() {
        let mut body = vec![
            assign(var("x"), var("a")),
            assign(var("x"), var("a")),
        ];

        eliminate_duplicate_assigns(&mut body);

        assert_eq!(body.len(), 1, "Expected single assign after dedup: {body:?}");
        assert!(matches!(&body[0], Stmt::Assign { target, value }
            if *target == var("x") && *value == var("a")));
    }

    // Regression: 6aa3e9f — eliminate_forwarding_stubs removes uninit decl +
    // forwarding assign patterns from empty else-branches.
    #[test]
    fn eliminate_forwarding_stub() {
        // let v1: i64; v2 = v1;  (v1 is uninit and has no other refs)
        // Both the decl and the forwarding assign should be removed.
        let mut body = vec![uninit_decl("v1"), assign(var("v2"), var("v1"))];

        eliminate_forwarding_stubs(&mut body);

        assert!(
            body.is_empty(),
            "Expected empty body after forwarding stub elimination: {body:?}"
        );
    }

    // Regression: b9a1d9e + 6d96d2f — dead VarDecl with pure init and 0 reads
    // is removed entirely.
    #[test]
    fn dead_decl_pure_removed() {
        // const v = 42; (no reads)
        let mut body = vec![const_decl("v", int(42))];

        fold_single_use_consts(&mut body);

        assert!(body.is_empty(), "Expected pure dead decl to be removed: {body:?}");
    }

    // Regression: b9a1d9e — dead VarDecl with side-effectful init and 0 reads
    // is converted to a bare expression statement (preserving the side effect).
    #[test]
    fn dead_decl_impure_kept_as_expr() {
        // const v = f(); (no reads — side effect must be preserved)
        let call = Expr::Call {
            func: "f".to_string(),
            args: vec![],
        };
        let mut body = vec![const_decl("v", call.clone())];

        fold_single_use_consts(&mut body);

        assert_eq!(body.len(), 1, "Expected one statement: {body:?}");
        match &body[0] {
            Stmt::Expr(expr) => {
                assert!(
                    matches!(expr, Expr::Call { func, .. } if func == "f"),
                    "Expected bare Call expression, got: {expr:?}"
                );
            }
            other => panic!("Expected Stmt::Expr, got: {other:?}"),
        }
    }

    // Regression: 46e4a73 — after self-assign elimination empties the then-branch,
    // the if-statement should flip: `if (c) {} else { B }` → `if (!c) { B }`.
    #[test]
    fn flip_empty_then_after_self_assign() {
        // if (c) { x = x; } else { x = 1; }
        // After eliminate_self_assigns: if (c) {} else { x = 1; }
        // Then flip: if (!c) { x = 1; }
        let mut body = vec![Stmt::If {
            cond: var("c"),
            then_body: vec![assign(var("x"), var("x"))],
            else_body: vec![assign(var("x"), int(1))],
        }];

        eliminate_self_assigns(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                // Condition should be negated.
                assert!(
                    matches!(cond, Expr::Not(_)),
                    "Expected negated condition, got: {cond:?}"
                );
                // The real work should be in then_body now.
                assert_eq!(then_body.len(), 1);
                assert!(matches!(&then_body[0], Stmt::Assign { target, value }
                    if *target == var("x") && *value == int(1)));
                // else_body should be empty.
                assert!(else_body.is_empty());
            }
            other => panic!("Expected If, got: {other:?}"),
        }
    }

    // Regression: 46e4a73 — when self-assign elimination empties both branches,
    // the entire if-statement is removed.
    #[test]
    fn remove_fully_empty_if() {
        // if (c) { x = x; } else { y = y; }
        // After eliminate_self_assigns: both branches empty → remove If entirely.
        let mut body = vec![Stmt::If {
            cond: var("c"),
            then_body: vec![assign(var("x"), var("x"))],
            else_body: vec![assign(var("y"), var("y"))],
        }];

        eliminate_self_assigns(&mut body);

        assert!(
            body.is_empty(),
            "Expected empty body after fully-empty if removal: {body:?}"
        );
    }

    // Regression: 868decb — self-assigns must be preserved during ternary rewrite
    // so the pass can see the identity branch. The ternary pass intentionally
    // skips identity-branch patterns (`x = cond ? val : x`), leaving them as
    // if/else for later cleanup by eliminate_self_assigns. The critical property:
    // the combined pipeline (ternary first, then self-assign elimination) produces
    // a clean `if (c) { x = 1; }` instead of losing the assignment.
    #[test]
    fn ternary_with_passthrough_branch() {
        // if (c) { x = 1; } else { x = x; }
        // Step 1: rewrite_ternary skips it (identity branch in else).
        // Step 2: eliminate_self_assigns removes `x = x` → flips to `if (!c) {} else { x = 1; }`
        //         then flips empty then → `if (c) { x = 1; }`.
        // Wait — eliminate_self_assigns removes x=x from else, leaving
        // `if (c) { x = 1; } else {}` which stays as-is (then is non-empty).
        let mut body = vec![Stmt::If {
            cond: var("c"),
            then_body: vec![assign(var("x"), int(1))],
            else_body: vec![assign(var("x"), var("x"))],
        }];

        // Ternary pass intentionally does NOT rewrite identity branches.
        rewrite_ternary(&mut body);
        assert!(matches!(&body[0], Stmt::If { .. }), "Should remain If");

        // Self-assign elimination cleans up the identity branch.
        eliminate_self_assigns(&mut body);

        assert_eq!(body.len(), 1);
        match &body[0] {
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                // then_body has the real assign, else_body is empty.
                assert_eq!(*cond, var("c"));
                assert_eq!(then_body.len(), 1);
                assert!(matches!(&then_body[0], Stmt::Assign { target, value }
                    if *target == var("x") && *value == int(1)));
                assert!(else_body.is_empty());
            }
            other => panic!("Expected If, got: {other:?}"),
        }
    }

    // Regression: forward_substitute consumed the init assignment of a
    // loop-carried variable (e.g., `count = 3` before a `while(true)` loop),
    // inlining the literal `3` into the loop body. Since the counter is
    // reassigned each iteration (`count = count - 1`), the substituted
    // literal never decrements and the loop runs forever.
    #[test]
    fn forward_sub_no_consume_loop_carried_var() {
        // let count; count = 3; while (true) { ... count = count - 1; if (!count) break; }
        let mut body = vec![
            Stmt::VarDecl {
                name: "count".to_string(),
                ty: None,
                init: None,
                mutable: true,
            },
            assign(var("count"), int(3)),
            Stmt::Loop {
                body: vec![
                    Stmt::Expr(Expr::Call {
                        func: "doWork".to_string(),
                        args: vec![],
                    }),
                    assign(
                        var("count"),
                        Expr::Binary {
                            op: BinOp::Sub,
                            lhs: Box::new(var("count")),
                            rhs: Box::new(int(1)),
                        },
                    ),
                    Stmt::If {
                        cond: Expr::Call {
                            func: "isDone".to_string(),
                            args: vec![var("count")],
                        },
                        then_body: vec![Stmt::Break],
                        else_body: vec![],
                    },
                ],
            },
        ];

        forward_substitute(&mut body);

        // The `count = 3` assignment must NOT be consumed — the loop body
        // reassigns count, so inlining `3` would make the loop infinite.
        let has_count_assign = body.iter().any(|s| {
            matches!(s, Stmt::Assign { target, value }
                if *target == var("count") && *value == int(3))
        });
        assert!(
            has_count_assign,
            "Init assign `count = 3` was incorrectly consumed by forward_substitute: {body:?}"
        );
    }

    // Regression: forward_substitute used stmt_references_var (which counts
    // writes) for adjacency check, but count_var_refs_in_stmt (reads only)
    // for the total count. When the adjacent stmt only *wrote* the var in a
    // nested assign target, the substitute found no read to replace, silently
    // dropping the value expression.
    //
    // Pattern:
    //   let i;
    //   i = cond ? 1 : 0;       ← assign to i
    //   if (x) { i = 0; }       ← nested write to i (not a read)
    //   return i;                ← the single read
    //
    // Bug: forward_substitute saw total_refs=1, adjacent stmt "references" i
    // (via the write), removed the assign, but substitute_var_in_stmt found
    // no read in the if stmt to replace → ternary expression silently dropped.
    #[test]
    fn forward_sub_no_consume_nested_write_only() {
        let mut body = vec![
            Stmt::VarDecl {
                name: "i".to_string(),
                ty: None,
                init: None,
                mutable: true,
            },
            assign(
                var("i"),
                Expr::Ternary {
                    cond: Box::new(var("cond")),
                    then_val: Box::new(int(1)),
                    else_val: Box::new(int(0)),
                },
            ),
            Stmt::If {
                cond: var("x"),
                then_body: vec![assign(var("i"), int(0))],
                else_body: vec![],
            },
            Stmt::Return(Some(var("i"))),
        ];

        forward_substitute(&mut body);

        // The ternary assign must NOT be consumed — it should remain because
        // the adjacent if only writes i, not reads it.
        let has_ternary_assign = body.iter().any(|s| {
            matches!(s, Stmt::Assign { target, value }
                if *target == var("i") && matches!(value, Expr::Ternary { .. }))
        });
        assert!(
            has_ternary_assign,
            "Ternary assign `i = cond ? 1 : 0` was incorrectly consumed: {body:?}"
        );
    }
}
