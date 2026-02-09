//! AST-to-AST rewrite passes.
//!
//! These run after Shape→AST lowering to detect and simplify patterns that
//! are easier to match on the high-level AST than during lowering.

use super::ast::{Expr, Stmt};
use super::inst::CmpKind;
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
            Stmt::While { body, .. } | Stmt::Loop { body } => {
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
            _ => {}
        }
    }
}

/// Try to fold a single const. Returns `true` if a fold was performed.
fn try_fold_one_const(body: &mut Vec<Stmt>) -> bool {
    for i in 0..body.len() {
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
            return true;
        }

        if total_refs != 1 {
            continue;
        }

        // Find the statement containing the single use.
        let use_idx = (i + 1..body.len())
            .find(|&j| stmt_references_var(&body[j], &name))
            .unwrap();

        let adjacent = use_idx == i + 1;

        if !adjacent {
            // Non-adjacent: only fold if all intervening statements are SE-free.
            let all_intervening_pure = body[i + 1..use_idx]
                .iter()
                .all(|s| !stmt_has_side_effects(s));
            if !all_intervening_pure {
                continue;
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
        | Expr::CoroutineCreate { .. }
        | Expr::CoroutineResume(_)
        | Expr::Yield(_) => true,
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
        | Expr::CoroutineResume(inner) => count_var_refs_in_expr(inner, name),
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

/// Count occurrences of `Var(name)` in a statement (recursing into nested bodies).
fn count_var_refs_in_stmt(stmt: &Stmt, name: &str) -> usize {
    match stmt {
        Stmt::VarDecl { name: n, init, .. } => {
            usize::from(n == name) + init.as_ref().map_or(0, |e| count_var_refs_in_expr(e, name))
        }
        Stmt::Assign { target, value } => {
            count_var_refs_in_expr(target, name) + count_var_refs_in_expr(value, name)
        }
        Stmt::CompoundAssign { target, value, .. } => {
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
        Stmt::Return(e) => e.as_ref().map_or(0, |e| count_var_refs_in_expr(e, name)),
        Stmt::Dispatch { blocks, .. } => blocks
            .iter()
            .flat_map(|(_, stmts)| stmts.iter())
            .map(|s| count_var_refs_in_stmt(s, name))
            .sum(),
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
        | Expr::CoroutineResume(inner) => substitute_var_in_expr(inner, name, replacement),
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
            substitute_var_in_expr(target, name, replacement)
                || substitute_var_in_expr(value, name, replacement)
        }
        Stmt::CompoundAssign { target, value, .. } => {
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
        Stmt::Break | Stmt::Continue | Stmt::LabeledBreak { .. } => false,
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
            Stmt::While { body, .. } | Stmt::Loop { body } => {
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
            Stmt::While { body, .. } | Stmt::Loop { body } => {
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
        // Not a compound statement — can't narrow into it.
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Self-assignment elimination
// ---------------------------------------------------------------------------

/// Remove no-op self-assignments (`x = x;`) produced by out-of-SSA coalescing.
///
/// When multiple SSA values share a name, pass-through branches emit `x = x`
/// which is a no-op. This must run AFTER ternary detection so that pass-through
/// branches are available for ternary pattern matching.
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
            Stmt::While { body, .. } | Stmt::Loop { body } => {
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
            Stmt::While { body, .. } | Stmt::Loop { body } => 1 + count_stmts(body),
            Stmt::For {
                init,
                update,
                body,
                ..
            } => 1 + count_stmts(init) + count_stmts(update) + count_stmts(body),
            Stmt::Dispatch { blocks, .. } => {
                1 + blocks.iter().map(|(_, b)| count_stmts(b)).sum::<usize>()
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
}
