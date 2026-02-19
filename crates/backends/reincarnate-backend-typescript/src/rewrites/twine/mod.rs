//! Twine-specific JsExpr → JsExpr rewrite pass.
//!
//! Shared tree traversal dispatches to engine-specific sub-modules:
//! - `harlowe` — Harlowe.H method call rewrites (passthrough — lowered in core)
//! - `sugarcube` — SugarCube.Engine native JS construct rewrites
//!
//! All other SystemCalls pass through to runtime modules via the printer's
//! `SystemCall` fallback + auto-import machinery.

mod engine;
mod harlowe;
mod sugarcube;

use std::collections::HashMap;

use crate::js_ast::{JsExpr, JsFunction, JsStmt};

/// Returns the bare function names that a SystemCall rewrite will introduce,
/// if any. Used by import generation to emit the correct imports before
/// the rewrite pass runs.
pub fn rewrite_introduced_calls(system: &str, method: &str) -> &'static [&'static str] {
    if system == "Harlowe.H" {
        harlowe::rewrite_introduced_calls(method)
    } else if system == "Harlowe.Engine" {
        engine::rewrite_introduced_calls(method)
    } else {
        // SugarCube rewrites produce only built-in JS constructs (new, typeof, etc.)
        // and calls to Math.pow / String — none of which need function-module imports.
        &[]
    }
}

/// Rewrite a function's body, resolving Twine SystemCalls that map
/// to native JS constructs and inlining closure bodies as arrow functions.
pub fn rewrite_twine_function(
    mut func: JsFunction,
    closure_bodies: &HashMap<String, JsFunction>,
) -> JsFunction {
    rewrite_stmts(&mut func.body, closure_bodies);
    func
}

fn rewrite_stmts(stmts: &mut [JsStmt], closures: &HashMap<String, JsFunction>) {
    for stmt in stmts.iter_mut() {
        rewrite_stmt(stmt, closures);
    }
}

fn rewrite_stmt(stmt: &mut JsStmt, closures: &HashMap<String, JsFunction>) {
    match stmt {
        JsStmt::VarDecl { init, .. } => {
            if let Some(expr) = init {
                rewrite_expr(expr, closures);
            }
        }
        JsStmt::Assign { target, value } => {
            rewrite_expr(target, closures);
            rewrite_expr(value, closures);
        }
        JsStmt::CompoundAssign { target, value, .. } => {
            rewrite_expr(target, closures);
            rewrite_expr(value, closures);
        }
        JsStmt::Expr(e) => rewrite_expr(e, closures),
        JsStmt::Return(Some(e)) => rewrite_expr(e, closures),
        JsStmt::Return(None) => {}
        JsStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            rewrite_expr(cond, closures);
            rewrite_stmts(then_body, closures);
            rewrite_stmts(else_body, closures);
        }
        JsStmt::While { cond, body } => {
            rewrite_expr(cond, closures);
            rewrite_stmts(body, closures);
        }
        JsStmt::For {
            init,
            cond,
            update,
            body,
        } => {
            rewrite_stmts(init, closures);
            rewrite_expr(cond, closures);
            rewrite_stmts(update, closures);
            rewrite_stmts(body, closures);
        }
        JsStmt::Loop { body } => rewrite_stmts(body, closures),
        JsStmt::ForOf { iterable, body, .. } => {
            rewrite_expr(iterable, closures);
            rewrite_stmts(body, closures);
        }
        JsStmt::Throw(e) => rewrite_expr(e, closures),
        JsStmt::Dispatch { blocks, .. } => {
            for (_, stmts) in blocks {
                rewrite_stmts(stmts, closures);
            }
        }
        JsStmt::Switch {
            value,
            cases,
            default_body,
        } => {
            rewrite_expr(value, closures);
            for (_, stmts) in cases {
                rewrite_stmts(stmts, closures);
            }
            rewrite_stmts(default_body, closures);
        }
        JsStmt::Break | JsStmt::Continue | JsStmt::LabeledBreak { .. } => {}
    }
}

fn rewrite_expr(expr: &mut JsExpr, closures: &HashMap<String, JsFunction>) {
    // Recurse into children first.
    rewrite_expr_children(expr, closures);

    // Then attempt to resolve SystemCall patterns.
    let replacement = match expr {
        JsExpr::SystemCall {
            system,
            method,
            args,
        } => try_rewrite_system_call(system, method, args, closures),
        _ => None,
    };

    if let Some(new_expr) = replacement {
        *expr = new_expr;
    }
}

fn rewrite_expr_children(expr: &mut JsExpr, closures: &HashMap<String, JsFunction>) {
    match expr {
        JsExpr::Binary { lhs, rhs, .. } | JsExpr::Cmp { lhs, rhs, .. } => {
            rewrite_expr(lhs, closures);
            rewrite_expr(rhs, closures);
        }
        JsExpr::LogicalOr { lhs, rhs } | JsExpr::LogicalAnd { lhs, rhs } => {
            rewrite_expr(lhs, closures);
            rewrite_expr(rhs, closures);
        }
        JsExpr::Unary { expr: inner, .. } => rewrite_expr(inner, closures),
        JsExpr::Not(inner) | JsExpr::PostIncrement(inner) | JsExpr::Spread(inner) => {
            rewrite_expr(inner, closures)
        }
        JsExpr::Field { object, .. } => rewrite_expr(object, closures),
        JsExpr::Index { collection, index } => {
            rewrite_expr(collection, closures);
            rewrite_expr(index, closures);
        }
        JsExpr::Call { callee, args } => {
            rewrite_expr(callee, closures);
            for arg in args {
                rewrite_expr(arg, closures);
            }
        }
        JsExpr::Ternary {
            cond,
            then_val,
            else_val,
        } => {
            rewrite_expr(cond, closures);
            rewrite_expr(then_val, closures);
            rewrite_expr(else_val, closures);
        }
        JsExpr::ArrayInit(items) | JsExpr::TupleInit(items) => {
            for item in items {
                rewrite_expr(item, closures);
            }
        }
        JsExpr::ObjectInit(fields) => {
            for (_, val) in fields {
                rewrite_expr(val, closures);
            }
        }
        JsExpr::New { callee, args } => {
            rewrite_expr(callee, closures);
            for arg in args {
                rewrite_expr(arg, closures);
            }
        }
        JsExpr::TypeOf(inner) => rewrite_expr(inner, closures),
        JsExpr::In { key, object } => {
            rewrite_expr(key, closures);
            rewrite_expr(object, closures);
        }
        JsExpr::Delete { object, key } => {
            rewrite_expr(object, closures);
            rewrite_expr(key, closures);
        }
        JsExpr::Cast { expr: inner, .. } | JsExpr::TypeCheck { expr: inner, .. } => {
            rewrite_expr(inner, closures);
        }
        JsExpr::ArrowFunction { body, .. } => rewrite_stmts(body, closures),
        JsExpr::SuperCall(args) | JsExpr::SuperMethodCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg, closures);
            }
        }
        JsExpr::SuperGet(_) => {}
        JsExpr::SuperSet { value, .. } => rewrite_expr(value, closures),
        JsExpr::GeneratorCreate { args, .. } => {
            for arg in args {
                rewrite_expr(arg, closures);
            }
        }
        JsExpr::GeneratorResume(inner) => rewrite_expr(inner, closures),
        JsExpr::Yield(inner) => {
            if let Some(e) = inner {
                rewrite_expr(e, closures);
            }
        }
        JsExpr::Activation => {}
        JsExpr::SystemCall { args, .. } => {
            for arg in args {
                rewrite_expr(arg, closures);
            }
        }
        JsExpr::Literal(_) | JsExpr::Var(_) | JsExpr::This => {}
    }
}

/// Dispatch a SystemCall to the appropriate engine-specific rewriter.
fn try_rewrite_system_call(
    system: &str,
    method: &str,
    args: &mut Vec<JsExpr>,
    closures: &HashMap<String, JsFunction>,
) -> Option<JsExpr> {
    if system == "Harlowe.H" {
        return harlowe::try_rewrite(method, args);
    }
    if system == "Harlowe.Engine" {
        return engine::try_rewrite(method, args, closures);
    }
    if system == "SugarCube.Engine" {
        return sugarcube::try_rewrite(method, args, closures);
    }
    None
}
