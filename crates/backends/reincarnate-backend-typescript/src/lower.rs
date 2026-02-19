//! Core AST → JS AST mechanical lowering pass.
//!
//! Converts the engine-agnostic `Stmt`/`Expr` tree into `JsStmt`/`JsExpr`
//! with a 1:1 structural mapping. Engine-specific rewrites (scope-lookup
//! resolution, SystemCall → native JS constructs, etc.) are handled by a
//! separate post-lowering rewrite pass (e.g. `rewrites::flash`).

use reincarnate_core::ir::ast::{AstFunction, Expr, Stmt};

use crate::js_ast::{JsExpr, JsFunction, JsStmt};

// ---------------------------------------------------------------------------
// Lowering context
// ---------------------------------------------------------------------------

/// Context for the lowering pass.
pub struct LowerCtx {
    /// Self parameter name — the IR parameter that maps to `this`.
    pub self_param_name: Option<String>,
}

// ---------------------------------------------------------------------------
// Function lowering
// ---------------------------------------------------------------------------

/// Lower an entire function from core AST to JS AST.
pub fn lower_function(ast: &AstFunction, ctx: &LowerCtx) -> JsFunction {
    let body = lower_stmts(&ast.body, ctx);

    JsFunction {
        name: ast.name.clone(),
        params: ast.params.clone(),
        return_ty: ast.return_ty.clone(),
        body,
        is_generator: ast.is_generator,
        visibility: ast.visibility,
        method_kind: ast.method_kind,
        has_rest_param: ast.has_rest_param,
    }
}

// ---------------------------------------------------------------------------
// Statement lowering
// ---------------------------------------------------------------------------

fn lower_stmts(stmts: &[Stmt], ctx: &LowerCtx) -> Vec<JsStmt> {
    stmts.iter().map(|s| lower_stmt(s, ctx)).collect()
}

/// Lower a single statement.
fn lower_stmt(stmt: &Stmt, ctx: &LowerCtx) -> JsStmt {
    match stmt {
        Stmt::VarDecl {
            name,
            ty,
            init,
            mutable,
        } => JsStmt::VarDecl {
            name: name.clone(),
            ty: ty.clone(),
            init: init.as_ref().map(|e| lower_expr(e, ctx)),
            mutable: *mutable,
        },

        Stmt::Assign { target, value } => JsStmt::Assign {
            target: lower_expr(target, ctx),
            value: lower_expr(value, ctx),
        },

        Stmt::CompoundAssign { target, op, value } => JsStmt::CompoundAssign {
            target: lower_expr(target, ctx),
            op: *op,
            value: lower_expr(value, ctx),
        },

        Stmt::Expr(expr) => JsStmt::Expr(lower_expr(expr, ctx)),

        Stmt::If {
            cond,
            then_body,
            else_body,
        } => JsStmt::If {
            cond: lower_expr(cond, ctx),
            then_body: lower_stmts(then_body, ctx),
            else_body: lower_stmts(else_body, ctx),
        },

        Stmt::While { cond, body } => JsStmt::While {
            cond: lower_expr(cond, ctx),
            body: lower_stmts(body, ctx),
        },

        Stmt::For {
            init,
            cond,
            update,
            body,
        } => JsStmt::For {
            init: lower_stmts(init, ctx),
            cond: lower_expr(cond, ctx),
            update: lower_stmts(update, ctx),
            body: lower_stmts(body, ctx),
        },

        Stmt::Loop { body } => JsStmt::Loop {
            body: lower_stmts(body, ctx),
        },

        Stmt::ForOf {
            binding,
            declare,
            iterable,
            body,
        } => JsStmt::ForOf {
            binding: binding.clone(),
            declare: *declare,
            iterable: lower_expr(iterable, ctx),
            body: lower_stmts(body, ctx),
        },

        Stmt::Return(expr) => JsStmt::Return(expr.as_ref().map(|e| lower_expr(e, ctx))),
        Stmt::Break => JsStmt::Break,
        Stmt::Continue => JsStmt::Continue,
        Stmt::LabeledBreak { depth } => JsStmt::LabeledBreak { depth: *depth },

        Stmt::Dispatch { blocks, entry } => JsStmt::Dispatch {
            blocks: blocks
                .iter()
                .map(|(idx, stmts)| (*idx, lower_stmts(stmts, ctx)))
                .collect(),
            entry: *entry,
        },

        Stmt::Switch {
            value,
            cases,
            default_body,
        } => JsStmt::Switch {
            value: lower_expr(value, ctx),
            cases: cases
                .iter()
                .map(|(c, stmts)| (c.clone(), lower_stmts(stmts, ctx)))
                .collect(),
            default_body: lower_stmts(default_body, ctx),
        },
    }
}

// ---------------------------------------------------------------------------
// Expression lowering
// ---------------------------------------------------------------------------

/// Lower a single expression from core AST to JS AST.
fn lower_expr(expr: &Expr, ctx: &LowerCtx) -> JsExpr {
    match expr {
        Expr::Literal(c) => JsExpr::Literal(c.clone()),

        Expr::Var(name) => {
            if let Some(ref self_name) = ctx.self_param_name {
                if name == self_name {
                    return JsExpr::This;
                }
            }
            JsExpr::Var(name.clone())
        }

        Expr::Binary { op, lhs, rhs } => JsExpr::Binary {
            op: *op,
            lhs: Box::new(lower_expr(lhs, ctx)),
            rhs: Box::new(lower_expr(rhs, ctx)),
        },

        Expr::Unary { op, expr: inner } => JsExpr::Unary {
            op: *op,
            expr: Box::new(lower_expr(inner, ctx)),
        },

        Expr::Cmp { kind, lhs, rhs } => JsExpr::Cmp {
            kind: *kind,
            lhs: Box::new(lower_expr(lhs, ctx)),
            rhs: Box::new(lower_expr(rhs, ctx)),
        },

        Expr::Field { object, field } => lower_field(object, field, ctx),

        Expr::Index { collection, index } => JsExpr::Index {
            collection: Box::new(lower_expr(collection, ctx)),
            index: Box::new(lower_expr(index, ctx)),
        },

        Expr::Call { func: fname, args } => lower_call(fname, args, ctx),

        Expr::CallIndirect { callee, args } => JsExpr::Call {
            callee: Box::new(lower_expr(callee, ctx)),
            args: lower_exprs(args, ctx),
        },

        Expr::SystemCall {
            system,
            method,
            args,
        } => JsExpr::SystemCall {
            system: system.clone(),
            method: method.clone(),
            args: lower_exprs(args, ctx),
        },

        Expr::MethodCall {
            receiver,
            method,
            args,
        } => JsExpr::Call {
            callee: Box::new(JsExpr::Field {
                object: Box::new(lower_expr(receiver, ctx)),
                field: method.clone(),
            }),
            args: lower_exprs(args, ctx),
        },

        Expr::Ternary {
            cond,
            then_val,
            else_val,
        } => JsExpr::Ternary {
            cond: Box::new(lower_expr(cond, ctx)),
            then_val: Box::new(lower_expr(then_val, ctx)),
            else_val: Box::new(lower_expr(else_val, ctx)),
        },

        Expr::LogicalOr { lhs, rhs } => JsExpr::LogicalOr {
            lhs: Box::new(lower_expr(lhs, ctx)),
            rhs: Box::new(lower_expr(rhs, ctx)),
        },

        Expr::LogicalAnd { lhs, rhs } => JsExpr::LogicalAnd {
            lhs: Box::new(lower_expr(lhs, ctx)),
            rhs: Box::new(lower_expr(rhs, ctx)),
        },

        Expr::Cast { expr: inner, ty, kind } => JsExpr::Cast {
            expr: Box::new(lower_expr(inner, ctx)),
            ty: ty.clone(),
            kind: *kind,
        },

        Expr::TypeCheck { expr: inner, ty } => JsExpr::TypeCheck {
            expr: Box::new(lower_expr(inner, ctx)),
            ty: ty.clone(),
        },

        Expr::ArrayInit(elems) => JsExpr::ArrayInit(lower_exprs(elems, ctx)),

        Expr::StructInit { name: _, fields } => {
            let mut seen = std::collections::HashMap::<String, usize>::new();
            let mut pairs: Vec<(String, JsExpr)> = Vec::with_capacity(fields.len());
            for (name, val) in fields {
                let lowered = lower_expr(val, ctx);
                if name == "..." {
                    // Spread entries are never deduplicated
                    pairs.push((name.clone(), lowered));
                } else if let Some(&idx) = seen.get(name) {
                    eprintln!("warning: duplicate key '{name}' in object literal (last value wins)");
                    pairs[idx].1 = lowered;
                } else {
                    seen.insert(name.clone(), pairs.len());
                    pairs.push((name.clone(), lowered));
                }
            }
            JsExpr::ObjectInit(pairs)
        }

        Expr::TupleInit(elems) => JsExpr::TupleInit(lower_exprs(elems, ctx)),

        Expr::GlobalRef(name) => JsExpr::Var(name.clone()),

        Expr::CoroutineCreate { func: fname, args } => JsExpr::GeneratorCreate {
            func: fname.clone(),
            args: lower_exprs(args, ctx),
        },

        Expr::CoroutineResume(inner) => JsExpr::GeneratorResume(Box::new(lower_expr(inner, ctx))),

        Expr::Yield(v) => JsExpr::Yield(v.as_ref().map(|e| Box::new(lower_expr(e, ctx)))),

        Expr::Not(inner) => JsExpr::Not(Box::new(lower_expr(inner, ctx))),

        Expr::PostIncrement(inner) => JsExpr::PostIncrement(Box::new(lower_expr(inner, ctx))),

        Expr::Spread(inner) => JsExpr::Spread(Box::new(lower_expr(inner, ctx))),

        Expr::MakeClosure { .. } => {
            // Phase 1 stub: full capture wiring is implemented when frontends
            // start emitting Op::MakeClosure. This path is not reachable today.
            todo!("MakeClosure lowering: wire capture params to JsExpr::ArrowFunction")
        }
    }
}

/// Lower a slice of expressions.
fn lower_exprs(exprs: &[Expr], ctx: &LowerCtx) -> Vec<JsExpr> {
    exprs.iter().map(|e| lower_expr(e, ctx)).collect()
}

// ---------------------------------------------------------------------------
// Field access lowering
// ---------------------------------------------------------------------------

/// Lower a field access with `::` namespace stripping (IR convention).
fn lower_field(object: &Expr, field: &str, ctx: &LowerCtx) -> JsExpr {
    let effective = if field.contains("::") {
        field.rsplit("::").next().unwrap_or(field)
    } else {
        field
    };

    JsExpr::Field {
        object: Box::new(lower_expr(object, ctx)),
        field: effective.to_string(),
    }
}

// ---------------------------------------------------------------------------
// Call lowering
// ---------------------------------------------------------------------------

/// Lower a Call expression, handling dotted paths and free function calls.
fn lower_call(fname: &str, args: &[Expr], ctx: &LowerCtx) -> JsExpr {
    // Dotted name (e.g. Math.max) → global function call.
    if fname.contains('.') {
        return JsExpr::Call {
            callee: Box::new(build_dotted_path(fname)),
            args: lower_exprs(args, ctx),
        };
    }

    // Free function call.
    JsExpr::Call {
        callee: Box::new(JsExpr::Var(fname.to_string())),
        args: lower_exprs(args, ctx),
    }
}

/// Build a JsExpr for a dotted path like `"Math.max"` → `Field(Var("Math"), "max")`.
fn build_dotted_path(name: &str) -> JsExpr {
    let mut parts = name.split('.');
    let first = parts.next().unwrap();
    let mut expr = JsExpr::Var(first.to_string());
    for part in parts {
        expr = JsExpr::Field {
            object: Box::new(expr),
            field: part.to_string(),
        };
    }
    expr
}
