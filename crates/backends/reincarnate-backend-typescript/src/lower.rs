//! Core AST → JS AST lowering pass.
//!
//! Converts the engine-agnostic `Stmt`/`Expr` tree into `JsStmt`/`JsExpr`
//! with resolved JS constructs. Engine-specific SystemCall rewrites are
//! delegated to the appropriate rewrite module (e.g. `rewrites::flash`).

use reincarnate_core::ir::ast::{AstFunction, Expr, Stmt};
use reincarnate_core::ir::MethodKind;

use crate::js_ast::{JsExpr, JsFunction, JsStmt};
use crate::rewrites::flash::{self, FlashLowerCtx, StmtRewrite};

// ---------------------------------------------------------------------------
// Lowering context
// ---------------------------------------------------------------------------

/// Context for the lowering pass.
pub struct LowerCtx {
    /// Self parameter name — the IR parameter that maps to `this`.
    pub self_param_name: Option<String>,
    /// Flash/AVM2 context (present when the engine is Flash).
    pub flash: Option<FlashLowerCtx>,
}

// ---------------------------------------------------------------------------
// Function lowering
// ---------------------------------------------------------------------------

/// Lower an entire function from core AST to JS AST.
pub fn lower_function(ast: &AstFunction, ctx: &LowerCtx) -> JsFunction {
    let mut body = lower_stmts(&ast.body, ctx);

    // In constructors, hoist super() call to the top of the body.
    if ast.method_kind == MethodKind::Constructor {
        hoist_super_call(&mut body);
    }

    JsFunction {
        name: ast.name.clone(),
        params: ast.params.clone(),
        return_ty: ast.return_ty.clone(),
        body,
        is_generator: ast.is_generator,
        visibility: ast.visibility,
        method_kind: ast.method_kind,
    }
}

/// Move the first `super()` call to position 0 in a constructor body.
fn hoist_super_call(body: &mut Vec<JsStmt>) {
    let pos = body
        .iter()
        .position(|s| matches!(s, JsStmt::Expr(JsExpr::SuperCall(_))));
    if let Some(i) = pos {
        if i > 0 {
            let stmt = body.remove(i);
            body.insert(0, stmt);
        }
    }
}

// ---------------------------------------------------------------------------
// Statement lowering
// ---------------------------------------------------------------------------

fn lower_stmts(stmts: &[Stmt], ctx: &LowerCtx) -> Vec<JsStmt> {
    stmts.iter().filter_map(|s| lower_stmt(s, ctx)).collect()
}

/// Lower a single statement. Returns `None` to skip (e.g. suppressed super).
fn lower_stmt(stmt: &Stmt, ctx: &LowerCtx) -> Option<JsStmt> {
    match stmt {
        Stmt::VarDecl {
            name,
            ty,
            init,
            mutable,
        } => Some(JsStmt::VarDecl {
            name: name.clone(),
            ty: ty.clone(),
            init: init.as_ref().map(|e| lower_expr(e, ctx)),
            mutable: *mutable,
        }),

        Stmt::Assign { target, value } => Some(JsStmt::Assign {
            target: lower_expr(target, ctx),
            value: lower_expr(value, ctx),
        }),

        Stmt::CompoundAssign { target, op, value } => Some(JsStmt::CompoundAssign {
            target: lower_expr(target, ctx),
            op: *op,
            value: lower_expr(value, ctx),
        }),

        Stmt::Expr(expr) => {
            // Try statement-level rewrites for SystemCalls.
            if let Expr::SystemCall {
                system,
                method,
                args,
            } = expr
            {
                if let Some(ref flash) = ctx.flash {
                    match flash::try_lower_system_call_stmt(system, method, args, flash, ctx) {
                        StmtRewrite::Replace(js_stmt) => return Some(js_stmt),
                        StmtRewrite::Skip => return None,
                        StmtRewrite::Pass => {}
                    }
                }
            }
            Some(JsStmt::Expr(lower_expr(expr, ctx)))
        }

        Stmt::If {
            cond,
            then_body,
            else_body,
        } => Some(JsStmt::If {
            cond: lower_expr(cond, ctx),
            then_body: lower_stmts(then_body, ctx),
            else_body: lower_stmts(else_body, ctx),
        }),

        Stmt::While { cond, body } => Some(JsStmt::While {
            cond: lower_expr(cond, ctx),
            body: lower_stmts(body, ctx),
        }),

        Stmt::For {
            init,
            cond,
            update,
            body,
        } => Some(JsStmt::For {
            init: lower_stmts(init, ctx),
            cond: lower_expr(cond, ctx),
            update: lower_stmts(update, ctx),
            body: lower_stmts(body, ctx),
        }),

        Stmt::Loop { body } => Some(JsStmt::Loop {
            body: lower_stmts(body, ctx),
        }),

        Stmt::ForOf {
            binding,
            declare,
            iterable,
            body,
        } => Some(JsStmt::ForOf {
            binding: binding.clone(),
            declare: *declare,
            iterable: lower_expr(iterable, ctx),
            body: lower_stmts(body, ctx),
        }),

        Stmt::Return(expr) => Some(JsStmt::Return(expr.as_ref().map(|e| lower_expr(e, ctx)))),
        Stmt::Break => Some(JsStmt::Break),
        Stmt::Continue => Some(JsStmt::Continue),
        Stmt::LabeledBreak { depth } => Some(JsStmt::LabeledBreak { depth: *depth }),

        Stmt::Dispatch { blocks, entry } => Some(JsStmt::Dispatch {
            blocks: blocks
                .iter()
                .map(|(idx, stmts)| (*idx, lower_stmts(stmts, ctx)))
                .collect(),
            entry: *entry,
        }),
    }
}

// ---------------------------------------------------------------------------
// Expression lowering
// ---------------------------------------------------------------------------

/// Lower a single expression from core AST to JS AST.
pub fn lower_expr(expr: &Expr, ctx: &LowerCtx) -> JsExpr {
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

        Expr::Binary { op, lhs, rhs } => {
            // Flash: scope-lookup operand stripping.
            if ctx.flash.is_some() {
                if flash::is_scope_lookup(lhs) {
                    return lower_expr(rhs, ctx);
                }
                if flash::is_scope_lookup(rhs) {
                    return lower_expr(lhs, ctx);
                }
            }
            JsExpr::Binary {
                op: *op,
                lhs: Box::new(lower_expr(lhs, ctx)),
                rhs: Box::new(lower_expr(rhs, ctx)),
            }
        }

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
        } => {
            // Try engine-specific rewrite.
            if let Some(ref flash) = ctx.flash {
                if let Some(js) =
                    flash::try_lower_system_call_expr(system, method, args, flash, ctx)
                {
                    return js;
                }
            }
            // Fallback: unmapped system call passthrough.
            JsExpr::SystemCall {
                system: system.clone(),
                method: method.clone(),
                args: lower_exprs(args, ctx),
            }
        }

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

        Expr::Cast { expr: inner, ty } => JsExpr::Cast {
            expr: Box::new(lower_expr(inner, ctx)),
            ty: ty.clone(),
        },

        Expr::TypeCheck { expr: inner, ty } => JsExpr::TypeCheck {
            expr: Box::new(lower_expr(inner, ctx)),
            ty: ty.clone(),
        },

        Expr::ArrayInit(elems) => JsExpr::ArrayInit(lower_exprs(elems, ctx)),

        Expr::StructInit { name: _, fields } => JsExpr::ObjectInit(
            fields
                .iter()
                .map(|(name, val)| (name.clone(), lower_expr(val, ctx)))
                .collect(),
        ),

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
    }
}

/// Lower a slice of expressions.
pub fn lower_exprs(exprs: &[Expr], ctx: &LowerCtx) -> Vec<JsExpr> {
    exprs.iter().map(|e| lower_expr(e, ctx)).collect()
}

// ---------------------------------------------------------------------------
// Field access lowering
// ---------------------------------------------------------------------------

/// Lower a field access, handling scope-lookup resolution and namespace stripping.
fn lower_field(object: &Expr, field: &str, ctx: &LowerCtx) -> JsExpr {
    // Flash: scope-lookup in object position → resolve via Flash module.
    if let Some(ref flash) = ctx.flash {
        if let Some(result) = flash::try_resolve_field(object, field, flash) {
            return result;
        }
    }

    // Strip `::` namespace from field name (IR convention).
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

/// Lower a Call expression, handling qualified names, scope lookups, and
/// receiver-based dispatch.
fn lower_call(fname: &str, args: &[Expr], ctx: &LowerCtx) -> JsExpr {
    // Case 1: Qualified name (contains `::`) with args → method dispatch.
    if fname.contains("::") && !args.is_empty() {
        let method = fname.rsplit("::").next().unwrap_or(fname);
        let receiver = &args[0];
        let rest = &args[1..];

        // Flash: check if receiver is a scope-lookup.
        if let Some(ref flash) = ctx.flash {
            if let Some(result) = flash::try_resolve_scope_call(method, receiver, rest, flash, ctx)
            {
                return result;
            }
        }

        return JsExpr::Call {
            callee: Box::new(JsExpr::Field {
                object: Box::new(lower_expr(receiver, ctx)),
                field: method.to_string(),
            }),
            args: lower_exprs(rest, ctx),
        };
    }

    // Case 2: Unqualified with scope-lookup receiver → strip scope lookup.
    if !args.is_empty() {
        if let Some(ref flash) = ctx.flash {
            if flash::is_scope_lookup(&args[0]) {
                let rest = &args[1..];
                let lowered_rest = lower_exprs(rest, ctx);
                let use_this =
                    (flash.has_self && flash.method_names.contains(fname)) || flash.is_cinit;
                let callee = if use_this {
                    JsExpr::Field {
                        object: Box::new(JsExpr::This),
                        field: fname.to_string(),
                    }
                } else {
                    JsExpr::Var(fname.to_string())
                };
                return JsExpr::Call {
                    callee: Box::new(callee),
                    args: lowered_rest,
                };
            }
        }
    }

    // Case 3: Dotted name (e.g. Math.max) → global function call.
    if fname.contains('.') {
        return JsExpr::Call {
            callee: Box::new(build_dotted_path(fname)),
            args: lower_exprs(args, ctx),
        };
    }

    // Case 4: Unqualified with args → receiver.method(rest).
    if !args.is_empty() {
        let receiver = &args[0];
        let rest = &args[1..];
        return JsExpr::Call {
            callee: Box::new(JsExpr::Field {
                object: Box::new(lower_expr(receiver, ctx)),
                field: fname.to_string(),
            }),
            args: lower_exprs(rest, ctx),
        };
    }

    // Case 5: Bare function call with no args.
    JsExpr::Call {
        callee: Box::new(JsExpr::Var(fname.to_string())),
        args: Vec::new(),
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
