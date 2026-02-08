//! Lower `Shape + Function` to the high-level AST (`Vec<Stmt>`).
//!
//! This is the single place that handles expression inlining, constant
//! propagation, dead-code elimination, while-loop condition hoisting,
//! ternary detection, Math.max/min, and LogicalOr/And → short-circuit
//! operators. Each backend only pretty-prints the resulting AST.

use std::collections::{HashMap, HashSet};

use crate::entity::EntityRef;
use crate::transforms::util::value_operands;

use super::ast::{AstFunction, BinOp, Expr, Stmt, UnaryOp};
use super::block::BlockId;
use super::func::Function;
use super::inst::{CmpKind, InstId, Op};
use super::structurize::{BlockArgAssign, Shape};
use super::ty::Type;
use super::value::{Constant, ValueId};

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Lower a structured function into AST form.
///
/// `shape` is the structurized control-flow tree (from `structurize()`).
pub fn lower_function(func: &Function, shape: &Shape) -> AstFunction {
    let mut ctx = LowerCtx::new(func);
    adjust_use_counts_for_shapes(&mut ctx, func, shape);

    let mut body = lower_shape(&mut ctx, func, shape);
    strip_trailing_void_return(&mut body);

    // Collect block-param declarations that were referenced.
    let decls = ctx.collect_block_param_decls(func);

    // Prepend declarations to body.
    let mut full_body = decls;
    full_body.append(&mut body);

    AstFunction {
        name: func.name.clone(),
        params: build_params(func, &ctx),
        return_ty: func.sig.return_ty.clone(),
        body: full_body,
        is_generator: func.coroutine.is_some(),
        visibility: func.visibility,
        method_kind: func.method_kind,
    }
}

fn build_params(func: &Function, ctx: &LowerCtx) -> Vec<(String, Type)> {
    func.blocks[func.entry]
        .params
        .iter()
        .map(|p| (ctx.value_name(p.value), p.ty.clone()))
        .collect()
}

// ---------------------------------------------------------------------------
// Lowering context — replaces EmitCtx
// ---------------------------------------------------------------------------

struct LowerCtx {
    /// Number of times each value is used as an operand (after shape adjustments).
    use_counts: HashMap<ValueId, usize>,
    /// Deferred single-use pure instructions for lazy expression building.
    lazy_inlines: HashMap<ValueId, InstId>,
    /// Constants that are always inlined (not removed on read).
    constant_inlines: HashMap<ValueId, Constant>,
    /// Alloc results whose immediately-following Store is merged.
    alloc_inits: HashMap<ValueId, ValueId>,
    /// Store InstIds that were merged into their preceding Alloc.
    skip_stores: HashSet<InstId>,
    /// Debug names for values.
    value_names: HashMap<ValueId, String>,
    /// Entry-block parameter ValueIds (declared in function signature).
    entry_params: HashSet<ValueId>,
    /// Block-param ValueIds that have been referenced during lowering.
    referenced_block_params: HashSet<ValueId>,
    /// Set of ValueIds whose inline expressions involve side effects.
    /// Tracks which deferred inlines carry side-effecting sub-expressions.
    side_effecting_inlines: HashMap<ValueId, Expr>,
}

impl LowerCtx {
    fn new(func: &Function) -> Self {
        let use_counts = compute_use_counts(func);
        let (alloc_inits, skip_stores) = compute_merged_stores(func);
        let value_names: HashMap<ValueId, String> = func
            .value_names
            .iter()
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        let entry_params: HashSet<ValueId> = func.blocks[func.entry]
            .params
            .iter()
            .map(|p| p.value)
            .collect();

        Self {
            use_counts,
            lazy_inlines: HashMap::new(),
            constant_inlines: HashMap::new(),
            alloc_inits,
            skip_stores,
            value_names,
            entry_params,
            referenced_block_params: HashSet::new(),
            side_effecting_inlines: HashMap::new(),
        }
    }

    fn use_count(&self, v: ValueId) -> usize {
        self.use_counts.get(&v).copied().unwrap_or(0)
    }

    fn should_inline(&self, v: ValueId) -> bool {
        self.use_count(v) == 1
    }

    /// Get a display name for a value.
    fn value_name(&self, v: ValueId) -> String {
        if let Some(name) = self.value_names.get(&v) {
            name.clone()
        } else {
            format!("v{}", v.index())
        }
    }

    /// Build an expression for a value reference.
    ///
    /// If the value was deferred for lazy inlining, builds its expression
    /// on demand. Constants are always inlined. Otherwise returns `Expr::Var`.
    fn build_val(&mut self, func: &Function, v: ValueId) -> Expr {
        // Check for constant inline first (always inlined, not consumed).
        if let Some(c) = self.constant_inlines.get(&v) {
            return Expr::Literal(c.clone());
        }

        // Check for side-effecting inline expression.
        if let Some(expr) = self.side_effecting_inlines.remove(&v) {
            return expr;
        }

        // Check for lazy inline (deferred single-use pure instruction).
        if let Some(inst_id) = self.lazy_inlines.remove(&v) {
            if let Some(expr) = self.build_inst_expr(func, inst_id) {
                return expr;
            }
        }

        // Track block-param references for declaration generation.
        if !self.entry_params.contains(&v) {
            self.referenced_block_params.insert(v);
        }

        Expr::Var(self.value_name(v))
    }

    /// Build an Expr from an instruction (for inlining).
    fn build_inst_expr(&mut self, func: &Function, inst_id: InstId) -> Option<Expr> {
        let inst = &func.insts[inst_id];
        build_expr_from_op(self, func, &inst.op, inst.result)
    }

    /// Flush all deferred memory-read lazy inlines into real statements.
    fn flush_pending_reads(&mut self, func: &Function, stmts: &mut Vec<Stmt>) {
        let to_flush: Vec<(ValueId, InstId)> = self
            .lazy_inlines
            .iter()
            .filter(|(_, &iid)| {
                matches!(
                    func.insts[iid].op,
                    Op::GetField { .. } | Op::GetIndex { .. } | Op::Load(..)
                )
            })
            .map(|(&v, &iid)| (v, iid))
            .collect();

        for (v, iid) in to_flush {
            self.lazy_inlines.remove(&v);
            // Bump use count so it gets a real variable assignment.
            *self.use_counts.entry(v).or_insert(1) = 2;
            lower_inst(self, func, iid, stmts);
        }
    }

    /// Flush side-effecting inline expressions as real variable assignments.
    fn flush_side_effecting_inlines(&mut self, stmts: &mut Vec<Stmt>) {
        let to_flush: Vec<ValueId> = self
            .side_effecting_inlines
            .keys()
            .copied()
            .collect();

        for v in to_flush {
            let expr = self.side_effecting_inlines.remove(&v).unwrap();
            let is_inst_result = !self.entry_params.contains(&v);
            stmts.push(Stmt::VarDecl {
                name: self.value_name(v),
                ty: None,
                init: Some(expr),
                mutable: false,
            });
            // Suppress later let prefix since we've already declared.
            if is_inst_result {
                // Already declared, no further action needed.
            }
        }
    }

    /// Collect block-param declarations for all non-entry blocks.
    fn collect_block_param_decls(&self, func: &Function) -> Vec<Stmt> {
        let mut decls = Vec::new();
        let mut declared = HashSet::new();
        // Pre-seed with entry params.
        for p in &func.blocks[func.entry].params {
            declared.insert(self.value_name(p.value));
        }
        for (block_id, block) in func.blocks.iter() {
            if block_id == func.entry {
                continue;
            }
            for param in &block.params {
                let name = self.value_name(param.value);
                if self.referenced_block_params.contains(&param.value)
                    && declared.insert(name.clone())
                {
                    decls.push(Stmt::VarDecl {
                        name,
                        ty: Some(param.ty.clone()),
                        init: None,
                        mutable: true,
                    });
                }
            }
        }
        decls
    }
}

// ---------------------------------------------------------------------------
// Use counting
// ---------------------------------------------------------------------------

fn compute_use_counts(func: &Function) -> HashMap<ValueId, usize> {
    let mut counts = HashMap::new();
    for (_, inst) in func.insts.iter() {
        for v in value_operands(&inst.op) {
            *counts.entry(v).or_insert(0) += 1;
        }
    }
    counts
}

fn compute_merged_stores(func: &Function) -> (HashMap<ValueId, ValueId>, HashSet<InstId>) {
    let mut alloc_inits = HashMap::new();
    let mut skip_stores = HashSet::new();
    for block in func.blocks.values() {
        for pair in block.insts.windows(2) {
            let alloc_inst = &func.insts[pair[0]];
            if let Op::Alloc(_) = &alloc_inst.op {
                let alloc_result = alloc_inst.result.unwrap();
                if let Op::Store { ptr, value } = &func.insts[pair[1]].op {
                    if *ptr == alloc_result {
                        alloc_inits.insert(alloc_result, *value);
                        skip_stores.insert(pair[1]);
                    }
                }
            }
        }
    }
    (alloc_inits, skip_stores)
}

// ---------------------------------------------------------------------------
// Shape-aware use count adjustments
// ---------------------------------------------------------------------------

fn adjust_use_counts_for_shapes(ctx: &mut LowerCtx, func: &Function, shape: &Shape) {
    match shape {
        Shape::LogicalOr {
            block,
            cond,
            rhs_body,
            ..
        }
        | Shape::LogicalAnd {
            block,
            cond,
            rhs_body,
            ..
        } => {
            if logical_rhs_body_emits_empty(ctx, func, rhs_body) {
                let brif_cond = brif_cond_of(func, *block);
                if brif_cond == Some(*cond) {
                    if let Some(count) = ctx.use_counts.get_mut(cond) {
                        *count = count.saturating_sub(1);
                    }
                } else {
                    adjust_for_stripped_not(ctx, func, *block, *cond);
                }
            }
            adjust_use_counts_for_shapes(ctx, func, rhs_body);
        }
        Shape::IfElse {
            block,
            cond,
            then_assigns,
            then_body,
            then_trailing_assigns,
            else_assigns,
            else_body,
            else_trailing_assigns,
        } => {
            if let Some((_, then_src, else_src)) = try_ternary_assigns(
                ctx,
                func,
                (then_assigns, then_body, then_trailing_assigns),
                (else_assigns, else_body, else_trailing_assigns),
            ) {
                if try_minmax(func, *block, *cond, then_src, else_src).is_some() {
                    for &iid in func.blocks[*block].insts.iter().rev() {
                        let inst = &func.insts[iid];
                        if inst.result == Some(*cond) {
                            if let Op::Cmp(_, lhs, rhs) = &inst.op {
                                for v in [*lhs, *rhs] {
                                    if let Some(c) = ctx.use_counts.get_mut(&v) {
                                        if *c > 1 {
                                            *c -= 1;
                                        }
                                    }
                                }
                            }
                            break;
                        }
                    }
                }
            }
            adjust_use_counts_for_shapes(ctx, func, then_body);
            adjust_use_counts_for_shapes(ctx, func, else_body);
        }
        Shape::WhileLoop { body, .. }
        | Shape::ForLoop { body, .. }
        | Shape::Loop { body, .. } => {
            adjust_use_counts_for_shapes(ctx, func, body);
        }
        Shape::Seq(parts) => {
            for part in parts {
                adjust_use_counts_for_shapes(ctx, func, part);
            }
        }
        _ => {}
    }
}

fn brif_cond_of(func: &Function, block: BlockId) -> Option<ValueId> {
    func.blocks[block]
        .insts
        .last()
        .and_then(|&iid| match &func.insts[iid].op {
            Op::BrIf { cond: c, .. } => Some(*c),
            _ => None,
        })
}

fn adjust_for_stripped_not(
    ctx: &mut LowerCtx,
    func: &Function,
    block: BlockId,
    cond: ValueId,
) {
    let Some(bc) = brif_cond_of(func, block) else {
        return;
    };
    if bc == cond {
        return;
    }
    let is_not_of_cond = func
        .insts
        .iter()
        .any(|(_, inst)| {
            inst.result == Some(bc)
                && matches!(&inst.op, Op::Not(inner) if *inner == cond)
        });
    if is_not_of_cond {
        if let Some(count) = ctx.use_counts.get_mut(&cond) {
            *count = count.saturating_sub(1);
        }
    }
}

fn logical_rhs_body_emits_empty(ctx: &LowerCtx, func: &Function, shape: &Shape) -> bool {
    match shape {
        Shape::Seq(parts) => parts
            .iter()
            .all(|p| logical_rhs_body_emits_empty(ctx, func, p)),
        Shape::Block(b) => {
            let block = &func.blocks[*b];
            for &inst_id in &block.insts {
                let inst = &func.insts[inst_id];
                match &inst.op {
                    Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => break,
                    _ => {
                        if let Some(r) = inst.result {
                            if !ctx.should_inline(r) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                }
            }
            true
        }
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Ternary / Math.max/min detection
// ---------------------------------------------------------------------------

fn try_ternary_assigns(
    ctx: &LowerCtx,
    func: &Function,
    then_branch: (&[BlockArgAssign], &Shape, &[BlockArgAssign]),
    else_branch: (&[BlockArgAssign], &Shape, &[BlockArgAssign]),
) -> Option<(ValueId, ValueId, ValueId)> {
    let (then_assigns, then_body, then_trailing) = then_branch;
    let (else_assigns, else_body, else_trailing) = else_branch;

    let then_all: Vec<_> = then_assigns.iter().chain(then_trailing).collect();
    let else_all: Vec<_> = else_assigns.iter().chain(else_trailing).collect();

    if then_all.len() != 1 || else_all.len() != 1 {
        return None;
    }
    let then_a = then_all[0];
    let else_a = else_all[0];
    if ctx.value_name(then_a.dst) != ctx.value_name(else_a.dst) {
        return None;
    }

    if !is_trivial_body(ctx, func, then_body) || !is_trivial_body(ctx, func, else_body) {
        return None;
    }

    Some((then_a.dst, then_a.src, else_a.src))
}

fn is_trivial_body(ctx: &LowerCtx, func: &Function, shape: &Shape) -> bool {
    match shape {
        Shape::Block(b) => {
            let block = &func.blocks[*b];
            for &inst_id in &block.insts {
                let inst = &func.insts[inst_id];
                match &inst.op {
                    Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => break,
                    _ => {
                        if let Some(r) = inst.result {
                            if !ctx.should_inline(r)
                                && ctx.use_count(r) > 0
                            {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                }
            }
            true
        }
        Shape::Seq(parts) => parts.iter().all(|p| is_trivial_body(ctx, func, p)),
        _ => false,
    }
}

fn try_minmax(
    func: &Function,
    block: BlockId,
    cond: ValueId,
    then_val: ValueId,
    else_val: ValueId,
) -> Option<(&'static str, ValueId, ValueId)> {
    let blk = &func.blocks[block];
    let cmp = blk.insts.iter().rev().find_map(|&inst_id| {
        let inst = &func.insts[inst_id];
        if inst.result == Some(cond) {
            if let Op::Cmp(kind, lhs, rhs) = &inst.op {
                return Some((*kind, *lhs, *rhs));
            }
        }
        None
    })?;
    let (kind, lhs, rhs) = cmp;

    let equiv = |a: ValueId, b: ValueId| -> bool {
        if a == b {
            return true;
        }
        let as_f64 = |v: ValueId| -> Option<f64> {
            func.insts.iter().find_map(|(_, inst)| {
                if inst.result == Some(v) {
                    match &inst.op {
                        Op::Const(Constant::Int(n)) => Some(*n as f64),
                        Op::Const(Constant::UInt(n)) => Some(*n as f64),
                        Op::Const(Constant::Float(n)) => Some(*n),
                        _ => None,
                    }
                } else {
                    None
                }
            })
        };
        matches!((as_f64(a), as_f64(b)), (Some(x), Some(y)) if x == y)
    };

    match kind {
        CmpKind::Ge | CmpKind::Gt => {
            if equiv(then_val, lhs) && equiv(else_val, rhs) {
                Some(("Math.max", then_val, else_val))
            } else if equiv(then_val, rhs) && equiv(else_val, lhs) {
                Some(("Math.min", then_val, else_val))
            } else {
                None
            }
        }
        CmpKind::Le | CmpKind::Lt => {
            if equiv(then_val, lhs) && equiv(else_val, rhs) {
                Some(("Math.min", then_val, else_val))
            } else if equiv(then_val, rhs) && equiv(else_val, lhs) {
                Some(("Math.max", then_val, else_val))
            } else {
                None
            }
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Expression building — converts Op → Expr
// ---------------------------------------------------------------------------

/// Whether an instruction is pure enough to defer for lazy inlining.
fn is_deferrable(op: &Op) -> bool {
    matches!(
        op,
        Op::Const(_)
            | Op::Add(..)
            | Op::Sub(..)
            | Op::Mul(..)
            | Op::Div(..)
            | Op::Rem(..)
            | Op::Neg(..)
            | Op::Not(..)
            | Op::BitAnd(..)
            | Op::BitOr(..)
            | Op::BitXor(..)
            | Op::BitNot(..)
            | Op::Shl(..)
            | Op::Shr(..)
            | Op::Cmp(..)
            | Op::Cast(..)
            | Op::Copy(..)
            | Op::GetField { .. }
            | Op::GetIndex { .. }
            | Op::Load(..)
            | Op::Select { .. }
            | Op::ArrayInit(..)
            | Op::TupleInit(..)
            | Op::StructInit { .. }
            | Op::GlobalRef(..)
            | Op::TypeCheck(..)
    )
}

fn is_memory_write(op: &Op) -> bool {
    matches!(op, Op::SetField { .. } | Op::SetIndex { .. } | Op::Store { .. })
}

fn has_side_effecting_operand(ctx: &LowerCtx, func: &Function, inst_id: InstId) -> bool {
    for v in value_operands(&func.insts[inst_id].op) {
        if ctx.side_effecting_inlines.contains_key(&v) {
            return true;
        }
    }
    false
}

/// Build an Expr from an Op (for single-use inlining).
fn build_expr_from_op(
    ctx: &mut LowerCtx,
    func: &Function,
    op: &Op,
    _result: Option<ValueId>,
) -> Option<Expr> {
    Some(match op {
        Op::Const(c) => Expr::Literal(c.clone()),

        // Arithmetic
        Op::Add(a, b) => Expr::Binary {
            op: BinOp::Add,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::Sub(a, b) => Expr::Binary {
            op: BinOp::Sub,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::Mul(a, b) => Expr::Binary {
            op: BinOp::Mul,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::Div(a, b) => Expr::Binary {
            op: BinOp::Div,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::Rem(a, b) => Expr::Binary {
            op: BinOp::Rem,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::Neg(a) => Expr::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(ctx.build_val(func, *a)),
        },

        // Bitwise
        Op::BitAnd(a, b) => Expr::Binary {
            op: BinOp::BitAnd,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::BitOr(a, b) => Expr::Binary {
            op: BinOp::BitOr,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::BitXor(a, b) => Expr::Binary {
            op: BinOp::BitXor,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::BitNot(a) => Expr::Unary {
            op: UnaryOp::BitNot,
            expr: Box::new(ctx.build_val(func, *a)),
        },
        Op::Shl(a, b) => Expr::Binary {
            op: BinOp::Shl,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },
        Op::Shr(a, b) => Expr::Binary {
            op: BinOp::Shr,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },

        // Comparison
        Op::Cmp(kind, a, b) => Expr::Cmp {
            kind: *kind,
            lhs: Box::new(ctx.build_val(func, *a)),
            rhs: Box::new(ctx.build_val(func, *b)),
        },

        // Logic
        Op::Not(a) => Expr::Not(Box::new(ctx.build_val(func, *a))),
        Op::Select {
            cond,
            on_true,
            on_false,
        } => Expr::Ternary {
            cond: Box::new(ctx.build_val(func, *cond)),
            then_val: Box::new(ctx.build_val(func, *on_true)),
            else_val: Box::new(ctx.build_val(func, *on_false)),
        },

        // Memory / fields
        Op::Load(ptr) => ctx.build_val(func, *ptr),
        Op::GetField { object, field } => Expr::Field {
            object: Box::new(ctx.build_val(func, *object)),
            field: field.clone(),
        },
        Op::GetIndex { collection, index } => Expr::Index {
            collection: Box::new(ctx.build_val(func, *collection)),
            index: Box::new(ctx.build_val(func, *index)),
        },

        // Calls
        Op::Call {
            func: fname,
            args,
        } => Expr::Call {
            func: fname.clone(),
            args: args.iter().map(|a| ctx.build_val(func, *a)).collect(),
        },
        Op::CallIndirect { callee, args } => Expr::CallIndirect {
            callee: Box::new(ctx.build_val(func, *callee)),
            args: args.iter().map(|a| ctx.build_val(func, *a)).collect(),
        },
        Op::SystemCall {
            system,
            method,
            args,
        } => Expr::SystemCall {
            system: system.clone(),
            method: method.clone(),
            args: args.iter().map(|a| ctx.build_val(func, *a)).collect(),
        },

        // Type operations
        Op::Cast(v, ty) => Expr::Cast {
            expr: Box::new(ctx.build_val(func, *v)),
            ty: ty.clone(),
        },
        Op::TypeCheck(v, ty) => Expr::TypeCheck {
            expr: Box::new(ctx.build_val(func, *v)),
            ty: ty.clone(),
        },

        // Aggregate construction
        Op::StructInit { name, fields } => Expr::StructInit {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|(n, v)| (n.clone(), ctx.build_val(func, *v)))
                .collect(),
        },
        Op::ArrayInit(elems) => {
            Expr::ArrayInit(elems.iter().map(|v| ctx.build_val(func, *v)).collect())
        }
        Op::TupleInit(elems) => {
            Expr::TupleInit(elems.iter().map(|v| ctx.build_val(func, *v)).collect())
        }

        // Coroutines
        Op::Yield(v) => Expr::Yield(v.map(|yv| Box::new(ctx.build_val(func, yv)))),
        Op::CoroutineCreate {
            func: fname,
            args,
        } => Expr::CoroutineCreate {
            func: fname.clone(),
            args: args.iter().map(|a| ctx.build_val(func, *a)).collect(),
        },
        Op::CoroutineResume(v) => Expr::CoroutineResume(Box::new(ctx.build_val(func, *v))),

        // Misc
        Op::GlobalRef(name) => Expr::GlobalRef(name.clone()),
        Op::Copy(src) => ctx.build_val(func, *src),

        // Control flow — not expression-level.
        Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } | Op::Return(_) => return None,
        Op::Alloc(_) | Op::Store { .. } | Op::SetField { .. } | Op::SetIndex { .. } => {
            return None;
        }
    })
}

/// Whether a call-like op has side effects (for tracking inline purity).
fn is_side_effecting_op(op: &Op) -> bool {
    matches!(
        op,
        Op::Call { .. }
            | Op::CallIndirect { .. }
            | Op::SystemCall { .. }
            | Op::Yield(..)
            | Op::CoroutineResume(..)
    )
}

// ---------------------------------------------------------------------------
// Instruction lowering — single instruction → Stmt(s)
// ---------------------------------------------------------------------------

fn lower_inst(ctx: &mut LowerCtx, func: &Function, inst_id: InstId, stmts: &mut Vec<Stmt>) {
    let inst = &func.insts[inst_id];
    let result = inst.result;

    match &inst.op {
        // Skip terminators — handled by shape.
        Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => {}

        Op::Return(v) => {
            stmts.push(Stmt::Return(v.map(|rv| ctx.build_val(func, rv))));
        }

        Op::Alloc(ty) => {
            let r = result.unwrap();
            let init_val = ctx.alloc_inits.get(&r).copied();
            let init = init_val.map(|iv| ctx.build_val(func, iv));
            stmts.push(Stmt::VarDecl {
                name: ctx.value_name(r),
                ty: Some(ty.clone()),
                init,
                mutable: true,
            });
        }

        Op::Store { ptr, value } => {
            if !ctx.skip_stores.contains(&inst_id) {
                let target = ctx.build_val(func, *ptr);
                let val = ctx.build_val(func, *value);
                stmts.push(Stmt::Assign {
                    target,
                    value: val,
                });
            }
        }

        Op::SetField {
            object,
            field,
            value,
        } => {
            let target = Expr::Field {
                object: Box::new(ctx.build_val(func, *object)),
                field: field.clone(),
            };
            let val = ctx.build_val(func, *value);
            stmts.push(Stmt::Assign {
                target,
                value: val,
            });
        }

        Op::SetIndex {
            collection,
            index,
            value,
        } => {
            let target = Expr::Index {
                collection: Box::new(ctx.build_val(func, *collection)),
                index: Box::new(ctx.build_val(func, *index)),
            };
            let val = ctx.build_val(func, *value);
            stmts.push(Stmt::Assign {
                target,
                value: val,
            });
        }

        // Yield — always emit directly (side effects).
        Op::Yield(_) => {
            if let Some(r) = result {
                let expr = build_expr_from_op(ctx, func, &inst.op, result).unwrap();
                stmts.push(Stmt::VarDecl {
                    name: ctx.value_name(r),
                    ty: None,
                    init: Some(expr),
                    mutable: false,
                });
            } else {
                let expr = build_expr_from_op(ctx, func, &inst.op, result).unwrap();
                stmts.push(Stmt::Expr(expr));
            }
        }

        // Everything else — build expression and inline or declare.
        _ => {
            if let Some(r) = result {
                let expr = build_expr_from_op(ctx, func, &inst.op, result)
                    .unwrap_or_else(|| Expr::Var(ctx.value_name(r)));

                let count = ctx.use_count(r);
                let side_effecting = is_side_effecting_op(&inst.op);

                if count == 0 && !side_effecting {
                    // Dead — skip.
                } else if count == 0 && side_effecting {
                    // Side-effecting but unused result — emit as expression stmt.
                    stmts.push(Stmt::Expr(expr));
                } else if count == 1 && !side_effecting {
                    // Single-use pure — should have been deferred/inlined already.
                    // If we get here, emit as VarDecl.
                    stmts.push(Stmt::VarDecl {
                        name: ctx.value_name(r),
                        ty: None,
                        init: Some(expr),
                        mutable: false,
                    });
                } else if count == 1 && side_effecting {
                    // Single-use side-effecting — store for inline at use site.
                    ctx.side_effecting_inlines.insert(r, expr);
                } else {
                    // Multi-use — emit as VarDecl.
                    stmts.push(Stmt::VarDecl {
                        name: ctx.value_name(r),
                        ty: None,
                        init: Some(expr),
                        mutable: false,
                    });
                }
            } else {
                // No result — expression statement.
                if let Some(expr) = build_expr_from_op(ctx, func, &inst.op, result) {
                    stmts.push(Stmt::Expr(expr));
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Block instruction lowering
// ---------------------------------------------------------------------------

fn lower_block_instructions(
    ctx: &mut LowerCtx,
    func: &Function,
    block_id: BlockId,
    stmts: &mut Vec<Stmt>,
) {
    ctx.flush_side_effecting_inlines(stmts);

    let block = &func.blocks[block_id];
    for &inst_id in &block.insts {
        let inst = &func.insts[inst_id];
        match &inst.op {
            Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => {
                break;
            }
            _ => {
                if is_memory_write(&inst.op) {
                    ctx.flush_pending_reads(func, stmts);
                }

                if let Some(r) = inst.result {
                    let count = ctx.use_count(r);
                    if is_deferrable(&inst.op) {
                        if count == 0 {
                            continue;
                        }
                        if let Op::Const(ref c) = inst.op {
                            ctx.constant_inlines.insert(r, c.clone());
                            continue;
                        }
                        if count == 1
                            && !has_side_effecting_operand(ctx, func, inst_id)
                        {
                            ctx.lazy_inlines.insert(r, inst_id);
                            continue;
                        }
                    }
                }
                lower_inst(ctx, func, inst_id, stmts);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Shape lowering — Shape → Vec<Stmt>
// ---------------------------------------------------------------------------

fn lower_shape(ctx: &mut LowerCtx, func: &Function, shape: &Shape) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    lower_shape_into(ctx, func, shape, &mut stmts);
    stmts
}

fn lower_shape_into(
    ctx: &mut LowerCtx,
    func: &Function,
    shape: &Shape,
    stmts: &mut Vec<Stmt>,
) {
    match shape {
        Shape::Block(block_id) => {
            lower_block_instructions(ctx, func, *block_id, stmts);
        }

        Shape::Seq(parts) => {
            for part in parts {
                lower_shape_into(ctx, func, part, stmts);
            }
        }

        Shape::IfElse {
            block,
            cond,
            then_assigns,
            then_body,
            then_trailing_assigns,
            else_assigns,
            else_body,
            else_trailing_assigns,
        } => {
            lower_block_instructions(ctx, func, *block, stmts);
            ctx.flush_side_effecting_inlines(stmts);

            // Try ternary pattern.
            if let Some((dst, then_src, else_src)) = try_ternary_assigns(
                ctx,
                func,
                (then_assigns, then_body, then_trailing_assigns),
                (else_assigns, else_body, else_trailing_assigns),
            ) {
                // Process trivial bodies so their insts get deferred.
                lower_shape(ctx, func, then_body);
                lower_shape(ctx, func, else_body);

                if let Some((name, a, b)) =
                    try_minmax(func, *block, *cond, then_src, else_src)
                {
                    // Math.max / Math.min
                    let expr = Expr::Call {
                        func: name.to_string(),
                        args: vec![
                            ctx.build_val(func, a),
                            ctx.build_val(func, b),
                        ],
                    };
                    ctx.referenced_block_params.insert(dst);
                    stmts.push(Stmt::Assign {
                        target: Expr::Var(ctx.value_name(dst)),
                        value: expr,
                    });
                } else {
                    // Ternary: dst = cond ? then_src : else_src
                    let cond_expr = ctx.build_val(func, *cond);
                    let then_expr = ctx.build_val(func, then_src);
                    let else_expr = ctx.build_val(func, else_src);
                    ctx.referenced_block_params.insert(dst);
                    stmts.push(Stmt::Assign {
                        target: Expr::Var(ctx.value_name(dst)),
                        value: Expr::Ternary {
                            cond: Box::new(cond_expr),
                            then_val: Box::new(then_expr),
                            else_val: Box::new(else_expr),
                        },
                    });
                }
            } else {
                let cond_expr = ctx.build_val(func, *cond);

                let mut then_stmts = Vec::new();
                lower_arg_assigns(ctx, func, then_assigns, &mut then_stmts);
                lower_shape_into(ctx, func, then_body, &mut then_stmts);
                lower_arg_assigns(ctx, func, then_trailing_assigns, &mut then_stmts);

                let mut else_stmts = Vec::new();
                lower_arg_assigns(ctx, func, else_assigns, &mut else_stmts);
                lower_shape_into(ctx, func, else_body, &mut else_stmts);
                lower_arg_assigns(ctx, func, else_trailing_assigns, &mut else_stmts);

                let then_empty = then_stmts.is_empty();
                let else_empty = else_stmts.is_empty();

                match (then_empty, else_empty) {
                    (true, true) => {
                        // Both empty — skip.
                    }
                    (false, true) => {
                        stmts.push(Stmt::If {
                            cond: cond_expr,
                            then_body: then_stmts,
                            else_body: Vec::new(),
                        });
                    }
                    (true, false) => {
                        // Negate condition and use else as then.
                        let neg = negate_cond_expr(ctx, func, *block, *cond);
                        stmts.push(Stmt::If {
                            cond: neg,
                            then_body: else_stmts,
                            else_body: Vec::new(),
                        });
                    }
                    (false, false) => {
                        stmts.push(Stmt::If {
                            cond: cond_expr,
                            then_body: then_stmts,
                            else_body: else_stmts,
                        });
                    }
                }
            }
        }

        Shape::WhileLoop {
            header,
            cond,
            cond_negated,
            body,
        } => {
            let mut loop_body = Vec::new();
            lower_block_instructions(ctx, func, *header, &mut loop_body);

            let break_expr = if *cond_negated {
                ctx.build_val(func, *cond)
            } else {
                negate_cond_expr(ctx, func, *header, *cond)
            };
            loop_body.push(Stmt::If {
                cond: break_expr,
                then_body: vec![Stmt::Break],
                else_body: Vec::new(),
            });

            let mut body_stmts = lower_shape(ctx, func, body);
            strip_trailing_continue(&mut body_stmts);
            loop_body.append(&mut body_stmts);

            stmts.push(Stmt::Loop { body: loop_body });
        }

        Shape::ForLoop {
            header,
            init_assigns,
            cond,
            cond_negated,
            update_assigns,
            body,
        } => {
            lower_arg_assigns(ctx, func, init_assigns, stmts);

            let mut loop_body = Vec::new();
            lower_block_instructions(ctx, func, *header, &mut loop_body);

            let break_expr = if *cond_negated {
                ctx.build_val(func, *cond)
            } else {
                negate_cond_expr(ctx, func, *header, *cond)
            };
            loop_body.push(Stmt::If {
                cond: break_expr,
                then_body: vec![Stmt::Break],
                else_body: Vec::new(),
            });

            let mut body_stmts = lower_shape(ctx, func, body);
            strip_trailing_continue(&mut body_stmts);
            loop_body.append(&mut body_stmts);

            lower_arg_assigns(ctx, func, update_assigns, &mut loop_body);

            stmts.push(Stmt::Loop { body: loop_body });
        }

        Shape::Loop { header: _, body } => {
            let loop_body = lower_shape(ctx, func, body);
            stmts.push(Stmt::Loop { body: loop_body });
        }

        Shape::Break => {
            stmts.push(Stmt::Break);
        }

        Shape::Continue => {
            stmts.push(Stmt::Continue);
        }

        Shape::LabeledBreak { depth } => {
            stmts.push(Stmt::LabeledBreak { depth: *depth });
        }

        Shape::LogicalOr {
            block,
            cond,
            phi,
            rhs_body,
            rhs,
        } => {
            lower_block_instructions(ctx, func, *block, stmts);
            let body_stmts = lower_shape(ctx, func, rhs_body);

            if body_stmts.is_empty() {
                // Short-circuit: phi = cond || rhs
                let expr = Expr::LogicalOr {
                    lhs: Box::new(ctx.build_val(func, *cond)),
                    rhs: Box::new(ctx.build_val(func, *rhs)),
                };
                emit_or_inline_ast(ctx, *phi, expr, stmts);
            } else {
                // Full if/else for non-trivial rhs.
                let cond_expr = ctx.build_val(func, *cond);
                let then_stmts = vec![Stmt::Assign {
                    target: Expr::Var(ctx.value_name(*phi)),
                    value: ctx.build_val(func, *cond),
                }];
                let mut else_stmts = body_stmts;
                else_stmts.push(Stmt::Assign {
                    target: Expr::Var(ctx.value_name(*phi)),
                    value: ctx.build_val(func, *rhs),
                });
                ctx.referenced_block_params.insert(*phi);
                stmts.push(Stmt::If {
                    cond: cond_expr,
                    then_body: then_stmts,
                    else_body: else_stmts,
                });
            }
        }

        Shape::LogicalAnd {
            block,
            cond,
            phi,
            rhs_body,
            rhs,
        } => {
            lower_block_instructions(ctx, func, *block, stmts);
            let body_stmts = lower_shape(ctx, func, rhs_body);

            if body_stmts.is_empty() {
                // Short-circuit: phi = cond && rhs
                let expr = Expr::LogicalAnd {
                    lhs: Box::new(ctx.build_val(func, *cond)),
                    rhs: Box::new(ctx.build_val(func, *rhs)),
                };
                emit_or_inline_ast(ctx, *phi, expr, stmts);
            } else {
                // Full if/else for non-trivial rhs.
                let cond_expr = ctx.build_val(func, *cond);
                let mut then_stmts = body_stmts;
                then_stmts.push(Stmt::Assign {
                    target: Expr::Var(ctx.value_name(*phi)),
                    value: ctx.build_val(func, *rhs),
                });
                let else_stmts = vec![Stmt::Assign {
                    target: Expr::Var(ctx.value_name(*phi)),
                    value: ctx.build_val(func, *cond),
                }];
                ctx.referenced_block_params.insert(*phi);
                stmts.push(Stmt::If {
                    cond: cond_expr,
                    then_body: then_stmts,
                    else_body: else_stmts,
                });
            }
        }

        Shape::Dispatch { blocks, entry } => {
            let mut dispatch_blocks = Vec::new();
            for &block_id in blocks {
                let block_stmts = lower_dispatch_block(ctx, func, block_id);
                dispatch_blocks.push((block_id.index() as usize, block_stmts));
            }
            stmts.push(Stmt::Dispatch {
                blocks: dispatch_blocks,
                entry: entry.index() as usize,
            });
        }
    }
}

/// Lower a dispatch block — emits all instructions including terminators.
fn lower_dispatch_block(
    ctx: &mut LowerCtx,
    func: &Function,
    block_id: BlockId,
) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    let block = &func.blocks[block_id];
    for &inst_id in &block.insts {
        lower_inst(ctx, func, inst_id, &mut stmts);
    }
    stmts
}

/// Either inline a single-use expression or emit it as a VarDecl/Assign.
fn emit_or_inline_ast(ctx: &mut LowerCtx, v: ValueId, expr: Expr, stmts: &mut Vec<Stmt>) {
    let count = ctx.use_count(v);
    if count == 1 {
        ctx.side_effecting_inlines.insert(v, expr);
    } else if count == 0 {
        stmts.push(Stmt::Expr(expr));
    } else {
        ctx.referenced_block_params.insert(v);
        stmts.push(Stmt::VarDecl {
            name: ctx.value_name(v),
            ty: None,
            init: Some(expr),
            mutable: false,
        });
    }
}

// ---------------------------------------------------------------------------
// Condition negation
// ---------------------------------------------------------------------------

fn negate_cond_expr(
    ctx: &mut LowerCtx,
    func: &Function,
    block: BlockId,
    cond: ValueId,
) -> Expr {
    // Try to find a Cmp instruction that can be inverted.
    for &inst_id in &func.blocks[block].insts {
        let inst = &func.insts[inst_id];
        if inst.result == Some(cond) {
            if let Op::Cmp(kind, a, b) = &inst.op {
                if ctx.should_inline(cond) {
                    return Expr::Cmp {
                        kind: kind.inverse(),
                        lhs: Box::new(ctx.build_val(func, *a)),
                        rhs: Box::new(ctx.build_val(func, *b)),
                    };
                }
            }
            break;
        }
    }
    Expr::Not(Box::new(ctx.build_val(func, cond)))
}

// ---------------------------------------------------------------------------
// Arg assigns
// ---------------------------------------------------------------------------

fn lower_arg_assigns(
    ctx: &mut LowerCtx,
    func: &Function,
    assigns: &[BlockArgAssign],
    stmts: &mut Vec<Stmt>,
) {
    for assign in assigns {
        ctx.referenced_block_params.insert(assign.dst);
        stmts.push(Stmt::Assign {
            target: Expr::Var(ctx.value_name(assign.dst)),
            value: ctx.build_val(func, assign.src),
        });
    }
}

// ---------------------------------------------------------------------------
// Trailing statement stripping
// ---------------------------------------------------------------------------

fn strip_trailing_continue(stmts: &mut Vec<Stmt>) {
    if matches!(stmts.last(), Some(Stmt::Continue)) {
        stmts.pop();
    }
}

fn strip_trailing_void_return(stmts: &mut Vec<Stmt>) {
    if matches!(stmts.last(), Some(Stmt::Return(None))) {
        stmts.pop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::func::Visibility;
    use crate::ir::structurize::structurize;
    use crate::ir::ty::FunctionSig;

    #[test]
    fn simple_add_inlines() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("add", sig, Visibility::Public);
        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));
        let func = fb.build();

        let shape = Shape::Block(func.entry);
        let ast = lower_function(&func, &shape);

        assert_eq!(ast.name, "add");
        assert_eq!(ast.params.len(), 2);
        // Single-use sum should be inlined into return.
        assert_eq!(ast.body.len(), 1);
        assert!(matches!(&ast.body[0], Stmt::Return(Some(Expr::Binary { op: BinOp::Add, .. }))));
    }

    #[test]
    fn constant_inlining() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let a = fb.param(0);
        let c = fb.const_int(42);
        let sum = fb.add(a, c);
        fb.ret(Some(sum));
        let func = fb.build();

        let shape = Shape::Block(func.entry);
        let ast = lower_function(&func, &shape);

        // Constant and single-use sum both inlined into return.
        assert_eq!(ast.body.len(), 1);
        match &ast.body[0] {
            Stmt::Return(Some(Expr::Binary { rhs, .. })) => {
                assert!(matches!(rhs.as_ref(), Expr::Literal(Constant::Int(42))));
            }
            other => panic!("Expected return with binary, got: {other:?}"),
        }
    }

    #[test]
    fn if_else_lowering() {
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("choose", sig, Visibility::Public);
        let cond = fb.param(0);
        let x = fb.param(1);
        let y = fb.param(2);

        let (then_block, then_vals) = fb.create_block_with_params(&[Type::Int(64)]);
        let (else_block, else_vals) = fb.create_block_with_params(&[Type::Int(64)]);

        fb.br_if(cond, then_block, &[x], else_block, &[y]);

        fb.switch_to_block(then_block);
        fb.ret(Some(then_vals[0]));

        fb.switch_to_block(else_block);
        fb.ret(Some(else_vals[0]));

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let ast = lower_function(&func, &shape);

        // Should produce block param decl + if/else (as ternary assign or full if/else).
        assert!(!ast.body.is_empty());
    }

    #[test]
    fn dead_pure_eliminated() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let a = fb.param(0);
        let b = fb.param(1);
        let _dead = fb.add(a, b); // unused
        fb.ret(None);
        let func = fb.build();

        let shape = Shape::Block(func.entry);
        let ast = lower_function(&func, &shape);

        // Dead add should be eliminated, trailing void return stripped.
        assert!(ast.body.is_empty(), "Expected empty body, got: {:?}", ast.body);
    }
}
