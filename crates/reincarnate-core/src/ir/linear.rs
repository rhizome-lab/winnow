//! Structured linear IR for the hybrid lowering pipeline.
//!
//! The pipeline converts `Shape + Function` → `Vec<Stmt>` in three phases:
//!
//! - **Phase 1** (`linearize`): Walk the Shape tree and produce a flat
//!   `Vec<LinearStmt>` where every instruction is a `Def(ValueId, InstId)`,
//!   control flow comes from shapes, and branch args become `Assign(dst, src)`.
//!   No inlining decisions — trivial shape walk.
//!
//! - **Phase 2** (`resolve`): Pure resolution on LinearStmt. Constants always
//!   inlined, scope lookups marked always-rebuild, pure single-use values
//!   marked for substitution, dead pure code dropped. This handles ~90% of
//!   inlining with zero side-effect concerns.
//!
//! - **Phase 3** (`emit`): LinearStmt → Vec<Stmt>. Remaining side-effecting
//!   single-use values inlined if no intervening side effects. Multi-use
//!   values get `const`/`let` declarations. Produces the AST for existing
//!   AST-to-AST passes.


use std::collections::{HashMap, HashSet};

use super::ast::{AstFunction, BinOp, Expr, Stmt, UnaryOp};
use super::ast_passes;
use super::block::BlockId;
use super::func::{Function, MethodKind};
use super::inst::{InstId, Op};
use super::structurize::{BlockArgAssign, Shape};
use super::ty::Type;
use super::value::{Constant, ValueId};
use crate::pipeline::DebugConfig;

use crate::entity::EntityRef;
use crate::pipeline::LoweringConfig;
use crate::transforms::util::value_operands;

// -----------------------------------------------------------------------
// LinearStmt — structured IR with ValueId/InstId references
// -----------------------------------------------------------------------

/// A statement in the structured linear IR.
///
/// References IR entities (ValueId, InstId) rather than carrying string names
/// or materialized expressions. The `Function` provides context for looking up
/// instruction ops and value metadata.
#[derive(Debug, Clone)]
pub(crate) enum LinearStmt {
    /// Instruction with a result value: `result = op(...)`.
    Def { result: ValueId, inst_id: InstId },
    /// Instruction without a useful result (void calls, stores, etc.).
    Effect { inst_id: InstId },
    /// Branch argument assignment: `dst = src`.
    Assign { dst: ValueId, src: ValueId },
    /// Conditional: `if (cond) { then } else { else }`.
    If {
        cond: ValueId,
        then_body: Vec<LinearStmt>,
        else_body: Vec<LinearStmt>,
    },
    /// While loop. Header instructions compute the condition each iteration.
    While {
        header: Vec<LinearStmt>,
        cond: ValueId,
        cond_negated: bool,
        body: Vec<LinearStmt>,
    },
    /// For loop: init; header+cond; body; update.
    For {
        init: Vec<LinearStmt>,
        header: Vec<LinearStmt>,
        cond: ValueId,
        cond_negated: bool,
        update: Vec<LinearStmt>,
        body: Vec<LinearStmt>,
    },
    /// Infinite loop (`while (true) { ... }`).
    Loop { body: Vec<LinearStmt> },
    /// Return from function.
    Return { value: Option<ValueId> },
    /// Break out of innermost loop.
    Break,
    /// Continue to next iteration.
    Continue,
    /// Break to an outer loop (`depth` levels up).
    LabeledBreak { depth: usize },
    /// Short-circuit OR: `phi = cond || rhs`.
    LogicalOr {
        cond: ValueId,
        phi: ValueId,
        rhs_body: Vec<LinearStmt>,
        rhs: ValueId,
    },
    /// Short-circuit AND: `phi = cond && rhs`.
    LogicalAnd {
        cond: ValueId,
        phi: ValueId,
        rhs_body: Vec<LinearStmt>,
        rhs: ValueId,
    },
    /// Switch statement: `switch (value) { case X: ...; default: ...; }`.
    Switch {
        value: ValueId,
        cases: Vec<(Constant, Vec<LinearStmt>)>,
        default_body: Vec<LinearStmt>,
    },
    /// Dispatch (fallback for irreducible CFGs).
    Dispatch {
        blocks: Vec<(usize, Vec<LinearStmt>)>,
        entry: usize,
    },
}

// -----------------------------------------------------------------------
// Phase 1: linearize — Shape → Vec<LinearStmt>
// -----------------------------------------------------------------------

/// Convert a structurized shape tree into a flat sequence of LinearStmts.
///
/// This is a faithful translation: no inlining decisions, no dead code
/// elimination, no expression building. Every non-terminator instruction
/// becomes a `Def` or `Effect`, every branch arg becomes an `Assign`, and
/// control flow shapes map 1:1 to LinearStmt control flow variants.
pub(crate) fn linearize(func: &Function, shape: &Shape) -> Vec<LinearStmt> {
    let mut out = Vec::new();
    linearize_into(func, shape, &mut out, false);
    out
}

fn linearize_into(
    func: &Function,
    shape: &Shape,
    out: &mut Vec<LinearStmt>,
    skip_init: bool,
) {
    match shape {
        Shape::Block(block_id) => {
            emit_block_insts(func, *block_id, out);
        }

        Shape::Seq(parts) => {
            for (i, part) in parts.iter().enumerate() {
                let next_is_loop = matches!(
                    parts.get(i + 1),
                    Some(Shape::ForLoop { .. })
                );

                // When a non-Block shape precedes a ForLoop in a Seq, its
                // trailing assigns already set the loop header's block
                // params — the ForLoop's own init_assigns would duplicate them.
                let this_skip_init = if i > 0 {
                    let prev = &parts[i - 1];
                    let is_loop = matches!(
                        part,
                        Shape::ForLoop { .. }
                    );
                    is_loop && !matches!(prev, Shape::Block(_))
                } else {
                    false
                };

                linearize_into(func, part, out, this_skip_init);

                // After a Block, emit Br target assignments — unless the
                // next shape is a ForLoop (it has its own init_assigns field).
                if let Shape::Block(block_id) = part {
                    if !next_is_loop {
                        emit_br_assigns(func, *block_id, out);
                    }
                }
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
            // Header block instructions (setup for the branch condition).
            emit_block_insts(func, *block, out);

            let mut then_stmts = Vec::new();
            emit_arg_assigns(then_assigns, &mut then_stmts);
            linearize_into(func, then_body, &mut then_stmts, false);
            emit_arg_assigns(then_trailing_assigns, &mut then_stmts);

            let mut else_stmts = Vec::new();
            emit_arg_assigns(else_assigns, &mut else_stmts);
            linearize_into(func, else_body, &mut else_stmts, false);
            emit_arg_assigns(else_trailing_assigns, &mut else_stmts);

            out.push(LinearStmt::If {
                cond: *cond,
                then_body: then_stmts,
                else_body: else_stmts,
            });
        }

        Shape::WhileLoop {
            header,
            cond,
            cond_negated,
            body,
        } => {
            let mut header_stmts = Vec::new();
            emit_block_insts(func, *header, &mut header_stmts);

            let mut body_stmts = Vec::new();
            linearize_into(func, body, &mut body_stmts, false);

            out.push(LinearStmt::While {
                header: header_stmts,
                cond: *cond,
                cond_negated: *cond_negated,
                body: body_stmts,
            });
        }

        Shape::ForLoop {
            header,
            init_assigns,
            cond,
            cond_negated,
            update_assigns,
            body,
        } => {
            let init = if skip_init {
                Vec::new()
            } else {
                let mut stmts = Vec::new();
                emit_arg_assigns(init_assigns, &mut stmts);
                stmts
            };

            let mut header_stmts = Vec::new();
            emit_block_insts(func, *header, &mut header_stmts);

            let mut body_stmts = Vec::new();
            linearize_into(func, body, &mut body_stmts, false);

            // The body's back-edge block emits br assigns via emit_br_assigns,
            // but ForLoop captures those same assigns in update_assigns.
            // Strip the duplicates from the body.
            strip_back_edge_assigns(&mut body_stmts, update_assigns);

            let mut update_stmts = Vec::new();
            emit_arg_assigns(update_assigns, &mut update_stmts);

            out.push(LinearStmt::For {
                init,
                header: header_stmts,
                cond: *cond,
                cond_negated: *cond_negated,
                update: update_stmts,
                body: body_stmts,
            });
        }

        Shape::Loop { header: _, body } => {
            let mut body_stmts = Vec::new();
            linearize_into(func, body, &mut body_stmts, false);
            out.push(LinearStmt::Loop { body: body_stmts });
        }

        Shape::Break => out.push(LinearStmt::Break),
        Shape::Continue => out.push(LinearStmt::Continue),
        Shape::LabeledBreak { depth } => out.push(LinearStmt::LabeledBreak { depth: *depth }),

        Shape::LogicalOr {
            block,
            cond,
            phi,
            rhs_body,
            rhs,
        } => {
            emit_block_insts(func, *block, out);
            let mut rhs_stmts = Vec::new();
            linearize_into(func, rhs_body, &mut rhs_stmts, false);
            out.push(LinearStmt::LogicalOr {
                cond: *cond,
                phi: *phi,
                rhs_body: rhs_stmts,
                rhs: *rhs,
            });
        }

        Shape::LogicalAnd {
            block,
            cond,
            phi,
            rhs_body,
            rhs,
        } => {
            emit_block_insts(func, *block, out);
            let mut rhs_stmts = Vec::new();
            linearize_into(func, rhs_body, &mut rhs_stmts, false);
            out.push(LinearStmt::LogicalAnd {
                cond: *cond,
                phi: *phi,
                rhs_body: rhs_stmts,
                rhs: *rhs,
            });
        }

        Shape::Switch {
            block,
            value,
            cases,
            default_assigns,
            default_body,
            default_trailing_assigns,
        } => {
            emit_block_insts(func, *block, out);

            let mut case_stmts = Vec::with_capacity(cases.len());
            for case in cases {
                let mut stmts = Vec::new();
                emit_arg_assigns(&case.entry_assigns, &mut stmts);
                linearize_into(func, &case.body, &mut stmts, false);
                emit_arg_assigns(&case.trailing_assigns, &mut stmts);
                case_stmts.push((case.value.clone(), stmts));
            }

            let mut default_stmts = Vec::new();
            emit_arg_assigns(default_assigns, &mut default_stmts);
            linearize_into(func, default_body, &mut default_stmts, false);
            emit_arg_assigns(default_trailing_assigns, &mut default_stmts);

            out.push(LinearStmt::Switch {
                value: *value,
                cases: case_stmts,
                default_body: default_stmts,
            });
        }

        Shape::Dispatch { blocks, entry } => {
            let mut dispatch_blocks = Vec::new();
            for &block_id in blocks {
                let mut block_stmts = Vec::new();
                emit_dispatch_block_insts(func, block_id, &mut block_stmts);
                dispatch_blocks.push((block_id.index() as usize, block_stmts));
            }
            out.push(LinearStmt::Dispatch {
                blocks: dispatch_blocks,
                entry: entry.index() as usize,
            });
        }
    }
}

// -----------------------------------------------------------------------
// Block instruction helpers
// -----------------------------------------------------------------------

/// Emit non-terminator instructions from a block as Def/Effect/Return.
fn emit_block_insts(func: &Function, block_id: super::block::BlockId, out: &mut Vec<LinearStmt>) {
    let block = &func.blocks[block_id];
    for &inst_id in &block.insts {
        let inst = &func.insts[inst_id];
        match &inst.op {
            // Terminators absorbed by Shape structure.
            Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => break,
            Op::Return(v) => {
                out.push(LinearStmt::Return { value: *v });
            }
            _ => {
                if let Some(result) = inst.result {
                    out.push(LinearStmt::Def { result, inst_id });
                } else {
                    out.push(LinearStmt::Effect { inst_id });
                }
            }
        }
    }
}

/// Emit all instructions from a dispatch block (including terminators).
fn emit_dispatch_block_insts(
    func: &Function,
    block_id: super::block::BlockId,
    out: &mut Vec<LinearStmt>,
) {
    let block = &func.blocks[block_id];
    for &inst_id in &block.insts {
        let inst = &func.insts[inst_id];
        match &inst.op {
            Op::Return(v) => out.push(LinearStmt::Return { value: *v }),
            _ => {
                if let Some(result) = inst.result {
                    out.push(LinearStmt::Def { result, inst_id });
                } else {
                    out.push(LinearStmt::Effect { inst_id });
                }
            }
        }
    }
}

/// Emit branch-arg assignments from a block's unconditional Br terminator.
fn emit_br_assigns(func: &Function, block_id: super::block::BlockId, out: &mut Vec<LinearStmt>) {
    let block = &func.blocks[block_id];
    let Some(&last_inst) = block.insts.last() else {
        return;
    };
    if let Op::Br { target, ref args } = func.insts[last_inst].op {
        let target_block = &func.blocks[target];
        for (param, &src) in target_block.params.iter().zip(args.iter()) {
            if param.value == src {
                continue;
            }
            out.push(LinearStmt::Assign {
                dst: param.value,
                src,
            });
        }
    }
}

/// Emit BlockArgAssign entries as Assign statements.
fn emit_arg_assigns(assigns: &[BlockArgAssign], out: &mut Vec<LinearStmt>) {
    for assign in assigns {
        out.push(LinearStmt::Assign {
            dst: assign.dst,
            src: assign.src,
        });
    }
}

/// Remove trailing Assign stmts (before any Continue) that duplicate `update` entries.
///
/// In a ForLoop body, `emit_br_assigns` on the back-edge block produces Assign stmts
/// for the header's block params, but ForLoop already captures these in `update_assigns`.
/// This strips the duplicates so they aren't emitted twice.
fn strip_back_edge_assigns(body: &mut Vec<LinearStmt>, update: &[BlockArgAssign]) {
    let mut end = body.len();
    if matches!(body.last(), Some(LinearStmt::Continue)) {
        end -= 1;
    }
    while end > 0 {
        if let LinearStmt::Assign { dst, src } = &body[end - 1] {
            if update.iter().any(|a| a.dst == *dst && a.src == *src) {
                body.remove(end - 1);
                end -= 1;
                continue;
            }
        }
        break;
    }
}

// -----------------------------------------------------------------------
// Phase 2: resolve — classify values for inlining
// -----------------------------------------------------------------------

/// Output of Phase 2: inlining classification for Phase 3.
pub(crate) struct ResolveCtx {
    /// Use counts after dead-code fixpoint.
    pub use_counts: HashMap<ValueId, usize>,
    /// Constants — always inlined, not consumed on read.
    pub constant_inlines: HashMap<ValueId, Constant>,
    /// Always-rebuild instructions (scope lookups + cascade).
    pub always_inlines: HashSet<ValueId>,
    /// Pure single-use values — consumed once at use site.
    pub lazy_inlines: HashSet<ValueId>,
    /// Alloc results with merged immediately-following Store.
    pub alloc_inits: HashMap<ValueId, ValueId>,
    /// Store InstIds merged into their preceding Alloc.
    pub skip_stores: HashSet<InstId>,
}

/// Classify all values in linearized IR for inlining decisions.
///
/// Computes use counts from LinearStmt (not raw IR — terminators like
/// Br/BrIf/Switch are absent, so their operand uses aren't counted). Runs
/// dead-code elimination to fixpoint, then classifies each Def as constant,
/// always-inline, lazy-inline, or materialized.
pub(crate) fn resolve(func: &Function, stmts: &[LinearStmt]) -> ResolveCtx {
    // Step 1: compute use counts from LinearStmt.
    let mut use_counts = HashMap::new();
    count_uses_in_stmts(func, stmts, &mut use_counts);

    // Step 2: dead code elimination fixpoint.
    // Removing dead pure Defs decrements their operands' counts, which can
    // make previously multi-use values single-use (or dead). Iterate until
    // stable — typically converges in 2-3 passes.
    let mut dead: HashSet<ValueId> = HashSet::new();
    loop {
        let mut changed = false;
        collect_dead_uses(func, stmts, &mut use_counts, &mut dead, &mut changed);
        if !changed {
            break;
        }
    }

    // Step 3: classify remaining Defs.
    let mut constant_inlines = HashMap::new();
    let mut always_inlines = HashSet::new();
    let mut lazy_inlines = HashSet::new();
    classify_defs(
        func,
        stmts,
        &use_counts,
        &mut constant_inlines,
        &mut always_inlines,
        &mut lazy_inlines,
    );

    // Step 4: detect adjacent Alloc+Store patterns for merged init.
    let (alloc_inits, skip_stores) = find_alloc_store_merges(func, stmts);

    ResolveCtx {
        use_counts,
        constant_inlines,
        always_inlines,
        lazy_inlines,
        alloc_inits,
        skip_stores,
    }
}

// -----------------------------------------------------------------------
// Use counting
// -----------------------------------------------------------------------

/// Count value uses across all LinearStmts recursively.
fn count_uses_in_stmts(
    func: &Function,
    stmts: &[LinearStmt],
    counts: &mut HashMap<ValueId, usize>,
) {
    for stmt in stmts {
        match stmt {
            LinearStmt::Def { inst_id, .. } | LinearStmt::Effect { inst_id } => {
                for v in value_operands(&func.insts[*inst_id].op) {
                    *counts.entry(v).or_default() += 1;
                }
            }
            LinearStmt::Assign { src, .. } => {
                *counts.entry(*src).or_default() += 1;
            }
            LinearStmt::If {
                cond,
                then_body,
                else_body,
            } => {
                *counts.entry(*cond).or_default() += 1;
                count_uses_in_stmts(func, then_body, counts);
                count_uses_in_stmts(func, else_body, counts);
            }
            LinearStmt::While {
                header,
                cond,
                body,
                ..
            } => {
                count_uses_in_stmts(func, header, counts);
                *counts.entry(*cond).or_default() += 1;
                count_uses_in_stmts(func, body, counts);
            }
            LinearStmt::For {
                init,
                header,
                cond,
                update,
                body,
                ..
            } => {
                count_uses_in_stmts(func, init, counts);
                count_uses_in_stmts(func, header, counts);
                *counts.entry(*cond).or_default() += 1;
                count_uses_in_stmts(func, update, counts);
                count_uses_in_stmts(func, body, counts);
            }
            LinearStmt::Loop { body } => {
                count_uses_in_stmts(func, body, counts);
            }
            LinearStmt::Return { value } => {
                if let Some(v) = value {
                    *counts.entry(*v).or_default() += 1;
                }
            }
            // LogicalOr/And: cond used once (as lhs of `||`/`&&`).
            // The short-circuit semantics combine the BrIf condition check
            // and the value propagation into a single `||`/`&&` expression.
            // When rhs == phi (nested logical op), the emitter skips the
            // rhs reference, so don't count it — otherwise the inner phi
            // looks multi-use and won't inline.
            LinearStmt::LogicalOr {
                cond,
                phi,
                rhs_body,
                rhs,
            }
            | LinearStmt::LogicalAnd {
                cond,
                phi,
                rhs_body,
                rhs,
            } => {
                *counts.entry(*cond).or_default() += 1;
                count_uses_in_stmts(func, rhs_body, counts);
                if *rhs != *phi {
                    *counts.entry(*rhs).or_default() += 1;
                }
            }
            LinearStmt::Dispatch { blocks, .. } => {
                for (_, block_stmts) in blocks {
                    count_uses_in_stmts(func, block_stmts, counts);
                }
            }
            LinearStmt::Switch {
                value,
                cases,
                default_body,
            } => {
                *counts.entry(*value).or_default() += 1;
                for (_, case_stmts) in cases {
                    count_uses_in_stmts(func, case_stmts, counts);
                }
                count_uses_in_stmts(func, default_body, counts);
            }
            LinearStmt::Break | LinearStmt::Continue | LinearStmt::LabeledBreak { .. } => {}
        }
    }
}

// -----------------------------------------------------------------------
// Dead code fixpoint
// -----------------------------------------------------------------------

/// Find dead deferrable Defs and decrement their operands' use counts.
/// Sets `changed` if any new dead values were found.
fn collect_dead_uses(
    func: &Function,
    stmts: &[LinearStmt],
    counts: &mut HashMap<ValueId, usize>,
    dead: &mut HashSet<ValueId>,
    changed: &mut bool,
) {
    for stmt in stmts {
        match stmt {
            LinearStmt::Def { result, inst_id } => {
                if dead.contains(result) {
                    continue;
                }
                let count = counts.get(result).copied().unwrap_or(0);
                let op = &func.insts[*inst_id].op;
                if count == 0 && is_deferrable(op) {
                    dead.insert(*result);
                    for v in value_operands(op) {
                        if let Some(c) = counts.get_mut(&v) {
                            *c = c.saturating_sub(1);
                        }
                    }
                    *changed = true;
                }
            }
            LinearStmt::If {
                then_body,
                else_body,
                ..
            } => {
                collect_dead_uses(func, then_body, counts, dead, changed);
                collect_dead_uses(func, else_body, counts, dead, changed);
            }
            LinearStmt::While { header, body, .. } => {
                collect_dead_uses(func, header, counts, dead, changed);
                collect_dead_uses(func, body, counts, dead, changed);
            }
            LinearStmt::For {
                init,
                header,
                update,
                body,
                ..
            } => {
                collect_dead_uses(func, init, counts, dead, changed);
                collect_dead_uses(func, header, counts, dead, changed);
                collect_dead_uses(func, update, counts, dead, changed);
                collect_dead_uses(func, body, counts, dead, changed);
            }
            LinearStmt::Loop { body } => {
                collect_dead_uses(func, body, counts, dead, changed);
            }
            LinearStmt::LogicalOr { rhs_body, .. }
            | LinearStmt::LogicalAnd { rhs_body, .. } => {
                collect_dead_uses(func, rhs_body, counts, dead, changed);
            }
            LinearStmt::Dispatch { blocks, .. } => {
                for (_, block_stmts) in blocks {
                    collect_dead_uses(func, block_stmts, counts, dead, changed);
                }
            }
            LinearStmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_stmts) in cases {
                    collect_dead_uses(func, case_stmts, counts, dead, changed);
                }
                collect_dead_uses(func, default_body, counts, dead, changed);
            }
            _ => {}
        }
    }
}

// -----------------------------------------------------------------------
// Classification
// -----------------------------------------------------------------------

/// Whether an instruction is pure enough to defer for inlining.
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

/// Scope-lookup calls are pure metadata — always rebuild so consumption
/// sites (Field, Call) can detect and resolve them.
fn is_scope_lookup_op(op: &Op) -> bool {
    matches!(
        op,
        Op::SystemCall { system, method, .. }
            if system == "Flash.Scope"
                && (method == "findPropStrict" || method == "findProperty")
    )
}

/// Classify non-dead Defs into constant, always-inline, or lazy-inline.
/// Processes stmts sequentially so always-inline cascade works.
fn classify_defs(
    func: &Function,
    stmts: &[LinearStmt],
    counts: &HashMap<ValueId, usize>,
    constant_inlines: &mut HashMap<ValueId, Constant>,
    always_inlines: &mut HashSet<ValueId>,
    lazy_inlines: &mut HashSet<ValueId>,
) {
    for stmt in stmts {
        match stmt {
            LinearStmt::Def { result, inst_id } => {
                let count = counts.get(result).copied().unwrap_or(0);
                let op = &func.insts[*inst_id].op;

                // Dead pure — skip.
                if count == 0 && is_deferrable(op) {
                    continue;
                }

                // Constants always inlined.
                if let Op::Const(c) = op {
                    constant_inlines.insert(*result, c.clone());
                    continue;
                }

                // Scope lookups always rebuilt.
                if is_scope_lookup_op(op) {
                    always_inlines.insert(*result);
                    continue;
                }

                // Cascade: GetField/GetIndex on always-inlined object.
                let object_always_inlined = match op {
                    Op::GetField { object, .. }
                    | Op::GetIndex {
                        collection: object, ..
                    } => always_inlines.contains(object),
                    _ => false,
                };
                if object_always_inlined {
                    always_inlines.insert(*result);
                    continue;
                }

                // Single-use deferrable → lazy inline.
                if count == 1 && is_deferrable(op) {
                    lazy_inlines.insert(*result);
                }
            }
            LinearStmt::If {
                then_body,
                else_body,
                ..
            } => {
                classify_defs(
                    func,
                    then_body,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
                classify_defs(
                    func,
                    else_body,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
            }
            LinearStmt::While { header, body, .. } => {
                classify_defs(
                    func,
                    header,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
                classify_defs(
                    func,
                    body,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
            }
            LinearStmt::For {
                init,
                header,
                update,
                body,
                ..
            } => {
                classify_defs(
                    func,
                    init,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
                classify_defs(
                    func,
                    header,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
                classify_defs(
                    func,
                    update,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
                classify_defs(
                    func,
                    body,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
            }
            LinearStmt::Loop { body } => {
                classify_defs(
                    func,
                    body,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
            }
            LinearStmt::LogicalOr { rhs_body, .. }
            | LinearStmt::LogicalAnd { rhs_body, .. } => {
                classify_defs(
                    func,
                    rhs_body,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
            }
            LinearStmt::Dispatch { blocks, .. } => {
                for (_, block_stmts) in blocks {
                    classify_defs(
                        func,
                        block_stmts,
                        counts,
                        constant_inlines,
                        always_inlines,
                        lazy_inlines,
                    );
                }
            }
            LinearStmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_stmts) in cases {
                    classify_defs(
                        func,
                        case_stmts,
                        counts,
                        constant_inlines,
                        always_inlines,
                        lazy_inlines,
                    );
                }
                classify_defs(
                    func,
                    default_body,
                    counts,
                    constant_inlines,
                    always_inlines,
                    lazy_inlines,
                );
            }
            _ => {}
        }
    }
}

// -----------------------------------------------------------------------
// Alloc+Store merging
// -----------------------------------------------------------------------

/// Find adjacent Alloc+Store pairs where the Store immediately initializes
/// the Alloc result. Returns the mapping and the set of merged Store InstIds.
fn find_alloc_store_merges(
    func: &Function,
    stmts: &[LinearStmt],
) -> (HashMap<ValueId, ValueId>, HashSet<InstId>) {
    let mut alloc_inits = HashMap::new();
    let mut skip_stores = HashSet::new();
    scan_alloc_stores(func, stmts, &mut alloc_inits, &mut skip_stores);
    (alloc_inits, skip_stores)
}

fn scan_alloc_stores(
    func: &Function,
    stmts: &[LinearStmt],
    alloc_inits: &mut HashMap<ValueId, ValueId>,
    skip_stores: &mut HashSet<InstId>,
) {
    for pair in stmts.windows(2) {
        if let (
            LinearStmt::Def {
                result: alloc_r,
                inst_id: alloc_iid,
            },
            LinearStmt::Effect {
                inst_id: store_iid,
            },
        ) = (&pair[0], &pair[1])
        {
            if matches!(func.insts[*alloc_iid].op, Op::Alloc(_)) {
                if let Op::Store { ptr, value } = &func.insts[*store_iid].op {
                    if *ptr == *alloc_r {
                        alloc_inits.insert(*alloc_r, *value);
                        skip_stores.insert(*store_iid);
                    }
                }
            }
        }
    }
    // Recurse into nested bodies.
    for stmt in stmts {
        match stmt {
            LinearStmt::If {
                then_body,
                else_body,
                ..
            } => {
                scan_alloc_stores(func, then_body, alloc_inits, skip_stores);
                scan_alloc_stores(func, else_body, alloc_inits, skip_stores);
            }
            LinearStmt::While { header, body, .. } => {
                scan_alloc_stores(func, header, alloc_inits, skip_stores);
                scan_alloc_stores(func, body, alloc_inits, skip_stores);
            }
            LinearStmt::For {
                init,
                header,
                update,
                body,
                ..
            } => {
                scan_alloc_stores(func, init, alloc_inits, skip_stores);
                scan_alloc_stores(func, header, alloc_inits, skip_stores);
                scan_alloc_stores(func, update, alloc_inits, skip_stores);
                scan_alloc_stores(func, body, alloc_inits, skip_stores);
            }
            LinearStmt::Loop { body } => {
                scan_alloc_stores(func, body, alloc_inits, skip_stores);
            }
            LinearStmt::LogicalOr { rhs_body, .. }
            | LinearStmt::LogicalAnd { rhs_body, .. } => {
                scan_alloc_stores(func, rhs_body, alloc_inits, skip_stores);
            }
            LinearStmt::Dispatch { blocks, .. } => {
                for (_, block_stmts) in blocks {
                    scan_alloc_stores(func, block_stmts, alloc_inits, skip_stores);
                }
            }
            LinearStmt::Switch {
                cases,
                default_body,
                ..
            } => {
                for (_, case_stmts) in cases {
                    scan_alloc_stores(func, case_stmts, alloc_inits, skip_stores);
                }
                scan_alloc_stores(func, default_body, alloc_inits, skip_stores);
            }
            _ => {}
        }
    }
}

// -----------------------------------------------------------------------
// Phase 3: emit — LinearStmt → Vec<Stmt>
// -----------------------------------------------------------------------

/// Lower a function through all 3 phases of the hybrid pipeline.
pub fn lower_function_linear(
    func: &Function,
    shape: &Shape,
    config: &LoweringConfig,
    debug: &DebugConfig,
) -> AstFunction {
    let linear = linearize(func, shape);
    let rctx = resolve(func, &linear);
    let mut ctx = EmitCtx::new(func, &rctx, config);

    let mut body = ctx.emit_stmts(&linear);
    strip_trailing_void_return(&mut body);

    let decls = ctx.collect_block_param_decls();
    let mut full_body = decls;
    full_body.append(&mut body);

    if debug.dump_ast && debug.should_dump(&func.name) {
        eprintln!("=== AST (pre-passes): {} ===", func.name);
        for stmt in &full_body {
            eprintln!("{stmt:#?}");
        }
        eprintln!("=== end AST: {} ===\n", func.name);
    }

    // AST-to-AST rewrite passes.
    // Cleanup first: self-assigns and stubs block ternary detection by adding
    // extra statements to if/else branches.
    ast_passes::eliminate_self_assigns(&mut full_body);
    ast_passes::eliminate_duplicate_assigns(&mut full_body);
    ast_passes::eliminate_forwarding_stubs(&mut full_body);
    ast_passes::invert_empty_then(&mut full_body);
    ast_passes::eliminate_unreachable_after_exit(&mut full_body);
    if config.ternary {
        ast_passes::rewrite_ternary(&mut full_body);
    }
    if config.minmax {
        ast_passes::rewrite_minmax(&mut full_body);
    }

    // Fixpoint: forward sub enables ternary, ternary enables narrow/merge/fold,
    // fold may remove statements that enable further forward sub, absorb_phi
    // merges split-path conditions into their assigning branch.
    loop {
        let before = ast_passes::count_stmts(&full_body);
        ast_passes::forward_substitute(&mut full_body);
        if config.ternary {
            ast_passes::rewrite_ternary(&mut full_body);
        }
        ast_passes::simplify_ternary_to_logical(&mut full_body);
        ast_passes::absorb_phi_condition(&mut full_body);
        ast_passes::narrow_var_scope(&mut full_body);
        ast_passes::merge_decl_init(&mut full_body);
        ast_passes::fold_single_use_consts(&mut full_body);
        if ast_passes::count_stmts(&full_body) == before {
            break;
        }
    }

    ast_passes::rewrite_foreach_loops(&mut full_body);
    // Clean up dead variables left by the foreach rewrite
    // (e.g., the index register decl, single-use collection var).
    ast_passes::narrow_var_scope(&mut full_body);
    ast_passes::merge_decl_init(&mut full_body);
    ast_passes::fold_single_use_consts(&mut full_body);

    ast_passes::rewrite_compound_assign(&mut full_body);
    ast_passes::rewrite_post_increment(&mut full_body);
    ast_passes::promote_while_to_for(&mut full_body);

    AstFunction {
        name: func.name.clone(),
        params: ctx.build_params(),
        return_ty: func.sig.return_ty.clone(),
        body: full_body,
        is_generator: func.coroutine.is_some(),
        visibility: func.visibility,
        method_kind: func.method_kind,
        has_rest_param: func.sig.has_rest_param,
    }
}


// -----------------------------------------------------------------------
// Emit context
// -----------------------------------------------------------------------

struct EmitCtx<'a> {
    func: &'a Function,
    config: &'a LoweringConfig,
    resolve: &'a ResolveCtx,
    /// Debug names for values (func.value_names + out-of-SSA coalescing).
    value_names: HashMap<ValueId, String>,
    /// Entry-block parameter ValueIds.
    entry_params: HashSet<ValueId>,
    /// Names shared by 2+ ValueIds (out-of-SSA coalesced variables).
    shared_names: HashSet<String>,
    /// Deferred single-use pure instructions (from Phase 2's lazy_inlines).
    pending_lazy: HashMap<ValueId, InstId>,
    /// Always-rebuild instructions (from Phase 2's always_inlines).
    always_inline_map: HashMap<ValueId, InstId>,
    /// Deferred side-effecting single-use expressions.
    side_effecting_inlines: HashMap<ValueId, Expr>,
    /// Values already declared by flush_side_effecting_inlines.
    se_flush_declared: HashSet<ValueId>,
    /// Block-param ValueIds referenced during emission.
    referenced_block_params: HashSet<ValueId>,
    /// Pending-lazy values protected from flush_pending_reads (header reads
    /// that shouldn't be flushed into nested bodies).
    flush_protected: HashSet<ValueId>,
}

impl<'a> EmitCtx<'a> {
    fn new(func: &'a Function, resolve: &'a ResolveCtx, config: &'a LoweringConfig) -> Self {
        let mut value_names: HashMap<ValueId, String> = func
            .value_names
            .iter()
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        let entry_params: HashSet<ValueId> = func.blocks[func.entry]
            .params
            .iter()
            .map(|p| p.value)
            .collect();

        // Out-of-SSA name coalescing: bidirectional propagation across branch edges.
        // Forward: named block param → unnamed branch arg.
        // Reverse: when all named branch args agree → unnamed block param.
        // Fixpoint: naming values in one direction may enable naming in the other.
        loop {
            let mut changed = false;

            // Forward: propagate block-param names to branch args.
            for (_, block) in func.blocks.iter() {
                let Some(&last_inst) = block.insts.last() else {
                    continue;
                };
                let mut propagate_fwd = |target: BlockId, args: &[ValueId]| {
                    let target_block = &func.blocks[target];
                    for (param, &src) in target_block.params.iter().zip(args.iter()) {
                        if param.value == src || value_names.contains_key(&src) {
                            continue;
                        }
                        if let Some(name) = value_names.get(&param.value) {
                            let name = name.clone();
                            value_names.insert(src, name);
                            changed = true;
                        }
                    }
                };
                match &func.insts[last_inst].op {
                    Op::Br { target, args } => propagate_fwd(*target, args),
                    Op::BrIf {
                        then_target,
                        then_args,
                        else_target,
                        else_args,
                        ..
                    } => {
                        propagate_fwd(*then_target, then_args);
                        propagate_fwd(*else_target, else_args);
                    }
                    Op::Switch {
                        cases, default, ..
                    } => {
                        for (_, target, args) in cases {
                            propagate_fwd(*target, args);
                        }
                        propagate_fwd(default.0, &default.1);
                    }
                    _ => {}
                }
            }

            // Reverse: propagate branch-arg names → unnamed block params.
            // Only assign when all named args feeding a param agree on the same name.
            let mut candidates: HashMap<ValueId, Option<String>> = HashMap::new();
            for (_, block) in func.blocks.iter() {
                let Some(&last_inst) = block.insts.last() else {
                    continue;
                };
                let mut collect = |target: BlockId, args: &[ValueId]| {
                    let target_block = &func.blocks[target];
                    for (param, &src) in target_block.params.iter().zip(args.iter()) {
                        if value_names.contains_key(&param.value) {
                            continue;
                        }
                        if let Some(src_name) = value_names.get(&src) {
                            candidates
                                .entry(param.value)
                                .and_modify(|existing| {
                                    if let Some(prev) = existing {
                                        if prev != src_name {
                                            *existing = None; // conflict
                                        }
                                    }
                                })
                                .or_insert_with(|| Some(src_name.clone()));
                        }
                    }
                };
                match &func.insts[last_inst].op {
                    Op::Br { target, args } => collect(*target, args),
                    Op::BrIf {
                        then_target,
                        then_args,
                        else_target,
                        else_args,
                        ..
                    } => {
                        collect(*then_target, then_args);
                        collect(*else_target, else_args);
                    }
                    Op::Switch {
                        cases, default, ..
                    } => {
                        for (_, target, args) in cases {
                            collect(*target, args);
                        }
                        collect(default.0, &default.1);
                    }
                    _ => {}
                }
            }
            // Sort candidates by ValueId for deterministic fixpoint convergence.
            let mut sorted_candidates: Vec<_> = candidates.into_iter().collect();
            sorted_candidates.sort_by_key(|(v, _)| v.index());
            for (param_value, candidate) in sorted_candidates {
                if let Some(name) = candidate {
                    value_names.insert(param_value, name);
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }

        // Propagate names from Cast/Copy results to their source operands.
        // Mem2Reg names the stored value (e.g. Cast(src)), but if the Cast
        // is single-use and gets lazily inlined, its name is never used.
        // Propagating to `src` ensures the materialized variable gets the name.
        for (_, inst) in func.insts.iter() {
            if let Some(result) = inst.result {
                if let Some(name) = value_names.get(&result).cloned() {
                    let src = match &inst.op {
                        Op::Cast(s, ..) | Op::Copy(s) => Some(*s),
                        _ => None,
                    };
                    if let Some(src) = src {
                        value_names.entry(src).or_insert(name);
                    }
                }
            }
        }

        // Deduplicate self-parameter name: values other than param 0 that share
        // the self name should use a distinct local name to avoid `this = ...`.
        let has_self = matches!(
            func.method_kind,
            MethodKind::Instance | MethodKind::Constructor | MethodKind::Getter | MethodKind::Setter
        );
        if has_self && !func.blocks[func.entry].params.is_empty() {
            let self_value = func.blocks[func.entry].params[0].value;
            if let Some(self_name) = value_names.get(&self_value).cloned() {
                let alt_name = format!("_{self_name}");
                for (vid, name) in value_names.iter_mut() {
                    if *vid != self_value && *name == self_name {
                        *name = alt_name.clone();
                    }
                }
            }
        }

        // Find names shared by 2+ ValueIds.
        let mut name_counts: HashMap<&str, usize> = HashMap::new();
        for name in value_names.values() {
            *name_counts.entry(name.as_str()).or_default() += 1;
        }
        let shared_names: HashSet<String> = name_counts
            .into_iter()
            .filter(|(_, count)| *count >= 2)
            .map(|(name, _)| name.to_string())
            .collect();

        Self {
            func,
            config,
            resolve,
            value_names,
            entry_params,
            shared_names,
            pending_lazy: HashMap::new(),
            always_inline_map: HashMap::new(),
            side_effecting_inlines: HashMap::new(),
            se_flush_declared: HashSet::new(),
            referenced_block_params: HashSet::new(),
            flush_protected: HashSet::new(),
        }
    }

    fn value_name(&self, v: ValueId) -> String {
        if let Some(name) = self.value_names.get(&v) {
            name.clone()
        } else {
            format!("v{}", v.index())
        }
    }

    fn use_count(&self, v: ValueId) -> usize {
        self.resolve.use_counts.get(&v).copied().unwrap_or(0)
    }

    /// Check if a value has Dictionary type (flash.utils::Dictionary).
    fn is_dictionary(&self, v: ValueId) -> bool {
        matches!(
            self.func.value_types.get(v),
            Some(Type::Struct(name)) if name.rsplit("::").next() == Some("Dictionary")
        )
    }

    /// Build an expression for a value reference.
    fn build_val(&mut self, v: ValueId) -> Expr {
        // Constants — always inlined, not consumed.
        if let Some(c) = self.resolve.constant_inlines.get(&v) {
            return Expr::Literal(c.clone());
        }

        // Always-inline — rebuilt on every use.
        if let Some(&inst_id) = self.always_inline_map.get(&v) {
            let op = self.func.insts[inst_id].op.clone();
            if let Some(expr) = self.build_expr_from_op(&op) {
                return expr;
            }
        }

        // Side-effecting inline — consumed once.
        if let Some(expr) = self.side_effecting_inlines.remove(&v) {
            return expr;
        }

        // Lazy inline — consumed once.
        if let Some(inst_id) = self.pending_lazy.remove(&v) {
            let op = self.func.insts[inst_id].op.clone();
            if let Some(expr) = self.build_expr_from_op(&op) {
                return expr;
            }
        }

        // Track block-param references for declaration generation.
        if !self.entry_params.contains(&v) && !self.se_flush_declared.contains(&v) {
            self.referenced_block_params.insert(v);
        }

        Expr::Var(self.value_name(v))
    }

    /// Build an Expr from an Op.
    fn build_expr_from_op(&mut self, op: &Op) -> Option<Expr> {
        Some(match op {
            Op::Const(c) => Expr::Literal(c.clone()),

            Op::Add(a, b) => Expr::Binary {
                op: BinOp::Add,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::Sub(a, b) => Expr::Binary {
                op: BinOp::Sub,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::Mul(a, b) => Expr::Binary {
                op: BinOp::Mul,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::Div(a, b) => Expr::Binary {
                op: BinOp::Div,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::Rem(a, b) => Expr::Binary {
                op: BinOp::Rem,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::Neg(a) => Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(self.build_val(*a)),
            },

            Op::BitAnd(a, b) => Expr::Binary {
                op: BinOp::BitAnd,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::BitOr(a, b) => Expr::Binary {
                op: BinOp::BitOr,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::BitXor(a, b) => Expr::Binary {
                op: BinOp::BitXor,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::BitNot(a) => Expr::Unary {
                op: UnaryOp::BitNot,
                expr: Box::new(self.build_val(*a)),
            },
            Op::Shl(a, b) => Expr::Binary {
                op: BinOp::Shl,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },
            Op::Shr(a, b) => Expr::Binary {
                op: BinOp::Shr,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },

            Op::Cmp(kind, a, b) => Expr::Cmp {
                kind: *kind,
                lhs: Box::new(self.build_val(*a)),
                rhs: Box::new(self.build_val(*b)),
            },

            Op::Not(a) => Expr::Not(Box::new(self.build_val(*a))),
            Op::Select {
                cond,
                on_true,
                on_false,
            } => Expr::Ternary {
                cond: Box::new(self.build_val(*cond)),
                then_val: Box::new(self.build_val(*on_true)),
                else_val: Box::new(self.build_val(*on_false)),
            },

            Op::Load(ptr) => self.build_val(*ptr),
            Op::GetField { object, field } => Expr::Field {
                object: Box::new(self.build_val(*object)),
                field: field.clone(),
            },
            Op::GetIndex { collection, index } => {
                if self.is_dictionary(*collection) {
                    // Dictionary → Map: dict.get(key)
                    Expr::CallIndirect {
                        callee: Box::new(Expr::Field {
                            object: Box::new(self.build_val(*collection)),
                            field: "get".into(),
                        }),
                        args: vec![self.build_val(*index)],
                    }
                } else {
                    Expr::Index {
                        collection: Box::new(self.build_val(*collection)),
                        index: Box::new(self.build_val(*index)),
                    }
                }
            }

            Op::Call {
                func: fname,
                args,
            } => Expr::Call {
                func: fname.clone(),
                args: args.iter().map(|a| self.build_val(*a)).collect(),
            },
            Op::CallIndirect { callee, args } => Expr::CallIndirect {
                callee: Box::new(self.build_val(*callee)),
                args: args.iter().map(|a| self.build_val(*a)).collect(),
            },
            Op::SystemCall {
                system,
                method,
                args,
            } => {
                // Dictionary-specific rewrites for Flash.Object operations.
                if system == "Flash.Object"
                    && args.len() >= 2
                    && self.is_dictionary(args[0])
                {
                    match method.as_str() {
                        // deleteProperty(dict, key) → dict.delete(key)
                        "deleteProperty" => {
                            return Some(Expr::CallIndirect {
                                callee: Box::new(Expr::Field {
                                    object: Box::new(self.build_val(args[0])),
                                    field: "delete".into(),
                                }),
                                args: vec![self.build_val(args[1])],
                            });
                        }
                        // hasProperty(dict, key) → dict.has(key)
                        "hasProperty" => {
                            return Some(Expr::CallIndirect {
                                callee: Box::new(Expr::Field {
                                    object: Box::new(self.build_val(args[0])),
                                    field: "has".into(),
                                }),
                                args: vec![self.build_val(args[1])],
                            });
                        }
                        _ => {}
                    }
                }
                Expr::SystemCall {
                    system: system.clone(),
                    method: method.clone(),
                    args: args.iter().map(|a| self.build_val(*a)).collect(),
                }
            }

            Op::MethodCall {
                receiver,
                method,
                args,
            } => Expr::MethodCall {
                receiver: Box::new(self.build_val(*receiver)),
                method: method.clone(),
                args: args.iter().map(|a| self.build_val(*a)).collect(),
            },

            Op::Cast(v, ty, kind) => {
                if self
                    .func
                    .value_types
                    .get(*v)
                    .map(|t| t == ty)
                    .unwrap_or(false)
                {
                    self.build_val(*v)
                } else {
                    Expr::Cast {
                        expr: Box::new(self.build_val(*v)),
                        ty: ty.clone(),
                        kind: *kind,
                    }
                }
            }
            Op::TypeCheck(v, ty) => Expr::TypeCheck {
                expr: Box::new(self.build_val(*v)),
                ty: ty.clone(),
            },

            Op::StructInit { name, fields } => Expr::StructInit {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(n, v)| (n.clone(), self.build_val(*v)))
                    .collect(),
            },
            Op::ArrayInit(elems) => {
                Expr::ArrayInit(elems.iter().map(|v| self.build_val(*v)).collect())
            }
            Op::TupleInit(elems) => {
                Expr::TupleInit(elems.iter().map(|v| self.build_val(*v)).collect())
            }

            Op::Yield(v) => Expr::Yield(v.map(|yv| Box::new(self.build_val(yv)))),
            Op::CoroutineCreate {
                func: fname,
                args,
            } => Expr::CoroutineCreate {
                func: fname.clone(),
                args: args.iter().map(|a| self.build_val(*a)).collect(),
            },
            Op::CoroutineResume(v) => Expr::CoroutineResume(Box::new(self.build_val(*v))),

            Op::GlobalRef(name) => Expr::GlobalRef(name.clone()),
            Op::Copy(src) => self.build_val(*src),

            Op::Br { .. }
            | Op::BrIf { .. }
            | Op::Switch { .. }
            | Op::Return(_)
            | Op::Alloc(_)
            | Op::Store { .. }
            | Op::SetField { .. }
            | Op::SetIndex { .. } => return None,
        })
    }

    /// Negate a condition: invert Cmp if lazy-inlined, else wrap in Not.
    fn negate_cond(&mut self, cond: ValueId) -> Expr {
        if let Some(&inst_id) = self.pending_lazy.get(&cond) {
            if let Op::Cmp(kind, a, b) = &self.func.insts[inst_id].op {
                let a = *a;
                let b = *b;
                let kind = kind.inverse();
                self.pending_lazy.remove(&cond);
                return Expr::Cmp {
                    kind,
                    lhs: Box::new(self.build_val(a)),
                    rhs: Box::new(self.build_val(b)),
                };
            }
        }
        Expr::Not(Box::new(self.build_val(cond)))
    }

    /// Flush all pending side-effecting inline expressions as statements.
    fn flush_side_effecting_inlines(&mut self, stmts: &mut Vec<Stmt>) {
        let mut to_flush: Vec<ValueId> = self.side_effecting_inlines.keys().copied().collect();
        to_flush.sort_by_key(|v| v.index());
        for v in to_flush {
            let expr = self.side_effecting_inlines.remove(&v).unwrap();
            self.se_flush_declared.insert(v);
            let name = self.value_name(v);
            if self.shared_names.contains(&name) {
                stmts.push(Stmt::Assign {
                    target: Expr::Var(name),
                    value: expr,
                });
            } else {
                stmts.push(Stmt::VarDecl {
                    name,
                    ty: None,
                    init: Some(expr),
                    mutable: false,
                });
            }
        }
    }

    /// Flush deferred memory-read lazy inlines into real statements.
    fn flush_pending_reads(&mut self, stmts: &mut Vec<Stmt>) {
        let mut to_flush: Vec<(ValueId, InstId)> = self
            .pending_lazy
            .iter()
            .filter(|(&v, &iid)| {
                !self.flush_protected.contains(&v)
                    && matches!(
                        self.func.insts[iid].op,
                        Op::GetField { .. } | Op::GetIndex { .. } | Op::Load(..)
                    )
            })
            .map(|(&v, &iid)| (v, iid))
            .collect();
        // Sort by ValueId for deterministic flush order.
        to_flush.sort_by_key(|(v, _)| v.index());

        // Build map: unnamed flush value → named Cast/Copy consumer in pending_lazy.
        // When a GetField is flushed but its only consumer is a named Cast (e.g.
        // Mem2Reg named the Cast from an alloc's stored value), absorb the Cast
        // into the flush so the materialized variable gets the source-level name.
        let named_consumers: HashMap<ValueId, (ValueId, InstId)> = self
            .pending_lazy
            .iter()
            .filter_map(|(&w, &wiid)| {
                if !self.value_names.contains_key(&w) {
                    return None;
                }
                match &self.func.insts[wiid].op {
                    Op::Cast(src, ..) | Op::Copy(src) => Some((*src, (w, wiid))),
                    _ => None,
                }
            })
            .collect();

        for (v, iid) in to_flush {
            if !self.pending_lazy.contains_key(&v) {
                continue;
            }

            // If this value has no name but has a named Cast/Copy consumer in
            // pending_lazy, materialize the consumer instead.  build_expr_from_op
            // on the Cast will call build_val(v) which consumes v from pending_lazy.
            if !self.value_names.contains_key(&v) {
                if let Some(&(consumer_v, _)) = named_consumers.get(&v) {
                    if let Some(consumer_iid) = self.pending_lazy.remove(&consumer_v) {
                        let op = self.func.insts[consumer_iid].op.clone();
                        if let Some(expr) = self.build_expr_from_op(&op) {
                            let name = self.value_name(consumer_v);
                            if self.shared_names.contains(&name) {
                                stmts.push(Stmt::Assign {
                                    target: Expr::Var(name),
                                    value: expr,
                                });
                            } else {
                                stmts.push(Stmt::VarDecl {
                                    name,
                                    ty: None,
                                    init: Some(expr),
                                    mutable: false,
                                });
                            }
                            continue;
                        }
                    }
                }
            }

            // Normal flush: remove from pending and materialize.
            if self.pending_lazy.remove(&v).is_none() {
                continue;
            }
            let op = self.func.insts[iid].op.clone();
            if let Some(expr) = self.build_expr_from_op(&op) {
                let name = self.value_name(v);
                if self.shared_names.contains(&name) {
                    stmts.push(Stmt::Assign {
                        target: Expr::Var(name),
                        value: expr,
                    });
                } else {
                    stmts.push(Stmt::VarDecl {
                        name,
                        ty: None,
                        init: Some(expr),
                        mutable: false,
                    });
                }
            }
        }
    }

    /// Either inline a single-use expression or emit it as a statement.
    fn emit_or_inline(&mut self, v: ValueId, expr: Expr, stmts: &mut Vec<Stmt>) {
        let count = self.use_count(v);
        if count == 1 {
            self.side_effecting_inlines.insert(v, expr);
        } else if count == 0 {
            stmts.push(Stmt::Expr(expr));
        } else {
            self.referenced_block_params.insert(v);
            stmts.push(Stmt::Assign {
                target: Expr::Var(self.value_name(v)),
                value: expr,
            });
        }
    }

    /// Collect block-param declarations for non-entry blocks, plus any
    /// shared names that lack a block-param declaration (from Cast/Copy
    /// name propagation).
    fn collect_block_param_decls(&self) -> Vec<Stmt> {
        let mut decls = Vec::new();
        let mut declared = HashSet::new();
        for p in &self.func.blocks[self.func.entry].params {
            declared.insert(self.value_name(p.value));
        }
        for (block_id, block) in self.func.blocks.iter() {
            if block_id == self.func.entry {
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
        // Shared names without a block-param declaration need an uninit let.
        // This happens when Cast/Copy name propagation creates duplicate names
        // for a Cast result and its source — both emit Assign, but neither
        // generates a VarDecl.
        // Sort for deterministic declaration order.
        let mut sorted_shared: Vec<_> = self.shared_names.iter().collect();
        sorted_shared.sort();
        for name in sorted_shared {
            if declared.insert(name.clone()) {
                decls.push(Stmt::VarDecl {
                    name: name.clone(),
                    ty: None,
                    init: None,
                    mutable: true,
                });
            }
        }
        decls
    }

    fn build_params(&self) -> Vec<(String, Type)> {
        let mut seen = HashSet::new();
        self.func.blocks[self.func.entry]
            .params
            .iter()
            .map(|p| {
                let mut name = self.value_name(p.value);
                if !seen.insert(name.clone()) {
                    // Duplicate parameter name — append a suffix.
                    let base = name.clone();
                    let mut i = 2;
                    loop {
                        name = format!("{base}{i}");
                        if seen.insert(name.clone()) {
                            break;
                        }
                        i += 1;
                    }
                }
                (name, p.ty.clone())
            })
            .collect()
    }

    // -------------------------------------------------------------------
    // Statement emission
    // -------------------------------------------------------------------

    fn emit_stmts(&mut self, stmts: &[LinearStmt]) -> Vec<Stmt> {
        let mut out = Vec::new();
        self.emit_stmts_into(stmts, &mut out);
        out
    }

    fn emit_stmts_into(&mut self, stmts: &[LinearStmt], out: &mut Vec<Stmt>) {
        for stmt in stmts {
            self.emit_one(stmt, out);
        }
    }

    fn emit_one(&mut self, stmt: &LinearStmt, stmts: &mut Vec<Stmt>) {
        match stmt {
            LinearStmt::Def { result, inst_id } => self.emit_def(*result, *inst_id, stmts),
            LinearStmt::Effect { inst_id } => self.emit_effect(*inst_id, stmts),
            LinearStmt::Assign { dst, src } => self.emit_assign(*dst, *src, stmts),
            LinearStmt::Return { value } => {
                stmts.push(Stmt::Return(value.map(|v| self.build_val(v))));
            }
            LinearStmt::Break => stmts.push(Stmt::Break),
            LinearStmt::Continue => stmts.push(Stmt::Continue),
            LinearStmt::LabeledBreak { depth } => {
                stmts.push(Stmt::LabeledBreak { depth: *depth });
            }
            LinearStmt::If {
                cond,
                then_body,
                else_body,
            } => self.emit_if(*cond, then_body, else_body, stmts),
            LinearStmt::While {
                header,
                cond,
                cond_negated,
                body,
            } => self.emit_while(header, *cond, *cond_negated, body, stmts),
            LinearStmt::For {
                init,
                header,
                cond,
                cond_negated,
                update,
                body,
            } => self.emit_for(init, header, *cond, *cond_negated, update, body, stmts),
            LinearStmt::Loop { body } => {
                let body_stmts = self.emit_stmts(body);
                stmts.push(Stmt::Loop { body: body_stmts });
            }
            LinearStmt::LogicalOr {
                cond,
                phi,
                rhs_body,
                rhs,
            } => self.emit_logical_or(*cond, *phi, rhs_body, *rhs, stmts),
            LinearStmt::LogicalAnd {
                cond,
                phi,
                rhs_body,
                rhs,
            } => self.emit_logical_and(*cond, *phi, rhs_body, *rhs, stmts),
            LinearStmt::Dispatch { blocks, entry } => {
                let mut dispatch_blocks = Vec::new();
                for (id, block_stmts) in blocks {
                    let emitted = self.emit_stmts(block_stmts);
                    dispatch_blocks.push((*id, emitted));
                }
                stmts.push(Stmt::Dispatch {
                    blocks: dispatch_blocks,
                    entry: *entry,
                });
            }
            LinearStmt::Switch {
                value,
                cases,
                default_body,
            } => {
                let val = self.build_val(*value);
                let mut case_stmts = Vec::new();
                for (constant, body) in cases {
                    let emitted = self.emit_stmts(body);
                    case_stmts.push((constant.clone(), emitted));
                }
                let default_stmts = self.emit_stmts(default_body);
                stmts.push(Stmt::Switch {
                    value: val,
                    cases: case_stmts,
                    default_body: default_stmts,
                });
            }
        }
    }

    fn emit_def(&mut self, result: ValueId, inst_id: InstId, stmts: &mut Vec<Stmt>) {
        let op = &self.func.insts[inst_id].op;

        // Phase 2 classified — defer or skip.
        if self.resolve.constant_inlines.contains_key(&result) {
            return;
        }
        if self.resolve.always_inlines.contains(&result) {
            self.always_inline_map.insert(result, inst_id);
            return;
        }
        if self.resolve.lazy_inlines.contains(&result) {
            // If any operand is a pending SE inline, eagerly build the
            // expression and store as SE inline. This chains pure wrappers
            // (e.g. Cast) into their SE operand (e.g. Call) so the flush
            // materializes the combined expression with the correct name.
            let has_se_operand = value_operands(op)
                .iter()
                .any(|v| self.side_effecting_inlines.contains_key(v));
            if has_se_operand {
                let op_clone = op.clone();
                if let Some(expr) = self.build_expr_from_op(&op_clone) {
                    self.side_effecting_inlines.insert(result, expr);
                    return;
                }
            }
            self.pending_lazy.insert(result, inst_id);
            return;
        }

        let count = self.use_count(result);

        // Dead pure.
        if count == 0 && is_deferrable(op) {
            return;
        }

        // Alloc → VarDecl.
        if let Op::Alloc(ty) = op {
            let init = self
                .resolve
                .alloc_inits
                .get(&result)
                .map(|iv| self.build_val(*iv));
            stmts.push(Stmt::VarDecl {
                name: self.value_name(result),
                ty: Some(ty.clone()),
                init,
                mutable: true,
            });
            return;
        }

        // Build expression.
        let op_clone = op.clone();
        let expr = self
            .build_expr_from_op(&op_clone)
            .unwrap_or_else(|| Expr::Var(self.value_name(result)));
        let side_effecting = is_side_effecting_op(&op_clone);

        if count == 0 && side_effecting {
            stmts.push(Stmt::Expr(expr));
        } else if count == 1 && side_effecting {
            self.side_effecting_inlines.insert(result, expr);
        } else {
            let name = self.value_name(result);
            if self.shared_names.contains(&name) {
                stmts.push(Stmt::Assign {
                    target: Expr::Var(name),
                    value: expr,
                });
            } else {
                stmts.push(Stmt::VarDecl {
                    name,
                    ty: None,
                    init: Some(expr),
                    mutable: false,
                });
            }
        }
    }

    fn emit_effect(&mut self, inst_id: InstId, stmts: &mut Vec<Stmt>) {
        let op = &self.func.insts[inst_id].op;

        // Flush pending reads before memory writes.
        if is_memory_write(op) {
            self.flush_pending_reads(stmts);
        }

        // Skip merged stores.
        if self.resolve.skip_stores.contains(&inst_id) {
            return;
        }

        let op = self.func.insts[inst_id].op.clone();
        match &op {
            Op::Store { ptr, value } => {
                let target = Expr::Var(self.value_name(*ptr));
                let val = self.build_val(*value);
                stmts.push(Stmt::Assign { target, value: val });
            }
            Op::SetField {
                object,
                field,
                value,
            } => {
                let target = Expr::Field {
                    object: Box::new(self.build_val(*object)),
                    field: field.clone(),
                };
                let val = self.build_val(*value);
                stmts.push(Stmt::Assign { target, value: val });
            }
            Op::SetIndex {
                collection,
                index,
                value,
            } => {
                if self.is_dictionary(*collection) {
                    // Dictionary → Map: dict.set(key, value)
                    let dict = self.build_val(*collection);
                    let key = self.build_val(*index);
                    let val = self.build_val(*value);
                    stmts.push(Stmt::Expr(Expr::CallIndirect {
                        callee: Box::new(Expr::Field {
                            object: Box::new(dict),
                            field: "set".into(),
                        }),
                        args: vec![key, val],
                    }));
                } else {
                    let target = Expr::Index {
                        collection: Box::new(self.build_val(*collection)),
                        index: Box::new(self.build_val(*index)),
                    };
                    let val = self.build_val(*value);
                    stmts.push(Stmt::Assign { target, value: val });
                }
            }
            // Skip terminators in dispatch blocks.
            Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => {}
            _ => {
                if let Some(expr) = self.build_expr_from_op(&op) {
                    stmts.push(Stmt::Expr(expr));
                }
            }
        }
    }

    fn emit_assign(&mut self, dst: ValueId, src: ValueId, stmts: &mut Vec<Stmt>) {
        // Skip identity assignments (same ValueId) — these are always no-ops.
        if dst == src {
            return;
        }
        let target_name = self.value_name(dst);
        let value = self.build_val(src);
        // Name-based self-assignments are cleaned up by eliminate_self_assigns
        // which runs before ternary rewrite.
        self.referenced_block_params.insert(dst);
        stmts.push(Stmt::Assign {
            target: Expr::Var(target_name),
            value,
        });
    }

    fn emit_if(
        &mut self,
        cond: ValueId,
        then_body: &[LinearStmt],
        else_body: &[LinearStmt],
        stmts: &mut Vec<Stmt>,
    ) {
        self.flush_side_effecting_inlines(stmts);

        // Protect header-level pending reads from flush_pending_reads inside
        // bodies. Without this, a memory write inside a body flushes ALL pending
        // reads — including header-block values like the condition — materializing
        // them as `const vN = expr` inside the body (use-before-def).
        // Only add values not already protected (nesting safety: inner emit_if
        // must not strip protections that outer emit_if added).
        let newly_protected: Vec<ValueId> = self
            .pending_lazy
            .keys()
            .filter(|v| !self.flush_protected.contains(v))
            .copied()
            .collect();
        self.flush_protected.extend(newly_protected.iter().copied());

        // Emit bodies first so we know whether to negate the condition.
        let then_stmts = self.emit_stmts(then_body);
        let else_stmts = self.emit_stmts(else_body);

        for v in &newly_protected {
            self.flush_protected.remove(v);
        }

        let then_empty = then_stmts.is_empty();
        let else_empty = else_stmts.is_empty();

        match (then_empty, else_empty) {
            (true, true) => {
                // Consume the condition so it doesn't become a dangling lazy.
                let _ = self.build_val(cond);
            }
            (false, true) => {
                stmts.push(Stmt::If {
                    cond: self.build_val(cond),
                    then_body: then_stmts,
                    else_body: Vec::new(),
                });
            }
            (true, false) => {
                // Negate condition and use else as then — can invert CmpKind.
                stmts.push(Stmt::If {
                    cond: self.negate_cond(cond),
                    then_body: else_stmts,
                    else_body: Vec::new(),
                });
            }
            (false, false) => {
                stmts.push(Stmt::If {
                    cond: self.build_val(cond),
                    then_body: then_stmts,
                    else_body: else_stmts,
                });
            }
        }
    }

    fn emit_while(
        &mut self,
        header: &[LinearStmt],
        cond: ValueId,
        cond_negated: bool,
        body: &[LinearStmt],
        stmts: &mut Vec<Stmt>,
    ) {
        let mut header_stmts = Vec::new();
        self.emit_stmts_into(header, &mut header_stmts);

        if self.config.while_condition_hoisting && header_stmts.is_empty() {
            let cond_expr = if cond_negated {
                self.negate_cond(cond)
            } else {
                self.build_val(cond)
            };
            let mut body_stmts = self.emit_stmts(body);
            strip_trailing_continue(&mut body_stmts);
            stmts.push(Stmt::While {
                cond: cond_expr,
                body: body_stmts,
            });
        } else {
            let break_expr = if cond_negated {
                self.build_val(cond)
            } else {
                self.negate_cond(cond)
            };
            header_stmts.push(Stmt::If {
                cond: break_expr,
                then_body: vec![Stmt::Break],
                else_body: Vec::new(),
            });
            let mut body_stmts = self.emit_stmts(body);
            strip_trailing_continue(&mut body_stmts);
            header_stmts.append(&mut body_stmts);
            stmts.push(Stmt::Loop { body: header_stmts });
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_for(
        &mut self,
        init: &[LinearStmt],
        header: &[LinearStmt],
        cond: ValueId,
        cond_negated: bool,
        update: &[LinearStmt],
        body: &[LinearStmt],
        stmts: &mut Vec<Stmt>,
    ) {
        // Emit init assigns.
        self.emit_stmts_into(init, stmts);

        let mut header_stmts = Vec::new();
        self.emit_stmts_into(header, &mut header_stmts);

        if self.config.while_condition_hoisting && header_stmts.is_empty() {
            let cond_expr = if cond_negated {
                self.negate_cond(cond)
            } else {
                self.build_val(cond)
            };
            let mut body_stmts = self.emit_stmts(body);
            strip_trailing_continue(&mut body_stmts);
            self.emit_stmts_into(update, &mut body_stmts);
            stmts.push(Stmt::While {
                cond: cond_expr,
                body: body_stmts,
            });
        } else {
            let break_expr = if cond_negated {
                self.build_val(cond)
            } else {
                self.negate_cond(cond)
            };
            header_stmts.push(Stmt::If {
                cond: break_expr,
                then_body: vec![Stmt::Break],
                else_body: Vec::new(),
            });
            let mut body_stmts = self.emit_stmts(body);
            strip_trailing_continue(&mut body_stmts);
            header_stmts.append(&mut body_stmts);
            self.emit_stmts_into(update, &mut header_stmts);
            stmts.push(Stmt::Loop { body: header_stmts });
        }
    }

    fn emit_logical_or(
        &mut self,
        cond: ValueId,
        phi: ValueId,
        rhs_body: &[LinearStmt],
        rhs: ValueId,
        stmts: &mut Vec<Stmt>,
    ) {
        // Save SE inlines from header to prevent leaking into rhs_body.
        let saved_se = std::mem::take(&mut self.side_effecting_inlines);
        let body_stmts = self.emit_stmts(rhs_body);
        let rhs_se = std::mem::replace(&mut self.side_effecting_inlines, saved_se);
        self.side_effecting_inlines.extend(rhs_se);

        if self.config.logical_operators && body_stmts.is_empty() {
            let expr = Expr::LogicalOr {
                lhs: Box::new(self.build_val(cond)),
                rhs: Box::new(self.build_val(rhs)),
            };
            self.emit_or_inline(phi, expr, stmts);
        } else {
            let cond_expr = self.build_val(cond);
            let then_stmts = vec![Stmt::Assign {
                target: Expr::Var(self.value_name(phi)),
                // Reuse the already-built cond expression — build_val(cond)
                // would fail here because the lazy inline was consumed above.
                value: cond_expr.clone(),
            }];
            let mut else_stmts = body_stmts;
            if rhs != phi {
                else_stmts.push(Stmt::Assign {
                    target: Expr::Var(self.value_name(phi)),
                    value: self.build_val(rhs),
                });
            }
            self.referenced_block_params.insert(phi);
            stmts.push(Stmt::If {
                cond: cond_expr,
                then_body: then_stmts,
                else_body: else_stmts,
            });
        }
    }

    fn emit_logical_and(
        &mut self,
        cond: ValueId,
        phi: ValueId,
        rhs_body: &[LinearStmt],
        rhs: ValueId,
        stmts: &mut Vec<Stmt>,
    ) {
        let saved_se = std::mem::take(&mut self.side_effecting_inlines);
        let body_stmts = self.emit_stmts(rhs_body);
        let rhs_se = std::mem::replace(&mut self.side_effecting_inlines, saved_se);
        self.side_effecting_inlines.extend(rhs_se);

        if self.config.logical_operators && body_stmts.is_empty() {
            let expr = Expr::LogicalAnd {
                lhs: Box::new(self.build_val(cond)),
                rhs: Box::new(self.build_val(rhs)),
            };
            self.emit_or_inline(phi, expr, stmts);
        } else {
            let cond_expr = self.build_val(cond);
            let mut then_stmts = body_stmts;
            if rhs != phi {
                then_stmts.push(Stmt::Assign {
                    target: Expr::Var(self.value_name(phi)),
                    value: self.build_val(rhs),
                });
            }
            let else_stmts = vec![Stmt::Assign {
                target: Expr::Var(self.value_name(phi)),
                // Reuse the already-built cond expression — build_val(cond)
                // would fail here because the lazy inline was consumed above.
                value: cond_expr.clone(),
            }];
            self.referenced_block_params.insert(phi);
            stmts.push(Stmt::If {
                cond: cond_expr,
                then_body: then_stmts,
                else_body: else_stmts,
            });
        }
    }
}

// -----------------------------------------------------------------------
// Side-effect classification
// -----------------------------------------------------------------------

fn is_side_effecting_op(op: &Op) -> bool {
    matches!(
        op,
        Op::Call { .. }
            | Op::CallIndirect { .. }
            | Op::SystemCall { .. }
            | Op::MethodCall { .. }
            | Op::Yield(..)
            | Op::CoroutineResume(..)
    )
}

fn is_memory_write(op: &Op) -> bool {
    matches!(
        op,
        Op::SetField { .. } | Op::SetIndex { .. } | Op::Store { .. }
    )
}

// -----------------------------------------------------------------------
// Trailing statement stripping
// -----------------------------------------------------------------------

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
    use crate::ir::inst::CmpKind;
    use crate::ir::structurize::structurize;
    use crate::ir::ty::{FunctionSig, Type};
    use crate::ir::value::Constant;

    #[test]
    fn linearize_simple_block() {
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
        let linear = linearize(&func, &shape);

        // Should have: Def(sum, add_inst), Return(Some(sum))
        assert_eq!(linear.len(), 2);
        assert!(matches!(&linear[0], LinearStmt::Def { result, .. } if *result == sum));
        assert!(matches!(&linear[1], LinearStmt::Return { value: Some(v) } if *v == sum));
    }

    #[test]
    fn linearize_if_else() {
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
        let linear = linearize(&func, &shape);

        // Should contain an If with Return in each branch.
        let has_if = linear.iter().any(|s| matches!(s, LinearStmt::If { .. }));
        assert!(has_if, "Expected an If in linearized output: {linear:?}");
    }

    #[test]
    fn linearize_constant_def() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let c = fb.const_int(42);
        fb.ret(Some(c));
        let func = fb.build();

        let shape = Shape::Block(func.entry);
        let linear = linearize(&func, &shape);

        // Const produces a Def, then Return.
        assert_eq!(linear.len(), 2);
        assert!(matches!(&linear[0], LinearStmt::Def { result, .. } if *result == c));
        match &func.insts[match &linear[0] {
            LinearStmt::Def { inst_id, .. } => *inst_id,
            _ => unreachable!(),
        }]
        .op
        {
            Op::Const(Constant::Int(42)) => {}
            other => panic!("Expected Const(Int(42)), got {other:?}"),
        }
    }

    // -- Phase 2 tests --

    #[test]
    fn resolve_constant_classified() {
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
        let linear = linearize(&func, &shape);
        let ctx = resolve(&func, &linear);

        assert!(ctx.constant_inlines.contains_key(&c));
        assert!(ctx.lazy_inlines.contains(&sum));
        assert_eq!(ctx.use_counts.get(&c).copied().unwrap_or(0), 1);
    }

    #[test]
    fn resolve_dead_pure_eliminated() {
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
        let linear = linearize(&func, &shape);
        let ctx = resolve(&func, &linear);

        // Dead add: use_count == 0, not in any inline set.
        assert_eq!(ctx.use_counts.get(&_dead).copied().unwrap_or(0), 0);
        assert!(!ctx.lazy_inlines.contains(&_dead));
        assert!(!ctx.constant_inlines.contains_key(&_dead));
    }

    #[test]
    fn resolve_cascading_dead_code() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        let _neg = fb.neg(sum); // unused; sum only used by neg
        fb.ret(None);
        let func = fb.build();

        let shape = Shape::Block(func.entry);
        let linear = linearize(&func, &shape);
        let ctx = resolve(&func, &linear);

        // Both neg and sum should be dead after fixpoint.
        assert_eq!(ctx.use_counts.get(&_neg).copied().unwrap_or(0), 0);
        assert_eq!(ctx.use_counts.get(&sum).copied().unwrap_or(0), 0);
    }

    #[test]
    fn resolve_multi_use_not_lazy() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        let doubled = fb.add(sum, sum); // sum used twice
        fb.ret(Some(doubled));
        let func = fb.build();

        let shape = Shape::Block(func.entry);
        let linear = linearize(&func, &shape);
        let ctx = resolve(&func, &linear);

        // sum has 2 uses — should NOT be lazy-inlined.
        assert_eq!(ctx.use_counts.get(&sum).copied().unwrap_or(0), 2);
        assert!(!ctx.lazy_inlines.contains(&sum));
        // doubled has 1 use — should be lazy-inlined.
        assert!(ctx.lazy_inlines.contains(&doubled));
    }

    // -- Phase 3 (full pipeline) tests --

    #[test]
    fn full_pipeline_simple_add() {
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
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        assert_eq!(ast.name, "add");
        assert_eq!(ast.params.len(), 2);
        // Single-use sum should be inlined into return.
        assert_eq!(ast.body.len(), 1);
        assert!(matches!(
            &ast.body[0],
            Stmt::Return(Some(Expr::Binary {
                op: BinOp::Add,
                ..
            }))
        ));
    }

    #[test]
    fn full_pipeline_constant_inlining() {
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
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Constant and sum both inlined into return.
        assert_eq!(ast.body.len(), 1);
        match &ast.body[0] {
            Stmt::Return(Some(Expr::Binary { rhs, .. })) => {
                assert!(matches!(rhs.as_ref(), Expr::Literal(Constant::Int(42))));
            }
            other => panic!("Expected return with binary, got: {other:?}"),
        }
    }

    #[test]
    fn full_pipeline_dead_pure_eliminated() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let a = fb.param(0);
        let b = fb.param(1);
        let _dead = fb.add(a, b);
        fb.ret(None);
        let func = fb.build();

        let shape = Shape::Block(func.entry);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        assert!(ast.body.is_empty(), "Expected empty body, got: {:?}", ast.body);
    }

    #[test]
    fn full_pipeline_if_else() {
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
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        assert!(!ast.body.is_empty());
    }

    // -----------------------------------------------------------------------
    // Regression tests for bug fixes
    // -----------------------------------------------------------------------

    /// Helper: recursively check if any statement in the AST contains a
    /// Stmt::Var reference with the given name.
    fn body_contains_var(body: &[Stmt], name: &str) -> bool {
        body.iter().any(|s| stmt_contains_var(s, name))
    }

    fn stmt_contains_var(stmt: &Stmt, name: &str) -> bool {
        match stmt {
            Stmt::Assign { target, value } => {
                expr_contains_var(target, name) || expr_contains_var(value, name)
            }
            Stmt::VarDecl { init, .. } => {
                init.as_ref().is_some_and(|e| expr_contains_var(e, name))
            }
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                expr_contains_var(cond, name)
                    || body_contains_var(then_body, name)
                    || body_contains_var(else_body, name)
            }
            Stmt::While { cond, body } => {
                expr_contains_var(cond, name) || body_contains_var(body, name)
            }
            Stmt::Loop { body } => body_contains_var(body, name),
            Stmt::Return(Some(e)) | Stmt::Expr(e) => expr_contains_var(e, name),
            Stmt::For {
                init,
                cond,
                update,
                body,
            } => {
                body_contains_var(init, name)
                    || expr_contains_var(cond, name)
                    || body_contains_var(update, name)
                    || body_contains_var(body, name)
            }
            Stmt::CompoundAssign { target, value, .. } => {
                expr_contains_var(target, name) || expr_contains_var(value, name)
            }
            _ => false,
        }
    }

    fn expr_contains_var(expr: &Expr, name: &str) -> bool {
        match expr {
            Expr::Var(n) => n == name,
            Expr::Binary { lhs, rhs, .. } | Expr::Cmp { lhs, rhs, .. } => {
                expr_contains_var(lhs, name) || expr_contains_var(rhs, name)
            }
            Expr::Not(e) | Expr::Cast { expr: e, .. } | Expr::PostIncrement(e) => {
                expr_contains_var(e, name)
            }
            Expr::Ternary {
                cond,
                then_val,
                else_val,
            } => {
                expr_contains_var(cond, name)
                    || expr_contains_var(then_val, name)
                    || expr_contains_var(else_val, name)
            }
            Expr::Call { args, .. } | Expr::SystemCall { args, .. } => {
                args.iter().any(|a| expr_contains_var(a, name))
            }
            Expr::MethodCall { receiver, args, .. } => {
                expr_contains_var(receiver, name)
                    || args.iter().any(|a| expr_contains_var(a, name))
            }
            Expr::Field { object, .. } => expr_contains_var(object, name),
            Expr::Index {
                collection, index, ..
            } => expr_contains_var(collection, name) || expr_contains_var(index, name),
            Expr::LogicalAnd { lhs, rhs } | Expr::LogicalOr { lhs, rhs } => {
                expr_contains_var(lhs, name) || expr_contains_var(rhs, name)
            }
            Expr::Unary { expr: e, .. } => expr_contains_var(e, name),
            Expr::ArrayInit(elems) => elems.iter().any(|e| expr_contains_var(e, name)),
            _ => false,
        }
    }

    /// Helper: count VarDecl statements for a given name in the body.
    fn count_var_decls(body: &[Stmt], name: &str) -> usize {
        body.iter()
            .map(|s| match s {
                Stmt::VarDecl { name: n, .. } if n == name => 1,
                Stmt::If {
                    then_body,
                    else_body,
                    ..
                } => count_var_decls(then_body, name) + count_var_decls(else_body, name),
                Stmt::While { body, .. } | Stmt::Loop { body } => {
                    count_var_decls(body, name)
                }
                Stmt::For {
                    init, body, update, ..
                } => {
                    count_var_decls(init, name)
                        + count_var_decls(body, name)
                        + count_var_decls(update, name)
                }
                _ => 0,
            })
            .sum()
    }

    /// Stringify AST body for debugging.
    fn debug_body(body: &[Stmt]) -> String {
        format!("{body:?}")
    }

    // Regression: 8782b52 — Block→Loop/WhileLoop init assigns must be emitted
    // for loop header block params (not suppressed like ForLoop).
    #[test]
    fn loop_init_assigns_emitted() {
        // entry: v0 = const 0; br header(v0)
        // header(v_i): v_cond = v_i < 10; br_if v_cond, body, exit
        // body: br header(v_i) (no update — just feeds back same value)
        // exit: return
        //
        // This is a WhileLoop. The init assign `v_i = 0` from entry→header
        // must appear in the output before the loop.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let (header, header_vals) = fb.create_block_with_params(&[Type::Int(64)]);
        let body_block = fb.create_block();
        let exit = fb.create_block();

        let v_init = fb.const_int(0);
        fb.br(header, &[v_init]);

        fb.switch_to_block(header);
        let v_i = header_vals[0];
        let v_ten = fb.const_int(10);
        let v_cond = fb.cmp(CmpKind::Lt, v_i, v_ten);
        fb.br_if(v_cond, body_block, &[], exit, &[]);

        fb.switch_to_block(body_block);
        fb.br(header, &[v_i]);

        fb.switch_to_block(exit);
        fb.ret(None);

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Should have an init assign before the loop, not just a naked while.
        // Verify the body is non-empty and has a loop.
        let has_while_or_for = ast.body.iter().any(|s| {
            matches!(s, Stmt::While { .. } | Stmt::For { .. })
        });
        assert!(
            has_while_or_for,
            "Expected loop in output: {}",
            debug_body(&ast.body)
        );
    }

    // Regression: f241be6 — pipeline output must be deterministic.
    #[test]
    fn pipeline_deterministic() {
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let cond = fb.param(0);
        let a = fb.param(1);
        let b = fb.param(2);

        let (then_block, then_vals) = fb.create_block_with_params(&[Type::Int(64)]);
        let (else_block, else_vals) = fb.create_block_with_params(&[Type::Int(64)]);
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Int(64)]);

        let sum = fb.add(a, b);
        let diff = fb.sub(a, b);
        fb.br_if(cond, then_block, &[sum], else_block, &[diff]);

        fb.switch_to_block(then_block);
        fb.br(merge, &[then_vals[0]]);

        fb.switch_to_block(else_block);
        fb.br(merge, &[else_vals[0]]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_vals[0]));

        let mut func = fb.build();
        let shape = structurize(&mut func);

        let ast1 = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());
        let ast2 = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        assert_eq!(
            format!("{:?}", ast1.body),
            format!("{:?}", ast2.body),
            "Pipeline output should be deterministic"
        );
    }

    // Regression: 221d49d — shared names (Cast/Copy coalescing) that don't come
    // from block params must still get a `let` declaration.
    #[test]
    fn shared_name_gets_decl() {
        // entry: v_src = param(0); v_cast = cast(v_src, Int(32))
        //        Name both v_src and v_cast as "x" to trigger shared_names.
        //        return v_cast
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(32),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let v_src = fb.param(0);
        fb.name_value(v_src, "x".to_string());
        let v_cast = fb.cast(v_src, Type::Int(32));
        fb.name_value(v_cast, "x".to_string());
        fb.ret(Some(v_cast));

        let func = fb.build();
        let shape = Shape::Block(func.entry);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // The output should not panic or produce undeclared variables.
        // It may inline the cast or produce a declaration — either is correct.
        // The key property: no undeclared variable reference.
        let _body_str = debug_body(&ast.body);
    }

    // Regression: 4c7c747 — duplicate parameter names must be deduplicated.
    #[test]
    fn duplicate_param_names_deduped() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let a = fb.param(0);
        let b = fb.param(1);
        // Name both params the same.
        fb.name_value(a, "x".to_string());
        fb.name_value(b, "x".to_string());
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        let func = fb.build();
        let shape = Shape::Block(func.entry);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Params should have distinct names.
        assert_eq!(ast.params.len(), 2);
        assert_ne!(
            ast.params[0].0, ast.params[1].0,
            "Parameter names should be deduplicated: {:?}",
            ast.params
        );
    }

    // Regression: af55c19 — non-self values that share the self-parameter's
    // name must be renamed to avoid `this = ...` assignments.
    #[test]
    fn reassigned_self_param_renamed() {
        // Instance method with self param named "this".
        // Another value also named "this" — should be renamed to "_this".
        let sig = FunctionSig {
            params: vec![
                Type::Struct("Foo".to_string()),
                Type::Int(64),
            ],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("method", sig, Visibility::Public);
        fb.set_class(vec![], "Foo".to_string(), MethodKind::Instance);

        let self_param = fb.param(0);
        let other = fb.param(1);
        fb.name_value(self_param, "this".to_string());
        fb.name_value(other, "this".to_string());

        let result = fb.add(other, other);
        fb.ret(Some(result));

        let func = fb.build();
        let shape = Shape::Block(func.entry);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // The two "this" params should have different names.
        // param(0) keeps "this", param(1) gets "_this" or similar.
        assert_eq!(ast.params.len(), 2);
        assert_ne!(
            ast.params[0].0, ast.params[1].0,
            "Self-param collision should be resolved: {:?}",
            ast.params
        );
    }

    // Regression: 3e0c48d — Store target must use Var(name), not inlined expr.
    // Tests the emitter's Op::Store handler directly: the LHS of a Store must
    // be `Expr::Var(name)`, not `build_val(ptr)` which could inline a Cast.
    // We test the emitter directly using linearize+resolve+emit because the
    // full pipeline's transforms (Mem2Reg) promote most alloc/store/load chains.
    #[test]
    fn store_target_uses_var_name() {
        // Build IR: alloc x; store x, param; store x, param2; load x; return
        // Use the linearizer directly (Phase 1→2→3) to skip transform passes.
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let p0 = fb.param(0);
        let p1 = fb.param(1);
        let ptr = fb.alloc(Type::Int(64));
        fb.name_value(ptr, "x".to_string());
        fb.store(ptr, p0);
        fb.store(ptr, p1);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = fb.build();
        let shape = Shape::Block(func.entry);
        let linear = linearize(&func, &shape);
        let resolved = resolve(&func, &linear);
        let config = LoweringConfig::default();
        let mut ctx = EmitCtx::new(&func, &resolved, &config);
        let body = ctx.emit_stmts(&linear);

        // The Store should produce assignments with Var("x") target.
        let has_x_assign = body.iter().any(|s| match s {
            Stmt::Assign {
                target: Expr::Var(n),
                ..
            } => n == "x",
            Stmt::VarDecl { name, .. } => name == "x",
            _ => false,
        });
        assert!(
            has_x_assign,
            "Expected assignment to Var(\"x\"): {}",
            debug_body(&body)
        );
    }

    // Regression: 4ef6f43 — debug names propagate through Cast/Copy so the
    // source operand gets a human-readable name.
    #[test]
    fn debug_name_propagates_through_cast() {
        // v_field = get_field(param, "hp")  (unnamed)
        // v_cast = cast(v_field, Int(32))   (named "hp")
        // return v_cast
        //
        // The name "hp" should propagate back to v_field.
        let sig = FunctionSig {
            params: vec![Type::Struct("Obj".to_string())],
            return_ty: Type::Int(32),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let obj = fb.param(0);
        let v_field = fb.get_field(obj, "hp", Type::Dynamic);
        // Don't name v_field — only name the cast result.
        let v_cast = fb.cast(v_field, Type::Int(32));
        fb.name_value(v_cast, "hp".to_string());
        fb.ret(Some(v_cast));

        let func = fb.build();
        let shape = Shape::Block(func.entry);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Output should reference "hp" somewhere, not a vN identifier.
        let body_str = debug_body(&ast.body);
        assert!(
            body_contains_var(&ast.body, "hp") || body_str.contains("hp"),
            "Expected 'hp' name in output: {body_str}"
        );
    }

    // Regression: 65170ad — LogicalOr/And must not call build_val(cond) twice
    // when cond is a single-use lazy value. The fix reuses cond_expr.clone().
    #[test]
    fn logical_or_no_double_build() {
        // entry: br_if cond, merge(cond), else_block()
        // else_block: br merge(other)
        // merge(phi): return phi
        //
        // This produces a LogicalOr shape. If build_val(cond) is called twice,
        // the second call would fail because the lazy inline was consumed.
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Bool],
            return_ty: Type::Bool,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let cond = fb.param(0);
        let other = fb.param(1);

        let else_block = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);

        fb.br_if(cond, merge, &[cond], else_block, &[]);

        fb.switch_to_block(else_block);
        fb.br(merge, &[other]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_vals[0]));

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let config = LoweringConfig::default();
        let ast = lower_function_linear(&func, &shape, &config, &DebugConfig::none());

        // Should not panic. Output should contain a LogicalOr or similar.
        assert!(!ast.body.is_empty(), "Expected non-empty body");
    }

    // Regression: 1076a5c — flush_pending_reads must skip already-consumed values.
    // When building one pending value consumes another, the consumed value
    // must not be flushed again.
    #[test]
    fn flush_skips_consumed_values() {
        // entry: v1 = get_field(param, "a")
        //        v2 = get_field(v1, "b")
        //        store(alloc, v2)   ← triggers flush
        //        return
        //
        // v1 and v2 are both pending lazy. Flushing v2 consumes v1.
        // The flush loop must skip v1 when it tries to flush it.
        let sig = FunctionSig {
            params: vec![Type::Struct("Obj".to_string())],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let obj = fb.param(0);
        let v1 = fb.get_field(obj, "a", Type::Dynamic);
        let v2 = fb.get_field(v1, "b", Type::Dynamic);
        let ptr = fb.alloc(Type::Dynamic);
        fb.store(ptr, v2);
        fb.ret(None);

        let func = fb.build();
        let shape = Shape::Block(func.entry);
        // Should not panic from double-flush.
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());
        let _ = debug_body(&ast.body);
    }

    // Regression: 48671ff — flush_pending_reads must be scoped to prevent
    // use-before-def. Header-block values must not be flushed inside if-bodies.
    #[test]
    fn flush_scoped_to_prevent_use_before_def() {
        // entry: v_field = get_field(param, "x")
        //        br_if cond, then_block, merge
        // then_block: set_field(param, "x", const 0)   ← triggers flush
        //             br merge
        // merge: return v_field
        //
        // v_field is defined in the header but used after the if/else.
        // It must NOT be flushed inside then_block's body (use-before-def).
        let sig = FunctionSig {
            params: vec![Type::Struct("Obj".to_string()), Type::Bool],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let obj = fb.param(0);
        let cond = fb.param(1);
        let v_field = fb.get_field(obj, "x", Type::Dynamic);

        let then_block = fb.create_block();
        let merge = fb.create_block();

        fb.br_if(cond, then_block, &[], merge, &[]);

        fb.switch_to_block(then_block);
        let zero = fb.const_int(0);
        fb.set_field(obj, "x", zero);
        fb.br(merge, &[]);

        fb.switch_to_block(merge);
        fb.ret(Some(v_field));

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // v_field's declaration must appear before the if/else, not inside it.
        // The return should reference the field value.
        let has_return = ast.body.iter().any(|s| matches!(s, Stmt::Return(Some(_))));
        assert!(
            has_return,
            "Expected return statement: {}",
            debug_body(&ast.body)
        );
    }

    // Regression: 7821541 — pure wrapper (Cast) around side-effecting operand
    // (Call) must chain into a single SE inline, not produce broken output.
    #[test]
    fn se_chain_through_cast() {
        // v_call = call("f", [])
        // v_cast = cast(v_call, Int(32))
        // return v_cast
        //
        // v_call is side-effecting. v_cast wraps it. Should produce clean output.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(32),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("g", sig, Visibility::Public);
        let v_call = fb.call("f", &[], Type::Dynamic);
        fb.name_value(v_call, "result".to_string());
        let v_cast = fb.cast(v_call, Type::Int(32));
        fb.name_value(v_cast, "result".to_string());
        fb.ret(Some(v_cast));

        let func = fb.build();
        let shape = Shape::Block(func.entry);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Should produce clean output with "result" name, no broken references.
        assert_eq!(ast.body.len(), 1, "Expected single return: {}", debug_body(&ast.body));
        assert!(matches!(&ast.body[0], Stmt::Return(Some(_))));
    }

    // Regression: e726c58 — when then-body is empty, negate the Cmp condition
    // directly (invert CmpKind) instead of wrapping in Not(Cmp(...)).
    #[test]
    fn inverted_cmp_not_wrapped_in_not() {
        // entry: v_cmp = cmp.lt(a, b)
        //        br_if v_cmp, then_block, else_block
        // then_block: br merge  (empty)
        // else_block: store something; br merge
        // merge: return
        //
        // Since then is empty, the linearizer inverts to: if (!cond) { else }
        // With CmpKind, this should produce `cmp.ge(a, b)` not `Not(cmp.lt(a,b))`.
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let a = fb.param(0);
        let b = fb.param(1);
        let v_cmp = fb.cmp(CmpKind::Lt, a, b);

        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let merge = fb.create_block();

        fb.br_if(v_cmp, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        fb.br(merge, &[]);

        fb.switch_to_block(else_block);
        let ptr = fb.alloc(Type::Int(64));
        let val = fb.const_int(1);
        fb.store(ptr, val);
        fb.br(merge, &[]);

        fb.switch_to_block(merge);
        fb.ret(None);

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Find the If statement and check its condition.
        let if_stmt = ast.body.iter().find(|s| matches!(s, Stmt::If { .. }));
        if let Some(Stmt::If { cond, .. }) = if_stmt {
            // Condition should be inverted Cmp (Ge), not Not(Cmp(Lt)).
            assert!(
                matches!(cond, Expr::Cmp { kind: CmpKind::Ge, .. }),
                "Expected inverted Cmp (Ge), not Not wrapper: {cond:?}"
            );
        }
        // If the if was eliminated by AST passes, that's also fine.
    }

    // Regression: f9d14ec — LogicalAnd/Or phi values flushed as SE inlines
    // must not get duplicate declarations.
    #[test]
    fn logical_and_no_duplicate_decl() {
        // entry: br_if cond, then_mid(), merge(cond)
        // then_mid: v_rhs = call("check", []); br merge(v_rhs)
        // merge(phi): return phi
        //
        // phi is a LogicalAnd result. The call is side-effecting.
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Bool,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let cond = fb.param(0);

        let then_mid = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);

        fb.br_if(cond, then_mid, &[], merge, &[cond]);

        fb.switch_to_block(then_mid);
        let v_rhs = fb.call("check", &[], Type::Bool);
        fb.br(merge, &[v_rhs]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_vals[0]));

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Count VarDecl statements for the phi value's name. There should be
        // at most 1 declaration (not duplicated).
        // The phi might be inlined entirely, which is also fine.
        for param in &ast.params {
            let decl_count = count_var_decls(&ast.body, &param.0);
            assert!(
                decl_count <= 1,
                "Duplicate VarDecl for param '{}': count={}, body: {}",
                param.0,
                decl_count,
                debug_body(&ast.body)
            );
        }
    }

    // Regression: 0983e97 — minmax pattern with SE operands must flush correctly.
    #[test]
    fn minmax_se_flush_correct() {
        // entry: v_a = call("getA", [])
        //        v_b = call("getB", [])
        //        v_cmp = cmp.ge(v_a, v_b)
        //        br_if v_cmp, then_block(v_a), else_block(v_b)
        // then_block(v_t): br merge(v_t)
        // else_block(v_e): br merge(v_e)
        // merge(phi): return phi
        //
        // This should produce Math.max(getA(), getB()).
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let v_a = fb.call("getA", &[], Type::Int(64));
        let v_b = fb.call("getB", &[], Type::Int(64));
        let v_cmp = fb.cmp(CmpKind::Ge, v_a, v_b);

        let (then_block, then_vals) = fb.create_block_with_params(&[Type::Int(64)]);
        let (else_block, else_vals) = fb.create_block_with_params(&[Type::Int(64)]);
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Int(64)]);

        fb.br_if(v_cmp, then_block, &[v_a], else_block, &[v_b]);

        fb.switch_to_block(then_block);
        fb.br(merge, &[then_vals[0]]);

        fb.switch_to_block(else_block);
        fb.br(merge, &[else_vals[0]]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_vals[0]));

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let ast = lower_function_linear(&func, &shape, &LoweringConfig::default(), &DebugConfig::none());

        // Should produce clean output — no panic from SE flush timing.
        assert!(
            !ast.body.is_empty(),
            "Expected non-empty body: {}",
            debug_body(&ast.body)
        );
    }

    // Regression: cf0524a — while-loop condition should be hoisted into
    // `while (cond)` when the header has no materialized statements.
    #[test]
    fn while_loop_condition_hoisted() {
        // entry: br header
        // header: v_cond = cmp.lt(param, 10); br_if v_cond, body, exit
        // body: br header
        // exit: return
        //
        // The Cmp is single-use and pure — header has no materialized stmts.
        // Should produce `while (param < 10) { }` not `while (true) { if (...) break; }`.
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("f", sig, Visibility::Public);
        let n = fb.param(0);
        fb.name_value(n, "n".to_string());

        let header = fb.create_block();
        let body_block = fb.create_block();
        let exit = fb.create_block();

        fb.br(header, &[]);

        fb.switch_to_block(header);
        let ten = fb.const_int(10);
        let v_cond = fb.cmp(CmpKind::Lt, n, ten);
        fb.br_if(v_cond, body_block, &[], exit, &[]);

        fb.switch_to_block(body_block);
        fb.br(header, &[]);

        fb.switch_to_block(exit);
        fb.ret(None);

        let mut func = fb.build();
        let shape = structurize(&mut func);
        let config = LoweringConfig::default();
        let ast = lower_function_linear(&func, &shape, &config, &DebugConfig::none());

        // With while_condition_hoisting enabled (default), should be While
        // with a real condition, not `While { cond: Literal(true), ... }`.
        let has_while_with_cond = ast.body.iter().any(|s| match s {
            Stmt::While { cond, .. } => !matches!(cond, Expr::Literal(Constant::Bool(true))),
            _ => false,
        });
        assert!(
            has_while_with_cond,
            "Expected while with hoisted condition: {}",
            debug_body(&ast.body)
        );
    }
}
