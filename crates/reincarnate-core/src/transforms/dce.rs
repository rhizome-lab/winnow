use std::collections::{HashMap, HashSet, VecDeque};

use crate::error::CoreError;
use crate::ir::{BlockId, Constant, Function, InstId, Module, Op, ValueId};
use crate::pipeline::{Transform, TransformResult};

/// Dead code elimination transform — removes unused instructions and unreachable blocks.
///
/// Four phases per function:
/// 1. Simplify constant branches (`BrIf`/`Switch` with known conditions → `Br`)
/// 2. Mark reachable blocks via CFG walk from entry
/// 3. Mark live instructions backward from side-effectful roots
/// 4. Remove dead instructions and clear unreachable blocks
pub struct DeadCodeElimination;

/// Build a map from ValueId → Constant for all `Op::Const` instructions.
fn build_const_map(func: &Function) -> HashMap<ValueId, Constant> {
    let mut map = HashMap::new();
    for (_, inst) in func.insts.iter() {
        if let (Op::Const(c), Some(result)) = (&inst.op, inst.result) {
            map.insert(result, c.clone());
        }
    }
    map
}

/// Returns true if the given constant is truthy for branch purposes.
fn is_truthy(c: &Constant) -> bool {
    match c {
        Constant::Bool(b) => *b,
        Constant::Int(n) => *n != 0,
        Constant::UInt(n) => *n != 0,
        Constant::Float(f) => *f != 0.0,
        Constant::String(s) => !s.is_empty(),
        Constant::Null => false,
    }
}

/// Phase 1: Simplify branches with constant conditions to unconditional branches.
fn simplify_constant_branches(func: &mut Function) {
    let consts = build_const_map(func);

    for inst_id in func.insts.keys().collect::<Vec<_>>() {
        let inst = &func.insts[inst_id];
        let new_op = match &inst.op {
            Op::BrIf {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } => {
                if let Some(c) = consts.get(cond) {
                    if is_truthy(c) {
                        Some(Op::Br {
                            target: *then_target,
                            args: then_args.clone(),
                        })
                    } else {
                        Some(Op::Br {
                            target: *else_target,
                            args: else_args.clone(),
                        })
                    }
                } else {
                    None
                }
            }
            Op::Switch {
                value,
                cases,
                default,
            } => {
                if let Some(c) = consts.get(value) {
                    let matched = cases.iter().find(|(case_val, _, _)| case_val == c);
                    if let Some((_, target, args)) = matched {
                        Some(Op::Br {
                            target: *target,
                            args: args.clone(),
                        })
                    } else {
                        Some(Op::Br {
                            target: default.0,
                            args: default.1.clone(),
                        })
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(op) = new_op {
            func.insts[inst_id].op = op;
        }
    }
}

/// Phase 2: Find all blocks reachable from the entry block via CFG walk.
fn find_reachable_blocks(func: &Function) -> HashSet<BlockId> {
    let mut reachable = HashSet::new();
    let mut worklist = VecDeque::new();
    worklist.push_back(func.entry);
    reachable.insert(func.entry);

    while let Some(block_id) = worklist.pop_front() {
        let block = &func.blocks[block_id];
        for &inst_id in &block.insts {
            for target in branch_targets(&func.insts[inst_id].op) {
                if reachable.insert(target) {
                    worklist.push_back(target);
                }
            }
        }
    }

    reachable
}

/// Extract branch targets from a control-flow instruction.
fn branch_targets(op: &Op) -> Vec<BlockId> {
    match op {
        Op::Br { target, .. } => vec![*target],
        Op::BrIf {
            then_target,
            else_target,
            ..
        } => vec![*then_target, *else_target],
        Op::Switch {
            cases, default, ..
        } => {
            let mut targets: Vec<BlockId> = cases.iter().map(|(_, t, _)| *t).collect();
            targets.push(default.0);
            targets
        }
        _ => vec![],
    }
}

/// Returns true if the instruction has side effects and must be kept.
fn has_side_effects(op: &Op) -> bool {
    matches!(
        op,
        // Control flow
        Op::Br { .. }
            | Op::BrIf { .. }
            | Op::Switch { .. }
            | Op::Return(_)
            | Op::Yield(_)
            // Mutation
            | Op::Store { .. }
            | Op::SetField { .. }
            | Op::SetIndex { .. }
            // Calls (may have arbitrary side effects)
            | Op::Call { .. }
            | Op::CallIndirect { .. }
            | Op::SystemCall { .. }
            // Coroutine operations
            | Op::CoroutineCreate { .. }
            | Op::CoroutineResume(_)
    )
}

/// Extract all ValueId operands from an Op.
fn value_operands(op: &Op) -> Vec<ValueId> {
    match op {
        Op::Const(_) => vec![],
        Op::Add(a, b)
        | Op::Sub(a, b)
        | Op::Mul(a, b)
        | Op::Div(a, b)
        | Op::Rem(a, b)
        | Op::BitAnd(a, b)
        | Op::BitOr(a, b)
        | Op::BitXor(a, b)
        | Op::Shl(a, b)
        | Op::Shr(a, b) => vec![*a, *b],
        Op::Neg(a) | Op::BitNot(a) | Op::Not(a) | Op::Copy(a) => vec![*a],
        Op::Cmp(_, a, b) => vec![*a, *b],
        Op::Br { args, .. } => args.clone(),
        Op::BrIf {
            cond,
            then_args,
            else_args,
            ..
        } => {
            let mut ops = vec![*cond];
            ops.extend(then_args);
            ops.extend(else_args);
            ops
        }
        Op::Switch {
            value,
            cases,
            default,
            ..
        } => {
            let mut ops = vec![*value];
            for (_, _, args) in cases {
                ops.extend(args);
            }
            ops.extend(&default.1);
            ops
        }
        Op::Return(v) | Op::Yield(v) => v.iter().copied().collect(),
        Op::Alloc(_) => vec![],
        Op::Load(ptr) => vec![*ptr],
        Op::Store { ptr, value } => vec![*ptr, *value],
        Op::GetField { object, .. } => vec![*object],
        Op::SetField { object, value, .. } => vec![*object, *value],
        Op::GetIndex { collection, index } => vec![*collection, *index],
        Op::SetIndex {
            collection,
            index,
            value,
        } => vec![*collection, *index, *value],
        Op::Call { args, .. } => args.clone(),
        Op::CallIndirect { callee, args } => {
            let mut ops = vec![*callee];
            ops.extend(args);
            ops
        }
        Op::SystemCall { args, .. } => args.clone(),
        Op::Cast(a, _) | Op::TypeCheck(a, _) => vec![*a],
        Op::StructInit { fields, .. } => fields.iter().map(|(_, v)| *v).collect(),
        Op::ArrayInit(elems) | Op::TupleInit(elems) => elems.clone(),
        Op::CoroutineCreate { args, .. } => args.clone(),
        Op::CoroutineResume(v) => vec![*v],
        Op::GlobalRef(_) => vec![],
    }
}

/// Phase 3 & 4: Mark live instructions and rewrite the function.
/// Returns true if any changes were made.
fn eliminate_dead_code(func: &mut Function) -> bool {
    // Phase 1: Simplify constant branches.
    simplify_constant_branches(func);

    // Phase 2: Find reachable blocks.
    let reachable = find_reachable_blocks(func);

    // Build producer map: ValueId → InstId (which instruction produces each value).
    let mut producer: HashMap<ValueId, InstId> = HashMap::new();
    for block_id in func.blocks.keys() {
        if !reachable.contains(&block_id) {
            continue;
        }
        for &inst_id in &func.blocks[block_id].insts {
            if let Some(result) = func.insts[inst_id].result {
                producer.insert(result, inst_id);
            }
        }
    }

    // Phase 3: Mark live instructions via backward worklist.
    let mut live = HashSet::new();
    let mut worklist: VecDeque<InstId> = VecDeque::new();

    // Seed with side-effectful instructions in reachable blocks.
    for &block_id in &reachable.iter().copied().collect::<Vec<_>>() {
        for &inst_id in &func.blocks[block_id].insts {
            if has_side_effects(&func.insts[inst_id].op) && live.insert(inst_id) {
                worklist.push_back(inst_id);
            }
        }
    }

    // Propagate liveness to operand producers.
    while let Some(inst_id) = worklist.pop_front() {
        for operand in value_operands(&func.insts[inst_id].op) {
            if let Some(&prod_id) = producer.get(&operand) {
                if live.insert(prod_id) {
                    worklist.push_back(prod_id);
                }
            }
        }
    }

    // Phase 4: Rewrite — filter instructions in reachable blocks, clear unreachable blocks.
    let mut changed = false;
    for block_id in func.blocks.keys().collect::<Vec<_>>() {
        if reachable.contains(&block_id) {
            let before = func.blocks[block_id].insts.len();
            func.blocks[block_id]
                .insts
                .retain(|inst_id| live.contains(inst_id));
            if func.blocks[block_id].insts.len() != before {
                changed = true;
            }
        } else {
            if !func.blocks[block_id].insts.is_empty()
                || !func.blocks[block_id].params.is_empty()
            {
                changed = true;
            }
            func.blocks[block_id].insts.clear();
            func.blocks[block_id].params.clear();
        }
    }
    changed
}

impl Transform for DeadCodeElimination {
    fn name(&self) -> &str {
        "dead-code-elimination"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            changed |= eliminate_dead_code(&mut module.functions[func_id]);
        }
        Ok(TransformResult {
            module,
            changed,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityRef;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::ty::FunctionSig;
    use crate::ir::{FuncId, Type, Visibility};

    fn apply_dce(func: Function) -> Function {
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let result = DeadCodeElimination.apply(module).unwrap();
        result.module.functions[FuncId::new(0)].clone()
    }

    /// Count non-empty instructions in a block.
    fn block_inst_count(func: &Function, block: BlockId) -> usize {
        func.blocks[block].insts.len()
    }

    /// Dead arithmetic is removed: unused add result gets eliminated.
    #[test]
    fn dead_arithmetic_removed() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(1);
        let b = fb.const_int(2);
        let _sum = fb.add(a, b); // unused
        fb.ret(None);

        let func = apply_dce(fb.build());
        // Only the return should remain — consts and add are dead.
        let entry = func.entry;
        assert_eq!(block_inst_count(&func, entry), 1);
        assert!(matches!(
            func.insts[func.blocks[entry].insts[0]].op,
            Op::Return(None)
        ));
    }

    /// Used arithmetic is kept: result feeds a return.
    #[test]
    fn used_arithmetic_kept() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(1);
        let b = fb.const_int(2);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        let func = apply_dce(fb.build());
        // const 1, const 2, add, return — all live.
        let entry = func.entry;
        assert_eq!(block_inst_count(&func, entry), 4);
    }

    /// Side effects are kept: Call with unused result is preserved.
    #[test]
    fn side_effects_kept() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let _call_result = fb.call("side_effect", &[], Type::Void);
        fb.ret(None);

        let func = apply_dce(fb.build());
        // Call and return both kept.
        let entry = func.entry;
        assert_eq!(block_inst_count(&func, entry), 2);
    }

    /// Chained dead code: `a = const 1; b = add(a, a)` where `b` unused — both removed.
    #[test]
    fn chained_dead_code() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(1);
        let _b = fb.add(a, a); // unused chain
        fb.ret(None);

        let func = apply_dce(fb.build());
        let entry = func.entry;
        assert_eq!(block_inst_count(&func, entry), 1);
    }

    /// Constant branch simplified: `BrIf(const true, A, B)` → `Br(A)`, B's dead code removed.
    #[test]
    fn constant_branch_simplified() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let then_block = fb.create_block();
        let else_block = fb.create_block();

        let cond = fb.const_bool(true);
        fb.br_if(cond, then_block, &[], else_block, &[]);

        // then_block returns 1
        fb.switch_to_block(then_block);
        let one = fb.const_int(1);
        fb.ret(Some(one));

        // else_block returns 2 — should be unreachable
        fb.switch_to_block(else_block);
        let two = fb.const_int(2);
        fb.ret(Some(two));

        let func = apply_dce(fb.build());

        // Entry should have Br (simplified from BrIf) + the const that feeds it.
        let entry = func.entry;
        let entry_insts: Vec<&Op> = func.blocks[entry]
            .insts
            .iter()
            .map(|id| &func.insts[*id].op)
            .collect();
        // The BrIf was simplified to Br; the const(true) may or may not be kept
        // depending on whether Br references it. Br doesn't reference the condition
        // value, so const(true) should be dead.
        assert!(entry_insts
            .iter()
            .any(|op| matches!(op, Op::Br { target, .. } if *target == then_block)));

        // then_block should be reachable with its instructions.
        assert!(block_inst_count(&func, then_block) >= 2);

        // else_block should be cleared (unreachable).
        assert_eq!(block_inst_count(&func, else_block), 0);
        assert!(func.blocks[else_block].params.is_empty());
    }

    /// Unreachable block is cleared: block with no predecessors has its instructions removed.
    #[test]
    fn unreachable_block_cleared() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let dead_block = fb.create_block();

        // Entry returns immediately — dead_block is never targeted.
        fb.ret(None);

        // Put some instructions in the dead block.
        fb.switch_to_block(dead_block);
        let a = fb.const_int(42);
        fb.ret(Some(a));

        let func = apply_dce(fb.build());
        assert_eq!(block_inst_count(&func, dead_block), 0);
        assert!(func.blocks[dead_block].params.is_empty());
    }
}
