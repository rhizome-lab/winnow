use std::collections::{HashMap, HashSet, VecDeque};

use crate::error::CoreError;
use crate::ir::{BlockId, Function, Module, Op, ValueId};
use crate::pipeline::{Transform, TransformResult};

/// CFG simplification transform — removes redundant blocks and simplifies control flow.
///
/// Three phases per function, iterated to a fixed point:
/// 1. Forward empty blocks (blocks whose only instruction is an unconditional `Br`)
/// 2. Merge blocks (single-predecessor blocks absorbed into their predecessor)
/// 3. Cleanup unreachable blocks (clear instructions and params)
pub struct CfgSimplify;

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

/// Find all blocks reachable from the entry block via BFS.
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

/// Build a predecessor map: for each block, which blocks branch to it.
fn build_predecessor_map(func: &Function) -> HashMap<BlockId, Vec<BlockId>> {
    let mut preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for block_id in func.blocks.keys() {
        preds.entry(block_id).or_default();
        for &inst_id in &func.blocks[block_id].insts {
            for target in branch_targets(&func.insts[inst_id].op) {
                preds.entry(target).or_default().push(block_id);
            }
        }
    }
    preds
}

/// Replace ValueIds in an Op using a substitution map.
fn substitute_values_in_op(op: &mut Op, subst: &HashMap<ValueId, ValueId>) {
    let sub = |v: &mut ValueId| {
        if let Some(&new) = subst.get(v) {
            *v = new;
        }
    };

    match op {
        Op::Const(_) => {}
        Op::Add(a, b)
        | Op::Sub(a, b)
        | Op::Mul(a, b)
        | Op::Div(a, b)
        | Op::Rem(a, b)
        | Op::BitAnd(a, b)
        | Op::BitOr(a, b)
        | Op::BitXor(a, b)
        | Op::Shl(a, b)
        | Op::Shr(a, b) => {
            sub(a);
            sub(b);
        }
        Op::Neg(a) | Op::BitNot(a) | Op::Not(a) | Op::Copy(a) => sub(a),
        Op::Cmp(_, a, b) => {
            sub(a);
            sub(b);
        }
        Op::Br { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::BrIf {
            cond,
            then_args,
            else_args,
            ..
        } => {
            sub(cond);
            for a in then_args {
                sub(a);
            }
            for a in else_args {
                sub(a);
            }
        }
        Op::Switch {
            value,
            cases,
            default,
            ..
        } => {
            sub(value);
            for (_, _, args) in cases {
                for a in args {
                    sub(a);
                }
            }
            for a in &mut default.1 {
                sub(a);
            }
        }
        Op::Return(v) => {
            if let Some(v) = v {
                sub(v);
            }
        }
        Op::Yield(v) => {
            if let Some(v) = v {
                sub(v);
            }
        }
        Op::Alloc(_) => {}
        Op::Load(ptr) => sub(ptr),
        Op::Store { ptr, value } => {
            sub(ptr);
            sub(value);
        }
        Op::GetField { object, .. } => sub(object),
        Op::SetField { object, value, .. } => {
            sub(object);
            sub(value);
        }
        Op::GetIndex { collection, index } => {
            sub(collection);
            sub(index);
        }
        Op::SetIndex {
            collection,
            index,
            value,
        } => {
            sub(collection);
            sub(index);
            sub(value);
        }
        Op::Call { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::CallIndirect { callee, args } => {
            sub(callee);
            for a in args {
                sub(a);
            }
        }
        Op::SystemCall { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::Cast(a, _) | Op::TypeCheck(a, _) => sub(a),
        Op::StructInit { fields, .. } => {
            for (_, v) in fields {
                sub(v);
            }
        }
        Op::ArrayInit(elems) | Op::TupleInit(elems) => {
            for e in elems {
                sub(e);
            }
        }
        Op::CoroutineCreate { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::CoroutineResume(v) => sub(v),
        Op::GlobalRef(_) => {}
    }
}

/// Rewrite branch targets in an Op: replace `old` block with `new` block,
/// optionally remapping args via an index mapping.
/// `arg_remap[i]` gives the index into the predecessor's original args to use
/// for the i-th arg of the new target.
fn redirect_block_target_in_op(
    op: &mut Op,
    old: BlockId,
    new: BlockId,
    new_args_template: Option<&[ValueId]>,
) {
    let remap_args = |target: &mut BlockId, args: &mut Vec<ValueId>| {
        if *target == old {
            *target = new;
            if let Some(template) = new_args_template {
                // Build new args from the template. Template values that are
                // block params of `old` have already been resolved to concrete
                // values by the caller through the arg_index_remap path,
                // so we just assign directly.
                *args = template.to_vec();
            }
        }
    };

    match op {
        Op::Br { target, args } => remap_args(target, args),
        Op::BrIf {
            then_target,
            then_args,
            else_target,
            else_args,
            ..
        } => {
            remap_args(then_target, then_args);
            remap_args(else_target, else_args);
        }
        Op::Switch {
            cases, default, ..
        } => {
            for (_, target, args) in cases {
                remap_args(target, args);
            }
            remap_args(&mut default.0, &mut default.1);
        }
        _ => {}
    }
}

/// Phase 1: Forward empty blocks.
///
/// A block is "empty" if its only instruction is `Br { target, args }`.
/// We redirect predecessors to bypass the empty block.
///
/// Returns true if any changes were made.
fn forward_empty_blocks(func: &mut Function) -> bool {
    let mut changed = false;

    // Identify forwarding candidates: blocks with exactly one instruction that is a Br.
    // Collect the forwarding info before mutating.
    let mut forwards: HashMap<BlockId, (BlockId, Vec<ValueId>)> = HashMap::new();

    for block_id in func.blocks.keys() {
        // Never forward the entry block.
        if block_id == func.entry {
            continue;
        }

        let block = &func.blocks[block_id];
        if block.insts.len() != 1 {
            continue;
        }

        let inst = &func.insts[block.insts[0]];
        if let Op::Br { target, args } = &inst.op {
            forwards.insert(block_id, (*target, args.clone()));
        }
    }

    if forwards.is_empty() {
        return false;
    }

    // Resolve transitive forwarding chains: if A→B→C and both are forwarders,
    // resolve A to C. Also detect self-loops (A→A) and remove them.
    let mut resolved: HashMap<BlockId, BlockId> = HashMap::new();
    for &block_id in forwards.keys() {
        let mut target = forwards[&block_id].0;
        let mut visited = HashSet::new();
        visited.insert(block_id);
        while let Some((next_target, _)) = forwards.get(&target) {
            if !visited.insert(target) {
                // Cycle detected — this chain loops back on itself.
                break;
            }
            target = *next_target;
        }
        resolved.insert(block_id, target);
    }

    // For each forwarding block, determine if all its Br args come from block params.
    // Build an index remap: br_arg[i] = params[remap[i]].
    // If any arg is NOT a block param, we can't remap — skip unless block has no params
    // (constant forwarding case).
    struct ForwardInfo {
        target: BlockId,
        /// If None, the block has no params and the Br args are fixed values.
        /// If Some, maps Br arg index → block param index.
        param_remap: Option<Vec<usize>>,
        /// The fixed args for the no-params case.
        fixed_args: Vec<ValueId>,
    }

    let mut forward_info: HashMap<BlockId, ForwardInfo> = HashMap::new();

    for (&block_id, (direct_target, br_args)) in &forwards {
        let final_target = resolved[&block_id];

        // Skip if forwarding resolves to self (block is part of a forwarding cycle).
        if final_target == block_id {
            continue;
        }

        let params = &func.blocks[block_id].params;

        if params.is_empty() {
            // No params — fixed args forwarding.
            // For chained no-param forwarding, use the resolved target but keep
            // the immediate block's args (they're constants, and intermediates
            // don't add params).
            forward_info.insert(
                block_id,
                ForwardInfo {
                    target: final_target,
                    param_remap: None,
                    fixed_args: br_args.clone(),
                },
            );
            continue;
        }

        // Build param value → param index map.
        let param_index: HashMap<ValueId, usize> = params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.value, i))
            .collect();

        // Check each Br arg is a block param.
        let mut remap = Vec::with_capacity(br_args.len());
        let mut all_params = true;
        for arg in br_args {
            if let Some(&idx) = param_index.get(arg) {
                remap.push(idx);
            } else {
                all_params = false;
                break;
            }
        }

        if all_params {
            // For parameterized blocks, use the direct target — transitive
            // resolution through parameterized intermediates is handled by fixpoint.
            forward_info.insert(
                block_id,
                ForwardInfo {
                    target: *direct_target,
                    param_remap: Some(remap),
                    fixed_args: vec![],
                },
            );
        }
    }

    // Now rewrite predecessors.
    for block_id in func.blocks.keys().collect::<Vec<_>>() {
        let inst_ids: Vec<_> = func.blocks[block_id].insts.clone();
        for inst_id in inst_ids {
            let targets = branch_targets(&func.insts[inst_id].op);
            for fwd_block in targets {
                let info = match forward_info.get(&fwd_block) {
                    Some(info) => info,
                    None => continue,
                };

                // Safety: skip if forwarding would create a self-loop.
                if info.target == block_id {
                    continue;
                }

                // Build the new args for the redirected branch.
                match &info.param_remap {
                    None => {
                        // Fixed args case: replace target and use the fixed args.
                        redirect_block_target_in_op(
                            &mut func.insts[inst_id].op,
                            fwd_block,
                            info.target,
                            Some(&info.fixed_args),
                        );
                        changed = true;
                    }
                    Some(remap) => {
                        // Remapped case: get the predecessor's current args for fwd_block,
                        // then remap them.
                        let pred_args = get_branch_args(&func.insts[inst_id].op, fwd_block);
                        if let Some(pred_args) = pred_args {
                            let new_args: Vec<ValueId> =
                                remap.iter().map(|&idx| pred_args[idx]).collect();
                            redirect_block_target_in_op(
                                &mut func.insts[inst_id].op,
                                fwd_block,
                                info.target,
                                Some(&new_args),
                            );
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    changed
}

/// Get the branch args from an Op for a specific target block.
fn get_branch_args(op: &Op, target: BlockId) -> Option<Vec<ValueId>> {
    match op {
        Op::Br {
            target: t, args, ..
        } if *t == target => Some(args.clone()),
        Op::BrIf {
            then_target,
            then_args,
            else_target,
            else_args,
            ..
        } => {
            if *then_target == target {
                Some(then_args.clone())
            } else if *else_target == target {
                Some(else_args.clone())
            } else {
                None
            }
        }
        Op::Switch {
            cases, default, ..
        } => {
            for (_, t, args) in cases {
                if *t == target {
                    return Some(args.clone());
                }
            }
            if default.0 == target {
                Some(default.1.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Phase 2: Merge blocks.
///
/// If block A ends with `Br { target: B, args }`, B has exactly one predecessor (A),
/// B is not the entry block, A ≠ B, and B is non-empty, merge B into A.
///
/// Returns true if any changes were made.
fn merge_blocks(func: &mut Function) -> bool {
    let mut changed = false;
    let preds = build_predecessor_map(func);

    for block_a in func.blocks.keys().collect::<Vec<_>>() {
        let a_insts = &func.blocks[block_a].insts;
        if a_insts.is_empty() {
            continue;
        }

        let last_inst_id = *a_insts.last().unwrap();
        let (target_b, br_args) = match &func.insts[last_inst_id].op {
            Op::Br { target, args } => (*target, args.clone()),
            _ => continue,
        };

        // B must not be the entry block.
        if target_b == func.entry {
            continue;
        }
        // A must not equal B.
        if block_a == target_b {
            continue;
        }
        // B must have exactly one predecessor.
        if preds.get(&target_b).map_or(0, |p| p.len()) != 1 {
            continue;
        }
        // B must be non-empty (already cleared blocks are skipped).
        if func.blocks[target_b].insts.is_empty() {
            continue;
        }

        // Build substitution: B's param values → A's branch args.
        let b_params: Vec<ValueId> = func.blocks[target_b]
            .params
            .iter()
            .map(|p| p.value)
            .collect();
        let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
        for (param_val, arg_val) in b_params.iter().zip(br_args.iter()) {
            subst.insert(*param_val, *arg_val);
        }

        // Take B's instructions.
        let b_insts: Vec<_> = func.blocks[target_b].insts.clone();

        // Rewrite operands in B's instructions using the substitution.
        // Also rewrite any branch targets from B back to B → A (self-references after merge).
        for &inst_id in &b_insts {
            substitute_values_in_op(&mut func.insts[inst_id].op, &subst);
            redirect_block_target_in_op(&mut func.insts[inst_id].op, target_b, block_a, None);
        }

        // Remove A's terminal Br.
        func.blocks[block_a].insts.pop();

        // Append B's instructions to A.
        func.blocks[block_a].insts.extend_from_slice(&b_insts);

        // Clear B.
        func.blocks[target_b].insts.clear();
        func.blocks[target_b].params.clear();

        changed = true;
    }

    changed
}

/// Phase 3: Cleanup unreachable blocks.
fn cleanup_unreachable(func: &mut Function) -> bool {
    let reachable = find_reachable_blocks(func);
    let mut changed = false;

    for block_id in func.blocks.keys().collect::<Vec<_>>() {
        if !reachable.contains(&block_id)
            && (!func.blocks[block_id].insts.is_empty()
                || !func.blocks[block_id].params.is_empty())
        {
            func.blocks[block_id].insts.clear();
            func.blocks[block_id].params.clear();
            changed = true;
        }
    }

    changed
}

/// Run CFG simplification on a single function.
/// Returns true if any changes were made.
fn simplify_cfg(func: &mut Function) -> bool {
    let mut any_changed = false;
    loop {
        let mut changed = false;
        changed |= forward_empty_blocks(func);
        changed |= merge_blocks(func);
        changed |= cleanup_unreachable(func);
        if !changed {
            break;
        }
        any_changed = true;
    }
    any_changed
}

impl Transform for CfgSimplify {
    fn name(&self) -> &str {
        "cfg-simplify"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            changed |= simplify_cfg(&mut module.functions[func_id]);
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

    fn apply_cfg_simplify(func: Function) -> Function {
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let result = CfgSimplify.apply(module).unwrap();
        result.module.functions[FuncId::new(0)].clone()
    }

    /// Empty block forwarded (no params): entry → B → C becomes entry → C,
    /// then C is merged into entry since it has only one predecessor.
    #[test]
    fn empty_block_forwarded_no_params() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let block_b = fb.create_block();
        let block_c = fb.create_block();

        // entry → B
        fb.br(block_b, &[]);

        // B → C (empty forwarder)
        fb.switch_to_block(block_b);
        fb.br(block_c, &[]);

        // C returns
        fb.switch_to_block(block_c);
        fb.ret(None);

        let func = apply_cfg_simplify(fb.build());

        // After forwarding entry→C, C has one predecessor (entry) so it gets merged.
        // Entry should now contain the return directly.
        let entry = func.entry;
        let last_inst = *func.blocks[entry].insts.last().unwrap();
        assert!(
            matches!(func.insts[last_inst].op, Op::Return(_)),
            "expected Return after forwarding + merge, got {:?}",
            func.insts[last_inst].op
        );
        // B and C should be cleared.
        assert!(func.blocks[block_b].insts.is_empty());
        assert!(func.blocks[block_c].insts.is_empty());
    }

    /// Identity forwarding: B has params and forwards them unchanged → bypassed,
    /// then C is merged into entry.
    #[test]
    fn identity_forwarding() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let (block_b, b_params) = fb.create_block_with_params(&[Type::Int(64)]);
        let (block_c, _c_params) = fb.create_block_with_params(&[Type::Int(64)]);

        // entry: const 42, br B(42)
        let val = fb.const_int(42);
        fb.br(block_b, &[val]);

        // B(p0): br C(p0) — identity forwarding
        fb.switch_to_block(block_b);
        fb.br(block_c, &[b_params[0]]);

        // C(p0): return p0
        fb.switch_to_block(block_c);
        fb.ret(Some(_c_params[0]));

        let func = apply_cfg_simplify(fb.build());

        // After forwarding entry→C and merging C into entry, entry should contain:
        // const 42, return(42)
        // (The return's operand gets substituted from C's param to the branch arg `val`.)
        let entry = func.entry;
        let last_inst = *func.blocks[entry].insts.last().unwrap();
        match &func.insts[last_inst].op {
            Op::Return(Some(v)) => assert_eq!(*v, val),
            other => panic!("expected Return(Some(val)), got {:?}", other),
        }
    }

    /// Remapped forwarding: B swaps param order in its Br → predecessor args rewritten,
    /// then C is merged into entry with substituted values.
    #[test]
    fn remapped_forwarding() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let (block_b, b_params) =
            fb.create_block_with_params(&[Type::Int(64), Type::Int(64)]);
        let (block_c, c_params) =
            fb.create_block_with_params(&[Type::Int(64), Type::Int(64)]);

        // entry: br B(10, 20)
        let v10 = fb.const_int(10);
        let v20 = fb.const_int(20);
        fb.br(block_b, &[v10, v20]);

        // B(p0, p1): br C(p1, p0) — swapped
        fb.switch_to_block(block_b);
        fb.br(block_c, &[b_params[1], b_params[0]]);

        // C(p0, p1): return p0
        fb.switch_to_block(block_c);
        fb.ret(Some(c_params[0]));

        let func = apply_cfg_simplify(fb.build());

        // After forwarding, entry→C with args (v20, v10).
        // C has one predecessor so it gets merged into entry.
        // C's return(p0) gets substituted: p0 → v20 (first arg passed to C).
        let entry = func.entry;
        let last_inst = *func.blocks[entry].insts.last().unwrap();
        match &func.insts[last_inst].op {
            Op::Return(Some(v)) => assert_eq!(*v, v20),
            other => panic!("expected Return(Some(v20)), got {:?}", other),
        }
    }

    /// Block merging: A branches to B (sole predecessor) → B merged into A, B cleared.
    #[test]
    fn block_merging() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let block_b = fb.create_block();

        // entry: br B()
        fb.br(block_b, &[]);

        // B: const 42, return 42
        fb.switch_to_block(block_b);
        let val = fb.const_int(42);
        fb.ret(Some(val));

        let func = apply_cfg_simplify(fb.build());

        // B should be cleared (merged into entry).
        assert!(func.blocks[block_b].insts.is_empty());

        // Entry should now contain B's instructions.
        let entry = func.entry;
        let ops: Vec<_> = func.blocks[entry]
            .insts
            .iter()
            .map(|id| &func.insts[*id].op)
            .collect();
        // Should have: const 42, return 42
        assert_eq!(ops.len(), 2);
        assert!(matches!(ops[0], Op::Const(_)));
        assert!(matches!(ops[1], Op::Return(Some(_))));
    }

    /// Entry block is never forwarded through.
    #[test]
    fn entry_block_preserved() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let block_b = fb.create_block();

        // Entry just branches to B (making entry an "empty" forwarder).
        // But entry should never be forwarded through since it's the entry block.
        fb.br(block_b, &[]);

        fb.switch_to_block(block_b);
        fb.ret(None);

        let func = apply_cfg_simplify(fb.build());

        // Entry should still exist with its branch (though B may be merged into it).
        // The key assertion: the function still works — entry is the start.
        let entry = func.entry;
        assert!(!func.blocks[entry].insts.is_empty());
    }

    /// Self-loop preserved: a block branching to itself is not broken.
    #[test]
    fn self_loop_preserved() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let loop_block = fb.create_block();

        // entry → loop_block
        fb.br(loop_block, &[]);

        // loop_block → loop_block (infinite loop)
        fb.switch_to_block(loop_block);
        fb.br(loop_block, &[]);

        let func = apply_cfg_simplify(fb.build());

        // loop_block should still branch to itself.
        // (It may have been merged into entry, but the self-loop should survive.)
        let entry = func.entry;
        let last_inst = *func.blocks[entry].insts.last().unwrap();
        match &func.insts[last_inst].op {
            Op::Br { target, .. } => {
                // Either loop_block still exists, or it was merged into entry
                // forming a self-loop on entry.
                assert!(
                    *target == loop_block || *target == entry,
                    "self-loop should be preserved"
                );
            }
            other => panic!("expected Br, got {:?}", other),
        }
    }

    /// Multiple predecessors prevent merge: B with 2+ predecessors stays separate.
    #[test]
    fn multiple_predecessors_prevent_merge() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let block_a = fb.create_block();
        let block_b = fb.create_block();

        let cond = fb.const_bool(true);

        // entry branches to both A and B.
        fb.br_if(cond, block_a, &[], block_b, &[]);

        // A → B
        fb.switch_to_block(block_a);
        fb.br(block_b, &[]);

        // B has two predecessors (entry and A), so it can't be merged.
        fb.switch_to_block(block_b);
        fb.ret(None);

        let func = apply_cfg_simplify(fb.build());

        // B should still have its return instruction (not merged away).
        assert!(!func.blocks[block_b].insts.is_empty());
    }

    /// Chained forwarding: A → B → C where B and C are both empty → resolved via fixpoint.
    #[test]
    fn chained_forwarding() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let block_b = fb.create_block();
        let block_c = fb.create_block();
        let block_d = fb.create_block();

        // entry → B
        fb.br(block_b, &[]);

        // B → C (empty)
        fb.switch_to_block(block_b);
        fb.br(block_c, &[]);

        // C → D (empty)
        fb.switch_to_block(block_c);
        fb.br(block_d, &[]);

        // D returns
        fb.switch_to_block(block_d);
        fb.ret(None);

        let func = apply_cfg_simplify(fb.build());

        // After simplification, entry should reach D directly (or D merged into entry).
        let entry = func.entry;
        let ops: Vec<_> = func.blocks[entry]
            .insts
            .iter()
            .map(|id| &func.insts[*id].op)
            .collect();

        // Should end with a return (D merged) or Br to D.
        let last = ops.last().unwrap();
        match last {
            Op::Return(_) => {} // D was merged all the way in
            Op::Br { target, .. } => assert_eq!(*target, block_d),
            other => panic!("expected Return or Br to D, got {:?}", other),
        }
    }
}
