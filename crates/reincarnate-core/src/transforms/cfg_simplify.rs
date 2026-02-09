use std::collections::{HashMap, HashSet, VecDeque};

use crate::error::CoreError;
use crate::ir::{BlockId, Function, Inst, Module, Op, Type, ValueId};
use crate::pipeline::{Transform, TransformResult};

use super::util::{branch_targets, substitute_values_in_op};

/// CFG simplification transform — removes redundant blocks and simplifies control flow.
///
/// Four phases per function, iterated to a fixed point:
/// 1. Forward empty blocks (blocks whose only instruction is an unconditional `Br`)
/// 2. Merge blocks (single-predecessor blocks absorbed into their predecessor)
/// 3. Eliminate trivial block parameters (all predecessors pass the same value)
/// 4. Cleanup unreachable blocks (clear instructions and params)
pub struct CfgSimplify;

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
                            // Validate all remap indices are within bounds.
                            if remap.iter().all(|&idx| idx < pred_args.len()) {
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
            // Propagate name from B's param to the substitution value.
            if let Some(name) = func.value_names.get(param_val).cloned() {
                func.value_names.entry(*arg_val).or_insert(name);
            }
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

/// Remove the branch argument at `index` from any branch targeting `target` in `op`.
fn remove_branch_arg_at(op: &mut Op, target: BlockId, index: usize) {
    match op {
        Op::Br {
            target: t, args, ..
        } if *t == target => {
            args.remove(index);
        }
        Op::BrIf {
            then_target,
            then_args,
            else_target,
            else_args,
            ..
        } => {
            if *then_target == target {
                then_args.remove(index);
            }
            if *else_target == target {
                else_args.remove(index);
            }
        }
        Op::Switch {
            cases, default, ..
        } => {
            for (_, t, args) in cases.iter_mut() {
                if *t == target {
                    args.remove(index);
                }
            }
            if default.0 == target {
                default.1.remove(index);
            }
        }
        _ => {}
    }
}

/// Collect all branch argument lists targeting `target` from an Op.
/// Unlike `get_branch_args` which returns the first match, this returns ALL
/// arg lists (e.g., both then_args and else_args if both target the same block).
fn collect_all_branch_args(op: &Op, target: BlockId) -> Vec<&Vec<ValueId>> {
    let mut result = Vec::new();
    match op {
        Op::Br {
            target: t, args, ..
        } if *t == target => {
            result.push(args);
        }
        Op::BrIf {
            then_target,
            then_args,
            else_target,
            else_args,
            ..
        } => {
            if *then_target == target {
                result.push(then_args);
            }
            if *else_target == target {
                result.push(else_args);
            }
        }
        Op::Switch {
            cases, default, ..
        } => {
            for (_, t, args) in cases {
                if *t == target {
                    result.push(args);
                }
            }
            if default.0 == target {
                result.push(&default.1);
            }
        }
        _ => {}
    }
    result
}

/// Phase 3: Eliminate trivial block parameters.
///
/// A block parameter is "trivial" when every incoming edge passes the same ValueId
/// for that parameter position. In that case the parameter can be removed and all
/// uses of the parameter value can be replaced with the single incoming value.
///
/// Returns true if any changes were made.
fn eliminate_trivial_params(func: &mut Function) -> bool {
    let reachable = find_reachable_blocks(func);
    let mut subst: HashMap<ValueId, ValueId> = HashMap::new();

    // Collect removals: (block, param indices to remove in reverse order).
    let mut removals: Vec<(BlockId, Vec<usize>)> = Vec::new();

    for block_id in func.blocks.keys().collect::<Vec<_>>() {
        if block_id == func.entry {
            continue;
        }
        if !reachable.contains(&block_id) {
            continue;
        }
        let params = &func.blocks[block_id].params;
        if params.is_empty() {
            continue;
        }

        // Collect all incoming arg lists for this block from all instructions.
        let mut incoming: Vec<&Vec<ValueId>> = Vec::new();
        for src_block in func.blocks.keys().collect::<Vec<_>>() {
            if !reachable.contains(&src_block) {
                continue;
            }
            for &inst_id in &func.blocks[src_block].insts {
                incoming.extend(collect_all_branch_args(&func.insts[inst_id].op, block_id));
            }
        }

        if incoming.is_empty() {
            continue;
        }

        let mut trivial_indices: Vec<usize> = Vec::new();

        for (i, param) in params.iter().enumerate() {
            let mut uniform_value: Option<ValueId> = None;
            let mut is_trivial = true;

            for args in &incoming {
                if i >= args.len() {
                    is_trivial = false;
                    break;
                }
                let val = args[i];
                match uniform_value {
                    None => uniform_value = Some(val),
                    Some(v) if v == val => {}
                    Some(_) => {
                        is_trivial = false;
                        break;
                    }
                }
            }

            if is_trivial {
                if let Some(v) = uniform_value {
                    // Propagate name from param to the single incoming value.
                    if let Some(name) = func.value_names.get(&param.value).cloned() {
                        func.value_names.entry(v).or_insert(name);
                    }
                    subst.insert(param.value, v);
                    trivial_indices.push(i);
                }
            }
        }

        if !trivial_indices.is_empty() {
            trivial_indices.reverse();
            removals.push((block_id, trivial_indices));
        }
    }

    if subst.is_empty() {
        return false;
    }

    // Resolve transitive substitutions: if subst has {A→B, B→C}, resolve A→C.
    // This prevents dangling references when an intermediate value's block
    // parameter was also eliminated.
    let resolved_subst: HashMap<ValueId, ValueId> = subst
        .keys()
        .map(|&from| {
            let mut val = subst[&from];
            let mut depth = 0;
            while let Some(&next) = subst.get(&val) {
                val = next;
                depth += 1;
                if depth > subst.len() {
                    break; // cycle guard
                }
            }
            (from, val)
        })
        .collect();
    let subst = resolved_subst;

    // Remove trivial params from blocks.
    let removal_map: HashMap<BlockId, &Vec<usize>> =
        removals.iter().map(|(b, idx)| (*b, idx)).collect();

    for (block_id, indices) in &removals {
        for &i in indices {
            func.blocks[*block_id].params.remove(i);
        }
    }

    // Remove branch args and apply substitutions only in reachable blocks.
    for &block_id in &reachable {
        for &inst_id in &func.blocks[block_id].insts {
            // Remove branch args from instructions that target affected blocks.
            // Deduplicate targets since remove_branch_arg_at handles all arms
            // of a BrIf/Switch in one call.
            let mut seen = HashSet::new();
            for target in branch_targets(&func.insts[inst_id].op) {
                if !seen.insert(target) {
                    continue;
                }
                if let Some(indices) = removal_map.get(&target) {
                    for &i in *indices {
                        remove_branch_arg_at(&mut func.insts[inst_id].op, target, i);
                    }
                }
            }

            // Apply value substitution.
            substitute_values_in_op(&mut func.insts[inst_id].op, &subst);
        }
    }

    true
}

/// Phase 4: Cleanup unreachable blocks.
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

/// Phase 5: Collapse same-target BrIf.
///
/// When a `BrIf` has `then_target == else_target`, replace it with a `Br`.
/// For argument positions where `then_args[i] != else_args[i]`, insert a
/// `Select { cond, on_true, on_false }` instruction to merge the values.
///
/// Returns true if any changes were made.
fn collapse_same_target_brif(func: &mut Function) -> bool {
    let mut changed = false;

    for block_id in func.blocks.keys().collect::<Vec<_>>() {
        let insts = func.blocks[block_id].insts.clone();
        let Some(&last_inst_id) = insts.last() else {
            continue;
        };

        let (cond, target, then_args, else_args) = match &func.insts[last_inst_id].op {
            Op::BrIf {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } if then_target == else_target => {
                (*cond, *then_target, then_args.clone(), else_args.clone())
            }
            _ => continue,
        };

        // Build unified args, inserting Select where they differ.
        let mut unified_args = Vec::with_capacity(then_args.len());
        for (t_arg, e_arg) in then_args.iter().zip(else_args.iter()) {
            if t_arg == e_arg {
                unified_args.push(*t_arg);
            } else {
                // Determine the result type from the target block's param.
                let param_ty = if unified_args.len() < func.blocks[target].params.len() {
                    func.blocks[target].params[unified_args.len()].ty.clone()
                } else {
                    Type::Dynamic
                };
                let result_val = func.value_types.push(param_ty);
                // Transfer the block param's name to the Select result.
                let param_idx = unified_args.len();
                if param_idx < func.blocks[target].params.len() {
                    let param_val = func.blocks[target].params[param_idx].value;
                    if let Some(name) = func.value_names.get(&param_val).cloned() {
                        func.value_names.insert(result_val, name);
                    }
                }
                let select_inst = func.insts.push(Inst {
                    op: Op::Select {
                        cond,
                        on_true: *t_arg,
                        on_false: *e_arg,
                    },
                    result: Some(result_val),
                    span: None,
                });
                // Insert the Select before the terminator.
                let term_pos = func.blocks[block_id].insts.len() - 1;
                func.blocks[block_id].insts.insert(term_pos, select_inst);
                unified_args.push(result_val);
            }
        }

        // Replace BrIf with Br.
        func.insts[last_inst_id].op = Op::Br {
            target,
            args: unified_args,
        };
        changed = true;
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
        changed |= collapse_same_target_brif(func);
        changed |= eliminate_trivial_params(func);
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
            return_ty: Type::Void, ..Default::default() };
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
            return_ty: Type::Int(64), ..Default::default() };
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
            return_ty: Type::Int(64), ..Default::default() };
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
            return_ty: Type::Int(64), ..Default::default() };
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
            return_ty: Type::Void, ..Default::default() };
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
            return_ty: Type::Void, ..Default::default() };
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

    /// Multiple predecessors prevent merge — but if forward_empty_blocks causes
    /// both arms of a BrIf to target B, the same-target collapse converts to Br,
    /// making B single-predecessor, so it gets merged into entry.
    #[test]
    fn multiple_predecessors_collapsed_via_same_target() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let block_a = fb.create_block();
        let block_b = fb.create_block();

        let cond = fb.const_bool(true);

        // entry branches to both A and B.
        fb.br_if(cond, block_a, &[], block_b, &[]);

        // A → B (empty forwarder)
        fb.switch_to_block(block_a);
        fb.br(block_b, &[]);

        // B: return
        fb.switch_to_block(block_b);
        fb.ret(None);

        let func = apply_cfg_simplify(fb.build());

        // After forwarding, BrIf targets B from both arms → collapses to Br(B) →
        // B merges into entry. Entry should end with Return.
        let entry = func.entry;
        let last_inst = *func.blocks[entry].insts.last().unwrap();
        assert!(
            matches!(func.insts[last_inst].op, Op::Return(_)),
            "expected Return after collapse + merge, got {:?}",
            func.insts[last_inst].op
        );
    }

    /// Chained forwarding: A → B → C where B and C are both empty → resolved via fixpoint.
    #[test]
    fn chained_forwarding() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
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

    /// Trivial param eliminated: both predecessors pass the same value → param removed.
    /// With same-target BrIf collapse, the empty forwarders get eliminated first,
    /// producing BrIf(cond, merge, [val], merge, [val]) → Br(merge, [val]) → merge merged.
    #[test]
    fn trivial_param_eliminated() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let (merge, merge_params) = fb.create_block_with_params(&[Type::Int(64)]);
        let then_block = fb.create_block();
        let else_block = fb.create_block();

        // entry: const 42, br_if → then / else
        let val = fb.const_int(42);
        let cond = fb.const_bool(true);
        fb.br_if(cond, then_block, &[], else_block, &[]);

        // then → merge(val)
        fb.switch_to_block(then_block);
        fb.br(merge, &[val]);

        // else → merge(val)  — same value as then
        fb.switch_to_block(else_block);
        fb.br(merge, &[val]);

        // merge(p0): return p0
        fb.switch_to_block(merge);
        fb.ret(Some(merge_params[0]));

        let func = apply_cfg_simplify(fb.build());

        // After forwarding + same-target collapse + merge, entry should return val directly.
        let entry = func.entry;
        let last_inst = *func.blocks[entry].insts.last().unwrap();
        match &func.insts[last_inst].op {
            Op::Return(Some(v)) => assert_eq!(*v, val),
            other => panic!("expected Return(Some(val)), got {:?}", other),
        }
    }

    /// Non-trivial param: predecessors pass different values → Select replaces the phi.
    #[test]
    fn non_trivial_param_becomes_select() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let (merge, merge_params) = fb.create_block_with_params(&[Type::Int(64)]);
        let then_block = fb.create_block();
        let else_block = fb.create_block();

        let val_a = fb.const_int(1);
        let val_b = fb.const_int(2);
        let cond = fb.const_bool(true);
        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        fb.br(merge, &[val_a]);

        fb.switch_to_block(else_block);
        fb.br(merge, &[val_b]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_params[0]));

        let func = apply_cfg_simplify(fb.build());

        // After forwarding + same-target collapse, a Select(cond, val_a, val_b) is
        // inserted and merge merges into entry. Entry should have Select + Return.
        let entry = func.entry;
        let has_select = func.blocks[entry]
            .insts
            .iter()
            .any(|&id| matches!(func.insts[id].op, Op::Select { .. }));
        assert!(has_select, "different args should produce Select");

        let last_inst = *func.blocks[entry].insts.last().unwrap();
        assert!(
            matches!(func.insts[last_inst].op, Op::Return(Some(_))),
            "should end with Return"
        );
    }

    /// Mixed params: 3 params, 2 trivial + 1 non-trivial → Select for the differing one.
    #[test]
    fn mixed_trivial_and_non_trivial_params() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);

        let (merge, merge_params) = fb.create_block_with_params(&[
            Type::Int(64),
            Type::Int(64),
            Type::Int(64),
        ]);
        let then_block = fb.create_block();
        let else_block = fb.create_block();

        let shared_a = fb.const_int(10);
        let shared_c = fb.const_int(30);
        let diff_then = fb.const_int(20);
        let diff_else = fb.const_int(21);
        let cond = fb.const_bool(true);
        fb.br_if(cond, then_block, &[], else_block, &[]);

        // then → merge(shared_a, diff_then, shared_c)
        fb.switch_to_block(then_block);
        fb.br(merge, &[shared_a, diff_then, shared_c]);

        // else → merge(shared_a, diff_else, shared_c)
        fb.switch_to_block(else_block);
        fb.br(merge, &[shared_a, diff_else, shared_c]);

        // merge(p0, p1, p2): return p1
        fb.switch_to_block(merge);
        fb.ret(Some(merge_params[1]));

        let func = apply_cfg_simplify(fb.build());

        // After forwarding + same-target collapse: Select for param 1 (different),
        // shared_a and shared_c passed directly. Then merge merges into entry.
        let entry = func.entry;
        let has_select = func.blocks[entry]
            .insts
            .iter()
            .any(|&id| matches!(func.insts[id].op, Op::Select { .. }));
        assert!(has_select, "differing param should produce Select");

        let last_inst = *func.blocks[entry].insts.last().unwrap();
        assert!(
            matches!(func.insts[last_inst].op, Op::Return(Some(_))),
            "should end with Return"
        );
    }

    /// BrIf where both arms target the same block with identical args → Br.
    #[test]
    fn collapse_same_target_brif_identical_args() {
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);
        let val = fb.param(1);

        let (merge, merge_params) = fb.create_block_with_params(&[Type::Int(64)]);

        // BrIf cond → merge(val), merge(val)
        fb.br_if(cond, merge, &[val], merge, &[val]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_params[0]));

        let func = apply_cfg_simplify(fb.build());

        // Should collapse to Br + eliminate trivial param.
        let entry = func.entry;
        let last_inst = *func.blocks[entry].insts.last().unwrap();
        // After merging, the return should reference val directly.
        match &func.insts[last_inst].op {
            Op::Return(Some(v)) => assert_eq!(*v, val),
            other => panic!("expected Return(Some(val)), got {:?}", other),
        }
        // No Select should be inserted since args are identical.
        let has_select = func.insts.values().any(|i| matches!(i.op, Op::Select { .. }));
        assert!(!has_select, "identical args should not produce Select");
    }

    /// BrIf where both arms target the same block with different args → Select + Br.
    #[test]
    fn collapse_same_target_brif_different_args() {
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);
        let a = fb.param(1);
        let b = fb.param(2);

        let (merge, merge_params) = fb.create_block_with_params(&[Type::Int(64)]);

        // BrIf cond → merge(a), merge(b)
        fb.br_if(cond, merge, &[a], merge, &[b]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_params[0]));

        let func = apply_cfg_simplify(fb.build());

        // After collapse + merge + trivial param elimination, entry should contain
        // a Select and a Return that uses the Select's result.
        let entry = func.entry;
        let has_select = func.blocks[entry]
            .insts
            .iter()
            .any(|&id| matches!(func.insts[id].op, Op::Select { .. }));
        assert!(has_select, "differing args should produce a Select");

        let last_inst = *func.blocks[entry].insts.last().unwrap();
        assert!(
            matches!(func.insts[last_inst].op, Op::Return(Some(_))),
            "should end with Return"
        );
    }

    /// BrIf with different targets is not collapsed.
    #[test]
    fn collapse_same_target_brif_preserves_different_targets() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);

        let block_a = fb.create_block();
        let block_b = fb.create_block();

        fb.br_if(cond, block_a, &[], block_b, &[]);

        fb.switch_to_block(block_a);
        fb.ret(None);

        fb.switch_to_block(block_b);
        fb.ret(None);

        let func = apply_cfg_simplify(fb.build());

        // Should still have a BrIf (different targets).
        let entry = func.entry;
        let has_brif = func.blocks[entry].insts.iter().any(|&id| {
            matches!(func.insts[id].op, Op::BrIf { .. })
        });
        assert!(
            has_brif,
            "different targets should preserve BrIf"
        );
    }
}
