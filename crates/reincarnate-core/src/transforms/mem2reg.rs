use std::collections::{HashMap, HashSet, VecDeque};

use crate::entity::EntityRef;
use crate::error::CoreError;
use crate::ir::block::BlockParam;
use crate::ir::{
    build_cfg, compute_dominators_lt, BlockId, Function, InstId, Module, Op, Type, ValueId,
};
use crate::pipeline::{Transform, TransformResult};

use super::util::{
    append_branch_arg_for_target, compute_dominance_frontiers, dom_tree_preorder,
    substitute_values_in_op, value_operands,
};

/// Mem2Reg transform — promotes Alloc/Store/Load patterns into direct value
/// references using SSA construction, and eliminates Copy instructions.
///
/// Three sub-passes:
/// 1. Copy elimination — replaces `Copy(v)` with `v`
/// 2. Single-store promotion — fast path for allocs stored to exactly once
/// 3. Multi-store SSA — full Cytron-style phi placement + rename for allocs
///    with multiple stores across different blocks
pub struct Mem2Reg;

/// Promote alloc/store/load chains and copy aliases in a function.
fn promote_function(func: &mut Function) -> bool {
    let copy_changed = eliminate_copies(func);
    let single_changed = promote_single_store(func);
    let multi_changed = promote_multi_store(func);
    copy_changed || single_changed || multi_changed
}

// -------------------------------------------------------------------------
// Sub-pass 1: Copy elimination
// -------------------------------------------------------------------------

fn eliminate_copies(func: &mut Function) -> bool {
    let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
    let mut dead_insts: HashSet<InstId> = HashSet::new();

    for (inst_id, inst) in func.insts.iter() {
        if let Op::Copy(src) = &inst.op {
            if let Some(r) = inst.result {
                subst.insert(r, *src);
                dead_insts.insert(inst_id);
            }
        }
    }

    if subst.is_empty() {
        return false;
    }

    resolve_transitive(&mut subst);

    // Transfer names from copy results to sources.
    for (inst_id, inst) in func.insts.iter() {
        if dead_insts.contains(&inst_id) {
            if let Op::Copy(src) = &inst.op {
                if let Some(r) = inst.result {
                    if let Some(name) = func.value_names.remove(&r) {
                        func.value_names.entry(*src).or_insert(name);
                    }
                }
            }
        }
    }

    apply_subst_and_remove(func, &subst, &dead_insts);
    true
}

// -------------------------------------------------------------------------
// Sub-pass 2: Single-store promotion
// -------------------------------------------------------------------------

fn promote_single_store(func: &mut Function) -> bool {
    let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
    let mut dead_insts: HashSet<InstId> = HashSet::new();

    let mut alloc_results: HashSet<ValueId> = HashSet::new();
    let mut store_count: HashMap<ValueId, usize> = HashMap::new();
    let mut store_value: HashMap<ValueId, ValueId> = HashMap::new();
    let mut store_inst_map: HashMap<ValueId, InstId> = HashMap::new();
    let mut alloc_inst_map: HashMap<ValueId, InstId> = HashMap::new();

    for (inst_id, inst) in func.insts.iter() {
        match &inst.op {
            Op::Alloc(_) => {
                if let Some(r) = inst.result {
                    alloc_results.insert(r);
                    alloc_inst_map.insert(r, inst_id);
                }
            }
            Op::Store { ptr, value } => {
                if alloc_results.contains(ptr) {
                    *store_count.entry(*ptr).or_default() += 1;
                    store_value.insert(*ptr, *value);
                    store_inst_map.insert(*ptr, inst_id);
                }
            }
            _ => {}
        }
    }

    let single_store: HashSet<ValueId> = store_count
        .iter()
        .filter(|(_, &c)| c == 1)
        .map(|(&ptr, _)| ptr)
        .collect();

    for (inst_id, inst) in func.insts.iter() {
        if let Op::Load(ptr) = &inst.op {
            if single_store.contains(ptr) {
                if let Some(r) = inst.result {
                    subst.insert(r, store_value[ptr]);
                    dead_insts.insert(inst_id);
                }
            }
        }
    }

    for ptr in &single_store {
        if let Some(&id) = alloc_inst_map.get(ptr) {
            dead_insts.insert(id);
        }
        if let Some(&id) = store_inst_map.get(ptr) {
            dead_insts.insert(id);
        }
    }

    if subst.is_empty() {
        return false;
    }

    resolve_transitive(&mut subst);

    // Transfer names: alloc pointer name → stored value.
    for ptr in &single_store {
        if let Some(name) = func.value_names.remove(ptr) {
            let target = store_value[ptr];
            func.value_names.entry(target).or_insert(name);
        }
    }

    apply_subst_and_remove(func, &subst, &dead_insts);
    true
}

// -------------------------------------------------------------------------
// Sub-pass 3: Multi-store SSA promotion (Cytron et al.)
// -------------------------------------------------------------------------

/// Info about a promotable alloc with multiple stores.
struct AllocInfo {
    alloc_inst: InstId,
    ty: Type,
    stores: Vec<(InstId, BlockId)>,
    loads: Vec<(InstId, BlockId)>,
}

fn promote_multi_store(func: &mut Function) -> bool {
    // Step 1: Find promotable multi-store allocs.
    // An alloc is promotable if its pointer only appears in Store(ptr) and Load(ptr) positions.
    let mut alloc_ptrs: HashMap<ValueId, (InstId, Type)> = HashMap::new();
    for (inst_id, inst) in func.insts.iter() {
        if let Op::Alloc(ty) = &inst.op {
            if let Some(r) = inst.result {
                alloc_ptrs.insert(r, (inst_id, ty.clone()));
            }
        }
    }

    if alloc_ptrs.is_empty() {
        return false;
    }

    // Check for escaping: any use of the alloc ptr that isn't Store{ptr} or Load(ptr).
    let mut escaped: HashSet<ValueId> = HashSet::new();
    for (_inst_id, inst) in func.insts.iter() {
        match &inst.op {
            Op::Store { ptr, value } => {
                // ptr position is fine; value position means escape
                if alloc_ptrs.contains_key(value) {
                    escaped.insert(*value);
                }
                // ptr must be the alloc — if ptr is an alloc, that's fine
                let _ = ptr;
            }
            Op::Load(_) => {
                // Load(ptr) is fine
            }
            op => {
                // Any other op using an alloc ptr means it escapes.
                for operand in value_operands(op) {
                    if alloc_ptrs.contains_key(&operand) {
                        escaped.insert(operand);
                    }
                }
            }
        }
    }

    // Build inst→block map.
    let mut inst_block: HashMap<InstId, BlockId> = HashMap::new();
    for (block_id, block) in func.blocks.iter() {
        for &inst_id in &block.insts {
            inst_block.insert(inst_id, block_id);
        }
    }

    // Collect per-alloc info for multi-store candidates.
    let mut alloc_infos: Vec<(ValueId, AllocInfo)> = Vec::new();
    for (&ptr, &(alloc_inst, ref ty)) in &alloc_ptrs {
        if escaped.contains(&ptr) {
            continue;
        }

        let mut stores = Vec::new();
        let mut loads = Vec::new();
        let mut store_count = 0usize;

        for (inst_id, inst) in func.insts.iter() {
            match &inst.op {
                Op::Store { ptr: p, .. } if *p == ptr => {
                    store_count += 1;
                    if let Some(&blk) = inst_block.get(&inst_id) {
                        stores.push((inst_id, blk));
                    }
                }
                Op::Load(p) if *p == ptr => {
                    if let Some(&blk) = inst_block.get(&inst_id) {
                        loads.push((inst_id, blk));
                    }
                }
                _ => {}
            }
        }

        // Only handle multi-store (single-store already handled above).
        if store_count >= 2 {
            alloc_infos.push((
                ptr,
                AllocInfo {
                    alloc_inst,
                    ty: ty.clone(),
                    stores,
                    loads,
                },
            ));
        }
    }

    if alloc_infos.is_empty() {
        return false;
    }

    // Sort by ptr ValueId index for deterministic processing order.
    alloc_infos.sort_by_key(|(ptr, _)| ptr.index());

    // Step 2: Build CFG, dominators, dominance frontiers.
    let cfg = build_cfg(func);
    let idom = compute_dominators_lt(func.entry, &cfg.preds, &cfg.succs);
    let df = compute_dominance_frontiers(&cfg.preds, &idom);
    let dom_order = dom_tree_preorder(func.entry, &idom);

    let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
    let mut dead_insts: HashSet<InstId> = HashSet::new();
    // Track phi placements: (block, alloc_index) → phi ValueId
    let mut phi_values: HashMap<(BlockId, usize), ValueId> = HashMap::new();

    // Step 3: Phi placement per alloc.
    for (alloc_idx, (ptr, info)) in alloc_infos.iter().enumerate() {
        let store_blocks: HashSet<BlockId> = info.stores.iter().map(|(_, b)| *b).collect();

        // Iterated dominance frontier worklist.
        let mut phi_blocks: HashSet<BlockId> = HashSet::new();
        let mut worklist: VecDeque<BlockId> = store_blocks.iter().copied().collect();
        while let Some(block) = worklist.pop_front() {
            if let Some(df_set) = df.get(&block) {
                for &df_block in df_set {
                    if phi_blocks.insert(df_block) {
                        worklist.push_back(df_block);
                    }
                }
            }
        }

        // Insert block params for phi nodes.
        for &phi_block in &phi_blocks {
            let phi_value = func.value_types.push(info.ty.clone());
            func.blocks[phi_block].params.push(BlockParam {
                value: phi_value,
                ty: info.ty.clone(),
            });
            phi_values.insert((phi_block, alloc_idx), phi_value);
        }

        // Dead stores alloc.
        if info.loads.is_empty() {
            dead_insts.insert(info.alloc_inst);
            for &(store_id, _) in &info.stores {
                dead_insts.insert(store_id);
            }
            // Remove phi block params we just added (no loads means no need).
            for &phi_block in &phi_blocks {
                if let Some(val) = phi_values.remove(&(phi_block, alloc_idx)) {
                    func.blocks[phi_block].params.retain(|p| p.value != val);
                }
            }
            continue;
        }

        // Step 4: Rename — preorder dom-tree walk.
        // Track the reaching definition at exit of each block so children in the
        // dom tree can restore the correct state.
        let mut reaching_at_exit: HashMap<BlockId, ValueId> = HashMap::new();

        // Create initial value (Const(Null)) at entry for "load before store" case.
        let initial_value = {
            let v = func.value_types.push(info.ty.clone());
            let init_inst_id = func.insts.push(crate::ir::Inst {
                op: Op::Const(crate::ir::Constant::Null),
                result: Some(v),
                span: None,
            });
            func.blocks[func.entry].insts.insert(0, init_inst_id);
            inst_block.insert(init_inst_id, func.entry);
            v
        };

        for &block in &dom_order {
            // Restore reaching def from this block's idom (or use initial).
            let mut current_def = if block == func.entry {
                initial_value
            } else if let Some(&idom_block) = idom.get(&block) {
                reaching_at_exit.get(&idom_block).copied().unwrap_or(initial_value)
            } else {
                initial_value
            };

            // If this block has a phi for this alloc, use the phi value.
            if let Some(&phi_val) = phi_values.get(&(block, alloc_idx)) {
                current_def = phi_val;
                // Transfer name from alloc to phi.
                if let Some(name) = func.value_names.get(ptr).cloned() {
                    func.value_names.entry(phi_val).or_insert(name);
                }
            }

            // Process instructions in order.
            let block_insts: Vec<InstId> = func.blocks[block].insts.clone();
            for &inst_id in &block_insts {
                let op = func.insts[inst_id].op.clone();
                match &op {
                    Op::Store { ptr: p, value } if *p == *ptr => {
                        current_def = *value;
                        dead_insts.insert(inst_id);
                        // Transfer name from alloc to stored value so the
                        // instruction result keeps the source-level variable
                        // name (e.g. v11 = Math.round(damage) → "damage").
                        if let Some(name) = func.value_names.get(ptr).cloned() {
                            func.value_names.entry(*value).or_insert(name);
                        }
                    }
                    Op::Load(p) if *p == *ptr => {
                        if let Some(r) = func.insts[inst_id].result {
                            subst.insert(r, current_def);
                            dead_insts.insert(inst_id);
                        }
                    }
                    _ => {}
                }
            }

            reaching_at_exit.insert(block, current_def);

            // For each successor with a phi: append current value as branch arg.
            if let Some(last_inst_id) = func.blocks[block].insts.last().copied() {
                let targets: Vec<BlockId> = {
                    let op = &func.insts[last_inst_id].op;
                    super::util::branch_targets(op)
                };
                for succ in targets {
                    if phi_values.contains_key(&(succ, alloc_idx)) {
                        append_branch_arg_for_target(
                            &mut func.insts[last_inst_id].op,
                            succ,
                            current_def,
                        );
                    }
                }
            }
        }

        // Mark alloc dead.
        dead_insts.insert(info.alloc_inst);

        // Transfer name from alloc pointer to first stored value.
        if let Some(name) = func.value_names.remove(ptr) {
            if let Some(&(_, first_val)) = info.stores.first() {
                let _ = first_val;
            }
            // Name goes to the initial value or phi — already handled above.
            // If not yet set, put it on initial.
            func.value_names.entry(initial_value).or_insert(name);
        }
    }

    if subst.is_empty() && dead_insts.is_empty() {
        return false;
    }

    resolve_transitive(&mut subst);
    apply_subst_and_remove(func, &subst, &dead_insts);
    true
}

// -------------------------------------------------------------------------
// Shared helpers
// -------------------------------------------------------------------------

/// Resolve transitive aliases: v3 → v2 → v1 becomes v3 → v1.
fn resolve_transitive(subst: &mut HashMap<ValueId, ValueId>) {
    loop {
        let mut changed = false;
        let snapshot: Vec<_> = subst.iter().map(|(k, v)| (*k, *v)).collect();
        for (key, target) in snapshot {
            if let Some(&next) = subst.get(&target) {
                if next != subst[&key] {
                    subst.insert(key, next);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
}

/// Apply a substitution map to all surviving instructions and remove dead ones.
fn apply_subst_and_remove(
    func: &mut Function,
    subst: &HashMap<ValueId, ValueId>,
    dead_insts: &HashSet<InstId>,
) {
    if !subst.is_empty() {
        let inst_ids: Vec<_> = func.insts.keys().collect();
        for inst_id in inst_ids {
            if dead_insts.contains(&inst_id) {
                continue;
            }
            substitute_values_in_op(&mut func.insts[inst_id].op, subst);
        }
        // Also substitute in block params (not typical, but belt-and-suspenders).
    }

    if !dead_insts.is_empty() {
        for block_id in func.blocks.keys().collect::<Vec<_>>() {
            func.blocks[block_id]
                .insts
                .retain(|id| !dead_insts.contains(id));
        }
    }
}

impl Transform for Mem2Reg {
    fn name(&self) -> &str {
        "mem2reg"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            changed |= promote_function(&mut module.functions[func_id]);
        }
        Ok(TransformResult { module, changed })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::ty::FunctionSig;
    use crate::ir::{FuncId, Visibility};

    fn apply_mem2reg(func: Function) -> Function {
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let result = Mem2Reg.apply(module).unwrap();
        result.module.functions[FuncId::new(0)].clone()
    }

    #[test]
    fn single_store_alloc_promoted() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("identity", sig, Visibility::Private);
        let param = fb.param(0);
        let ptr = fb.alloc(Type::Int(64));
        fb.store(ptr, param);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        assert_eq!(func.blocks[entry].insts.len(), 1);
        let ret_inst = &func.insts[func.blocks[entry].insts[0]];
        if let Op::Return(Some(v)) = &ret_inst.op {
            assert_eq!(v.index(), 0, "return should reference param v0");
        } else {
            panic!("expected Return(Some(v0))");
        }
    }

    #[test]
    fn multi_store_same_block() {
        // Two stores in the same block — load should get the last stored value.
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("multi_same", sig, Visibility::Private);
        let param = fb.param(0);
        let ptr = fb.alloc(Type::Int(64));
        fb.store(ptr, param);
        let two = fb.const_int(2);
        fb.store(ptr, two);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        // Alloc, both Stores, Load removed. Const(2) + Return remain.
        // Plus the initial Const(Null) inserted by multi-store at entry start.
        // But we should check the Return references the const 2.
        let ret_inst = func.blocks[entry]
            .insts
            .iter()
            .find(|&&id| matches!(func.insts[id].op, Op::Return(_)))
            .unwrap();
        if let Op::Return(Some(v)) = &func.insts[*ret_inst].op {
            // Should reference the const_int(2) value, not the param.
            assert_eq!(*v, two, "return should reference const 2");
        } else {
            panic!("expected Return");
        }
    }

    #[test]
    fn multi_store_diamond_phi() {
        // Diamond CFG:
        //   entry: alloc, store(ptr, param), br_if → then, else
        //   then: store(ptr, const 10), br → merge
        //   else: store(ptr, const 20), br → merge
        //   merge: load(ptr), return
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("diamond", sig, Visibility::Private);
        let cond = fb.param(0);
        let _param = fb.param(1);

        let ptr = fb.alloc(Type::Int(64));
        fb.store(ptr, _param);

        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let merge_block = fb.create_block();

        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let ten = fb.const_int(10);
        fb.store(ptr, ten);
        fb.br(merge_block, &[]);

        fb.switch_to_block(else_block);
        let twenty = fb.const_int(20);
        fb.store(ptr, twenty);
        fb.br(merge_block, &[]);

        fb.switch_to_block(merge_block);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());

        // The merge block should have a block param (phi).
        assert!(
            !func.blocks[merge_block].params.is_empty(),
            "merge block should have a phi param"
        );

        // The return should reference the phi value (a block param of merge).
        let phi_val = func.blocks[merge_block].params.last().unwrap().value;
        let ret_id = func.blocks[merge_block]
            .insts
            .iter()
            .find(|&&id| matches!(func.insts[id].op, Op::Return(_)))
            .unwrap();
        if let Op::Return(Some(v)) = &func.insts[*ret_id].op {
            assert_eq!(*v, phi_val, "return should reference phi value");
        } else {
            panic!("expected Return");
        }

        // The branches to merge should carry args.
        // then_block's terminator should pass const 10.
        let then_term = *func.blocks[then_block].insts.last().unwrap();
        if let Op::Br { args, .. } = &func.insts[then_term].op {
            assert!(!args.is_empty(), "then→merge should carry branch args");
            assert_eq!(args.last().copied(), Some(ten));
        } else {
            panic!("expected Br from then block");
        }

        // else_block's terminator should pass const 20.
        let else_term = *func.blocks[else_block].insts.last().unwrap();
        if let Op::Br { args, .. } = &func.insts[else_term].op {
            assert!(!args.is_empty(), "else→merge should carry branch args");
            assert_eq!(args.last().copied(), Some(twenty));
        } else {
            panic!("expected Br from else block");
        }
    }

    #[test]
    fn multi_store_loop_phi() {
        // Loop:
        //   entry: alloc, store(ptr, 0), br → header
        //   header: load(ptr), br_if(cond) → body, exit
        //   body: store(ptr, load+1), br → header
        //   exit: return loaded
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("loop_phi", sig, Visibility::Private);
        let cond = fb.param(0);

        let ptr = fb.alloc(Type::Int(64));
        let zero = fb.const_int(0);
        fb.store(ptr, zero);

        let header = fb.create_block();
        let body = fb.create_block();
        let exit = fb.create_block();

        fb.br(header, &[]);

        fb.switch_to_block(header);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.br_if(cond, body, &[], exit, &[]);

        fb.switch_to_block(body);
        let one = fb.const_int(1);
        let sum = fb.add(loaded, one);
        fb.store(ptr, sum);
        fb.br(header, &[]);

        fb.switch_to_block(exit);
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());

        // Header should have a phi param (loop carries value back).
        assert!(
            !func.blocks[header].params.is_empty(),
            "header should have a phi param for loop variable"
        );

        // No Load or Store instructions should remain in any block.
        for (_blk_id, block) in func.blocks.iter() {
            for &inst_id in &block.insts {
                assert!(
                    !matches!(func.insts[inst_id].op, Op::Load(_)),
                    "no Load should remain after promotion"
                );
                assert!(
                    !matches!(func.insts[inst_id].op, Op::Store { .. }),
                    "no Store should remain after promotion"
                );
            }
        }
    }

    #[test]
    fn escaped_alloc_not_promoted() {
        // Alloc whose pointer is passed to a Call — should not be promoted.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("escaped", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Int(64));
        let one = fb.const_int(1);
        fb.store(ptr, one);
        let two = fb.const_int(2);
        fb.store(ptr, two);
        // Pass the alloc ptr to a call — this escapes it.
        let _call_result = fb.call("consume", &[ptr], Type::Void);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        // Should still have Load and Store since alloc escaped.
        let has_load = func.blocks[entry]
            .insts
            .iter()
            .any(|&id| matches!(func.insts[id].op, Op::Load(_)));
        assert!(has_load, "escaped alloc should keep Load");
    }

    #[test]
    fn no_store_alloc_skipped() {
        // Alloc with loads but no stores — should not crash.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("no_store", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Int(64));
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        // Should not panic.
        let _func = apply_mem2reg(fb.build());
    }

    #[test]
    fn copy_eliminated() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("add_copy", sig, Visibility::Private);
        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        let copied = fb.copy(sum);
        fb.ret(Some(copied));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        assert_eq!(func.blocks[entry].insts.len(), 2);
        let ret_inst = &func.insts[*func.blocks[entry].insts.last().unwrap()];
        if let Op::Return(Some(v)) = &ret_inst.op {
            assert_eq!(v.index(), 2, "return should reference add result v2");
        } else {
            panic!("expected Return(Some(v2))");
        }
    }

    #[test]
    fn transitive_chain() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("chain", sig, Visibility::Private);
        let param = fb.param(0);
        let c1 = fb.copy(param);
        let c2 = fb.copy(c1);
        let c3 = fb.copy(c2);
        fb.ret(Some(c3));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        assert_eq!(func.blocks[entry].insts.len(), 1);
        let ret_inst = &func.insts[func.blocks[entry].insts[0]];
        if let Op::Return(Some(v)) = &ret_inst.op {
            assert_eq!(v.index(), 0, "transitive chain should resolve to v0");
        } else {
            panic!("expected Return(Some(v0))");
        }
    }

    // ---- Identity & idempotency tests ----

    /// Idempotent: second run reports no change after arena compaction.
    ///
    /// Mem2Reg leaves dead instructions in the arena (the pipeline's compact_insts
    /// handles cleanup). We compact between runs to test semantic idempotency.
    #[test]
    fn idempotent_after_transform() {
        // Test with alloc/store/load — the full promotion path.
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0);
        let ptr = fb.alloc(Type::Int(64));
        fb.store(ptr, p);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let r1 = Mem2Reg.apply(module).unwrap();
        assert!(r1.changed);
        // Compact to remove dead instructions left by promotion.
        let mut module = r1.module;
        for func in module.functions.values_mut() {
            func.compact_insts();
        }
        let r2 = Mem2Reg.apply(module).unwrap();
        assert!(!r2.changed, "mem2reg should be idempotent after compaction");
    }

    #[test]
    fn unchanged_returns_false() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("add", sig, Visibility::Private);
        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = Mem2Reg.apply(module).unwrap();
        assert!(!result.changed, "no alloc/copy to eliminate → unchanged");
    }

    // ---- Edge case tests ----

    /// Alloc with only a load (no store) — load remains (undef).
    #[test]
    fn alloc_no_store_load_remains() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Int(64));
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        // Single-store promotion requires exactly 1 store — 0 stores means
        // neither sub-pass handles it. Load should remain or get a null init.
        let entry = func.entry;
        let has_return = func.blocks[entry].insts.iter()
            .any(|&id| matches!(func.insts[id].op, Op::Return(Some(_))));
        assert!(has_return, "function should still return a value");
    }

    /// Multiple stores in the same block — last store wins.
    #[test]
    fn alloc_multiple_stores_same_block_last_wins() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Int(64));
        let a = fb.const_int(10);
        fb.store(ptr, a);
        let b = fb.const_int(20);
        fb.store(ptr, b);
        let c = fb.const_int(30);
        fb.store(ptr, c);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        let ret = func.blocks[entry].insts.iter()
            .find(|&&id| matches!(func.insts[id].op, Op::Return(_)))
            .unwrap();
        if let Op::Return(Some(v)) = &func.insts[*ret].op {
            assert_eq!(*v, c, "should return last stored value");
        }
    }

    /// Store in loop — phi placement for back-edge.
    #[test]
    fn alloc_store_in_loop_phi() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);
        let ptr = fb.alloc(Type::Int(64));
        let init = fb.const_int(0);
        fb.store(ptr, init);

        let header = fb.create_block();
        let body = fb.create_block();
        let exit = fb.create_block();

        fb.br(header, &[]);

        fb.switch_to_block(header);
        fb.br_if(cond, body, &[], exit, &[]);

        fb.switch_to_block(body);
        let one = fb.const_int(1);
        fb.store(ptr, one);
        fb.br(header, &[]);

        fb.switch_to_block(exit);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        // Header should have a phi param from the back-edge.
        assert!(
            !func.blocks[header].params.is_empty(),
            "header should have phi params from loop back-edge"
        );
    }
}
