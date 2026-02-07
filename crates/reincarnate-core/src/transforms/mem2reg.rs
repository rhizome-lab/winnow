use std::collections::{HashMap, HashSet};

use crate::error::CoreError;
use crate::ir::{Function, Module, Op, ValueId};
use crate::pipeline::{Transform, TransformResult};

use super::util::substitute_values_in_op;

/// Mem2Reg transform — promotes single-store Alloc/Store/Load patterns into
/// direct value references and eliminates Copy instructions.
///
/// This is a simplified form of the classic SSA mem2reg: for each `Alloc` whose
/// pointer is stored to exactly once, all `Load`s of that pointer are replaced
/// with the stored value, and the `Alloc`/`Store`/`Load` instructions are
/// removed. `Copy` instructions are also replaced by aliasing the result to
/// the source.
///
/// This pass should run before DCE so the now-dead instructions get cleaned up.
pub struct Mem2Reg;

/// Promote single-store alloc/store/load chains and copy aliases in a function.
/// Returns true if any changes were made.
fn promote_function(func: &mut Function) -> bool {
    let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
    let mut dead_insts: HashSet<crate::ir::InstId> = HashSet::new();

    // Phase 1: Find all Alloc results and count their Stores.
    let mut alloc_results: HashSet<ValueId> = HashSet::new();
    let mut store_count: HashMap<ValueId, usize> = HashMap::new();
    let mut store_value: HashMap<ValueId, ValueId> = HashMap::new();
    let mut store_inst: HashMap<ValueId, crate::ir::InstId> = HashMap::new();
    let mut alloc_inst: HashMap<ValueId, crate::ir::InstId> = HashMap::new();

    for (inst_id, inst) in func.insts.iter() {
        match &inst.op {
            Op::Alloc(_) => {
                if let Some(r) = inst.result {
                    alloc_results.insert(r);
                    alloc_inst.insert(r, inst_id);
                }
            }
            Op::Store { ptr, value } => {
                if alloc_results.contains(ptr) {
                    *store_count.entry(*ptr).or_default() += 1;
                    store_value.insert(*ptr, *value);
                    store_inst.insert(*ptr, inst_id);
                }
            }
            _ => {}
        }
    }

    // Phase 2: For single-store allocs, alias all loads to the stored value.
    let single_store: HashSet<ValueId> = store_count
        .iter()
        .filter(|(_, &c)| c == 1)
        .map(|(&ptr, _)| ptr)
        .collect();

    for (inst_id, inst) in func.insts.iter() {
        match &inst.op {
            Op::Load(ptr) if single_store.contains(ptr) => {
                if let Some(r) = inst.result {
                    subst.insert(r, store_value[ptr]);
                    dead_insts.insert(inst_id);
                }
            }
            Op::Copy(src) => {
                if let Some(r) = inst.result {
                    subst.insert(r, *src);
                    dead_insts.insert(inst_id);
                }
            }
            _ => {}
        }
    }

    // Mark alloc and store instructions for eliminated pointers as dead.
    for ptr in &single_store {
        if let Some(&id) = alloc_inst.get(ptr) {
            dead_insts.insert(id);
        }
        if let Some(&id) = store_inst.get(ptr) {
            dead_insts.insert(id);
        }
    }

    if subst.is_empty() {
        return false;
    }

    // Resolve transitive aliases: v3 → v2 → v1 becomes v3 → v1.
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

    // Propagate value names through promotions:
    // - Single-store allocs: transfer name from the alloc pointer to the stored value.
    for ptr in &single_store {
        if let Some(name) = func.value_names.remove(ptr) {
            let target = store_value[ptr];
            func.value_names.entry(target).or_insert(name);
        }
    }
    // - Copy elimination: transfer name from copy result to source.
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

    // Phase 3: Rewrite all surviving instructions to use the substitution map.
    let inst_ids: Vec<_> = func.insts.keys().collect();
    for inst_id in inst_ids {
        if dead_insts.contains(&inst_id) {
            continue;
        }
        substitute_values_in_op(&mut func.insts[inst_id].op, &subst);
    }

    // Phase 4: Remove dead instructions from all blocks.
    for block_id in func.blocks.keys().collect::<Vec<_>>() {
        func.blocks[block_id]
            .insts
            .retain(|id| !dead_insts.contains(id));
    }

    true
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
    use crate::entity::EntityRef;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::ty::FunctionSig;
    use crate::ir::{FuncId, Type, Visibility};

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
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("identity", sig, Visibility::Private);
        let param = fb.param(0);
        // Alloc → Store → Load chain.
        let ptr = fb.alloc(Type::Int(64));
        fb.store(ptr, param);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        // Alloc, Store, Load all removed; only Return remains.
        assert_eq!(func.blocks[entry].insts.len(), 1);
        // Return should reference the original param (v0), not the load result.
        let ret_inst = &func.insts[func.blocks[entry].insts[0]];
        if let Op::Return(Some(v)) = &ret_inst.op {
            assert_eq!(v.index(), 0, "return should reference param v0");
        } else {
            panic!("expected Return(Some(v0))");
        }
    }

    #[test]
    fn multi_store_alloc_not_promoted() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("multi", sig, Visibility::Private);
        let param = fb.param(0);
        let ptr = fb.alloc(Type::Int(64));
        fb.store(ptr, param);
        let two = fb.const_int(2);
        fb.store(ptr, two); // second store
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        // Multi-store: nothing should be removed (except Copy if any).
        // Alloc + Store + Const + Store + Load + Return = 6
        assert_eq!(func.blocks[entry].insts.len(), 6);
    }

    #[test]
    fn copy_eliminated() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("add_copy", sig, Visibility::Private);
        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        let copied = fb.copy(sum);
        fb.ret(Some(copied));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        // Copy removed; Add + Return remain.
        assert_eq!(func.blocks[entry].insts.len(), 2);
        // Return should reference the add result directly.
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
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("chain", sig, Visibility::Private);
        let param = fb.param(0);
        let c1 = fb.copy(param);
        let c2 = fb.copy(c1);
        let c3 = fb.copy(c2);
        fb.ret(Some(c3));

        let func = apply_mem2reg(fb.build());
        let entry = func.entry;
        // All copies removed; only Return remains.
        assert_eq!(func.blocks[entry].insts.len(), 1);
        let ret_inst = &func.insts[func.blocks[entry].insts[0]];
        if let Op::Return(Some(v)) = &ret_inst.op {
            assert_eq!(v.index(), 0, "transitive chain should resolve to v0");
        } else {
            panic!("expected Return(Some(v0))");
        }
    }

    #[test]
    fn unchanged_returns_false() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64),
        };
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
}
