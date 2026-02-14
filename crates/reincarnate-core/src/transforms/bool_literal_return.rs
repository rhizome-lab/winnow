use std::collections::HashSet;

use crate::error::CoreError;
use crate::ir::{Constant, Function, Module, Op, Type, ValueId};
use crate::pipeline::{Transform, TransformResult};

/// Bool literal return inference — rewrites functions that return only 0/1
/// integer constants to return `true`/`false` with `return_ty = Bool`.
///
/// This enables downstream `RedundantCastElimination` to remove `Cast(v, Bool)`
/// at call sites, eliminating `Boolean()` wrappers in the output.
pub struct BoolLiteralReturn;

/// Trace a value backward through the SSA graph to find all leaf constants.
///
/// Returns `Some(leaves)` if all leaves are bool-compatible constants
/// (Int(0), Int(1), Bool(_)), or `None` if any non-bool leaf is found.
fn trace_to_leaves(func: &Function, start: ValueId) -> Option<Vec<ValueId>> {
    let mut leaves = Vec::new();
    let mut worklist = vec![start];
    let mut visited = HashSet::new();

    while let Some(v) = worklist.pop() {
        if !visited.insert(v) {
            continue;
        }

        // Find the instruction that produces this value.
        let inst_id = func.insts.keys().find(|&id| func.insts[id].result == Some(v));

        if let Some(inst_id) = inst_id {
            match &func.insts[inst_id].op {
                Op::Const(Constant::Int(0 | 1) | Constant::Bool(_)) => {
                    leaves.push(v);
                }
                Op::Const(_) => return None,
                Op::Copy(src) | Op::Cast(src, _, _) => {
                    worklist.push(*src);
                }
                Op::Select {
                    on_true, on_false, ..
                } => {
                    worklist.push(*on_true);
                    worklist.push(*on_false);
                }
                _ => return None,
            }
        } else {
            // Value is a block param — trace incoming args from all branches.
            let mut found = false;
            for block_id in func.blocks.keys() {
                let block = &func.blocks[block_id];
                for (param_idx, param) in block.params.iter().enumerate() {
                    if param.value == v {
                        for src_block_id in func.blocks.keys() {
                            for &src_inst_id in &func.blocks[src_block_id].insts {
                                let args_for_block = branch_args_for_target(
                                    &func.insts[src_inst_id].op,
                                    block_id,
                                );
                                for args in args_for_block {
                                    if param_idx < args.len() {
                                        worklist.push(args[param_idx]);
                                    }
                                }
                            }
                        }
                        found = true;
                    }
                }
            }
            if !found {
                return None;
            }
        }
    }

    if leaves.is_empty() {
        None
    } else {
        Some(leaves)
    }
}

/// Extract the argument lists targeting a specific block from a branch op.
fn branch_args_for_target(op: &Op, target: crate::ir::BlockId) -> Vec<&[ValueId]> {
    let mut result = Vec::new();
    match op {
        Op::Br {
            target: t, args, ..
        } => {
            if *t == target {
                result.push(args.as_slice());
            }
        }
        Op::BrIf {
            then_target,
            then_args,
            else_target,
            else_args,
            ..
        } => {
            if *then_target == target {
                result.push(then_args.as_slice());
            }
            if *else_target == target {
                result.push(else_args.as_slice());
            }
        }
        Op::Switch {
            cases, default, ..
        } => {
            for (_, block, args) in cases {
                if *block == target {
                    result.push(args.as_slice());
                }
            }
            if default.0 == target {
                result.push(default.1.as_slice());
            }
        }
        _ => {}
    }
    result
}

/// Analyze and rewrite a function's return values from Int(0/1) to Bool.
///
/// Rewrites the leaf constant instructions in place, changing `Int(0)` to
/// `Bool(false)` and `Int(1)` to `Bool(true)`, then sets `return_ty = Bool`.
fn infer_bool_return(func: &mut Function) -> bool {
    if func.sig.return_ty != Type::Dynamic {
        return false;
    }

    // Collect all Return values.
    let return_vals: Vec<ValueId> = func
        .blocks
        .keys()
        .flat_map(|block_id| {
            func.blocks[block_id]
                .insts
                .iter()
                .filter_map(|&inst_id| {
                    if let Op::Return(Some(val)) = &func.insts[inst_id].op {
                        Some(*val)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect();

    if return_vals.is_empty() {
        return false;
    }

    // Trace all return values to their leaf constants.
    let mut all_leaves = HashSet::new();
    for &ret_val in &return_vals {
        match trace_to_leaves(func, ret_val) {
            Some(leaves) => all_leaves.extend(leaves),
            None => return false,
        }
    }

    // Rewrite each leaf constant: Int(0) → Bool(false), Int(1) → Bool(true).
    for leaf_val in &all_leaves {
        let inst_id = func
            .insts
            .keys()
            .find(|&id| func.insts[id].result == Some(*leaf_val))
            .expect("leaf must have a producing instruction");

        let bool_val = match &func.insts[inst_id].op {
            Op::Const(Constant::Int(0)) => false,
            Op::Const(Constant::Int(1)) => true,
            Op::Const(Constant::Bool(b)) => *b,
            _ => unreachable!("trace_to_leaves guarantees bool-compatible leaves"),
        };

        func.insts[inst_id].op = Op::Const(Constant::Bool(bool_val));
        func.value_types[*leaf_val] = Type::Bool;
    }

    func.sig.return_ty = Type::Bool;
    true
}

/// Update call-site value types for functions whose return type changed to Bool.
fn propagate_call_types(func: &mut Function, changed_funcs: &HashSet<String>) -> bool {
    let mut changed = false;

    for inst_id in func.insts.keys().collect::<Vec<_>>() {
        let target_name = match &func.insts[inst_id].op {
            Op::Call { func: name, .. } => Some(name.clone()),
            _ => None,
        };

        if let Some(name) = target_name {
            if changed_funcs.contains(&name) {
                if let Some(result_val) = func.insts[inst_id].result {
                    if func.value_types[result_val] != Type::Bool {
                        func.value_types[result_val] = Type::Bool;
                        changed = true;
                    }
                }
            }
        }
    }

    changed
}

impl Transform for BoolLiteralReturn {
    fn name(&self) -> &str {
        "bool-literal-return"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        let mut changed_funcs: HashSet<String> = HashSet::new();

        // Phase 1: analyze & rewrite each function.
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            if infer_bool_return(&mut module.functions[func_id]) {
                changed_funcs.insert(module.functions[func_id].name.clone());
                changed = true;
            }
        }

        // Phase 2: propagate return types to call sites.
        if !changed_funcs.is_empty() {
            for func_id in module.functions.keys().collect::<Vec<_>>() {
                changed |= propagate_call_types(&mut module.functions[func_id], &changed_funcs);
            }
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
    use crate::ir::{FuncId, Visibility};

    // ---- Identity & idempotency tests ----

    /// Function with already-Bool return type → no changes.
    #[test]
    fn identity_no_change() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Public);
        let v = fb.const_bool(true);
        fb.ret(Some(v));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(!result.changed);
    }

    /// Bool literal return is idempotent.
    #[test]
    fn idempotent_after_transform() {
        use crate::transforms::util::test_helpers::assert_idempotent;
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Public);
        let cond = fb.param(0);
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        fb.br_if(cond, then_block, &[], else_block, &[]);
        fb.switch_to_block(then_block);
        let one = fb.const_int(1);
        fb.ret(Some(one));
        fb.switch_to_block(else_block);
        let zero = fb.const_int(0);
        fb.ret(Some(zero));
        assert_idempotent(&BoolLiteralReturn, fb.build());
    }

    #[test]
    fn basic_bool_inference() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("is_ready", sig, Visibility::Public);

        let cond = fb.param(0);
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let one = fb.const_int(1);
        fb.ret(Some(one));

        fb.switch_to_block(else_block);
        let zero = fb.const_int(0);
        fb.ret(Some(zero));

        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions[FuncId::new(0)];
        assert_eq!(func.sig.return_ty, Type::Bool);

        for block_id in func.blocks.keys() {
            for &inst_id in &func.blocks[block_id].insts {
                if let Op::Const(Constant::Bool(_)) = &func.insts[inst_id].op {
                    let val = func.insts[inst_id].result.unwrap();
                    assert_eq!(func.value_types[val], Type::Bool);
                }
            }
        }
    }

    #[test]
    fn non_bool_return_unchanged() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("get_count", sig, Visibility::Public);
        let val = fb.const_int(42);
        fb.ret(Some(val));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(!result.changed);
        assert_eq!(
            result.module.functions[FuncId::new(0)].sig.return_ty,
            Type::Dynamic
        );
    }

    #[test]
    fn already_typed_skipped() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("is_done", sig, Visibility::Public);
        let val = fb.const_bool(true);
        fb.ret(Some(val));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(!result.changed);
    }

    #[test]
    fn through_block_params() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("check", sig, Visibility::Public);

        let cond = fb.param(0);
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let (merge_block, merge_vals) = fb.create_block_with_params(&[Type::Dynamic]);

        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let one = fb.const_int(1);
        fb.br(merge_block, &[one]);

        fb.switch_to_block(else_block);
        let zero = fb.const_int(0);
        fb.br(merge_block, &[zero]);

        fb.switch_to_block(merge_block);
        fb.ret(Some(merge_vals[0]));

        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(result.changed);
        assert_eq!(
            result.module.functions[FuncId::new(0)].sig.return_ty,
            Type::Bool
        );
    }

    #[test]
    fn call_site_propagation() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("is_ready", sig, Visibility::Public);
        let val = fb.const_int(1);
        fb.ret(Some(val));
        let bool_func = fb.build();

        let caller_sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let result = fb.call("is_ready", &[], Type::Dynamic);
        let _cast = fb.coerce(result, Type::Bool);
        fb.ret(None);
        let caller_func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(bool_func);
        mb.add_function(caller_func);
        let module = mb.build();

        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(result.changed);

        let caller = &result.module.functions[FuncId::new(1)];
        for inst_id in caller.insts.keys() {
            if let Op::Call { func: name, .. } = &caller.insts[inst_id].op {
                if name == "is_ready" {
                    let result_val = caller.insts[inst_id].result.unwrap();
                    assert_eq!(caller.value_types[result_val], Type::Bool);
                }
            }
        }
    }

    #[test]
    fn void_function_skipped() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("do_stuff", sig, Visibility::Public);
        fb.ret(None);
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(!result.changed);
    }

    // ---- Edge case tests ----

    /// Returns 0 and 2 → not bool, unchanged.
    #[test]
    fn returns_two_not_bool() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Public);
        let cond = fb.param(0);
        let then_b = fb.create_block();
        let else_b = fb.create_block();
        fb.br_if(cond, then_b, &[], else_b, &[]);

        fb.switch_to_block(then_b);
        let zero = fb.const_int(0);
        fb.ret(Some(zero));

        fb.switch_to_block(else_b);
        let two = fb.const_int(2);
        fb.ret(Some(two));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(!result.changed, "0 and 2 should not be converted to Bool");
    }

    /// Function already returning Bool → no change.
    #[test]
    fn already_bool_return_no_change() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Public);
        let v = fb.const_bool(false);
        fb.ret(Some(v));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(!result.changed);
    }

    #[test]
    fn mixed_returns_unchanged() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("mixed", sig, Visibility::Public);

        let cond = fb.param(0);
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let one = fb.const_int(1);
        fb.ret(Some(one));

        fb.switch_to_block(else_block);
        let val = fb.const_int(42);
        fb.ret(Some(val));

        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = BoolLiteralReturn.apply(module).unwrap();
        assert!(!result.changed);
    }
}
