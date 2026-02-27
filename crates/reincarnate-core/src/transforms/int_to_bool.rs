use std::collections::{BTreeMap, HashMap, HashSet};

use crate::error::CoreError;
use crate::ir::ty::parse_type_notation;
use crate::ir::{Constant, Function, Module, Op, Type, ValueId};
use crate::pipeline::{Transform, TransformResult};
use crate::project::ExternalMethodSig;

/// Demand-driven boolean promotion — identifies Int(0/1) values used in
/// boolean contexts and promotes them to Bool(false/true).
///
/// This pass reads `external_function_sigs` (populated from runtime
/// `function_signatures`) plus internal `FunctionSig` param types to find
/// positions that demand a `Bool`. It then traces backward through SSA to
/// find leaf constants. If all leaves are 0/1/Bool, the entire chain is
/// promoted to Bool.
///
/// Also infers Bool return types for functions that only return 0/1 constants
/// (replaces the former `BoolLiteralReturn` pass).
pub struct IntToBoolPromotion;

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

/// Rewrite leaf constants: Int(0) → Bool(false), Int(1) → Bool(true).
/// Also sets value_types to Bool for all values in the traced chain.
fn rewrite_leaves(func: &mut Function, leaves: &[ValueId]) {
    for &leaf_val in leaves {
        let inst_id = func
            .insts
            .keys()
            .find(|&id| func.insts[id].result == Some(leaf_val))
            .expect("leaf must have a producing instruction");

        let bool_val = match &func.insts[inst_id].op {
            Op::Const(Constant::Int(0)) => false,
            Op::Const(Constant::Int(1)) => true,
            Op::Const(Constant::Bool(b)) => *b,
            _ => unreachable!("trace_to_leaves guarantees bool-compatible leaves"),
        };

        func.insts[inst_id].op = Op::Const(Constant::Bool(bool_val));
        func.value_types[leaf_val] = Type::Bool;
    }
}

/// Set value_types to Bool for all intermediate values in a traced chain.
fn set_chain_types(func: &mut Function, start: ValueId) {
    let mut worklist = vec![start];
    let mut visited = HashSet::new();

    while let Some(v) = worklist.pop() {
        if !visited.insert(v) {
            continue;
        }
        func.value_types[v] = Type::Bool;

        let inst_id = func.insts.keys().find(|&id| func.insts[id].result == Some(v));

        if let Some(inst_id) = inst_id {
            match &func.insts[inst_id].op {
                Op::Const(_) => {} // leaf — already handled
                Op::Copy(src) | Op::Cast(src, _, _) => {
                    worklist.push(*src);
                }
                Op::Select {
                    on_true, on_false, ..
                } => {
                    worklist.push(*on_true);
                    worklist.push(*on_false);
                }
                _ => {}
            }
        } else {
            // Block param — trace incoming branch args
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
                    }
                }
            }
        }
    }
}

/// Build a map from function name → parsed param types from external sigs.
fn build_external_param_types(
    sigs: &BTreeMap<String, ExternalMethodSig>,
) -> HashMap<String, Vec<Type>> {
    sigs.iter()
        .map(|(name, sig)| {
            let types = sig.params.iter().map(|p| parse_type_notation(p)).collect();
            (name.clone(), types)
        })
        .collect()
}

/// Collect all values that are demanded as Bool in a function.
fn collect_bool_demands(
    func: &Function,
    external_param_types: &HashMap<String, Vec<Type>>,
    internal_sigs: &HashMap<String, Vec<Type>>,
    struct_fields: &HashMap<String, HashMap<String, Type>>,
) -> Vec<ValueId> {
    let mut demands = Vec::new();

    for inst_id in func.insts.keys() {
        let inst = &func.insts[inst_id];
        match &inst.op {
            // External function call: check param types from runtime sigs
            Op::Call { func: name, args } => {
                if let Some(param_types) = external_param_types.get(name.as_str()) {
                    for (i, arg) in args.iter().enumerate() {
                        if param_types.get(i) == Some(&Type::Bool) {
                            demands.push(*arg);
                        }
                    }
                }
                // Internal function call: check sig param types
                if let Some(param_types) = internal_sigs.get(name.as_str()) {
                    for (i, arg) in args.iter().enumerate() {
                        if param_types.get(i) == Some(&Type::Bool) {
                            demands.push(*arg);
                        }
                    }
                }
            }
            // SetField: if the target field is Bool-typed, demand bool for the value
            Op::SetField {
                object,
                field,
                value,
            } => {
                if let Type::Struct(name) = &func.value_types[*object] {
                    if let Some(field_ty) = struct_fields
                        .get(name)
                        .and_then(|fields| fields.get(field))
                    {
                        if *field_ty == Type::Bool {
                            demands.push(*value);
                        }
                    }
                }
            }
            // BrIf condition is always boolean
            Op::BrIf { cond, .. } => {
                demands.push(*cond);
            }
            // Not operand is boolean
            Op::Not(operand) => {
                demands.push(*operand);
            }
            _ => {}
        }
    }

    demands
}

/// Phase 1-3: Promote demanded Int(0/1) values to Bool within a function.
/// Returns true if any changes were made.
fn promote_demands(
    func: &mut Function,
    external_param_types: &HashMap<String, Vec<Type>>,
    internal_sigs: &HashMap<String, Vec<Type>>,
    struct_fields: &HashMap<String, HashMap<String, Type>>,
) -> bool {
    let demands = collect_bool_demands(func, external_param_types, internal_sigs, struct_fields);
    let mut changed = false;

    for demand_val in demands {
        // Skip values already typed as Bool
        if func.value_types[demand_val] == Type::Bool {
            continue;
        }

        if let Some(leaves) = trace_to_leaves(func, demand_val) {
            rewrite_leaves(func, &leaves);
            set_chain_types(func, demand_val);
            changed = true;
        }
    }

    changed
}

/// Returns true if the function body contains a `withInstances` system call.
///
/// Functions that call `withInstances` may have their real return value
/// propagated through the with-callback via a side channel (live_result).
/// The visible `Op::Return` paths in the outer function are only fallback paths
/// (e.g. `return false` when no instance is found), which would otherwise
/// cause `infer_bool_return` to falsely promote the return type to Bool.
fn function_has_with_instances(func: &Function) -> bool {
    for inst_id in func.insts.keys() {
        if let Op::SystemCall { system, method, .. } = &func.insts[inst_id].op {
            if system == "GameMaker.Instance" && method == "withInstances" {
                return true;
            }
        }
    }
    false
}

/// Phase 4: Infer Bool return type for functions that only return 0/1.
/// Returns true if the return type was changed.
fn infer_bool_return(func: &mut Function) -> bool {
    if func.sig.return_ty != Type::Dynamic {
        return false;
    }

    // Skip functions that contain withInstances calls — the real return value
    // may come from inside a with-body callback and is not visible in the
    // outer function's Op::Return instructions.
    if function_has_with_instances(func) {
        return false;
    }

    let mut return_vals = Vec::new();
    for block_id in func.blocks.keys() {
        for &inst_id in &func.blocks[block_id].insts {
            match &func.insts[inst_id].op {
                Op::Return(Some(val)) => return_vals.push(*val),
                // Bare return (Exit) — function has void-return paths,
                // so we cannot infer Bool return type.
                Op::Return(None) => return false,
                _ => {}
            }
        }
    }

    if return_vals.is_empty() {
        return false;
    }

    let mut all_leaves = HashSet::new();
    for &ret_val in &return_vals {
        match trace_to_leaves(func, ret_val) {
            Some(leaves) => all_leaves.extend(leaves),
            None => return false,
        }
    }

    // Rewrite leaves and set chain types for all return values
    let leaves_vec: Vec<ValueId> = all_leaves.into_iter().collect();
    rewrite_leaves(func, &leaves_vec);
    for &ret_val in &return_vals {
        set_chain_types(func, ret_val);
    }

    func.sig.return_ty = Type::Bool;
    true
}

/// Phase 5: Propagate return type changes to call sites.
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

impl Transform for IntToBoolPromotion {
    fn name(&self) -> &str {
        "int-to-bool-promotion"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;

        // Build external param type map once
        let external_param_types = build_external_param_types(&module.external_function_sigs);

        // Build struct field type map from module structs + external type defs
        let mut struct_fields: HashMap<String, HashMap<String, Type>> = HashMap::new();
        for s in &module.structs {
            let fields: HashMap<String, Type> = s
                .fields
                .iter()
                .map(|(n, t, _)| (n.clone(), t.clone()))
                .collect();
            struct_fields.insert(s.name.clone(), fields);
        }
        for (name, ext) in &module.external_type_defs {
            if !ext.fields.is_empty() {
                let fields: HashMap<String, Type> = ext
                    .fields
                    .iter()
                    .map(|(f, t)| (f.clone(), parse_type_notation(t)))
                    .collect();
                struct_fields
                    .entry(name.clone())
                    .or_default()
                    .extend(fields);
            }
        }

        // Build internal function sig param types map
        let internal_sigs: HashMap<String, Vec<Type>> = module
            .functions
            .keys()
            .map(|fid| {
                let f = &module.functions[fid];
                (f.name.clone(), f.sig.params.clone())
            })
            .collect();

        // Phase 1-3: Promote demanded values in each function
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            changed |= promote_demands(
                &mut module.functions[func_id],
                &external_param_types,
                &internal_sigs,
                &struct_fields,
            );
        }

        // Phase 4: Infer Bool return types
        let mut changed_funcs: HashSet<String> = HashSet::new();
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            if infer_bool_return(&mut module.functions[func_id]) {
                changed_funcs.insert(module.functions[func_id].name.clone());
                changed = true;
            }
        }

        // Phase 5: Cross-function propagation
        if !changed_funcs.is_empty() {
            for func_id in module.functions.keys().collect::<Vec<_>>() {
                changed |=
                    propagate_call_types(&mut module.functions[func_id], &changed_funcs);
            }

            // Re-run demand promotion with newly Bool-typed call results
            // (they may now feed into boolean demand positions)
            let updated_internal_sigs: HashMap<String, Vec<Type>> = module
                .functions
                .keys()
                .map(|fid| {
                    let f = &module.functions[fid];
                    (f.name.clone(), f.sig.params.clone())
                })
                .collect();

            for func_id in module.functions.keys().collect::<Vec<_>>() {
                changed |= promote_demands(
                    &mut module.functions[func_id],
                    &external_param_types,
                    &updated_internal_sigs,
                    &struct_fields,
                );
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

    // ---- Basic tests ----

    #[test]
    fn no_change_when_no_demands() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Public);
        let val = fb.const_int(42);
        fb.ret(Some(val));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(!result.changed);
    }

    #[test]
    fn promotes_int_at_bool_param_position() {
        // Create a function that calls an external function with Bool param
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("caller", sig, Visibility::Public);
        let zero = fb.const_int(0);
        fb.call("set_visible", &[zero], Type::Void);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let mut module = mb.build();

        // Add external sig: set_visible(boolean) → void
        module.external_function_sigs.insert(
            "set_visible".to_string(),
            ExternalMethodSig {
                params: vec!["boolean".to_string()],
                returns: "void".to_string(),
            },
        );

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions[FuncId::new(0)];
        // The const should now be Bool(false)
        for inst_id in func.insts.keys() {
            if let Op::Const(Constant::Bool(false)) = &func.insts[inst_id].op {
                let val = func.insts[inst_id].result.unwrap();
                assert_eq!(func.value_types[val], Type::Bool);
                return;
            }
        }
        panic!("Expected Bool(false) constant not found");
    }

    #[test]
    fn promotes_one_at_bool_param() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("caller", sig, Visibility::Public);
        let one = fb.const_int(1);
        fb.call("set_visible", &[one], Type::Void);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let mut module = mb.build();

        module.external_function_sigs.insert(
            "set_visible".to_string(),
            ExternalMethodSig {
                params: vec!["boolean".to_string()],
                returns: "void".to_string(),
            },
        );

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions[FuncId::new(0)];
        for inst_id in func.insts.keys() {
            if let Op::Const(Constant::Bool(true)) = &func.insts[inst_id].op {
                return; // Found it
            }
        }
        panic!("Expected Bool(true) constant not found");
    }

    #[test]
    fn does_not_promote_non_01() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("caller", sig, Visibility::Public);
        let two = fb.const_int(2);
        fb.call("set_visible", &[two], Type::Void);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let mut module = mb.build();

        module.external_function_sigs.insert(
            "set_visible".to_string(),
            ExternalMethodSig {
                params: vec!["boolean".to_string()],
                returns: "void".to_string(),
            },
        );

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(!result.changed);
    }

    #[test]
    fn promotes_through_block_params() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("caller", sig, Visibility::Public);
        let cond = fb.param(0);

        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Dynamic]);
        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let one = fb.const_int(1);
        fb.br(merge, &[one]);

        fb.switch_to_block(else_block);
        let zero = fb.const_int(0);
        fb.br(merge, &[zero]);

        fb.switch_to_block(merge);
        fb.call("set_visible", &[merge_vals[0]], Type::Void);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let mut module = mb.build();

        module.external_function_sigs.insert(
            "set_visible".to_string(),
            ExternalMethodSig {
                params: vec!["boolean".to_string()],
                returns: "void".to_string(),
            },
        );

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);

        // The merge value should be typed Bool
        let func = &result.module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[merge_vals[0]], Type::Bool);
    }

    #[test]
    fn infers_bool_return_type() {
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

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);
        assert_eq!(
            result.module.functions[FuncId::new(0)].sig.return_ty,
            Type::Bool
        );
    }

    #[test]
    fn propagates_bool_return_to_call_sites() {
        // Function that returns 0/1
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("is_ready", sig, Visibility::Public);
        let val = fb.const_int(1);
        fb.ret(Some(val));
        let bool_func = fb.build();

        // Caller that uses the result
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

        let result = IntToBoolPromotion.apply(module).unwrap();
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
    fn promotes_brif_condition() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Public);
        let one = fb.const_int(1);
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        fb.br_if(one, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        fb.ret(None);
        fb.switch_to_block(else_block);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions[FuncId::new(0)];
        for inst_id in func.insts.keys() {
            if let Op::Const(Constant::Bool(true)) = &func.insts[inst_id].op {
                return;
            }
        }
        panic!("Expected Bool(true) for BrIf condition");
    }

    #[test]
    fn promotes_not_operand() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Public);
        let zero = fb.const_int(0);
        let _negated = fb.not(zero);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);
    }

    #[test]
    fn internal_function_bool_param_demand() {
        // Function with Bool param
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("set_flag", sig, Visibility::Public);
        let _p = fb.param(0);
        fb.ret(None);
        let callee = fb.build();

        // Caller passing Int(1) to the Bool param
        let caller_sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let one = fb.const_int(1);
        fb.call("set_flag", &[one], Type::Void);
        fb.ret(None);
        let caller = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(callee);
        mb.add_function(caller);
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);
    }

    #[test]
    fn idempotent() {
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
        assert_idempotent(&IntToBoolPromotion, fb.build());
    }

    #[test]
    fn skip_already_bool() {
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
        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(!result.changed);
    }

    #[test]
    fn does_not_infer_bool_return_with_with_instances() {
        // A function with a withInstances call should NOT have its return type
        // inferred as Bool, even if the only visible return is `return false`.
        // The real return value is hidden inside the callback via live_result.
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("getNearestScreenType", sig, Visibility::Public);
        let self_param = fb.param(0);
        // Simulate: withInstances(obj, callback)
        fb.system_call("GameMaker.Instance", "withInstances", &[self_param], Type::Void);
        // Fallback: return false (0)
        let zero = fb.const_int(0);
        fb.ret(Some(zero));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        // Return type must NOT be changed to Bool
        assert_eq!(
            result.module.functions[FuncId::new(0)].sig.return_ty,
            Type::Dynamic,
        );
    }

    #[test]
    fn does_not_infer_bool_return_with_bare_exit() {
        // Function with one explicit `return 0` and one bare `exit` (Return(None)).
        // Should NOT infer Bool return — the bare exit produces `undefined`.
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("step", sig, Visibility::Public);
        let cond = fb.param(0);
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        fb.ret(None); // bare exit

        fb.switch_to_block(else_block);
        let zero = fb.const_int(0);
        fb.ret(Some(zero));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        // Return type must NOT be changed to Bool
        assert_eq!(
            result.module.functions[FuncId::new(0)].sig.return_ty,
            Type::Dynamic,
        );
    }

    #[test]
    fn promotes_int_at_bool_struct_field() {
        use crate::ir::module::StructDef;
        // Setting a Bool-typed field with Int(1) should promote to Bool(true).
        let sig = FunctionSig {
            params: vec![Type::Struct("Obj".into())],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);
        let obj = fb.param(0);
        let one = fb.const_int(1);
        fb.set_field(obj, "persistent", one);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        mb.add_struct(StructDef {
            name: "Obj".into(),
            namespace: vec![],
            fields: vec![("persistent".into(), Type::Bool, None)],
            visibility: Visibility::Public,
        });
        let module = mb.build();

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions[FuncId::new(0)];
        for inst_id in func.insts.keys() {
            if let Op::Const(Constant::Bool(true)) = &func.insts[inst_id].op {
                return;
            }
        }
        panic!("Expected Bool(true) constant for struct field assignment");
    }

    #[test]
    fn mixed_bool_and_non_bool_params() {
        // set_thing(number, boolean) — only second param should trigger demand
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("caller", sig, Visibility::Public);
        let forty_two = fb.const_int(42);
        let one = fb.const_int(1);
        fb.call("set_thing", &[forty_two, one], Type::Void);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let mut module = mb.build();

        module.external_function_sigs.insert(
            "set_thing".to_string(),
            ExternalMethodSig {
                params: vec!["number".to_string(), "boolean".to_string()],
                returns: "void".to_string(),
            },
        );

        let result = IntToBoolPromotion.apply(module).unwrap();
        assert!(result.changed);

        // 42 should still be Int, 1 should be Bool
        let func = &result.module.functions[FuncId::new(0)];
        let mut found_int42 = false;
        let mut found_bool_true = false;
        for inst_id in func.insts.keys() {
            match &func.insts[inst_id].op {
                Op::Const(Constant::Int(42)) => found_int42 = true,
                Op::Const(Constant::Bool(true)) => found_bool_true = true,
                _ => {}
            }
        }
        assert!(found_int42, "42 should remain Int");
        assert!(found_bool_true, "1 should become Bool(true)");
    }
}
