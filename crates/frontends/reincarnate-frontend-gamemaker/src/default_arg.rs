//! GML default argument recovery pass.
//!
//! GMS2.3+ compiles optional function parameters into the function body as:
//!
//! ```gml
//! if (argument0 === undefined) argument0 = default_value;
//! ```
//!
//! After IR translation and Mem2Reg, this becomes a chain of blocks:
//!
//! ```text
//! entry_block(..., v_arg: arg):
//!     v_undef = get_field self, "undefined"
//!     v_cmp = cmp.eq v_arg, v_undef
//!     br_if v_cmp, default_block, continue_block(v_arg)
//!
//! default_block:
//!     v_default = const <value>
//!     br continue_block(v_default)
//!
//! continue_block(v_resolved):
//!     ... next check or function body ...
//! ```
//!
//! This pass detects that pattern, extracts constant defaults, and sets
//! `FunctionSig.defaults[param_index] = Some(constant)` so the emitted
//! TypeScript has optional parameters, eliminating TS2554/TS2555 errors.
//!
//! The body check is left in place — it's redundant but harmless; removing it
//! would require block rewriting which isn't worth the complexity.

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::inst::CmpKind;
use reincarnate_core::ir::{BlockId, Constant, Function, Module, Op, ValueId};
use reincarnate_core::pipeline::{Transform, TransformResult};

pub struct GmlDefaultArgRecovery;

impl Transform for GmlDefaultArgRecovery {
    fn name(&self) -> &str {
        "gml-default-arg-recovery"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func in module.functions.values_mut() {
            changed |= recover_defaults(func);
        }
        Ok(TransformResult { module, changed })
    }
}

/// Try to recover default argument values from the entry block chain.
fn recover_defaults(func: &mut Function) -> bool {
    // Need at least 2 params (self + one arg) to have default-check patterns.
    if func.sig.params.len() < 2 {
        return false;
    }

    // Skip if any defaults are already set.
    if func.sig.defaults.iter().any(|d| d.is_some()) {
        return false;
    }

    // Collect recovered defaults: (param_index, constant).
    let defaults = scan_default_chain(func);
    if defaults.is_empty() {
        return false;
    }

    // Ensure defaults vec is long enough.
    while func.sig.defaults.len() < func.sig.params.len() {
        func.sig.defaults.push(None);
    }

    for (param_idx, constant) in &defaults {
        func.sig.defaults[*param_idx] = Some(constant.clone());
    }

    true
}

/// Walk the entry block chain looking for the `if (arg === undefined) arg = default` pattern.
/// Returns a list of (param_index, default_constant) pairs.
fn scan_default_chain(func: &Function) -> Vec<(usize, Constant)> {
    let mut results = Vec::new();
    let mut current_block = func.entry;

    // Collect entry block param values for identifying which param is being checked.
    let entry_params: Vec<ValueId> = func.blocks[func.entry]
        .params
        .iter()
        .map(|p| p.value)
        .collect();

    // We need at least one param (self) to do GetField on.
    if entry_params.is_empty() {
        return results;
    }

    let self_param = entry_params[0];

    while let Some((param_idx, constant, next_block)) =
        try_match_default_check(func, current_block, self_param, &entry_params)
    {
        results.push((param_idx, constant));
        current_block = next_block;
    }

    results
}

/// Try to match the default-check pattern in a single block.
///
/// Pattern:
///   v_undef = get_field self, "undefined"
///   v_cmp = cmp.eq param[N], v_undef
///   br_if v_cmp, then_block, else_block(param[N])
///
/// then_block:
///   v_default = const <value>
///   br else_block(v_default)
///
/// Returns (param_index, default_constant, else_block_id) on success.
fn try_match_default_check(
    func: &Function,
    block_id: BlockId,
    self_param: ValueId,
    entry_params: &[ValueId],
) -> Option<(usize, Constant, BlockId)> {
    let block = &func.blocks[block_id];
    let insts = &block.insts;

    // We need at least 2-3 instructions: possibly GetField, Cmp, BrIf.
    // The GetField for "undefined" might be in this block or a previous one,
    // so we need to find a Cmp that compares a param with an undefined value.

    // Find any value that is `get_field self, "undefined"` in this block.
    let mut undefined_val = None;
    for &inst_id in insts {
        let inst = &func.insts[inst_id];
        if let Op::GetField { object, field } = &inst.op {
            if *object == self_param && field == "undefined" {
                undefined_val = inst.result;
                break;
            }
        }
    }
    let undefined_val = undefined_val?;

    // Find a Cmp.Eq comparing an entry param with the undefined value.
    let mut cmp_val = None;
    let mut checked_param_idx = None;
    for &inst_id in insts {
        let inst = &func.insts[inst_id];
        if let Op::Cmp(CmpKind::Eq, lhs, rhs) = &inst.op {
            // Check if one side is an entry param and the other is the undefined sentinel.
            if *rhs == undefined_val {
                if let Some(idx) = entry_params.iter().position(|&p| p == *lhs) {
                    cmp_val = inst.result;
                    checked_param_idx = Some(idx);
                    break;
                }
            }
            if *lhs == undefined_val {
                if let Some(idx) = entry_params.iter().position(|&p| p == *rhs) {
                    cmp_val = inst.result;
                    checked_param_idx = Some(idx);
                    break;
                }
            }
        }
    }
    let cmp_val = cmp_val?;
    let checked_param_idx = checked_param_idx?;

    // Find the BrIf using this comparison result.
    for &inst_id in insts {
        let inst = &func.insts[inst_id];
        if let Op::BrIf {
            cond,
            then_target,
            then_args,
            else_target,
            else_args,
        } = &inst.op
        {
            if *cond != cmp_val {
                continue;
            }

            // The `then_target` is the default-assignment block (condition is true = arg is undefined).
            // The `else_target` is the continuation block (arg already has a value).
            let default_block = *then_target;
            let continue_block = *else_target;

            // Verify: then_args should be empty (default block doesn't take params from here).
            if !then_args.is_empty() {
                continue;
            }

            // Verify: else_args should pass the original param through.
            // (It forwards the existing argument value to the continuation block.)
            // We don't strictly need to verify this — the pattern is clear enough
            // from the GetField/Cmp/BrIf structure.

            // Check the default block: should have a Const + Br to continue_block.
            let default_blk = &func.blocks[default_block];

            // Find the constant and the branch in the default block.
            let mut found_const = None;
            let mut found_br = false;
            for &dinst_id in &default_blk.insts {
                let dinst = &func.insts[dinst_id];
                match &dinst.op {
                    Op::Const(c) => {
                        found_const = Some(c.clone());
                    }
                    Op::Br { target, args } => {
                        if *target == continue_block && args.len() == else_args.len() {
                            found_br = true;
                        }
                    }
                    _ => {}
                }
            }

            if let (Some(constant), true) = (found_const, found_br) {
                return Some((checked_param_idx, constant, continue_block));
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use reincarnate_core::ir::builder::FunctionBuilder;
    use reincarnate_core::ir::ModuleBuilder;
    use reincarnate_core::ir::ty::{FunctionSig, Type};
    use reincarnate_core::ir::Visibility;

    fn emit_constant(fb: &mut FunctionBuilder, c: &Constant) -> ValueId {
        match c {
            Constant::Null => fb.const_null(),
            Constant::Bool(b) => fb.const_bool(*b),
            Constant::Int(n) => fb.const_int(*n),
            Constant::UInt(n) => fb.const_uint(*n),
            Constant::Float(f) => fb.const_float(*f),
            Constant::String(s) => fb.const_string(s.as_str()),
        }
    }

    /// Build a function with the GML default-argument pattern:
    ///   if (arg === self.undefined) arg = default;
    fn build_test_function(defaults: &[Constant]) -> Module {
        // sig: (self, arg0, arg1, ...)
        let n_args = defaults.len();
        let mut params = vec![Type::Dynamic]; // self
        let mut sig_defaults = vec![None]; // self has no default
        for _ in 0..n_args {
            params.push(Type::Dynamic);
            sig_defaults.push(None);
        }
        let sig = FunctionSig {
            params,
            defaults: sig_defaults,
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("test_func", sig, Visibility::Public);

        let self_param = fb.param(0);

        // For each argument with a default, build the check chain.
        // Start from entry block.
        for (i, default_val) in defaults.iter().enumerate() {
            let arg_param = fb.param(1 + i);

            // get_field self, "undefined"
            let undef = fb.get_field(self_param, "undefined", Type::Dynamic);

            // cmp.eq arg, undef
            let cmp = fb.cmp(CmpKind::Eq, arg_param, undef);

            // Create default block and continue block
            let default_block = fb.create_block();
            let (continue_block, continue_vals) =
                fb.create_block_with_params(&[Type::Dynamic]);

            // br_if cmp, default_block, continue_block(arg_param)
            fb.br_if(cmp, default_block, &[], continue_block, &[arg_param]);

            // default_block: const default_val; br continue_block(const)
            fb.switch_to_block(default_block);
            let const_val = emit_constant(&mut fb, default_val);
            fb.br(continue_block, &[const_val]);

            // continue_block: continue building the chain
            fb.switch_to_block(continue_block);
            // continue_vals[0] is the resolved value — would be used by later code
            let _ = continue_vals;
        }

        // Final: return
        fb.ret(None);

        let func = fb.build();
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        mb.build()
    }

    #[test]
    fn test_single_default() {
        let module = build_test_function(&[Constant::Float(0.0)]);
        let pass = GmlDefaultArgRecovery;
        let result = pass.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions.values().next().unwrap();
        assert_eq!(func.sig.defaults.len(), 2); // self + arg0
        assert_eq!(func.sig.defaults[0], None); // self
        assert_eq!(func.sig.defaults[1], Some(Constant::Float(0.0))); // arg0
    }

    #[test]
    fn test_multiple_defaults() {
        let module = build_test_function(&[
            Constant::String("???".to_string()),
            Constant::Float(1.0),
            Constant::Bool(false),
        ]);
        let pass = GmlDefaultArgRecovery;
        let result = pass.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions.values().next().unwrap();
        assert_eq!(func.sig.defaults.len(), 4); // self + 3 args
        assert_eq!(func.sig.defaults[0], None);
        assert_eq!(
            func.sig.defaults[1],
            Some(Constant::String("???".to_string()))
        );
        assert_eq!(func.sig.defaults[2], Some(Constant::Float(1.0)));
        assert_eq!(func.sig.defaults[3], Some(Constant::Bool(false)));
    }

    #[test]
    fn test_no_defaults_no_change() {
        // Function with no default-check pattern.
        let sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Dynamic],
            defaults: vec![None, None],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("plain_func", sig, Visibility::Public);
        fb.ret(None);
        let func = fb.build();
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let pass = GmlDefaultArgRecovery;
        let result = pass.apply(module).unwrap();
        assert!(!result.changed);
    }
}
