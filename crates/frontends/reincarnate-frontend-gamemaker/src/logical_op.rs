//! GML logical-op pattern normalization.
//!
//! GML bytecode compiles `a || b` as:
//!   if a { result = 1.0 } else { result = b }
//! and `a && b` as:
//!   if a { result = b } else { result = 0.0 }
//!
//! After Mem2Reg these become block-param patterns:
//!   BrIf cond, then_block[], else_block[]
//!   then_block: { Const(1.0), Br merge(v_1) }  ← trivially pure, passes truthy
//!   else_block: { ...real computation..., Br merge(v_rhs) }
//!
//! The shared structurizer detects the *canonical* `||`/`&&` form where the
//! short-circuit branch forwards the condition itself (not a constant 1/0).
//! This pass normalizes the const-truthy/falsy arg to `v_cond` so the standard
//! detection fires, emitting `cond || rhs` / `cond && rhs` instead of a ternary.
//!
//! Guard: the non-trivial branch must have real (non-const) computation, to
//! avoid converting genuine ternaries like `cond ? 1 : 2` to `cond || 2`.

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{BlockId, Constant, Function, InstId, Module, Op, ValueId};
use reincarnate_core::pipeline::{Transform, TransformResult};

pub struct GmlLogicalOpNormalize;

impl Transform for GmlLogicalOpNormalize {
    fn name(&self) -> &str {
        "gml-logical-op-normalize"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func in module.functions.values_mut() {
            changed |= normalize_logical_ops(func);
        }
        Ok(TransformResult { module, changed })
    }
}

fn normalize_logical_ops(func: &mut Function) -> bool {
    let mut changed = false;
    // Collect all BrIf instructions up front to avoid borrow conflicts.
    let brifs: Vec<(InstId, ValueId, BlockId, BlockId)> = func
        .insts
        .iter()
        .filter_map(|(id, inst)| {
            if let Op::BrIf { cond, then_target, else_target, .. } = &inst.op {
                Some((id, *cond, *then_target, *else_target))
            } else {
                None
            }
        })
        .collect();

    for (_, cond, then_target, else_target) in &brifs {
        let (cond, then_target, else_target) = (*cond, *then_target, *else_target);
        // Try GML OR: then-block is trivially pure with a const-truthy result,
        // else-block has real computation.
        // Guard: skip if the trivial block is shared by multiple BrIf predecessors —
        // normalizing a shared block is unsafe because the replacement value (cond)
        // is only correct for THIS BrIf, not for the other predecessor(s).
        if let Some(br_inst_id) = trivially_pure_const_branch(func, then_target, true) {
            let then_pred_count = brifs.iter().filter(|(_, _, t, _)| *t == then_target).count();
            if then_pred_count == 1 && !is_trivially_pure_block(func, else_target) {
                func.insts[br_inst_id].op = replace_br_arg(func, br_inst_id, cond);
                changed = true;
                continue;
            }
        }
        // Try GML AND: else-block is trivially pure with a const-falsy result,
        // then-block has real computation.
        if let Some(br_inst_id) = trivially_pure_const_branch(func, else_target, false) {
            let else_pred_count = brifs.iter().filter(|(_, _, _, e)| *e == else_target).count();
            if else_pred_count == 1 && !is_trivially_pure_block(func, then_target) {
                func.insts[br_inst_id].op = replace_br_arg(func, br_inst_id, cond);
                changed = true;
            }
        }
    }
    changed
}

/// If `block` is trivially pure (only `Op::Const` instructions) and its
/// terminator `Br` passes a single arg that is const-truthy (for OR, `want_truthy=true`)
/// or const-falsy (for AND, `want_truthy=false`), return the `InstId` of the Br.
fn trivially_pure_const_branch(
    func: &Function,
    block: BlockId,
    want_truthy: bool,
) -> Option<InstId> {
    let blk = &func.blocks[block];
    // Block must take no params (empty — it's just the intermediate branch block).
    if !blk.params.is_empty() {
        return None;
    }
    // All instructions except the last must be Op::Const.
    let n = blk.insts.len();
    if n == 0 {
        return None;
    }
    for &inst_id in &blk.insts[..n - 1] {
        if !matches!(func.insts[inst_id].op, Op::Const(_)) {
            return None;
        }
    }
    // Last instruction must be Br with a single arg.
    let last_id = blk.insts[n - 1];
    let Op::Br { args, .. } = &func.insts[last_id].op else {
        return None;
    };
    if args.len() != 1 {
        return None;
    }
    let arg = args[0];
    // The arg must be a const truthy or falsy value.
    let is_match = func.insts.iter().any(|(_, inst)| {
        inst.result == Some(arg)
            && match &inst.op {
                Op::Const(c) => {
                    if want_truthy {
                        is_const_truthy(c)
                    } else {
                        is_const_falsy(c)
                    }
                }
                _ => false,
            }
    });
    if is_match { Some(last_id) } else { None }
}

/// Return true if `block` contains only `Op::Const` instructions (plus a Br
/// terminator). Used to reject genuine ternaries where BOTH branches are pure.
fn is_trivially_pure_block(func: &Function, block: BlockId) -> bool {
    let blk = &func.blocks[block];
    blk.insts.iter().all(|&id| {
        matches!(
            func.insts[id].op,
            Op::Const(_) | Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } | Op::Return(_)
        )
    })
}

/// Build a new `Op::Br` for `br_inst_id` with the single arg replaced by `new_arg`.
fn replace_br_arg(func: &Function, br_inst_id: InstId, new_arg: ValueId) -> Op {
    let Op::Br { target, .. } = &func.insts[br_inst_id].op else {
        unreachable!("replace_br_arg called on non-Br");
    };
    Op::Br { target: *target, args: vec![new_arg] }
}

fn is_const_truthy(c: &Constant) -> bool {
    match c {
        Constant::Bool(true) | Constant::Int(1) => true,
        Constant::Float(f) => *f == 1.0,
        _ => false,
    }
}

fn is_const_falsy(c: &Constant) -> bool {
    match c {
        Constant::Bool(false) | Constant::Int(0) | Constant::Null => true,
        Constant::Float(f) => *f == 0.0,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use reincarnate_core::ir::builder::FunctionBuilder;
    use reincarnate_core::ir::{CmpKind, FunctionSig, Op, Type, Visibility};
    use reincarnate_core::ir::builder::ModuleBuilder;

    /// GML compiles `a && b && c` with a SHARED else-block:
    ///   Bf(else)  // if !a
    ///   Bf(else)  // if !b
    ///   [c]
    ///   B(merge)
    /// else:
    ///   push 0
    ///   // fall-through to merge
    ///
    /// The normalization pass must NOT replace the shared else-block's
    /// `br merge(const_0)` with `br merge(cond_a)`, because when
    /// reached via the inner BrIf (cond_a = true, cond_b = false),
    /// using cond_a=true would incorrectly make the merge condition true.
    #[test]
    fn test_shared_else_block_not_normalized() {
        // IR:
        //   block0: br_if cond_a, block1, block3
        //   block1: br_if cond_b, block2, block3    ← block3 shared
        //   block2: v_cmp = cmp.lt a, b; br block4(v_cmp)
        //   block3: v_zero = const 0; br block4(v_zero)   ← shared else
        //   block4(v_merge: bool): return v_merge
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Bool,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("double_and", sig, Visibility::Public);
        let cond_a = fb.param(0);
        let cond_b = fb.param(1);
        let a = fb.param(2);
        let b = fb.param(3);

        let block1 = fb.create_block();
        let block2 = fb.create_block();
        let block3 = fb.create_block(); // shared else
        let (block4, block4_params) = fb.create_block_with_params(&[Type::Bool]);
        let v_merge = block4_params[0];

        // block0: br_if cond_a, block1, block3
        fb.br_if(cond_a, block1, &[], block3, &[]);

        // block1: br_if cond_b, block2, block3
        fb.switch_to_block(block1);
        fb.br_if(cond_b, block2, &[], block3, &[]);

        // block2: v_cmp = cmp.lt(a, b); br block4(v_cmp)
        fb.switch_to_block(block2);
        let v_cmp = fb.cmp(CmpKind::Lt, a, b);
        fb.br(block4, &[v_cmp]);

        // block3 (shared else): v_zero = const 0; br block4(v_zero)
        fb.switch_to_block(block3);
        let v_zero = fb.const_int(0);
        fb.br(block4, &[v_zero]);

        // block4: return v_merge
        fb.switch_to_block(block4);
        fb.ret(Some(v_merge));

        let func = fb.build();
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        // Run the normalization pass.
        let result = GmlLogicalOpNormalize.apply(module).unwrap();
        assert!(!result.changed, "Pass must not modify a shared else-block");

        // Find block3's Br and confirm its arg is still v_zero (const 0), not cond_a.
        let func = result.module.functions.values().next().unwrap();
        let b3 = &func.blocks[block3];
        let last_inst = func.insts[*b3.insts.last().unwrap()].op.clone();
        let Op::Br { args, .. } = last_inst else {
            panic!("Expected Br terminator in block3");
        };
        assert_eq!(args[0], v_zero, "block3's Br arg must remain v_zero (const 0), not be replaced with cond_a");
    }
}
