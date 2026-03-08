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
use reincarnate_core::ir::block::Block;
use reincarnate_core::ir::inst::Inst;
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

    for (brif_id, cond, then_target, else_target) in &brifs {
        let (brif_id, cond, then_target, else_target) =
            (*brif_id, *cond, *then_target, *else_target);
        // Try GML OR: then-block is trivially pure with a const-truthy result,
        // else-block has real computation.
        // Guard: skip if the trivial block is shared by multiple BrIf predecessors —
        // normalizing a shared block is unsafe because the replacement value (cond)
        // is only correct for THIS BrIf, not for the other predecessor(s).
        // Guard: skip if the merge target is a loop header — the trivially-truthy
        // then-block would be a loop initializer, not a short-circuit OR arm.
        // Guard: skip if else_target IS the merge target — that means the else branch
        // jumps directly to the join point (a plain `if (cond) { x = 1 }`, not `||`).
        // In a real `||`, the else branch has a body that computes the RHS and THEN
        // jumps to the merge; merge_target ≠ else_target.
        if let Some(br_inst_id) = trivially_pure_const_branch(func, then_target, true) {
            let then_pred_count = brifs.iter().filter(|(_, _, t, _)| *t == then_target).count();
            let merge_target = get_br_target(func, br_inst_id);
            if then_pred_count == 1
                && else_target != merge_target
                && !is_trivially_pure_block(func, else_target)
                && !is_loop_header(func, merge_target)
            {
                func.insts[br_inst_id].op = replace_br_arg(func, br_inst_id, cond);
                changed = true;
                continue;
            }
        }
        // Try GML AND: else-block is trivially pure with a const-falsy result,
        // then-block has real computation.
        // Guard: skip if the merge target is a loop header — the trivially-falsy
        // else-block is initializing a loop variable (e.g. `counter = 0`), not
        // short-circuiting a logical AND. Substituting `cond` would replace the
        // correctly-typed loop counter initial value with a bool condition.
        // Guard: skip if then_target IS the merge target — that means the then branch
        // jumps directly to the join point (a plain `if (!cond) { x = 0 }`, not `&&`).
        if let Some(br_inst_id) = trivially_pure_const_branch(func, else_target, false) {
            let else_pred_count = brifs.iter().filter(|(_, _, _, e)| *e == else_target).count();
            let merge_target = get_br_target(func, br_inst_id);
            if then_target != merge_target
                && !is_trivially_pure_block(func, then_target)
                && !is_loop_header(func, merge_target)
            {
                if else_pred_count == 1 {
                    // Sole predecessor: rewrite the trivial block in place.
                    func.insts[br_inst_id].op = replace_br_arg(func, br_inst_id, cond);
                    changed = true;
                } else {
                    // Shared else-block (e.g. nested `if (a) { if (b) { ... } }` where
                    // both BrIf instructions share the same skip target). We must NOT
                    // rewrite the shared block in place — that would corrupt the other
                    // predecessor's path. Instead, insert a fresh bypass block
                    // `Br merge(cond)` and redirect only this BrIf's else_target.
                    let bypass = insert_bypass_block(func, merge_target, cond);
                    update_brif_else_target(&mut func.insts[brif_id].op, bypass);
                    changed = true;
                }
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

/// Return the target of the `Br` instruction `br_inst_id`.
fn get_br_target(func: &Function, br_inst_id: InstId) -> BlockId {
    let Op::Br { target, .. } = &func.insts[br_inst_id].op else {
        unreachable!("get_br_target called on non-Br");
    };
    *target
}

/// Return all successor block IDs of `block`'s terminator instruction.
fn block_successors(func: &Function, block: BlockId) -> Vec<BlockId> {
    let blk = &func.blocks[block];
    let Some(&last_id) = blk.insts.last() else {
        return vec![];
    };
    match &func.insts[last_id].op {
        Op::Br { target, .. } => vec![*target],
        Op::BrIf { then_target, else_target, .. } => vec![*then_target, *else_target],
        Op::Switch { cases, default, .. } => {
            let mut succs: Vec<BlockId> = cases.iter().map(|(_, b, _)| *b).collect();
            succs.push(default.0);
            succs
        }
        _ => vec![],
    }
}

/// Return true if `block` is a loop header — i.e., at least one block reachable
/// from `block` has `block` as a successor (back-edge).
fn is_loop_header(func: &Function, block: BlockId) -> bool {
    let mut visited = std::collections::HashSet::new();
    let mut queue = std::collections::VecDeque::new();
    queue.push_back(block);
    while let Some(bid) = queue.pop_front() {
        if !visited.insert(bid) {
            continue;
        }
        for succ in block_successors(func, bid) {
            if succ == block {
                return true;
            }
            queue.push_back(succ);
        }
    }
    false
}

/// Insert a new bypass block `Br merge_target(bypass_arg)` into `func` and
/// return its `BlockId`. Used to redirect a BrIf's else_target so that only
/// this path receives `bypass_arg` while the shared else block is left intact.
fn insert_bypass_block(func: &mut Function, merge_target: BlockId, bypass_arg: ValueId) -> BlockId {
    let br_inst = func.insts.push(Inst { op: Op::Br { target: merge_target, args: vec![bypass_arg] }, result: None, span: None });
    func.blocks.push(Block { params: vec![], insts: vec![br_inst] })
}

/// Rewrite `op` (a `BrIf`) to use `new_else` as its else_target.
fn update_brif_else_target(op: &mut Op, new_else: BlockId) {
    match op {
        Op::BrIf { else_target, .. } => *else_target = new_else,
        _ => unreachable!("update_brif_else_target called on non-BrIf"),
    }
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
    /// The normalization pass must NOT modify the shared else-block in place —
    /// replacing `br merge(const_0)` with `br merge(cond_a)` would corrupt the
    /// inner BrIf's path (cond_b=false would incorrectly propagate cond_a=true).
    ///
    /// Instead, for the inner BrIf (cond_b), the pass inserts a fresh bypass block
    /// `Br merge(cond_b)` and redirects only that BrIf's else_target to it,
    /// leaving the shared else-block (block3) intact.
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
        // The pass should have changed something: it creates a bypass block for block1's
        // inner BrIf (cond_b) and redirects its else_target to the new bypass.
        assert!(result.changed, "Pass should create a bypass block for the inner BrIf");

        let func = result.module.functions.values().next().unwrap();

        // block3's Br arg must still be v_zero — the shared block must NOT be modified.
        let b3 = &func.blocks[block3];
        let last_inst = func.insts[*b3.insts.last().unwrap()].op.clone();
        let Op::Br { args, .. } = last_inst else {
            panic!("Expected Br terminator in block3");
        };
        assert_eq!(args[0], v_zero, "block3's Br arg must remain v_zero (const 0), not be replaced");

        // block1's BrIf else_target should no longer be block3 — it points to the bypass block.
        let b1 = &func.blocks[block1];
        let b1_brif = func.insts[*b1.insts.last().unwrap()].op.clone();
        let Op::BrIf { else_target, .. } = b1_brif else {
            panic!("Expected BrIf terminator in block1");
        };
        assert_ne!(else_target, block3, "block1's BrIf else_target should be redirected to bypass, not block3");

        // The new bypass block should Br to block4 with cond_b as the arg.
        let bypass_blk = &func.blocks[else_target];
        assert_eq!(bypass_blk.insts.len(), 1);
        let bypass_br = func.insts[bypass_blk.insts[0]].op.clone();
        let Op::Br { target: bypass_target, args: bypass_args } = bypass_br else {
            panic!("Expected Br in bypass block");
        };
        assert_eq!(bypass_target, block4, "bypass block should Br to block4");
        assert_eq!(bypass_args[0], cond_b, "bypass block should forward cond_b to merge");
    }

    /// Regression test: a BrIf whose else-block is trivially falsy (const 0 + Br)
    /// must NOT be rewritten when the Br's target is a loop header.
    ///
    /// Pattern:
    ///   block0: BrIf(v_cond) → then=block1(real), else=block2(trivially falsy)
    ///   block2: v_zero = const 0; br block3(v_zero)   ← trivially falsy, sole pred
    ///   block3(v_counter: i64): BrIf(...) → body=block4, exit=block5
    ///   block4: br block3(v_one)     ← back-edge → block3 is a loop header
    ///   block5: return
    ///
    /// Without the guard, GmlLogicalOpNormalize would replace block2's
    /// `br block3(v_zero)` with `br block3(v_cond)`, changing the type of
    /// the loop counter's initial value from i64 to bool — causing TS2322.
    #[test]
    fn test_loop_header_not_normalized_as_logical_and() {
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("loop_header_and_guard", sig, Visibility::Public);
        let v_cond = fb.param(0);
        let v_a = fb.param(1);
        let v_b = fb.param(2);

        let block1 = fb.create_block();
        let block2 = fb.create_block();
        let (block3, block3_params) = fb.create_block_with_params(&[Type::Int(64)]);
        let v_counter = block3_params[0];
        let block4 = fb.create_block();
        let block5 = fb.create_block();

        // block0: BrIf(v_cond) → block1, block2
        fb.br_if(v_cond, block1, &[], block2, &[]);

        // block1: v_cmp = cmp.lt(v_a, v_b); br block3(v_a)  ← not trivially pure
        fb.switch_to_block(block1);
        let _v_cmp = fb.cmp(CmpKind::Lt, v_a, v_b);
        fb.br(block3, &[v_a]);

        // block2: v_zero = const 0; br block3(v_zero)  ← trivially falsy, sole pred
        fb.switch_to_block(block2);
        let v_zero = fb.const_int(0);
        fb.br(block3, &[v_zero]);

        // block3(v_counter: i64): BrIf(v_cond) → block4, block5
        fb.switch_to_block(block3);
        fb.br_if(v_cond, block4, &[], block5, &[]);

        // block4: v_one = const 1; br block3(v_one)  ← back-edge → block3 is loop header
        fb.switch_to_block(block4);
        let v_one = fb.const_int(1);
        let _ = v_counter; // used structurally as the loop param, not in body here
        fb.br(block3, &[v_one]);

        // block5: return
        fb.switch_to_block(block5);
        fb.ret(None);

        let func = fb.build();
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = GmlLogicalOpNormalize.apply(module).unwrap();

        let func = result.module.functions.values().next().unwrap();

        // block2's Br arg must remain v_zero — NOT replaced with v_cond.
        let b2 = &func.blocks[block2];
        let last_inst = func.insts[*b2.insts.last().unwrap()].op.clone();
        let Op::Br { args, .. } = last_inst else {
            panic!("Expected Br terminator in block2");
        };
        assert_eq!(
            args[0], v_zero,
            "block2's Br arg must remain v_zero (loop counter init), not be replaced with v_cond"
        );
    }

    /// Regression test: a plain `if (cond) { x = 1 }` must NOT be rewritten as `||`.
    ///
    /// Pattern (BigYellowEye::step `var m = 4; if (timer > 60) { m = 1; }`):
    ///   block0: BrIf(cond) → then=block1(trivially pure, const 1), else=block2(real code)
    ///   block1: v1 = const 1; br block2(v1)   ← trivially pure, truthy
    ///   block2(v_m: i64): ...                  ← merge target == else_target
    ///
    /// Without the guard, GmlLogicalOpNormalize would replace block1's
    /// `br block2(v1)` with `br block2(cond)`, changing the type of
    /// the merged value from i64 to bool — causing TS2322.
    #[test]
    fn test_if_then_not_rewritten_as_logical_or() {
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64)],
            return_ty: Type::Dynamic,
            ..Default::default()
        };
        let mut fb = FunctionBuilder::new("if_then_guard", sig, Visibility::Public);
        let v_cond = fb.param(0);
        let v_init = fb.param(1); // simulates the initial value passed in else path

        let block1 = fb.create_block();
        let (block2, block2_params) = fb.create_block_with_params(&[Type::Int(64)]);
        let v_m = block2_params[0];

        // block0: BrIf(cond) → then=block1, else=block2(v_init)
        // Note: else_target == merge_target (block2 is the merge)
        fb.br_if(v_cond, block1, &[], block2, &[v_init]);

        // block1: v1 = const 1; br block2(v1)  ← trivially pure, truthy
        fb.switch_to_block(block1);
        let v1 = fb.const_int(1);
        fb.br(block2, &[v1]);

        // block2(v_m): use v_m in a non-trivial way, then return
        fb.switch_to_block(block2);
        let _ = v_m;
        fb.ret(None);

        let func = fb.build();
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = GmlLogicalOpNormalize.apply(module).unwrap();
        // The pass must NOT modify block1's Br arg — it's a plain if-then, not ||.
        let func = result.module.functions.values().next().unwrap();
        let b1 = &func.blocks[block1];
        let last_inst = func.insts[*b1.insts.last().unwrap()].op.clone();
        let Op::Br { args, .. } = last_inst else {
            panic!("Expected Br terminator in block1");
        };
        assert_eq!(
            args[0], v1,
            "block1's Br arg must remain v1 (const 1, int), not be replaced with cond (bool)"
        );
    }
}
