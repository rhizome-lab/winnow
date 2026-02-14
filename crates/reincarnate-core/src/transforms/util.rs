use std::collections::{HashMap, HashSet};

use crate::entity::EntityRef;
use crate::ir::{BlockId, Op, ValueId};

/// Extract branch targets from a control-flow instruction.
pub fn branch_targets(op: &Op) -> Vec<BlockId> {
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

/// Extract all ValueId operands from an Op.
pub fn value_operands(op: &Op) -> Vec<ValueId> {
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
        Op::Select {
            cond,
            on_true,
            on_false,
        } => vec![*cond, *on_true, *on_false],
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
        Op::MethodCall {
            receiver, args, ..
        } => {
            let mut ops = vec![*receiver];
            ops.extend(args);
            ops
        }
        Op::Cast(a, ..) | Op::TypeCheck(a, _) => vec![*a],
        Op::StructInit { fields, .. } => fields.iter().map(|(_, v)| *v).collect(),
        Op::ArrayInit(elems) | Op::TupleInit(elems) => elems.clone(),
        Op::CoroutineCreate { args, .. } => args.clone(),
        Op::CoroutineResume(v) => vec![*v],
        Op::GlobalRef(_) => vec![],
    }
}

/// Replace ValueIds in an Op using a substitution map.
pub fn substitute_values_in_op(op: &mut Op, subst: &HashMap<ValueId, ValueId>) {
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
        Op::Select {
            cond,
            on_true,
            on_false,
        } => {
            sub(cond);
            sub(on_true);
            sub(on_false);
        }
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
        Op::Return(v) | Op::Yield(v) => {
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
        Op::MethodCall {
            receiver, args, ..
        } => {
            sub(receiver);
            for a in args {
                sub(a);
            }
        }
        Op::Cast(a, ..) | Op::TypeCheck(a, _) => sub(a),
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

/// Compute dominance frontiers using the CHK algorithm.
///
/// For each join point (block with >=2 predecessors), walks up the idom tree
/// from each predecessor until reaching the join's immediate dominator, adding
/// the join to each visited block's DF set.
pub fn compute_dominance_frontiers(
    preds: &HashMap<BlockId, Vec<BlockId>>,
    idom: &HashMap<BlockId, BlockId>,
) -> HashMap<BlockId, HashSet<BlockId>> {
    let mut df: HashMap<BlockId, HashSet<BlockId>> = HashMap::new();
    for (&block, block_preds) in preds {
        if block_preds.len() < 2 {
            continue;
        }
        let idom_block = match idom.get(&block) {
            Some(&d) => d,
            None => continue,
        };
        for &pred in block_preds {
            let mut runner = pred;
            while runner != idom_block {
                df.entry(runner).or_default().insert(block);
                match idom.get(&runner) {
                    Some(&d) if d != runner => runner = d,
                    _ => break,
                }
            }
        }
    }
    df
}

/// Append a branch argument for edges targeting a specific block.
///
/// Handles Br, BrIf (both arms), and Switch (cases + default).
pub fn append_branch_arg_for_target(op: &mut Op, target: BlockId, value: ValueId) {
    match op {
        Op::Br {
            target: t, args, ..
        } if *t == target => {
            args.push(value);
        }
        Op::BrIf {
            then_target,
            then_args,
            else_target,
            else_args,
            ..
        } => {
            if *then_target == target {
                then_args.push(value);
            }
            if *else_target == target {
                else_args.push(value);
            }
        }
        Op::Switch {
            cases, default, ..
        } => {
            for (_, t, args) in cases.iter_mut() {
                if *t == target {
                    args.push(value);
                }
            }
            if default.0 == target {
                default.1.push(value);
            }
        }
        _ => {}
    }
}

/// Build a preorder traversal of the dominator tree.
pub fn dom_tree_preorder(
    entry: BlockId,
    idom: &HashMap<BlockId, BlockId>,
) -> Vec<BlockId> {
    // Build children map from idom.
    let mut children: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for (&block, &parent) in idom {
        if block != parent {
            children.entry(parent).or_default().push(block);
        }
    }
    // Sort children for determinism.
    for c in children.values_mut() {
        c.sort_by_key(|b| b.index());
    }
    let mut order = Vec::new();
    let mut stack = vec![entry];
    while let Some(block) = stack.pop() {
        order.push(block);
        if let Some(kids) = children.get(&block) {
            // Push in reverse so leftmost child is processed first.
            for &kid in kids.iter().rev() {
                stack.push(kid);
            }
        }
    }
    order
}

#[cfg(test)]
#[allow(dead_code)]
pub(crate) mod test_helpers {
    use super::*;
    use crate::entity::EntityRef;
    use crate::ir::builder::ModuleBuilder;
    use crate::ir::{BlockId, FuncId, Function, Op, ValueId};
    use crate::pipeline::Transform;
    use std::collections::HashSet;

    /// Validate IR well-formedness — panics with diagnostic on any violation.
    pub fn assert_well_formed(func: &Function) {
        let all_values: HashSet<ValueId> = (0..func.value_types.len())
            .map(|i| ValueId::new(i as u32))
            .collect();

        // Build the set of blocks that contain instructions.
        let reachable_blocks: HashSet<BlockId> = {
            let mut reachable = HashSet::new();
            let mut worklist = vec![func.entry];
            reachable.insert(func.entry);
            while let Some(block_id) = worklist.pop() {
                for &inst_id in &func.blocks[block_id].insts {
                    for target in branch_targets(&func.insts[inst_id].op) {
                        if reachable.insert(target) {
                            worklist.push(target);
                        }
                    }
                }
            }
            reachable
        };

        for &block_id in &reachable_blocks {
            let block = &func.blocks[block_id];

            // Check block params have valid value IDs.
            for param in &block.params {
                assert!(
                    all_values.contains(&param.value),
                    "block {:?} param {:?} not in value_types",
                    block_id,
                    param.value
                );
            }

            // Check instructions.
            assert!(
                !block.insts.is_empty(),
                "reachable block {:?} has no instructions",
                block_id
            );

            for &inst_id in &block.insts {
                let inst = &func.insts[inst_id];

                // Check result value exists.
                if let Some(result) = inst.result {
                    assert!(
                        all_values.contains(&result),
                        "inst {:?} result {:?} not in value_types",
                        inst_id,
                        result
                    );
                }

                // Check operand values exist.
                for operand in value_operands(&inst.op) {
                    assert!(
                        all_values.contains(&operand),
                        "inst {:?} operand {:?} not in value_types (op: {:?})",
                        inst_id,
                        operand,
                        inst.op
                    );
                }

                // Check branch targets are valid blocks.
                for target in branch_targets(&inst.op) {
                    assert!(
                        func.blocks.keys().any(|b| b == target),
                        "inst {:?} branches to non-existent block {:?}",
                        inst_id,
                        target
                    );
                }
            }

            // Check last instruction is a terminator.
            let last_inst = &func.insts[*block.insts.last().unwrap()];
            assert!(
                matches!(
                    &last_inst.op,
                    Op::Br { .. }
                        | Op::BrIf { .. }
                        | Op::Switch { .. }
                        | Op::Return(_)
                        | Op::Yield(_)
                ),
                "reachable block {:?} does not end with a terminator, last op: {:?}",
                block_id,
                last_inst.op
            );

            // Check branch arg count matches target block param count.
            let last_op = &last_inst.op;
            check_branch_arg_counts(func, block_id, last_op);
        }
    }

    fn check_branch_arg_counts(func: &Function, _src: BlockId, op: &Op) {
        match op {
            Op::Br { target, args } => {
                let param_count = func.blocks[*target].params.len();
                assert_eq!(
                    args.len(),
                    param_count,
                    "Br to {:?} has {} args but target has {} params",
                    target,
                    args.len(),
                    param_count
                );
            }
            Op::BrIf {
                then_target,
                then_args,
                else_target,
                else_args,
                ..
            } => {
                let then_params = func.blocks[*then_target].params.len();
                assert_eq!(
                    then_args.len(),
                    then_params,
                    "BrIf then-arm to {:?} has {} args but target has {} params",
                    then_target,
                    then_args.len(),
                    then_params
                );
                let else_params = func.blocks[*else_target].params.len();
                assert_eq!(
                    else_args.len(),
                    else_params,
                    "BrIf else-arm to {:?} has {} args but target has {} params",
                    else_target,
                    else_args.len(),
                    else_params
                );
            }
            Op::Switch {
                cases, default, ..
            } => {
                for (val, target, args) in cases {
                    let param_count = func.blocks[*target].params.len();
                    assert_eq!(
                        args.len(),
                        param_count,
                        "Switch case {:?} to {:?} has {} args but target has {} params",
                        val,
                        target,
                        args.len(),
                        param_count
                    );
                }
                let default_params = func.blocks[default.0].params.len();
                assert_eq!(
                    default.1.len(),
                    default_params,
                    "Switch default to {:?} has {} args but target has {} params",
                    default.0,
                    default.1.len(),
                    default_params
                );
            }
            _ => {}
        }
    }

    /// Apply a transform, validate well-formedness, then apply again and assert
    /// the second run reports no changes (idempotent).
    pub fn assert_idempotent<T: Transform>(pass: &T, func: Function) {
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let r1 = pass.apply(module).unwrap();
        assert_well_formed(&r1.module.functions[FuncId::new(0)]);
        let r2 = pass.apply(r1.module).unwrap();
        assert!(
            !r2.changed,
            "{} not idempotent: second apply still reports changes",
            pass.name()
        );
        assert_well_formed(&r2.module.functions[FuncId::new(0)]);
    }

    /// Apply a transform to a function, returning the (changed, Function) pair.
    pub fn apply_transform<T: Transform>(pass: &T, func: Function) -> (bool, Function) {
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let result = pass.apply(module).unwrap();
        (
            result.changed,
            result.module.functions[FuncId::new(0)].clone(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityRef;

    fn bid(n: u32) -> BlockId {
        BlockId::new(n)
    }

    #[test]
    fn dominance_frontier_diamond() {
        // Diamond CFG:
        //     0
        //    / \
        //   1   2
        //    \ /
        //     3
        let preds: HashMap<BlockId, Vec<BlockId>> = [
            (bid(0), vec![]),
            (bid(1), vec![bid(0)]),
            (bid(2), vec![bid(0)]),
            (bid(3), vec![bid(1), bid(2)]),
        ]
        .into_iter()
        .collect();

        let idom: HashMap<BlockId, BlockId> = [
            (bid(0), bid(0)),
            (bid(1), bid(0)),
            (bid(2), bid(0)),
            (bid(3), bid(0)),
        ]
        .into_iter()
        .collect();

        let df = compute_dominance_frontiers(&preds, &idom);

        // DF(1) = {3}, DF(2) = {3}, DF(0) = {}, DF(3) = {}
        assert_eq!(
            df.get(&bid(1)),
            Some(&[bid(3)].into_iter().collect()),
        );
        assert_eq!(
            df.get(&bid(2)),
            Some(&[bid(3)].into_iter().collect()),
        );
        assert!(df.get(&bid(0)).is_none_or(|s| s.is_empty()));
        assert!(df.get(&bid(3)).is_none_or(|s| s.is_empty()));
    }

    #[test]
    fn dominance_frontier_loop() {
        // Loop CFG:
        //   0 → 1 → 2
        //       ↑   ↓
        //       └───┘
        //       1 → 3 (exit)
        let preds: HashMap<BlockId, Vec<BlockId>> = [
            (bid(0), vec![]),
            (bid(1), vec![bid(0), bid(2)]),
            (bid(2), vec![bid(1)]),
            (bid(3), vec![bid(1)]),
        ]
        .into_iter()
        .collect();

        // idom: 0→0, 1→0, 2→1, 3→1
        let idom: HashMap<BlockId, BlockId> = [
            (bid(0), bid(0)),
            (bid(1), bid(0)),
            (bid(2), bid(1)),
            (bid(3), bid(1)),
        ]
        .into_iter()
        .collect();

        let df = compute_dominance_frontiers(&preds, &idom);

        // DF(2) = {1} (back-edge), DF(1) = {1} (self-loop via 2→1)
        assert!(df.get(&bid(2)).is_some_and(|s| s.contains(&bid(1))));
        // Block 1 is a join with preds 0,2 and idom(1)=0.
        // Walk from pred 2: runner=2, idom(2)=1≠0 → DF(2)∋1, runner=1, idom(1)=0=idom_block → stop
        // Walk from pred 0: runner=0=idom_block → stop
        // So DF(2) = {1}
        assert_eq!(
            df.get(&bid(2)),
            Some(&[bid(1)].into_iter().collect()),
        );
    }

    #[test]
    fn append_branch_arg_br() {
        let mut op = Op::Br {
            target: bid(1),
            args: vec![],
        };
        let v = ValueId::new(42);
        append_branch_arg_for_target(&mut op, bid(1), v);
        if let Op::Br { args, .. } = &op {
            assert_eq!(args, &[v]);
        }
    }

    #[test]
    fn append_branch_arg_brif_both_arms() {
        let mut op = Op::BrIf {
            cond: ValueId::new(0),
            then_target: bid(1),
            then_args: vec![],
            else_target: bid(1),
            else_args: vec![],
        };
        let v = ValueId::new(42);
        append_branch_arg_for_target(&mut op, bid(1), v);
        if let Op::BrIf {
            then_args,
            else_args,
            ..
        } = &op
        {
            assert_eq!(then_args, &[v]);
            assert_eq!(else_args, &[v]);
        }
    }

    #[test]
    fn dom_tree_preorder_diamond() {
        let idom: HashMap<BlockId, BlockId> = [
            (bid(0), bid(0)),
            (bid(1), bid(0)),
            (bid(2), bid(0)),
            (bid(3), bid(0)),
        ]
        .into_iter()
        .collect();

        let order = dom_tree_preorder(bid(0), &idom);
        assert_eq!(order[0], bid(0));
        assert_eq!(order.len(), 4);
        // Children of 0 are 1, 2, 3 — all should appear after 0.
        assert!(order.contains(&bid(1)));
        assert!(order.contains(&bid(2)));
        assert!(order.contains(&bid(3)));
    }
}
