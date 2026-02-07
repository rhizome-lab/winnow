//! Structured control flow reconstruction.
//!
//! Analyzes a function's block-based CFG and produces a `Shape` tree that
//! represents `if`/`else`, `while`, `for`, and general loops. Falls back to
//! a dispatch-loop (`Shape::Dispatch`) for irreducible subgraphs.
//!
//! This is a read-only analysis — the IR is not modified.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::entity::EntityRef;
use crate::ir::{BlockId, Function, Op, ValueId};
use crate::transforms::util::branch_targets;

// -------------------------------------------------------------------------
// Shape types
// -------------------------------------------------------------------------

/// Assignment of a block argument: `dst = src` at a branch site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockArgAssign {
    pub dst: ValueId,
    pub src: ValueId,
}

/// A structured control-flow shape recovered from the block-based CFG.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Shape {
    /// Emit one block's non-terminator instructions.
    Block(BlockId),
    /// Execute shapes in order.
    Seq(Vec<Shape>),
    /// `if (cond) { then } else { else_ }`
    IfElse {
        block: BlockId,
        cond: ValueId,
        cond_negated: bool,
        then_assigns: Vec<BlockArgAssign>,
        then_body: Box<Shape>,
        then_trailing_assigns: Vec<BlockArgAssign>,
        else_assigns: Vec<BlockArgAssign>,
        else_body: Box<Shape>,
        else_trailing_assigns: Vec<BlockArgAssign>,
    },
    /// `while (cond) { body }`
    WhileLoop {
        header: BlockId,
        cond: ValueId,
        cond_negated: bool,
        body: Box<Shape>,
    },
    /// `for (init; cond; update) { body }`
    ForLoop {
        header: BlockId,
        init_assigns: Vec<BlockArgAssign>,
        cond: ValueId,
        cond_negated: bool,
        update_assigns: Vec<BlockArgAssign>,
        body: Box<Shape>,
    },
    /// `while (true) { body }` — general loop with Break/Continue.
    Loop {
        header: BlockId,
        body: Box<Shape>,
    },
    /// `break;`
    Break,
    /// `continue;`
    Continue,
    /// `break` to an outer loop, `depth` levels up (0 = innermost).
    LabeledBreak { depth: usize },
    /// Short-circuit OR: `phi = cond || rhs`
    /// `rhs_body` executes only when `cond` is falsy.
    LogicalOr {
        block: BlockId,
        cond: ValueId,
        phi: ValueId,
        rhs_body: Box<Shape>,
        rhs: ValueId,
    },
    /// Short-circuit AND: `phi = cond && rhs`
    /// `rhs_body` executes only when `cond` is truthy.
    LogicalAnd {
        block: BlockId,
        cond: ValueId,
        phi: ValueId,
        rhs_body: Box<Shape>,
        rhs: ValueId,
    },
    /// Fallback dispatch for irreducible CFG subgraphs.
    Dispatch {
        blocks: Vec<BlockId>,
        entry: BlockId,
    },
}

// -------------------------------------------------------------------------
// CFG helpers
// -------------------------------------------------------------------------

/// Predecessor and successor maps for a function's CFG.
struct Cfg {
    succs: HashMap<BlockId, Vec<BlockId>>,
    preds: HashMap<BlockId, Vec<BlockId>>,
}

fn build_cfg(func: &Function) -> Cfg {
    let mut succs: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    let mut preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();

    for (block_id, _block) in func.blocks.iter() {
        succs.entry(block_id).or_default();
        preds.entry(block_id).or_default();
    }

    for (block_id, block) in func.blocks.iter() {
        if let Some(&last_inst) = block.insts.last() {
            for target in branch_targets(&func.insts[last_inst].op) {
                succs.entry(block_id).or_default().push(target);
                preds.entry(target).or_default().push(block_id);
            }
        }
    }

    Cfg { succs, preds }
}

// -------------------------------------------------------------------------
// Dominator computation (Lengauer-Tarjan)
// -------------------------------------------------------------------------

/// Iterative path compression for the Lengauer-Tarjan union-find forest.
///
/// Updates `label` entries so each node records the vertex with minimum
/// `semi` value on its path to the forest root, and compresses ancestor
/// pointers for future lookups. `usize::MAX` in `ancestor` means "root".
fn lt_compress(v: usize, ancestor: &mut [usize], label: &mut [usize], semi: &[usize]) {
    let mut path = Vec::new();
    let mut u = v;
    while ancestor[u] != usize::MAX && ancestor[ancestor[u]] != usize::MAX {
        path.push(u);
        u = ancestor[u];
    }
    for &node in path.iter().rev() {
        let a = ancestor[node];
        if semi[label[a]] < semi[label[node]] {
            label[node] = label[a];
        }
        ancestor[node] = ancestor[a];
    }
}

/// EVAL: returns the vertex with minimum semidominator on the path from
/// `v` to the root of its tree in the forest.
fn lt_eval(v: usize, ancestor: &mut [usize], label: &mut [usize], semi: &[usize]) -> usize {
    if ancestor[v] == usize::MAX {
        return v;
    }
    lt_compress(v, ancestor, label, semi);
    label[v]
}

/// Lengauer-Tarjan dominator tree computation.
///
/// Nearly linear time with path compression. Works for any CFG given as
/// predecessor/successor maps. Used for both dominator and post-dominator
/// computation.
fn compute_dominators_lt(
    entry: BlockId,
    preds: &HashMap<BlockId, Vec<BlockId>>,
    succs: &HashMap<BlockId, Vec<BlockId>>,
) -> HashMap<BlockId, BlockId> {
    // Phase 1: Iterative DFS numbering (avoids stack overflow on large functions).
    let mut dfnum: HashMap<BlockId, usize> = HashMap::new();
    let mut vertex: Vec<BlockId> = Vec::new();
    let mut dfs_parent: Vec<usize> = Vec::new();

    let mut stack: Vec<(BlockId, usize)> = vec![(entry, usize::MAX)];
    while let Some((block, parent_df)) = stack.pop() {
        if dfnum.contains_key(&block) {
            continue;
        }
        let df = vertex.len();
        dfnum.insert(block, df);
        vertex.push(block);
        dfs_parent.push(parent_df);

        if let Some(s) = succs.get(&block) {
            for &succ in s.iter().rev() {
                if !dfnum.contains_key(&succ) {
                    stack.push((succ, df));
                }
            }
        }
    }

    let n = vertex.len();
    if n <= 1 {
        let mut idom = HashMap::new();
        idom.insert(entry, entry);
        return idom;
    }

    // Phase 2: Compute semidominators and immediate dominators.
    let mut semi: Vec<usize> = (0..n).collect();
    let mut idom_idx: Vec<usize> = vec![0; n];
    let mut ancestor: Vec<usize> = vec![usize::MAX; n];
    let mut label: Vec<usize> = (0..n).collect();
    let mut bucket: Vec<Vec<usize>> = vec![Vec::new(); n];

    for i in (1..n).rev() {
        let w = vertex[i];
        let p = dfs_parent[i];

        if let Some(w_preds) = preds.get(&w) {
            for &v in w_preds {
                if let Some(&v_df) = dfnum.get(&v) {
                    let u = lt_eval(v_df, &mut ancestor, &mut label, &semi);
                    if semi[u] < semi[i] {
                        semi[i] = semi[u];
                    }
                }
            }
        }

        bucket[semi[i]].push(i);
        ancestor[i] = p;

        for v in std::mem::take(&mut bucket[p]) {
            let u = lt_eval(v, &mut ancestor, &mut label, &semi);
            idom_idx[v] = if semi[u] < semi[v] { u } else { p };
        }
    }

    // Phase 3: Adjust immediate dominators.
    for i in 1..n {
        if idom_idx[i] != semi[i] {
            idom_idx[i] = idom_idx[idom_idx[i]];
        }
    }

    let mut result = HashMap::with_capacity(n);
    result.insert(entry, entry);
    for i in 1..n {
        result.insert(vertex[i], vertex[idom_idx[i]]);
    }
    result
}

/// Compute dominators using Lengauer-Tarjan on the forward CFG.
fn compute_dominators(func: &Function, cfg: &Cfg) -> HashMap<BlockId, BlockId> {
    compute_dominators_lt(func.entry, &cfg.preds, &cfg.succs)
}

/// Check if `a` dominates `b`.
fn dominates(a: BlockId, b: BlockId, idom: &HashMap<BlockId, BlockId>) -> bool {
    let mut cur = b;
    loop {
        if cur == a {
            return true;
        }
        match idom.get(&cur) {
            Some(&parent) if parent != cur => cur = parent,
            _ => return false,
        }
    }
}

// -------------------------------------------------------------------------
// Post-dominator computation (virtual exit + Lengauer-Tarjan)
// -------------------------------------------------------------------------

/// Compute post-dominators using a virtual exit node and Lengauer-Tarjan.
///
/// Adds a virtual exit that all return blocks flow to, ensuring correct
/// post-dominator computation even with multiple exits.
fn compute_post_dominators(func: &Function, cfg: &Cfg) -> HashMap<BlockId, BlockId> {
    let exits: Vec<BlockId> = func
        .blocks
        .iter()
        .filter_map(|(id, block)| {
            if let Some(&last) = block.insts.last() {
                if matches!(func.insts[last].op, Op::Return(_)) {
                    return Some(id);
                }
            }
            None
        })
        .collect();

    if exits.is_empty() {
        return HashMap::new();
    }

    // Build reverse CFG: for forward edge A→B, reverse has B→A.
    let mut rev_succs: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    let mut rev_preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for (block_id, _) in func.blocks.iter() {
        rev_succs.entry(block_id).or_default();
        rev_preds.entry(block_id).or_default();
    }
    for (block_id, targets) in &cfg.succs {
        for target in targets {
            rev_succs.entry(*target).or_default().push(*block_id);
            rev_preds.entry(*block_id).or_default().push(*target);
        }
    }

    // Virtual exit node — all return blocks flow to it in the forward CFG,
    // so in the reverse CFG it has edges TO all exit blocks.
    let virtual_exit = BlockId::new(u32::MAX);
    rev_succs.insert(virtual_exit, exits.clone());
    rev_preds.entry(virtual_exit).or_default();
    for &exit in &exits {
        rev_preds.entry(exit).or_default().push(virtual_exit);
    }

    let ipdom = compute_dominators_lt(virtual_exit, &rev_preds, &rev_succs);

    // Keep only real blocks; drop entries pointing to the virtual exit
    // (those blocks post-dominate all paths to exits — no real merge point).
    ipdom
        .into_iter()
        .filter(|(k, v)| *k != virtual_exit && *v != virtual_exit)
        .collect()
}

// -------------------------------------------------------------------------
// Loop detection
// -------------------------------------------------------------------------

/// A natural loop: header block + set of body blocks.
struct NaturalLoop {
    header: BlockId,
    body: HashSet<BlockId>,
}

/// Detect natural loops via back edges (u→v where v dominates u).
fn detect_loops(cfg: &Cfg, idom: &HashMap<BlockId, BlockId>) -> Vec<NaturalLoop> {
    let mut loops: HashMap<BlockId, HashSet<BlockId>> = HashMap::new();

    for (block, targets) in &cfg.succs {
        for &target in targets {
            if dominates(target, *block, idom) {
                // Back edge: block → target (target is loop header).
                let body = loops.entry(target).or_default();
                // BFS backward from block to header to find loop body.
                let mut queue = VecDeque::new();
                if *block != target {
                    body.insert(*block);
                    queue.push_back(*block);
                }
                body.insert(target);
                while let Some(cur) = queue.pop_front() {
                    if let Some(preds) = cfg.preds.get(&cur) {
                        for &pred in preds {
                            if body.insert(pred) && pred != target {
                                queue.push_back(pred);
                            }
                        }
                    }
                }
            }
        }
    }

    loops
        .into_iter()
        .map(|(header, body)| NaturalLoop { header, body })
        .collect()
}

// -------------------------------------------------------------------------
// Structurizer
// -------------------------------------------------------------------------

/// Maximum recursion depth before falling back to a dispatch loop.
/// Real-world Flash functions can have 1000+ blocks forming deeply nested
/// if/else chains; without a depth limit, the structurizer will overflow
/// the stack.
const MAX_DEPTH: usize = 200;

/// Context for the recursive structurizer.
struct Structurizer<'a> {
    func: &'a Function,
    cfg: Cfg,
    idom: HashMap<BlockId, BlockId>,
    ipdom: HashMap<BlockId, BlockId>,
    loops: Vec<NaturalLoop>,
    /// Stack of loop headers we're currently inside (innermost last).
    loop_stack: Vec<BlockId>,
    /// Current recursion depth.
    depth: usize,
    /// Blocks already emitted (not inside a loop). Prevents exponential
    /// blowup when BrIf branches share downstream blocks and the BFS
    /// merge heuristic picks a non-optimal merge point.
    emitted: HashSet<BlockId>,
}

impl<'a> Structurizer<'a> {
    fn new(func: &'a Function) -> Self {
        let cfg = build_cfg(func);
        let idom = compute_dominators(func, &cfg);
        let ipdom = compute_post_dominators(func, &cfg);
        let loops = detect_loops(&cfg, &idom);

        Structurizer {
            func,
            cfg,
            idom,
            ipdom,
            loops,
            loop_stack: Vec::new(),
            depth: 0,
            emitted: HashSet::new(),
        }
    }

    /// Is this block a loop header?
    fn is_loop_header(&self, block: BlockId) -> bool {
        self.loops.iter().any(|l| l.header == block)
    }

    /// Get the terminator Op of a block.
    ///
    /// Finds the *first* control-flow terminator (Br, BrIf, Switch, Return)
    /// rather than assuming it's the last instruction. This is defensive
    /// against frontends that may emit dead instructions after a terminator
    /// (e.g. a redundant `Br` following a `BrIf`).
    fn terminator(&self, block: BlockId) -> Option<&Op> {
        let blk = &self.func.blocks[block];
        for &inst_id in &blk.insts {
            let op = &self.func.insts[inst_id].op;
            if matches!(
                op,
                Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } | Op::Return(_)
            ) {
                return Some(op);
            }
        }
        // Fallback: use the last instruction if it exists.
        blk.insts.last().map(|&id| &self.func.insts[id].op)
    }

    /// If `cond` is produced by `Not(inner)` in `block`, return `inner`.
    fn strip_not(&self, block: BlockId, cond: ValueId) -> Option<ValueId> {
        for &inst_id in &self.func.blocks[block].insts {
            let inst = &self.func.insts[inst_id];
            if inst.result == Some(cond) {
                if let Op::Not(inner) = &inst.op {
                    return Some(*inner);
                }
                break;
            }
        }
        None
    }

    /// Build branch arg assignments for a branch to `target` with `args`.
    fn branch_assigns(&self, target: BlockId, args: &[ValueId]) -> Vec<BlockArgAssign> {
        let target_block = &self.func.blocks[target];
        target_block
            .params
            .iter()
            .zip(args.iter())
            .map(|(param, &src)| BlockArgAssign {
                dst: param.value,
                src,
            })
            .collect()
    }

    /// Walk a shape tree to find trailing assigns from a `Br` to `merge`.
    ///
    /// When the structurizer stops at a merge boundary, the `Br { target: merge, args }`
    /// terminator's args are dropped. This recovers them by finding the last block
    /// in the shape and checking if it branches to the merge with args.
    fn trailing_merge_assigns(&self, shape: &Shape, merge: BlockId) -> Vec<BlockArgAssign> {
        match shape {
            Shape::Block(b) => {
                if let Some(Op::Br { target, args }) = self.terminator(*b).cloned() {
                    if target == merge {
                        return self.branch_assigns(merge, &args);
                    }
                }
                vec![]
            }
            Shape::Seq(parts) => parts
                .last()
                .map(|last| self.trailing_merge_assigns(last, merge))
                .unwrap_or_default(),
            _ => vec![],
        }
    }

    /// Main entry: structurize starting from `block`, continuing until we
    /// reach `until` (exclusive) or run out of blocks.
    fn structurize_region(
        &mut self,
        block: BlockId,
        until: Option<BlockId>,
        loop_body: Option<&HashSet<BlockId>>,
    ) -> Shape {
        if Some(block) == until {
            return Shape::Seq(vec![]);
        }

        // Prevent exponential blowup: if this block was already emitted,
        // skip it. Each block should appear at most once in structured output.
        // Loop headers are exempted (they're revisited via Continue).
        if !self.emitted.insert(block) {
            return Shape::Seq(vec![]);
        }

        // Guard against deep recursion on functions with hundreds of blocks
        // (common in ActionScript-generated code with long if/else chains).
        // Fall back to a dispatch loop for the remaining blocks.
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            self.depth -= 1;
            return self.fallback_dispatch(block, until, loop_body);
        }

        let result = self.structurize_region_inner(block, until, loop_body);
        self.depth -= 1;
        result
    }

    /// Inner implementation of `structurize_region` (depth tracking is in the wrapper).
    fn structurize_region_inner(
        &mut self,
        block: BlockId,
        until: Option<BlockId>,
        loop_body: Option<&HashSet<BlockId>>,
    ) -> Shape {
        // Check if this block is a loop header we're supposed to process.
        if self.is_loop_header(block) && !self.loop_stack.contains(&block) {
            return self.structurize_loop(block, until);
        }

        let Some(term) = self.terminator(block).cloned() else {
            // Empty block — treat as a no-op block.
            return Shape::Block(block);
        };

        match &term {
            Op::Return(_) => Shape::Block(block),

            Op::Br { target, args } => {
                let assigns = self.branch_assigns(*target, args);

                // Branch to a loop header in our stack → Continue.
                if let Some(lb) = loop_body {
                    if !lb.contains(target) {
                        // Exiting the loop.
                        if !assigns.is_empty() {
                            // Need to emit assigns before break.
                            let mut parts = vec![Shape::Block(block)];
                            parts.push(Shape::Break);
                            return Shape::Seq(parts);
                        }
                        // Block + Break (block needed for non-terminator insts).
                        return Shape::Seq(vec![Shape::Block(block), Shape::Break]);
                    }
                }

                if self.loop_stack.contains(target) {
                    // Continue to a loop header.
                    if !assigns.is_empty() {
                        return Shape::Seq(vec![Shape::Block(block), Shape::Continue]);
                    }
                    return Shape::Seq(vec![Shape::Block(block), Shape::Continue]);
                }

                if Some(*target) == until {
                    // Reached the merge point.
                    return Shape::Block(block);
                }

                // Linear chain.
                let rest = self.structurize_region(*target, until, loop_body);
                let mut parts = vec![Shape::Block(block)];
                match rest {
                    Shape::Seq(inner) => parts.extend(inner),
                    other => parts.push(other),
                }
                Shape::Seq(parts)
            }

            Op::BrIf {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } => {
                let mut cond = *cond;
                let mut then_target = *then_target;
                let mut else_target = *else_target;
                let mut then_args = then_args.clone();
                let mut else_args = else_args.clone();

                // Normalize: strip Not from condition, swap branches.
                if let Some(inner) = self.strip_not(block, cond) {
                    cond = inner;
                    std::mem::swap(&mut then_target, &mut else_target);
                    std::mem::swap(&mut then_args, &mut else_args);
                }

                let then_assigns = self.branch_assigns(then_target, &then_args);
                let else_assigns = self.branch_assigns(else_target, &else_args);

                // Check if inside a loop and branches target header/exit.
                if let Some(lb) = loop_body {
                    let then_in_loop = lb.contains(&then_target);
                    let else_in_loop = lb.contains(&else_target);
                    let current_header = self.loop_stack.last().copied();
                    let then_is_header = Some(then_target) == current_header;
                    let else_is_header = Some(else_target) == current_header;

                    if !then_in_loop && !else_in_loop {
                        // Both exit — break.
                        return Shape::Seq(vec![Shape::Block(block), Shape::Break]);
                    }

                    // then→exit, else→continue (back to header)
                    if !then_in_loop && else_is_header {
                        return Shape::IfElse {
                            block,
                            cond,
                            cond_negated: false,
                            then_assigns,
                            then_body: Box::new(Shape::Break),
                            then_trailing_assigns: vec![],
                            else_assigns,
                            else_body: Box::new(Shape::Continue),
                            else_trailing_assigns: vec![],
                        };
                    }

                    // then→continue (back to header), else→exit
                    if then_is_header && !else_in_loop {
                        return Shape::IfElse {
                            block,
                            cond,
                            cond_negated: false,
                            then_assigns,
                            then_body: Box::new(Shape::Continue),
                            then_trailing_assigns: vec![],
                            else_assigns,
                            else_body: Box::new(Shape::Break),
                            else_trailing_assigns: vec![],
                        };
                    }

                    // then→exit, else→body (continues in loop)
                    if !then_in_loop && else_in_loop {
                        let else_body_shape =
                            self.structurize_region(else_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            cond_negated: false,
                            then_assigns,
                            then_body: Box::new(Shape::Break),
                            then_trailing_assigns: vec![],
                            else_assigns,
                            else_body: Box::new(else_body_shape),
                            else_trailing_assigns: vec![],
                        };
                    }

                    // then→body (continues in loop), else→exit
                    if then_in_loop && !else_in_loop {
                        let then_body_shape =
                            self.structurize_region(then_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            cond_negated: false,
                            then_assigns,
                            then_body: Box::new(then_body_shape),
                            then_trailing_assigns: vec![],
                            else_assigns,
                            else_body: Box::new(Shape::Break),
                            else_trailing_assigns: vec![],
                        };
                    }

                    // Both in loop — one might be the header (continue).
                    if then_is_header {
                        let else_body_shape =
                            self.structurize_region(else_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            cond_negated: false,
                            then_assigns,
                            then_body: Box::new(Shape::Continue),
                            then_trailing_assigns: vec![],
                            else_assigns,
                            else_body: Box::new(else_body_shape),
                            else_trailing_assigns: vec![],
                        };
                    }
                    if else_is_header {
                        let then_body_shape =
                            self.structurize_region(then_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            cond_negated: false,
                            then_assigns,
                            then_body: Box::new(then_body_shape),
                            then_trailing_assigns: vec![],
                            else_assigns,
                            else_body: Box::new(Shape::Continue),
                            else_trailing_assigns: vec![],
                        };
                    }
                }

                // Find merge point via post-dominator.
                let mut merge = self.find_merge(block, then_target, else_target, until);

                // If post-dominator didn't give us a merge, try BFS intersection
                // of blocks reachable from both targets. Without a merge point,
                // both branches would independently traverse shared downstream
                // blocks, causing exponential blowup. BFS finds the first shared
                // block to use as the merge boundary.
                if merge.is_none() && then_target != else_target {
                    merge = self.find_merge_bfs(then_target, else_target, until, loop_body);
                }

                let then_body = if then_target == merge.unwrap_or(then_target) && merge.is_some() {
                    Shape::Seq(vec![])
                } else {
                    self.structurize_region(then_target, merge.or(until), loop_body)
                };

                let else_body = if else_target == merge.unwrap_or(else_target) && merge.is_some() {
                    Shape::Seq(vec![])
                } else {
                    self.structurize_region(else_target, merge.or(until), loop_body)
                };

                // Recover trailing assigns from Br-to-merge that the
                // structurizer drops when it stops at the merge boundary.
                let (then_trailing_assigns, else_trailing_assigns) =
                    if let Some(merge_block) = merge {
                        (
                            self.trailing_merge_assigns(&then_body, merge_block),
                            self.trailing_merge_assigns(&else_body, merge_block),
                        )
                    } else {
                        (vec![], vec![])
                    };

                // Normalize: if then is structurally empty and else is not,
                // swap branches and negate the condition. This avoids the
                // emitter having to detect and flip empty-then at render time.
                let then_empty = matches!(&then_body, Shape::Seq(v) if v.is_empty())
                    && then_assigns.is_empty()
                    && then_trailing_assigns.is_empty();
                let else_empty = matches!(&else_body, Shape::Seq(v) if v.is_empty())
                    && else_assigns.is_empty()
                    && else_trailing_assigns.is_empty();
                let (cond_negated, then_assigns, then_body, then_trailing_assigns,
                     else_assigns, else_body, else_trailing_assigns) =
                    if then_empty && !else_empty {
                        (true, else_assigns, else_body, else_trailing_assigns,
                         then_assigns, then_body, then_trailing_assigns)
                    } else {
                        (false, then_assigns, then_body, then_trailing_assigns,
                         else_assigns, else_body, else_trailing_assigns)
                    };

                let if_shape = Shape::IfElse {
                    block,
                    cond,
                    cond_negated,
                    then_assigns,
                    then_body: Box::new(then_body),
                    then_trailing_assigns,
                    else_assigns,
                    else_body: Box::new(else_body),
                    else_trailing_assigns,
                };
                let if_shape = self.try_logical_op(if_shape);

                if let Some(merge_block) = merge {
                    if Some(merge_block) != until {
                        let rest = self.structurize_region(merge_block, until, loop_body);
                        let mut parts = vec![if_shape];
                        match rest {
                            Shape::Seq(inner) => parts.extend(inner),
                            other => parts.push(other),
                        }
                        Shape::Seq(parts)
                    } else {
                        if_shape
                    }
                } else {
                    if_shape
                }
            }

            Op::Switch { .. } => {
                // Fallback: emit block as-is (dispatch will handle it).
                Shape::Block(block)
            }

            _ => Shape::Block(block),
        }
    }

    /// Find merge point for an if/else using post-dominators.
    fn find_merge(
        &self,
        block: BlockId,
        _then_target: BlockId,
        _else_target: BlockId,
        until: Option<BlockId>,
    ) -> Option<BlockId> {
        // The immediate post-dominator of the branch block is the merge point.
        if let Some(&ipdom) = self.ipdom.get(&block) {
            if ipdom != block {
                // Don't return merge if it's outside our region.
                if let Some(u) = until {
                    // The merge must be dominated by our region boundary or be
                    // the boundary itself.
                    if ipdom == u || dominates(block, ipdom, &self.idom) {
                        return Some(ipdom);
                    }
                }
                return Some(ipdom);
            }
        }
        None
    }

    /// Try to find a merge point by BFS from both targets.
    ///
    /// Returns the first block reachable from both `a` and `b` (in BFS order
    /// from `a`), stopping at `until` and loop boundaries.
    fn find_merge_bfs(
        &self,
        a: BlockId,
        b: BlockId,
        until: Option<BlockId>,
        loop_body: Option<&HashSet<BlockId>>,
    ) -> Option<BlockId> {
        // Collect blocks reachable from `b`.
        let mut reachable_from_b = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(b);
        while let Some(cur) = queue.pop_front() {
            if !reachable_from_b.insert(cur) {
                continue;
            }
            if Some(cur) == until {
                continue;
            }
            if let Some(lb) = loop_body {
                if !lb.contains(&cur) {
                    continue;
                }
            }
            if let Some(succs) = self.cfg.succs.get(&cur) {
                for &s in succs {
                    queue.push_back(s);
                }
            }
        }

        // BFS from `a`, return first block also reachable from `b`
        // (skip `a` itself since it's not a merge point).
        let mut visited_a = HashSet::new();
        let mut q = VecDeque::new();
        if let Some(succs) = self.cfg.succs.get(&a) {
            for &s in succs {
                q.push_back(s);
            }
        }
        while let Some(cur) = q.pop_front() {
            if !visited_a.insert(cur) {
                continue;
            }
            if Some(cur) == until {
                continue;
            }
            if let Some(lb) = loop_body {
                if !lb.contains(&cur) {
                    continue;
                }
            }
            if reachable_from_b.contains(&cur) {
                return Some(cur);
            }
            if let Some(succs) = self.cfg.succs.get(&cur) {
                for &s in succs {
                    q.push_back(s);
                }
            }
        }

        None
    }

    /// Build a Dispatch fallback for remaining blocks when recursion depth
    /// is exceeded. Collects all reachable blocks from `block` up to `until`.
    fn fallback_dispatch(
        &self,
        block: BlockId,
        until: Option<BlockId>,
        loop_body: Option<&HashSet<BlockId>>,
    ) -> Shape {
        let mut blocks = Vec::new();
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(block);

        while let Some(b) = queue.pop_front() {
            if !visited.insert(b) {
                continue;
            }
            if Some(b) == until {
                continue;
            }
            if let Some(lb) = loop_body {
                if !lb.contains(&b) {
                    continue;
                }
            }
            blocks.push(b);
            if let Some(succs) = self.cfg.succs.get(&b) {
                for &s in succs {
                    queue.push_back(s);
                }
            }
        }

        if blocks.is_empty() {
            Shape::Seq(vec![])
        } else {
            Shape::Dispatch {
                entry: block,
                blocks,
            }
        }
    }

    /// Structurize a loop starting at `header`.
    fn structurize_loop(&mut self, header: BlockId, until: Option<BlockId>) -> Shape {
        // Find the loop body.
        let loop_body: HashSet<BlockId> = self
            .loops
            .iter()
            .find(|l| l.header == header)
            .map(|l| l.body.clone())
            .unwrap_or_default();

        let term = self.terminator(header).cloned();

        // Find the exit block (successor of header not in loop body, or
        // the "next" block after the loop).
        let exit_block = self.find_loop_exit(header, &loop_body);

        self.loop_stack.push(header);

        let shape = match term.as_ref() {
            Some(Op::BrIf {
                cond,
                then_target,
                else_target,
                ..
            }) => {
                let mut cond = *cond;
                let mut then_target = *then_target;
                let mut else_target = *else_target;

                // Normalize: strip Not from condition, swap branches.
                if let Some(inner) = self.strip_not(header, cond) {
                    cond = inner;
                    std::mem::swap(&mut then_target, &mut else_target);
                }

                let then_in_loop = loop_body.contains(&then_target);
                let else_in_loop = loop_body.contains(&else_target);

                if then_in_loop && !else_in_loop {
                    // while (cond) { then_body }
                    let body = self.structurize_loop_body(
                        then_target,
                        header,
                        &loop_body,
                    );
                    self.try_for_loop(header, cond, false, body, &loop_body)
                } else if !then_in_loop && else_in_loop {
                    // while (!cond) { else_body }
                    let body = self.structurize_loop_body(
                        else_target,
                        header,
                        &loop_body,
                    );
                    self.try_for_loop(header, cond, true, body, &loop_body)
                } else {
                    // Both branches in loop or both exit — general loop.
                    self.structurize_general_loop(header, &loop_body)
                }
            }
            _ => {
                // Header doesn't end with BrIf — general loop.
                self.structurize_general_loop(header, &loop_body)
            }
        };

        self.loop_stack.pop();

        // Continue with code after the loop.
        if let Some(exit) = exit_block {
            if Some(exit) != until {
                let rest = self.structurize_region(exit, until, None);
                let mut parts = vec![shape];
                match rest {
                    Shape::Seq(inner) => parts.extend(inner),
                    other => parts.push(other),
                }
                Shape::Seq(parts)
            } else {
                shape
            }
        } else {
            shape
        }
    }

    /// Find the block that follows the loop (the exit target).
    fn find_loop_exit(
        &self,
        header: BlockId,
        loop_body: &HashSet<BlockId>,
    ) -> Option<BlockId> {
        // Look for successors of loop blocks that are outside the loop.
        match self.terminator(header) {
            Some(Op::BrIf {
                then_target,
                else_target,
                ..
            }) => {
                if !loop_body.contains(then_target) {
                    Some(*then_target)
                } else if !loop_body.contains(else_target) {
                    Some(*else_target)
                } else {
                    // Both in loop — look at other blocks for exits.
                    self.find_exit_in_body(loop_body)
                }
            }
            _ => self.find_exit_in_body(loop_body),
        }
    }

    /// Find an exit from any block in the loop body.
    fn find_exit_in_body(&self, loop_body: &HashSet<BlockId>) -> Option<BlockId> {
        for &block in loop_body {
            if let Some(succs) = self.cfg.succs.get(&block) {
                for &s in succs {
                    if !loop_body.contains(&s) {
                        return Some(s);
                    }
                }
            }
        }
        None
    }

    /// Structurize the body of a while/for loop (from body_entry up to
    /// the back edge to header).
    fn structurize_loop_body(
        &mut self,
        body_entry: BlockId,
        _header: BlockId,
        loop_body: &HashSet<BlockId>,
    ) -> Shape {
        // Structurize the body with the header as the "until" boundary
        // (when we hit a branch back to header, emit Continue).
        self.structurize_region(body_entry, None, Some(loop_body))
    }

    /// Try to upgrade a WhileLoop to a ForLoop based on block arguments.
    fn try_for_loop(
        &self,
        header: BlockId,
        cond: ValueId,
        cond_negated: bool,
        body: Shape,
        loop_body: &HashSet<BlockId>,
    ) -> Shape {
        let header_block = &self.func.blocks[header];

        // Need block parameters on the header for a for-loop pattern.
        if header_block.params.is_empty() {
            return Shape::WhileLoop {
                header,
                cond,
                cond_negated,
                body: Box::new(body),
            };
        }

        // Find pre-loop Br (predecessor outside the loop).
        let pre_loop_assigns = self.find_pre_loop_assigns(header, loop_body);
        // Find back-edge Br (predecessor inside the loop branching to header).
        let update_assigns = self.find_back_edge_assigns(header, loop_body);

        if let (Some(init_assigns), Some(update_assigns)) = (pre_loop_assigns, update_assigns) {
            Shape::ForLoop {
                header,
                init_assigns,
                cond,
                cond_negated,
                update_assigns,
                body: Box::new(body),
            }
        } else {
            Shape::WhileLoop {
                header,
                cond,
                cond_negated,
                body: Box::new(body),
            }
        }
    }

    /// Find assignments from the pre-loop branch to the header.
    fn find_pre_loop_assigns(
        &self,
        header: BlockId,
        loop_body: &HashSet<BlockId>,
    ) -> Option<Vec<BlockArgAssign>> {
        let preds = self.cfg.preds.get(&header)?;
        for &pred in preds {
            if loop_body.contains(&pred) {
                continue; // Skip back edges.
            }
            if let Some(Op::Br { target, args }) = self.terminator(pred) {
                if *target == header {
                    return Some(self.branch_assigns(header, args));
                }
            }
        }
        None
    }

    /// Find assignments from the back-edge branch to the header.
    fn find_back_edge_assigns(
        &self,
        header: BlockId,
        loop_body: &HashSet<BlockId>,
    ) -> Option<Vec<BlockArgAssign>> {
        let preds = self.cfg.preds.get(&header)?;
        for &pred in preds {
            if !loop_body.contains(&pred) {
                continue; // Skip non-loop predecessors.
            }
            if pred == header {
                continue; // Skip self-loops at header level.
            }
            if let Some(Op::Br { target, args }) = self.terminator(pred) {
                if *target == header {
                    return Some(self.branch_assigns(header, args));
                }
            }
        }
        None
    }

    /// Try to upgrade an IfElse into a LogicalOr or LogicalAnd.
    ///
    /// **OR:** `then_assigns == [{phi, cond}]`, `then_body` empty,
    /// `then_trailing` empty, `else_assigns` empty,
    /// `else_trailing == [{phi, rhs}]`
    /// → `LogicalOr { block, cond, phi, rhs_body: else_body, rhs }`
    ///
    /// **AND:** `else_assigns == [{phi, cond}]`, `else_body` empty,
    /// `else_trailing` empty, `then_assigns` empty,
    /// `then_trailing == [{phi, rhs}]`
    /// → `LogicalAnd { block, cond, phi, rhs_body: then_body, rhs }`
    ///
    /// Also handles **inverted** variants where the short-circuit branch
    /// assigns `!cond` instead of `cond`:
    ///
    /// **Inverted AND:** `then_assigns == [{phi, !cond}]`, `then_body` empty,
    /// `then_trailing` empty, `else_assigns` empty,
    /// `else_trailing == [{phi, rhs}]`
    /// → `LogicalAnd { block, cond: !cond, phi, rhs_body: else_body, rhs }`
    ///
    /// **Inverted OR:** `else_assigns == [{phi, !cond}]`, `else_body` empty,
    /// `else_trailing` empty, `then_assigns` empty,
    /// `then_trailing == [{phi, rhs}]`
    /// → `LogicalOr { block, cond: !cond, phi, rhs_body: then_body, rhs }`
    fn try_logical_op(&self, shape: Shape) -> Shape {
        let Shape::IfElse {
            block,
            cond,
            cond_negated: _,
            ref then_assigns,
            ref then_body,
            ref then_trailing_assigns,
            ref else_assigns,
            ref else_body,
            ref else_trailing_assigns,
        } = shape
        else {
            return shape;
        };

        // OR: then directly assigns phi=cond, else computes rhs
        if then_assigns.len() == 1
            && then_assigns[0].src == cond
            && **then_body == Shape::Seq(vec![])
            && then_trailing_assigns.is_empty()
            && else_assigns.is_empty()
            && else_trailing_assigns.len() == 1
            && else_trailing_assigns[0].dst == then_assigns[0].dst
        {
            let phi = then_assigns[0].dst;
            let rhs = else_trailing_assigns[0].src;
            return Shape::LogicalOr {
                block,
                cond,
                phi,
                rhs_body: else_body.clone(),
                rhs,
            };
        }

        // AND: else directly assigns phi=cond, then computes rhs
        if else_assigns.len() == 1
            && else_assigns[0].src == cond
            && **else_body == Shape::Seq(vec![])
            && else_trailing_assigns.is_empty()
            && then_assigns.is_empty()
            && then_trailing_assigns.len() == 1
            && then_trailing_assigns[0].dst == else_assigns[0].dst
        {
            let phi = else_assigns[0].dst;
            let rhs = then_trailing_assigns[0].src;
            return Shape::LogicalAnd {
                block,
                cond,
                phi,
                rhs_body: then_body.clone(),
                rhs,
            };
        }

        // Inverted AND: then assigns phi=!cond (always false when cond is
        // truthy), else computes rhs → phi = !cond && rhs
        //
        // This occurs when the ABC compiler emits a separate inverse
        // comparison (e.g. CmpLt for BrIf + CmpGe for the short-circuit
        // value) instead of reusing the same value.
        if then_assigns.len() == 1
            && is_boolean_inverse(self.func, then_assigns[0].src, cond)
            && **then_body == Shape::Seq(vec![])
            && then_trailing_assigns.is_empty()
            && else_assigns.is_empty()
            && else_trailing_assigns.len() == 1
            && else_trailing_assigns[0].dst == then_assigns[0].dst
        {
            let phi = then_assigns[0].dst;
            let real_cond = then_assigns[0].src;
            let rhs = else_trailing_assigns[0].src;
            return Shape::LogicalAnd {
                block,
                cond: real_cond,
                phi,
                rhs_body: else_body.clone(),
                rhs,
            };
        }

        // Inverted OR: else assigns phi=!cond (always true when cond is
        // falsy), then computes rhs → phi = !cond || rhs
        if else_assigns.len() == 1
            && is_boolean_inverse(self.func, else_assigns[0].src, cond)
            && **else_body == Shape::Seq(vec![])
            && else_trailing_assigns.is_empty()
            && then_assigns.is_empty()
            && then_trailing_assigns.len() == 1
            && then_trailing_assigns[0].dst == else_assigns[0].dst
        {
            let phi = else_assigns[0].dst;
            let real_cond = else_assigns[0].src;
            let rhs = then_trailing_assigns[0].src;
            return Shape::LogicalOr {
                block,
                cond: real_cond,
                phi,
                rhs_body: then_body.clone(),
                rhs,
            };
        }

        shape
    }

    /// Structurize a general loop (while(true) with break/continue).
    fn structurize_general_loop(
        &mut self,
        header: BlockId,
        loop_body: &HashSet<BlockId>,
    ) -> Shape {
        let body = self.structurize_region(header, None, Some(loop_body));
        Shape::Loop {
            header,
            body: Box::new(body),
        }
    }
}

// -------------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------------

/// Check if two values are boolean inverses of each other.
///
/// Recognizes:
/// - `Not(a)` vs `a`
/// - `Cmp(k1, x, y)` vs `Cmp(k2, x, y)` where `k1 == k2.inverse()`
///
/// Comparison operands are considered equal if they share the same
/// `ValueId` or are structurally identical constants.
fn is_boolean_inverse(func: &Function, a: ValueId, b: ValueId) -> bool {
    let a_op = func
        .insts
        .iter()
        .find_map(|(_, inst)| (inst.result == Some(a)).then_some(&inst.op));
    let b_op = func
        .insts
        .iter()
        .find_map(|(_, inst)| (inst.result == Some(b)).then_some(&inst.op));
    match (a_op, b_op) {
        (Some(Op::Not(inner)), _) if *inner == b => true,
        (_, Some(Op::Not(inner))) if *inner == a => true,
        (Some(Op::Cmp(k1, x1, y1)), Some(Op::Cmp(k2, x2, y2))) => {
            values_equivalent(func, *x1, *x2)
                && values_equivalent(func, *y1, *y2)
                && *k1 == k2.inverse()
        }
        _ => false,
    }
}

/// Check if two values are equivalent: same `ValueId` or identical constants.
fn values_equivalent(func: &Function, a: ValueId, b: ValueId) -> bool {
    if a == b {
        return true;
    }
    let a_const = func.insts.iter().find_map(|(_, inst)| match &inst.op {
        Op::Const(c) if inst.result == Some(a) => Some(c),
        _ => None,
    });
    let b_const = func.insts.iter().find_map(|(_, inst)| match &inst.op {
        Op::Const(c) if inst.result == Some(b) => Some(c),
        _ => None,
    });
    matches!((a_const, b_const), (Some(a), Some(b)) if a == b)
}

// -------------------------------------------------------------------------
// Public API
// -------------------------------------------------------------------------

/// Structurize a function's CFG into a `Shape` tree.
///
/// Single-block functions return `Shape::Block(entry)`.
/// Multi-block functions are analyzed for if/else, loops, etc.
/// Recursion depth is bounded by `MAX_DEPTH`; the dominator and
/// post-dominator computations are nearly linear (Lengauer-Tarjan).
pub fn structurize(func: &Function) -> Shape {
    if func.blocks.len() == 1 {
        return Shape::Block(func.entry);
    }

    let mut s = Structurizer::new(func);
    s.structurize_region(func.entry, None, None)
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::{CmpKind, FunctionSig, Type, Visibility};

    #[test]
    fn test_single_block() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("id", sig, Visibility::Public);
        let a = fb.param(0);
        fb.ret(Some(a));

        let func = fb.build();
        let shape = structurize(&func);
        assert_eq!(shape, Shape::Block(func.entry));
    }

    #[test]
    fn test_linear_chain() {
        // entry → b1 → b2 (return)
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("chain", sig, Visibility::Public);

        let b1 = fb.create_block();
        let b2 = fb.create_block();

        fb.br(b1, &[]);
        fb.switch_to_block(b1);
        fb.br(b2, &[]);
        fb.switch_to_block(b2);
        fb.ret(None);

        let func = fb.build();
        let shape = structurize(&func);

        // Should be Seq([Block(entry), Block(b1), Block(b2)])
        match &shape {
            Shape::Seq(parts) => {
                assert_eq!(parts.len(), 3);
                assert_eq!(parts[0], Shape::Block(func.entry));
            }
            _ => panic!("Expected Seq, got {shape:?}"),
        }
    }

    #[test]
    fn test_if_else_diamond() {
        //   entry: br_if cond, then, else
        //   then:  br merge
        //   else:  br merge
        //   merge: return
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("diamond", sig, Visibility::Public);
        let cond = fb.param(0);

        let then_block = fb.create_block();
        let else_block = fb.create_block();
        let merge_block = fb.create_block();

        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        fb.br(merge_block, &[]);

        fb.switch_to_block(else_block);
        fb.br(merge_block, &[]);

        fb.switch_to_block(merge_block);
        fb.ret(None);

        let func = fb.build();
        let shape = structurize(&func);

        // Should contain an IfElse.
        fn has_if_else(shape: &Shape) -> bool {
            match shape {
                Shape::IfElse { .. } => true,
                Shape::Seq(parts) => parts.iter().any(has_if_else),
                _ => false,
            }
        }
        assert!(has_if_else(&shape), "Expected IfElse in {shape:?}");
    }

    #[test]
    fn test_if_then() {
        //   entry: br_if cond, then, merge
        //   then:  br merge
        //   merge: return
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("if_then", sig, Visibility::Public);
        let cond = fb.param(0);

        let then_block = fb.create_block();
        let merge_block = fb.create_block();

        fb.br_if(cond, then_block, &[], merge_block, &[]);

        fb.switch_to_block(then_block);
        fb.br(merge_block, &[]);

        fb.switch_to_block(merge_block);
        fb.ret(None);

        let func = fb.build();
        let shape = structurize(&func);

        fn has_if_else(shape: &Shape) -> bool {
            match shape {
                Shape::IfElse { .. } => true,
                Shape::Seq(parts) => parts.iter().any(has_if_else),
                _ => false,
            }
        }
        assert!(has_if_else(&shape), "Expected IfElse in {shape:?}");

        // The else body should be empty (Seq([])) since else goes directly
        // to merge.
        fn find_if_else(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::IfElse { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_if_else),
                _ => None,
            }
        }
        if let Some(Shape::IfElse { else_body, .. }) = find_if_else(&shape) {
            assert_eq!(**else_body, Shape::Seq(vec![]));
        }
    }

    #[test]
    fn test_while_loop() {
        //   entry:  br header
        //   header: br_if cond, body, exit
        //   body:   br header
        //   exit:   return
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("while_loop", sig, Visibility::Public);
        let cond = fb.param(0);

        let header = fb.create_block();
        let body = fb.create_block();
        let exit = fb.create_block();

        fb.br(header, &[]);

        fb.switch_to_block(header);
        fb.br_if(cond, body, &[], exit, &[]);

        fb.switch_to_block(body);
        fb.br(header, &[]);

        fb.switch_to_block(exit);
        fb.ret(None);

        let func = fb.build();
        let shape = structurize(&func);

        fn has_while(shape: &Shape) -> bool {
            match shape {
                Shape::WhileLoop { .. } => true,
                Shape::Seq(parts) => parts.iter().any(has_while),
                _ => false,
            }
        }
        assert!(has_while(&shape), "Expected WhileLoop in {shape:?}");
    }

    #[test]
    fn test_for_loop() {
        //   entry:  v_init = 0; br header(v_init)
        //   header(v_i): v_cond = v_i < 10; br_if v_cond, body, exit
        //   body:   v_next = v_i + 1; br header(v_next)
        //   exit:   return
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("for_loop", sig, Visibility::Public);

        let (header, header_vals) = fb.create_block_with_params(&[Type::Int(64)]);
        let body = fb.create_block();
        let exit = fb.create_block();

        let v_init = fb.const_int(0);
        fb.br(header, &[v_init]);

        fb.switch_to_block(header);
        let v_i = header_vals[0];
        let v_n = fb.const_int(10);
        let v_cond = fb.cmp(CmpKind::Lt, v_i, v_n);
        fb.br_if(v_cond, body, &[], exit, &[]);

        fb.switch_to_block(body);
        let v_one = fb.const_int(1);
        let v_next = fb.add(v_i, v_one);
        fb.br(header, &[v_next]);

        fb.switch_to_block(exit);
        fb.ret(None);

        let func = fb.build();
        let shape = structurize(&func);

        fn has_for(shape: &Shape) -> bool {
            match shape {
                Shape::ForLoop { .. } => true,
                Shape::Seq(parts) => parts.iter().any(has_for),
                _ => false,
            }
        }
        assert!(has_for(&shape), "Expected ForLoop in {shape:?}");
    }

    #[test]
    fn test_general_loop() {
        //   entry:  br header
        //   header: <compute>; br body
        //   body:   br_if cond, exit, header  (exit in middle)
        //   exit:   return
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("general_loop", sig, Visibility::Public);
        let cond = fb.param(0);

        let header = fb.create_block();
        let body = fb.create_block();
        let exit = fb.create_block();

        fb.br(header, &[]);

        fb.switch_to_block(header);
        fb.br(body, &[]);

        fb.switch_to_block(body);
        fb.br_if(cond, exit, &[], header, &[]);

        fb.switch_to_block(exit);
        fb.ret(None);

        let func = fb.build();
        let shape = structurize(&func);

        fn has_loop(shape: &Shape) -> bool {
            match shape {
                Shape::Loop { .. } => true,
                Shape::Seq(parts) => parts.iter().any(has_loop),
                _ => false,
            }
        }
        assert!(has_loop(&shape), "Expected Loop in {shape:?}");
    }

    #[test]
    fn test_nested_if_in_loop() {
        //   entry: br header
        //   header: br_if cond, if_then, if_else
        //   if_then: br merge
        //   if_else: br merge
        //   merge: br_if loop_cond, header, exit
        //   exit: return
        //
        // This is a general loop (exit not at header) with if/else inside.
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Bool],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("nested", sig, Visibility::Public);
        let cond = fb.param(0);
        let loop_cond = fb.param(1);

        let header = fb.create_block();
        let if_then = fb.create_block();
        let if_else = fb.create_block();
        let merge = fb.create_block();
        let exit = fb.create_block();

        fb.br(header, &[]);

        fb.switch_to_block(header);
        fb.br_if(cond, if_then, &[], if_else, &[]);

        fb.switch_to_block(if_then);
        fb.br(merge, &[]);

        fb.switch_to_block(if_else);
        fb.br(merge, &[]);

        fb.switch_to_block(merge);
        fb.br_if(loop_cond, header, &[], exit, &[]);

        fb.switch_to_block(exit);
        fb.ret(None);

        let func = fb.build();
        let shape = structurize(&func);

        fn has_loop(shape: &Shape) -> bool {
            match shape {
                Shape::Loop { .. } => true,
                Shape::Seq(parts) => parts.iter().any(has_loop),
                _ => false,
            }
        }
        assert!(has_loop(&shape), "Expected Loop in {shape:?}");
    }

    #[test]
    fn test_dominators() {
        //   entry → a, b
        //   a → merge
        //   b → merge
        //   merge → (return)
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("dom_test", sig, Visibility::Public);
        let cond = fb.param(0);

        let a = fb.create_block();
        let b = fb.create_block();
        let merge = fb.create_block();

        fb.br_if(cond, a, &[], b, &[]);

        fb.switch_to_block(a);
        fb.br(merge, &[]);

        fb.switch_to_block(b);
        fb.br(merge, &[]);

        fb.switch_to_block(merge);
        fb.ret(None);

        let func = fb.build();
        let cfg = build_cfg(&func);
        let idom = compute_dominators(&func, &cfg);

        // Entry dominates everything.
        assert!(dominates(func.entry, a, &idom));
        assert!(dominates(func.entry, b, &idom));
        assert!(dominates(func.entry, merge, &idom));

        // a does not dominate merge (b also reaches it).
        assert!(!dominates(a, merge, &idom));
        assert!(!dominates(b, merge, &idom));

        // merge's idom should be entry.
        assert_eq!(idom[&merge], func.entry);
    }

    #[test]
    fn test_logical_or_trailing_merge_assigns() {
        // entry: br_if cond, merge(cond), else_mid()
        // else_mid: v_cmp = cmp.gt ...; br merge(v_cmp)
        // merge(v_phi): return v_phi
        //
        // This is the short-circuit OR pattern: v_phi = cond || v_cmp.
        // The structurizer should upgrade the IfElse to LogicalOr.
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Bool,
        };
        let mut fb = FunctionBuilder::new("trailing_else", sig, Visibility::Public);
        let cond = fb.param(0);
        let a = fb.param(1);
        let b = fb.param(2);

        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);
        let else_mid = fb.create_block();

        // entry: br_if cond → merge(cond), else_mid()
        fb.br_if(cond, merge, &[cond], else_mid, &[]);

        // else_mid: v_cmp = a > b; br merge(v_cmp)
        fb.switch_to_block(else_mid);
        let v_cmp = fb.cmp(CmpKind::Gt, a, b);
        fb.br(merge, &[v_cmp]);

        // merge(v_phi): return v_phi
        fb.switch_to_block(merge);
        let v_phi = merge_vals[0];
        fb.ret(Some(v_phi));

        let func = fb.build();
        let shape = structurize(&func);

        // Should produce LogicalOr.
        fn find_logical_or(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::LogicalOr { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_logical_or),
                _ => None,
            }
        }

        let lo = find_logical_or(&shape).expect("Expected LogicalOr in shape");
        if let Shape::LogicalOr {
            cond: lo_cond,
            phi,
            rhs,
            ..
        } = lo
        {
            assert_eq!(*lo_cond, cond);
            assert_eq!(*phi, v_phi);
            assert_eq!(*rhs, v_cmp);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_if_else_both_trailing_assigns() {
        // entry: br_if cond, then_mid(), else_mid()
        // then_mid: v1 = const 1; br merge(v1)
        // else_mid: v2 = const 2; br merge(v2)
        // merge(v_phi): return
        //
        // Both branches go through intermediate blocks before reaching
        // merge with args. Verify both sets of assigns are captured.
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("trailing_both", sig, Visibility::Public);
        let cond = fb.param(0);

        let then_mid = fb.create_block();
        let else_mid = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Int(64)]);

        fb.br_if(cond, then_mid, &[], else_mid, &[]);

        fb.switch_to_block(then_mid);
        let v1 = fb.const_int(1);
        fb.br(merge, &[v1]);

        fb.switch_to_block(else_mid);
        let v2 = fb.const_int(2);
        fb.br(merge, &[v2]);

        fb.switch_to_block(merge);
        let v_phi = merge_vals[0];
        fb.ret(Some(v_phi));

        let func = fb.build();
        let shape = structurize(&func);

        fn find_if_else(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::IfElse { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_if_else),
                _ => None,
            }
        }

        let ie = find_if_else(&shape).expect("Expected IfElse in shape");
        if let Shape::IfElse {
            then_trailing_assigns,
            else_trailing_assigns,
            ..
        } = ie
        {
            assert!(
                then_trailing_assigns
                    .iter()
                    .any(|a| a.dst == v_phi && a.src == v1),
                "then_trailing_assigns should contain v_phi=v1, got: {then_trailing_assigns:?}"
            );
            assert!(
                else_trailing_assigns
                    .iter()
                    .any(|a| a.dst == v_phi && a.src == v2),
                "else_trailing_assigns should contain v_phi=v2, got: {else_trailing_assigns:?}"
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_logical_and() {
        // entry: br_if cond, then_mid(), merge(cond)
        // then_mid: v_cmp = cmp.lt ...; br merge(v_cmp)
        // merge(v_phi): return v_phi
        //
        // Short-circuit AND: v_phi = cond && v_cmp.
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Bool,
        };
        let mut fb = FunctionBuilder::new("logical_and", sig, Visibility::Public);
        let cond = fb.param(0);
        let a = fb.param(1);
        let b = fb.param(2);

        let then_mid = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);

        // entry: br_if cond → then_mid(), merge(cond)
        fb.br_if(cond, then_mid, &[], merge, &[cond]);

        // then_mid: v_cmp = a < b; br merge(v_cmp)
        fb.switch_to_block(then_mid);
        let v_cmp = fb.cmp(CmpKind::Lt, a, b);
        fb.br(merge, &[v_cmp]);

        // merge(v_phi): return v_phi
        fb.switch_to_block(merge);
        let v_phi = merge_vals[0];
        fb.ret(Some(v_phi));

        let func = fb.build();
        let shape = structurize(&func);

        fn find_logical_and(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::LogicalAnd { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_logical_and),
                _ => None,
            }
        }

        let la = find_logical_and(&shape).expect("Expected LogicalAnd in shape");
        if let Shape::LogicalAnd {
            cond: la_cond,
            phi,
            rhs,
            ..
        } = la
        {
            assert_eq!(*la_cond, cond);
            assert_eq!(*phi, v_phi);
            assert_eq!(*rhs, v_cmp);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_logical_or_with_empty_body() {
        // entry: br_if cond, merge(cond), else_block()
        // else_block: br merge(other)   ← no intermediate computation
        // merge(v_phi): return v_phi
        //
        // Simplest OR: v_phi = cond || other (rhs_body is empty).
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Bool],
            return_ty: Type::Bool,
        };
        let mut fb = FunctionBuilder::new("or_simple", sig, Visibility::Public);
        let cond = fb.param(0);
        let other = fb.param(1);

        let else_block = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);

        fb.br_if(cond, merge, &[cond], else_block, &[]);

        fb.switch_to_block(else_block);
        fb.br(merge, &[other]);

        fb.switch_to_block(merge);
        let v_phi = merge_vals[0];
        fb.ret(Some(v_phi));

        let func = fb.build();
        let shape = structurize(&func);

        fn find_logical_or(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::LogicalOr { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_logical_or),
                _ => None,
            }
        }

        let lo = find_logical_or(&shape).expect("Expected LogicalOr in shape");
        if let Shape::LogicalOr { rhs_body, .. } = lo {
            // The rhs_body should contain only Block(else_block) which
            // has no non-terminator instructions.
            assert!(
                matches!(**rhs_body, Shape::Block(_)),
                "Expected Block in rhs_body, got: {rhs_body:?}"
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_inverted_logical_and() {
        // Simulates the ABC pattern where && is lowered with inverse
        // comparisons:
        //
        //   entry: v_ge = cmp.ge(x, 0); v_lt = cmp.lt(x, 0);
        //          br_if v_lt, merge(v_ge), rhs_block()
        //   rhs_block: v_rhs = cmp.ge(y, 60); br merge(v_rhs)
        //   merge(v_phi): return v_phi
        //
        // v_lt is true ⟹ v_ge is false (short-circuit), so:
        //   v_phi = v_ge && v_rhs
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Bool,
        };
        let mut fb = FunctionBuilder::new("inv_and", sig, Visibility::Public);
        let x = fb.param(0);
        let y = fb.param(1);

        let zero = fb.const_int(0);
        let v_ge = fb.cmp(CmpKind::Ge, x, zero);
        let zero2 = fb.const_int(0);
        let v_lt = fb.cmp(CmpKind::Lt, x, zero2);

        let rhs_block = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);

        // br_if v_lt → merge(v_ge), rhs_block()
        fb.br_if(v_lt, merge, &[v_ge], rhs_block, &[]);

        fb.switch_to_block(rhs_block);
        let sixty = fb.const_int(60);
        let v_rhs = fb.cmp(CmpKind::Ge, y, sixty);
        fb.br(merge, &[v_rhs]);

        fb.switch_to_block(merge);
        let v_phi = merge_vals[0];
        fb.ret(Some(v_phi));

        let func = fb.build();
        let shape = structurize(&func);

        fn find_logical_and(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::LogicalAnd { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_logical_and),
                _ => None,
            }
        }

        let la = find_logical_and(&shape).expect("Expected LogicalAnd in shape");
        if let Shape::LogicalAnd {
            cond: la_cond,
            phi,
            rhs,
            ..
        } = la
        {
            assert_eq!(*la_cond, v_ge, "cond should be the v_ge (inverse of BrIf condition)");
            assert_eq!(*phi, v_phi);
            assert_eq!(*rhs, v_rhs);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_inverted_logical_or() {
        // Simulates an inverted OR where BrIf uses cond but else branch
        // assigns !cond (always true when cond is false):
        //
        //   entry: v_lt = cmp.lt(x, 0); v_ge = cmp.ge(x, 0);
        //          br_if v_lt, rhs_block(), merge(v_ge)
        //   rhs_block: v_rhs = cmp.ge(y, 60); br merge(v_rhs)
        //   merge(v_phi): return v_phi
        //
        // v_lt is false ⟹ v_ge is true (short-circuit), so:
        //   v_phi = v_ge || v_rhs
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Bool,
        };
        let mut fb = FunctionBuilder::new("inv_or", sig, Visibility::Public);
        let x = fb.param(0);
        let y = fb.param(1);

        let zero = fb.const_int(0);
        let v_lt = fb.cmp(CmpKind::Lt, x, zero);
        let zero2 = fb.const_int(0);
        let v_ge = fb.cmp(CmpKind::Ge, x, zero2);

        let rhs_block = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);

        // br_if v_lt → rhs_block(), merge(v_ge)
        fb.br_if(v_lt, rhs_block, &[], merge, &[v_ge]);

        fb.switch_to_block(rhs_block);
        let sixty = fb.const_int(60);
        let v_rhs = fb.cmp(CmpKind::Ge, y, sixty);
        fb.br(merge, &[v_rhs]);

        fb.switch_to_block(merge);
        let v_phi = merge_vals[0];
        fb.ret(Some(v_phi));

        let func = fb.build();
        let shape = structurize(&func);

        fn find_logical_or(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::LogicalOr { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_logical_or),
                _ => None,
            }
        }

        let lo = find_logical_or(&shape).expect("Expected LogicalOr in shape");
        if let Shape::LogicalOr {
            cond: lo_cond,
            phi,
            rhs,
            ..
        } = lo
        {
            assert_eq!(*lo_cond, v_ge, "cond should be v_ge (inverse of BrIf condition)");
            assert_eq!(*phi, v_phi);
            assert_eq!(*rhs, v_rhs);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_inverted_logical_and_with_not() {
        // Same as inverted AND but using Not(v) instead of inverse Cmp:
        //
        //   entry: v_bool = cmp.ge(x, 0); v_not = not(v_bool);
        //          br_if v_not, merge(v_bool), rhs_block()
        //   rhs_block: v_rhs = ...; br merge(v_rhs)
        //   merge(v_phi): return v_phi
        //
        //   v_phi = v_bool && v_rhs
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Bool,
        };
        let mut fb = FunctionBuilder::new("inv_and_not", sig, Visibility::Public);
        let x = fb.param(0);
        let y = fb.param(1);

        let zero = fb.const_int(0);
        let v_bool = fb.cmp(CmpKind::Ge, x, zero);
        let v_not = fb.not(v_bool);

        let rhs_block = fb.create_block();
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Bool]);

        fb.br_if(v_not, merge, &[v_bool], rhs_block, &[]);

        fb.switch_to_block(rhs_block);
        let sixty = fb.const_int(60);
        let v_rhs = fb.cmp(CmpKind::Ge, y, sixty);
        fb.br(merge, &[v_rhs]);

        fb.switch_to_block(merge);
        let v_phi = merge_vals[0];
        fb.ret(Some(v_phi));

        let func = fb.build();
        let shape = structurize(&func);

        fn find_logical_and(shape: &Shape) -> Option<&Shape> {
            match shape {
                s @ Shape::LogicalAnd { .. } => Some(s),
                Shape::Seq(parts) => parts.iter().find_map(find_logical_and),
                _ => None,
            }
        }

        let la = find_logical_and(&shape).expect("Expected LogicalAnd in shape");
        if let Shape::LogicalAnd {
            cond: la_cond, ..
        } = la
        {
            assert_eq!(*la_cond, v_bool, "cond should be v_bool (inner of Not)");
        } else {
            unreachable!();
        }
    }
}
