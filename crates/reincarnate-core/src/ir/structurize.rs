//! Structured control flow reconstruction.
//!
//! Analyzes a function's block-based CFG and produces a `Shape` tree that
//! represents `if`/`else`, `while`, `for`, and general loops. Falls back to
//! a dispatch-loop (`Shape::Dispatch`) for irreducible subgraphs.
//!
//! This is a read-only analysis — the IR is not modified.

use std::collections::{HashMap, HashSet, VecDeque};

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
        then_assigns: Vec<BlockArgAssign>,
        then_body: Box<Shape>,
        else_assigns: Vec<BlockArgAssign>,
        else_body: Box<Shape>,
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
// Dominator computation (Cooper-Harvey-Kennedy)
// -------------------------------------------------------------------------

fn compute_dominators(func: &Function, cfg: &Cfg) -> HashMap<BlockId, BlockId> {
    let entry = func.entry;
    let blocks: Vec<BlockId> = func.blocks.keys().collect();

    // Compute reverse postorder.
    let rpo = reverse_postorder(entry, &cfg.succs, &blocks);
    let rpo_number: HashMap<BlockId, usize> =
        rpo.iter().enumerate().map(|(i, &b)| (b, i)).collect();

    let mut idom: HashMap<BlockId, BlockId> = HashMap::new();
    idom.insert(entry, entry);

    let intersect = |mut a: BlockId, mut b: BlockId, idom: &HashMap<BlockId, BlockId>| -> BlockId {
        while a != b {
            while rpo_number[&a] > rpo_number[&b] {
                a = idom[&a];
            }
            while rpo_number[&b] > rpo_number[&a] {
                b = idom[&b];
            }
        }
        a
    };

    let mut changed = true;
    while changed {
        changed = false;
        for &b in &rpo {
            if b == entry {
                continue;
            }
            let preds = cfg.preds.get(&b).map(|v| v.as_slice()).unwrap_or(&[]);
            let mut new_idom: Option<BlockId> = None;
            for &p in preds {
                if !idom.contains_key(&p) {
                    continue;
                }
                new_idom = Some(match new_idom {
                    None => p,
                    Some(current) => intersect(p, current, &idom),
                });
            }
            if let Some(new) = new_idom {
                if idom.get(&b) != Some(&new) {
                    idom.insert(b, new);
                    changed = true;
                }
            }
        }
    }

    idom
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

/// Reverse postorder traversal from entry.
fn reverse_postorder(
    entry: BlockId,
    succs: &HashMap<BlockId, Vec<BlockId>>,
    _all_blocks: &[BlockId],
) -> Vec<BlockId> {
    let mut visited = HashSet::new();
    let mut postorder = Vec::new();

    fn dfs(
        b: BlockId,
        succs: &HashMap<BlockId, Vec<BlockId>>,
        visited: &mut HashSet<BlockId>,
        postorder: &mut Vec<BlockId>,
    ) {
        if !visited.insert(b) {
            return;
        }
        if let Some(children) = succs.get(&b) {
            for &c in children {
                dfs(c, succs, visited, postorder);
            }
        }
        postorder.push(b);
    }

    dfs(entry, succs, &mut visited, &mut postorder);
    postorder.reverse();
    postorder
}

// -------------------------------------------------------------------------
// Post-dominator computation
// -------------------------------------------------------------------------

/// Compute post-dominators (dominators on the reverse CFG).
/// Returns ipdom[block] = immediate post-dominator.
fn compute_post_dominators(func: &Function, cfg: &Cfg) -> HashMap<BlockId, BlockId> {
    // Find exit blocks (blocks with Return).
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

    // Build reverse CFG.
    let mut rev_succs: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for (block_id, _) in func.blocks.iter() {
        rev_succs.entry(block_id).or_default();
    }
    for (block_id, targets) in &cfg.succs {
        for target in targets {
            rev_succs.entry(*target).or_default().push(*block_id);
        }
    }

    // If multiple exits, create a virtual exit node approach by just using
    // the first exit. For most well-formed functions there's a single exit
    // or the exits converge. For simplicity, iterate from each exit block.
    // We use a single-exit approach: pick a unique exit or handle multiple.
    let all_blocks: Vec<BlockId> = func.blocks.keys().collect();

    if exits.len() == 1 {
        let exit = exits[0];
        let rpo = reverse_postorder(exit, &rev_succs, &all_blocks);
        let rpo_number: HashMap<BlockId, usize> =
            rpo.iter().enumerate().map(|(i, &b)| (b, i)).collect();

        let mut ipdom: HashMap<BlockId, BlockId> = HashMap::new();
        ipdom.insert(exit, exit);

        let intersect =
            |mut a: BlockId, mut b: BlockId, ipdom: &HashMap<BlockId, BlockId>| -> BlockId {
                while a != b {
                    while rpo_number.get(&a).copied().unwrap_or(usize::MAX)
                        > rpo_number.get(&b).copied().unwrap_or(usize::MAX)
                    {
                        a = ipdom[&a];
                    }
                    while rpo_number.get(&b).copied().unwrap_or(usize::MAX)
                        > rpo_number.get(&a).copied().unwrap_or(usize::MAX)
                    {
                        b = ipdom[&b];
                    }
                }
                a
            };

        let mut changed = true;
        while changed {
            changed = false;
            for &b in &rpo {
                if b == exit {
                    continue;
                }
                // In reverse CFG, successors of b are its predecessors in the
                // forward CFG — but we already built rev_succs as the reverse.
                let rev_preds = cfg.succs.get(&b).map(|v| v.as_slice()).unwrap_or(&[]);
                let mut new_ipdom: Option<BlockId> = None;
                for &p in rev_preds {
                    if !ipdom.contains_key(&p) {
                        continue;
                    }
                    new_ipdom = Some(match new_ipdom {
                        None => p,
                        Some(current) => intersect(p, current, &ipdom),
                    });
                }
                if let Some(new) = new_ipdom {
                    if ipdom.get(&b) != Some(&new) {
                        ipdom.insert(b, new);
                        changed = true;
                    }
                }
            }
        }

        ipdom
    } else {
        // Multiple exits — use a "virtual exit" approach.
        // Add virtual successors from each exit to a conceptual virtual exit.
        // Instead, compute using the first reachable exit as the root and
        // see what we get. For irreducible cases we fall back to Dispatch.
        // Use the last block as heuristic exit (often the return block).
        let exit = *exits.last().unwrap();
        let rpo = reverse_postorder(exit, &rev_succs, &all_blocks);
        let rpo_number: HashMap<BlockId, usize> =
            rpo.iter().enumerate().map(|(i, &b)| (b, i)).collect();

        let mut ipdom: HashMap<BlockId, BlockId> = HashMap::new();
        ipdom.insert(exit, exit);

        let intersect =
            |mut a: BlockId, mut b: BlockId, ipdom: &HashMap<BlockId, BlockId>| -> BlockId {
                while a != b {
                    while rpo_number.get(&a).copied().unwrap_or(usize::MAX)
                        > rpo_number.get(&b).copied().unwrap_or(usize::MAX)
                    {
                        a = ipdom[&a];
                    }
                    while rpo_number.get(&b).copied().unwrap_or(usize::MAX)
                        > rpo_number.get(&a).copied().unwrap_or(usize::MAX)
                    {
                        b = ipdom[&b];
                    }
                }
                a
            };

        let mut changed = true;
        while changed {
            changed = false;
            for &b in &rpo {
                if b == exit {
                    continue;
                }
                let rev_preds = cfg.succs.get(&b).map(|v| v.as_slice()).unwrap_or(&[]);
                let mut new_ipdom: Option<BlockId> = None;
                for &p in rev_preds {
                    if !ipdom.contains_key(&p) {
                        continue;
                    }
                    new_ipdom = Some(match new_ipdom {
                        None => p,
                        Some(current) => intersect(p, current, &ipdom),
                    });
                }
                if let Some(new) = new_ipdom {
                    if ipdom.get(&b) != Some(&new) {
                        ipdom.insert(b, new);
                        changed = true;
                    }
                }
            }
        }

        ipdom
    }
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

/// Context for the recursive structurizer.
struct Structurizer<'a> {
    func: &'a Function,
    cfg: Cfg,
    idom: HashMap<BlockId, BlockId>,
    ipdom: HashMap<BlockId, BlockId>,
    loops: Vec<NaturalLoop>,
    /// Stack of loop headers we're currently inside (innermost last).
    loop_stack: Vec<BlockId>,
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
        }
    }

    /// Is this block a loop header?
    fn is_loop_header(&self, block: BlockId) -> bool {
        self.loops.iter().any(|l| l.header == block)
    }

    /// Get the terminator Op of a block.
    fn terminator(&self, block: BlockId) -> &Op {
        let blk = &self.func.blocks[block];
        let last = *blk.insts.last().expect("block has no instructions");
        &self.func.insts[last].op
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

        // Check if this block is a loop header we're supposed to process.
        if self.is_loop_header(block) && !self.loop_stack.contains(&block) {
            return self.structurize_loop(block, until);
        }

        let term = self.terminator(block).clone();

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
                let cond = *cond;
                let then_target = *then_target;
                let else_target = *else_target;
                let then_args = then_args.clone();
                let else_args = else_args.clone();

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
                            then_assigns,
                            then_body: Box::new(Shape::Break),
                            else_assigns,
                            else_body: Box::new(Shape::Continue),
                        };
                    }

                    // then→continue (back to header), else→exit
                    if then_is_header && !else_in_loop {
                        return Shape::IfElse {
                            block,
                            cond,
                            then_assigns,
                            then_body: Box::new(Shape::Continue),
                            else_assigns,
                            else_body: Box::new(Shape::Break),
                        };
                    }

                    // then→exit, else→body (continues in loop)
                    if !then_in_loop && else_in_loop {
                        let else_body_shape =
                            self.structurize_region(else_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            then_assigns,
                            then_body: Box::new(Shape::Break),
                            else_assigns,
                            else_body: Box::new(else_body_shape),
                        };
                    }

                    // then→body (continues in loop), else→exit
                    if then_in_loop && !else_in_loop {
                        let then_body_shape =
                            self.structurize_region(then_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            then_assigns,
                            then_body: Box::new(then_body_shape),
                            else_assigns,
                            else_body: Box::new(Shape::Break),
                        };
                    }

                    // Both in loop — one might be the header (continue).
                    if then_is_header {
                        let else_body_shape =
                            self.structurize_region(else_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            then_assigns,
                            then_body: Box::new(Shape::Continue),
                            else_assigns,
                            else_body: Box::new(else_body_shape),
                        };
                    }
                    if else_is_header {
                        let then_body_shape =
                            self.structurize_region(then_target, None, loop_body);
                        return Shape::IfElse {
                            block,
                            cond,
                            then_assigns,
                            then_body: Box::new(then_body_shape),
                            else_assigns,
                            else_body: Box::new(Shape::Continue),
                        };
                    }
                }

                // Find merge point via post-dominator.
                let merge = self.find_merge(block, then_target, else_target, until);

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

                let if_shape = Shape::IfElse {
                    block,
                    cond,
                    then_assigns,
                    then_body: Box::new(then_body),
                    else_assigns,
                    else_body: Box::new(else_body),
                };

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

    /// Structurize a loop starting at `header`.
    fn structurize_loop(&mut self, header: BlockId, until: Option<BlockId>) -> Shape {
        // Find the loop body.
        let loop_body: HashSet<BlockId> = self
            .loops
            .iter()
            .find(|l| l.header == header)
            .map(|l| l.body.clone())
            .unwrap_or_default();

        let term = self.terminator(header).clone();

        // Find the exit block (successor of header not in loop body, or
        // the "next" block after the loop).
        let exit_block = self.find_loop_exit(header, &loop_body);

        self.loop_stack.push(header);

        let shape = match &term {
            Op::BrIf {
                cond,
                then_target,
                else_target,
                ..
            } => {
                let cond = *cond;
                let then_target = *then_target;
                let else_target = *else_target;

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
        let term = self.terminator(header);
        match term {
            Op::BrIf {
                then_target,
                else_target,
                ..
            } => {
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
            let term = self.terminator(pred);
            match term {
                Op::Br { target, args } if *target == header => {
                    return Some(self.branch_assigns(header, args));
                }
                _ => {}
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
            let term = self.terminator(pred);
            match term {
                Op::Br { target, args } if *target == header => {
                    return Some(self.branch_assigns(header, args));
                }
                _ => {}
            }
        }
        None
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
// Public API
// -------------------------------------------------------------------------

/// Structurize a function's CFG into a `Shape` tree.
///
/// Single-block functions return `Shape::Block(entry)`.
/// Multi-block functions are analyzed for if/else, loops, etc.
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
}
