use std::collections::{BTreeSet, HashMap, HashSet};

use datawin::bytecode::decode::{self, Instruction, Operand};
use datawin::bytecode::opcode::Opcode;
use datawin::bytecode::types::{ComparisonKind, DataType, InstanceType, VariableRef};
use reincarnate_core::entity::EntityRef;
use reincarnate_core::ir::builder::FunctionBuilder;
use reincarnate_core::ir::block::BlockId;
use reincarnate_core::ir::func::{CaptureMode, Function, MethodKind, Visibility};
use reincarnate_core::ir::inst::{CmpKind, Op};
use reincarnate_core::ir::ty::{FunctionSig, Type};
use reincarnate_core::ir::value::{Constant, ValueId};

/// Context for translating a single code entry.
pub struct TranslateCtx<'a> {
    /// FUNC function entries: entry_index → resolved name.
    pub function_names: &'a HashMap<u32, String>,
    /// GMS2.3+ pushref asset name map: (type_tag << 24) | asset_idx → raw GML name.
    /// Built from SPRT (type 1), SOND (type 2), BGND (type 3), SCPT (type 5),
    /// FONT (type 6), SHDR (type 8), ROOM (type 9).
    pub asset_ref_names: &'a HashMap<u32, String>,
    /// VARI variable entries: entry_index → (name, instance_type).
    pub variables: &'a [(String, i32)],
    /// FUNC linked-list reference map: absolute bytecode address → func entry index.
    pub func_ref_map: &'a HashMap<usize, usize>,
    /// VARI linked-list reference map: absolute bytecode address → vari entry index.
    pub vari_ref_map: &'a HashMap<usize, usize>,
    /// Absolute file offset where this code entry's bytecode begins.
    pub bytecode_offset: usize,
    /// Pre-resolved local variable names: `(local_index, name)` pairs.
    /// Derived from `CodeLocals` by the caller before constructing `TranslateCtx`.
    /// Empty when no debug info is available.
    pub local_names: &'a [(u32, String)],
    /// Pre-resolved string table (STRG chunk), indexed by string id.
    /// Used for `Push StringIndex(idx)` instructions.
    pub string_table: &'a [String],
    /// Whether this is an instance method (has self param).
    pub has_self: bool,
    /// Whether this is a collision event (has other param).
    pub has_other: bool,
    /// Number of declared arguments.
    pub arg_count: u16,
    /// Object names indexed by object ID (for resolving numeric instance IDs).
    pub obj_names: &'a [String],
    /// Class name for event handlers (used to type the self parameter).
    pub class_name: Option<&'a str>,
    /// Object index of the owning object (for recognizing self-references).
    /// The GameMaker compiler often uses the object's own index as the instance
    /// type instead of -1 (Own). When `instance >= 0` and matches this index,
    /// the access should be treated as `self.field`, not a cross-object reference.
    pub self_object_index: Option<usize>,
    /// Object indices of all ancestors in the parent chain.
    /// When `instance >= 0` matches any ancestor, the access is also self (inherited field).
    pub ancestor_indices: HashSet<usize>,
    /// Set of clean script names (for injecting self at call sites).
    pub script_names: &'a HashSet<String>,
    /// True when translating a with-body closure (extracted from a PushEnv/PopEnv pair).
    /// In this context, a PopEnv instruction is an early-exit signal — the outer with-loop
    /// is managed by `withInstances`, so we do NOT emit `withEnd()` for PopEnv.
    pub is_with_body: bool,
}

/// Translate a single code entry's bytecode into an IR Function.
///
/// Returns `(main_func, extra_funcs)` where `extra_funcs` are closure
/// functions extracted from `with`-block bodies (one per PushEnv/PopEnv pair).
pub fn translate_code_entry(
    bytecode: &[u8],
    func_name: &str,
    ctx: &TranslateCtx,
) -> Result<(Function, Vec<Function>), String> {
    let all_instructions = decode::decode(bytecode).map_err(|e| format!("{func_name}: {e}"))?;
    if all_instructions.is_empty() {
        let func = build_empty_function(func_name, ctx)?;
        return Ok((func, vec![]));
    }

    // Filter to only instructions reachable from the entry point.
    // In GMS2.3+ shared bytecode blobs, the decoded range may include
    // sibling functions' code beyond this function's Ret/Exit. Without
    // filtering, their branches create spurious block starts that cause
    // stack underflows.
    let instructions = filter_reachable(&all_instructions);

    // Pre-detect with-block ranges so we can exclude their blocks from the outer CFG.
    let with_ranges = find_with_ranges(&instructions);

    // Pass 1 & 2: Create IR blocks, excluding with-body offsets.
    // Old-style scripts may use argumentN without declaring parameters —
    // scan for implicit argument references to determine true arg count.
    let scan = scan_implicit_args(&instructions, ctx);
    let effective_arg_count = ctx.arg_count.max(scan.count);
    let mut sig = build_signature_with_args(ctx, effective_arg_count);
    // Scripts that read `argument_count` or use `argument[dynamic_idx]` are truly
    // variadic — they accept any number of arguments at the call site.  Emit a
    // rest parameter `...args: any[]` so TypeScript call sites are not flagged for
    // passing extra arguments.
    if scan.uses_dynamic_args {
        sig.params.push(Type::Array(Box::new(Type::Dynamic)));
        sig.defaults.push(None);
        sig.has_rest_param = true;
    }
    let mut fb = FunctionBuilder::new(func_name, sig, Visibility::Public);

    // Name parameters.
    let mut param_idx = 0;
    if ctx.has_self {
        fb.name_value(fb.param(param_idx), "self".to_string());
        param_idx += 1;
    }
    if ctx.has_other {
        fb.name_value(fb.param(param_idx), "other".to_string());
        param_idx += 1;
    }
    for i in 0..effective_arg_count {
        // For declared args, use debug names from code_locals.
        // For implicit args (argumentN pattern), always use argumentN —
        // code_locals indices can collide with unrelated locals.
        let name = if i < ctx.arg_count {
            arg_name(ctx, i).unwrap_or_else(|| format!("argument{i}"))
        } else {
            format!("argument{i}")
        };
        fb.name_value(fb.param(param_idx), name);
        param_idx += 1;
    }
    // If this is a variadic script, record the rest param ValueId in a special
    // locals entry so the translation loop can reference it when it encounters
    // `argument_count` reads or dynamic `argument[N]` accesses.
    let rest_param_id = if scan.uses_dynamic_args {
        let id = fb.param(param_idx);
        fb.name_value(id, "args".to_string());
        Some(id)
    } else {
        None
    };

    let (block_map, block_params, block_entry_depths) =
        setup_blocks(&mut fb, &instructions, &with_ranges, 0);

    // Allocate locals.
    let mut locals = allocate_locals(&mut fb, ctx);
    // Stash the rest param (if any) in locals under the reserved key "_args" so
    // inner translation helpers can look it up without adding extra parameters.
    if let Some(rest_id) = rest_param_id {
        locals.insert("_args".to_string(), rest_id);
    }

    // Pass 3: Translate instructions.
    fb.switch_to_block(fb.entry_block());
    let mut extra_funcs = Vec::new();
    let terminated = run_translation_loop(
        &instructions,
        func_name,
        &mut fb,
        &block_map,
        &block_params,
        &block_entry_depths,
        &with_ranges,
        &mut locals,
        ctx,
        &mut extra_funcs,
    )?;

    // If the last block wasn't terminated, add a void return.
    if !terminated {
        fb.ret(None);
    }

    let mut func = fb.build();
    detect_switches(&mut func);
    Ok((func, extra_funcs))
}

// ---------------------------------------------------------------------------
// Switch detection
// ---------------------------------------------------------------------------

/// Detect BrIf chains that represent switch statements and rewrite them
/// as `Op::Switch`. GML bytecode compiles switch statements as a chain of
/// Dup+Cmp(Eq)+Bf, producing the following IR pattern:
///
/// ```text
/// block0(..., switch_val):
///   copy_v = Copy(switch_val)
///   case_const = Const(42)
///   cmp = Cmp(Eq, copy_v, case_const)
///   BrIf cmp, case_body[...], next_block[switch_val]
/// ```
///
/// Each block in the chain tests one case. The switch value is threaded
/// through block parameters. The chain ends with a `Br` to the default body.
fn detect_switches(func: &mut Function) {
    let num_blocks = func.blocks.len();
    let mut consumed = HashSet::new();

    for block_idx in 0..num_blocks {
        let block_id = BlockId::new(block_idx as u32);
        if consumed.contains(&block_id) {
            continue;
        }

        // Try to extract a switch chain starting at this block.
        if let Some(chain) = extract_switch_chain(func, block_id) {
            if chain.cases.len() < 2 {
                continue;
            }
            // Mark intermediate blocks as consumed.
            for &mid in &chain.intermediate_blocks {
                consumed.insert(mid);
            }
            // Rewrite the first block's terminator to Op::Switch.
            rewrite_to_switch(func, block_id, &chain);
        }
    }
}

/// A detected switch chain.
struct SwitchChain {
    /// The original switch value in the first block.
    switch_value: ValueId,
    /// Collected cases: (constant, target_block, target_args).
    cases: Vec<(Constant, BlockId, Vec<ValueId>)>,
    /// Default target and args (from the final Br).
    default: (BlockId, Vec<ValueId>),
    /// Intermediate comparison blocks (to be cleared).
    intermediate_blocks: Vec<BlockId>,
    /// Instruction IDs to remove from the first block (Copy, Const, Cmp).
    first_block_remove_insts: Vec<reincarnate_core::ir::inst::InstId>,
}

/// Try to extract a switch chain starting from `block_id`.
fn extract_switch_chain(func: &Function, block_id: BlockId) -> Option<SwitchChain> {
    let (switch_value, case_const, case_target, case_args, next_block, next_args, remove_insts) =
        match_switch_block(func, block_id, None)?;

    let mut cases = vec![(case_const, case_target, case_args)];
    let mut intermediate = Vec::new();
    let mut current = next_block;

    // The switch value is passed to the next block via args. Find which param
    // position it maps to.
    let param_idx = if next_args.len() == 1 {
        0
    } else {
        next_args.iter().position(|a| *a == switch_value)?
    };

    loop {
        // The switch value in the next block is its block parameter at param_idx.
        let next_block_data = &func.blocks[current];
        if param_idx >= next_block_data.params.len() {
            return None;
        }
        let next_switch_val = next_block_data.params[param_idx].value;

        if let Some((_, case_const, case_target, case_args, next, _, _)) =
            match_switch_block(func, current, Some(next_switch_val))
        {
            cases.push((case_const, case_target, case_args));
            intermediate.push(current);
            current = next;
        } else {
            // Check if this block is the default (just a Br).
            let default = match_default_block(func, current)?;
            intermediate.push(current);
            return Some(SwitchChain {
                switch_value,
                cases,
                default,
                intermediate_blocks: intermediate,
                first_block_remove_insts: remove_insts,
            });
        }
    }
}

/// A single case match result from a switch chain block.
type SwitchBlockMatch = (
    ValueId,                                   // switch_value
    Constant,                                  // case_constant
    BlockId,                                   // case_target
    Vec<ValueId>,                              // case_args
    BlockId,                                   // else_target
    Vec<ValueId>,                              // else_args
    Vec<reincarnate_core::ir::inst::InstId>,   // insts_to_remove
);

/// Match a single block in the switch chain.
fn match_switch_block(
    func: &Function,
    block_id: BlockId,
    expected_switch_val: Option<ValueId>,
) -> Option<SwitchBlockMatch> {
    let block = &func.blocks[block_id];
    if block.insts.is_empty() {
        return None;
    }

    // The terminator must be the last instruction and a BrIf.
    let term_id = *block.insts.last()?;
    let term = &func.insts[term_id];
    let (cond_val, then_target, then_args, else_target, else_args) = match &term.op {
        Op::BrIf {
            cond,
            then_target,
            then_args,
            else_target,
            else_args,
        } => (*cond, *then_target, then_args.clone(), *else_target, else_args.clone()),
        _ => return None,
    };

    // The condition must be a Cmp(Eq, lhs, rhs) where one operand is the
    // switch value (or a Copy of it) and the other is a Const.
    let cond_inst_id = find_def_inst(func, block_id, cond_val)?;
    let cond_inst = &func.insts[cond_inst_id];
    let (cmp_lhs, cmp_rhs) = match &cond_inst.op {
        Op::Cmp(CmpKind::Eq, lhs, rhs) => (*lhs, *rhs),
        _ => return None,
    };

    // One of lhs/rhs should be a Const, the other the switch value (possibly via Copy).
    let (switch_operand, case_const) = {
        let lhs_const = find_const(func, block_id, cmp_lhs);
        let rhs_const = find_const(func, block_id, cmp_rhs);
        match (lhs_const, rhs_const) {
            (None, Some(c)) => (cmp_lhs, c),
            (Some(c), None) => (cmp_rhs, c),
            _ => return None,
        }
    };

    // Resolve through Copy to find the actual switch value.
    let switch_val = resolve_through_copy(func, block_id, switch_operand);

    // If we have an expected switch value, verify it matches.
    if let Some(expected) = expected_switch_val {
        if switch_val != expected {
            return None;
        }
    }

    // Collect instruction IDs to remove from this block (Const, Copy, Cmp).
    let mut remove = Vec::new();
    // Only collect remove_insts for the first block; intermediate blocks
    // will be cleared entirely.
    if expected_switch_val.is_none() {
        if let Some(id) = find_def_inst(func, block_id, switch_operand) {
            if matches!(func.insts[id].op, Op::Copy(_)) {
                remove.push(id);
            }
        }
        // Find the Const instruction.
        let const_val = if find_const(func, block_id, cmp_lhs).is_some() {
            cmp_lhs
        } else {
            cmp_rhs
        };
        if let Some(id) = find_def_inst(func, block_id, const_val) {
            remove.push(id);
        }
        remove.push(cond_inst_id);
    }

    // GML Bf swaps then/else: then=fallthrough (next case), else=case body.
    // But in our IR, the BrIf condition is true → then_target.
    // With Cmp(Eq), true means "matched", so then_target is the case body
    // and else_target is the next comparison block.
    Some((
        switch_val,
        case_const,
        then_target,
        then_args,
        else_target,
        else_args,
        remove,
    ))
}

/// Match a default block (just a Br terminator, possibly with block-arg assigns).
fn match_default_block(
    func: &Function,
    block_id: BlockId,
) -> Option<(BlockId, Vec<ValueId>)> {
    let block = &func.blocks[block_id];
    // The block should have only a terminator (Br).
    let term_id = *block.insts.last()?;
    let term = &func.insts[term_id];
    match &term.op {
        Op::Br { target, args } => Some((*target, args.clone())),
        _ => None,
    }
}

/// Find the instruction in `block_id` that defines `value`.
fn find_def_inst(
    func: &Function,
    block_id: BlockId,
    value: ValueId,
) -> Option<reincarnate_core::ir::inst::InstId> {
    func.blocks[block_id]
        .insts
        .iter()
        .find(|&&inst_id| func.insts[inst_id].result == Some(value))
        .copied()
}

/// If `value` is defined by Op::Const in `block_id`, return the constant.
fn find_const(func: &Function, block_id: BlockId, value: ValueId) -> Option<Constant> {
    let inst_id = find_def_inst(func, block_id, value)?;
    match &func.insts[inst_id].op {
        Op::Const(c) => Some(c.clone()),
        _ => None,
    }
}

/// Resolve through Copy instructions: if `value` is defined by Copy(src),
/// return src; otherwise return value as-is.
fn resolve_through_copy(func: &Function, block_id: BlockId, value: ValueId) -> ValueId {
    if let Some(inst_id) = find_def_inst(func, block_id, value) {
        if let Op::Copy(src) = &func.insts[inst_id].op {
            return *src;
        }
    }
    value
}

/// Rewrite a block's terminator from BrIf to Op::Switch.
fn rewrite_to_switch(func: &mut Function, block_id: BlockId, chain: &SwitchChain) {
    // Remove the Copy, Const, and Cmp instructions from the first block.
    let remove_set: HashSet<_> = chain.first_block_remove_insts.iter().copied().collect();
    func.blocks[block_id]
        .insts
        .retain(|id| !remove_set.contains(id));

    // Replace the terminator (last inst, which is the BrIf) with Op::Switch.
    let term_id = *func.blocks[block_id].insts.last().unwrap();
    func.insts[term_id].op = Op::Switch {
        value: chain.switch_value,
        cases: chain.cases.clone(),
        default: chain.default.clone(),
    };

    // Clear intermediate blocks (they're now dead).
    for &mid in &chain.intermediate_blocks {
        func.blocks[mid].insts.clear();
        func.blocks[mid].params.clear();
    }
}

/// Filter instructions to only those reachable from the entry point.
///
/// In GMS2.3+ shared bytecode blobs, the decoded byte range may extend
/// past this function's terminal instruction into sibling functions' code.
/// This function walks the control flow from instruction 0, following
/// branches and fall-through, stopping at Ret/Exit. Only reachable
/// instructions are returned.
fn filter_reachable(instructions: &[Instruction]) -> Vec<Instruction> {
    let offset_to_idx: HashMap<usize, usize> = instructions
        .iter()
        .enumerate()
        .map(|(i, inst)| (inst.offset, i))
        .collect();

    let mut visited = vec![false; instructions.len()];
    let mut worklist = vec![0usize]; // start at instruction index 0

    while let Some(idx) = worklist.pop() {
        if idx >= instructions.len() || visited[idx] {
            continue;
        }
        visited[idx] = true;
        let inst = &instructions[idx];

        match inst.opcode {
            Opcode::B => {
                if let Operand::Branch(offset) = inst.operand {
                    let target = (inst.offset as i64 + offset as i64) as usize;
                    if let Some(&ti) = offset_to_idx.get(&target) {
                        worklist.push(ti);
                    }
                }
            }
            Opcode::Bt | Opcode::Bf | Opcode::PushEnv | Opcode::PopEnv => {
                if let Operand::Branch(offset) = inst.operand {
                    let target = (inst.offset as i64 + offset as i64) as usize;
                    if let Some(&ti) = offset_to_idx.get(&target) {
                        worklist.push(ti);
                    }
                }
                worklist.push(idx + 1);
            }
            Opcode::Ret | Opcode::Exit => {
                // Terminal — don't follow.
            }
            _ => {
                worklist.push(idx + 1);
            }
        }
    }

    instructions
        .iter()
        .enumerate()
        .filter(|(i, _)| visited[*i])
        .map(|(_, inst)| inst.clone())
        .collect()
}

/// Pass 1: Identify basic block start offsets.
fn find_block_starts(instructions: &[Instruction]) -> BTreeSet<usize> {
    let mut starts = BTreeSet::new();
    starts.insert(0);

    for (i, inst) in instructions.iter().enumerate() {
        match inst.opcode {
            Opcode::B | Opcode::Bt | Opcode::Bf | Opcode::PushEnv | Opcode::PopEnv => {
                if let Operand::Branch(offset) = inst.operand {
                    let target = (inst.offset as i64 + offset as i64) as usize;
                    starts.insert(target);
                }
                // Fall-through for conditional branches.
                if matches!(inst.opcode, Opcode::Bt | Opcode::Bf | Opcode::PushEnv | Opcode::PopEnv) {
                    if let Some(next) = instructions.get(i + 1) {
                        starts.insert(next.offset);
                    }
                }
                // Unconditional branch: next instruction is a block start too
                // (it might be a jump target from elsewhere).
                if inst.opcode == Opcode::B {
                    if let Some(next) = instructions.get(i + 1) {
                        starts.insert(next.offset);
                    }
                }
            }
            Opcode::Ret | Opcode::Exit => {
                // Don't create a block start after Ret/Exit. If the code
                // after is reachable (e.g., after a conditional early return),
                // some branch already targets it and that branch creates the
                // block start. Adding one here would cause spurious blocks
                // from trailing bytecode of sibling functions in GMS2.3+
                // shared bytecode blobs.
            }
            _ => {}
        }
    }

    starts
}

/// Build function signature from context.
fn build_signature(ctx: &TranslateCtx) -> FunctionSig {
    build_signature_with_args(ctx, ctx.arg_count)
}

fn build_signature_with_args(ctx: &TranslateCtx, arg_count: u16) -> FunctionSig {
    let mut params = Vec::new();
    let mut defaults = Vec::new();
    if ctx.has_self {
        let self_ty = ctx
            .class_name
            .map(|name| Type::Struct(name.to_string()))
            .unwrap_or(Type::Dynamic);
        params.push(self_ty);
        defaults.push(None);
    }
    if ctx.has_other {
        params.push(Type::Dynamic);
        defaults.push(None);
    }
    for _ in 0..arg_count {
        params.push(Type::Dynamic);
        // GML arguments are optional and default to 0 when not provided.
        defaults.push(Some(reincarnate_core::ir::value::Constant::Float(0.0)));
    }
    FunctionSig {
        params,
        defaults,
        return_ty: Type::Dynamic,
        ..Default::default()
    }
}

/// Parse `argumentN` variable names and return the index N, if any.
fn parse_argument_index(name: &str) -> Option<usize> {
    name.strip_prefix("argument").and_then(|s| s.parse::<usize>().ok())
}

/// Result of scanning for implicit argument references in a GML script.
struct ImplicitArgScan {
    /// Number of implicit arguments detected via `argumentN` or `argument[K]` with
    /// a constant index (max index + 1), or 0 if none found.
    count: u16,
    /// True when the script uses dynamic argument access: reads `argument_count`
    /// (to determine how many args were passed at runtime) or reads `argument[N]`
    /// with a non-constant index. Scripts with dynamic argument access must be
    /// emitted with a rest parameter (`...args: any[]`) so TypeScript call sites
    /// can pass any number of arguments without TS2554 errors.
    uses_dynamic_args: bool,
}

/// Scan instructions for implicit `argument0`..`argumentN` references
/// (variables with Builtin/Own instance type whose name matches `argumentN`)
/// and `argument[N]` references (Stacktop instance type with name "argument"
/// preceded by a constant integer push).
///
/// Also detects dynamic argument access patterns (`argument_count` reads or
/// `argument[N]` with a non-constant index) which require a rest parameter.
fn scan_implicit_args(instructions: &[Instruction], ctx: &TranslateCtx) -> ImplicitArgScan {
    let mut max_idx: Option<usize> = None;
    let mut uses_dynamic_args = false;
    for (i, inst) in instructions.iter().enumerate() {
        if let Operand::Variable { var_ref, instance } = &inst.operand {
            let it = InstanceType::from_i16(*instance);
            if matches!(it, Some(InstanceType::Own) | Some(InstanceType::Builtin)) {
                let name = resolve_variable_name(inst, ctx);
                if let Some(idx) = parse_argument_index(&name) {
                    max_idx = Some(max_idx.map_or(idx, |m: usize| m.max(idx)));
                } else if name == "argument_count" {
                    // Script reads how many arguments were passed — must be variadic.
                    uses_dynamic_args = true;
                }
            } else if matches!(it, Some(InstanceType::Stacktop)) {
                let name = resolve_variable_name(inst, ctx);
                if name == "argument" {
                    if let Some(idx) = preceding_const_int(instructions, i) {
                        // argument[N] pattern (GMS2): preceding instruction pushes the index.
                        let idx = idx as usize;
                        max_idx = Some(max_idx.map_or(idx, |m: usize| m.max(idx)));
                    } else {
                        // Dynamic index — script accesses argument[variable].
                        uses_dynamic_args = true;
                    }
                }
            } else if is_2d_array_access(var_ref, *instance) {
                let name = resolve_variable_name(inst, ctx);
                if name == "argument" {
                    // argument[N] pattern (GM:S): 2D array access, dim2 is the index.
                    // Pattern: pushi -1, pushi N, push/pop [obj].argument
                    // dim2 is 1 instruction back from this one.
                    if let Some(idx) = preceding_const_int(instructions, i) {
                        let idx = idx as usize;
                        max_idx = Some(max_idx.map_or(idx, |m: usize| m.max(idx)));
                    } else {
                        // Dynamic index — script accesses argument[variable].
                        uses_dynamic_args = true;
                    }
                }
            }
        }
    }
    ImplicitArgScan {
        count: max_idx.map_or(0, |m| (m + 1) as u16),
        uses_dynamic_args,
    }
}

/// Extract a constant integer from the instruction preceding `idx`, if any.
fn preceding_const_int(instructions: &[Instruction], idx: usize) -> Option<i64> {
    if idx == 0 {
        return None;
    }
    let prev = &instructions[idx - 1];
    match prev.operand {
        Operand::Int16(v) => Some(v as i64),
        Operand::Int32(v) => Some(v as i64),
        Operand::Int64(v) => Some(v),
        _ => None,
    }
}

/// Get a name for argument index `i`.
fn arg_name(ctx: &TranslateCtx, i: u16) -> Option<String> {
    ctx.local_names
        .iter()
        .find(|(idx, _)| *idx == i as u32)
        .map(|(_, name)| name.clone())
}

/// Allocate local variable slots in the entry block.
fn allocate_locals(
    fb: &mut FunctionBuilder,
    ctx: &TranslateCtx,
) -> HashMap<String, ValueId> {
    let mut locals = HashMap::new();
    for (_, name) in ctx.local_names {
        let slot = fb.alloc(Type::Dynamic);
        fb.name_value(slot, name.clone());
        locals.insert(name.clone(), slot);
    }
    locals
}

/// Build an empty function with just a void return.
fn build_empty_function(name: &str, ctx: &TranslateCtx) -> Result<Function, String> {
    let sig = build_signature(ctx);
    let mut fb = FunctionBuilder::new(name, sig, Visibility::Public);
    fb.ret(None);
    Ok(fb.build())
}

// ---------------------------------------------------------------------------
// With-block (PushEnv/PopEnv) helpers
// ---------------------------------------------------------------------------

/// Map each PushEnv instruction index to its corresponding PopEnv index.
///
/// Uses the PushEnv's branch operand to locate the matching PopEnv, handling
/// both GMS1 and GMS2.3+ bytecode conventions:
///
/// - **GMS1**: `PushEnv Branch(N)` where `offset + N = PopEnv.offset`.
/// - **GMS2.3+**: `PushEnv Branch(N)` where `offset + N = continuation.offset`
///   (instruction AFTER PopEnv). Since PopEnv is a 4-byte instruction,
///   PopEnv.offset = continuation - 4. We also try continuation - 8 as a
///   fallback for cases where the continuation is measured differently.
///
/// Stack-based nesting cannot be used because GML emits "early-exit" PopEnv
/// instructions (e.g. `return` inside a `with` body) that would be incorrectly
/// paired with an inner PushEnv, causing the wrong body slice to be extracted.
///
/// PushEnvs whose PopEnv lies in a sibling code entry (GMS2.3+ cross-code-entry
/// with-blocks) are left unmatched. The translate_instruction fallback handles
/// them by executing the body once for `self` only.
fn find_with_ranges(instructions: &[Instruction]) -> HashMap<usize, usize> {
    // Build offset → index map for this slice.
    let offset_to_idx: HashMap<usize, usize> = instructions
        .iter()
        .enumerate()
        .map(|(i, inst)| (inst.offset, i))
        .collect();

    let mut result = HashMap::new();

    for (i, inst) in instructions.iter().enumerate() {
        if inst.opcode != Opcode::PushEnv {
            continue;
        }
        let branch_offset = match inst.operand {
            Operand::Branch(off) => off,
            _ => continue,
        };
        // PushEnv Branch(N): the target is either:
        //   GMS1 style — target == PopEnv.offset  (branch jumps to the PopEnv)
        //   GMS2.3+ style — target == PopEnv.offset + sizeof(PopEnv)  (jumps to continuation)
        //
        // PopEnv is a 4-byte instruction, so sizeof(PopEnv) = 4.
        // So in GMS2.3+: PopEnv.offset = branch_target - 4.
        let branch_target = (inst.offset as i64 + branch_offset as i64) as usize;

        // Try GMS1: branch target IS the PopEnv.
        if let Some(&popenv_idx) = offset_to_idx.get(&branch_target) {
            if instructions[popenv_idx].opcode == Opcode::PopEnv {
                result.insert(i, popenv_idx);
                continue;
            }
        }

        // Try GMS2.3+: branch target is the continuation (PopEnv = target - 4).
        if branch_target >= 4 {
            let popenv_off = branch_target - 4;
            if let Some(&popenv_idx) = offset_to_idx.get(&popenv_off) {
                if instructions[popenv_idx].opcode == Opcode::PopEnv {
                    result.insert(i, popenv_idx);
                    continue;
                }
            }
        }

        // Also try target - 8 (some GMS2 versions may encode continuation differently).
        if branch_target >= 8 {
            let popenv_off = branch_target - 8;
            if let Some(&popenv_idx) = offset_to_idx.get(&popenv_off) {
                if instructions[popenv_idx].opcode == Opcode::PopEnv {
                    result.insert(i, popenv_idx);
                    continue;
                }
            }
        }

        // Neither heuristic found the PopEnv: the matching PopEnv is in a sibling
        // code entry (GMS2.3+ cross-code-entry with-block). The unmatched PushEnv
        // falls through to translate_instruction which handles it by discarding the
        // target and falling through to the body — semantically incomplete but valid.
    }

    result
}

/// Find the names of outer local variables accessed in a slice of instructions.
///
/// Used to determine which locals a with-body closure needs to capture.
fn scan_body_local_names(body_insts: &[Instruction], ctx: &TranslateCtx<'_>) -> Vec<String> {
    // Build a fast lookup set from the outer function's declared local names so we
    // only capture names that actually have an alloc slot (see `allocate_locals`).
    let known_locals: HashSet<&str> = ctx.local_names.iter().map(|(_, n)| n.as_str()).collect();
    let mut seen = HashSet::new();
    let mut names = Vec::new();
    for inst in body_insts {
        if let Operand::Variable { instance, .. } = &inst.operand {
            if matches!(InstanceType::from_i16(*instance), Some(InstanceType::Local)) {
                let name = resolve_variable_name(inst, ctx);
                // Skip names that don't correspond to a declared local — resolve_variable_name
                // returns "var_unknown_{offset}" when the VARI lookup fails, and these have no
                // alloc slot in the outer locals map.
                if known_locals.contains(name.as_str()) && seen.insert(name.clone()) {
                    names.push(name);
                }
            }
        }
    }
    names
}

/// Return true if any VARI instruction in `body_insts` uses `InstanceType::Other`.
/// Used to decide whether to capture the outer self for `other.field` access.
fn scan_body_uses_other(body_insts: &[Instruction], _ctx: &TranslateCtx<'_>) -> bool {
    body_insts.iter().any(|inst| {
        matches!(&inst.operand,
            Operand::Variable { instance, .. }
                if matches!(InstanceType::from_i16(*instance), Some(InstanceType::Other))
        )
    })
}

/// Find all `argument[N]` indices accessed in a with-body.
///
/// A with-body is compiled as a nested function; the outer function's arguments
/// are not visible as params inside it.  The caller uses this list to capture
/// each needed argument as an extra closure parameter (`_argument{N}`).
fn scan_body_argument_indices(body_insts: &[Instruction], ctx: &TranslateCtx<'_>) -> Vec<usize> {
    let mut seen: HashSet<usize> = HashSet::new();
    let mut indices: Vec<usize> = Vec::new();
    for (i, inst) in body_insts.iter().enumerate() {
        if let Operand::Variable { var_ref, instance } = &inst.operand {
            let instance_ty = InstanceType::from_i16(*instance);
            let found: Option<usize> = if matches!(instance_ty, Some(InstanceType::Arg)) {
                // InstanceType::Arg: variable_id is the argument index directly.
                Some(var_ref.variable_id as usize)
            } else if matches!(
                instance_ty,
                Some(InstanceType::Own) | Some(InstanceType::Builtin)
            ) {
                // Named form: argument0, argument1, ...
                parse_argument_index(&resolve_variable_name(inst, ctx))
            } else if var_ref.ref_type == 0 && *instance >= 0 {
                // 2D-array form: `argument[N]`.  dim1 (the argument index) is
                // the value on top of the stack just before this instruction,
                // i.e. pushed by the immediately preceding Push/PushI.
                let var_name = resolve_variable_name(inst, ctx);
                if var_name == "argument" {
                    i.checked_sub(1)
                        .and_then(|j| body_insts.get(j))
                        .and_then(|prev| match prev.operand {
                            Operand::Int16(v) if v >= 0 => Some(v as usize),
                            Operand::Int32(v) if v >= 0 => Some(v as usize),
                            Operand::Int64(v) if v >= 0 => Some(v as usize),
                            _ => None,
                        })
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(idx) = found {
                if seen.insert(idx) {
                    indices.push(idx);
                }
            }
        }
    }
    indices.sort_unstable();
    indices
}

/// Create IR blocks for a set of instructions, skipping with-body offsets.
///
/// Returns `(block_map, block_params, block_entry_depths)`.
///
/// - `entry_offset`: the bytecode offset that maps to the entry block (0 for
///   outer functions, the first body instruction's offset for inner closures).
/// - With-body offsets (inside any PushEnv/PopEnv range in `with_ranges`) are
///   excluded from block creation — they will be handled by separate closure
///   functions and must not appear as dead blocks in the outer function's CFG.
#[allow(clippy::type_complexity)]
fn setup_blocks(
    fb: &mut FunctionBuilder,
    instructions: &[Instruction],
    with_ranges: &HashMap<usize, usize>,
    entry_offset: usize,
) -> (
    HashMap<usize, BlockId>,
    HashMap<usize, Vec<ValueId>>,
    HashMap<usize, usize>,
) {
    let block_starts = find_block_starts(instructions);
    let block_entry_depths = compute_block_stack_depths(instructions, &block_starts);

    // Collect offsets that belong to with-body ranges (body + PopEnv).
    // Blocks at these offsets are owned by extracted closures, not the outer function.
    let body_offsets: HashSet<usize> = with_ranges
        .iter()
        .flat_map(|(&pi, &popi)| {
            instructions[pi + 1..=popi].iter().map(|i| i.offset)
        })
        .collect();

    let mut block_map: HashMap<usize, BlockId> = HashMap::new();
    let mut block_params: HashMap<usize, Vec<ValueId>> = HashMap::new();
    block_map.insert(entry_offset, fb.entry_block());
    for &off in &block_starts {
        if off != entry_offset && !body_offsets.contains(&off) {
            let block = fb.create_block();
            block_map.insert(off, block);
            let depth = block_entry_depths.get(&off).copied().unwrap_or(0);
            if depth > 0 {
                let types: Vec<Type> = vec![Type::Dynamic; depth];
                let params = fb.add_block_params(block, &types);
                block_params.insert(off, params);
            }
        }
    }
    (block_map, block_params, block_entry_depths)
}

/// Extract a `with`-block body as a standalone closure [`Function`].
///
/// The closure has signature `(_self: Dynamic, cap0: Dynamic, ...) -> Void`
/// where `_self` receives the iterated instance at call time and `cap0..` are
/// captured outer locals (ByValue).
#[allow(clippy::too_many_arguments)]
fn translate_with_body(
    body_insts: &[Instruction],
    inner_name: &str,
    ctx: &TranslateCtx<'_>,
    captured_names: &[String],
    has_outer_self: bool,
    extra_funcs: &mut Vec<Function>,
) -> Result<Function, String> {
    use reincarnate_core::ir::ty::FunctionSig;

    let sig = FunctionSig {
        params: vec![Type::Dynamic], // _self
        return_ty: Type::Void,
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new(inner_name, sig, Visibility::Public);
    fb.name_value(fb.param(0), "_self".to_string());

    // Declare capture parameters (ByValue snapshots of outer locals).
    let capture_ids = if captured_names.is_empty() {
        vec![]
    } else {
        fb.add_capture_params(
            captured_names
                .iter()
                .map(|n| (n.clone(), Type::Dynamic, CaptureMode::ByValue))
                .collect(),
        )
    };

    let inner_with_ranges = find_with_ranges(body_insts);
    let entry_offset = body_insts.first().map_or(0, |inst| inst.offset);
    let (block_map, block_params, block_entry_depths) =
        setup_blocks(&mut fb, body_insts, &inner_with_ranges, entry_offset);

    // Allocate outer local variable slots (reusing the outer ctx's local names).
    let mut locals = allocate_locals(&mut fb, ctx);

    // Allocate alloc slots for captured argument variables (_argument0, _argument1, …).
    // These are not GML locals so they aren't in ctx.local_names / allocate_locals.
    for name in captured_names {
        if name.starts_with("_argument") && !locals.contains_key(name) {
            let slot = fb.alloc(Type::Dynamic);
            locals.insert(name.clone(), slot);
        }
    }

    // Pre-store captured values into their alloc slots so the body can read them.
    for (i, name) in captured_names.iter().enumerate() {
        if let Some(&slot) = locals.get(name) {
            fb.store(slot, capture_ids[i]);
        }
    }

    // Inner context: same VARI/FUNC tables but no declared args, class-typed self.
    let inner_ctx = TranslateCtx {
        has_self: true,
        has_other: has_outer_self,
        arg_count: 0,
        class_name: None,
        function_names: ctx.function_names,
        asset_ref_names: ctx.asset_ref_names,
        variables: ctx.variables,
        func_ref_map: ctx.func_ref_map,
        vari_ref_map: ctx.vari_ref_map,
        bytecode_offset: ctx.bytecode_offset,
        local_names: ctx.local_names,
        string_table: ctx.string_table,
        obj_names: ctx.obj_names,
        self_object_index: ctx.self_object_index,
        ancestor_indices: ctx.ancestor_indices.clone(),
        script_names: ctx.script_names,
        // This IS a with-body closure — PopEnv inside is an early-exit signal,
        // not a loop-control instruction (the loop is managed by withInstances).
        is_with_body: true,
    };

    fb.switch_to_block(fb.entry_block());
    let terminated = run_translation_loop(
        body_insts,
        inner_name,
        &mut fb,
        &block_map,
        &block_params,
        &block_entry_depths,
        &inner_with_ranges,
        &mut locals,
        &inner_ctx,
        extra_funcs,
    )?;

    if !terminated {
        fb.ret(None);
    }

    let mut func = fb.build();
    func.method_kind = MethodKind::Closure;
    detect_switches(&mut func);
    Ok(func)
}

/// Core translation loop shared by [`translate_code_entry`] and [`translate_with_body`].
///
/// Handles the `skip_until` mechanism that skips over with-body instruction ranges
/// (they are extracted into separate closure functions instead of being translated
/// inline).  Returns `true` if the last block was terminated.
#[allow(clippy::too_many_arguments)]
fn run_translation_loop(
    instructions: &[Instruction],
    func_name: &str,
    fb: &mut FunctionBuilder,
    block_map: &HashMap<usize, BlockId>,
    block_params: &HashMap<usize, Vec<ValueId>>,
    block_entry_depths: &HashMap<usize, usize>,
    with_ranges: &HashMap<usize, usize>,
    locals: &mut HashMap<String, ValueId>,
    ctx: &TranslateCtx<'_>,
    extra_funcs: &mut Vec<Function>,
) -> Result<bool, String> {
    let mut stack: Vec<ValueId> = Vec::new();
    // Track GML type sizes (in 4-byte units) for each value on the stack.
    // Used by Dup to compute correct item count when items have different sizes
    // (e.g., Variable = 4 units vs Int16 = 1 unit).
    let mut gml_sizes: HashMap<ValueId, u8> = HashMap::new();
    let mut terminated = false;
    // Set to true when a 2D array VARI read leaves the original dim indices on
    // the stack (compound assignment Dup pattern). The subsequent Pop must use
    // reversed pop order: value on top, dim1 below, _dim2 at bottom.
    let mut compound_2d_pending = false;
    // Array reference captured by pushac (0xFFFC) for use by popaf (0xFFFD).
    // pushac pops the top-of-stack array reference and stores it here so that
    // popaf can use it without popping a third item from the stack.
    let mut pushac_array: Option<ValueId> = None;
    // When Some(n), skip instructions with index < n (with-body instructions
    // that have been extracted into a closure).
    let mut skip_until: Option<usize> = None;

    for (inst_idx, inst) in instructions.iter().enumerate() {
        // Skip instructions that belong to a with-body extracted as a closure.
        if let Some(skip) = skip_until {
            if inst_idx < skip {
                continue;
            }
            skip_until = None;
        }

        // Check if this instruction starts a new block.
        if inst_idx > 0 {
            if let Some(&block) = block_map.get(&inst.offset) {
                // Emit fall-through branch if previous block wasn't terminated.
                if !terminated {
                    let depth = block_entry_depths.get(&inst.offset).copied().unwrap_or(0);
                    let args = get_branch_args(&stack, depth);
                    fb.br(block, &args);
                }
                fb.switch_to_block(block);
                stack.clear();
                compound_2d_pending = false;
                pushac_array = None;
                if let Some(params) = block_params.get(&inst.offset) {
                    for &p in params {
                        // Block params are Variable-sized (16 bytes = 4 units).
                        gml_sizes.insert(p, 4);
                    }
                    stack.extend(params.iter().copied());
                }
                terminated = false;
            }
        }

        if terminated {
            continue;
        }

        // Special handling for PushEnv: extract the with-body as a closure.
        if inst.opcode == Opcode::PushEnv {
            if let Some(&popenv_idx) = with_ranges.get(&inst_idx) {
                let target_obj = pop(&mut stack, inst)?;
                let body_insts = &instructions[inst_idx + 1..popenv_idx];

                // Determine which outer locals the body needs to capture.
                let scanned_names = scan_body_local_names(body_insts, ctx);
                // If the outer context has a self and the body accesses `other`, capture
                // the outer self as _other (prepended so it becomes the first capture param).
                let has_outer_self =
                    ctx.has_self && scan_body_uses_other(body_insts, ctx);
                let mut captured_names: Vec<String> = Vec::new();
                let mut capture_vals: Vec<ValueId> = Vec::new();
                if has_outer_self {
                    captured_names.push("_other".to_string());
                    // Outer self is always param 0 (both for regular event handlers and
                    // nested with-bodies where param 0 is the current iterated _self).
                    capture_vals.push(fb.param(0));
                }
                // Capture any argument[N] variables the with-body reads from the outer
                // function.  The inner closure has no argument params of its own, so
                // each outer argument[N] must be passed in as a named capture.
                let outer_arg_offset = if ctx.has_self { 1 } else { 0 }
                    + if ctx.has_other { 1 } else { 0 };
                for n in scan_body_argument_indices(body_insts, ctx) {
                    let outer_idx = outer_arg_offset + n;
                    if outer_idx < fb.param_count() {
                        captured_names.push(format!("_argument{n}"));
                        capture_vals.push(fb.param(outer_idx));
                    }
                }
                for name in &scanned_names {
                    captured_names.push(name.clone());
                    let &slot = locals
                        .get(name)
                        .expect("captured local must have an alloc slot");
                    capture_vals.push(fb.load(slot, Type::Dynamic));
                }

                // Build the inner closure function (may recursively extract nested withs).
                let inner_name = format!("{func_name}_with_{:04x}", inst.offset);
                let inner_func = translate_with_body(
                    body_insts,
                    &inner_name,
                    ctx,
                    &captured_names,
                    has_outer_self,
                    extra_funcs,
                )?;
                extra_funcs.push(inner_func);

                // Emit: withInstances(target, closure)
                let closure_val =
                    fb.make_closure(&inner_name, &capture_vals, Type::Dynamic);
                fb.system_call(
                    "GameMaker.Instance",
                    "withInstances",
                    &[target_obj, closure_val],
                    Type::Void,
                );

                // Branch unconditionally to the post-with block.
                let post_with_idx = popenv_idx + 1;
                if post_with_idx < instructions.len() {
                    let post_with_off = instructions[post_with_idx].offset;
                    let fall_block =
                        block_map.get(&post_with_off).copied().ok_or_else(|| {
                            format!(
                                "{func_name}: no block at post-with offset {post_with_off:#x}"
                            )
                        })?;
                    let depth =
                        block_entry_depths.get(&post_with_off).copied().unwrap_or(0);
                    let args = get_branch_args(&stack, depth);
                    fb.br(fall_block, &args);
                } else {
                    fb.ret(None);
                }
                terminated = true;
                // Skip the body instructions and the PopEnv; resume after PopEnv.
                skip_until = Some(popenv_idx + 1);
                continue;
            }
        }

        translate_instruction(
            inst,
            instructions,
            inst_idx,
            fb,
            &mut stack,
            block_map,
            locals,
            ctx,
            &mut terminated,
            block_entry_depths,
            &mut gml_sizes,
            &mut compound_2d_pending,
            &mut pushac_array,
        )?;
    }

    Ok(terminated)
}

/// Resolve a branch target offset to (target_offset, BlockId).
fn resolve_branch_target(
    inst: &Instruction,
    offset: i32,
    block_map: &HashMap<usize, BlockId>,
) -> Result<(usize, BlockId), String> {
    let target = (inst.offset as i64 + offset as i64) as usize;
    let block = block_map.get(&target).copied().ok_or_else(|| {
        format!(
            "unresolved branch target at offset {:#x} → {:#x}",
            inst.offset, target
        )
    })?;
    Ok((target, block))
}

/// Resolve a variable reference to its name using the VARI linked-list reference map.
///
/// The variable operand word is at `inst.offset + 4` within the code entry's bytecode.
/// We compute the absolute file address and look it up in the pre-built reference map.
fn resolve_variable_name(inst: &Instruction, ctx: &TranslateCtx) -> String {
    // first_address points to the instruction word; lookup by instruction address.
    let abs_addr = ctx.bytecode_offset + inst.offset;
    if let Some(&vari_idx) = ctx.vari_ref_map.get(&abs_addr) {
        if vari_idx < ctx.variables.len() {
            return ctx.variables[vari_idx].0.clone();
        }
    }
    format!("var_unknown_{:x}", inst.offset)
}

/// Map GML DataType to IR Type.
fn datatype_to_ir_type(dt: DataType) -> Type {
    match dt {
        DataType::Double => Type::Float(64),
        DataType::Float => Type::Float(32),
        DataType::Int32 => Type::Int(32),
        DataType::Int64 => Type::Int(64),
        DataType::Bool => Type::Bool,
        DataType::String => Type::String,
        _ => Type::Dynamic,
    }
}

/// Map GML ComparisonKind to IR CmpKind.
fn comparison_to_cmp_kind(cmp: ComparisonKind) -> CmpKind {
    match cmp {
        ComparisonKind::Less => CmpKind::Lt,
        ComparisonKind::LessEqual => CmpKind::Le,
        ComparisonKind::Equal => CmpKind::Eq,
        ComparisonKind::NotEqual => CmpKind::Ne,
        ComparisonKind::GreaterEqual => CmpKind::Ge,
        ComparisonKind::Greater => CmpKind::Gt,
    }
}

/// Check if a variable operand represents a 2D array access.
///
/// In older GameMaker bytecode (pre-GMS2), array variable access uses
/// `ref_type == 0` with a non-negative instance type. Two index values
/// (2D indices) are popped from the stack before the variable is accessed.
/// This is distinct from `ref_type == 0xA0` (160) which uses singleton
/// instance field access without popping indices.
fn is_2d_array_access(var_ref: &VariableRef, instance: i16) -> bool {
    instance >= 0 && var_ref.ref_type == 0
}

/// Check if a variable operand uses stacktop-via-ref_type encoding.
///
/// In older GameMaker bytecode (pre-GMS2), `ref_type == 0x80` with a
/// non-negative instance type indicates that the target instance is on the
/// operand stack, similar to the GMS2 Stacktop instance type (-9). The
/// `instance` field provides a type hint (which object type to expect) but
/// the actual instance ID is popped from the stack at runtime.
fn is_stacktop_ref(var_ref: &VariableRef, instance: i16) -> bool {
    instance >= 0 && var_ref.ref_type == 0x80
}

/// Return the GML stack slot size of a DataType in 4-byte units.
///
/// The GML VM stack uses variable-width slots:
///   - Int16, Int32, Boolean, String: 4 bytes (1 unit)
///   - Double, Int64: 8 bytes (2 units)
///   - Variable (RValue): 16 bytes (4 units)
///
/// This matters for `Dup(N)` which duplicates `(N+1) * sizeof(type1)` bytes,
/// not `N+1` items.
fn gml_slot_units(dt: DataType) -> u8 {
    match dt {
        DataType::Variable => 4,
        DataType::Double | DataType::Int64 => 2,
        _ => 1, // Int16, Int32, Boolean, String
    }
}

/// Compute the stack effect (pops, pushes) of an instruction.
fn stack_effect(inst: &Instruction) -> (usize, usize) {
    match inst.opcode {
        Opcode::PushI | Opcode::Push | Opcode::PushLoc | Opcode::PushGlb | Opcode::PushBltn => {
            if let Operand::Variable { var_ref, instance } = &inst.operand {
                if matches!(InstanceType::from_i16(*instance), Some(InstanceType::Stacktop))
                    || is_stacktop_ref(var_ref, *instance)
                {
                    (1, 1) // pops instance from stack, pushes field value
                } else if is_2d_array_access(var_ref, *instance) {
                    (2, 1) // pops 2D indices, pushes value
                } else {
                    (0, 1)
                }
            } else {
                (0, 1)
            }
        }
        Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div
        | Opcode::Rem | Opcode::Mod => (2, 1),
        Opcode::Neg | Opcode::Not => (1, 1),
        Opcode::And | Opcode::Or | Opcode::Xor | Opcode::Shl | Opcode::Shr => (2, 1),
        Opcode::Cmp => (2, 1),
        Opcode::Conv => (1, 1),
        Opcode::Dup => {
            // Dup(N): high byte is DupExtra (GMS2.3+ extended flag), low byte is dup_size.
            // DupExtra != 0 → GMS2.3+ extended encoding (no-op for our IR, no net stack change).
            // DupExtra == 0 → normal dup; approximated as pushing dup_size+1 items.
            if let Operand::Dup(n) = inst.operand {
                let dup_extra = (n >> 8) & 0xFF;
                let dup_size = n & 0xFF;
                if dup_extra != 0 {
                    (0, 0) // swap or no-op: no net item change
                } else {
                    (0, dup_size as usize + 1)
                }
            } else {
                (0, 1)
            }
        }
        Opcode::Popz => (1, 0),
        Opcode::Pop => {
            if let Operand::Variable { var_ref, instance } = &inst.operand {
                if matches!(InstanceType::from_i16(*instance), Some(InstanceType::Stacktop))
                    || is_stacktop_ref(var_ref, *instance)
                {
                    (2, 0) // pops value + instance from stack
                } else if is_2d_array_access(var_ref, *instance) {
                    (3, 0) // pops value + 2D indices
                } else {
                    (1, 0)
                }
            } else {
                (1, 0)
            }
        }
        Opcode::Call => {
            if let Operand::Call { argc, .. } = inst.operand {
                (argc as usize, 1)
            } else {
                (0, 1)
            }
        }
        Opcode::CallV => {
            // CallV pops: function ref + instance + argc args
            if let Operand::Call { argc, .. } = inst.operand {
                (argc as usize + 2, 1)
            } else {
                (2, 1)
            }
        }
        Opcode::Ret => (1, 0),
        Opcode::Exit => (0, 0),
        Opcode::B => (0, 0),
        Opcode::Bt | Opcode::Bf => (1, 0),
        Opcode::PushEnv => (1, 0),
        Opcode::PopEnv => (0, 0),
        Opcode::Break => {
            if let Operand::Break { signal, .. } = inst.operand {
                match signal {
                    0xFFFF => (0, 0),                     // chkindex
                    0xFFFC => (1, 0),                     // pushac — captures array ref (pops 1)
                    0xFFFB => (1, 0),                     // setowner — pops owner ID
                    0xFFFE => (2, 1),                     // pushaf
                    0xFFFD => (2, 0),                     // popaf — pops value + index (array from pushac)
                    0xFFF6 => (0, 1),                     // chknullish — pushes boolean
                    0xFFF5 => (0, 1),                     // pushref — pushes function ref
                    0xFFFA => (0, 1),                     // isstaticok — pushes boolean
                    0xFFF9 => (0, 0),                     // setstatic — nop
                    0xFFF8 | 0xFFF7 => (0, 0),            // savearef, restorearef — nop
                    _ => (0, 0),
                }
            } else {
                (0, 0)
            }
        }
    }
}

/// Pre-compute the operand stack depth at each block entry point.
fn compute_block_stack_depths(
    instructions: &[Instruction],
    block_starts: &BTreeSet<usize>,
) -> HashMap<usize, usize> {
    let mut depths: HashMap<usize, usize> = HashMap::new();
    depths.insert(0, 0);

    let mut depth: i32 = 0;
    let mut terminated = false;

    for (i, inst) in instructions.iter().enumerate() {
        if block_starts.contains(&inst.offset) && i > 0 {
            if !terminated {
                depths.entry(inst.offset).or_insert(depth as usize);
            }
            if let Some(&d) = depths.get(&inst.offset) {
                depth = d as i32;
                terminated = false;
            } else {
                // Unreachable block (no incoming edge recorded a depth).
                // Don't process instructions or propagate depths from here.
                depth = 0;
                terminated = true;
            }
        }

        if terminated {
            continue;
        }

        let (pops, pushes) = stack_effect(inst);
        depth -= pops as i32;
        if depth < 0 {
            depth = 0;
        }

        match inst.opcode {
            Opcode::B => {
                if let Operand::Branch(offset) = inst.operand {
                    let target = (inst.offset as i64 + offset as i64) as usize;
                    depths.entry(target).or_insert(depth as usize);
                }
                terminated = true;
            }
            Opcode::Bt | Opcode::Bf => {
                if let Operand::Branch(offset) = inst.operand {
                    let target = (inst.offset as i64 + offset as i64) as usize;
                    depths.entry(target).or_insert(depth as usize);
                    if let Some(next) = instructions.get(i + 1) {
                        depths.entry(next.offset).or_insert(depth as usize);
                    }
                }
                terminated = true;
            }
            Opcode::PushEnv => {
                if let Operand::Branch(offset) = inst.operand {
                    let target = (inst.offset as i64 + offset as i64) as usize;
                    depths.entry(target).or_insert(depth as usize);
                    if let Some(next) = instructions.get(i + 1) {
                        depths.entry(next.offset).or_insert(depth as usize);
                    }
                }
                terminated = true;
            }
            Opcode::PopEnv => {
                if let Operand::Branch(offset) = inst.operand {
                    let target = (inst.offset as i64 + offset as i64) as usize;
                    depths.entry(target).or_insert(depth as usize);
                    if let Some(next) = instructions.get(i + 1) {
                        depths.entry(next.offset).or_insert(depth as usize);
                    }
                }
                terminated = true;
            }
            Opcode::Ret | Opcode::Exit => {
                terminated = true;
            }
            _ => {}
        }

        depth += pushes as i32;
    }

    depths
}

/// Build branch arguments from the current stack based on target block's entry depth.
fn get_branch_args(stack: &[ValueId], target_depth: usize) -> Vec<ValueId> {
    stack.iter().take(target_depth).copied().collect()
}

/// Translate a single instruction.
#[allow(clippy::too_many_arguments)]
fn translate_instruction(
    inst: &Instruction,
    instructions: &[Instruction],
    inst_idx: usize,
    fb: &mut FunctionBuilder,
    stack: &mut Vec<ValueId>,
    block_map: &HashMap<usize, BlockId>,
    locals: &mut HashMap<String, ValueId>,
    ctx: &TranslateCtx,
    terminated: &mut bool,
    block_entry_depths: &HashMap<usize, usize>,
    gml_sizes: &mut HashMap<ValueId, u8>,
    compound_2d_pending: &mut bool,
    pushac_array: &mut Option<ValueId>,
) -> Result<(), String> {
    match inst.opcode {
        // ============================================================
        // Constants
        // ============================================================
        Opcode::PushI | Opcode::Push | Opcode::PushLoc | Opcode::PushGlb | Opcode::PushBltn => {
            let depth_before = stack.len();
            translate_push(inst, fb, stack, locals, ctx, compound_2d_pending)?;
            // Annotate newly pushed value with its GML type size.
            if stack.len() > depth_before {
                if let Some(&val) = stack.last() {
                    let units = match &inst.operand {
                        Operand::Variable { .. } => 4, // Variable reads → RValue (16 bytes)
                        _ => gml_slot_units(inst.type1),
                    };
                    gml_sizes.insert(val, units);
                }
            }
        }

        // ============================================================
        // Arithmetic (binary)
        // ============================================================
        Opcode::Add => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.add(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Sub => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.sub(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Mul => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.mul(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Div => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.div(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Rem | Opcode::Mod => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.rem(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }

        // ============================================================
        // Unary
        // ============================================================
        Opcode::Neg => {
            let a = pop(stack, inst)?;
            let r = fb.neg(a);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Not => {
            let a = pop(stack, inst)?;
            let r = fb.not(a);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }

        // ============================================================
        // Bitwise
        // ============================================================
        Opcode::And => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.bit_and(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Or => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.bit_or(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Xor => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.bit_xor(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Shl => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.shl(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }
        Opcode::Shr => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            let r = fb.shr(a, b);
            gml_sizes.insert(r, gml_slot_units(inst.type1));
            stack.push(r);
        }

        // ============================================================
        // Comparison
        // ============================================================
        Opcode::Cmp => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            if let Operand::Comparison(kind) = inst.operand {
                let cmp_kind = comparison_to_cmp_kind(kind);
                let r = fb.cmp(cmp_kind, a, b);
                gml_sizes.insert(r, 1); // Boolean = 4 bytes = 1 unit
                stack.push(r);
            } else {
                return Err(format!(
                    "{:#x}: Cmp without comparison operand",
                    inst.offset
                ));
            }
        }

        // ============================================================
        // Control flow
        // ============================================================
        Opcode::B => {
            if let Operand::Branch(offset) = inst.operand {
                let (target_off, target) = resolve_branch_target(inst, offset, block_map)?;
                let depth = block_entry_depths.get(&target_off).copied().unwrap_or(0);
                let args = get_branch_args(stack, depth);
                fb.br(target, &args);
                *terminated = true;
            }
        }
        Opcode::Bt => {
            if let Operand::Branch(offset) = inst.operand {
                let cond = pop(stack, inst)?;
                let branch_target = resolve_branch_target(inst, offset, block_map).ok();
                let fall_target = resolve_fallthrough(instructions, inst_idx, block_map).ok();
                match (branch_target, fall_target) {
                    (Some((then_off, then_blk)), Some((else_off, else_blk))) => {
                        let then_args = get_branch_args(stack, block_entry_depths.get(&then_off).copied().unwrap_or(0));
                        let else_args = get_branch_args(stack, block_entry_depths.get(&else_off).copied().unwrap_or(0));
                        fb.br_if(cond, then_blk, &then_args, else_blk, &else_args);
                    }
                    (Some((off, blk)), None) => {
                        // Fall-through past end → branch or implicit return.
                        let ret_blk = fb.create_block();
                        fb.br_if(cond, blk, &get_branch_args(stack, block_entry_depths.get(&off).copied().unwrap_or(0)), ret_blk, &[]);
                        fb.switch_to_block(ret_blk);
                        fb.ret(None);
                    }
                    (None, Some((off, blk))) => {
                        // Branch target past end → fall-through or implicit return.
                        let ret_blk = fb.create_block();
                        fb.br_if(cond, ret_blk, &[], blk, &get_branch_args(stack, block_entry_depths.get(&off).copied().unwrap_or(0)));
                        fb.switch_to_block(ret_blk);
                        fb.ret(None);
                    }
                    (None, None) => {
                        // Both targets past end → pop condition and implicit return.
                        fb.ret(None);
                    }
                }
                *terminated = true;
            }
        }
        Opcode::Bf => {
            if let Operand::Branch(offset) = inst.operand {
                let cond = pop(stack, inst)?;
                let branch_target = resolve_branch_target(inst, offset, block_map).ok();
                let fall_target = resolve_fallthrough(instructions, inst_idx, block_map).ok();
                // Bf branches when false, so: then=fallthrough, else=branch
                match (fall_target, branch_target) {
                    (Some((then_off, then_blk)), Some((else_off, else_blk))) => {
                        let then_args = get_branch_args(stack, block_entry_depths.get(&then_off).copied().unwrap_or(0));
                        let else_args = get_branch_args(stack, block_entry_depths.get(&else_off).copied().unwrap_or(0));
                        fb.br_if(cond, then_blk, &then_args, else_blk, &else_args);
                    }
                    (Some((off, blk)), None) => {
                        let ret_blk = fb.create_block();
                        fb.br_if(cond, blk, &get_branch_args(stack, block_entry_depths.get(&off).copied().unwrap_or(0)), ret_blk, &[]);
                        fb.switch_to_block(ret_blk);
                        fb.ret(None);
                    }
                    (None, Some((off, blk))) => {
                        let ret_blk = fb.create_block();
                        fb.br_if(cond, ret_blk, &[], blk, &get_branch_args(stack, block_entry_depths.get(&off).copied().unwrap_or(0)));
                        fb.switch_to_block(ret_blk);
                        fb.ret(None);
                    }
                    (None, None) => {
                        fb.ret(None);
                    }
                }
                *terminated = true;
            }
        }

        // ============================================================
        // Return / Exit
        // ============================================================
        Opcode::Ret => {
            let val = pop(stack, inst)?;
            fb.ret(Some(val));
            *terminated = true;
        }
        Opcode::Exit => {
            fb.ret(None);
            *terminated = true;
        }

        // ============================================================
        // Stack management
        // ============================================================
        Opcode::Popz => {
            let _ = pop(stack, inst)?;
        }
        Opcode::Dup => {
            if let Operand::Dup(n) = inst.operand {
                let dup_extra = (n >> 8) & 0xFF;
                let dup_size = (n & 0xFF) as usize;
                if dup_extra != 0 {
                    // GMS2.3+ extended Dup encoding (DupExtra != 0).  Used in two cases:
                    //
                    // 1. Struct swap marker (type=Variable, dup_size=0): pure no-op in the VM.
                    //
                    // 2. Byte-reorder swap (dup_size > 0): the GML VM uses this before popaf
                    //    in compound array writes (e.g. `arr[i] -= 2`).  The pattern is:
                    //      Dup(normal)  → copies arr+i to top; originals stay below
                    //      pushaf       → pops copies, pushes arr[i]; originals still below
                    //      <arithmetic> → pushes new_value on top
                    //      Dup(swap)    → ← here; stack is [..., arr, i, new_value]
                    //      popaf        → needs (value=top, index=next, array=below)
                    //    After the arithmetic, our ValueId stack is already in the right
                    //    order for popaf.  The GML VM needs the swap because Variable is
                    //    16 bytes while the computed result may be 4 bytes, so the VM's
                    //    fixed-size popaf would read the wrong bytes without reordering.
                    //    Our stack holds one ValueId per logical value regardless of byte
                    //    size, so the byte-alignment problem doesn't exist here.  No-op.
                } else {
                    // Normal dup: duplicate (dup_size + 1) * type_unit units from stack top.
                    let type_unit = gml_slot_units(inst.type1) as usize;
                    let total_units = (dup_size + 1) * type_unit;

                    // Count backwards from stack top to find how many items
                    // correspond to total_units.
                    let mut units_remaining = total_units;
                    let mut item_count = 0;
                    for &v in stack.iter().rev() {
                        if units_remaining == 0 {
                            break;
                        }
                        let item_units = gml_sizes.get(&v).copied().unwrap_or(1) as usize;
                        if item_units > units_remaining {
                            // Item is larger than remaining units — this shouldn't
                            // happen with well-formed bytecode. Include it anyway.
                            item_count += 1;
                            break;
                        }
                        units_remaining -= item_units;
                        item_count += 1;
                    }

                    if stack.len() < item_count {
                        return Err(format!(
                            "{:#x}: Dup({}) on stack of depth {} (need {} items for {} units)",
                            inst.offset, dup_size, stack.len(), item_count, total_units
                        ));
                    }
                    let start = stack.len() - item_count;
                    let to_dup: Vec<ValueId> = stack[start..].to_vec();
                    for &v in &to_dup {
                        let copied = fb.copy(v);
                        gml_sizes.insert(copied, gml_sizes.get(&v).copied().unwrap_or(1));
                        stack.push(copied);
                    }
                }
            } else {
                if stack.is_empty() {
                    return Err(format!("{:#x}: Dup(0) on stack of depth 0", inst.offset));
                }
                let v = *stack.last().unwrap();
                let copied = fb.copy(v);
                gml_sizes.insert(copied, gml_sizes.get(&v).copied().unwrap_or(1));
                stack.push(copied);
            }
        }

        // ============================================================
        // Pop (variable store)
        // ============================================================
        Opcode::Pop => {
            translate_pop(inst, fb, stack, locals, ctx, compound_2d_pending)?;
        }

        // ============================================================
        // Function calls
        // ============================================================
        Opcode::Call => {
            if let Operand::Call { function_id, argc } = inst.operand {
                // first_address points to the Call instruction word.
                let abs_addr = ctx.bytecode_offset + inst.offset;
                let func_name = ctx.func_ref_map.get(&abs_addr)
                    .and_then(|&idx| ctx.function_names.get(&(idx as u32)))
                    .cloned()
                    .unwrap_or_else(|| format!("func_unknown_{function_id}"));

                // GMS2.3+ internal built-in functions — resolve to IR values directly.
                // @@This@@ returns the calling instance (self). Replacing it with the
                // self parameter avoids emitting a `this` expression in free functions
                // (which have no implicit `this` binding).
                if func_name == "@@This@@" && argc == 0 {
                    let val = if ctx.has_self { fb.param(0) } else { fb.const_null() };
                    gml_sizes.insert(val, 4);
                    stack.push(val);
                    return Ok(());
                }

                let mut args = Vec::with_capacity(argc as usize + 1);
                for _ in 0..argc {
                    args.push(pop(stack, inst)?);
                }
                // Scripts receive the caller's instance as an implicit first arg.
                if ctx.script_names.contains(&func_name) {
                    let self_val = if ctx.has_self {
                        fb.param(0)
                    } else {
                        fb.const_null()
                    };
                    args.insert(0, self_val);
                }
                // instance_destroy() with no explicit target destroys the calling instance.
                // Inject self as the first arg so the runtime knows which instance to remove.
                if func_name == "instance_destroy" && argc == 0 && ctx.has_self {
                    args.push(fb.param(0));
                }
                let result = fb.call(&func_name, &args, Type::Dynamic);
                gml_sizes.insert(result, 4); // Call returns Variable (16 bytes)
                stack.push(result);
            }
        }
        Opcode::CallV => {
            if let Operand::Call { argc, .. } = inst.operand {
                let callee = pop(stack, inst)?;
                // CallV also pops the instance/receiver below the function ref.
                let _instance = pop(stack, inst)?;
                let mut args = Vec::with_capacity(argc as usize);
                for _ in 0..argc {
                    args.push(pop(stack, inst)?);
                }
                let result = fb.call_indirect(callee, &args, Type::Dynamic);
                gml_sizes.insert(result, 4); // CallV returns Variable (16 bytes)
                stack.push(result);
            }
        }

        // ============================================================
        // Type conversion
        // ============================================================
        Opcode::Conv => {
            let val = pop(stack, inst)?;
            let target_ty = datatype_to_ir_type(inst.type2);
            let coerced = fb.coerce(val, target_ty);
            gml_sizes.insert(coerced, gml_slot_units(inst.type2));
            stack.push(coerced);
        }

        // ============================================================
        // PushEnv / PopEnv (with-blocks)
        // ============================================================
        Opcode::PushEnv => {
            if let Operand::Branch(_offset) = inst.operand {
                // Unmatched PushEnv: the matching PopEnv is in a sibling code entry
                // (GMS2.3+ cross-code-entry with-block).  We can't extract the full
                // body as a closure here.  Pop the target object (discarded) and fall
                // through to the body block — this executes the body for `self` only,
                // which is semantically incomplete but produces valid TypeScript.
                let _target_obj = pop(stack, inst)?;
                let (body_off, body_block) = resolve_fallthrough(instructions, inst_idx, block_map)?;
                let args = get_branch_args(stack, block_entry_depths.get(&body_off).copied().unwrap_or(0));
                fb.br(body_block, &args);
                *terminated = true;
            }
        }
        Opcode::PopEnv => {
            if let Operand::Branch(_) = inst.operand {
                if ctx.is_with_body {
                    // Inside a with-body closure, PopEnv is an early-exit signal
                    // (e.g., `return` or `break` inside a `with` body). The outer
                    // iteration is managed by withInstances — just return from the
                    // closure. Any subsequent RET will be in a dead block (DCE cleans
                    // it up).
                    fb.ret(None);
                } else {
                    // Unmatched PopEnv (sibling of an unmatched PushEnv in the same
                    // function, or the loop-back PopEnv of a GMS2.3+ cross-code-entry
                    // with-block).  Fall through to the continuation block.
                    let (fall_off, fall) = resolve_fallthrough(instructions, inst_idx, block_map)?;
                    let args =
                        get_branch_args(stack, block_entry_depths.get(&fall_off).copied().unwrap_or(0));
                    fb.br(fall, &args);
                }
                *terminated = true;
            }
        }

        // ============================================================
        // Break (special signals)
        // ============================================================
        Opcode::Break => {
            if let Operand::Break { signal, .. } = inst.operand {
                match signal {
                    0xFFFF => {} // chkindex — nop for decompilation
                    0xFFFE => {
                        // pushaf — array get
                        let index = pop(stack, inst)?;
                        let array = pop(stack, inst)?;
                        let val = fb.get_index(array, index, Type::Dynamic);
                        gml_sizes.insert(val, 4); // Variable (16 bytes)
                        stack.push(val);
                    }
                    0xFFFD => {
                        // popaf — array set.
                        // The array reference was captured by the preceding pushac.
                        // Pop value (top) and index; use the pushac-stored array.
                        let value = pop(stack, inst)?;
                        let index = pop(stack, inst)?;
                        let array = pushac_array.take().unwrap_or_else(|| {
                            pop(stack, inst).unwrap_or_else(|_| fb.const_int(-6))
                        });
                        fb.set_index(array, index, value);
                    }
                    0xFFFC => {
                        // pushac — capture the array reference for the upcoming popaf.
                        // GMS2.3+ uses this to anchor the array variable before the
                        // value and index are pushed onto the stack.  We pop the
                        // reference off the stack and stash it in pushac_array so
                        // that popaf can retrieve it without over-popping.
                        let arr = pop(stack, inst)?;
                        *pushac_array = Some(arr);
                    }
                    0xFFFB => {
                        // setowner — pops the owner instance ID from the stack.
                        let owner = pop(stack, inst)?;
                        let _ = owner;
                    }
                    0xFFFA => {
                        // isstaticok — static init guard. Pushes true if statics
                        // are already initialized; used with Bt to skip init code.
                        // For decompilation we push false so the init code is emitted.
                        let r = fb.const_bool(false);
                        gml_sizes.insert(r, 1); // Boolean (4 bytes)
                        stack.push(r);
                    }
                    0xFFF9 => {} // setstatic — set static scope, nop for decompilation
                    0xFFF8 => {} // savearef — save array ref to temp, nop for decompilation
                    0xFFF7 => {} // restorearef — restore array ref from temp, nop for decompilation
                    0xFFF6 => {
                        // chknullish — check if top of stack is nullish (undefined).
                        // Pushes boolean; original value stays on stack below.
                        // Used for ?? (nullish coalescing) and ?. (optional chaining).
                        let val = *stack.last().ok_or_else(|| {
                            format!("{:#x}: stack underflow on chknullish", inst.offset)
                        })?;
                        let null_val = fb.const_null();
                        let is_null = fb.cmp(CmpKind::Eq, val, null_val);
                        gml_sizes.insert(is_null, 1); // Boolean (4 bytes)
                        stack.push(is_null);
                    }
                    0xFFF5 => {
                        // pushref — push function reference onto stack.
                        // The extra Int32 operand is a direct FUNC table index (not
                        // a linked-list offset like Call operands). Look up the name
                        // in function_names using the extra value as the index.
                        // Fall back to func_ref_map (for GMS1 compatibility) and
                        // then to a placeholder.
                        let func_name = if let Operand::Break { extra: Some(idx), .. } = inst.operand {
                            let key = idx as u32;
                            let type_tag = key >> 24;
                            if type_tag == 0 {
                                // Type 0: direct FUNC index.
                                ctx.function_names.get(&key).cloned()
                            } else {
                                // Non-zero type tag: look up named asset (sprite, sound, font, etc.).
                                ctx.asset_ref_names.get(&key).cloned()
                            }
                        } else {
                            None
                        }
                        .or_else(|| {
                            let abs_addr = ctx.bytecode_offset + inst.offset;
                            ctx.func_ref_map.get(&abs_addr)
                                .and_then(|&i| ctx.function_names.get(&(i as u32)))
                                .cloned()
                        })
                        .unwrap_or_else(|| {
                            let abs_addr = ctx.bytecode_offset + inst.offset;
                            format!("func_ref_unknown_{:#x}", abs_addr)
                        });
                        let val = fb.global_ref(&func_name, Type::Dynamic);
                        gml_sizes.insert(val, 4); // Variable (16 bytes)
                        stack.push(val);
                    }
                    _ => {
                        // Unknown break signal, emit as system call.
                        let sig_val = fb.const_int(signal as i64);
                        fb.system_call(
                            "GameMaker.Debug",
                            "break",
                            &[sig_val],
                            Type::Void,
                        );
                    }
                }
            }
        }
    }

    Ok(())
}

/// Translate a push instruction.
fn translate_push(
    inst: &Instruction,
    fb: &mut FunctionBuilder,
    stack: &mut Vec<ValueId>,
    locals: &mut HashMap<String, ValueId>,
    ctx: &TranslateCtx,
    compound_2d_pending: &mut bool,
) -> Result<(), String> {
    match &inst.operand {
        Operand::Int16(v) => stack.push(fb.const_int(*v as i64)),
        Operand::Int32(v) => stack.push(fb.const_int(*v as i64)),
        Operand::Int64(v) => stack.push(fb.const_int(*v)),
        Operand::Double(v) => stack.push(fb.const_float(*v)),
        Operand::Float(v) => stack.push(fb.const_float(*v as f64)),
        Operand::Bool(v) => stack.push(fb.const_bool(*v)),
        Operand::StringIndex(idx) => {
            let s = ctx.string_table.get(*idx as usize).ok_or_else(|| {
                format!("string index {} out of range (table size={})", idx, ctx.string_table.len())
            })?;
            stack.push(fb.const_string(s));
        }
        Operand::Variable { var_ref, instance } => {
            translate_push_variable(inst, fb, stack, locals, ctx, var_ref, *instance, compound_2d_pending)?;
        }
        _ => {
            return Err(format!(
                "{:#x}: unexpected Push operand {:?}",
                inst.offset, inst.operand
            ));
        }
    }
    Ok(())
}

/// Translate a Push with Variable operand (load from variable).
#[allow(clippy::too_many_arguments)]
fn translate_push_variable(
    inst: &Instruction,
    fb: &mut FunctionBuilder,
    stack: &mut Vec<ValueId>,
    locals: &mut HashMap<String, ValueId>,
    ctx: &TranslateCtx,
    var_ref: &VariableRef,
    instance: i16,
    compound_2d_pending: &mut bool,
) -> Result<(), String> {
    let var_name = resolve_variable_name(inst, ctx);

    // Handle stacktop-via-ref_type (ref_type == 0x80 with instance >= 0).
    // The target instance is on the stack (pushed before this instruction).
    // In GMS2.3+, Push Int32(-9) followed by a stacktop variable access is
    // the "self" pattern — resolve to the self parameter directly.
    if is_stacktop_ref(var_ref, instance) {
        let raw_target = pop(stack, inst)?;
        let target = if ctx.has_self {
            if matches!(fb.try_resolve_const(raw_target), Some(Constant::Int(-9))) {
                fb.param(0)
            } else {
                raw_target
            }
        } else {
            raw_target
        };
        if ctx.has_self && target == fb.param(0) {
            let val = fb.get_field(target, &var_name, Type::Dynamic);
            stack.push(val);
        } else if let Some(Constant::Int(obj_idx)) = fb.try_resolve_const(target) {
            // Constant integer target = object index pushed before stacktop access.
            // Resolve to getOn(objName, field) for clean class-based access.

            if obj_idx >= 0 {
                let obj_id = if let Some(name) = ctx.obj_names.get(obj_idx as usize) {
                    fb.const_string(name)
                } else {
                    fb.const_int(obj_idx)
                };
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Instance",
                    "getOn",
                    &[obj_id, name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            } else {
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Instance",
                    "getField",
                    &[target, name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            }
        } else {
            let name_val = fb.const_string(&var_name);
            let val = fb.system_call(
                "GameMaker.Instance",
                "getField",
                &[target, name_val],
                Type::Dynamic,
            );
            stack.push(val);
        }
        return Ok(());
    }

    // Handle 2D array access (ref_type == 0 with non-negative instance).
    // The instruction pops 2 indices from the stack (dim1, dim2).
    if is_2d_array_access(var_ref, instance) {
        // Stack layout: [dim2, dim1] with dim1 on top.
        // dim1 is the meaningful first-dimension index.
        let dim1 = pop(stack, inst)?; // first-dimension index (top of stack)
        let _dim2 = pop(stack, inst)?; // second-dimension (ignored for 1D)
        // If the stack still has 2+ items after popping the indices, those are
        // the original (pre-Dup) copies left by a compound assignment pattern:
        //   push dim2, push dim1, Dup, VARI-read (← here), arithmetic, VARI-write
        // The subsequent Pop must use reversed order: new_value=top, dim1, dim2.
        if stack.len() >= 2 {
            *compound_2d_pending = true;
        }
        if var_name == "argument" {
            // argument[N] → function parameter (or captured argument in a with-body).
            if let Some(Constant::Int(idx)) = fb.try_get_const(dim1) {
                let n = *idx as usize;
                if let Some(&slot) = locals.get(&format!("_argument{n}")) {
                    // Inside a with-body: argument was captured as a local slot.
                    stack.push(fb.load(slot, Type::Dynamic));
                } else {
                    let param_offset = if ctx.has_self { 1 } else { 0 }
                        + if ctx.has_other { 1 } else { 0 };
                    let param = fb.param(param_offset + n);
                    stack.push(param);
                }
            } else {
                // Dynamic index — if the function has a rest param, index into it:
                // `argument[expr]` → `args[expr]`.  Otherwise fall back to getField.
                if let Some(&rest_id) = locals.get("_args") {
                    let val = fb.get_index(rest_id, dim1, Type::Dynamic);
                    stack.push(val);
                } else {
                    let name_val = fb.const_string(&var_name);
                    let val = fb.system_call(
                        "GameMaker.Instance",
                        "getField",
                        &[dim1, name_val],
                        Type::Dynamic,
                    );
                    stack.push(val);
                }
            }
        } else if ctx.has_self {
            // ref_type==0 with instance >= 0: the instance field is the VARI
            // table's scope owner, NOT the target object.  The actual target
            // is always the current instance (self).  Cross-object access uses
            // ref_type 0x80 (stacktop) or 0xA0 (singleton) instead.
            let is_scalar = matches!(fb.try_get_const(dim1), Some(Constant::Int(-1)));
            let self_param = fb.param(0);
            if is_scalar {
                let field_val = fb.get_field(self_param, &var_name, Type::Dynamic);
                stack.push(field_val);
            } else {
                let field_val = fb.get_field(self_param, &var_name, Type::Dynamic);
                let indexed = fb.get_index(field_val, dim1, Type::Dynamic);
                stack.push(indexed);
            }
        } else {
            // Script context (no self): the instance field identifies the
            // target object for cross-object variable access.
            let is_scalar = matches!(fb.try_get_const(dim1), Some(Constant::Int(-1)));
            let obj_id = if let Some(name) = ctx.obj_names.get(instance as usize) {
                fb.const_string(name)
            } else {
                fb.const_int(instance as i64)
            };
            let name_val = fb.const_string(&var_name);
            let args: Vec<ValueId> = if is_scalar {
                vec![obj_id, name_val]
            } else {
                vec![obj_id, name_val, dim1]
            };
            let val = fb.system_call(
                "GameMaker.Instance",
                "getOn",
                &args,
                Type::Dynamic,
            );
            stack.push(val);
        }
        return Ok(());
    }

    // The GameMaker compiler sometimes uses the owning object's index (or a
    // parent's index) as the instance type for self-references instead of -1
    // (Own). Normalize here.
    let instance = if instance >= 0
        && ctx.has_self
        && (ctx.self_object_index == Some(instance as usize)
            || ctx.ancestor_indices.contains(&(instance as usize)))
    {
        -1 // Treat as Own (self)
    } else {
        instance
    };

    match InstanceType::from_i16(instance) {
        Some(InstanceType::Local) => {
            // Local variable: load from alloc slot.
            if let Some(&slot) = locals.get(&var_name) {
                let val = fb.load(slot, Type::Dynamic);
                stack.push(val);
            } else {
                // Fallback: create an on-the-fly alloc and register it for reuse.
                let slot = fb.alloc(Type::Dynamic);
                fb.name_value(slot, var_name.clone());
                locals.insert(var_name, slot);
                let val = fb.load(slot, Type::Dynamic);
                stack.push(val);
            }
        }
        Some(InstanceType::Own) | Some(InstanceType::Builtin) => {
            if let Some(arg_idx) = parse_argument_index(&var_name) {
                // Implicit argument variable → function parameter (or captured slot).
                if let Some(&slot) = locals.get(&format!("_argument{arg_idx}")) {
                    stack.push(fb.load(slot, Type::Dynamic));
                } else {
                    let param_offset = if ctx.has_self { 1 } else { 0 }
                        + if ctx.has_other { 1 } else { 0 };
                    let param = fb.param(param_offset + arg_idx);
                    stack.push(param);
                }
            } else if var_name == "argument_count" {
                // `argument_count` is a GML built-in that returns the number of
                // arguments passed to the current script call.  When the function
                // was emitted with a rest parameter (stored as "_args" in locals),
                // translate this as `args.length`.  Otherwise fall through to the
                // normal self/global field lookup (best-effort fallback).
                if let Some(&rest_id) = locals.get("_args") {
                    let val = fb.get_field(rest_id, "length", Type::Dynamic);
                    stack.push(val);
                } else if ctx.has_self {
                    let self_param = fb.param(0);
                    let val = fb.get_field(self_param, &var_name, Type::Dynamic);
                    stack.push(val);
                } else {
                    let name_val = fb.const_string(&var_name);
                    let val = fb.system_call(
                        "GameMaker.Global",
                        "get",
                        &[name_val],
                        Type::Dynamic,
                    );
                    stack.push(val);
                }
            } else if ctx.has_self {
                let self_param = fb.param(0);
                let val = fb.get_field(self_param, &var_name, Type::Dynamic);
                stack.push(val);
            } else {
                // Script context without self: variable is a global.
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Global",
                    "get",
                    &[name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            }
        }
        Some(InstanceType::Global) => {
            let name_val = fb.const_string(&var_name);
            let val = fb.system_call(
                "GameMaker.Global",
                "get",
                &[name_val],
                Type::Dynamic,
            );
            stack.push(val);
        }
        Some(InstanceType::Other) => {
            if ctx.has_other {
                let other_idx = if ctx.has_self { 1 } else { 0 };
                let other_param = fb.param(other_idx);
                let val = fb.get_field(other_param, &var_name, Type::Dynamic);
                stack.push(val);
            } else {
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Instance",
                    "getOther",
                    &[name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            }
        }
        Some(InstanceType::All) => {
            let name_val = fb.const_string(&var_name);
            let val = fb.system_call(
                "GameMaker.Instance",
                "getAll",
                &[name_val],
                Type::Dynamic,
            );
            stack.push(val);
        }
        Some(InstanceType::Stacktop) => {
            let raw_target = pop(stack, inst)?;
            // In GMS2.3+ struct methods, `Push Int32(-9)` followed by a Stacktop
            // variable access is the "push current struct self" pattern.
            // Resolve to self parameter instead of emitting the literal -9.
            let target = if ctx.has_self {
                if matches!(fb.try_resolve_const(raw_target), Some(Constant::Int(-9))) {
                    fb.param(0)
                } else {
                    raw_target
                }
            } else {
                raw_target
            };
            if var_name == "argument" {
                // argument[N] → function parameter access
                if let Some(Constant::Int(idx)) = fb.try_get_const(target) {
                    let param_offset = if ctx.has_self { 1 } else { 0 }
                        + if ctx.has_other { 1 } else { 0 };
                    let param = fb.param(param_offset + *idx as usize);
                    stack.push(param);
                } else if let Some(&rest_id) = locals.get("_args") {
                    // Dynamic index with rest param — `argument[expr]` → `args[expr]`.
                    let val = fb.get_index(rest_id, target, Type::Dynamic);
                    stack.push(val);
                } else {
                    // Dynamic index — fall back to getField
                    let name_val = fb.const_string(&var_name);
                    let val = fb.system_call(
                        "GameMaker.Instance",
                        "getField",
                        &[target, name_val],
                        Type::Dynamic,
                    );
                    stack.push(val);
                }
            } else if ctx.has_self && target == fb.param(0) {
                // Self-field read in struct method — use get_field for clean output.
                let val = fb.get_field(target, &var_name, Type::Dynamic);
                stack.push(val);
            } else if let Some(Constant::Int(obj_idx)) = fb.try_resolve_const(target) {
    
                if obj_idx >= 0 {
                    let obj_id = if let Some(name) = ctx.obj_names.get(obj_idx as usize) {
                        fb.const_string(name)
                    } else {
                        fb.const_int(obj_idx)
                    };
                    let name_val = fb.const_string(&var_name);
                    let val = fb.system_call(
                        "GameMaker.Instance",
                        "getOn",
                        &[obj_id, name_val],
                        Type::Dynamic,
                    );
                    stack.push(val);
                } else {
                    let name_val = fb.const_string(&var_name);
                    let val = fb.system_call(
                        "GameMaker.Instance",
                        "getField",
                        &[target, name_val],
                        Type::Dynamic,
                    );
                    stack.push(val);
                }
            } else {
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Instance",
                    "getField",
                    &[target, name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            }
        }
        Some(InstanceType::Arg) => {
            // Argument variable: map to function parameter (or captured slot).
            let arg_idx = var_ref.variable_id as usize;
            if let Some(&slot) = locals.get(&format!("_argument{arg_idx}")) {
                stack.push(fb.load(slot, Type::Dynamic));
            } else {
                let param_offset = if ctx.has_self { 1 } else { 0 }
                    + if ctx.has_other { 1 } else { 0 };
                let idx = param_offset + arg_idx;
                if idx < fb.param_count() {
                    let param = fb.param(idx);
                    stack.push(param);
                } else {
                    // Out-of-range argument access — emit as dynamic lookup.
                    let name_val = fb.const_string(format!("argument{arg_idx}"));
                    let val = fb.system_call(
                        "GameMaker.Argument",
                        "get",
                        &[name_val],
                        Type::Dynamic,
                    );
                    stack.push(val);
                }
            }
        }
        _ => {
            // Positive value = specific object ID.
            if instance >= 0 {
                let obj_id = if let Some(name) = ctx.obj_names.get(instance as usize) {
                    fb.const_string(name)
                } else {
                    fb.const_int(instance as i64)
                };
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Instance",
                    "getOn",
                    &[obj_id, name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            } else {
                // Unknown instance type — treat as global.
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Global",
                    "get",
                    &[name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            }
        }
    }
    Ok(())
}

/// Translate a Pop instruction (store to variable).
fn translate_pop(
    inst: &Instruction,
    fb: &mut FunctionBuilder,
    stack: &mut Vec<ValueId>,
    locals: &mut HashMap<String, ValueId>,
    ctx: &TranslateCtx,
    compound_2d_pending: &mut bool,
) -> Result<(), String> {
    if let Operand::Variable { var_ref, instance } = &inst.operand {
        let var_name = resolve_variable_name(inst, ctx);

        // Handle stacktop-via-ref_type (ref_type == 0x80 with instance >= 0).
        // The target instance is on the stack (top), value to store is below.
        // In GMS2.3+, Push Int32(-9) is the "self" sentinel — resolve to the
        // self parameter directly.
        if is_stacktop_ref(var_ref, *instance) {
            let raw_target = pop(stack, inst)?; // instance (top of stack)
            let value = pop(stack, inst)?; // value to store (below)
            let target = if ctx.has_self {
                if matches!(fb.try_resolve_const(raw_target), Some(Constant::Int(-9))) {
                    fb.param(0)
                } else {
                    raw_target
                }
            } else {
                raw_target
            };
            if ctx.has_self && target == fb.param(0) {
                fb.set_field(target, &var_name, value);
            } else if let Some(Constant::Int(obj_idx)) = fb.try_resolve_const(target) {
                // Constant integer target = object index pushed before stacktop access.
                // Resolve to setOn(objName, field, value) for clean class-based access.
    
                if obj_idx >= 0 {
                    let obj_id = if let Some(name) = ctx.obj_names.get(obj_idx as usize) {
                        fb.const_string(name)
                    } else {
                        fb.const_int(obj_idx)
                    };
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Instance",
                        "setOn",
                        &[obj_id, name_val, value],
                        Type::Void,
                    );
                } else {
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Instance",
                        "setField",
                        &[target, name_val, value],
                        Type::Void,
                    );
                }
            } else {
                let name_val = fb.const_string(&var_name);
                fb.system_call(
                    "GameMaker.Instance",
                    "setField",
                    &[target, name_val, value],
                    Type::Void,
                );
            }
            return Ok(());
        }

        // Handle 2D array access (ref_type == 0 with non-negative instance).
        //
        // Simple assignment: stack is [value, dim2, dim1] with dim1 on top.
        // The value was pushed first, then the indices.
        //
        // Compound assignment (+=, -=, etc.): the compiler Dups the indices
        // before the VARI read, leaving originals below. After the read and
        // arithmetic, the stack becomes [dim2, dim1, new_value] with new_value
        // on top. The `compound_2d_pending` flag is set by translate_push_variable
        // when it detects the originals remaining after the 2D read.
        if is_2d_array_access(var_ref, *instance) {
            let (dim1, value) = if *compound_2d_pending {
                *compound_2d_pending = false;
                // Compound: new_value=top, dim1=next, _dim2=bottom
                let value = pop(stack, inst)?;
                let dim1 = pop(stack, inst)?;
                let _dim2 = pop(stack, inst)?;
                (dim1, value)
            } else {
                // Simple: dim1=top, _dim2=next, value=bottom
                let dim1 = pop(stack, inst)?;
                let _dim2 = pop(stack, inst)?;
                let value = pop(stack, inst)?;
                (dim1, value)
            };
            if var_name == "argument" {
                // argument[N] = value → store to function parameter slot (or captured slot).
                if let Some(Constant::Int(idx)) = fb.try_get_const(dim1) {
                    if *idx >= 0 {
                        let n = *idx as usize;
                        if let Some(&slot) = locals.get(&format!("_argument{n}")) {
                            // Inside a with-body: update the captured argument slot.
                            fb.store(slot, value);
                        } else {
                            let param_offset = if ctx.has_self { 1 } else { 0 }
                                + if ctx.has_other { 1 } else { 0 };
                            let abs_idx = param_offset + n;
                            if abs_idx < fb.param_count() {
                                let param = fb.param(abs_idx);
                                let slot = fb.alloc(Type::Dynamic);
                                fb.store(slot, param);
                                fb.store(slot, value);
                            }
                            // else: OOB (with-body uncaptured arg or invalid game code) — skip
                        }
                    }
                    // Negative index: invalid argument write — skip.
                } else {
                    // Dynamic index — fall back to setField
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Instance",
                        "setField",
                        &[dim1, name_val, value],
                        Type::Void,
                    );
                }
            } else if ctx.has_self {
                // ref_type==0 with instance >= 0: the instance field is the
                // VARI table's scope owner, NOT the target object.  The actual
                // target is always self.  (See translate_push_variable.)
                let is_scalar = matches!(fb.try_get_const(dim1), Some(Constant::Int(-1)));
                let self_param = fb.param(0);
                if is_scalar {
                    fb.set_field(self_param, &var_name, value);
                } else {
                    let field_val =
                        fb.get_field(self_param, &var_name, Type::Dynamic);
                    fb.set_index(field_val, dim1, value);
                }
            } else {
                // Script context (no self): cross-object variable access.
                let is_scalar = matches!(fb.try_get_const(dim1), Some(Constant::Int(-1)));
                let obj_id =
                    if let Some(name) = ctx.obj_names.get(*instance as usize) {
                        fb.const_string(name)
                    } else {
                        fb.const_int(*instance as i64)
                    };
                let name_val = fb.const_string(&var_name);
                let args: Vec<ValueId> = if is_scalar {
                    vec![obj_id, name_val, value]
                } else {
                    vec![obj_id, name_val, dim1, value]
                };
                fb.system_call(
                    "GameMaker.Instance",
                    "setOn",
                    &args,
                    Type::Void,
                );
            }
            return Ok(());
        }

        // Non-2D-array Pop: single value on top of stack.
        let value = pop(stack, inst)?;

        // Normalize self-referencing instance types (see translate_push_variable).
        let instance = if *instance >= 0
            && ctx.has_self
            && (ctx.self_object_index == Some(*instance as usize)
                || ctx.ancestor_indices.contains(&(*instance as usize)))
        {
            -1
        } else {
            *instance
        };

        match InstanceType::from_i16(instance) {
            Some(InstanceType::Local) => {
                if let Some(&slot) = locals.get(&var_name) {
                    fb.store(slot, value);
                } else {
                    // Orphan local — create slot and register for reuse.
                    let slot = fb.alloc(Type::Dynamic);
                    fb.name_value(slot, var_name.clone());
                    locals.insert(var_name, slot);
                    fb.store(slot, value);
                }
            }
            Some(InstanceType::Own) | Some(InstanceType::Builtin) => {
                if let Some(arg_idx) = parse_argument_index(&var_name) {
                    // Implicit argument variable → store via local slot.
                    let param_offset = if ctx.has_self { 1 } else { 0 }
                        + if ctx.has_other { 1 } else { 0 };
                    if let Some(&slot) = locals.get(&format!("_argument{arg_idx}")) {
                        // Inside a with-body: update the captured argument slot.
                        fb.store(slot, value);
                    } else {
                        let abs_idx = param_offset + arg_idx;
                        if abs_idx < fb.param_count() {
                            let param = fb.param(abs_idx);
                            let slot = fb.alloc(Type::Dynamic);
                            fb.name_value(slot, var_name.clone());
                            fb.store(slot, param);
                            fb.store(slot, value);
                            locals.insert(var_name, slot);
                        }
                        // else: OOB (with-body uncaptured arg or invalid game code) — skip.
                    }
                } else if ctx.has_self {
                    let self_param = fb.param(0);
                    fb.set_field(self_param, &var_name, value);
                } else {
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Global",
                        "set",
                        &[name_val, value],
                        Type::Void,
                    );
                }
            }
            Some(InstanceType::Global) => {
                let name_val = fb.const_string(&var_name);
                fb.system_call(
                    "GameMaker.Global",
                    "set",
                    &[name_val, value],
                    Type::Void,
                );
            }
            Some(InstanceType::Other) => {
                if ctx.has_other {
                    let other_idx = if ctx.has_self { 1 } else { 0 };
                    let other_param = fb.param(other_idx);
                    fb.set_field(other_param, &var_name, value);
                } else {
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Instance",
                        "setOther",
                        &[name_val, value],
                        Type::Void,
                    );
                }
            }
            Some(InstanceType::All) => {
                let name_val = fb.const_string(&var_name);
                fb.system_call(
                    "GameMaker.Instance",
                    "setAll",
                    &[name_val, value],
                    Type::Void,
                );
            }
            Some(InstanceType::Stacktop) => {
                let raw_target = pop(stack, inst)?;
                // In GMS2.3+ struct methods, -9 is the self-reference sentinel.
                let target = if ctx.has_self {
                    if matches!(fb.try_resolve_const(raw_target), Some(Constant::Int(-9))) {
                        fb.param(0)
                    } else {
                        raw_target
                    }
                } else {
                    raw_target
                };
                if var_name == "argument" {
                    // argument[N] = value → store to function parameter slot
                    if let Some(Constant::Int(idx)) = fb.try_get_const(target) {
                        if *idx >= 0 {
                            let n = *idx as usize;
                            let param_offset = if ctx.has_self { 1 } else { 0 }
                                + if ctx.has_other { 1 } else { 0 };
                            if let Some(&slot) = locals.get(&format!("_argument{n}")) {
                                // Inside a with-body: update the captured argument slot.
                                fb.store(slot, value);
                            } else {
                                let abs_idx = param_offset + n;
                                if abs_idx < fb.param_count() {
                                    let param = fb.param(abs_idx);
                                    let slot = fb.alloc(Type::Dynamic);
                                    fb.store(slot, param);
                                    fb.store(slot, value);
                                }
                                // else: OOB — skip.
                            }
                        }
                        // Negative index: invalid argument write — skip.
                    } else {
                        // Dynamic index — fall back to setField
                        let name_val = fb.const_string(&var_name);
                        fb.system_call(
                            "GameMaker.Instance",
                            "setField",
                            &[target, name_val, value],
                            Type::Void,
                        );
                    }
                } else if ctx.has_self && target == fb.param(0) {
                    // Self-field write in struct method — use set_field for clean output.
                    fb.set_field(target, &var_name, value);
                } else if let Some(Constant::Int(obj_idx)) = fb.try_resolve_const(target) {
        
                    if obj_idx >= 0 {
                        let obj_id = if let Some(name) = ctx.obj_names.get(obj_idx as usize) {
                            fb.const_string(name)
                        } else {
                            fb.const_int(obj_idx)
                        };
                        let name_val = fb.const_string(&var_name);
                        fb.system_call(
                            "GameMaker.Instance",
                            "setOn",
                            &[obj_id, name_val, value],
                            Type::Void,
                        );
                    } else {
                        let name_val = fb.const_string(&var_name);
                        fb.system_call(
                            "GameMaker.Instance",
                            "setField",
                            &[target, name_val, value],
                            Type::Void,
                        );
                    }
                } else {
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Instance",
                        "setField",
                        &[target, name_val, value],
                        Type::Void,
                    );
                }
            }
            _ => {
                if instance >= 0 {
                    let obj_id = if let Some(name) = ctx.obj_names.get(instance as usize) {
                        fb.const_string(name)
                    } else {
                        fb.const_int(instance as i64)
                    };
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Instance",
                        "setOn",
                        &[obj_id, name_val, value],
                        Type::Void,
                    );
                } else {
                    let name_val = fb.const_string(&var_name);
                    fb.system_call(
                        "GameMaker.Global",
                        "set",
                        &[name_val, value],
                        Type::Void,
                    );
                }
            }
        }
    } else {
        // Pop without variable destination: just discard.
        let _ = pop(stack, inst)?;
    }
    Ok(())
}

/// Pop a value from the operand stack.
fn pop(stack: &mut Vec<ValueId>, inst: &Instruction) -> Result<ValueId, String> {
    stack.pop().ok_or_else(|| {
        format!(
            "{:#x}: stack underflow on {:?}",
            inst.offset, inst.opcode
        )
    })
}

/// Resolve the fall-through target to (target_offset, BlockId).
fn resolve_fallthrough(
    instructions: &[Instruction],
    inst_idx: usize,
    block_map: &HashMap<usize, BlockId>,
) -> Result<(usize, BlockId), String> {
    let next = instructions.get(inst_idx + 1).ok_or_else(|| {
        format!(
            "no fall-through instruction after index {}",
            inst_idx
        )
    })?;
    let block = block_map.get(&next.offset).copied().ok_or_else(|| {
        format!(
            "fall-through offset {:#x} is not a block start",
            next.offset
        )
    })?;
    Ok((next.offset, block))
}

#[cfg(test)]
mod tests {
    use super::*;
    use datawin::bytecode::decode::Instruction;
    use datawin::bytecode::encode::encode;
    use datawin::bytecode::opcode::Opcode;
    use datawin::bytecode::types::{DataType, VariableRef};
    use reincarnate_core::ir::inst::Op;

    static EMPTY_ASSET_REF_NAMES: std::sync::LazyLock<HashMap<u32, String>> =
        std::sync::LazyLock::new(HashMap::new);

    /// Build a minimal `TranslateCtx` for tests.
    ///
    /// `bytecode_offset = 0` so vari_ref_map keys equal decoded instruction offsets.
    #[allow(clippy::too_many_arguments)]
    fn make_ctx<'a>(
        has_self: bool,
        arg_count: u16,
        variables: &'a [(String, i32)],
        vari_ref_map: &'a HashMap<usize, usize>,
        func_ref_map: &'a HashMap<usize, usize>,
        obj_names: &'a [String],
        function_names: &'a HashMap<u32, String>,
        script_names: &'a HashSet<String>,
    ) -> TranslateCtx<'a> {
        TranslateCtx {
            function_names,
            asset_ref_names: &EMPTY_ASSET_REF_NAMES,
            variables,
            func_ref_map,
            vari_ref_map,
            bytecode_offset: 0,
            local_names: &[],
            string_table: &[],
            has_self,
            has_other: false,
            arg_count,
            obj_names,
            class_name: None,
            self_object_index: None,
            ancestor_indices: HashSet::new(),
            script_names,
            is_with_body: false,
        }
    }

    /// Collect all `Op` values from a translated function.
    fn collect_ops(func: &reincarnate_core::ir::func::Function) -> Vec<Op> {
        func.insts.values().map(|i| i.op.clone()).collect()
    }

    /// Build and encode a Push instruction for an Int16 constant.
    fn pushi(val: i16) -> Instruction {
        Instruction {
            offset: 0,
            opcode: Opcode::PushI,
            type1: DataType::Int16,
            type2: DataType::Double,
            operand: Operand::Int16(val),
        }
    }

    /// Build and encode a Push.v.v (variable read) instruction.
    fn push_var(instance: i16, ref_type: u8) -> Instruction {
        Instruction {
            offset: 0,
            opcode: Opcode::Push,
            type1: DataType::Variable,
            type2: DataType::Variable,
            operand: Operand::Variable {
                var_ref: VariableRef { variable_id: 0, ref_type },
                instance,
            },
        }
    }

    /// Build and encode a Pop.v.v (variable write) instruction.
    fn pop_var(instance: i16, ref_type: u8) -> Instruction {
        Instruction {
            offset: 0,
            opcode: Opcode::Pop,
            type1: DataType::Variable,
            type2: DataType::Variable,
            operand: Operand::Variable {
                var_ref: VariableRef { variable_id: 0, ref_type },
                instance,
            },
        }
    }

    /// Build an Exit instruction (no-value return).
    fn exit_inst() -> Instruction {
        Instruction {
            offset: 0,
            opcode: Opcode::Exit,
            type1: DataType::Double,
            type2: DataType::Double,
            operand: Operand::None,
        }
    }

    /// Build a Ret instruction (pops value from stack and returns it).
    fn ret_inst() -> Instruction {
        Instruction {
            offset: 0,
            opcode: Opcode::Ret,
            type1: DataType::Variable,
            type2: DataType::Double,
            operand: Operand::None,
        }
    }

    // -----------------------------------------------------------------------
    // 2D array write — correct stack pop order
    // -----------------------------------------------------------------------

    /// Non-scalar 2D array write: `myarray[0] = 42`
    ///
    /// GML stack layout before Pop.v.v: `[value=42, dim2=-1, dim1=0]` (dim1 on top).
    /// The translator must pop dim1 first (top), then dim2, then value.
    /// Expected IR: `SetIndex(GetField(self, "myarray"), dim1_const, 42)`
    ///
    /// This regression guards the fix for the 2D array write stack pop order bug:
    /// if dim1 and value are accidentally swapped, a `SetField` (scalar) is emitted
    /// instead of a `SetIndex`, or the wrong value is stored.
    #[test]
    fn test_2d_array_write_nonscalar_emits_set_index() {
        // Instructions (bytecode_offset=0):
        // offset 0:  PushI.i 42   (value)           4 bytes
        // offset 4:  PushI.i -1   (dim2, don't care) 4 bytes
        // offset 8:  PushI.i 0    (dim1 = index)     4 bytes
        // offset 12: Pop.v.v var  (2D array write)   8 bytes
        // offset 20: Exit                             4 bytes
        let instructions = vec![
            pushi(42),
            pushi(-1),
            pushi(0), // dim1 = 0, non-negative → non-scalar
            pop_var(3, 0), // ref_type=0, instance>=0 → 2D array
            exit_inst(),
        ];
        let bytecode = encode(&instructions);

        let vars: Vec<(String, i32)> = vec![("myarray".into(), -1)];
        // Pop.v.v is at decoded offset 12 (4+4+4 bytes before it).
        let vari_ref_map: HashMap<usize, usize> = [(12, 0)].into_iter().collect();
        let fn_names: HashMap<u32, String> = HashMap::new();
        let func_ref_map: HashMap<usize, usize> = HashMap::new();
        let obj_names: Vec<String> = vec!["Obj0".into(); 4]; // 4 so index 3 is valid
        let script_names: HashSet<String> = HashSet::new();
        let ctx = make_ctx(true, 0, &vars, &vari_ref_map, &func_ref_map, &obj_names, &fn_names, &script_names);

        let (func, _) = translate_code_entry(&bytecode, "test_fn", &ctx)
            .expect("translation failed");
        let ops = collect_ops(&func);

        let has_set_index = ops.iter().any(|op| matches!(op, Op::SetIndex { .. }));
        let has_set_field = ops.iter().any(|op| matches!(op, Op::SetField { .. }));
        assert!(has_set_index, "expected SetIndex for non-scalar 2D array write; ops: {ops:?}");
        assert!(!has_set_field, "unexpected SetField for non-scalar 2D array write; ops: {ops:?}");
    }

    /// Scalar 2D array write: `myfield[# -1, -1] = 42` (dim1 = -1 → scalar access).
    ///
    /// When dim1 == -1 (the "don't care" sentinel), the translator treats the access
    /// as a plain field write: `SetField(self, "myfield", 42)`.
    #[test]
    fn test_2d_array_write_scalar_emits_set_field() {
        // offset 0:  PushI.i 42   (value)
        // offset 4:  PushI.i -1   (dim2)
        // offset 8:  PushI.i -1   (dim1 = -1 → scalar)
        // offset 12: Pop.v.v var
        // offset 20: Exit
        let instructions = vec![
            pushi(42),
            pushi(-1),
            pushi(-1), // dim1 = -1 → is_scalar = true
            pop_var(3, 0),
            exit_inst(),
        ];
        let bytecode = encode(&instructions);

        let vars: Vec<(String, i32)> = vec![("myfield".into(), -1)];
        let vari_ref_map: HashMap<usize, usize> = [(12, 0)].into_iter().collect();
        let fn_names: HashMap<u32, String> = HashMap::new();
        let func_ref_map: HashMap<usize, usize> = HashMap::new();
        let obj_names: Vec<String> = vec!["Obj0".into(); 4];
        let script_names: HashSet<String> = HashSet::new();
        let ctx = make_ctx(true, 0, &vars, &vari_ref_map, &func_ref_map, &obj_names, &fn_names, &script_names);

        let (func, _) = translate_code_entry(&bytecode, "test_fn", &ctx)
            .expect("translation failed");
        let ops = collect_ops(&func);

        let has_set_field = ops.iter().any(|op| matches!(op, Op::SetField { field, .. } if field == "myfield"));
        let has_set_index = ops.iter().any(|op| matches!(op, Op::SetIndex { .. }));
        assert!(has_set_field, "expected SetField for scalar 2D array write; ops: {ops:?}");
        assert!(!has_set_index, "unexpected SetIndex for scalar 2D array write; ops: {ops:?}");
    }

    // -----------------------------------------------------------------------
    // argument[N] variable mapping — 2D array pattern (GMS1)
    // -----------------------------------------------------------------------

    /// Build a Dup instruction.
    fn dup_inst(n: u16, type1: DataType) -> Instruction {
        Instruction {
            offset: 0,
            opcode: Opcode::Dup,
            type1,
            type2: DataType::Double,
            operand: Operand::Dup(n),
        }
    }

    /// Build an Add instruction (no operand, operates on stack).
    fn add_inst(type1: DataType) -> Instruction {
        Instruction {
            offset: 0,
            opcode: Opcode::Add,
            type1,
            type2: DataType::Double,
            operand: Operand::None,
        }
    }

    // -----------------------------------------------------------------------
    // 2D array compound assignment — correct stack layout (value on top)
    // -----------------------------------------------------------------------

    /// Compound 2D array write: `myarray[5] += 10`
    ///
    /// GML bytecode for compound assignment uses the Dup pattern:
    ///   push dim2 (artifact), push dim1 (index), Dup, Push.v.v (read), arithmetic, Pop.v.v (write)
    ///
    /// After the Dup+read+arithmetic, the stack is `[dim2, dim1, new_value]` with
    /// new_value on TOP — the OPPOSITE of simple assignment `[value, dim2, dim1]`.
    /// The `compound_2d_pending` flag, set by translate_push_variable, causes
    /// translate_pop to use the reversed pop order: value=top, dim1=next, dim2=bottom.
    ///
    /// This test guards that:
    /// 1. A `SetIndex` (not `SetField`) is emitted — index was non-scalar.
    /// 2. The index passed to `SetIndex` is the ORIGINAL dim1 constant (5), not
    ///    the Add result — confirming the new_value is stored, not used as index.
    #[test]
    fn test_2d_array_compound_write_uses_correct_operands() {
        // Bytecode sequence for `myarray[5] += 10`:
        // offset  0: PushI.i16 3  (dim2 artifact, 1 unit)    → 4 bytes
        // offset  4: PushI.i16 5  (dim1 = array index 5)     → 4 bytes
        // offset  8: Dup.i16 1    (dup top 2 items: 2 units)  → 4 bytes
        // offset 12: Push.v.v     (VARI read: pops dim1_copy+dim2_copy, pushes current) → 8 bytes
        // offset 20: PushI.i16 10 (value to add)              → 4 bytes
        // offset 24: Add.i16      (sum = current + 10)        → 4 bytes
        // offset 28: Pop.v.v      (VARI write, compound)      → 8 bytes
        // offset 36: Exit                                      → 4 bytes
        let instructions = vec![
            pushi(3),               // dim2 artifact
            pushi(5),               // dim1 = index
            dup_inst(1, DataType::Int16), // dup top 2 Int16 items (2 * 1 unit each)
            push_var(3, 0),         // 2D VARI read (ref_type=0, instance=3 ≥ 0)
            pushi(10),              // value to add
            add_inst(DataType::Int16), // sum
            pop_var(3, 0),          // 2D VARI write (same variable)
            exit_inst(),
        ];
        let bytecode = encode(&instructions);

        let vars: Vec<(String, i32)> = vec![("myarray".into(), -1)];
        // Push.v.v is at offset 12, Pop.v.v is at offset 28.
        let vari_ref_map: HashMap<usize, usize> =
            [(12, 0), (28, 0)].into_iter().collect();
        let fn_names: HashMap<u32, String> = HashMap::new();
        let func_ref_map: HashMap<usize, usize> = HashMap::new();
        let obj_names: Vec<String> = vec!["Obj0".into(); 4]; // index 3 valid
        let script_names: HashSet<String> = HashSet::new();
        let ctx = make_ctx(true, 0, &vars, &vari_ref_map, &func_ref_map, &obj_names, &fn_names, &script_names);

        let (func, _) = translate_code_entry(&bytecode, "test_compound_2d", &ctx)
            .expect("translation failed");

        // Collect (op, result_value_id) pairs to trace operand relationships.
        let insts: Vec<_> = func.insts.values().collect();

        // Find the SetIndex instruction.
        let set_index = insts.iter().find(|i| matches!(i.op, Op::SetIndex { .. }));
        assert!(set_index.is_some(), "expected SetIndex for compound 2D write; ops: {:?}",
            insts.iter().map(|i| &i.op).collect::<Vec<_>>());

        let Op::SetIndex { index, value, .. } = &set_index.unwrap().op else { unreachable!() };

        // The index must be the constant 5 (original dim1), NOT the Add result.
        let index_is_const_5 = insts.iter().any(|i| {
            i.result == Some(*index) && matches!(i.op, Op::Const(reincarnate_core::ir::Constant::Int(5)))
        });
        assert!(index_is_const_5,
            "SetIndex index should be dim1=const(5), not the Add result; index={index:?}, ops={insts:?}");

        // The value must NOT be the constant 10 or 3 — it should be the Add result.
        let value_is_plain_const = insts.iter().any(|i| {
            i.result == Some(*value) && matches!(i.op, Op::Const(_))
        });
        assert!(!value_is_plain_const,
            "SetIndex value should be the Add result (sum), not a plain constant; value={value:?}");
    }

    // -----------------------------------------------------------------------
    // PushEnv / PopEnv — with-block post-continuation must be reachable
    // -----------------------------------------------------------------------

    /// Post-with code (instructions after PopEnv) must appear in the IR.
    ///
    /// Before the fix, PopEnv's loop-back case emitted `br body_block` (an
    /// unconditional loop-back), leaving the fall-through block unreachable.
    /// After the fix, PopEnv always falls through to the next instruction so
    /// that the post-with code is reachable.
    ///
    /// Bytecode layout:
    ///   offset  0: PushI.i16 5              — target object
    ///   offset  4: PushEnv Branch(12)       — skip to PopEnv at offset 16
    ///   offset  8: PushI.i16 0              — body: push a value
    ///   offset 12: Popz                     — body: pop it
    ///   offset 16: PopEnv Branch(-8)        — loop-back sentinel (16-8=8 ≥ 0)
    ///   offset 20: PushI.i16 99             — post-with sentinel
    ///   offset 24: Popz                     — discard sentinel
    ///   offset 28: Ret
    #[test]
    fn test_popenv_fall_through_reaches_post_with_code() {
        fn popz_inst() -> Instruction {
            Instruction {
                offset: 0,
                opcode: Opcode::Popz,
                type1: DataType::Int16,
                type2: DataType::Double,
                operand: Operand::None,
            }
        }
        fn branch_inst(opcode: Opcode, byte_offset: i32) -> Instruction {
            Instruction {
                offset: 0,
                opcode,
                type1: DataType::Double,
                type2: DataType::Double,
                operand: Operand::Branch(byte_offset),
            }
        }

        // PushEnv Branch(12): skip body, target = offset 4+12=16 (PopEnv).
        // PopEnv Branch(-8): loop-back, sentinel = 16+(-8)=8 ≥ 0.
        let instructions = vec![
            pushi(5),                         // offset  0: push target
            branch_inst(Opcode::PushEnv, 12), // offset  4: skip to offset 16
            pushi(0),                         // offset  8: body push
            popz_inst(),                      // offset 12: body pop
            branch_inst(Opcode::PopEnv, -8),  // offset 16: loop-back to offset 8
            pushi(99),                        // offset 20: post-with sentinel
            popz_inst(),                      // offset 24: pop sentinel
            Instruction {                     // offset 28: Exit (void return)
                offset: 0,
                opcode: Opcode::Exit,
                type1: DataType::Double,
                type2: DataType::Double,
                operand: Operand::None,
            },
        ];
        let bytecode = encode(&instructions);

        let vars: Vec<(String, i32)> = vec![];
        let vari_ref_map: HashMap<usize, usize> = HashMap::new();
        let fn_names: HashMap<u32, String> = HashMap::new();
        let func_ref_map: HashMap<usize, usize> = HashMap::new();
        let obj_names: Vec<String> = vec![];
        let script_names: HashSet<String> = HashSet::new();
        let ctx = make_ctx(false, 0, &vars, &vari_ref_map, &func_ref_map, &obj_names, &fn_names, &script_names);

        let (func, extra_funcs) = translate_code_entry(&bytecode, "test_with_continuation", &ctx)
            .expect("translation failed");
        let ops = collect_ops(&func);

        // MakeClosure must appear (PushEnv → closure extraction).
        let has_make_closure = ops.iter().any(|op| matches!(op, Op::MakeClosure { .. }));
        assert!(has_make_closure, "MakeClosure must appear for with-block; ops: {ops:?}");

        // withInstances syscall must appear.
        let has_with_instances = ops.iter().any(|op| {
            matches!(op, Op::SystemCall { system, method, .. }
                if system == "GameMaker.Instance" && method == "withInstances")
        });
        assert!(has_with_instances, "withInstances syscall must appear; ops: {ops:?}");

        // An extra closure function must have been extracted.
        assert_eq!(extra_funcs.len(), 1, "expected 1 closure function; got {}", extra_funcs.len());
        assert_eq!(extra_funcs[0].method_kind, reincarnate_core::ir::func::MethodKind::Closure,
            "extra function must be MethodKind::Closure");

        // Post-with sentinel (Const 99) must be reachable in the outer function.
        let has_post_with_sentinel = ops.iter().any(|op| {
            matches!(op, Op::Const(reincarnate_core::ir::value::Constant::Int(99)))
        });
        assert!(has_post_with_sentinel,
            "post-with code (Const(99) sentinel) must be reachable; ops: {ops:?}");
    }

    /// `argument[1]` push with 2D array encoding maps to `fb.param(1)`, not a
    /// heap allocation or runtime lookup.
    ///
    /// GMS1 encodes `argument[N]` as a 2D array read with `ref_type=0`:
    ///   PushI -1 (dim2), PushI N (dim1), Push.v.v argument
    /// The translator must recognize this and map it to the Nth function parameter,
    /// not emit a `SystemCall("GameMaker.Instance", "getField", ...)`.
    #[test]
    fn test_argument_2d_array_push_maps_to_param() {
        // offset 0: PushI.i -1     (dim2, don't care)   4 bytes
        // offset 4: PushI.i 1      (dim1 = argument[1]) 4 bytes
        // offset 8: Push.v.v arg   (2D array read)       8 bytes
        // offset 16: Ret                                  4 bytes
        let instructions = vec![
            pushi(-1),
            pushi(1), // dim1 = 1 → argument index 1
            push_var(0, 0), // ref_type=0, instance=0 → 2D array
            ret_inst(),
        ];
        let bytecode = encode(&instructions);

        let vars: Vec<(String, i32)> = vec![("argument".into(), -2)]; // -2 = Builtin
        // Push.v.v is at decoded offset 8.
        let vari_ref_map: HashMap<usize, usize> = [(8, 0)].into_iter().collect();
        let fn_names: HashMap<u32, String> = HashMap::new();
        let func_ref_map: HashMap<usize, usize> = HashMap::new();
        let obj_names: Vec<String> = vec!["Obj0".into()];
        let script_names: HashSet<String> = HashSet::new();
        // No has_self; arg_count=0 (scan_implicit_args will detect argument[1]).
        let ctx = make_ctx(false, 0, &vars, &vari_ref_map, &func_ref_map, &obj_names, &fn_names, &script_names);

        let (func, _) = translate_code_entry(&bytecode, "test_fn", &ctx)
            .expect("translation failed");
        let ops = collect_ops(&func);

        // Must NOT fall back to a SystemCall (getField / getOn).
        let has_syscall = ops.iter().any(|op| {
            matches!(op, Op::SystemCall { system, method, .. }
                if system == "GameMaker.Instance" && (method == "getField" || method == "getOn"))
        });
        assert!(!has_syscall, "argument[N] must map to param, not syscall; ops: {ops:?}");

        // The function must have been given 2 params (argument0, argument1)
        // because scan_implicit_args detected argument[1].
        assert_eq!(func.sig.params.len(), 2, "expected 2 params for implicit argument[1]");
    }
}
