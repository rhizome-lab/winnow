use std::collections::{BTreeSet, HashMap, HashSet};

use datawin::bytecode::decode::{self, Instruction, Operand};
use datawin::bytecode::opcode::Opcode;
use datawin::bytecode::types::{ComparisonKind, DataType, InstanceType, VariableRef};
use datawin::chunks::func::CodeLocals;
use datawin::DataWin;
use reincarnate_core::entity::EntityRef;
use reincarnate_core::ir::builder::FunctionBuilder;
use reincarnate_core::ir::block::BlockId;
use reincarnate_core::ir::func::{Function, Visibility};
use reincarnate_core::ir::inst::{CmpKind, Op};
use reincarnate_core::ir::ty::{FunctionSig, Type};
use reincarnate_core::ir::value::{Constant, ValueId};

/// Context for translating a single code entry.
pub struct TranslateCtx<'a> {
    /// The DataWin file (for string resolution).
    pub dw: &'a DataWin,
    /// FUNC function entries: entry_index → resolved name.
    pub function_names: &'a HashMap<u32, String>,
    /// VARI variable entries: entry_index → (name, instance_type).
    pub variables: &'a [(String, i32)],
    /// FUNC linked-list reference map: absolute bytecode address → func entry index.
    pub func_ref_map: &'a HashMap<usize, usize>,
    /// VARI linked-list reference map: absolute bytecode address → vari entry index.
    pub vari_ref_map: &'a HashMap<usize, usize>,
    /// Absolute file offset where this code entry's bytecode begins.
    pub bytecode_offset: usize,
    /// Code-local variable names: local_index → name.
    pub locals: Option<&'a CodeLocals>,
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
}

/// Translate a single code entry's bytecode into an IR Function.
pub fn translate_code_entry(
    bytecode: &[u8],
    func_name: &str,
    ctx: &TranslateCtx,
) -> Result<reincarnate_core::ir::func::Function, String> {
    let instructions = decode::decode(bytecode).map_err(|e| format!("{func_name}: {e}"))?;
    if instructions.is_empty() {
        return build_empty_function(func_name, ctx);
    }



    // Pass 1: Find basic block boundaries.
    let block_starts = find_block_starts(&instructions);

    // Pass 2: Create IR blocks.
    // Old-style scripts may use argumentN without declaring parameters —
    // scan for implicit argument references to determine true arg count.
    let implicit_args = scan_implicit_args(&instructions, ctx);
    let effective_arg_count = ctx.arg_count.max(implicit_args);
    let sig = build_signature_with_args(ctx, effective_arg_count);
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

    // Pre-compute stack depths at each block entry.
    let block_entry_depths = compute_block_stack_depths(&instructions, &block_starts);

    // Block 0 = entry block (always offset 0). Create the rest.
    let mut block_map: HashMap<usize, BlockId> = HashMap::new();
    let mut block_params: HashMap<usize, Vec<ValueId>> = HashMap::new();
    block_map.insert(0, fb.entry_block());
    for &off in &block_starts {
        if off != 0 {
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

    // Allocate locals.
    let mut locals = allocate_locals(&mut fb, ctx);

    // Pass 3: Translate instructions.
    let mut stack: Vec<ValueId> = Vec::new();
    fb.switch_to_block(fb.entry_block());
    let mut terminated = false;

    for (inst_idx, inst) in instructions.iter().enumerate() {
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
                if let Some(params) = block_params.get(&inst.offset) {
                    stack.extend(params.iter().copied());
                }
                terminated = false;
            }
        }

        if terminated {
            continue;
        }

        translate_instruction(
            inst,
            &instructions,
            inst_idx,
            &mut fb,
            &mut stack,
            &block_map,
            &mut locals,
            ctx,
            &mut terminated,
            &block_entry_depths,
        )?;
    }

    // If the last block wasn't terminated, add a void return.
    if !terminated {
        fb.ret(None);
    }

    let mut func = fb.build();
    detect_switches(&mut func);
    Ok(func)
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
                if let Some(next) = instructions.get(i + 1) {
                    starts.insert(next.offset);
                }
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
    if ctx.has_self {
        let self_ty = ctx
            .class_name
            .map(|name| Type::Struct(name.to_string()))
            .unwrap_or(Type::Dynamic);
        params.push(self_ty);
    }
    if ctx.has_other {
        params.push(Type::Dynamic);
    }
    for _ in 0..arg_count {
        params.push(Type::Dynamic);
    }
    FunctionSig {
        params,
        return_ty: Type::Dynamic,
        ..Default::default()
    }
}

/// Parse `argumentN` variable names and return the index N, if any.
fn parse_argument_index(name: &str) -> Option<usize> {
    name.strip_prefix("argument").and_then(|s| s.parse::<usize>().ok())
}

/// Scan instructions for implicit `argument0`..`argumentN` references
/// (variables with Builtin/Own instance type whose name matches `argumentN`)
/// and `argument[N]` references (Stacktop instance type with name "argument"
/// preceded by a constant integer push).
/// Returns the number of implicit arguments (max index + 1), or 0 if none found.
fn scan_implicit_args(instructions: &[Instruction], ctx: &TranslateCtx) -> u16 {
    let mut max_idx: Option<usize> = None;
    for (i, inst) in instructions.iter().enumerate() {
        if let Operand::Variable { var_ref, instance } = &inst.operand {
            let it = InstanceType::from_i16(*instance);
            if matches!(it, Some(InstanceType::Own) | Some(InstanceType::Builtin)) {
                let name = resolve_variable_name(inst, ctx);
                if let Some(idx) = parse_argument_index(&name) {
                    max_idx = Some(max_idx.map_or(idx, |m: usize| m.max(idx)));
                }
            } else if matches!(it, Some(InstanceType::Stacktop)) {
                let name = resolve_variable_name(inst, ctx);
                if name == "argument" {
                    // argument[N] pattern (GMS2): preceding instruction pushes the index.
                    if let Some(idx) = preceding_const_int(instructions, i) {
                        let idx = idx as usize;
                        max_idx = Some(max_idx.map_or(idx, |m: usize| m.max(idx)));
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
                    }
                }
            }
        }
    }
    max_idx.map_or(0, |m| (m + 1) as u16)
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
    if let Some(code_locals) = ctx.locals {
        // In code_locals, arguments are listed alongside locals.
        // Arguments typically have low indices. We look for a match.
        for local in &code_locals.locals {
            if local.index == i as u32 {
                if let Ok(name) = local.name.resolve(ctx.dw.data()) {
                    return Some(name);
                }
            }
        }
    }
    None
}

/// Allocate local variable slots in the entry block.
fn allocate_locals(
    fb: &mut FunctionBuilder,
    ctx: &TranslateCtx,
) -> HashMap<String, ValueId> {
    let mut locals = HashMap::new();
    if let Some(code_locals) = ctx.locals {
        for local in &code_locals.locals {
            if let Ok(name) = local.name.resolve(ctx.dw.data()) {
                let slot = fb.alloc(Type::Dynamic);
                fb.name_value(slot, name.clone());
                locals.insert(name, slot);
            }
        }
    }
    locals
}

/// Build an empty function with just a void return.
fn build_empty_function(
    name: &str,
    ctx: &TranslateCtx,
) -> Result<reincarnate_core::ir::func::Function, String> {
    let sig = build_signature(ctx);
    let mut fb = FunctionBuilder::new(name, sig, Visibility::Public);
    fb.ret(None);
    Ok(fb.build())
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
            // Dup(N) duplicates the top N+1 values on the stack.
            if let Operand::Dup(n) = inst.operand {
                (0, n as usize + 1)
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
            if let Operand::Call { argc, .. } = inst.operand {
                (argc as usize + 1, 1)
            } else {
                (1, 1)
            }
        }
        Opcode::Ret => (1, 0),
        Opcode::Exit => (0, 0),
        Opcode::B => (0, 0),
        Opcode::Bt | Opcode::Bf => (1, 0),
        Opcode::PushEnv => (1, 0),
        Opcode::PopEnv => (0, 0),
        Opcode::Break => {
            if let Operand::Break(signal) = inst.operand {
                match signal {
                    0xFFFF | 0xFFFC | 0xFFFB => (0, 0),
                    0xFFFE => (2, 1),
                    0xFFFD => (3, 0),
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
) -> Result<(), String> {
    match inst.opcode {
        // ============================================================
        // Constants
        // ============================================================
        Opcode::PushI | Opcode::Push | Opcode::PushLoc | Opcode::PushGlb | Opcode::PushBltn => {
            translate_push(inst, fb, stack, locals, ctx)?;
        }

        // ============================================================
        // Arithmetic (binary)
        // ============================================================
        Opcode::Add => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.add(a, b));
        }
        Opcode::Sub => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.sub(a, b));
        }
        Opcode::Mul => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.mul(a, b));
        }
        Opcode::Div => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.div(a, b));
        }
        Opcode::Rem | Opcode::Mod => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.rem(a, b));
        }

        // ============================================================
        // Unary
        // ============================================================
        Opcode::Neg => {
            let a = pop(stack, inst)?;
            stack.push(fb.neg(a));
        }
        Opcode::Not => {
            let a = pop(stack, inst)?;
            stack.push(fb.not(a));
        }

        // ============================================================
        // Bitwise
        // ============================================================
        Opcode::And => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.bit_and(a, b));
        }
        Opcode::Or => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.bit_or(a, b));
        }
        Opcode::Xor => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.bit_xor(a, b));
        }
        Opcode::Shl => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.shl(a, b));
        }
        Opcode::Shr => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            stack.push(fb.shr(a, b));
        }

        // ============================================================
        // Comparison
        // ============================================================
        Opcode::Cmp => {
            let b = pop(stack, inst)?;
            let a = pop(stack, inst)?;
            if let Operand::Comparison(kind) = inst.operand {
                let cmp_kind = comparison_to_cmp_kind(kind);
                stack.push(fb.cmp(cmp_kind, a, b));
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
                let args = get_branch_args(stack, block_entry_depths.get(&target_off).copied().unwrap_or(0));
                fb.br(target, &args);
                *terminated = true;
            }
        }
        Opcode::Bt => {
            if let Operand::Branch(offset) = inst.operand {
                let cond = pop(stack, inst)?;
                let (then_off, then_target) = resolve_branch_target(inst, offset, block_map)?;
                let (else_off, else_target) = resolve_fallthrough(instructions, inst_idx, block_map)?;
                let then_args = get_branch_args(stack, block_entry_depths.get(&then_off).copied().unwrap_or(0));
                let else_args = get_branch_args(stack, block_entry_depths.get(&else_off).copied().unwrap_or(0));
                fb.br_if(cond, then_target, &then_args, else_target, &else_args);
                *terminated = true;
            }
        }
        Opcode::Bf => {
            if let Operand::Branch(offset) = inst.operand {
                let cond = pop(stack, inst)?;
                let (branch_off, branch_target) = resolve_branch_target(inst, offset, block_map)?;
                let (fall_off, fall_target) = resolve_fallthrough(instructions, inst_idx, block_map)?;
                let fall_args = get_branch_args(stack, block_entry_depths.get(&fall_off).copied().unwrap_or(0));
                let target_args = get_branch_args(stack, block_entry_depths.get(&branch_off).copied().unwrap_or(0));
                // Bf branches when false, so swap: then=fallthrough, else=branch
                fb.br_if(cond, fall_target, &fall_args, branch_target, &target_args);
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
            let count = if let Operand::Dup(n) = inst.operand {
                n as usize + 1
            } else {
                1
            };
            if stack.len() < count {
                return Err(format!("{:#x}: Dup({}) on stack of depth {}", inst.offset, count - 1, stack.len()));
            }
            let start = stack.len() - count;
            let to_dup: Vec<ValueId> = stack[start..].to_vec();
            for &v in &to_dup {
                let copied = fb.copy(v);
                stack.push(copied);
            }
        }

        // ============================================================
        // Pop (variable store)
        // ============================================================
        Opcode::Pop => {
            translate_pop(inst, fb, stack, locals, ctx)?;
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
                let result = fb.call(&func_name, &args, Type::Dynamic);
                stack.push(result);
            }
        }
        Opcode::CallV => {
            if let Operand::Call { argc, .. } = inst.operand {
                let callee = pop(stack, inst)?;
                let mut args = Vec::with_capacity(argc as usize);
                for _ in 0..argc {
                    args.push(pop(stack, inst)?);
                }
                let result = fb.call_indirect(callee, &args, Type::Dynamic);
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
            stack.push(coerced);
        }

        // ============================================================
        // PushEnv / PopEnv (with-blocks)
        // ============================================================
        Opcode::PushEnv => {
            if let Operand::Branch(offset) = inst.operand {
                let target_obj = pop(stack, inst)?;
                let _with_begin = fb.system_call(
                    "GameMaker.Instance",
                    "withBegin",
                    &[target_obj],
                    Type::Dynamic,
                );
                let (body_off, body_block) = resolve_fallthrough(instructions, inst_idx, block_map)?;
                let args = get_branch_args(stack, block_entry_depths.get(&body_off).copied().unwrap_or(0));
                fb.br(body_block, &args);
                *terminated = true;
                let _end_offset = offset;
            }
        }
        Opcode::PopEnv => {
            if let Operand::Branch(offset) = inst.operand {
                let sentinel = inst.offset as i64 + offset as i64;
                if sentinel < 0 || offset == -0x100000 * 4 {
                    // Break out of with-block (sentinel 0xF00000).
                    fb.system_call(
                        "GameMaker.Instance",
                        "withEnd",
                        &[],
                        Type::Void,
                    );
                    let (fall_off, fall) = resolve_fallthrough(instructions, inst_idx, block_map)?;
                    let args = get_branch_args(stack, block_entry_depths.get(&fall_off).copied().unwrap_or(0));
                    fb.br(fall, &args);
                    *terminated = true;
                } else {
                    // Loop back to with-body header.
                    fb.system_call(
                        "GameMaker.Instance",
                        "withEnd",
                        &[],
                        Type::Void,
                    );
                    let (loop_off, loop_target) = resolve_branch_target(inst, offset, block_map)?;
                    let args = get_branch_args(stack, block_entry_depths.get(&loop_off).copied().unwrap_or(0));
                    fb.br(loop_target, &args);
                    *terminated = true;
                }
            }
        }

        // ============================================================
        // Break (special signals)
        // ============================================================
        Opcode::Break => {
            if let Operand::Break(signal) = inst.operand {
                match signal {
                    0xFFFF => {} // chkindex — nop for decompilation
                    0xFFFE => {
                        // pushaf — array get
                        let index = pop(stack, inst)?;
                        let array = pop(stack, inst)?;
                        let val = fb.get_index(array, index, Type::Dynamic);
                        stack.push(val);
                    }
                    0xFFFD => {
                        // popaf — array set
                        let value = pop(stack, inst)?;
                        let index = pop(stack, inst)?;
                        let array = pop(stack, inst)?;
                        fb.set_index(array, index, value);
                    }
                    0xFFFC => {
                        // pushac — array copy (push reference)
                        // For decompilation, treat as a nop (value already on stack).
                    }
                    0xFFFB => {} // setowner — nop for decompilation
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
) -> Result<(), String> {
    match &inst.operand {
        Operand::Int16(v) => stack.push(fb.const_int(*v as i64)),
        Operand::Int32(v) => stack.push(fb.const_int(*v as i64)),
        Operand::Int64(v) => stack.push(fb.const_int(*v)),
        Operand::Double(v) => stack.push(fb.const_float(*v)),
        Operand::Float(v) => stack.push(fb.const_float(*v as f64)),
        Operand::Bool(v) => stack.push(fb.const_bool(*v)),
        Operand::StringIndex(idx) => {
            let s = ctx.dw.strings().map_err(|e| e.to_string())?.get(*idx as usize, ctx.dw.data()).map_err(|e| e.to_string())?;
            stack.push(fb.const_string(s));
        }
        Operand::Variable { var_ref, instance } => {
            translate_push_variable(inst, fb, stack, locals, ctx, var_ref, *instance)?;
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
fn translate_push_variable(
    inst: &Instruction,
    fb: &mut FunctionBuilder,
    stack: &mut Vec<ValueId>,
    locals: &mut HashMap<String, ValueId>,
    ctx: &TranslateCtx,
    var_ref: &VariableRef,
    instance: i16,
) -> Result<(), String> {
    let var_name = resolve_variable_name(inst, ctx);

    // Handle stacktop-via-ref_type (ref_type == 0x80 with instance >= 0).
    // In pre-GMS2 bytecode, the instance is on the stack rather than encoded
    // as instance type -9. Pop the instance and read the field from it.
    if is_stacktop_ref(var_ref, instance) {
        let target = pop(stack, inst)?;
        let name_val = fb.const_string(&var_name);
        let val = fb.system_call(
            "GameMaker.Instance",
            "getField",
            &[target, name_val],
            Type::Dynamic,
        );
        stack.push(val);
        return Ok(());
    }

    // Handle 2D array access (ref_type == 0 with non-negative instance).
    // The instruction pops 2 indices from the stack (dim1, dim2).
    if is_2d_array_access(var_ref, instance) {
        // Stack layout: [dim2, dim1] with dim1 on top.
        // dim1 is the meaningful first-dimension index.
        let dim1 = pop(stack, inst)?; // first-dimension index (top of stack)
        let _dim2 = pop(stack, inst)?; // second-dimension (ignored for 1D)
        if var_name == "argument" {
            // argument[N] → function parameter access
            if let Some(Constant::Int(idx)) = fb.try_get_const(dim1) {
                let param_offset = if ctx.has_self { 1 } else { 0 }
                    + if ctx.has_other { 1 } else { 0 };
                let param = fb.param(param_offset + *idx as usize);
                stack.push(param);
            } else {
                // Dynamic index — fall back to getField
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Instance",
                    "getField",
                    &[dim1, name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            }
        } else {
            // Array field access on a specific object.
            // dim1 == -1 means scalar (non-indexed) access.
            let obj_id = if let Some(name) = ctx.obj_names.get(instance as usize) {
                fb.const_string(name)
            } else {
                fb.const_int(instance as i64)
            };
            let name_val = fb.const_string(&var_name);
            let is_scalar = matches!(fb.try_get_const(dim1), Some(Constant::Int(-1)));
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
                // Implicit argument variable → function parameter.
                let param_offset = if ctx.has_self { 1 } else { 0 }
                    + if ctx.has_other { 1 } else { 0 };
                let param = fb.param(param_offset + arg_idx);
                stack.push(param);
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
            let target = pop(stack, inst)?;
            if var_name == "argument" {
                // argument[N] → function parameter access
                if let Some(Constant::Int(idx)) = fb.try_get_const(target) {
                    let param_offset = if ctx.has_self { 1 } else { 0 }
                        + if ctx.has_other { 1 } else { 0 };
                    let param = fb.param(param_offset + *idx as usize);
                    stack.push(param);
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
            // Argument variable: map to function parameter.
            let arg_idx = var_ref.variable_id;
            let param_offset = if ctx.has_self { 1 } else { 0 }
                + if ctx.has_other { 1 } else { 0 };
            let param = fb.param(param_offset + arg_idx as usize);
            stack.push(param);
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
) -> Result<(), String> {
    if let Operand::Variable { var_ref, instance } = &inst.operand {
        let var_name = resolve_variable_name(inst, ctx);

        // Handle stacktop-via-ref_type (ref_type == 0x80 with instance >= 0).
        // In pre-GMS2 bytecode, the instance is on the stack. Pop the
        // instance (top), then pop the value (below), and store.
        if is_stacktop_ref(var_ref, *instance) {
            let target = pop(stack, inst)?; // instance (top of stack)
            let value = pop(stack, inst)?; // value to store (below)
            let name_val = fb.const_string(&var_name);
            fb.system_call(
                "GameMaker.Instance",
                "setField",
                &[target, name_val, value],
                Type::Void,
            );
            return Ok(());
        }

        // Handle 2D array access (ref_type == 0 with non-negative instance).
        // Stack layout: [value, dim2, dim1] with dim1 on top.
        // dim1 is the meaningful first-dimension index; dim2 is the second
        // dimension (or a don't-care value for 1D arrays).
        if is_2d_array_access(var_ref, *instance) {
            let dim1 = pop(stack, inst)?; // first-dimension index (top of stack)
            let _dim2 = pop(stack, inst)?; // second-dimension (ignored for 1D)
            let value = pop(stack, inst)?; // value to store (bottom)
            if var_name == "argument" {
                // argument[N] = value → store to function parameter slot
                if let Some(Constant::Int(idx)) = fb.try_get_const(dim1) {
                    let param_offset = if ctx.has_self { 1 } else { 0 }
                        + if ctx.has_other { 1 } else { 0 };
                    let param = fb.param(param_offset + *idx as usize);
                    let slot = fb.alloc(Type::Dynamic);
                    fb.store(slot, param);
                    fb.store(slot, value);
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
            } else {
                // Array field store on a specific object.
                // dim1 == -1 means scalar (non-indexed) access.
                let obj_id = if let Some(name) = ctx.obj_names.get(*instance as usize) {
                    fb.const_string(name)
                } else {
                    fb.const_int(*instance as i64)
                };
                let name_val = fb.const_string(&var_name);
                let is_scalar = matches!(fb.try_get_const(dim1), Some(Constant::Int(-1)));
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
                    let param = fb.param(param_offset + arg_idx);
                    let slot = fb.alloc(Type::Dynamic);
                    fb.name_value(slot, var_name.clone());
                    fb.store(slot, param);
                    fb.store(slot, value);
                    locals.insert(var_name, slot);
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
                let target = pop(stack, inst)?;
                if var_name == "argument" {
                    // argument[N] = value → store to function parameter slot
                    if let Some(Constant::Int(idx)) = fb.try_get_const(target) {
                        let param_offset = if ctx.has_self { 1 } else { 0 }
                            + if ctx.has_other { 1 } else { 0 };
                        let param = fb.param(param_offset + *idx as usize);
                        let slot = fb.alloc(Type::Dynamic);
                        fb.store(slot, param);
                        fb.store(slot, value);
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
