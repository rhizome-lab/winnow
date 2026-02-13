use std::collections::{BTreeSet, HashMap};

use datawin::bytecode::decode::{self, Instruction, Operand};
use datawin::bytecode::opcode::Opcode;
use datawin::bytecode::types::{ComparisonKind, DataType, InstanceType, VariableRef};
use datawin::chunks::func::CodeLocals;
use datawin::DataWin;
use reincarnate_core::ir::builder::FunctionBuilder;
use reincarnate_core::ir::block::BlockId;
use reincarnate_core::ir::func::Visibility;
use reincarnate_core::ir::inst::CmpKind;
use reincarnate_core::ir::ty::{FunctionSig, Type};
use reincarnate_core::ir::value::ValueId;

/// Context for translating a single code entry.
pub struct TranslateCtx<'a> {
    /// The DataWin file (for string resolution).
    pub dw: &'a DataWin,
    /// FUNC function entries: function_id → resolved name.
    pub function_names: &'a HashMap<u32, String>,
    /// VARI variable entries: variable_id → (name, instance_type).
    pub variables: &'a [(String, i32)],
    /// Code-local variable names: local_index → name.
    pub locals: Option<&'a CodeLocals>,
    /// Whether this is an instance method (has self param).
    pub has_self: bool,
    /// Whether this is a collision event (has other param).
    pub has_other: bool,
    /// Number of declared arguments.
    pub arg_count: u16,
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
    let sig = build_signature(ctx);
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
    for i in 0..ctx.arg_count {
        if let Some(name) = arg_name(ctx, i) {
            fb.name_value(fb.param(param_idx), name);
        }
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
    let locals = allocate_locals(&mut fb, ctx);

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
            &locals,
            ctx,
            &mut terminated,
            &block_entry_depths,
        )?;
    }

    // If the last block wasn't terminated, add a void return.
    if !terminated {
        fb.ret(None);
    }

    Ok(fb.build())
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
    let mut params = Vec::new();
    if ctx.has_self {
        params.push(Type::Dynamic);
    }
    if ctx.has_other {
        params.push(Type::Dynamic);
    }
    for _ in 0..ctx.arg_count {
        params.push(Type::Dynamic);
    }
    FunctionSig {
        params,
        return_ty: Type::Dynamic,
        ..Default::default()
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
) -> HashMap<u32, ValueId> {
    let mut locals = HashMap::new();
    if let Some(code_locals) = ctx.locals {
        for local in &code_locals.locals {
            let slot = fb.alloc(Type::Dynamic);
            locals.insert(local.index, slot);
            if let Ok(name) = local.name.resolve(ctx.dw.data()) {
                fb.name_value(slot, name);
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

/// Resolve a variable reference to its name.
fn resolve_variable_name(var_ref: &VariableRef, ctx: &TranslateCtx) -> String {
    let id = var_ref.variable_id as usize;
    if id < ctx.variables.len() {
        ctx.variables[id].0.clone()
    } else {
        format!("var_{id}")
    }
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

/// Compute the stack effect (pops, pushes) of an instruction.
fn stack_effect(inst: &Instruction) -> (usize, usize) {
    match inst.opcode {
        Opcode::PushI | Opcode::Push | Opcode::PushLoc | Opcode::PushGlb | Opcode::PushBltn => {
            if let Operand::Variable { instance, .. } = &inst.operand {
                if matches!(InstanceType::from_i16(*instance), Some(InstanceType::Stacktop)) {
                    (1, 1)
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
        Opcode::Dup => (0, 1),
        Opcode::Popz => (1, 0),
        Opcode::Pop => {
            if let Operand::Variable { instance, .. } = &inst.operand {
                if matches!(InstanceType::from_i16(*instance), Some(InstanceType::Stacktop)) {
                    (2, 0)
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
    locals: &HashMap<u32, ValueId>,
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
            if let Some(&top) = stack.last() {
                let copied = fb.copy(top);
                stack.push(copied);
            } else {
                return Err(format!("{:#x}: Dup on empty stack", inst.offset));
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
                let func_name = ctx.function_names.get(&function_id).cloned().unwrap_or_else(|| {
                    format!("func_{function_id}")
                });
                let mut args = Vec::with_capacity(argc as usize);
                for _ in 0..argc {
                    args.push(pop(stack, inst)?);
                }
                args.reverse();
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
                args.reverse();
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
    locals: &HashMap<u32, ValueId>,
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
    locals: &HashMap<u32, ValueId>,
    ctx: &TranslateCtx,
    var_ref: &VariableRef,
    instance: i16,
) -> Result<(), String> {
    let var_name = resolve_variable_name(var_ref, ctx);

    match InstanceType::from_i16(instance) {
        Some(InstanceType::Local) => {
            // Local variable: load from alloc slot.
            if let Some(&slot) = locals.get(&var_ref.variable_id) {
                let val = fb.load(slot, Type::Dynamic);
                stack.push(val);
            } else {
                // Fallback: create an on-the-fly alloc.
                let slot = fb.alloc(Type::Dynamic);
                let val = fb.load(slot, Type::Dynamic);
                fb.name_value(slot, var_name);
                stack.push(val);
            }
        }
        Some(InstanceType::Own) | Some(InstanceType::Builtin) => {
            // Self/builtin variable: get_field on self param.
            if ctx.has_self {
                let self_param = fb.param(0);
                let val = fb.get_field(self_param, &var_name, Type::Dynamic);
                stack.push(val);
            } else {
                // Script context: use global ref as fallback.
                let val = fb.global_ref(&var_name, Type::Dynamic);
                stack.push(val);
            }
        }
        Some(InstanceType::Global) => {
            let val = fb.global_ref(&var_name, Type::Dynamic);
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
            let val = fb.get_field(target, &var_name, Type::Dynamic);
            stack.push(val);
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
                let obj_id = fb.const_int(instance as i64);
                let name_val = fb.const_string(&var_name);
                let val = fb.system_call(
                    "GameMaker.Instance",
                    "getOn",
                    &[obj_id, name_val],
                    Type::Dynamic,
                );
                stack.push(val);
            } else {
                // Unknown instance type.
                let val = fb.global_ref(&var_name, Type::Dynamic);
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
    locals: &HashMap<u32, ValueId>,
    ctx: &TranslateCtx,
) -> Result<(), String> {
    if let Operand::Variable { var_ref, instance } = &inst.operand {
        let value = pop(stack, inst)?;
        let var_name = resolve_variable_name(var_ref, ctx);

        match InstanceType::from_i16(*instance) {
            Some(InstanceType::Local) => {
                if let Some(&slot) = locals.get(&var_ref.variable_id) {
                    fb.store(slot, value);
                } else {
                    // Orphan local — create slot.
                    let slot = fb.alloc(Type::Dynamic);
                    fb.name_value(slot, var_name);
                    fb.store(slot, value);
                }
            }
            Some(InstanceType::Own) | Some(InstanceType::Builtin) => {
                if ctx.has_self {
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
                fb.set_field(target, &var_name, value);
            }
            _ => {
                if *instance >= 0 {
                    let obj_id = fb.const_int(*instance as i64);
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
