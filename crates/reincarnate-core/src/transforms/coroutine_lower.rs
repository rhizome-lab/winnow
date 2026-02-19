use std::collections::{HashMap, HashSet, VecDeque};

use crate::entity::{EntityRef, PrimaryMap};
use crate::error::CoreError;
use crate::ir::block::{Block, BlockParam};
use crate::ir::inst::Inst;
use crate::ir::module::StructDef;
use crate::ir::ty::FunctionSig;
use crate::ir::{BlockId, Constant, FuncId, Function, InstId, Module, Op, Type, ValueId, Visibility};
use crate::pipeline::{Transform, TransformResult};

use super::util::{branch_targets, value_operands};

/// Coroutine lowering transform — rewrites coroutine functions into
/// state-machine resume functions and rewrites callers accordingly.
pub struct CoroutineLowering;

/// A yield point discovered during block splitting.
struct YieldPoint {
    /// State index for this yield (1-based; 0 is function entry).
    state_index: u32,
    /// The block containing instructions before the yield.
    pre_block: BlockId,
    /// The block containing instructions after the yield (new block).
    post_block: BlockId,
    /// The value yielded (if any).
    yielded_value: Option<ValueId>,
    /// The ValueId that was the result of the Yield op (resume value).
    resume_value: Option<ValueId>,
}

// ============================================================================
// Phase 1: Split blocks at yield points
// ============================================================================

/// Split all blocks at `Op::Yield` instructions. Returns yield points in
/// discovery order. Modifies the function in-place by creating new blocks.
fn split_at_yields(func: &mut Function) -> Vec<YieldPoint> {
    let mut yield_points = Vec::new();
    let mut state_index = 1u32;

    // Use a worklist so newly created post-yield blocks also get processed.
    let mut worklist: VecDeque<BlockId> = func.blocks.keys().collect();

    while let Some(block_id) = worklist.pop_front() {
        // Find the first yield in this block.
        let yield_pos = func.blocks[block_id]
            .insts
            .iter()
            .position(|&inst_id| matches!(func.insts[inst_id].op, Op::Yield(_)));

        let yield_pos = match yield_pos {
            Some(pos) => pos,
            None => continue,
        };

        let yield_inst_id = func.blocks[block_id].insts[yield_pos];
        let yielded_value = match &func.insts[yield_inst_id].op {
            Op::Yield(v) => *v,
            _ => unreachable!(),
        };
        let resume_value = func.insts[yield_inst_id].result;

        // Split: post-yield instructions go to a new block.
        let post_insts: Vec<InstId> =
            func.blocks[block_id].insts[yield_pos + 1..].to_vec();

        // Truncate current block to before the yield (exclude yield itself).
        func.blocks[block_id].insts.truncate(yield_pos);

        // Create post-yield block. If the yield had a result (resume value),
        // make it a block parameter.
        let mut post_params = Vec::new();
        let new_resume_value = if let Some(rv) = resume_value {
            let ty = func.value_types[rv].clone();
            let new_val = func.value_types.push(ty.clone());
            post_params.push(BlockParam {
                value: new_val,
                ty,
            });
            Some(new_val)
        } else {
            None
        };

        let post_block = func.blocks.push(Block {
            params: post_params,
            insts: post_insts,
        });

        // If there was a resume value, substitute the old ValueId with the
        // new block param in all instructions of the post block.
        if let (Some(old_rv), Some(new_rv)) = (resume_value, new_resume_value) {
            let mut subst = HashMap::new();
            subst.insert(old_rv, new_rv);
            for &inst_id in &func.blocks[post_block].insts {
                super::util::substitute_values_in_op(
                    &mut func.insts[inst_id].op,
                    &subst,
                );
            }
        }

        yield_points.push(YieldPoint {
            state_index,
            pre_block: block_id,
            post_block,
            yielded_value,
            resume_value: new_resume_value,
        });

        state_index += 1;

        // The post block may contain more yields — add it to the worklist.
        worklist.push_back(post_block);
    }

    yield_points
}

// ============================================================================
// Phase 2: Cross-yield liveness
// ============================================================================

/// Build a map from ValueId → BlockId where the value is defined.
fn build_def_map(func: &Function) -> HashMap<ValueId, BlockId> {
    let mut def_map = HashMap::new();
    for block_id in func.blocks.keys() {
        // Block params are defined in this block.
        for param in &func.blocks[block_id].params {
            def_map.insert(param.value, block_id);
        }
        // Instruction results are defined in this block.
        for &inst_id in &func.blocks[block_id].insts {
            if let Some(result) = func.insts[inst_id].result {
                def_map.insert(result, block_id);
            }
        }
    }
    def_map
}

/// Find all blocks reachable from `start` via BFS.
fn reachable_from(func: &Function, start: BlockId) -> HashSet<BlockId> {
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    visited.insert(start);
    queue.push_back(start);
    while let Some(bid) = queue.pop_front() {
        for &inst_id in &func.blocks[bid].insts {
            for target in branch_targets(&func.insts[inst_id].op) {
                if visited.insert(target) {
                    queue.push_back(target);
                }
            }
        }
    }
    visited
}

/// Collect all ValueIds used in a set of blocks.
fn values_used_in_blocks(func: &Function, blocks: &HashSet<BlockId>) -> HashSet<ValueId> {
    let mut used = HashSet::new();
    for &bid in blocks {
        for &inst_id in &func.blocks[bid].insts {
            for v in value_operands(&func.insts[inst_id].op) {
                used.insert(v);
            }
        }
    }
    used
}

/// Compute values that are live across at least one yield point.
/// These need to be saved to the state struct.
fn cross_yield_live_values(
    func: &Function,
    yield_points: &[YieldPoint],
) -> Vec<ValueId> {
    let def_map = build_def_map(func);
    let mut live_set = HashSet::new();

    for yp in yield_points {
        // Blocks reachable from the post-yield block.
        let post_reachable = reachable_from(func, yp.post_block);
        // Values used in those blocks.
        let used_after = values_used_in_blocks(func, &post_reachable);

        // Blocks reachable from entry up to (and including) the pre-yield block.
        // We approximate: a value is "defined before" if its def block is NOT
        // in the post-reachable set (i.e., it was defined before the yield).
        for v in &used_after {
            if let Some(&def_block) = def_map.get(v) {
                if !post_reachable.contains(&def_block) {
                    live_set.insert(*v);
                }
            }
        }
    }

    let mut result: Vec<ValueId> = live_set.into_iter().collect();
    result.sort_by_key(|v| v.index());
    result
}

// ============================================================================
// Phase 3: Generate state struct
// ============================================================================

/// Generate a struct definition for the coroutine state.
fn generate_state_struct(
    func: &Function,
    live_values: &[ValueId],
    struct_name: &str,
) -> (StructDef, Vec<(String, Type, Option<crate::ir::Constant>)>) {
    let mut fields = Vec::new();

    // Internal state fields.
    fields.push(("__state".to_string(), Type::UInt(32), None));
    fields.push(("__done".to_string(), Type::Bool, None));

    // Original function parameters.
    for (i, param) in func.blocks[func.entry].params.iter().enumerate() {
        fields.push((format!("__p{i}"), param.ty.clone(), None));
    }

    // Cross-yield live values.
    for (i, &val) in live_values.iter().enumerate() {
        fields.push((format!("__v{i}"), func.value_types[val].clone(), None));
    }

    let struct_def = StructDef {
        name: struct_name.to_string(),
        namespace: Vec::new(),
        fields: fields.clone(),
        visibility: Visibility::Private,
    };

    (struct_def, fields)
}

// ============================================================================
// Phase 4: Build resume function
// ============================================================================

struct ResumeBuilder {
    blocks: PrimaryMap<BlockId, Block>,
    insts: PrimaryMap<InstId, Inst>,
    value_types: PrimaryMap<ValueId, Type>,
}

impl ResumeBuilder {
    fn new() -> Self {
        Self {
            blocks: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            value_types: PrimaryMap::new(),
        }
    }

    fn alloc_value(&mut self, ty: Type) -> ValueId {
        self.value_types.push(ty)
    }

    fn emit(&mut self, block: BlockId, op: Op, ty: Type) -> ValueId {
        let value = self.alloc_value(ty);
        let inst_id = self.insts.push(Inst {
            op,
            result: Some(value),
            span: None,
        });
        self.blocks[block].insts.push(inst_id);
        value
    }

    fn emit_void(&mut self, block: BlockId, op: Op) {
        let inst_id = self.insts.push(Inst {
            op,
            result: None,
            span: None,
        });
        self.blocks[block].insts.push(inst_id);
    }

    fn create_block(&mut self) -> BlockId {
        self.blocks.push(Block {
            params: Vec::new(),
            insts: Vec::new(),
        })
    }
}

/// Build the resume function body that replaces the original coroutine function.
fn build_resume_function(
    orig_func: &Function,
    yield_points: &[YieldPoint],
    live_values: &[ValueId],
    struct_name: &str,
    yield_ty: &Type,
) -> Function {
    let mut rb = ResumeBuilder::new();

    // New function signature: fn(state: Struct, resume_val: yield_ty) -> yield_ty
    let state_ty = Type::Struct(struct_name.to_string());
    let new_sig = FunctionSig {
        params: vec![state_ty.clone(), yield_ty.clone()],
        return_ty: yield_ty.clone(), ..Default::default() };

    // Create entry block with params: state, resume_val.
    let state_param = rb.alloc_value(state_ty.clone());
    let resume_param = rb.alloc_value(yield_ty.clone());
    let entry = rb.blocks.push(Block {
        params: vec![
            BlockParam {
                value: state_param,
                ty: state_ty.clone(),
            },
            BlockParam {
                value: resume_param,
                ty: yield_ty.clone(),
            },
        ],
        insts: Vec::new(),
    });

    // Create state dispatch blocks (one per state).
    let num_states = yield_points.len() + 1; // state 0 = entry, states 1..N = post-yield
    let mut state_blocks = Vec::with_capacity(num_states);
    for _ in 0..num_states {
        state_blocks.push(rb.create_block());
    }

    // Create done block.
    let done_block = rb.create_block();

    // Entry: read __state, switch on it.
    let state_field = rb.emit(
        entry,
        Op::GetField {
            object: state_param,
            field: "__state".to_string(),
        },
        Type::UInt(32),
    );

    let cases: Vec<(Constant, BlockId, Vec<ValueId>)> = (0..num_states as u64)
        .map(|i| (Constant::UInt(i), state_blocks[i as usize], vec![]))
        .collect();
    rb.emit_void(
        entry,
        Op::Switch {
            value: state_field,
            cases,
            default: (done_block, vec![]),
        },
    );

    // Build a map from original ValueId → field name for cross-yield values.
    let mut live_value_field: HashMap<ValueId, String> = HashMap::new();
    for (i, &val) in live_values.iter().enumerate() {
        live_value_field.insert(val, format!("__v{i}"));
    }

    let exit_ctx = ExitCtx {
        orig_func,
        yield_points,
        live_value_field: &live_value_field,
        state_param,
        state_blocks: &state_blocks,
    };

    // State 0: Load params from struct, copy original entry block instructions.
    {
        let sb = state_blocks[0];

        // Load params from struct into new ValueIds.
        let mut subst: HashMap<ValueId, ValueId> = HashMap::new();
        for (i, param) in orig_func.blocks[orig_func.entry].params.iter().enumerate() {
            let loaded = rb.emit(
                sb,
                Op::GetField {
                    object: state_param,
                    field: format!("__p{i}"),
                },
                param.ty.clone(),
            );
            subst.insert(param.value, loaded);
        }

        // Copy instructions from original entry block (after yield splitting,
        // these are the instructions before the first yield).
        copy_instructions_with_subst(
            &orig_func.insts,
            &orig_func.blocks[orig_func.entry].insts,
            &mut rb,
            sb,
            &mut subst,
        );

        emit_state_exit(
            &exit_ctx,
            &orig_func.blocks[orig_func.entry].insts,
            &mut rb,
            sb,
            &subst,
        );
    }

    // States 1..N: Post-yield blocks.
    for yp in yield_points {
        let sb = state_blocks[yp.state_index as usize];

        let mut subst: HashMap<ValueId, ValueId> = HashMap::new();

        // Restore cross-yield live values from struct.
        for &val in live_values {
            let field_name = &live_value_field[&val];
            let ty = orig_func.value_types[val].clone();
            let loaded = rb.emit(
                sb,
                Op::GetField {
                    object: state_param,
                    field: field_name.clone(),
                },
                ty,
            );
            subst.insert(val, loaded);
        }

        // Also restore params (they might be used after yield).
        let params_to_load: Vec<(usize, ValueId, Type)> = orig_func.blocks[orig_func.entry]
            .params
            .iter()
            .enumerate()
            .filter(|(_, param)| !subst.contains_key(&param.value))
            .map(|(i, param)| (i, param.value, param.ty.clone()))
            .collect();
        for (i, param_val, ty) in params_to_load {
            let loaded = rb.emit(
                sb,
                Op::GetField {
                    object: state_param,
                    field: format!("__p{i}"),
                },
                ty,
            );
            subst.insert(param_val, loaded);
        }

        // The resume value replaces the yield's result.
        if let Some(rv) = yp.resume_value {
            subst.insert(rv, resume_param);
        }

        // Copy post-yield block instructions.
        copy_instructions_with_subst(
            &orig_func.insts,
            &orig_func.blocks[yp.post_block].insts,
            &mut rb,
            sb,
            &mut subst,
        );

        emit_state_exit(
            &exit_ctx,
            &orig_func.blocks[yp.post_block].insts,
            &mut rb,
            sb,
            &subst,
        );
    }

    // Done block: return null.
    let null_val = rb.emit(
        done_block,
        Op::Const(Constant::Null),
        Type::Option(Box::new(Type::Dynamic)),
    );
    rb.emit_void(done_block, Op::Return(Some(null_val)));

    Function {
        name: orig_func.name.clone(),
        sig: new_sig,
        visibility: orig_func.visibility,
        namespace: orig_func.namespace.clone(),
        class: orig_func.class.clone(),
        method_kind: orig_func.method_kind,
        blocks: rb.blocks,
        insts: rb.insts,
        value_types: rb.value_types,
        entry,
        coroutine: None, // No longer a coroutine after lowering.
        value_names: std::collections::HashMap::new(),
        capture_params: Vec::new(),
    }
}

/// Copy instructions from `inst_ids` into the resume builder, applying value
/// substitution. Updates `subst` with new mappings for instruction results.
///
/// Skips terminal instructions (Return, Br, BrIf, Switch, Yield) — those are
/// handled separately by `emit_state_exit`.
fn copy_instructions_with_subst(
    orig_insts: &PrimaryMap<InstId, Inst>,
    inst_ids: &[InstId],
    rb: &mut ResumeBuilder,
    target_block: BlockId,
    subst: &mut HashMap<ValueId, ValueId>,
) {
    for &inst_id in inst_ids {
        let inst = &orig_insts[inst_id];

        // Skip terminators and yields — handled by emit_state_exit.
        if matches!(
            inst.op,
            Op::Return(_) | Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } | Op::Yield(_)
        ) {
            continue;
        }

        let mut new_op = inst.op.clone();
        super::util::substitute_values_in_op(&mut new_op, subst);

        if let Some(result) = inst.result {
            let ty = rb.value_types.get(result).cloned().unwrap_or(Type::Dynamic);
            // Use the original result's type if available in subst chain, otherwise Dynamic.
            let new_val = rb.emit(target_block, new_op, ty);
            subst.insert(result, new_val);
        } else {
            rb.emit_void(target_block, new_op);
        }
    }
}

/// Context for state-exit emission, bundled to avoid too-many-arguments.
struct ExitCtx<'a> {
    orig_func: &'a Function,
    yield_points: &'a [YieldPoint],
    live_value_field: &'a HashMap<ValueId, String>,
    state_param: ValueId,
    state_blocks: &'a [BlockId],
}

/// Emit the exit sequence for a state block. Looks at the last instruction(s)
/// of the original block to determine what to emit:
/// - Yield: save live values, update __state, return yielded value
/// - Return: set __done = true, return null
/// - Br/BrIf/Switch: intra-coroutine branches remapped to state blocks
fn emit_state_exit(
    ctx: &ExitCtx<'_>,
    orig_inst_ids: &[InstId],
    rb: &mut ResumeBuilder,
    target_block: BlockId,
    subst: &HashMap<ValueId, ValueId>,
) {
    if let Some(&last_inst_id) = orig_inst_ids.last() {
        let last_op = &ctx.orig_func.insts[last_inst_id].op;
        match last_op {
            Op::Return(_) => {
                // Set __done = true.
                let true_val = rb.emit(
                    target_block,
                    Op::Const(Constant::Bool(true)),
                    Type::Bool,
                );
                rb.emit_void(
                    target_block,
                    Op::SetField {
                        object: ctx.state_param,
                        field: "__done".to_string(),
                        value: true_val,
                    },
                );
                let null_val = rb.emit(
                    target_block,
                    Op::Const(Constant::Null),
                    Type::Option(Box::new(Type::Dynamic)),
                );
                rb.emit_void(target_block, Op::Return(Some(null_val)));
            }
            Op::Br { target, args } => {
                let mapped_target = map_block_to_state(
                    *target,
                    ctx.yield_points,
                    ctx.orig_func.entry,
                    ctx.state_blocks,
                );
                let mut new_args = args.clone();
                for a in &mut new_args {
                    if let Some(&new_v) = subst.get(a) {
                        *a = new_v;
                    }
                }
                rb.emit_void(
                    target_block,
                    Op::Br {
                        target: mapped_target,
                        args: new_args,
                    },
                );
            }
            Op::BrIf {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } => {
                let new_cond = subst.get(cond).copied().unwrap_or(*cond);
                let mapped_then = map_block_to_state(
                    *then_target,
                    ctx.yield_points,
                    ctx.orig_func.entry,
                    ctx.state_blocks,
                );
                let mapped_else = map_block_to_state(
                    *else_target,
                    ctx.yield_points,
                    ctx.orig_func.entry,
                    ctx.state_blocks,
                );
                let mut new_then_args = then_args.clone();
                let mut new_else_args = else_args.clone();
                for a in &mut new_then_args {
                    if let Some(&new_v) = subst.get(a) {
                        *a = new_v;
                    }
                }
                for a in &mut new_else_args {
                    if let Some(&new_v) = subst.get(a) {
                        *a = new_v;
                    }
                }
                rb.emit_void(
                    target_block,
                    Op::BrIf {
                        cond: new_cond,
                        then_target: mapped_then,
                        then_args: new_then_args,
                        else_target: mapped_else,
                        else_args: new_else_args,
                    },
                );
            }
            _ => {
                emit_yield_exit(ctx, orig_inst_ids, rb, target_block, subst);
            }
        }
    } else {
        emit_yield_exit(ctx, orig_inst_ids, rb, target_block, subst);
    }
}

/// Emit the yield-exit sequence: save cross-yield values, update __state, return yielded value.
fn emit_yield_exit(
    ctx: &ExitCtx<'_>,
    orig_inst_ids: &[InstId],
    rb: &mut ResumeBuilder,
    target_block: BlockId,
    subst: &HashMap<ValueId, ValueId>,
) {
    let orig_func = ctx.orig_func;
    let yield_points = ctx.yield_points;

    // Look for a yield point whose pre_block's insts match our inst_ids.
    let yp = yield_points.iter().find(|yp| {
        let pre_insts = &orig_func.blocks[yp.pre_block].insts;
        std::ptr::eq(pre_insts.as_slice(), orig_inst_ids)
            || std::ptr::eq(
                orig_func.blocks[yp.post_block].insts.as_slice(),
                orig_inst_ids,
            )
    });

    // Fallback: match by equality.
    let yp = yp.or_else(|| {
        yield_points.iter().find(|yp| {
            orig_func.blocks[yp.pre_block].insts.as_slice() == orig_inst_ids
        })
    });

    if let Some(yp) = yp {
        // Save cross-yield live values to struct.
        for (&val, field_name) in ctx.live_value_field {
            let resolved = subst.get(&val).copied().unwrap_or(val);
            rb.emit_void(
                target_block,
                Op::SetField {
                    object: ctx.state_param,
                    field: field_name.clone(),
                    value: resolved,
                },
            );
        }

        // Update __state to the next state.
        let next_state = rb.emit(
            target_block,
            Op::Const(Constant::UInt(yp.state_index as u64)),
            Type::UInt(32),
        );
        rb.emit_void(
            target_block,
            Op::SetField {
                object: ctx.state_param,
                field: "__state".to_string(),
                value: next_state,
            },
        );

        // Return the yielded value.
        let ret_val = if let Some(yv) = yp.yielded_value {
            subst.get(&yv).copied().unwrap_or(yv)
        } else {
            rb.emit(
                target_block,
                Op::Const(Constant::Null),
                Type::Option(Box::new(Type::Dynamic)),
            )
        };
        rb.emit_void(target_block, Op::Return(Some(ret_val)));
    } else {
        // No matching yield point — return null as a safety fallback.
        let null_val = rb.emit(
            target_block,
            Op::Const(Constant::Null),
            Type::Option(Box::new(Type::Dynamic)),
        );
        rb.emit_void(target_block, Op::Return(Some(null_val)));
    }
}

/// Map an original block ID to the corresponding state block in the resume function.
fn map_block_to_state(
    orig_block: BlockId,
    yield_points: &[YieldPoint],
    entry: BlockId,
    state_blocks: &[BlockId],
) -> BlockId {
    if orig_block == entry {
        return state_blocks[0];
    }
    for yp in yield_points {
        if orig_block == yp.post_block {
            return state_blocks[yp.state_index as usize];
        }
    }
    // Block doesn't correspond to a state — this shouldn't happen after
    // proper yield splitting, but return the first state as fallback.
    state_blocks[0]
}

// ============================================================================
// Phase 5: Rewrite callers
// ============================================================================

/// Rewrite CoroutineCreate and CoroutineResume ops across the module.
fn rewrite_callers(
    module: &mut Module,
    coroutine_func_id: FuncId,
    struct_name: &str,
    func_name: &str,
) {
    let struct_ty = Type::Struct(struct_name.to_string());

    for func_id in module.functions.keys().collect::<Vec<_>>() {
        if func_id == coroutine_func_id {
            continue; // Skip the coroutine function itself.
        }

        let inst_ids: Vec<InstId> = module.functions[func_id].insts.keys().collect();
        for inst_id in inst_ids {
            let inst = &module.functions[func_id].insts[inst_id];
            match &inst.op {
                Op::CoroutineCreate { func, args } if func == func_name => {
                    // Replace with StructInit.
                    let args = args.clone();
                    let mut fields = vec![];

                    // __state = 0
                    let state_val = module.functions[func_id].value_types.push(Type::UInt(32));
                    let state_inst_id = module.functions[func_id].insts.push(Inst {
                        op: Op::Const(Constant::UInt(0)),
                        result: Some(state_val),
                        span: None,
                    });
                    fields.push(("__state".to_string(), state_val));

                    // __done = false
                    let done_val = module.functions[func_id].value_types.push(Type::Bool);
                    let done_inst_id = module.functions[func_id].insts.push(Inst {
                        op: Op::Const(Constant::Bool(false)),
                        result: Some(done_val),
                        span: None,
                    });
                    fields.push(("__done".to_string(), done_val));

                    // __p0, __p1, ... from args
                    for (i, &arg) in args.iter().enumerate() {
                        fields.push((format!("__p{i}"), arg));
                    }

                    // Insert the const instructions before the CoroutineCreate.
                    // Find which block contains this inst_id and insert before it.
                    let block_id = find_inst_block(&module.functions[func_id], inst_id);
                    if let Some(block_id) = block_id {
                        let pos = module.functions[func_id].blocks[block_id]
                            .insts
                            .iter()
                            .position(|&id| id == inst_id)
                            .unwrap();
                        module.functions[func_id].blocks[block_id]
                            .insts
                            .insert(pos, done_inst_id);
                        module.functions[func_id].blocks[block_id]
                            .insts
                            .insert(pos, state_inst_id);
                    }

                    // Replace the CoroutineCreate with StructInit.
                    module.functions[func_id].insts[inst_id].op = Op::StructInit {
                        name: struct_name.to_string(),
                        fields,
                    };

                    // Update the result type.
                    if let Some(result) = module.functions[func_id].insts[inst_id].result {
                        module.functions[func_id].value_types[result] = struct_ty.clone();
                    }
                }
                Op::CoroutineResume(coroutine_val) => {
                    let coroutine_val = *coroutine_val;
                    // Check if this resumes a coroutine of the right type.
                    let val_ty = &module.functions[func_id].value_types[coroutine_val];
                    let is_our_coroutine = matches!(val_ty, Type::Coroutine { .. })
                        || *val_ty == struct_ty;

                    if is_our_coroutine {
                        // Replace with Call to the resume function.
                        let null_val =
                            module.functions[func_id].value_types.push(
                                Type::Option(Box::new(Type::Dynamic)),
                            );
                        let null_inst_id = module.functions[func_id].insts.push(Inst {
                            op: Op::Const(Constant::Null),
                            result: Some(null_val),
                            span: None,
                        });

                        // Insert null const before the resume.
                        let block_id =
                            find_inst_block(&module.functions[func_id], inst_id);
                        if let Some(block_id) = block_id {
                            let pos = module.functions[func_id].blocks[block_id]
                                .insts
                                .iter()
                                .position(|&id| id == inst_id)
                                .unwrap();
                            module.functions[func_id].blocks[block_id]
                                .insts
                                .insert(pos, null_inst_id);
                        }

                        // Replace CoroutineResume with Call.
                        module.functions[func_id].insts[inst_id].op = Op::Call {
                            func: func_name.to_string(),
                            args: vec![coroutine_val, null_val],
                        };
                    }
                }
                _ => {}
            }
        }
    }
}

/// Find which block contains an instruction.
fn find_inst_block(func: &Function, target_inst: InstId) -> Option<BlockId> {
    func.blocks
        .keys()
        .find(|&block_id| func.blocks[block_id].insts.contains(&target_inst))
}

// ============================================================================
// Transform implementation
// ============================================================================

impl Transform for CoroutineLowering {
    fn name(&self) -> &str {
        "coroutine-lowering"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;

        // Collect coroutine function IDs first.
        let coroutine_funcs: Vec<(FuncId, String)> = module
            .functions
            .iter()
            .filter_map(|(id, func)| {
                func.coroutine.as_ref().map(|_| (id, func.name.clone()))
            })
            .collect();

        for (func_id, func_name) in &coroutine_funcs {
            let func = &module.functions[*func_id];
            let coroutine_info = func.coroutine.clone().unwrap();
            let struct_name = format!("{}_state", func.name);

            // Phase 1: Split blocks at yield points.
            let mut func_clone = func.clone();
            let yield_points = split_at_yields(&mut func_clone);

            if yield_points.is_empty() {
                // No yields — clear coroutine flag but don't rewrite.
                module.functions[*func_id].coroutine = None;
                changed = true;
                continue;
            }

            // Phase 2: Cross-yield liveness.
            let live_values = cross_yield_live_values(&func_clone, &yield_points);

            // Phase 3: Generate state struct.
            let (struct_def, _fields) =
                generate_state_struct(&func_clone, &live_values, &struct_name);
            module.structs.push(struct_def);

            // Phase 4: Build resume function.
            let new_func = build_resume_function(
                &func_clone,
                &yield_points,
                &live_values,
                &struct_name,
                &coroutine_info.yield_ty,
            );
            module.functions[*func_id] = new_func;

            // Phase 5: Rewrite callers.
            rewrite_callers(
                &mut module,
                *func_id,
                &struct_name,
                func_name,
            );

            changed = true;
        }

        Ok(TransformResult { module, changed })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityRef;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::coroutine::CoroutineInfo;

    fn apply_lowering(module: Module) -> (Module, bool) {
        let result = CoroutineLowering.apply(module).unwrap();
        (result.module, result.changed)
    }

    /// Helper: build a simple coroutine function that yields once then returns.
    fn build_simple_yield_module() -> Module {
        // Coroutine: fn gen() yields Int(64), returns Void
        //   entry: yield 42; return
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen", sig, Visibility::Public);
        let val = fb.const_int(42);
        let _resume = fb.yield_(Some(val), Type::Dynamic);
        fb.ret(None);
        let mut func = fb.build();
        func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        mb.build()
    }

    // ---- Identity & idempotency tests ----

    /// Regular function (no yields) → no changes.
    #[test]
    fn identity_no_change() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("normal", sig, Visibility::Private);
        let p = fb.param(0);
        fb.ret(Some(p));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = CoroutineLowering.apply(module).unwrap();
        assert!(!result.changed);
    }

    /// Coroutine lowering is idempotent (lowered function has no Yield ops).
    #[test]
    fn idempotent_after_transform() {
        let module = build_simple_yield_module();
        let (module, changed) = apply_lowering(module);
        assert!(changed);
        let result = CoroutineLowering.apply(module).unwrap();
        assert!(!result.changed, "second apply should report no changes");
    }

    /// Simple yield: one yield then return → 2-state resume function, state struct created.
    #[test]
    fn simple_yield() {
        let module = build_simple_yield_module();
        let (module, changed) = apply_lowering(module);

        assert!(changed);

        // State struct should be created.
        assert_eq!(module.structs.len(), 1);
        assert_eq!(module.structs[0].name, "gen_state");
        // Should have at least __state and __done fields.
        let field_names: Vec<&str> = module.structs[0]
            .fields
            .iter()
            .map(|(name, _, _)| name.as_str())
            .collect();
        assert!(field_names.contains(&"__state"));
        assert!(field_names.contains(&"__done"));

        // Function should no longer be a coroutine.
        let func = &module.functions[FuncId::new(0)];
        assert!(func.coroutine.is_none());

        // Function should have new signature: (state, resume_val) -> yield_ty.
        assert_eq!(func.sig.params.len(), 2);
        assert_eq!(func.sig.params[0], Type::Struct("gen_state".to_string()));
        assert_eq!(func.sig.return_ty, Type::Int(64));

        // Should have a switch instruction in the entry block.
        let entry = func.entry;
        let has_switch = func.blocks[entry]
            .insts
            .iter()
            .any(|&id| matches!(func.insts[id].op, Op::Switch { .. }));
        assert!(has_switch, "entry should have a state dispatch switch");
    }

    /// Multiple yields: two yields → 3 states.
    #[test]
    fn multiple_yields() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen2", sig, Visibility::Public);
        let v1 = fb.const_int(1);
        let _r1 = fb.yield_(Some(v1), Type::Dynamic);
        let v2 = fb.const_int(2);
        let _r2 = fb.yield_(Some(v2), Type::Dynamic);
        fb.ret(None);
        let mut func = fb.build();
        func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let (module, changed) = apply_lowering(module);
        assert!(changed);

        // Check the switch has 3 cases (states 0, 1, 2).
        let func = &module.functions[FuncId::new(0)];
        let entry = func.entry;
        let switch_inst = func.blocks[entry]
            .insts
            .iter()
            .find(|&&id| matches!(func.insts[id].op, Op::Switch { .. }));
        assert!(switch_inst.is_some());
        if let Op::Switch { cases, .. } = &func.insts[*switch_inst.unwrap()].op {
            assert_eq!(cases.len(), 3, "should have 3 states for 2 yields");
        }
    }

    /// Yield in a loop: yield inside a loop → live values saved/restored.
    #[test]
    fn yield_in_loop() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen_loop", sig, Visibility::Public);
        let counter = fb.param(0);

        let loop_block = fb.create_block();
        let exit_block = fb.create_block();

        fb.br(loop_block, &[]);

        // Loop body: yield counter, then branch back.
        fb.switch_to_block(loop_block);
        let _resume = fb.yield_(Some(counter), Type::Dynamic);
        // Use counter after yield — it should be saved as a cross-yield live value.
        let one = fb.const_int(1);
        let _next = fb.add(counter, one);
        fb.br(exit_block, &[]);

        fb.switch_to_block(exit_block);
        fb.ret(None);

        let mut func = fb.build();
        func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let (module, changed) = apply_lowering(module);
        assert!(changed);

        // State struct should have saved values (__v0, etc.) or at least __p0.
        let struct_def = &module.structs[0];
        let field_names: Vec<&str> = struct_def
            .fields
            .iter()
            .map(|(name, _, _)| name.as_str())
            .collect();
        assert!(
            field_names.contains(&"__p0"),
            "should save function param: {:?}",
            field_names
        );
    }

    /// CoroutineCreate rewritten: caller's create op becomes struct_init.
    #[test]
    fn coroutine_create_rewritten() {
        // Build a coroutine function.
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen", sig, Visibility::Public);
        let p = fb.param(0);
        let _r = fb.yield_(Some(p), Type::Dynamic);
        fb.ret(None);
        let mut gen_func = fb.build();
        gen_func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        // Build a caller function.
        let caller_sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb2 = FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let arg = fb2.const_int(10);
        let _coro = fb2.coroutine_create("gen", &[arg], Type::Int(64), Type::Void);
        fb2.ret(None);
        let caller_func = fb2.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(gen_func);
        mb.add_function(caller_func);
        let module = mb.build();

        let (module, _) = apply_lowering(module);

        // The caller should now have a StructInit instead of CoroutineCreate.
        let caller = &module.functions[FuncId::new(1)];
        let has_struct_init = caller.insts.values().any(|inst| {
            matches!(&inst.op, Op::StructInit { name, .. } if name == "gen_state")
        });
        assert!(
            has_struct_init,
            "caller should have StructInit for gen_state"
        );
        let has_coroutine_create = caller
            .insts
            .values()
            .any(|inst| matches!(&inst.op, Op::CoroutineCreate { .. }));
        assert!(
            !has_coroutine_create,
            "caller should not have CoroutineCreate after lowering"
        );
    }

    /// CoroutineResume rewritten: caller's resume op becomes function call.
    #[test]
    fn coroutine_resume_rewritten() {
        // Build a coroutine function.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen", sig, Visibility::Public);
        let v = fb.const_int(1);
        let _r = fb.yield_(Some(v), Type::Dynamic);
        fb.ret(None);
        let mut gen_func = fb.build();
        gen_func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        // Build a caller that creates and resumes.
        let caller_sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb2 = FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let coro = fb2.coroutine_create("gen", &[], Type::Int(64), Type::Void);
        let result = fb2.coroutine_resume(coro, Type::Int(64));
        fb2.ret(Some(result));
        let caller_func = fb2.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(gen_func);
        mb.add_function(caller_func);
        let module = mb.build();

        let (module, _) = apply_lowering(module);

        // The caller should now have a Call instead of CoroutineResume.
        let caller = &module.functions[FuncId::new(1)];
        let has_call = caller.insts.values().any(|inst| {
            matches!(&inst.op, Op::Call { func, .. } if func == "gen")
        });
        assert!(has_call, "caller should have Call to gen");
        let has_resume = caller
            .insts
            .values()
            .any(|inst| matches!(&inst.op, Op::CoroutineResume(_)));
        assert!(
            !has_resume,
            "caller should not have CoroutineResume after lowering"
        );
    }

    /// Non-coroutine unchanged: regular functions pass through.
    #[test]
    fn non_coroutine_unchanged() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("normal", sig, Visibility::Private);
        fb.ret(None);
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let (module, changed) = apply_lowering(module);
        assert!(!changed);
        assert!(module.structs.is_empty());
    }

    // ---- Edge case tests ----

    /// No coroutine → no-op, changed == false.
    #[test]
    fn no_coroutine_noop() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("normal", sig, Visibility::Private);
        let p = fb.param(0);
        fb.ret(Some(p));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let (_, changed) = apply_lowering(module);
        assert!(!changed);
    }

    /// Yield as first instruction in entry block.
    #[test]
    fn yield_in_entry_block() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen", sig, Visibility::Public);
        let val = fb.const_int(99);
        let _r = fb.yield_(Some(val), Type::Dynamic);
        fb.ret(None);
        let mut func = fb.build();
        func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let (module, changed) = apply_lowering(module);
        assert!(changed);
        // Function should still have a valid switch dispatch.
        let func = &module.functions[FuncId::new(0)];
        let entry = func.entry;
        let has_switch = func.blocks[entry].insts.iter()
            .any(|&id| matches!(func.insts[id].op, Op::Switch { .. }));
        assert!(has_switch, "entry should have state dispatch");
    }

    // ---- Adversarial tests ----

    /// Yield in both branches of an if.
    #[test]
    fn yield_in_both_branches() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen", sig, Visibility::Public);
        let cond = fb.param(0);
        let then_b = fb.create_block();
        let else_b = fb.create_block();
        let merge = fb.create_block();

        fb.br_if(cond, then_b, &[], else_b, &[]);

        fb.switch_to_block(then_b);
        let v1 = fb.const_int(1);
        let _r1 = fb.yield_(Some(v1), Type::Dynamic);
        fb.br(merge, &[]);

        fb.switch_to_block(else_b);
        let v2 = fb.const_int(2);
        let _r2 = fb.yield_(Some(v2), Type::Dynamic);
        fb.br(merge, &[]);

        fb.switch_to_block(merge);
        fb.ret(None);

        let mut func = fb.build();
        func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let (module, changed) = apply_lowering(module);
        assert!(changed);
        // Should have 3 states (0=entry, 1=yield-in-then, 2=yield-in-else).
        let func = &module.functions[FuncId::new(0)];
        let entry = func.entry;
        let switch_inst = func.blocks[entry].insts.iter()
            .find(|&&id| matches!(func.insts[id].op, Op::Switch { .. }));
        assert!(switch_inst.is_some());
        if let Op::Switch { cases, .. } = &func.insts[*switch_inst.unwrap()].op {
            assert_eq!(cases.len(), 3, "2 yields → 3 states");
        }
    }

    /// Three yields in a row.
    #[test]
    fn multiple_yields_sequence() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen", sig, Visibility::Public);
        for i in 1..=3 {
            let v = fb.const_int(i);
            let _r = fb.yield_(Some(v), Type::Dynamic);
        }
        fb.ret(None);
        let mut func = fb.build();
        func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let (module, changed) = apply_lowering(module);
        assert!(changed);
        // 4 states: 0=entry, 1=after-yield1, 2=after-yield2, 3=after-yield3
        let func = &module.functions[FuncId::new(0)];
        let entry = func.entry;
        let switch_inst = func.blocks[entry].insts.iter()
            .find(|&&id| matches!(func.insts[id].op, Op::Switch { .. }));
        if let Op::Switch { cases, .. } = &func.insts[*switch_inst.unwrap()].op {
            assert_eq!(cases.len(), 4, "3 yields → 4 states");
        }
    }

    /// Cross-yield liveness: value defined before yield, used after → saved to struct field.
    #[test]
    fn cross_yield_liveness() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("gen", sig, Visibility::Public);

        // Define a value before yield.
        let val = fb.const_int(42);
        let _r = fb.yield_(Some(val), Type::Dynamic);
        // Use val after yield — it must be saved.
        let one = fb.const_int(1);
        let _sum = fb.add(val, one);
        fb.ret(None);

        let mut func = fb.build();
        func.coroutine = Some(CoroutineInfo {
            yield_ty: Type::Int(64),
            return_ty: Type::Void,
        });

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let (module, changed) = apply_lowering(module);
        assert!(changed);

        // The state struct should have __v0 for the cross-yield value.
        let struct_def = &module.structs[0];
        let field_names: Vec<&str> = struct_def
            .fields
            .iter()
            .map(|(name, _, _)| name.as_str())
            .collect();
        assert!(
            field_names.iter().any(|n| n.starts_with("__v")),
            "struct should have saved value fields: {:?}",
            field_names
        );
    }
}
