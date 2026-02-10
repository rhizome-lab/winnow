//! Core AVM2 bytecode → IR translation.
//!
//! Three-pass algorithm:
//! 1. Parse bytecode into `LocatedOp`s, find basic block boundaries
//! 2. Create IR blocks (exception handler entries get a `Dynamic` param)
//! 3. Walk opcodes, maintaining virtual operand stack, scope stack, and local registers

use std::collections::{BTreeSet, HashMap, HashSet};

use reincarnate_core::ir::{
    BlockId, CmpKind, Function, FunctionBuilder, FunctionSig, Type, ValueId, Visibility,
};
use swf::avm2::types::{AbcFile, MethodBody, Op};

use crate::abc::{
    offset_to_index_map, parse_bytecode, resolve_branch_target, resolve_switch_target, LocatedOp,
};
use crate::multiname::{classify_multiname, pool_string, resolve_multiname_index, resolve_type, MultinameKind};
use crate::scope::ScopeStack;

/// Translate a single method body into an IR function.
pub fn translate_method_body(
    abc: &AbcFile,
    body: &MethodBody,
    func_name: &str,
    sig: FunctionSig,
    param_names: &[Option<String>],
    has_self: bool,
) -> Result<Function, String> {
    let ops = parse_bytecode(&body.code)?;
    let offset_map = offset_to_index_map(&ops);
    let pool = &abc.constant_pool;

    // Pass 1: Find block boundaries.
    let mut block_starts = find_block_starts(&ops, &offset_map, body);

    // Always have index 0 as a block start.
    block_starts.insert(0);

    let block_start_list: Vec<usize> = block_starts.iter().copied().collect();

    // Pass 2: Create IR blocks.
    let mut fb = FunctionBuilder::new(func_name, sig.clone(), Visibility::Public);

    // Map from op index → BlockId.
    let mut block_map: HashMap<usize, BlockId> = HashMap::new();

    // The entry block (block0) corresponds to op index 0.
    block_map.insert(0, fb.entry_block());

    // Exception handler entry blocks.
    let exception_entries: BTreeSet<usize> = body
        .exceptions
        .iter()
        .filter_map(|e| offset_map.get(&(e.target_offset as usize)).copied())
        .collect();

    for &start in &block_start_list {
        if start == 0 {
            continue; // Already have entry block.
        }
        if exception_entries.contains(&start) {
            // Exception handlers receive the caught exception as a Dynamic parameter.
            let (block, _params) = fb.create_block_with_params(&[Type::Dynamic]);
            block_map.insert(start, block);
        } else {
            let block = fb.create_block();
            block_map.insert(start, block);
        }
    }

    // Pass 3: Translate opcodes.
    let num_locals = body.num_locals as usize;
    let num_params = sig.params.len();

    // Allocate local variable slots.
    // local 0 = this (or first param for static), 1..N = params, rest = locals
    let mut locals: Vec<ValueId> = Vec::with_capacity(num_locals);
    fb.switch_to_block(fb.entry_block());

    for i in 0..num_locals {
        let slot = fb.alloc(Type::Dynamic);
        if i < num_params {
            let param_val = fb.param(i);
            fb.store(slot, param_val);
        }
        locals.push(slot);
    }

    // Name parameters from debug info (MethodParam.name).
    for (i, name) in param_names.iter().enumerate() {
        if let Some(n) = name {
            if i < num_params {
                let param_val = fb.param(i);
                fb.name_value(param_val, n.clone());
            }
        }
    }

    // Track which registers have already been named by Op::Debug so we only
    // keep the first name.  AVM2 reuses registers for unrelated local
    // variables later in the method body — taking the last name would
    // mis-label earlier variables.
    let mut named_registers: HashSet<u8> = HashSet::new();

    // Op::Debug register numbering skips `this`: register 0 = first actual
    // parameter, not `this`.  Our locals[] includes `this` at index 0 for
    // instance methods, so we need an offset to align the two numbering
    // schemes.
    let debug_reg_offset: usize = if has_self { 1 } else { 0 };

    let mut stack: Vec<ValueId> = Vec::new();
    let mut scope_stack = ScopeStack::new();

    // Track block params we've already created for merge points (op_idx → Vec<ValueId>).
    let mut block_param_values: HashMap<usize, Vec<ValueId>> = HashMap::new();

    // For exception handler entry blocks, record the param values.
    for &start in &exception_entries {
        if let Some(&block) = block_map.get(&start) {
            let params = fb.add_block_params(block, &[]);
            // The block was created with one Dynamic param already.
            // We need to get the param ValueId. Since it was created with create_block_with_params,
            // we can't easily get it here. We'll handle it when we switch to that block.
            let _ = params;
        }
    }

    let mut current_block_start: usize = 0;

    for op_idx in 0..ops.len() {
        // Check if we're at a new block boundary.
        if block_starts.contains(&op_idx) && op_idx != current_block_start {
            // If we haven't terminated the previous block, insert an implicit fall-through.
            if !is_terminated(&ops, op_idx, &block_starts, current_block_start) {
                if let Some(&target_block) = block_map.get(&op_idx) {
                    let args = prepare_branch_args(
                        &mut fb,
                        &stack,
                        target_block,
                        op_idx,
                        &mut block_param_values,
                    );
                    fb.br(target_block, &args);
                }
            }

            current_block_start = op_idx;
            if let Some(&block) = block_map.get(&op_idx) {
                fb.switch_to_block(block);
                // Restore stack from block params.
                stack.clear();
                if let Some(param_vals) = block_param_values.get(&op_idx) {
                    stack.extend_from_slice(param_vals);
                }
                scope_stack.clear();
            }
        } else if op_idx == 0 {
            // Entry block — stack starts empty, just locals.
            current_block_start = 0;
        }

        translate_op(
            &mut fb,
            &ops,
            op_idx,
            pool,
            &offset_map,
            &block_map,
            &mut stack,
            &mut scope_stack,
            &mut locals,
            &mut block_param_values,
            &mut named_registers,
            debug_reg_offset,
            num_params,
        );
    }

    // Infer names for unnamed parameters (methods without Op::Debug info).
    let inferred = infer_param_names(func_name, &sig, has_self);
    #[allow(clippy::needless_range_loop)]
    for i in 0..num_params {
        let param_val = fb.param(i);
        if fb.has_name(param_val) {
            continue;
        }
        if has_self && i == 0 {
            continue; // skip `this`
        }
        let real_idx = if has_self { i - 1 } else { i };
        if let Some(Some(name)) = inferred.get(real_idx) {
            fb.name_value(param_val, name.clone());
            // Also name the alloc slot so Mem2Reg can propagate the name.
            fb.name_value(locals[i], name.clone());
        } else {
            let display_idx = if has_self { i } else { i + 1 };
            let name = format!("param{display_idx}");
            fb.name_value(param_val, name.clone());
            fb.name_value(locals[i], name);
        }
    }

    // Synthesize names for unnamed local registers (matching ffdec's `_locN`
    // convention).  Op::Debug only covers a subset of registers; this fills
    // the rest so mem2reg can propagate a name through all SSA values.
    for (i, &slot) in locals.iter().enumerate() {
        if i < num_params {
            continue; // params already named
        }
        if fb.has_name(slot) {
            continue; // already named by Op::Debug
        }
        fb.name_value(slot, format!("_loc{}", i + debug_reg_offset));
    }

    Ok(fb.build())
}

/// Infer parameter names for methods that lack Op::Debug info.
///
/// Four layers of inference, applied per-param in priority order:
/// 1. Method-name table (exact match on bare method name + param count)
/// 2. Pattern rules (suffix matching on method name)
/// 3. Type-based inference (derive name from Struct type)
/// 4. Generic fallback (param1, param2, …)
fn infer_param_names(method_name: &str, sig: &FunctionSig, has_self: bool) -> Vec<Option<String>> {
    let bare = method_name.rsplit("::").next().unwrap_or(method_name);
    // Real param count excludes `this`.
    let skip = if has_self { 1 } else { 0 };
    let real_params: Vec<&Type> = sig.params.iter().skip(skip).collect();
    let count = real_params.len();

    // Layer 1: Method-name table.
    if let Some(names) = match_method_table(bare, count) {
        return names;
    }

    let mut result = Vec::with_capacity(count);

    for (i, &ty) in real_params.iter().enumerate() {
        // Layer 2: Pattern rules.
        if count == 1 && (bare.ends_with("Handler") || bare.ends_with("Listener")) {
            result.push(Some("event".to_string()));
            continue;
        }

        // Layer 3: Type-based inference.
        if let Some(name) = name_from_type(ty) {
            result.push(Some(name));
            continue;
        }

        // Layer 4: Generic fallback.
        result.push(Some(format!("param{}", i + 1)));
    }

    result
}

/// Layer 1 lookup: exact match on bare method name + param count.
fn match_method_table(bare: &str, count: usize) -> Option<Vec<Option<String>>> {
    let names: &[&str] = match (bare, count) {
        ("setSize", 2) => &["width", "height"],
        ("move", 2) => &["x", "y"],
        ("setStyle", 2) => &["name", "value"],
        ("addItem", 1) => &["item"],
        ("addItemAt", 2) => &["item", "index"],
        ("removeItem", 1) => &["item"],
        ("removeItemAt", 1) => &["index"],
        ("replaceItemAt", 2) => &["item", "index"],
        ("getItemAt", 1) => &["index"],
        ("getItemIndex", 1) => &["item"],
        ("itemToLabel", 1) => &["item"],
        ("sortItemsOn", 2) => &["field", "options"],
        ("setVerticalScrollPosition", 2) => &["position", "fireEvent"],
        ("setHorizontalScrollPosition", 2) => &["position", "fireEvent"],
        ("setScrollPosition", 2) => &["position", "fireEvent"],
        ("scrollToIndex", 1) => &["index"],
        ("drawFocus", 1) => &["focused"],
        ("dispatchEvent", 1) => &["event"],
        ("passEvent", 1) => &["event"],
        ("moveSelectionHorizontally", 3) => &["code", "shiftKey", "ctrlKey"],
        ("moveSelectionVertically", 3) => &["code", "shiftKey", "ctrlKey"],
        ("setButton", 4) => &["index", "label", "callback", "toolTip"],
        ("showBottomButton", 4) => &["index", "label", "callback", "toolTip"],
        ("setMenuButton", 3) => &["name", "label", "callback"],
        ("setStatText", 2) => &["name", "value"],
        ("setStatBar", 2) => &["name", "value"],
        ("setOutputText", 1) => &["text"],
        ("selectSprite", 1) => &["index"],
        ("getMenuButtonByName", 1) => &["name"],
        ("indexOfButtonWithLabel", 1) => &["label"],
        _ => return None,
    };
    Some(names.iter().map(|s| Some(s.to_string())).collect())
}

/// Layer 3: derive a parameter name from a `Type::Struct` class name.
fn name_from_type(ty: &Type) -> Option<String> {
    let class = match ty {
        Type::Struct(name) => name,
        _ => return None,
    };

    // Strip package prefix. Names may use "::" (e.g. "flash.net::URLRequest")
    // or "." (e.g. "flash.events.Event") as separators.
    let short = class
        .rsplit("::")
        .next()
        .unwrap_or(class)
        .rsplit('.')
        .next()
        .unwrap_or(class);

    // *Event → "event"
    if short.ends_with("Event") {
        return Some("event".to_string());
    }

    // Detect leading acronym (consecutive uppercase, ≥2 chars).
    let upper_len = short.chars().take_while(|c| c.is_ascii_uppercase()).count();
    if upper_len >= 2 && upper_len < short.len() {
        // E.g. "URLRequest" → strip "URL" → "Request" → "request"
        let remainder = &short[upper_len - 1..]; // keep last uppercase as start
        // But if the acronym IS the whole prefix (like URL in URLRequest),
        // strip all but the transition char: "URLRequest" → "Request"
        let stripped = &short[upper_len..];
        if stripped.is_empty() {
            // Entire name is an acronym — just lowercase it.
            return Some(short.to_lowercase());
        }
        // "URLRequest" → acronym_len=3, stripped="equest", remainder="Request"
        return Some(lower_first(remainder));
    }

    // Normal PascalCase → lowerCamelCase.
    if short.len() > 1 && short.starts_with(|c: char| c.is_ascii_uppercase()) {
        return Some(lower_first(short));
    }

    None
}

/// Lowercase the first character of a string.
fn lower_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) => c.to_lowercase().to_string() + chars.as_str(),
        None => String::new(),
    }
}

/// Check if the block ending just before `op_idx` has already been terminated.
fn is_terminated(
    ops: &[LocatedOp],
    op_idx: usize,
    _block_starts: &BTreeSet<usize>,
    current_block_start: usize,
) -> bool {
    if op_idx == 0 {
        return false;
    }
    let prev = op_idx - 1;
    if prev < current_block_start {
        return true; // Empty block range, treat as terminated.
    }
    // Check if the previous instruction is a terminator.
    // Conditional branches (IfTrue, IfEq, etc.) are terminators too — they
    // emit a BrIf in the IR, so no fallthrough Br should follow them.
    matches!(
        ops[prev].op,
        Op::Jump { .. }
            | Op::ReturnValue
            | Op::ReturnVoid
            | Op::Throw
            | Op::LookupSwitch(_)
            | Op::IfTrue { .. }
            | Op::IfFalse { .. }
            | Op::IfEq { .. }
            | Op::IfNe { .. }
            | Op::IfLt { .. }
            | Op::IfLe { .. }
            | Op::IfGt { .. }
            | Op::IfGe { .. }
            | Op::IfStrictEq { .. }
            | Op::IfStrictNe { .. }
            | Op::IfNge { .. }
            | Op::IfNgt { .. }
            | Op::IfNle { .. }
            | Op::IfNlt { .. }
    )
}

/// Find all basic block start indices.
fn find_block_starts(
    ops: &[LocatedOp],
    offset_map: &HashMap<usize, usize>,
    body: &MethodBody,
) -> BTreeSet<usize> {
    let mut starts = BTreeSet::new();
    starts.insert(0);

    for (i, loc) in ops.iter().enumerate() {
        match &loc.op {
            // Unconditional jump
            Op::Jump { offset } => {
                if let Some(target) = resolve_branch_target(ops, offset_map, i, *offset) {
                    starts.insert(target);
                }
                // Fall-through after jump is also a block start (if reachable).
                if i + 1 < ops.len() {
                    starts.insert(i + 1);
                }
            }
            // Conditional branches
            Op::IfTrue { offset }
            | Op::IfFalse { offset }
            | Op::IfEq { offset }
            | Op::IfNe { offset }
            | Op::IfLt { offset }
            | Op::IfLe { offset }
            | Op::IfGt { offset }
            | Op::IfGe { offset }
            | Op::IfStrictEq { offset }
            | Op::IfStrictNe { offset }
            | Op::IfNge { offset }
            | Op::IfNgt { offset }
            | Op::IfNle { offset }
            | Op::IfNlt { offset } => {
                if let Some(target) = resolve_branch_target(ops, offset_map, i, *offset) {
                    starts.insert(target);
                }
                if i + 1 < ops.len() {
                    starts.insert(i + 1);
                }
            }
            // Switch
            Op::LookupSwitch(switch) => {
                if let Some(target) =
                    resolve_switch_target(ops, offset_map, i, switch.default_offset)
                {
                    starts.insert(target);
                }
                for &case_offset in switch.case_offsets.iter() {
                    if let Some(target) = resolve_switch_target(ops, offset_map, i, case_offset) {
                        starts.insert(target);
                    }
                }
                if i + 1 < ops.len() {
                    starts.insert(i + 1);
                }
            }
            // Return/throw terminators
            Op::ReturnValue | Op::ReturnVoid | Op::Throw => {
                if i + 1 < ops.len() {
                    starts.insert(i + 1);
                }
            }
            _ => {}
        }
    }

    // Exception handler targets.
    for exception in &body.exceptions {
        if let Some(&idx) = offset_map.get(&(exception.target_offset as usize)) {
            starts.insert(idx);
        }
    }

    starts
}

/// Prepare arguments for a branch to `target_block`.
///
/// On first visit, creates block params from the current stack types.
/// On subsequent visits, reuses the existing params.
fn prepare_branch_args(
    fb: &mut FunctionBuilder,
    stack: &[ValueId],
    target_block: BlockId,
    target_op_idx: usize,
    block_param_values: &mut HashMap<usize, Vec<ValueId>>,
) -> Vec<ValueId> {
    block_param_values.entry(target_op_idx).or_insert_with(|| {
        // First time branching to this block — create params from stack types.
        let types: Vec<Type> = stack.iter().map(|v| fb.value_type(*v)).collect();
        fb.add_block_params(target_block, &types)
    });
    // Return the current stack values as branch arguments.
    stack.to_vec()
}

/// Resolved property access — either a compile-time name or a runtime index.
enum PropertyAccess {
    Named(String),
    Indexed(ValueId),
}

/// Resolve a multiname into a `PropertyAccess`, popping runtime values from
/// the stack as needed.
fn resolve_property(
    pool: &swf::avm2::types::ConstantPool,
    index: &swf::avm2::types::Index<swf::avm2::types::Multiname>,
    stack: &mut Vec<ValueId>,
) -> PropertyAccess {
    match classify_multiname(pool, index) {
        MultinameKind::Named(name) => PropertyAccess::Named(name),
        MultinameKind::RuntimeNs(name) => {
            // Pop the namespace value from the stack and discard it.
            stack.pop();
            PropertyAccess::Named(name)
        }
        MultinameKind::RuntimeName => {
            // Pop the name/index value from the stack.
            let idx = stack.pop().unwrap_or_else(|| panic!("stack underflow for RuntimeName"));
            PropertyAccess::Indexed(idx)
        }
        MultinameKind::RuntimeBoth => {
            // Pop name first, then namespace (AVM2 stack order).
            let idx = stack.pop().unwrap_or_else(|| panic!("stack underflow for RuntimeBoth name"));
            stack.pop(); // namespace, discard
            PropertyAccess::Indexed(idx)
        }
    }
}

/// Translate a single AVM2 opcode.
#[allow(clippy::too_many_arguments)]
fn translate_op(
    fb: &mut FunctionBuilder,
    ops: &[LocatedOp],
    op_idx: usize,
    pool: &swf::avm2::types::ConstantPool,
    offset_map: &HashMap<usize, usize>,
    block_map: &HashMap<usize, BlockId>,
    stack: &mut Vec<ValueId>,
    scope_stack: &mut ScopeStack,
    locals: &mut [ValueId],
    block_param_values: &mut HashMap<usize, Vec<ValueId>>,
    named_registers: &mut HashSet<u8>,
    debug_reg_offset: usize,
    num_params: usize,
) {
    let loc = &ops[op_idx];
    match &loc.op {
        // ====================================================================
        // Constants
        // ====================================================================
        Op::PushByte { value } => {
            let v = fb.const_int(*value as i64);
            stack.push(v);
        }
        Op::PushShort { value } => {
            let v = fb.const_int(*value as i64);
            stack.push(v);
        }
        Op::PushInt { value } => {
            let i = value.0 as usize;
            let val = if i > 0 && i <= pool.ints.len() {
                pool.ints[i - 1] as i64
            } else {
                0
            };
            let v = fb.const_int(val);
            stack.push(v);
        }
        Op::PushUint { value } => {
            let i = value.0 as usize;
            let val = if i > 0 && i <= pool.uints.len() {
                pool.uints[i - 1] as u64
            } else {
                0
            };
            let v = fb.const_uint(val);
            stack.push(v);
        }
        Op::PushDouble { value } => {
            let i = value.0 as usize;
            let val = if i > 0 && i <= pool.doubles.len() {
                pool.doubles[i - 1]
            } else {
                f64::NAN
            };
            let v = fb.const_float(val);
            stack.push(v);
        }
        Op::PushString { value } => {
            let s = pool_string(pool, value);
            let v = fb.const_string(s);
            stack.push(v);
        }
        Op::PushTrue => {
            let v = fb.const_bool(true);
            stack.push(v);
        }
        Op::PushFalse => {
            let v = fb.const_bool(false);
            stack.push(v);
        }
        Op::PushNull => {
            let v = fb.const_null();
            stack.push(v);
        }
        Op::PushUndefined => {
            let v = fb.const_null();
            stack.push(v);
        }
        Op::PushNaN => {
            let v = fb.const_float(f64::NAN);
            stack.push(v);
        }
        Op::PushNamespace { .. } => {
            // Push a namespace as a dynamic value
            let v = fb.const_null();
            stack.push(v);
        }

        // ====================================================================
        // Local variable access
        // ====================================================================
        Op::GetLocal { index } => {
            let idx = *index as usize;
            if idx < locals.len() {
                let v = fb.load(locals[idx], Type::Dynamic);
                stack.push(v);
            }
        }
        Op::SetLocal { index } => {
            let idx = *index as usize;
            if let Some(val) = stack.pop() {
                if idx < locals.len() {
                    fb.store(locals[idx], val);
                }
            }
        }
        Op::Kill { index } => {
            let idx = *index as usize;
            if idx < locals.len() {
                let null = fb.const_null();
                fb.store(locals[idx], null);
            }
        }

        // ====================================================================
        // Arithmetic
        // ====================================================================
        Op::Add => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.add(a, b);
                stack.push(v);
            }
        }
        Op::AddI => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let a = fb.cast(a, Type::Int(32));
                let b = fb.cast(b, Type::Int(32));
                let v = fb.add(a, b);
                stack.push(v);
            }
        }
        Op::Subtract => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.sub(a, b);
                stack.push(v);
            }
        }
        Op::SubtractI => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let a = fb.cast(a, Type::Int(32));
                let b = fb.cast(b, Type::Int(32));
                let v = fb.sub(a, b);
                stack.push(v);
            }
        }
        Op::Multiply => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.mul(a, b);
                stack.push(v);
            }
        }
        Op::MultiplyI => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let a = fb.cast(a, Type::Int(32));
                let b = fb.cast(b, Type::Int(32));
                let v = fb.mul(a, b);
                stack.push(v);
            }
        }
        Op::Divide => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.div(a, b);
                stack.push(v);
            }
        }
        Op::Modulo => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.rem(a, b);
                stack.push(v);
            }
        }
        Op::Negate => {
            if let Some(a) = stack.pop() {
                let v = fb.neg(a);
                stack.push(v);
            }
        }
        Op::NegateI => {
            if let Some(a) = stack.pop() {
                let a = fb.cast(a, Type::Int(32));
                let v = fb.neg(a);
                stack.push(v);
            }
        }
        Op::Increment => {
            if let Some(a) = stack.pop() {
                let one = fb.const_float(1.0);
                let v = fb.add(a, one);
                stack.push(v);
            }
        }
        Op::IncrementI => {
            if let Some(a) = stack.pop() {
                let a = fb.cast(a, Type::Int(32));
                let one = fb.const_int(1);
                let v = fb.add(a, one);
                stack.push(v);
            }
        }
        Op::Decrement => {
            if let Some(a) = stack.pop() {
                let one = fb.const_float(1.0);
                let v = fb.sub(a, one);
                stack.push(v);
            }
        }
        Op::DecrementI => {
            if let Some(a) = stack.pop() {
                let a = fb.cast(a, Type::Int(32));
                let one = fb.const_int(1);
                let v = fb.sub(a, one);
                stack.push(v);
            }
        }
        Op::IncLocal { index } => {
            let idx = *index as usize;
            if idx < locals.len() {
                let val = fb.load(locals[idx], Type::Dynamic);
                let one = fb.const_float(1.0);
                let inc = fb.add(val, one);
                fb.store(locals[idx], inc);
            }
        }
        Op::IncLocalI { index } => {
            let idx = *index as usize;
            if idx < locals.len() {
                let val = fb.load(locals[idx], Type::Dynamic);
                let val = fb.cast(val, Type::Int(32));
                let one = fb.const_int(1);
                let inc = fb.add(val, one);
                fb.store(locals[idx], inc);
            }
        }
        Op::DecLocal { index } => {
            let idx = *index as usize;
            if idx < locals.len() {
                let val = fb.load(locals[idx], Type::Dynamic);
                let one = fb.const_float(1.0);
                let dec = fb.sub(val, one);
                fb.store(locals[idx], dec);
            }
        }
        Op::DecLocalI { index } => {
            let idx = *index as usize;
            if idx < locals.len() {
                let val = fb.load(locals[idx], Type::Dynamic);
                let val = fb.cast(val, Type::Int(32));
                let one = fb.const_int(1);
                let dec = fb.sub(val, one);
                fb.store(locals[idx], dec);
            }
        }

        // ====================================================================
        // Bitwise
        // ====================================================================
        Op::BitAnd => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.bit_and(a, b);
                stack.push(v);
            }
        }
        Op::BitOr => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.bit_or(a, b);
                stack.push(v);
            }
        }
        Op::BitXor => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.bit_xor(a, b);
                stack.push(v);
            }
        }
        Op::BitNot => {
            if let Some(a) = stack.pop() {
                let v = fb.bit_not(a);
                stack.push(v);
            }
        }
        Op::LShift => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.shl(a, b);
                stack.push(v);
            }
        }
        Op::RShift => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.shr(a, b);
                stack.push(v);
            }
        }
        Op::URShift => {
            // Unsigned right shift — cast to uint first.
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let a = fb.cast(a, Type::UInt(32));
                let v = fb.shr(a, b);
                stack.push(v);
            }
        }

        // ====================================================================
        // Comparison
        // ====================================================================
        Op::Equals => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.cmp(CmpKind::Eq, a, b);
                stack.push(v);
            }
        }
        Op::StrictEquals => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.cmp(CmpKind::Eq, a, b);
                stack.push(v);
            }
        }
        Op::LessThan => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.cmp(CmpKind::Lt, a, b);
                stack.push(v);
            }
        }
        Op::LessEquals => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.cmp(CmpKind::Le, a, b);
                stack.push(v);
            }
        }
        Op::GreaterThan => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.cmp(CmpKind::Gt, a, b);
                stack.push(v);
            }
        }
        Op::GreaterEquals => {
            if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.cmp(CmpKind::Ge, a, b);
                stack.push(v);
            }
        }
        Op::Not => {
            if let Some(a) = stack.pop() {
                let v = fb.not(a);
                stack.push(v);
            }
        }

        // ====================================================================
        // Type coercion / conversion
        // ====================================================================
        Op::CoerceA => {
            // Coerce to * (any) — no-op in IR, type is Dynamic.
        }
        Op::CoerceB | Op::ConvertB => {
            if let Some(a) = stack.pop() {
                let v = fb.cast(a, Type::Bool);
                stack.push(v);
            }
        }
        Op::CoerceD | Op::ConvertD => {
            if let Some(a) = stack.pop() {
                let v = fb.cast(a, Type::Float(64));
                stack.push(v);
            }
        }
        Op::CoerceI | Op::ConvertI => {
            if let Some(a) = stack.pop() {
                let v = fb.cast(a, Type::Int(32));
                stack.push(v);
            }
        }
        Op::CoerceU | Op::ConvertU => {
            if let Some(a) = stack.pop() {
                let v = fb.cast(a, Type::UInt(32));
                stack.push(v);
            }
        }
        Op::CoerceS | Op::ConvertS => {
            if let Some(a) = stack.pop() {
                let v = fb.cast(a, Type::String);
                stack.push(v);
            }
        }
        Op::CoerceO | Op::ConvertO => {
            if let Some(a) = stack.pop() {
                let v = fb.cast(a, Type::Dynamic);
                stack.push(v);
            }
        }
        Op::Coerce { index } => {
            if let Some(a) = stack.pop() {
                let ty = resolve_type(pool, index);
                let v = fb.cast(a, ty);
                stack.push(v);
            }
        }

        // ====================================================================
        // Type checking
        // ====================================================================
        Op::IsType { index } => {
            if let Some(a) = stack.pop() {
                let ty = resolve_type(pool, index);
                let v = fb.type_check(a, ty);
                stack.push(v);
            }
        }
        Op::IsTypeLate => {
            if let (Some(_type_val), Some(a)) = (stack.pop(), stack.pop()) {
                // Runtime type check — approximate as Dynamic type check.
                let v = fb.type_check(a, Type::Dynamic);
                stack.push(v);
            }
        }
        Op::AsType { type_name } => {
            if let Some(a) = stack.pop() {
                let ty = resolve_type(pool, type_name);
                let v = fb.cast(a, ty);
                stack.push(v);
            }
        }
        Op::AsTypeLate => {
            if let (Some(_type_val), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.cast(a, Type::Dynamic);
                stack.push(v);
            }
        }
        Op::InstanceOf => {
            if let (Some(_type_val), Some(a)) = (stack.pop(), stack.pop()) {
                let v = fb.type_check(a, Type::Dynamic);
                stack.push(v);
            }
        }
        Op::TypeOf => {
            if let Some(a) = stack.pop() {
                let v = fb.system_call("Flash.Object", "typeOf", &[a], Type::String);
                stack.push(v);
            }
        }

        // ====================================================================
        // Stack manipulation
        // ====================================================================
        Op::Dup => {
            if let Some(a) = stack.last().copied() {
                let v = fb.copy(a);
                stack.push(v);
            }
        }
        Op::Swap => {
            let len = stack.len();
            if len >= 2 {
                stack.swap(len - 1, len - 2);
            }
        }
        Op::Pop => {
            stack.pop();
        }

        // ====================================================================
        // Scope stack
        // ====================================================================
        Op::PushScope => {
            if let Some(val) = stack.pop() {
                scope_stack.push(val);
            }
        }
        Op::PushWith => {
            if let Some(val) = stack.pop() {
                scope_stack.push(val);
            }
        }
        Op::PopScope => {
            scope_stack.pop();
        }
        Op::GetScopeObject { index } => {
            let v = if let Some(val) = scope_stack.get(*index as usize) {
                fb.copy(val)
            } else {
                fb.const_null()
            };
            stack.push(v);
        }
        Op::GetGlobalScope => {
            let v = if let Some(val) = scope_stack.get(0) {
                fb.copy(val)
            } else {
                fb.const_null()
            };
            stack.push(v);
        }
        Op::GetOuterScope { index } => {
            let v =
                fb.system_call("Flash.Scope", "getOuterScope", &[], Type::Dynamic);
            let _ = index;
            stack.push(v);
        }

        // ====================================================================
        // Property access
        // ====================================================================
        Op::GetProperty { index } => {
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                let v = match prop {
                    PropertyAccess::Named(name) => fb.get_field(obj, &name, Type::Dynamic),
                    PropertyAccess::Indexed(idx) => fb.get_index(obj, idx, Type::Dynamic),
                };
                stack.push(v);
            }
        }
        Op::SetProperty { index } | Op::InitProperty { index } => {
            // AVM2 stack order: ..., object, [ns], [name], value
            // Pop value first, then resolve multiname (may pop runtime name),
            // then pop object.
            let val = stack.pop();
            let prop = resolve_property(pool, index, stack);
            if let (Some(val), Some(obj)) = (val, stack.pop()) {
                match prop {
                    PropertyAccess::Named(name) => fb.set_field(obj, &name, val),
                    PropertyAccess::Indexed(idx) => fb.set_index(obj, idx, val),
                }
            }
        }
        Op::DeleteProperty { index } => {
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                let key_val = match prop {
                    PropertyAccess::Named(name) => fb.const_string(&name),
                    PropertyAccess::Indexed(idx) => idx,
                };
                let v = fb.system_call(
                    "Flash.Object",
                    "deleteProperty",
                    &[obj, key_val],
                    Type::Bool,
                );
                stack.push(v);
            }
        }
        Op::GetSlot { index } => {
            if let Some(obj) = stack.pop() {
                let slot_name = format!("slot{index}");
                let v = fb.get_field(obj, &slot_name, Type::Dynamic);
                stack.push(v);
            }
        }
        Op::SetSlot { index } => {
            if let (Some(val), Some(obj)) = (stack.pop(), stack.pop()) {
                let slot_name = format!("slot{index}");
                fb.set_field(obj, &slot_name, val);
            }
        }
        Op::GetGlobalSlot { index } => {
            let name = format!("global_slot{index}");
            let v = fb.global_ref(&name, Type::Dynamic);
            stack.push(v);
        }
        Op::SetGlobalSlot { index } => {
            if let Some(val) = stack.pop() {
                let name = format!("global_slot{index}");
                let ptr = fb.global_ref(&name, Type::Dynamic);
                fb.store(ptr, val);
            }
        }
        Op::GetSuper { index } => {
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                let name_val = match prop {
                    PropertyAccess::Named(name) => fb.const_string(&name),
                    PropertyAccess::Indexed(idx) => idx,
                };
                let v = fb.system_call(
                    "Flash.Class",
                    "getSuper",
                    &[obj, name_val],
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::SetSuper { index } => {
            // AVM2 stack order: ..., object, [ns], [name], value
            let val = stack.pop();
            let prop = resolve_property(pool, index, stack);
            if let (Some(val), Some(obj)) = (val, stack.pop()) {
                let name_val = match prop {
                    PropertyAccess::Named(name) => fb.const_string(&name),
                    PropertyAccess::Indexed(idx) => idx,
                };
                fb.system_call("Flash.Class", "setSuper", &[obj, name_val, val], Type::Void);
            }
        }
        Op::GetLex { index } => {
            // GetLex is always statically named (AVM2 spec: no runtime multinames).
            let name = resolve_multiname_index(pool, index);
            let name_val = fb.const_string(&name);
            let v = fb.system_call(
                "Flash.Scope",
                "findPropStrict",
                &[name_val],
                Type::Dynamic,
            );
            let result = fb.get_field(v, &name, Type::Dynamic);
            stack.push(result);
        }

        // ====================================================================
        // Find property (scope chain)
        // ====================================================================
        Op::FindProperty { index } => {
            let prop = resolve_property(pool, index, stack);
            let name_val = match prop {
                PropertyAccess::Named(name) => fb.const_string(&name),
                PropertyAccess::Indexed(idx) => idx,
            };
            let v = fb.system_call(
                "Flash.Scope",
                "findProperty",
                &[name_val],
                Type::Dynamic,
            );
            stack.push(v);
        }
        Op::FindPropStrict { index } => {
            let prop = resolve_property(pool, index, stack);
            let name_val = match prop {
                PropertyAccess::Named(name) => fb.const_string(&name),
                PropertyAccess::Indexed(idx) => idx,
            };
            let v = fb.system_call(
                "Flash.Scope",
                "findPropStrict",
                &[name_val],
                Type::Dynamic,
            );
            stack.push(v);
        }
        Op::FindDef { index } => {
            let name = resolve_multiname_index(pool, index);
            let name_val = fb.const_string(&name);
            let v = fb.system_call(
                "Flash.Scope",
                "findDef",
                &[name_val],
                Type::Dynamic,
            );
            stack.push(v);
        }

        // ====================================================================
        // Calls
        // ====================================================================
        Op::CallProperty { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                match prop {
                    PropertyAccess::Named(ref name) if name.starts_with("flash.utils::") => {
                        let method = name.rsplit("::").next().unwrap_or(name);
                        let v = fb.system_call("Flash.Utils", method, &args, Type::Dynamic);
                        stack.push(v);
                    }
                    PropertyAccess::Named(name) => {
                        let mut call_args = vec![obj];
                        call_args.extend(args);
                        let v = fb.call(&name, &call_args, Type::Dynamic);
                        stack.push(v);
                    }
                    PropertyAccess::Indexed(idx) => {
                        let callee = fb.get_index(obj, idx, Type::Dynamic);
                        let mut call_args = vec![obj];
                        call_args.extend(args);
                        let v = fb.call_indirect(callee, &call_args, Type::Dynamic);
                        stack.push(v);
                    }
                }
            }
        }
        Op::CallPropVoid { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                match prop {
                    PropertyAccess::Named(ref name) if name.starts_with("flash.utils::") => {
                        let method = name.rsplit("::").next().unwrap_or(name);
                        fb.system_call("Flash.Utils", method, &args, Type::Void);
                    }
                    PropertyAccess::Named(name) => {
                        let mut call_args = vec![obj];
                        call_args.extend(args);
                        fb.call(&name, &call_args, Type::Void);
                    }
                    PropertyAccess::Indexed(idx) => {
                        let callee = fb.get_index(obj, idx, Type::Dynamic);
                        let mut call_args = vec![obj];
                        call_args.extend(args);
                        fb.call_indirect(callee, &call_args, Type::Void);
                    }
                }
            }
        }
        Op::CallPropLex { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                match prop {
                    PropertyAccess::Named(ref name) if name.starts_with("flash.utils::") => {
                        let method = name.rsplit("::").next().unwrap_or(name);
                        let v = fb.system_call("Flash.Utils", method, &args, Type::Dynamic);
                        stack.push(v);
                    }
                    PropertyAccess::Named(name) => {
                        let mut call_args = vec![obj];
                        call_args.extend(args);
                        let v = fb.call(&name, &call_args, Type::Dynamic);
                        stack.push(v);
                    }
                    PropertyAccess::Indexed(idx) => {
                        let callee = fb.get_index(obj, idx, Type::Dynamic);
                        let mut call_args = vec![obj];
                        call_args.extend(args);
                        let v = fb.call_indirect(callee, &call_args, Type::Dynamic);
                        stack.push(v);
                    }
                }
            }
        }
        Op::CallSuper { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                let name_val = match prop {
                    PropertyAccess::Named(name) => fb.const_string(&name),
                    PropertyAccess::Indexed(idx) => idx,
                };
                let mut call_args = vec![obj, name_val];
                call_args.extend(args);
                let v = fb.system_call(
                    "Flash.Class",
                    "callSuper",
                    &call_args,
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::CallSuperVoid { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                let name_val = match prop {
                    PropertyAccess::Named(name) => fb.const_string(&name),
                    PropertyAccess::Indexed(idx) => idx,
                };
                let mut call_args = vec![obj, name_val];
                call_args.extend(args);
                fb.system_call("Flash.Class", "callSuper", &call_args, Type::Void);
            }
        }
        Op::CallStatic { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            if let Some(receiver) = stack.pop() {
                let func_name = format!("method{}", index.0);
                let mut call_args = vec![receiver];
                call_args.extend(args);
                let v = fb.call(&func_name, &call_args, Type::Dynamic);
                stack.push(v);
            }
        }
        Op::CallMethod { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            if let Some(receiver) = stack.pop() {
                let func_name = format!("disp{index}");
                let mut call_args = vec![receiver];
                call_args.extend(args);
                let v = fb.call(&func_name, &call_args, Type::Dynamic);
                stack.push(v);
            }
        }
        Op::Call { num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            if let (Some(receiver), Some(callee)) = (stack.pop(), stack.pop()) {
                let mut call_args = vec![receiver];
                call_args.extend(args);
                let v = fb.call_indirect(callee, &call_args, Type::Dynamic);
                stack.push(v);
            }
        }

        // ====================================================================
        // Construction
        // ====================================================================
        Op::Construct { num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            if let Some(obj) = stack.pop() {
                let mut call_args = vec![obj];
                call_args.extend(args);
                let v = fb.system_call(
                    "Flash.Object",
                    "construct",
                    &call_args,
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::ConstructProp { index, num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            let prop_access = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                let prop = match prop_access {
                    PropertyAccess::Named(name) => fb.get_field(obj, &name, Type::Dynamic),
                    PropertyAccess::Indexed(idx) => fb.get_index(obj, idx, Type::Dynamic),
                };
                let mut call_args = vec![prop];
                call_args.extend(args);
                let v = fb.system_call(
                    "Flash.Object",
                    "construct",
                    &call_args,
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::ConstructSuper { num_args } => {
            let n = *num_args as usize;
            let args = pop_n(stack, n);
            if let Some(obj) = stack.pop() {
                let mut call_args = vec![obj];
                call_args.extend(args);
                fb.system_call("Flash.Class", "constructSuper", &call_args, Type::Void);
            }
        }

        // ====================================================================
        // Object / Array creation
        // ====================================================================
        Op::NewObject { num_args } => {
            // Pops num_args * 2 values (name, value pairs)
            let n = (*num_args as usize) * 2;
            let pairs = pop_n(stack, n);
            let v = fb.system_call("Flash.Object", "newObject", &pairs, Type::Dynamic);
            stack.push(v);
        }
        Op::NewArray { num_args } => {
            let n = *num_args as usize;
            let elems = pop_n(stack, n);
            let v = fb.array_init(&elems, Type::Dynamic);
            stack.push(v);
        }
        Op::NewActivation => {
            let v = fb.system_call(
                "Flash.Scope",
                "newActivation",
                &[],
                Type::Dynamic,
            );
            stack.push(v);
        }
        Op::NewFunction { index } => {
            let func_name = format!("anon_func{}", index.0);
            let name_val = fb.const_string(&func_name);
            let v = fb.system_call(
                "Flash.Object",
                "newFunction",
                &[name_val],
                Type::Dynamic,
            );
            stack.push(v);
        }
        Op::NewClass { index } => {
            let base = stack.pop().unwrap_or_else(|| fb.const_null());
            let class_name = format!("class{}", index.0);
            let name_val = fb.const_string(&class_name);
            let v = fb.system_call(
                "Flash.Class",
                "initClass",
                &[base, name_val],
                Type::Dynamic,
            );
            stack.push(v);
        }
        Op::NewCatch { index } => {
            let idx_val = fb.const_int(index.0 as i64);
            let v = fb.system_call(
                "Flash.Exception",
                "newCatchScope",
                &[idx_val],
                Type::Dynamic,
            );
            stack.push(v);
        }
        Op::ApplyType { num_types } => {
            let n = *num_types as usize;
            let type_args = pop_n(stack, n);
            if let Some(base) = stack.pop() {
                let mut args = vec![base];
                args.extend(type_args);
                let v = fb.system_call(
                    "Flash.Object",
                    "applyType",
                    &args,
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }

        // ====================================================================
        // Control flow - branches
        // ====================================================================
        Op::Jump { offset } => {
            if let Some(target_idx) = resolve_branch_target(ops, offset_map, op_idx, *offset) {
                if let Some(&target_block) = block_map.get(&target_idx) {
                    let args = prepare_branch_args(
                        fb,
                        stack,
                        target_block,
                        target_idx,
                        block_param_values,
                    );
                    fb.br(target_block, &args);
                }
            }
        }
        Op::IfTrue { offset } => {
            emit_conditional_branch(
                fb,
                ops,
                op_idx,
                *offset,
                offset_map,
                block_map,
                stack,
                block_param_values,
                |_fb, val| val,
            );
        }
        Op::IfFalse { offset } => {
            emit_conditional_branch(
                fb,
                ops,
                op_idx,
                *offset,
                offset_map,
                block_map,
                stack,
                block_param_values,
                |fb, val| fb.not(val),
            );
        }
        Op::IfEq { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Eq,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        Op::IfNe { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Ne,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        Op::IfLt { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Lt,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        Op::IfLe { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Le,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        Op::IfGt { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Gt,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        Op::IfGe { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Ge,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        Op::IfStrictEq { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Eq,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        Op::IfStrictNe { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Ne,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        // IfNge = !(a >= b) = a < b
        Op::IfNge { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Lt,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        // IfNgt = !(a > b) = a <= b
        Op::IfNgt { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Le,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        // IfNle = !(a <= b) = a > b
        Op::IfNle { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Gt,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }
        // IfNlt = !(a < b) = a >= b
        Op::IfNlt { offset } => {
            emit_comparison_branch(
                fb,
                ops,
                op_idx,
                *offset,
                CmpKind::Ge,
                offset_map,
                block_map,
                stack,
                block_param_values,
            );
        }

        // ====================================================================
        // Control flow - switch
        // ====================================================================
        Op::LookupSwitch(switch) => {
            if let Some(value) = stack.pop() {
                let mut cases = Vec::new();
                for (i, &case_offset) in switch.case_offsets.iter().enumerate() {
                    if let Some(target_idx) =
                        resolve_switch_target(ops, offset_map, op_idx, case_offset)
                    {
                        if let Some(&target_block) = block_map.get(&target_idx) {
                            let args = prepare_branch_args(
                                fb,
                                stack,
                                target_block,
                                target_idx,
                                block_param_values,
                            );
                            cases.push((
                                reincarnate_core::ir::Constant::Int(i as i64),
                                target_block,
                                args,
                            ));
                        }
                    }
                }
                if let Some(default_idx) =
                    resolve_switch_target(ops, offset_map, op_idx, switch.default_offset)
                {
                    if let Some(&default_block) = block_map.get(&default_idx) {
                        let default_args = prepare_branch_args(
                            fb,
                            stack,
                            default_block,
                            default_idx,
                            block_param_values,
                        );
                        fb.switch(value, cases, (default_block, default_args));
                    }
                }
            }
        }

        // ====================================================================
        // Return / throw
        // ====================================================================
        Op::ReturnValue => {
            if let Some(val) = stack.pop() {
                fb.ret(Some(val));
            } else {
                fb.ret(None);
            }
        }
        Op::ReturnVoid => {
            fb.ret(None);
        }
        Op::Throw => {
            if let Some(val) = stack.pop() {
                fb.system_call("Flash.Exception", "throw", &[val], Type::Void);
            }
        }

        // ====================================================================
        // Iterator / enumeration
        // ====================================================================
        Op::HasNext => {
            if let (Some(idx_val), Some(obj)) = (stack.pop(), stack.pop()) {
                let v = fb.system_call(
                    "Flash.Iterator",
                    "hasNext",
                    &[obj, idx_val],
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::HasNext2 {
            object_register,
            index_register,
        } => {
            let obj_idx = *object_register as usize;
            let idx_idx = *index_register as usize;
            let obj = if obj_idx < locals.len() {
                fb.load(locals[obj_idx], Type::Dynamic)
            } else {
                fb.const_null()
            };
            let idx = if idx_idx < locals.len() {
                fb.load(locals[idx_idx], Type::Dynamic)
            } else {
                fb.const_null()
            };
            // hasNext2 returns [updatedObj, newIndex, hasMore]
            let result = fb.system_call(
                "Flash.Iterator",
                "hasNext2",
                &[obj, idx],
                Type::Dynamic,
            );
            let idx0 = fb.const_int(0);
            let idx1 = fb.const_int(1);
            let idx2 = fb.const_int(2);
            let new_obj = fb.get_index(result, idx0, Type::Dynamic);
            let new_idx = fb.get_index(result, idx1, Type::Dynamic);
            let has_more = fb.get_index(result, idx2, Type::Bool);
            if obj_idx < locals.len() {
                fb.store(locals[obj_idx], new_obj);
            }
            if idx_idx < locals.len() {
                fb.store(locals[idx_idx], new_idx);
            }
            stack.push(has_more);
        }
        Op::NextName => {
            if let (Some(idx_val), Some(obj)) = (stack.pop(), stack.pop()) {
                let v = fb.system_call(
                    "Flash.Iterator",
                    "nextName",
                    &[obj, idx_val],
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::NextValue => {
            if let (Some(idx_val), Some(obj)) = (stack.pop(), stack.pop()) {
                let v = fb.system_call(
                    "Flash.Iterator",
                    "nextValue",
                    &[obj, idx_val],
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::In => {
            if let (Some(obj), Some(name)) = (stack.pop(), stack.pop()) {
                let v = fb.system_call(
                    "Flash.Object",
                    "hasProperty",
                    &[obj, name],
                    Type::Bool,
                );
                stack.push(v);
            }
        }

        // ====================================================================
        // Domain memory (Alchemy ops)
        // ====================================================================
        Op::Li8 => {
            if let Some(addr) = stack.pop() {
                let v = fb.system_call("Flash.Memory", "load_i8", &[addr], Type::Int(32));
                stack.push(v);
            }
        }
        Op::Li16 => {
            if let Some(addr) = stack.pop() {
                let v = fb.system_call("Flash.Memory", "load_i16", &[addr], Type::Int(32));
                stack.push(v);
            }
        }
        Op::Li32 => {
            if let Some(addr) = stack.pop() {
                let v = fb.system_call("Flash.Memory", "load_i32", &[addr], Type::Int(32));
                stack.push(v);
            }
        }
        Op::Lf32 => {
            if let Some(addr) = stack.pop() {
                let v =
                    fb.system_call("Flash.Memory", "load_f32", &[addr], Type::Float(32));
                stack.push(v);
            }
        }
        Op::Lf64 => {
            if let Some(addr) = stack.pop() {
                let v =
                    fb.system_call("Flash.Memory", "load_f64", &[addr], Type::Float(64));
                stack.push(v);
            }
        }
        Op::Si8 => {
            if let (Some(val), Some(addr)) = (stack.pop(), stack.pop()) {
                fb.system_call("Flash.Memory", "store_i8", &[addr, val], Type::Void);
            }
        }
        Op::Si16 => {
            if let (Some(val), Some(addr)) = (stack.pop(), stack.pop()) {
                fb.system_call("Flash.Memory", "store_i16", &[addr, val], Type::Void);
            }
        }
        Op::Si32 => {
            if let (Some(val), Some(addr)) = (stack.pop(), stack.pop()) {
                fb.system_call("Flash.Memory", "store_i32", &[addr, val], Type::Void);
            }
        }
        Op::Sf32 => {
            if let (Some(val), Some(addr)) = (stack.pop(), stack.pop()) {
                fb.system_call("Flash.Memory", "store_f32", &[addr, val], Type::Void);
            }
        }
        Op::Sf64 => {
            if let (Some(val), Some(addr)) = (stack.pop(), stack.pop()) {
                fb.system_call("Flash.Memory", "store_f64", &[addr, val], Type::Void);
            }
        }
        Op::Sxi1 => {
            if let Some(val) = stack.pop() {
                let v = fb.system_call("Flash.Memory", "sxi1", &[val], Type::Int(32));
                stack.push(v);
            }
        }
        Op::Sxi8 => {
            if let Some(val) = stack.pop() {
                let v = fb.system_call("Flash.Memory", "sxi8", &[val], Type::Int(32));
                stack.push(v);
            }
        }
        Op::Sxi16 => {
            if let Some(val) = stack.pop() {
                let v = fb.system_call("Flash.Memory", "sxi16", &[val], Type::Int(32));
                stack.push(v);
            }
        }

        // ====================================================================
        // XML
        // ====================================================================
        Op::EscXAttr => {
            if let Some(val) = stack.pop() {
                let v = fb.system_call(
                    "Flash.XML",
                    "escapeAttribute",
                    &[val],
                    Type::String,
                );
                stack.push(v);
            }
        }
        Op::EscXElem => {
            if let Some(val) = stack.pop() {
                let v = fb.system_call(
                    "Flash.XML",
                    "escapeElement",
                    &[val],
                    Type::String,
                );
                stack.push(v);
            }
        }
        Op::CheckFilter => {
            if let Some(val) = stack.pop() {
                let v = fb.system_call(
                    "Flash.XML",
                    "checkFilter",
                    &[val],
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::GetDescendants { index } => {
            let prop = resolve_property(pool, index, stack);
            if let Some(obj) = stack.pop() {
                let name_val = match prop {
                    PropertyAccess::Named(name) => fb.const_string(&name),
                    PropertyAccess::Indexed(idx) => idx,
                };
                let v = fb.system_call(
                    "Flash.XML",
                    "getDescendants",
                    &[obj, name_val],
                    Type::Dynamic,
                );
                stack.push(v);
            }
        }
        Op::Dxns { index } => {
            let ns = pool_string(pool, index);
            let ns_val = fb.const_string(&ns);
            fb.system_call(
                "Flash.XML",
                "setDefaultNamespace",
                &[ns_val],
                Type::Void,
            );
        }
        Op::DxnsLate => {
            if let Some(val) = stack.pop() {
                fb.system_call(
                    "Flash.XML",
                    "setDefaultNamespace",
                    &[val],
                    Type::Void,
                );
            }
        }

        // ====================================================================
        // Debug / no-ops
        // ====================================================================
        Op::Debug {
            is_local_register,
            register_name,
            register,
        } => {
            if *is_local_register && named_registers.insert(*register) {
                let name = pool_string(pool, register_name);
                if !name.is_empty() {
                    // Op::Debug registers skip `this`: register 0 = first
                    // actual parameter.  Apply offset to align with our
                    // locals[] array which includes `this` at index 0.
                    let idx = *register as usize + debug_reg_offset;
                    if idx < locals.len() {
                        if idx < num_params {
                            // Name the param value for the function signature,
                            // AND name the alloc slot so Mem2Reg can transfer
                            // the name to phi values when the param is reassigned.
                            fb.name_value(fb.param(idx), name.clone());
                            fb.name_value(locals[idx], name);
                        } else {
                            fb.name_value(locals[idx], name);
                        }
                    }
                }
            }
        }
        Op::DebugFile { .. }
        | Op::Bkpt
        | Op::BkptLine { .. }
        | Op::Timestamp
        | Op::Nop
        | Op::Label => {
            // No-op
        }
        Op::DebugLine { .. } => {
            // Could set span info on subsequent instructions, but skip for now.
        }
    }
}

/// Pop N values from the stack (in stack order, so last popped = deepest).
fn pop_n(stack: &mut Vec<ValueId>, n: usize) -> Vec<ValueId> {
    let start = stack.len().saturating_sub(n);
    let args: Vec<ValueId> = stack.drain(start..).collect();
    args
}

/// Emit a conditional branch from a single boolean value on the stack.
#[allow(clippy::too_many_arguments)]
fn emit_conditional_branch(
    fb: &mut FunctionBuilder,
    ops: &[LocatedOp],
    op_idx: usize,
    offset: i32,
    offset_map: &HashMap<usize, usize>,
    block_map: &HashMap<usize, BlockId>,
    stack: &mut Vec<ValueId>,
    block_param_values: &mut HashMap<usize, Vec<ValueId>>,
    transform: impl FnOnce(&mut FunctionBuilder, ValueId) -> ValueId,
) {
    if let Some(cond) = stack.pop() {
        let cond = transform(fb, cond);
        if let Some(target_idx) = resolve_branch_target(ops, offset_map, op_idx, offset) {
            let fallthrough_idx = op_idx + 1;
            if let (Some(&then_block), Some(&else_block)) =
                (block_map.get(&target_idx), block_map.get(&fallthrough_idx))
            {
                let then_args = prepare_branch_args(
                    fb,
                    stack,
                    then_block,
                    target_idx,
                    block_param_values,
                );
                let else_args = prepare_branch_args(
                    fb,
                    stack,
                    else_block,
                    fallthrough_idx,
                    block_param_values,
                );
                fb.br_if(cond, then_block, &then_args, else_block, &else_args);
            }
        }
    }
}

/// Emit a conditional branch from a comparison of two stack values.
#[allow(clippy::too_many_arguments)]
fn emit_comparison_branch(
    fb: &mut FunctionBuilder,
    ops: &[LocatedOp],
    op_idx: usize,
    offset: i32,
    cmp_kind: CmpKind,
    offset_map: &HashMap<usize, usize>,
    block_map: &HashMap<usize, BlockId>,
    stack: &mut Vec<ValueId>,
    block_param_values: &mut HashMap<usize, Vec<ValueId>>,
) {
    if let (Some(b), Some(a)) = (stack.pop(), stack.pop()) {
        let cond = fb.cmp(cmp_kind, a, b);
        if let Some(target_idx) = resolve_branch_target(ops, offset_map, op_idx, offset) {
            let fallthrough_idx = op_idx + 1;
            if let (Some(&then_block), Some(&else_block)) =
                (block_map.get(&target_idx), block_map.get(&fallthrough_idx))
            {
                let then_args = prepare_branch_args(
                    fb,
                    stack,
                    then_block,
                    target_idx,
                    block_param_values,
                );
                let else_args = prepare_branch_args(
                    fb,
                    stack,
                    else_block,
                    fallthrough_idx,
                    block_param_values,
                );
                fb.br_if(cond, then_block, &then_args, else_block, &else_args);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use swf::avm2::types::*;

    fn empty_pool() -> ConstantPool {
        ConstantPool {
            ints: vec![],
            uints: vec![],
            doubles: vec![],
            strings: vec![],
            namespaces: vec![],
            namespace_sets: vec![],
            multinames: vec![],
        }
    }

    fn make_abc(pool: ConstantPool, bodies: Vec<MethodBody>, methods: Vec<Method>) -> AbcFile {
        AbcFile {
            major_version: 46,
            minor_version: 16,
            constant_pool: pool,
            methods,
            metadata: vec![],
            instances: vec![],
            classes: vec![],
            scripts: vec![],
            method_bodies: bodies,
        }
    }

    #[test]
    fn translate_simple_return_void() {
        let pool = empty_pool();
        // Bytecode: returnvoid (opcode 0x47)
        let code = vec![0x47];
        let body = MethodBody {
            method: Index::new(0),
            max_stack: 1,
            num_locals: 1,
            init_scope_depth: 0,
            max_scope_depth: 1,
            code,
            exceptions: vec![],
            traits: vec![],
        };
        let method = Method {
            name: Index::new(0),
            params: vec![],
            return_type: Index::new(0),
            flags: MethodFlags::empty(),
            body: Some(Index::new(0)),
        };
        let abc = make_abc(pool, vec![body.clone()], vec![method]);
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };

        let func = translate_method_body(&abc, &body, "test", sig, &[], false).unwrap();
        let output = format!("{func}");
        assert!(output.contains("return"), "expected return in:\n{output}");
    }

    #[test]
    fn translate_push_int_and_return() {
        let pool = empty_pool();
        // pushbyte 42 (0x24 0x2A), returnvalue (0x48)
        let code = vec![0x24, 42, 0x48];
        let body = MethodBody {
            method: Index::new(0),
            max_stack: 1,
            num_locals: 1,
            init_scope_depth: 0,
            max_scope_depth: 1,
            code,
            exceptions: vec![],
            traits: vec![],
        };
        let method = Method {
            name: Index::new(0),
            params: vec![],
            return_type: Index::new(0),
            flags: MethodFlags::empty(),
            body: Some(Index::new(0)),
        };
        let abc = make_abc(pool, vec![body.clone()], vec![method]);
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(32), ..Default::default() };

        let func = translate_method_body(&abc, &body, "answer", sig, &[], false).unwrap();
        let output = format!("{func}");
        assert!(output.contains("const 42"), "expected const 42 in:\n{output}");
        assert!(output.contains("return"), "expected return in:\n{output}");
    }

    #[test]
    fn translate_add_two_bytes() {
        let pool = empty_pool();
        // pushbyte 3, pushbyte 4, add, returnvalue
        let code = vec![0x24, 3, 0x24, 4, 0xA0, 0x48];
        let body = MethodBody {
            method: Index::new(0),
            max_stack: 2,
            num_locals: 1,
            init_scope_depth: 0,
            max_scope_depth: 1,
            code,
            exceptions: vec![],
            traits: vec![],
        };
        let method = Method {
            name: Index::new(0),
            params: vec![],
            return_type: Index::new(0),
            flags: MethodFlags::empty(),
            body: Some(Index::new(0)),
        };
        let abc = make_abc(pool, vec![body.clone()], vec![method]);
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic, ..Default::default() };

        let func = translate_method_body(&abc, &body, "add_test", sig, &[], false).unwrap();
        let output = format!("{func}");
        assert!(output.contains("const 3"), "expected const 3 in:\n{output}");
        assert!(output.contains("const 4"), "expected const 4 in:\n{output}");
        assert!(output.contains("add "), "expected add in:\n{output}");
    }

    #[test]
    fn translate_getlocal_setlocal() {
        let pool = empty_pool();
        // pushbyte 10 (0x24 0x0A), setlocal 1 (0x63 0x01),
        // getlocal 1 (0x62 0x01), returnvalue (0x48)
        let code = vec![0x24, 10, 0x63, 0x01, 0x62, 0x01, 0x48];
        let body = MethodBody {
            method: Index::new(0),
            max_stack: 1,
            num_locals: 2,
            init_scope_depth: 0,
            max_scope_depth: 1,
            code,
            exceptions: vec![],
            traits: vec![],
        };
        let method = Method {
            name: Index::new(0),
            params: vec![],
            return_type: Index::new(0),
            flags: MethodFlags::empty(),
            body: Some(Index::new(0)),
        };
        let abc = make_abc(pool, vec![body.clone()], vec![method]);
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic, ..Default::default() };

        let func = translate_method_body(&abc, &body, "local_test", sig, &[], false).unwrap();
        let output = format!("{func}");
        assert!(output.contains("store"), "expected store in:\n{output}");
        assert!(output.contains("load"), "expected load in:\n{output}");
    }
}
