//! GML class-reference resolution IR pass.
//!
//! In GMS1 games, object type indices are pushed as plain integer constants —
//! there is no `pushref` (Break -11) instruction.  So a call like
//! `instance_create(x, y, 2)` generates `Op::Const(Int(2))` rather than a
//! `Op::GlobalRef("Enemy", ClassRef)`.  This pass finds those plain integer
//! (or `Cast(Int(N), Dynamic, Coerce)`) arguments at positions declared as
//! `"classref"` in `runtime.json`, and replaces them with a fresh
//! `Op::GlobalRef(name)` instruction whose type is `Type::ClassRef(name)`.
//!
//! Every backend then sees already-resolved class references in the IR,
//! instead of having to reimplement the lookup logic per backend.

use std::collections::HashMap;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::inst::{CastKind, Inst, InstId, Op};
use reincarnate_core::ir::ty::Type;
use reincarnate_core::ir::{Constant, Function, Module, ValueId};
use reincarnate_core::pipeline::{Transform, TransformResult};

/// GML class-reference resolution pass.
///
/// Replaces integer constants at `"classref"`-typed parameter positions in
/// calls to external functions with `Op::GlobalRef(name)` instructions typed
/// `Type::ClassRef(name)`.
pub struct GmlClassRefResolve;

impl Transform for GmlClassRefResolve {
    fn name(&self) -> &str {
        "gml-classref-resolve"
    }

    fn run_once(&self) -> bool {
        true
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        if module.object_names.is_empty() || module.external_function_sigs.is_empty() {
            return Ok(TransformResult { module, changed: false });
        }

        // Build a map: function name → list of param indices with "classref" type.
        let classref_params: HashMap<String, Vec<usize>> = module
            .external_function_sigs
            .iter()
            .filter_map(|(name, sig)| {
                let indices: Vec<usize> = sig
                    .params
                    .iter()
                    .enumerate()
                    .filter(|(_, p)| p.as_str() == "classref")
                    .map(|(i, _)| i)
                    .collect();
                if indices.is_empty() {
                    None
                } else {
                    Some((name.clone(), indices))
                }
            })
            .collect();

        if classref_params.is_empty() {
            return Ok(TransformResult { module, changed: false });
        }

        let object_names = module.object_names.clone();
        let mut changed = false;

        for func in module.functions.values_mut() {
            changed |= resolve_function(func, &classref_params, &object_names);
        }

        Ok(TransformResult { module, changed })
    }
}

/// Process a single function: find call sites with "classref" params whose
/// argument is a plain integer constant (or a Coerce cast thereof), and
/// replace each such argument with a new `Op::GlobalRef` value.
fn resolve_function(
    func: &mut Function,
    classref_params: &HashMap<String, Vec<usize>>,
    object_names: &[String],
) -> bool {
    // Build helper maps: value → integer constant it holds.
    //   const_ints:      result of Op::Const(Int(N))   → N
    //   cast_const_ints: result of Op::Cast(v, Dynamic, Coerce) where v is in const_ints → N
    let const_ints: HashMap<ValueId, i64> = func
        .insts
        .iter()
        .filter_map(|(_, inst)| {
            if let Op::Const(Constant::Int(n)) = &inst.op {
                inst.result.map(|v| (v, *n))
            } else {
                None
            }
        })
        .collect();

    let cast_const_ints: HashMap<ValueId, i64> = func
        .insts
        .iter()
        .filter_map(|(_, inst)| {
            if let Op::Cast(inner, Type::Dynamic, CastKind::Coerce) = &inst.op {
                let n = const_ints.get(inner).copied()?;
                inst.result.map(|v| (v, n))
            } else {
                None
            }
        })
        .collect();

    // Collect rewrites: (call_inst_id, arg_index, new_obj_name).
    // We need the block positions to insert the new GlobalRef instructions.
    let mut rewrites: Vec<(InstId, usize, String)> = Vec::new();

    for (inst_id, inst) in func.insts.iter() {
        let Op::Call { func: callee, args } = &inst.op else {
            continue;
        };
        let Some(indices) = classref_params.get(callee.as_str()) else {
            continue;
        };
        for &param_idx in indices {
            let Some(&arg_val) = args.get(param_idx) else {
                continue;
            };
            // Resolve the integer value — plain Const or Coerce-cast Const.
            let n = match const_ints.get(&arg_val).copied().or_else(|| cast_const_ints.get(&arg_val).copied()) {
                Some(n) => n,
                None => continue,
            };
            // Skip negative sentinel values (-1 all, -2 noone, etc.).
            if n < 0 {
                continue;
            }
            let Some(obj_name) = object_names.get(n as usize) else {
                continue;
            };
            rewrites.push((inst_id, param_idx, obj_name.clone()));
        }
    }

    if rewrites.is_empty() {
        return false;
    }

    // For each rewrite, allocate a new ValueId + InstId for GlobalRef, then
    // patch the call's arg list and insert the GlobalRef instruction immediately
    // before the call in its block.
    for (call_inst_id, param_idx, obj_name) in rewrites {
        let new_vid = func.value_types.push(Type::ClassRef(obj_name.clone()));
        let new_inst_id = func.insts.push(Inst {
            op: Op::GlobalRef(obj_name),
            result: Some(new_vid),
            span: None,
        });

        // Patch the call's argument list.
        if let Op::Call { args, .. } = &mut func.insts[call_inst_id].op {
            if let Some(arg) = args.get_mut(param_idx) {
                *arg = new_vid;
            }
        }

        // Insert the GlobalRef instruction immediately before the call in
        // whichever block contains the call.
        for block in func.blocks.values_mut() {
            if let Some(pos) = block.insts.iter().position(|&id| id == call_inst_id) {
                block.insts.insert(pos, new_inst_id);
                break;
            }
        }
    }

    true
}
