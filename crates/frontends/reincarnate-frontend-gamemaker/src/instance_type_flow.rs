//! GML instance type propagation pass.
//!
//! This pass performs two transformations based on OBJT class references:
//!
//! **Step B — instance_create return types**: When `instance_create_*` (or similar)
//! is called with an OBJT class reference as the last argument, narrow the result
//! type from `Dynamic` to `Struct(class_name)`. This allows TypeScript to infer
//! the concrete return type (e.g. `OEnemy`) from the class constructor argument.
//!
//! **Step C — object_index comparisons → TypeCheck**: When game code compares
//! `inst.object_index == OEnemy`, replace the comparison with `Op::TypeCheck(inst, OEnemy)`.
//! This emits as `inst instanceof OEnemy` in TypeScript, which is both more
//! idiomatic and type-safe.
//!
//! Both steps require first building `objref_map`: a scan for `Op::GlobalRef(name)`
//! instructions where `name` is a known OBJT object name.

use std::collections::{HashMap, HashSet};

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::inst::{CmpKind, Inst, InstId, Op};
use reincarnate_core::ir::{Function, Module, ValueId};
use reincarnate_core::ir::ty::Type;
use reincarnate_core::pipeline::{Transform, TransformResult};

/// GML instance type propagation pass.
///
/// `obj_names` is the set of all OBJT class names (e.g. `{"OEnemy", "Player"}`).
/// Must be run after TypeInference so `Dynamic` result types are already
/// assigned to instance-creation calls.
pub struct GmlInstanceTypeFlow {
    pub obj_names: HashSet<String>,
}

/// Functions whose last arg is an OBJT class and whose return value is an instance.
const INSTANCE_CREATE_FUNCS: &[&str] = &[
    "instance_create",
    "instance_create_depth",
    "instance_create_layer",
    "instance_find",
    "instance_nearest",
    "instance_furthest",
];

impl Transform for GmlInstanceTypeFlow {
    fn name(&self) -> &str {
        "gml-instance-type-flow"
    }

    fn run_once(&self) -> bool {
        true
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func in module.functions.values_mut() {
            changed |= self.process_function(func);
        }
        Ok(TransformResult { module, changed })
    }
}

impl GmlInstanceTypeFlow {
    fn process_function(&self, func: &mut Function) -> bool {
        let mut changed = false;

        // --- Step A: build objref_map ---
        // Map ValueId → class_name for every Op::GlobalRef whose name is an OBJT.
        let objref_map: HashMap<ValueId, &str> = func
            .insts
            .iter()
            .filter_map(|(_, inst)| {
                if let Op::GlobalRef(name) = &inst.op {
                    if self.obj_names.contains(name) {
                        inst.result.map(|vid| (vid, name.as_str()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        if objref_map.is_empty() {
            return false;
        }

        // --- Step B: narrow instance_create_* return types ---
        // Collect: (result_vid, class_name) for Op::Call to INSTANCE_CREATE_FUNCS
        // where any arg is in objref_map.
        let type_updates: Vec<(ValueId, String)> = func
            .insts
            .iter()
            .filter_map(|(_, inst)| {
                let (func_name, args) = match &inst.op {
                    Op::Call { func, args } => (func.as_str(), args.as_slice()),
                    _ => return None,
                };
                if !INSTANCE_CREATE_FUNCS.contains(&func_name) {
                    return None;
                }
                let result_vid = inst.result?;
                // Scan args for an OBJT reference.
                let class_name = args.iter().find_map(|a| objref_map.get(a).copied())?;
                Some((result_vid, class_name.to_string()))
            })
            .collect();

        for (vid, class_name) in type_updates {
            if func.value_types[vid] == Type::Dynamic {
                func.value_types[vid] = Type::Struct(class_name);
                changed = true;
            }
        }

        // --- Step C: rewrite object_index == OEnemy → TypeCheck ---
        // Collect: (inst_id, object_vid, class_name, negate) for Cmp(Eq|Ne, lhs, rhs)
        // where one side comes from GetField(x, "object_index") and the other is in objref_map.
        let cmp_rewrites: Vec<(InstId, ValueId, String, bool)> = func
            .insts
            .iter()
            .filter_map(|(id, inst)| {
                let (kind, lhs, rhs) = match &inst.op {
                    Op::Cmp(k @ (CmpKind::Eq | CmpKind::Ne), l, r) => (*k, *l, *r),
                    _ => return None,
                };
                let negate = kind == CmpKind::Ne;

                // Check if lhs is a GetField(_, "object_index") and rhs is OBJT ref.
                if let Some(class_name) = objref_map.get(&rhs) {
                    if let Some(object_vid) = get_field_object(func, lhs, "object_index") {
                        return Some((id, object_vid, class_name.to_string(), negate));
                    }
                }
                // Check the other way: rhs is GetField, lhs is OBJT ref.
                if let Some(class_name) = objref_map.get(&lhs) {
                    if let Some(object_vid) = get_field_object(func, rhs, "object_index") {
                        return Some((id, object_vid, class_name.to_string(), negate));
                    }
                }
                None
            })
            .collect();

        for (inst_id, object_vid, class_name, negate) in cmp_rewrites {
            let type_check_op = Op::TypeCheck(object_vid, Type::Struct(class_name));
            if negate {
                // Replace Cmp(Ne, ...) with Not(TypeCheck(...)).
                //
                // The original instruction's result ValueId is used by downstream
                // consumers. We keep it on the outer `Not`. The inner `TypeCheck`
                // gets a fresh ValueId.
                //
                // Before:  inst_id: Cmp(Ne, ...) → original_vid
                // After:   inst_id: TypeCheck(...)    → typecheck_vid  (new)
                //          not_id:  Not(typecheck_vid) → original_vid
                //
                // The `Not` instruction is inserted immediately after `inst_id` in
                // the containing block's instruction list.
                let original_vid = func.insts[inst_id].result;
                let typecheck_vid = func.value_types.push(Type::Bool);
                func.insts[inst_id].op = type_check_op;
                func.insts[inst_id].result = Some(typecheck_vid);

                let not_inst_id = func.insts.push(Inst {
                    op: Op::Not(typecheck_vid),
                    result: original_vid,
                    span: None,
                });
                if let Some(vid) = original_vid {
                    func.value_types[vid] = Type::Bool;
                }

                // Find the block that contains inst_id and insert not_inst_id after it.
                'outer: for block in func.blocks.values_mut() {
                    for (pos, &bid) in block.insts.iter().enumerate() {
                        if bid == inst_id {
                            block.insts.insert(pos + 1, not_inst_id);
                            break 'outer;
                        }
                    }
                }
            } else {
                func.insts[inst_id].op = type_check_op;
                if let Some(vid) = func.insts[inst_id].result {
                    func.value_types[vid] = Type::Bool;
                }
            }
            changed = true;
        }

        changed
    }
}

/// If `value` is the result of `Op::GetField { object, field }` with the given field name,
/// return `object`. Otherwise return `None`.
fn get_field_object(func: &Function, value: ValueId, field_name: &str) -> Option<ValueId> {
    func.insts.iter().find_map(|(_, inst)| {
        if inst.result == Some(value) {
            if let Op::GetField { object, field } = &inst.op {
                if field == field_name {
                    return Some(*object);
                }
            }
        }
        None
    })
}
