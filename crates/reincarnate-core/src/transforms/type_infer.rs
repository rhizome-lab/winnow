use std::collections::HashMap;

use crate::error::CoreError;
use crate::ir::{BlockId, Function, Inst, Module, Op, Type, ValueId};
use crate::pipeline::{Transform, TransformResult};

/// Type inference transform — refines `Dynamic` types to concrete types
/// by forward dataflow analysis with fixed-point iteration.
pub struct TypeInference;

/// Module-level type context built once before per-function inference.
struct ModuleContext {
    /// Struct name → field name → field type.
    struct_fields: HashMap<String, HashMap<String, Type>>,
    /// Global name → type.
    global_types: HashMap<String, Type>,
    /// Function name → return type.
    func_return_types: HashMap<String, Type>,
}

impl ModuleContext {
    fn from_module(module: &Module) -> Self {
        let mut struct_fields = HashMap::new();
        for s in &module.structs {
            let fields: HashMap<String, Type> = s.fields.iter().cloned().collect();
            struct_fields.insert(s.name.clone(), fields);
        }

        let global_types = module
            .globals
            .iter()
            .map(|g| (g.name.clone(), g.ty.clone()))
            .collect();

        let func_return_types = module
            .functions
            .values()
            .map(|f| (f.name.clone(), f.sig.return_ty.clone()))
            .collect();

        Self {
            struct_fields,
            global_types,
            func_return_types,
        }
    }
}

/// Replace `old` with `new` only if `old` is `Dynamic` and `new` is not.
/// Returns `true` if the type changed.
fn refine(old: &Type, new: &Type) -> Option<Type> {
    if *old == Type::Dynamic && *new != Type::Dynamic {
        Some(new.clone())
    } else {
        None
    }
}

/// Build a map from alloc ValueId → stored type, by scanning all Store instructions.
/// If all stores to a given alloc write the same concrete type, that type is recorded.
fn build_alloc_types(func: &Function) -> HashMap<ValueId, Type> {
    let mut alloc_stores: HashMap<ValueId, Option<Type>> = HashMap::new();

    for inst in func.insts.values() {
        if let Op::Store { ptr, value } = &inst.op {
            let stored_ty = func.value_types[*value].clone();
            let entry = alloc_stores.entry(*ptr).or_insert(None);
            match entry {
                None => *entry = Some(stored_ty),
                Some(existing) => {
                    if *existing != stored_ty {
                        // Mixed types — mark as Dynamic (won't refine).
                        *existing = Type::Dynamic;
                    }
                }
            }
        }
    }

    alloc_stores
        .into_iter()
        .filter_map(|(ptr, ty)| ty.map(|t| (ptr, t)))
        .collect()
}

/// Collect incoming branch arguments for each (block, param_index) pair.
fn collect_branch_args(func: &Function) -> HashMap<(BlockId, usize), Vec<ValueId>> {
    let mut incoming: HashMap<(BlockId, usize), Vec<ValueId>> = HashMap::new();

    for inst in func.insts.values() {
        let targets = branch_targets(&inst.op);
        for (block, args) in targets {
            for (i, arg) in args.iter().enumerate() {
                incoming.entry((block, i)).or_default().push(*arg);
            }
        }
    }

    incoming
}

/// Extract (target_block, args) pairs from a branch instruction.
fn branch_targets(op: &Op) -> Vec<(BlockId, &[ValueId])> {
    match op {
        Op::Br { target, args } => vec![(*target, args.as_slice())],
        Op::BrIf {
            then_target,
            then_args,
            else_target,
            else_args,
            ..
        } => vec![
            (*then_target, then_args.as_slice()),
            (*else_target, else_args.as_slice()),
        ],
        Op::Switch {
            cases, default, ..
        } => {
            let mut targets: Vec<(BlockId, &[ValueId])> =
                cases.iter().map(|(_, b, a)| (*b, a.as_slice())).collect();
            targets.push((default.0, default.1.as_slice()));
            targets
        }
        _ => vec![],
    }
}

/// Infer the type of an instruction's result given the current value types.
fn infer_inst_type(
    inst: &Inst,
    func: &Function,
    ctx: &ModuleContext,
    alloc_types: &HashMap<ValueId, Type>,
) -> Option<Type> {
    let result = inst.result?;
    let current = &func.value_types[result];

    let inferred = match &inst.op {
        Op::Const(c) => c.ty(),

        // Arithmetic: propagate type of first operand.
        Op::Add(a, _)
        | Op::Sub(a, _)
        | Op::Mul(a, _)
        | Op::Div(a, _)
        | Op::Rem(a, _) => func.value_types[*a].clone(),
        Op::Neg(a) => func.value_types[*a].clone(),

        // Bitwise: propagate type of first operand.
        Op::BitAnd(a, _)
        | Op::BitOr(a, _)
        | Op::BitXor(a, _)
        | Op::Shl(a, _)
        | Op::Shr(a, _) => func.value_types[*a].clone(),
        Op::BitNot(a) => func.value_types[*a].clone(),

        // Comparison and logic always produce Bool.
        Op::Cmp(..) | Op::Not(_) | Op::TypeCheck(..) => Type::Bool,

        // Cast always produces the target type.
        Op::Cast(_, ty) => ty.clone(),

        // Load: use tracked alloc type if available.
        Op::Load(ptr) => {
            if let Some(ty) = alloc_types.get(ptr) {
                ty.clone()
            } else {
                return None;
            }
        }

        // GetField: look up struct field type.
        Op::GetField { object, field } => {
            if let Type::Struct(name) = &func.value_types[*object] {
                ctx.struct_fields
                    .get(name)
                    .and_then(|fields| fields.get(field))
                    .cloned()
                    .unwrap_or(Type::Dynamic)
            } else {
                return None;
            }
        }

        // GetIndex: extract element type from Array.
        Op::GetIndex { collection, .. } => {
            if let Type::Array(elem_ty) = &func.value_types[*collection] {
                *elem_ty.clone()
            } else {
                return None;
            }
        }

        // Direct call: look up return type.
        Op::Call { func: name, .. } => ctx
            .func_return_types
            .get(name)
            .cloned()
            .unwrap_or(Type::Dynamic),

        // Copy: propagate source type.
        Op::Copy(v) => func.value_types[*v].clone(),

        // StructInit: always Struct(name).
        Op::StructInit { name, .. } => Type::Struct(name.clone()),

        // ArrayInit: infer element type from elements.
        Op::ArrayInit(elems) => {
            let elem_ty = infer_common_type(elems.iter().map(|v| &func.value_types[*v]));
            Type::Array(Box::new(elem_ty))
        }

        // TupleInit: collect element types.
        Op::TupleInit(elems) => {
            Type::Tuple(elems.iter().map(|v| func.value_types[*v].clone()).collect())
        }

        // GlobalRef: look up global type.
        Op::GlobalRef(name) => ctx
            .global_types
            .get(name)
            .cloned()
            .unwrap_or(Type::Dynamic),

        // SystemCall, CallIndirect, and everything else: keep current type.
        _ => return None,
    };

    refine(current, &inferred).map(|_| inferred)
}

/// Find the common type among an iterator of types.
/// Returns Dynamic if types disagree or the iterator is empty.
fn infer_common_type<'a>(mut types: impl Iterator<Item = &'a Type>) -> Type {
    let Some(first) = types.next() else {
        return Type::Dynamic;
    };
    if *first == Type::Dynamic {
        return Type::Dynamic;
    }
    for ty in types {
        if ty != first {
            return Type::Dynamic;
        }
    }
    first.clone()
}

/// Run type inference on a single function within the given module context.
/// Returns true if any types were refined.
fn infer_function(func: &mut Function, ctx: &ModuleContext) -> bool {
    let max_iters = func.value_types.len().max(1);
    let mut any_changed = false;

    for _ in 0..max_iters {
        let mut changed = false;

        // Rebuild alloc types each iteration (they depend on value_types).
        let alloc_types = build_alloc_types(func);

        // Forward pass over all instructions.
        // Collect updates first, then apply (avoids borrow conflict).
        let updates: Vec<(ValueId, Type)> = func
            .insts
            .values()
            .filter_map(|inst| {
                let result = inst.result?;
                let new_ty = infer_inst_type(inst, func, ctx, &alloc_types)?;
                Some((result, new_ty))
            })
            .collect();

        for (vid, ty) in updates {
            func.value_types[vid] = ty;
            changed = true;
        }

        // Block parameter refinement: check incoming branch arguments.
        let incoming = collect_branch_args(func);
        for (block_id, block) in func.blocks.iter() {
            for (i, param) in block.params.iter().enumerate() {
                if let Some(args) = incoming.get(&(block_id, i)) {
                    let common = infer_common_type(args.iter().map(|v| &func.value_types[*v]));
                    if let Some(refined) = refine(&func.value_types[param.value], &common) {
                        func.value_types[param.value] = refined;
                        changed = true;
                    }
                }
            }
        }

        if !changed {
            break;
        }
        any_changed = true;
    }

    // Sync BlockParam.ty fields with value_types.
    for block in func.blocks.keys().collect::<Vec<_>>() {
        let param_vals: Vec<(usize, Type)> = func.blocks[block]
            .params
            .iter()
            .enumerate()
            .filter_map(|(i, p)| {
                let vty = &func.value_types[p.value];
                if p.ty != *vty {
                    Some((i, vty.clone()))
                } else {
                    None
                }
            })
            .collect();
        for (i, ty) in param_vals {
            func.blocks[block].params[i].ty = ty;
            any_changed = true;
        }
    }

    any_changed
}

impl Transform for TypeInference {
    fn name(&self) -> &str {
        "type-inference"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let ctx = ModuleContext::from_module(&module);
        let mut changed = false;
        for func in module.functions.keys().collect::<Vec<_>>() {
            changed |= infer_function(&mut module.functions[func], &ctx);
        }
        Ok(TransformResult {
            module,
            changed,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityRef;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::ty::FunctionSig;
    use crate::ir::{CmpKind, FuncId, Global, StructDef, Visibility};

    /// Constants propagate: pushbyte 42 + add should infer Int(64) for the add result.
    #[test]
    fn constants_propagate_through_arithmetic() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(42);
        let b = fb.const_int(10);
        // Manually create add with Dynamic type to simulate flash frontend output.
        let sum = fb.add(a, b);
        fb.ret(Some(sum));
        let mut func = fb.build();

        // Force the add result to Dynamic (simulating untyped frontend).
        func.value_types[sum] = Type::Dynamic;

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[sum], Type::Int(64));
    }

    /// Local variable tracking: store Int(32) to alloc, load should infer Int(32).
    #[test]
    fn local_variable_tracking() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(32),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Int(32));
        let val = fb.const_int(42);
        fb.store(ptr, val);
        let loaded = fb.load(ptr, Type::Dynamic); // Frontend doesn't know the type.
        fb.ret(Some(loaded));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[loaded], Type::Int(64)); // Constant::Int is always Int(64).
    }

    /// Call return type: call to known function gets the function's return type.
    #[test]
    fn call_return_type() {
        // Create a callee function.
        let callee_sig = FunctionSig {
            params: vec![],
            return_ty: Type::String,
        };
        let mut callee_fb =
            FunctionBuilder::new("get_name", callee_sig, Visibility::Public);
        let s = callee_fb.const_string("hello");
        callee_fb.ret(Some(s));
        let callee = callee_fb.build();

        // Create a caller that calls get_name with Dynamic return type.
        let caller_sig = FunctionSig {
            params: vec![],
            return_ty: Type::String,
        };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let result = caller_fb.call("get_name", &[], Type::Dynamic);
        caller_fb.ret(Some(result));
        let caller = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(callee);
        mb.add_function(caller);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let caller_func = &module.functions[FuncId::new(1)];
        assert_eq!(caller_func.value_types[result], Type::String);
    }

    /// Struct field type: GetField on a known struct resolves field type.
    #[test]
    fn struct_field_type() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let vx = fb.const_int(10);
        let vy = fb.const_int(20);
        let obj = fb.struct_init("Point", vec![
            ("x".into(), vx),
            ("y".into(), vy),
        ]);
        let x = fb.get_field(obj, "x", Type::Dynamic);
        fb.ret(Some(x));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![
                ("x".into(), Type::Int(64)),
                ("y".into(), Type::Int(64)),
            ],
            visibility: Visibility::Public,
        });
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[x], Type::Int(64));
    }

    /// Block parameter join: two branches sending Int(32) to a block param → param becomes Int(32).
    #[test]
    fn block_parameter_join_same_type() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);

        // Merge block with a Dynamic param.
        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Dynamic]);
        let then_block = fb.create_block();
        let else_block = fb.create_block();

        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let a = fb.const_int(1);
        fb.br(merge, &[a]);

        fb.switch_to_block(else_block);
        let b = fb.const_int(2);
        fb.br(merge, &[b]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_vals[0]));

        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        // Both branches send Int(64), so the merge param should be Int(64).
        assert_eq!(func.value_types[merge_vals[0]], Type::Int(64));
        // BlockParam.ty should also be synced.
        assert_eq!(func.blocks[merge].params[0].ty, Type::Int(64));
    }

    /// Mixed types stay Dynamic: branches sending different types → param stays Dynamic.
    #[test]
    fn mixed_types_stay_dynamic() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);

        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Dynamic]);
        let then_block = fb.create_block();
        let else_block = fb.create_block();

        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let a = fb.const_int(1); // Int(64)
        fb.br(merge, &[a]);

        fb.switch_to_block(else_block);
        let b = fb.const_string("hello"); // String
        fb.br(merge, &[b]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_vals[0]));

        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        // Different types → stays Dynamic.
        assert_eq!(func.value_types[merge_vals[0]], Type::Dynamic);
    }

    /// GlobalRef resolves to the global's declared type.
    #[test]
    fn global_ref_type() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64),
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let g = fb.global_ref("counter", Type::Dynamic);
        fb.ret(Some(g));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_global(Global {
            name: "counter".into(),
            ty: Type::Int(64),
            visibility: Visibility::Private,
            mutable: true,
        });
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[g], Type::Int(64));
    }

    /// Comparison operations always produce Bool.
    #[test]
    fn comparison_produces_bool() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool,
        };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(1);
        let b = fb.const_int(2);
        let cmp = fb.cmp(CmpKind::Lt, a, b);
        fb.ret(Some(cmp));
        let mut func = fb.build();

        // Force cmp result to Dynamic.
        func.value_types[cmp] = Type::Dynamic;

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[cmp], Type::Bool);
    }
}
