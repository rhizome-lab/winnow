use std::collections::HashMap;

use crate::error::CoreError;
use crate::ir::ty::parse_type_notation;
use crate::ir::{BlockId, Constant, Function, Inst, Module, Op, Type, ValueId};
use crate::pipeline::{Transform, TransformResult};

/// Type inference transform — refines `Dynamic` types to concrete types
/// by forward dataflow analysis with fixed-point iteration.
pub struct TypeInference;

/// Module-level type context built once before per-function inference.
struct ModuleContext {
    /// Struct name → field name → field type.
    struct_fields: HashMap<String, HashMap<String, Type>>,
    /// Class name → static field name → field type.
    static_fields: HashMap<String, HashMap<String, Type>>,
    /// Global name → type.
    global_types: HashMap<String, Type>,
    /// Function name → return type.
    func_return_types: HashMap<String, Type>,
    /// (class_short_name, bare_method_name) → return type.
    method_return_types: HashMap<(String, String), Type>,
    /// class_short_name → super_class_short_name.
    class_hierarchy: HashMap<String, Option<String>>,
    /// bare_method_name → return type (only for unambiguous names across all classes).
    unique_method_types: HashMap<String, Type>,
}

impl ModuleContext {
    fn from_module(module: &Module) -> Self {
        let mut struct_fields = HashMap::new();
        for s in &module.structs {
            let fields: HashMap<String, Type> = s.fields.iter().map(|(n, t, _)| (n.clone(), t.clone())).collect();
            struct_fields.insert(s.name.clone(), fields);
        }

        let global_types = module
            .globals
            .iter()
            .map(|g| (g.name.clone(), g.ty.clone()))
            .collect();

        let mut func_return_types: HashMap<String, Type> = module
            .functions
            .values()
            .map(|f| (f.name.clone(), f.sig.return_ty.clone()))
            .collect();

        // Extend with external function signatures from runtime.
        for (name, sig) in &module.external_function_sigs {
            func_return_types
                .entry(name.clone())
                .or_insert_with(|| parse_type_notation(&sig.returns));
        }

        // Build method_return_types: (class, bare_name) → return type
        let mut method_return_types = HashMap::new();
        for f in module.functions.values() {
            if f.class.is_some() {
                if let Some(bare) = f.name.rsplit("::").next() {
                    if let Some(class) = &f.class {
                        method_return_types
                            .insert((class.clone(), bare.to_string()), f.sig.return_ty.clone());
                    }
                }
            }
        }

        // Build class_hierarchy and static_fields from module.classes
        let mut class_hierarchy: HashMap<String, Option<String>> = HashMap::new();
        let mut static_fields_map: HashMap<String, HashMap<String, Type>> = HashMap::new();
        for class in &module.classes {
            let super_short = class.super_class.as_ref().map(|sc| {
                sc.rsplit("::").next().unwrap_or(sc).to_string()
            });
            class_hierarchy.insert(class.name.clone(), super_short);
            if !class.static_fields.is_empty() {
                let fields: HashMap<String, Type> =
                    class.static_fields.iter().map(|(n, t, _)| (n.clone(), t.clone())).collect();
                static_fields_map.insert(class.name.clone(), fields);
            }
        }

        // Extend with external type definitions from runtime.
        for (name, ext) in &module.external_type_defs {
            // class_hierarchy: insert with parent short name
            class_hierarchy
                .entry(name.clone())
                .or_insert_with(|| ext.extends.clone());
            // struct_fields: parse field types
            if !ext.fields.is_empty() {
                let fields: HashMap<String, Type> = ext
                    .fields
                    .iter()
                    .map(|(f, t)| (f.clone(), parse_type_notation(t)))
                    .collect();
                struct_fields.entry(name.clone()).or_default().extend(fields);
            }
            // method_return_types: parse return types
            for (method, sig) in &ext.methods {
                method_return_types
                    .entry((name.clone(), method.clone()))
                    .or_insert_with(|| parse_type_notation(&sig.returns));
            }
        }

        // Build unique_method_types: bare names that resolve to a single return type
        let mut bare_name_types: HashMap<String, Option<Type>> = HashMap::new();
        for ((_, bare), ty) in &method_return_types {
            match bare_name_types.get(bare) {
                None => {
                    bare_name_types.insert(bare.clone(), Some(ty.clone()));
                }
                Some(Some(existing)) if *existing == *ty => {}
                Some(Some(_)) => {
                    bare_name_types.insert(bare.clone(), None);
                }
                Some(None) => {}
            }
        }
        let unique_method_types = bare_name_types
            .into_iter()
            .filter_map(|(name, ty)| ty.map(|t| (name, t)))
            .collect();

        Self {
            struct_fields,
            static_fields: static_fields_map,
            global_types,
            func_return_types,
            method_return_types,
            class_hierarchy,
            unique_method_types,
        }
    }

    /// Resolve a method's return type by walking the class hierarchy.
    fn resolve_method_return_type(&self, class: &str, method: &str) -> Option<Type> {
        let mut current = Some(class.to_string());
        let max_depth = self.class_hierarchy.len();
        for _ in 0..=max_depth {
            let Some(cls) = current else { break };
            if let Some(ty) = self.method_return_types.get(&(cls.clone(), method.to_string())) {
                return Some(ty.clone());
            }
            current = self.class_hierarchy.get(&cls).and_then(|s| s.clone());
        }
        None
    }

    /// Resolve a field's type by walking the class hierarchy upward,
    /// checking both instance and static fields.
    fn resolve_field_type(&self, class: &str, field: &str) -> Option<Type> {
        let bare = field.rsplit("::").next().unwrap_or(field);
        let mut current = Some(class.to_string());
        let max_depth = self.class_hierarchy.len();
        for _ in 0..=max_depth {
            let Some(cls) = current else { break };
            // Check instance fields.
            if let Some(fields) = self.struct_fields.get(&cls) {
                if let Some(ty) = fields.get(bare).or_else(|| fields.get(field)) {
                    return Some(ty.clone());
                }
            }
            // Check static fields.
            if let Some(fields) = self.static_fields.get(&cls) {
                if let Some(ty) = fields.get(bare).or_else(|| fields.get(field)) {
                    return Some(ty.clone());
                }
            }
            current = self.class_hierarchy.get(&cls).and_then(|s| s.clone());
        }
        None
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

/// Flatten a type into a list of non-Dynamic, non-Option, non-Union members,
/// tracking nullability. This ensures unions never nest.
fn flatten_into(ty: Type, nullable: &mut bool, out: &mut Vec<Type>) {
    match ty {
        Type::Dynamic => {}
        Type::Option(inner) => {
            *nullable = true;
            flatten_into(*inner, nullable, out);
        }
        Type::Union(v) => {
            for t in v {
                flatten_into(t, nullable, out);
            }
        }
        other => {
            if !out.contains(&other) {
                out.push(other);
            }
        }
    }
}

/// Merge two types into a union, deduplicating members.
/// `Dynamic` members are dropped. `Option` is unwrapped into a nullable flag
/// and its inner type is flattened into the member list. This prevents nesting
/// from iterative inference passes.
fn union_type(a: Type, b: Type) -> Type {
    let mut nullable = false;
    let mut types = Vec::new();
    flatten_into(a, &mut nullable, &mut types);
    flatten_into(b, &mut nullable, &mut types);

    let base = match types.len() {
        0 => Type::Dynamic,
        1 => types.into_iter().next().unwrap(),
        _ => Type::Union(types),
    };
    if nullable && base != Type::Dynamic {
        Type::Option(Box::new(base))
    } else {
        base
    }
}

/// Build a map from alloc ValueId → stored type, by scanning all Store instructions.
/// If all stores to a given alloc write the same concrete type, that type is recorded.
/// If stores write different concrete types, a `Type::Union` is produced.
fn build_alloc_types(func: &Function) -> HashMap<ValueId, Type> {
    let mut alloc_stores: HashMap<ValueId, Option<Type>> = HashMap::new();

    for inst in func.insts.values() {
        if let Op::Store { ptr, value } = &inst.op {
            let stored_ty = func.value_types[*value].clone();
            let entry = alloc_stores.entry(*ptr).or_insert(None);
            match entry {
                None => *entry = Some(stored_ty),
                Some(existing) => {
                    *existing = union_type(existing.clone(), stored_ty);
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
    const_strings: &HashMap<ValueId, String>,
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
        Op::Cast(_, ty, _) => ty.clone(),

        // Load: use tracked alloc type if available.
        Op::Load(ptr) => {
            if let Some(ty) = alloc_types.get(ptr) {
                ty.clone()
            } else {
                return None;
            }
        }

        // GetField: look up struct field type, walking class hierarchy.
        Op::GetField { object, field } => {
            if let Type::Struct(name) = &func.value_types[*object] {
                ctx.resolve_field_type(name, field)
                    .unwrap_or(Type::Dynamic)
            } else {
                return None;
            }
        }

        // GetIndex: extract element type from Array or value type from Map.
        Op::GetIndex { collection, .. } => match &func.value_types[*collection] {
            Type::Array(elem_ty) => *elem_ty.clone(),
            Type::Map(_, val_ty) => *val_ty.clone(),
            _ => return None,
        },

        // Direct call: look up return type via 3-strategy chain.
        Op::Call { func: name, args } => {
            // Strategy 1: exact qualified name lookup.
            if let Some(ty) = ctx.func_return_types.get(name) {
                ty.clone()
            }
            // Strategy 2: receiver-based — if first arg is Struct(class), walk hierarchy.
            else if let Some(first) = args.first() {
                if let Type::Struct(class) = &func.value_types[*first] {
                    let bare = name.rsplit("::").next().unwrap_or(name);
                    ctx.resolve_method_return_type(class, bare)
                        .unwrap_or_else(|| {
                            // Strategy 3: unique bare name fallback.
                            ctx.unique_method_types
                                .get(bare)
                                .cloned()
                                .unwrap_or(Type::Dynamic)
                        })
                } else {
                    // No struct receiver — try unique bare name.
                    let bare = name.rsplit("::").next().unwrap_or(name);
                    ctx.unique_method_types
                        .get(bare)
                        .cloned()
                        .unwrap_or(Type::Dynamic)
                }
            } else {
                Type::Dynamic
            }
        }

        // MethodCall: use receiver type to look up method return type.
        Op::MethodCall {
            receiver,
            method,
            args: _,
        } => {
            let bare = method.rsplit("::").next().unwrap_or(method);
            if let Type::Struct(class) = &func.value_types[*receiver] {
                ctx.resolve_method_return_type(class, bare)
                    .unwrap_or_else(|| {
                        ctx.unique_method_types
                            .get(bare)
                            .cloned()
                            .unwrap_or(Type::Dynamic)
                    })
            } else {
                ctx.unique_method_types
                    .get(bare)
                    .cloned()
                    .unwrap_or(Type::Dynamic)
            }
        }

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

        // Select: infer common type of the two branches.
        Op::Select {
            on_true, on_false, ..
        } => infer_common_type(
            [&func.value_types[*on_true], &func.value_types[*on_false]].into_iter(),
        ),

        // SystemCall: infer types for known patterns.
        Op::SystemCall {
            system,
            method,
            args,
        } => {
            match (system.as_str(), method.as_str()) {
                // findPropStrict with a const string matching a known class → Struct(name).
                ("Flash.Scope", "findPropStrict") => {
                    if let Some(first) = args.first() {
                        if let Some(name) = const_strings.get(first) {
                            let bare = name.rsplit("::").next().unwrap_or(name);
                            if ctx.struct_fields.contains_key(bare)
                                || ctx.class_hierarchy.contains_key(bare)
                            {
                                Type::Struct(bare.to_string())
                            } else {
                                return None;
                            }
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                // construct: if the constructor arg is Struct(name), the result is Struct(name).
                ("Flash.Object", "construct") => {
                    if let Some(first) = args.first() {
                        if let Type::Struct(name) = &func.value_types[*first] {
                            Type::Struct(name.clone())
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        }

        // CallIndirect and everything else: keep current type.
        _ => return None,
    };

    refine(current, &inferred).map(|_| inferred)
}

/// Find the common type among an iterator of types.
/// Returns the single type if all agree, a `Union` if they differ,
/// or `Dynamic` if any input is `Dynamic` or the iterator is empty.
fn infer_common_type<'a>(mut types: impl Iterator<Item = &'a Type>) -> Type {
    let Some(first) = types.next() else {
        return Type::Dynamic;
    };
    if *first == Type::Dynamic {
        return Type::Dynamic;
    }
    let mut result = first.clone();
    for ty in types {
        if *ty == Type::Dynamic {
            return Type::Dynamic;
        }
        if *ty != result {
            result = union_type(result, ty.clone());
        }
    }
    result
}

/// Run type inference on a single function within the given module context.
/// Returns true if any types were refined.
fn infer_function(func: &mut Function, ctx: &ModuleContext) -> bool {
    let max_iters = func.value_types.len().max(1);
    let mut any_changed = false;

    // Build const_strings once — string constants don't change during inference.
    let const_strings: HashMap<ValueId, String> = func
        .insts
        .values()
        .filter_map(|inst| {
            if let Op::Const(Constant::String(s)) = &inst.op {
                Some((inst.result?, s.clone()))
            } else {
                None
            }
        })
        .collect();

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
                let new_ty = infer_inst_type(inst, func, ctx, &alloc_types, &const_strings)?;
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

    // Refine alloc instruction types from store analysis.
    let alloc_types = build_alloc_types(func);
    for block in func.blocks.values() {
        for &inst_id in &block.insts {
            let inst = &func.insts[inst_id];
            if let Op::Alloc(ref ty) = inst.op {
                if *ty == Type::Dynamic {
                    if let Some(result) = inst.result {
                        if let Some(refined) = alloc_types.get(&result) {
                            if *refined != Type::Dynamic {
                                func.insts[inst_id].op = Op::Alloc(refined.clone());
                                any_changed = true;
                            }
                        }
                    }
                }
            }
        }
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

        // Infer return types from actual Return instructions.
        for func in module.functions.values_mut() {
            if func.sig.return_ty != Type::Dynamic {
                continue;
            }
            let mut return_types: Vec<&Type> = Vec::new();
            let mut has_void_return = false;
            for inst in func.insts.values() {
                if let Op::Return(val) = &inst.op {
                    match val {
                        Some(v) => return_types.push(&func.value_types[*v]),
                        None => has_void_return = true,
                    }
                }
            }
            let inferred = if return_types.is_empty() {
                Type::Void
            } else {
                infer_common_type(return_types.into_iter())
            };
            if has_void_return && inferred != Type::Dynamic && inferred != Type::Void {
                // Mixed void + value returns — keep Dynamic.
                continue;
            }
            if inferred != Type::Dynamic && func.sig.return_ty != inferred {
                func.sig.return_ty = inferred;
                changed = true;
            }
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
    use crate::ir::{ClassDef, CmpKind, FuncId, Global, StructDef, Visibility};

    /// Constants propagate: pushbyte 42 + add should infer Int(64) for the add result.
    #[test]
    fn constants_propagate_through_arithmetic() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
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
            return_ty: Type::Int(32), ..Default::default() };
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
            return_ty: Type::String, ..Default::default() };
        let mut callee_fb =
            FunctionBuilder::new("get_name", callee_sig, Visibility::Public);
        let s = callee_fb.const_string("hello");
        callee_fb.ret(Some(s));
        let callee = callee_fb.build();

        // Create a caller that calls get_name with Dynamic return type.
        let caller_sig = FunctionSig {
            params: vec![],
            return_ty: Type::String, ..Default::default() };
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
            return_ty: Type::Int(64), ..Default::default() };
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
            namespace: Vec::new(),
            fields: vec![
                ("x".into(), Type::Int(64), None),
                ("y".into(), Type::Int(64), None),
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
            return_ty: Type::Int(64), ..Default::default() };
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

    /// Mixed types produce Union: branches sending different types → Union.
    #[test]
    fn mixed_types_produce_union() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic, ..Default::default() };
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
        // Different types → Union.
        assert_eq!(
            func.value_types[merge_vals[0]],
            Type::Union(vec![Type::Int(64), Type::String])
        );
    }

    /// GlobalRef resolves to the global's declared type.
    #[test]
    fn global_ref_type() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
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
            init: None,
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
            return_ty: Type::Bool, ..Default::default() };
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

    /// Helper: build a module with a class method and a caller that invokes it
    /// via a bare name on a typed receiver.
    fn build_method_call_module(
        class_name: &str,
        method_bare: &str,
        method_return_ty: Type,
        super_class: Option<&str>,
    ) -> (Module, ValueId) {
        // The method: Creature::isNaga -> Bool
        let method_full = format!("{class_name}::{method_bare}");
        let method_sig = FunctionSig {
            params: vec![Type::Struct(class_name.to_string())],
            return_ty: method_return_ty, ..Default::default() };
        let mut method_fb =
            FunctionBuilder::new(&method_full, method_sig, Visibility::Public);
        let self_param = method_fb.param(0);
        method_fb.ret(Some(self_param));
        let mut method_func = method_fb.build();
        method_func.class = Some(class_name.to_string());

        // The caller calls bare "isNaga" with a Struct("Creature") receiver.
        let caller_sig = FunctionSig {
            params: vec![Type::Struct(class_name.to_string())],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let recv = caller_fb.param(0);
        let result = caller_fb.call(method_bare, &[recv], Type::Dynamic);
        caller_fb.ret(Some(result));
        let caller_func = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_struct(StructDef {
            name: class_name.into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        let method_id = mb.add_function(method_func);
        mb.add_function(caller_func);
        mb.add_class(ClassDef {
            name: class_name.into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![method_id],
            super_class: super_class.map(|s| s.to_string()),
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        (mb.build(), result)
    }

    /// Method call resolved via receiver type.
    #[test]
    fn method_call_resolved_via_receiver() {
        let (module, result) =
            build_method_call_module("Creature", "isNaga", Type::Bool, None);
        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let caller = &module.functions[FuncId::new(1)];
        assert_eq!(caller.value_types[result], Type::Bool);
    }

    /// Method call resolved via hierarchy walk (method on parent class).
    #[test]
    fn method_call_resolved_via_hierarchy() {
        // Parent class "Creature" has isNaga, child "Naga" extends it.
        let parent_method_sig = FunctionSig {
            params: vec![Type::Struct("Creature".to_string())],
            return_ty: Type::Bool, ..Default::default() };
        let mut parent_fb =
            FunctionBuilder::new("Creature::isNaga", parent_method_sig, Visibility::Public);
        let self_param = parent_fb.param(0);
        parent_fb.ret(Some(self_param));
        let mut parent_func = parent_fb.build();
        parent_func.class = Some("Creature".to_string());

        // Caller has a Naga receiver, calls bare "isNaga".
        let caller_sig = FunctionSig {
            params: vec![Type::Struct("Naga".to_string())],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let recv = caller_fb.param(0);
        let result = caller_fb.call("isNaga", &[recv], Type::Dynamic);
        caller_fb.ret(Some(result));
        let caller_func = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_struct(StructDef {
            name: "Creature".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        mb.add_struct(StructDef {
            name: "Naga".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        let parent_method_id = mb.add_function(parent_func);
        mb.add_function(caller_func);
        mb.add_class(ClassDef {
            name: "Creature".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![parent_method_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        mb.add_class(ClassDef {
            name: "Naga".into(),
            namespace: Vec::new(),
            struct_index: 1,
            methods: vec![],
            super_class: Some("Creature".to_string()),
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let caller = &module.functions[FuncId::new(1)];
        assert_eq!(caller.value_types[result], Type::Bool);
    }

    /// Unique bare name fallback when receiver is not a Struct.
    #[test]
    fn method_call_unique_fallback() {
        // Only one class defines "isNaga" → unique fallback works.
        let method_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Bool, ..Default::default() };
        let mut method_fb =
            FunctionBuilder::new("Creature::isNaga", method_sig, Visibility::Public);
        let self_param = method_fb.param(0);
        method_fb.ret(Some(self_param));
        let mut method_func = method_fb.build();
        method_func.class = Some("Creature".to_string());

        // Caller with Dynamic receiver.
        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let recv = caller_fb.param(0);
        let result = caller_fb.call("isNaga", &[recv], Type::Dynamic);
        caller_fb.ret(Some(result));
        let caller_func = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_struct(StructDef {
            name: "Creature".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        let method_id = mb.add_function(method_func);
        mb.add_function(caller_func);
        mb.add_class(ClassDef {
            name: "Creature".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![method_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let caller = &module.functions[FuncId::new(1)];
        assert_eq!(caller.value_types[result], Type::Bool);
    }

    /// Select with both branches Int(64) infers Int(64).
    #[test]
    fn select_same_type_inferred() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);
        let a = fb.const_int(1);
        let b = fb.const_int(2);
        let mut func = fb.build();

        // Manually insert a Select with Dynamic result type.
        let select_val = func.value_types.push(Type::Dynamic);
        let select_inst = func.insts.push(Inst {
            op: Op::Select {
                cond,
                on_true: a,
                on_false: b,
            },
            result: Some(select_val),
            span: None,
        });
        let entry = BlockId::new(0);
        // Insert before the terminator.
        let term_pos = func.blocks[entry].insts.len() - 1;
        func.blocks[entry].insts.insert(term_pos, select_inst);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[select_val], Type::Int(64));
    }

    /// Select with mixed types produces Union.
    #[test]
    fn select_mixed_types_produces_union() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);
        let a = fb.const_int(1);
        let b = fb.const_string("hello");
        let mut func = fb.build();

        let select_val = func.value_types.push(Type::Dynamic);
        let select_inst = func.insts.push(Inst {
            op: Op::Select {
                cond,
                on_true: a,
                on_false: b,
            },
            result: Some(select_val),
            span: None,
        });
        let entry = BlockId::new(0);
        let term_pos = func.blocks[entry].insts.len() - 1;
        func.blocks[entry].insts.insert(term_pos, select_inst);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(
            func.value_types[select_val],
            Type::Union(vec![Type::Int(64), Type::String])
        );
    }

    /// Ambiguous bare name stays Dynamic when multiple classes disagree on return type.
    #[test]
    fn method_call_ambiguous_stays_dynamic() {
        // Two classes define "getValue" with different return types.
        let method1_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Bool, ..Default::default() };
        let mut method1_fb =
            FunctionBuilder::new("ClassA::getValue", method1_sig, Visibility::Public);
        let s1 = method1_fb.param(0);
        method1_fb.ret(Some(s1));
        let mut method1 = method1_fb.build();
        method1.class = Some("ClassA".to_string());

        let method2_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Int(64), ..Default::default() };
        let mut method2_fb =
            FunctionBuilder::new("ClassB::getValue", method2_sig, Visibility::Public);
        let s2 = method2_fb.param(0);
        method2_fb.ret(Some(s2));
        let mut method2 = method2_fb.build();
        method2.class = Some("ClassB".to_string());

        // Caller with Dynamic receiver.
        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let recv = caller_fb.param(0);
        let result = caller_fb.call("getValue", &[recv], Type::Dynamic);
        caller_fb.ret(Some(result));
        let caller_func = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        let m1_id = mb.add_function(method1);
        let m2_id = mb.add_function(method2);
        mb.add_function(caller_func);
        mb.add_class(ClassDef {
            name: "ClassA".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![m1_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        mb.add_class(ClassDef {
            name: "ClassB".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![m2_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let caller = &module.functions[FuncId::new(2)];
        // Ambiguous — stays Dynamic.
        assert_eq!(caller.value_types[result], Type::Dynamic);
    }

    /// Alloc(Dynamic) refined to Alloc(Int(64)) when all stores agree.
    #[test]
    fn alloc_type_refined_from_stores() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Dynamic);
        let a = fb.const_int(1);
        fb.store(ptr, a);
        let b = fb.const_int(2);
        fb.store(ptr, b);
        let loaded = fb.load(ptr, Type::Dynamic);
        fb.ret(Some(loaded));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        // The alloc op should now be Alloc(Int(64)).
        let alloc_inst = func.insts.values().find(|i| matches!(&i.op, Op::Alloc(_))).unwrap();
        match &alloc_inst.op {
            Op::Alloc(ty) => assert_eq!(*ty, Type::Int(64)),
            other => panic!("expected Alloc, got {:?}", other),
        }
    }

    /// Alloc(Dynamic) becomes Alloc(Union([Int(64), String])) when stores disagree.
    #[test]
    fn alloc_type_union_from_mixed_stores() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Dynamic);
        let a = fb.const_int(1);
        fb.store(ptr, a);
        let b = fb.const_string("hello");
        fb.store(ptr, b);
        let loaded = fb.load(ptr, Type::Dynamic);
        fb.ret(Some(loaded));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        // Mixed stores — alloc becomes Union.
        let alloc_inst = func.insts.values().find(|i| matches!(&i.op, Op::Alloc(_))).unwrap();
        match &alloc_inst.op {
            Op::Alloc(ty) => assert_eq!(*ty, Type::Union(vec![Type::Int(64), Type::String])),
            other => panic!("expected Alloc, got {:?}", other),
        }
    }

    /// Null sentinel + concrete type → Option(ConcreteType).
    #[test]
    fn alloc_type_null_sentinel_absorbed() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Dynamic);
        let a = fb.const_int(1);
        fb.store(ptr, a);
        let b = fb.const_null();
        fb.store(ptr, b);
        let loaded = fb.load(ptr, Type::Dynamic);
        fb.ret(Some(loaded));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        let alloc_inst = func.insts.values().find(|i| matches!(&i.op, Op::Alloc(_))).unwrap();
        match &alloc_inst.op {
            Op::Alloc(ty) => assert_eq!(*ty, Type::Option(Box::new(Type::Int(64)))),
            other => panic!("expected Alloc, got {:?}", other),
        }
    }

    /// Null sentinel + Dynamic → stays Dynamic (no info from null alone).
    #[test]
    fn alloc_type_null_sentinel_with_dynamic_stays_dynamic() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let ptr = fb.alloc(Type::Dynamic);
        let b = fb.const_null();
        fb.store(ptr, b);
        let loaded = fb.load(ptr, Type::Dynamic);
        fb.ret(Some(loaded));
        let mut func = fb.build();

        // Simulate an unresolved store by manually inserting a Store with Dynamic value.
        let dyn_val = func.value_types.push(Type::Dynamic);
        let store_inst = func.insts.push(Inst {
            op: Op::Store { ptr, value: dyn_val },
            result: None,
            span: None,
        });
        let entry = BlockId::new(0);
        let term_pos = func.blocks[entry].insts.len() - 1;
        func.blocks[entry].insts.insert(term_pos, store_inst);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = TypeInference;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        let alloc_inst = func.insts.values().find(|i| matches!(&i.op, Op::Alloc(_))).unwrap();
        match &alloc_inst.op {
            Op::Alloc(ty) => assert_eq!(*ty, Type::Dynamic),
            other => panic!("expected Alloc, got {:?}", other),
        }
    }
}
