//! AVM2 class/instance → IR StructDef + method functions.

use reincarnate_core::ir::{
    ClassDef, Function, FunctionSig, MethodKind, Module, ModuleBuilder, StructDef, Type, Visibility,
};
use swf::avm2::types::{AbcFile, ConstantPool, Index, Trait, TraitKind};

use crate::multiname::{pool_string, resolve_multiname_index, resolve_multiname_structured, resolve_type, NsKind};
use crate::translate::translate_method_body;

/// Information about a translated class.
pub struct ClassInfo {
    pub name: String,
    pub namespace: Vec<String>,
    pub struct_def: StructDef,
    pub functions: Vec<Function>,
    pub super_class: Option<String>,
}

/// Translate a single AVM2 class (Instance + Class pair) into IR.
pub fn translate_class(abc: &AbcFile, class_idx: usize) -> Result<ClassInfo, String> {
    let instance = &abc.instances[class_idx];
    let class = &abc.classes[class_idx];
    let pool = &abc.constant_pool;

    // Resolve class name with structured namespace info.
    let (class_short_name, class_ns) =
        if let Some(qn) = resolve_multiname_structured(pool, &instance.name) {
            (qn.name, qn.namespace)
        } else {
            (resolve_multiname_index(pool, &instance.name), Vec::new())
        };

    // Resolve the private namespace string for this class so we can strip
    // redundant prefixes from trait names.
    let class_private_ns = find_private_ns_string(pool, &instance.name);

    // Resolve superclass name.
    let super_class = if instance.super_name.0 != 0 {
        Some(resolve_multiname_index(pool, &instance.super_name))
    } else {
        None
    };

    // Build StructDef from instance slot/const traits (fields).
    let fields = extract_fields(pool, &instance.traits);
    let struct_def = StructDef {
        name: class_short_name.clone(),
        namespace: class_ns.clone(),
        fields,
        visibility: Visibility::Public,
    };

    let mut functions = Vec::new();

    // Constructor: Instance.init_method
    if let Some(mut func) = translate_class_method(
        abc,
        &instance.init_method,
        &format!("{class_short_name}::new"),
        true,
        Some(&class_short_name),
    )? {
        func.namespace = class_ns.clone();
        func.class = Some(class_short_name.clone());
        func.method_kind = MethodKind::Constructor;
        functions.push(func);
    }

    // Instance methods from traits
    for trait_ in &instance.traits {
        if let Some(method_idx) = trait_method_index(trait_) {
            let bare_name = resolve_trait_bare_name(pool, trait_, class_private_ns.as_deref());
            let method_kind = trait_to_method_kind(&trait_.kind, false);
            let prefix = method_prefix(&trait_.kind);
            let func_name = format!("{class_short_name}::{prefix}{bare_name}");
            let visibility = trait_visibility(pool, trait_);
            if let Some(mut func) =
                translate_class_method(abc, &method_idx, &func_name, true, Some(&class_short_name))?
            {
                func.namespace = class_ns.clone();
                func.class = Some(class_short_name.clone());
                func.method_kind = method_kind;
                func.visibility = visibility;
                functions.push(func);
            }
        }
    }

    // Static initializer: Class.init_method
    if let Some(mut func) = translate_class_method(
        abc,
        &class.init_method,
        &format!("{class_short_name}::cinit"),
        false,
        None,
    )? {
        func.namespace = class_ns.clone();
        func.class = Some(class_short_name.clone());
        func.method_kind = MethodKind::Static;
        func.visibility = Visibility::Private;
        functions.push(func);
    }

    // Static methods from class traits
    for trait_ in &class.traits {
        if let Some(method_idx) = trait_method_index(trait_) {
            let bare_name = resolve_trait_bare_name(pool, trait_, class_private_ns.as_deref());
            let method_kind = trait_to_method_kind(&trait_.kind, true);
            let prefix = method_prefix(&trait_.kind);
            let func_name = format!("{class_short_name}::{prefix}{bare_name}");
            let visibility = trait_visibility(pool, trait_);
            if let Some(mut func) =
                translate_class_method(abc, &method_idx, &func_name, false, None)?
            {
                func.namespace = class_ns.clone();
                func.class = Some(class_short_name.clone());
                func.method_kind = method_kind;
                func.visibility = visibility;
                functions.push(func);
            }
        }
    }

    Ok(ClassInfo {
        name: class_short_name,
        namespace: class_ns,
        struct_def,
        functions,
        super_class,
    })
}

/// Extract slot/const trait fields as IR struct fields.
fn extract_fields(pool: &ConstantPool, traits: &[Trait]) -> Vec<(String, Type)> {
    let mut fields = Vec::new();
    for trait_ in traits {
        match &trait_.kind {
            TraitKind::Slot { type_name, .. } | TraitKind::Const { type_name, .. } => {
                let name = resolve_multiname_index(pool, &trait_.name);
                let ty = resolve_type(pool, type_name);
                fields.push((name, ty));
            }
            _ => {}
        }
    }
    fields
}

/// Get the method index from a trait that references a method.
fn trait_method_index(trait_: &Trait) -> Option<Index<swf::avm2::types::Method>> {
    match &trait_.kind {
        TraitKind::Method { method, .. }
        | TraitKind::Getter { method, .. }
        | TraitKind::Setter { method, .. }
        | TraitKind::Function { function: method, .. } => Some(*method),
        _ => None,
    }
}

/// Prefix for getter/setter methods in the function name.
fn method_prefix(kind: &TraitKind) -> &'static str {
    match kind {
        TraitKind::Getter { .. } => "get_",
        TraitKind::Setter { .. } => "set_",
        _ => "",
    }
}

/// Convert a trait kind to an IR `MethodKind`.
fn trait_to_method_kind(kind: &TraitKind, is_static: bool) -> MethodKind {
    match kind {
        TraitKind::Getter { .. } => MethodKind::Getter,
        TraitKind::Setter { .. } => MethodKind::Setter,
        TraitKind::Method { .. } | TraitKind::Function { .. } => {
            if is_static {
                MethodKind::Static
            } else {
                MethodKind::Instance
            }
        }
        _ => MethodKind::Free,
    }
}

/// Resolve the bare name of a trait, stripping redundant private namespace
/// prefixes that AVM2 duplicates from the class path.
fn resolve_trait_bare_name(
    pool: &ConstantPool,
    trait_: &Trait,
    class_private_ns: Option<&str>,
) -> String {
    // Try structured resolution first.
    if let Some(qn) = resolve_multiname_structured(pool, &trait_.name) {
        // If the trait's namespace is Private and matches the class's private
        // namespace, just use the bare name (strips the redundant prefix).
        if qn.ns_kind == NsKind::Private {
            if let Some(cpns) = class_private_ns {
                let trait_ns = qn.namespace.join(".");
                if trait_ns == cpns {
                    return qn.name;
                }
            }
        }
        return qn.name;
    }
    resolve_multiname_index(pool, &trait_.name)
}

/// Determine visibility from a trait's namespace kind.
fn trait_visibility(pool: &ConstantPool, trait_: &Trait) -> Visibility {
    if let Some(qn) = resolve_multiname_structured(pool, &trait_.name) {
        match qn.ns_kind {
            NsKind::Package | NsKind::PackageInternal | NsKind::Namespace => Visibility::Public,
            NsKind::Protected | NsKind::StaticProtected => Visibility::Protected,
            NsKind::Private | NsKind::Explicit => Visibility::Private,
        }
    } else {
        Visibility::Public
    }
}

/// Find the string of the private namespace associated with a class multiname.
fn find_private_ns_string(pool: &ConstantPool, class_name_idx: &Index<swf::avm2::types::Multiname>) -> Option<String> {
    // The class name's QName uses its own namespace. Private namespaces on
    // traits typically have the same string as the class's qualified path.
    if let Some(qn) = resolve_multiname_structured(pool, class_name_idx) {
        if qn.namespace.is_empty() {
            Some(qn.name.clone())
        } else {
            Some(format!("{}.{}", qn.namespace.join("."), qn.name))
        }
    } else {
        None
    }
}

/// Translate a method (by its index) into an IR function.
///
/// `has_self` indicates whether the first parameter is an implicit `this`.
/// `class_name` provides the owning class name so `this` can be typed as `Struct(name)`.
fn translate_class_method(
    abc: &AbcFile,
    method_idx: &Index<swf::avm2::types::Method>,
    func_name: &str,
    has_self: bool,
    class_name: Option<&str>,
) -> Result<Option<Function>, String> {
    let idx = method_idx.0 as usize;
    if idx >= abc.methods.len() {
        return Ok(None);
    }
    let method = &abc.methods[idx];
    let pool = &abc.constant_pool;

    // Find the method body
    let body = abc
        .method_bodies
        .iter()
        .find(|b| b.method.0 == method_idx.0);
    let Some(body) = body else {
        return Ok(None); // Native method, no body
    };

    // Build parameter types and extract parameter names.
    let mut param_types = Vec::new();
    let mut param_names: Vec<Option<String>> = Vec::new();
    if has_self {
        if let Some(name) = class_name {
            param_types.push(Type::Struct(name.to_string()));
        } else {
            param_types.push(Type::Dynamic);
        }
        param_names.push(None); // `this` — backend handles via self_value
    }
    for param in &method.params {
        param_types.push(resolve_type(pool, &param.kind));
        let name = param.name.as_ref().map(|idx| pool_string(pool, idx)).filter(|s| !s.is_empty());
        param_names.push(name);
    }

    let return_type = resolve_type(pool, &method.return_type);

    let sig = FunctionSig {
        params: param_types,
        return_ty: return_type,
    };

    let func = translate_method_body(abc, body, func_name, sig, &param_names)?;
    Ok(Some(func))
}

/// Translate all classes and scripts in an ABC file into an IR module.
pub fn translate_abc_to_module(abc: &AbcFile, module_name: &str) -> Result<Module, String> {
    let mut mb = ModuleBuilder::new(module_name);

    // Translate classes
    for i in 0..abc.instances.len() {
        let info = translate_class(abc, i)?;
        let struct_index = mb.struct_count();
        mb.add_struct(info.struct_def);

        let mut method_ids = Vec::new();
        for func in info.functions {
            let fid = mb.add_function(func);
            method_ids.push(fid);
        }

        mb.add_class(ClassDef {
            name: info.name,
            namespace: info.namespace,
            struct_index,
            methods: method_ids,
            super_class: info.super_class,
            visibility: Visibility::Public,
        });
    }

    // Translate script initializers
    for (i, script) in abc.scripts.iter().enumerate() {
        let func_name = format!("script{i}::init");
        let method_idx = &script.init_method;
        let idx = method_idx.0 as usize;
        if idx < abc.methods.len() {
            let method = &abc.methods[idx];
            let body = abc
                .method_bodies
                .iter()
                .find(|b| b.method.0 == method_idx.0);
            if let Some(body) = body {
                let return_type = resolve_type(&abc.constant_pool, &method.return_type);
                let sig = FunctionSig {
                    params: vec![],
                    return_ty: return_type,
                };
                let func = translate_method_body(abc, body, &func_name, sig, &[])?;
                mb.add_function(func);
            }
        }
    }

    Ok(mb.build())
}
