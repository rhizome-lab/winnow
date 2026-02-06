//! AVM2 class/instance → IR StructDef + method functions.

use reincarnate_core::ir::{
    Function, FunctionSig, Module, ModuleBuilder, StructDef, Type, Visibility,
};
use swf::avm2::types::{AbcFile, ConstantPool, Index, Trait, TraitKind};

use crate::multiname::{resolve_multiname_index, resolve_type};
use crate::translate::translate_method_body;

/// Information about a translated class.
pub struct ClassInfo {
    pub name: String,
    pub struct_def: StructDef,
    pub functions: Vec<Function>,
}

/// Translate a single AVM2 class (Instance + Class pair) into IR.
pub fn translate_class(abc: &AbcFile, class_idx: usize) -> Result<ClassInfo, String> {
    let instance = &abc.instances[class_idx];
    let class = &abc.classes[class_idx];
    let pool = &abc.constant_pool;

    let class_name = resolve_multiname_index(pool, &instance.name);

    // Build StructDef from instance slot/const traits (fields).
    let fields = extract_fields(pool, &instance.traits);
    let struct_def = StructDef {
        name: class_name.clone(),
        fields,
        visibility: Visibility::Public,
    };

    let mut functions = Vec::new();

    // Constructor: Instance.init_method
    if let Some(func) = translate_class_method(
        abc,
        &instance.init_method,
        &format!("{class_name}::new"),
        true,
    )? {
        functions.push(func);
    }

    // Instance methods from traits
    for trait_ in &instance.traits {
        if let Some(method_idx) = trait_method_index(trait_) {
            let trait_name = resolve_multiname_index(pool, &trait_.name);
            let prefix = method_prefix(&trait_.kind);
            let func_name = format!("{class_name}::{prefix}{trait_name}");
            if let Some(func) = translate_class_method(abc, &method_idx, &func_name, true)? {
                functions.push(func);
            }
        }
    }

    // Static initializer: Class.init_method
    if let Some(func) = translate_class_method(
        abc,
        &class.init_method,
        &format!("{class_name}::cinit"),
        false,
    )? {
        functions.push(func);
    }

    // Static methods from class traits
    for trait_ in &class.traits {
        if let Some(method_idx) = trait_method_index(trait_) {
            let trait_name = resolve_multiname_index(pool, &trait_.name);
            let prefix = method_prefix(&trait_.kind);
            let func_name = format!("{class_name}::{prefix}{trait_name}");
            if let Some(func) = translate_class_method(abc, &method_idx, &func_name, false)? {
                functions.push(func);
            }
        }
    }

    Ok(ClassInfo {
        name: class_name,
        struct_def,
        functions,
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

/// Translate a method (by its index) into an IR function.
///
/// `has_self` indicates whether the first parameter is an implicit `this`.
fn translate_class_method(
    abc: &AbcFile,
    method_idx: &Index<swf::avm2::types::Method>,
    func_name: &str,
    has_self: bool,
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

    // Build parameter types
    let mut param_types = Vec::new();
    if has_self {
        param_types.push(Type::Dynamic); // `this`
    }
    for param in &method.params {
        param_types.push(resolve_type(pool, &param.kind));
    }

    let return_type = resolve_type(pool, &method.return_type);

    let sig = FunctionSig {
        params: param_types,
        return_ty: return_type,
    };

    let func = translate_method_body(abc, body, func_name, sig)?;
    Ok(Some(func))
}

/// Translate all classes and scripts in an ABC file into an IR module.
pub fn translate_abc_to_module(abc: &AbcFile, module_name: &str) -> Result<Module, String> {
    let mut mb = ModuleBuilder::new(module_name);

    // Translate classes
    for i in 0..abc.instances.len() {
        let info = translate_class(abc, i)?;
        mb.add_struct(info.struct_def);
        for func in info.functions {
            mb.add_function(func);
        }
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
                let func = translate_method_body(abc, body, &func_name, sig)?;
                mb.add_function(func);
            }
        }
    }

    Ok(mb.build())
}
