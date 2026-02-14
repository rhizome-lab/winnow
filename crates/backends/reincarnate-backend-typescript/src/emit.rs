use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Write;
use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{
    structurize, CastKind, ClassDef, Constant, ExternalImport, FuncId, Function, MethodKind,
    Module, Op, StructDef, Type, Visibility,
};
use reincarnate_core::pipeline::LoweringConfig;
use reincarnate_core::project::{ExternalTypeDef, RuntimeConfig};

use crate::js_ast::{JsExpr, JsFunction, JsStmt};
use crate::runtime::SYSTEM_NAMES;
use crate::types::ts_type;

/// Which engine's rewrite pass to use.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EngineKind {
    Flash,
    GameMaker,
}

/// Detect engine from runtime config system_modules keys.
fn detect_engine(runtime_config: Option<&RuntimeConfig>) -> EngineKind {
    if let Some(cfg) = runtime_config {
        if cfg.system_modules.keys().any(|k| k.starts_with("GameMaker.")) {
            return EngineKind::GameMaker;
        }
    }
    EngineKind::Flash
}

/// Emit a single module into `output_dir`.
///
/// If the module has classes, emits a directory with one file per class plus
/// a barrel `index.ts`. Otherwise emits a flat `.ts` file.
pub fn emit_module(
    module: &mut Module,
    output_dir: &Path,
    lowering_config: &LoweringConfig,
    runtime_config: Option<&RuntimeConfig>,
) -> Result<(), CoreError> {
    if module.classes.is_empty() {
        let out = emit_module_to_string(module, lowering_config, runtime_config)?;
        let path = output_dir.join(format!("{}.ts", module.name));
        fs::write(&path, &out).map_err(CoreError::Io)?;
    } else {
        emit_module_to_dir(module, output_dir, lowering_config, runtime_config)?;
    }
    Ok(())
}

/// Emit a module to a string (flat output — for testing or class-free modules).
pub fn emit_module_to_string(module: &mut Module, lowering_config: &LoweringConfig, runtime_config: Option<&RuntimeConfig>) -> Result<String, CoreError> {
    let mut out = String::new();
    let class_names = build_class_names(module);
    let empty_type_defs = BTreeMap::new();
    let type_defs = runtime_config.map(|c| &c.type_definitions).unwrap_or(&empty_type_defs);
    let class_meta = ClassMeta::build(module, type_defs);
    let mut known_classes: HashSet<String> = class_names.values().cloned().collect();
    if let Some(rc) = runtime_config {
        known_classes.extend(rc.type_definitions.keys().cloned());
    }

    let engine = detect_engine(runtime_config);
    emit_runtime_imports(module, &mut out, runtime_config, engine);
    if let Some(preamble) = runtime_config.and_then(|c| c.class_preamble.as_ref()) {
        let _ = writeln!(
            out,
            "import {{ {} }} from \"./runtime/{}\";",
            preamble.names.join(", "),
            preamble.path,
        );
        out.push('\n');
    }
    emit_imports(module, &mut out);
    emit_structs(module, &mut out);
    emit_enums(module, &mut out);
    emit_globals(module, &mut out);

    // Single-file mode — globals are in the same scope, no ESM setter rewrite needed.
    let no_mutable_globals = HashSet::new();
    if module.classes.is_empty() {
        emit_functions(module, &class_names, &known_classes, &no_mutable_globals, lowering_config, engine, &mut out)?;
    } else {
        // Single-file mode: no circular imports (all classes in one scope).
        let no_late_bound = HashSet::new();
        let no_short_to_qualified = HashMap::new();
        let (class_groups, free_funcs) = group_by_class(module);
        for group in &class_groups {
            emit_class(group, module, &class_names, &class_meta, &no_mutable_globals, &no_late_bound, &no_short_to_qualified, &known_classes, lowering_config, engine, &mut out)?;
        }
        for &fid in &free_funcs {
            emit_function(&mut module.functions[fid], &class_names, &known_classes, &no_mutable_globals, lowering_config, engine, &mut out)?;
        }
    }

    Ok(out)
}

// ---------------------------------------------------------------------------
// ClassRegistry — maps qualified names to filesystem paths for imports
// ---------------------------------------------------------------------------

pub(crate) struct ClassEntry {
    pub(crate) short_name: String,
    /// Path segments from module root, e.g. ["classes", "Scenes", "Swamp", "Swamp"].
    pub(crate) path_segments: Vec<String>,
}

pub(crate) struct ClassRegistry {
    /// Keyed by both qualified name and bare name (fallback).
    pub(crate) classes: HashMap<String, ClassEntry>,
}

impl ClassRegistry {
    fn from_module(module: &Module) -> Self {
        let mut classes = HashMap::new();
        for class in &module.classes {
            let short = sanitize_ident(&class.name);
            let mut segments: Vec<String> =
                class.namespace.iter().map(|s| sanitize_ident(s)).collect();
            segments.push(short.clone());

            // Key by qualified name: "classes.Scenes.Areas.Swamp::CorruptedDriderScene"
            let qualified = qualified_class_name(class);
            classes.insert(
                qualified,
                ClassEntry {
                    short_name: short.clone(),
                    path_segments: segments.clone(),
                },
            );
            // Also key by bare name for fallback lookup (if not already taken).
            classes.entry(short.clone()).or_insert(ClassEntry {
                short_name: short,
                path_segments: segments,
            });
        }
        Self { classes }
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<&ClassEntry> {
        self.classes.get(name).or_else(|| {
            // Try extracting the short name after `::`
            let short = name.rsplit("::").next()?;
            self.classes.get(short)
        })
    }
}

/// Build a map from qualified class names to sanitized short names.
fn build_class_names(module: &Module) -> HashMap<String, String> {
    module
        .classes
        .iter()
        .map(|c| (qualified_class_name(c), sanitize_ident(&c.name)))
        .collect()
}

/// Pre-computed class hierarchy metadata used during emission.
struct ClassMeta {
    ancestor_sets: HashMap<String, HashSet<String>>,
    method_name_sets: HashMap<String, HashSet<String>>,
    instance_field_sets: HashMap<String, HashSet<String>>,
    static_method_owner_map: HashMap<String, HashMap<String, String>>,
    static_field_owner_map: HashMap<String, HashMap<String, String>>,
    /// Instance/Free method names that are bindable (excludes getters, setters, statics, constructors).
    bindable_method_sets: HashMap<String, HashSet<String>>,
}

impl ClassMeta {
    fn build(module: &Module, type_defs: &BTreeMap<String, ExternalTypeDef>) -> Self {
        Self {
            ancestor_sets: build_ancestor_sets(module, type_defs),
            method_name_sets: build_method_name_sets(module, type_defs),
            instance_field_sets: build_instance_field_sets(module, type_defs),
            static_method_owner_map: build_static_method_owner_map(module),
            static_field_owner_map: build_static_field_owner_map(module),
            bindable_method_sets: build_bindable_method_sets(module, type_defs),
        }
    }
}

/// Collect all member names (fields + methods) for an external type,
/// walking its `extends` chain through type_defs.
fn collect_external_members(
    start: &str,
    type_defs: &BTreeMap<String, ExternalTypeDef>,
) -> HashSet<String> {
    let mut members = HashSet::new();
    let mut current = Some(start);
    while let Some(name) = current {
        if let Some(def) = type_defs.get(name) {
            members.extend(def.fields.keys().cloned());
            members.extend(def.methods.keys().cloned());
            current = def.extends.as_deref();
        } else {
            break;
        }
    }
    members
}

/// Check whether an external type (or any of its ancestors) is marked `open`,
/// meaning instances may have arbitrary dynamic fields. Returns `true` if any
/// type in the inheritance chain has `open: true`.
fn is_open_type(start: &str, type_defs: &BTreeMap<String, ExternalTypeDef>) -> bool {
    let mut current = Some(start);
    while let Some(name) = current {
        if let Some(def) = type_defs.get(name) {
            if def.open {
                return true;
            }
            current = def.extends.as_deref();
        } else {
            break;
        }
    }
    false
}

/// Validate member accesses in a function against known type definitions.
///
/// Checks `GetField` and `SetField` operations: if the object has a known
/// `Struct` type, verifies that the field exists in the class hierarchy's
/// instance fields, method names (getters/setters), or static fields.
/// Falls back to external type definitions for pure-external types.
fn validate_member_accesses(
    func: &Function,
    function_class: Option<&str>,
    class_meta: &ClassMeta,
    registry: &ClassRegistry,
    short_to_qualified: &HashMap<String, String>,
    type_defs: &BTreeMap<String, ExternalTypeDef>,
) {
    for (_iid, inst) in func.insts.iter() {
        let (object, field) = match &inst.op {
            Op::GetField { object, field } => (*object, field.as_str()),
            Op::SetField { object, field, .. } => (*object, field.as_str()),
            _ => continue,
        };
        let bare = field.rsplit("::").next().unwrap_or(field);
        // Skip fields that are themselves class names (constructor references).
        if registry.lookup(field).is_some() || type_defs.contains_key(bare) {
            continue;
        }
        let ty = &func.value_types[object];
        let type_name = match ty {
            Type::Struct(name) => name.as_str(),
            _ => continue,
        };
        let short = type_name.rsplit("::").next().unwrap_or(type_name);
        // Try direct qualified-name lookup first (handles duplicate short names).
        // Fall back to function's own class (disambiguates collisions like two
        // classes with the same short name), then to short-name lookup.
        let qualified = if class_meta.instance_field_sets.contains_key(type_name)
            || class_meta.method_name_sets.contains_key(type_name)
        {
            type_name
        } else if let Some(fc) = function_class {
            let fc_short = fc.rsplit("::").next().unwrap_or(fc);
            if fc_short == short {
                fc
            } else {
                match short_to_qualified.get(short) {
                    Some(qn) => qn.as_str(),
                    None => {
                        if type_defs.contains_key(short)
                            && !is_open_type(short, type_defs)
                        {
                            let ext_members = collect_external_members(short, type_defs);
                            if !ext_members.contains(bare) {
                                eprintln!(
                                    "warning: {short} has no member '{bare}' (in {})",
                                    func.name
                                );
                            }
                        }
                        continue;
                    }
                }
            }
        } else {
            match short_to_qualified.get(short) {
                Some(qn) => qn.as_str(),
                None => {
                    // Pure-external type — validate against type_defs.
                    if type_defs.contains_key(short)
                        && !is_open_type(short, type_defs)
                    {
                        let ext_members = collect_external_members(short, type_defs);
                        if !ext_members.contains(bare) {
                            eprintln!(
                                "warning: {short} has no member '{bare}' (in {})",
                                func.name
                            );
                        }
                    }
                    continue;
                }
            }
        };
        let has_instance_field = class_meta
            .instance_field_sets
            .get(qualified)
            .is_some_and(|f| f.contains(bare));
        let has_method = class_meta
            .method_name_sets
            .get(qualified)
            .is_some_and(|m| {
                m.contains(bare)
                    || m.contains(&format!("get_{bare}"))
                    || m.contains(&format!("set_{bare}"))
            });
        let has_static_field = class_meta
            .static_field_owner_map
            .get(qualified)
            .is_some_and(|m| m.contains_key(bare));
        let has_static_method = class_meta
            .static_method_owner_map
            .get(qualified)
            .is_some_and(|m| m.contains_key(bare));
        if !has_instance_field && !has_method && !has_static_field && !has_static_method {
            // Final fallback: check external type_defs (handles local interfaces
            // that also have external type definitions with field metadata).
            if type_defs.contains_key(short) {
                if is_open_type(short, type_defs) {
                    continue;
                }
                let ext_members = collect_external_members(short, type_defs);
                if ext_members.contains(bare) {
                    continue;
                }
            }
            // Check if any ancestor in the class hierarchy is an open external type
            // (e.g. GML objects extend GMLObject which has open: true).
            if let Some(ancestors) = class_meta.ancestor_sets.get(qualified) {
                if ancestors.iter().any(|a| is_open_type(a, type_defs)) {
                    continue;
                }
            }
            eprintln!(
                "warning: {short} has no member '{bare}' (in {})",
                func.name
            );
        }
    }
}

/// Resolve a super_class string to a ClassDef, trying qualified name first
/// (handles duplicate short names) then falling back to short-name lookup.
fn resolve_parent<'a>(
    sc: &str,
    class_by_qualified: &HashMap<String, &'a ClassDef>,
    class_by_short: &HashMap<&str, &'a ClassDef>,
) -> Option<&'a ClassDef> {
    // Try qualified first (e.g. "Items.Armors::GooArmor").
    if let Some(parent) = class_by_qualified.get(sc) {
        return Some(parent);
    }
    // Fall back to short name.
    let short = sc.rsplit("::").next().unwrap_or(sc);
    class_by_short.get(short).copied()
}

/// Build a map from qualified class name to the set of ancestor short names.
///
/// For each class, the set includes the class's own short name and the short
/// names of all superclasses reachable via `super_class` links within the module.
fn build_ancestor_sets(module: &Module, type_defs: &BTreeMap<String, ExternalTypeDef>) -> HashMap<String, HashSet<String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();
    let class_by_qualified: HashMap<String, &ClassDef> =
        module.classes.iter().map(|c| (qualified_class_name(c), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut ancestors = HashSet::new();
        ancestors.insert(class.name.clone());
        let mut current = class;
        let mut external_start: Option<&str> = None;
        while let Some(ref sc) = current.super_class {
            let short = sc.rsplit("::").next().unwrap_or(sc);
            ancestors.insert(short.to_string());
            match resolve_parent(sc, &class_by_qualified, &class_by_short) {
                Some(parent) => current = parent,
                None => {
                    external_start = Some(short);
                    break;
                }
            }
        }
        // Continue walking through external type definitions.
        let mut ext_cur = external_start;
        while let Some(ext_name) = ext_cur {
            if let Some(def) = type_defs.get(ext_name) {
                ext_cur = def.extends.as_deref();
                if let Some(parent) = ext_cur {
                    ancestors.insert(parent.to_string());
                }
            } else {
                break;
            }
        }
        result.insert(qualified_class_name(class), ancestors);
    }
    result
}

/// Build a mapping from qualified class name → set of all method short names
/// visible through the class hierarchy (own methods + all ancestor methods).
fn build_method_name_sets(module: &Module, type_defs: &BTreeMap<String, ExternalTypeDef>) -> HashMap<String, HashSet<String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();
    let class_by_qualified: HashMap<String, &ClassDef> =
        module.classes.iter().map(|c| (qualified_class_name(c), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut names = HashSet::new();
        let mut current = class;
        let mut external_parent: Option<&str> = None;
        loop {
            for &fid in &current.methods {
                if let Some(f) = module.functions.get(fid) {
                    if !matches!(f.method_kind, MethodKind::Static | MethodKind::Closure) {
                        if let Some(short) = f.name.rsplit("::").next() {
                            names.insert(short.to_string());
                            // Getters/setters use get_/set_ prefix in their function
                            // name, but are accessed as bare property names in AVM2
                            // (via GetProperty/SetProperty, not explicit calls).
                            // Add the un-prefixed property name so resolve_field can
                            // recognise them as instance members.
                            match f.method_kind {
                                MethodKind::Getter => {
                                    if let Some(prop) = short.strip_prefix("get_") {
                                        names.insert(prop.to_string());
                                    }
                                }
                                MethodKind::Setter => {
                                    if let Some(prop) = short.strip_prefix("set_") {
                                        names.insert(prop.to_string());
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            match current.super_class {
                Some(ref sc) => {
                    let short = sc.rsplit("::").next().unwrap_or(sc);
                    match resolve_parent(sc, &class_by_qualified, &class_by_short) {
                        Some(parent) => current = parent,
                        None => {
                            external_parent = Some(short);
                            break;
                        }
                    }
                }
                None => break,
            }
        }
        // Continue walking through external type definitions.
        let mut ext_cur = external_parent;
        while let Some(ext_name) = ext_cur {
            if let Some(def) = type_defs.get(ext_name) {
                names.extend(def.methods.keys().cloned());
                ext_cur = def.extends.as_deref();
            } else {
                break;
            }
        }
        result.insert(qualified_class_name(class), names);
    }
    result
}

/// Build a mapping from qualified class name → set of bindable method short names.
/// Only includes Instance and Free methods — excludes Getter, Setter, Static,
/// and Constructor.  Does NOT include bare getter/setter property names.
fn build_bindable_method_sets(module: &Module, type_defs: &BTreeMap<String, ExternalTypeDef>) -> HashMap<String, HashSet<String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();
    let class_by_qualified: HashMap<String, &ClassDef> =
        module.classes.iter().map(|c| (qualified_class_name(c), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut names = HashSet::new();
        let mut current = class;
        let mut external_parent: Option<&str> = None;
        loop {
            for &fid in &current.methods {
                if let Some(f) = module.functions.get(fid) {
                    if matches!(f.method_kind, MethodKind::Instance | MethodKind::Free) {
                        if let Some(short) = f.name.rsplit("::").next() {
                            names.insert(short.to_string());
                        }
                    }
                }
            }
            match current.super_class {
                Some(ref sc) => {
                    let short = sc.rsplit("::").next().unwrap_or(sc);
                    match resolve_parent(sc, &class_by_qualified, &class_by_short) {
                        Some(parent) => current = parent,
                        None => {
                            external_parent = Some(short);
                            break;
                        }
                    }
                }
                None => break,
            }
        }
        // Continue walking through external type definitions.
        let mut ext_cur = external_parent;
        while let Some(ext_name) = ext_cur {
            if let Some(def) = type_defs.get(ext_name) {
                names.extend(def.methods.keys().cloned());
                ext_cur = def.extends.as_deref();
            } else {
                break;
            }
        }
        result.insert(qualified_class_name(class), names);
    }
    result
}

/// Build a mapping from qualified class name → map of static method short names
/// to the owning class short name, across the full ancestor chain.  Most-derived
/// class wins when multiple ancestors define the same static method.
fn build_static_method_owner_map(module: &Module) -> HashMap<String, HashMap<String, String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();
    let class_by_qualified: HashMap<String, &ClassDef> =
        module.classes.iter().map(|c| (qualified_class_name(c), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut owners: HashMap<String, String> = HashMap::new();
        let mut current = class;
        loop {
            for &fid in &current.methods {
                if let Some(f) = module.functions.get(fid) {
                    if f.method_kind == MethodKind::Static {
                        if let Some(short) = f.name.rsplit("::").next() {
                            owners
                                .entry(short.to_string())
                                .or_insert_with(|| current.name.clone());
                        }
                    }
                }
            }
            match current.super_class {
                Some(ref sc) => {
                    match resolve_parent(sc, &class_by_qualified, &class_by_short) {
                        Some(parent) => current = parent,
                        None => break,
                    }
                }
                None => break,
            }
        }
        result.insert(qualified_class_name(class), owners);
    }
    result
}

/// Build a mapping from qualified class name → map of static field short name →
/// owning class short name, walking the ancestor chain. This mirrors
/// `build_static_method_owner_map` but for static fields (both `readonly` with
/// values and mutable ones assigned in cinit).
fn build_static_field_owner_map(module: &Module) -> HashMap<String, HashMap<String, String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();
    let class_by_qualified: HashMap<String, &ClassDef> =
        module.classes.iter().map(|c| (qualified_class_name(c), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut owners: HashMap<String, String> = HashMap::new();
        let mut current = class;
        loop {
            for (name, _, _) in &current.static_fields {
                owners
                    .entry(name.clone())
                    .or_insert_with(|| current.name.clone());
            }
            match current.super_class {
                Some(ref sc) => {
                    match resolve_parent(sc, &class_by_qualified, &class_by_short) {
                        Some(parent) => current = parent,
                        None => break,
                    }
                }
                None => break,
            }
        }
        result.insert(qualified_class_name(class), owners);
    }
    result
}

/// Build a mapping from qualified class name → set of all instance field short
/// names visible through the class hierarchy (own fields + all ancestor fields).
fn build_instance_field_sets(module: &Module, type_defs: &BTreeMap<String, ExternalTypeDef>) -> HashMap<String, HashSet<String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();
    let class_by_qualified: HashMap<String, &ClassDef> =
        module.classes.iter().map(|c| (qualified_class_name(c), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut fields = HashSet::new();
        let mut current = class;
        let mut external_parent: Option<&str> = None;
        loop {
            let struct_def = &module.structs[current.struct_index];
            for (name, _ty, _) in &struct_def.fields {
                fields.insert(name.clone());
            }
            match current.super_class {
                Some(ref sc) => {
                    let short = sc.rsplit("::").next().unwrap_or(sc);
                    match resolve_parent(sc, &class_by_qualified, &class_by_short) {
                        Some(parent) => current = parent,
                        None => {
                            external_parent = Some(short);
                            break;
                        }
                    }
                }
                None => break,
            }
        }
        // Continue walking through external type definitions.
        let mut ext_cur = external_parent;
        while let Some(ext_name) = ext_cur {
            if let Some(def) = type_defs.get(ext_name) {
                fields.extend(def.fields.keys().cloned());
                ext_cur = def.extends.as_deref();
            } else {
                break;
            }
        }
        result.insert(qualified_class_name(class), fields);
    }
    result
}

/// Build a qualified name from a ClassDef's namespace + name.
fn qualified_class_name(class: &ClassDef) -> String {
    if class.namespace.is_empty() {
        class.name.clone()
    } else {
        format!("{}::{}", class.namespace.join("."), class.name)
    }
}

/// Compute a relative import path between two sets of path segments.
///
/// Both `from` and `to` are file-level segments (the last element is the
/// filename without extension).
fn relative_import_path(from: &[String], to: &[String]) -> String {
    // Find common prefix length (only among directory segments, not filenames).
    let from_dirs = from.len().saturating_sub(1);
    let to_dirs = to.len().saturating_sub(1);
    let common = from[..from_dirs]
        .iter()
        .zip(to[..to_dirs].iter())
        .take_while(|(a, b)| a == b)
        .count();

    // Go up from `from`'s directory.
    let ups = from_dirs - common;
    let mut parts = Vec::new();
    if ups == 0 {
        parts.push(".".to_string());
    } else {
        for _ in 0..ups {
            parts.push("..".to_string());
        }
    }
    // Go down into `to`'s remaining path.
    for seg in &to[common..] {
        parts.push(seg.clone());
    }
    parts.join("/")
}

/// Emit a module as a directory with one `.ts` file per class in nested dirs.
pub fn emit_module_to_dir(module: &mut Module, output_dir: &Path, lowering_config: &LoweringConfig, runtime_config: Option<&RuntimeConfig>) -> Result<(), CoreError> {
    let module_dir = output_dir.join(&module.name);
    fs::create_dir_all(&module_dir).map_err(CoreError::Io)?;

    let (class_groups, free_funcs) = group_by_class(module);
    let free_func_names: HashSet<String> = free_funcs
        .iter()
        .map(|&fid| module.functions[fid].name.clone())
        .collect();
    let registry = ClassRegistry::from_module(module);
    let class_names = build_class_names(module);
    let empty_type_defs = BTreeMap::new();
    let type_defs = runtime_config.map(|c| &c.type_definitions).unwrap_or(&empty_type_defs);
    let empty_mod_exports = BTreeMap::new();
    let module_exports = runtime_config.map(|c| &c.module_exports).unwrap_or(&empty_mod_exports);
    let class_meta = ClassMeta::build(module, type_defs);
    let global_names: HashSet<String> = module.globals.iter().map(|g| g.name.clone()).collect();
    let mutable_global_names: HashSet<String> = module.globals.iter()
        .filter(|g| g.mutable)
        .map(|g| g.name.clone())
        .collect();
    let short_to_qualified: HashMap<String, String> = module
        .classes
        .iter()
        .map(|c| (c.name.clone(), qualified_class_name(c)))
        .collect();
    let mut known_classes: HashSet<String> = class_names.values().cloned().collect();
    if let Some(rc) = runtime_config {
        known_classes.extend(rc.type_definitions.keys().cloned());
    }
    let engine = detect_engine(runtime_config);
    let mut barrel_exports: Vec<String> = Vec::new();

    // Globals → _globals.ts (at module root).
    if !module.globals.is_empty() {
        let mut out = String::new();

        // Collect type imports for Struct-typed globals.
        let mut type_imports: BTreeSet<String> = BTreeSet::new();
        for global in &module.globals {
            collect_global_type_imports(&global.ty, &registry, &mut type_imports);
        }
        for short_name in &type_imports {
            if let Some(entry) = registry.classes.get(short_name) {
                let rel = format!("./{}", entry.path_segments.join("/"));
                let _ = writeln!(out, "import type {{ {short_name} }} from \"{rel}\";");
            }
        }
        if !type_imports.is_empty() {
            out.push('\n');
        }

        for global in &module.globals {
            // const without initializer is invalid JS — demote to let.
            let kw = if global.mutable || global.init.is_none() { "let" } else { "const" };
            let ident = sanitize_ident(&global.name);
            let ts = ts_type(&global.ty);
            if let Some(val) = &global.init {
                let _ = writeln!(
                    out,
                    "export {kw} {ident}: {ts} = {};",
                    crate::ast_printer::emit_constant(val)
                );
            } else {
                let _ = writeln!(out, "export {kw} {ident}: {ts};");
            }
            // ESM setter for mutable globals — imports are read-only bindings.
            if global.mutable {
                let _ = writeln!(
                    out,
                    "export function $set_{ident}(v: {ts}) {{ {ident} = v; }}"
                );
            }
        }
        let path = module_dir.join("_globals.ts");
        fs::write(&path, &out).map_err(CoreError::Io)?;
        barrel_exports.push("_globals".to_string());
    }

    // Pre-collect all classes' direct value imports for cycle detection.
    let mut direct_value_imports: HashMap<String, BTreeSet<String>> = HashMap::new();
    for group in &class_groups {
        let qualified = qualified_class_name(&group.class_def);
        let empty_smo = HashMap::new();
        let smo = class_meta.static_method_owner_map.get(&qualified).unwrap_or(&empty_smo);
        let sfo = class_meta.static_field_owner_map.get(&qualified).unwrap_or(&empty_smo);
        let refs = collect_class_references(group, module, &registry, &module.external_imports, smo, sfo, &global_names, engine);
        direct_value_imports.insert(
            sanitize_ident(&group.class_def.name),
            refs.value_refs,
        );
    }
    let transitive_value_imports = compute_transitive_value_imports(&direct_value_imports);

    for group in &class_groups {
        let class_def = &group.class_def;
        let short_name = sanitize_ident(&class_def.name);

        // Path segments for this class: namespace segments + class name.
        let mut segments: Vec<String> =
            class_def.namespace.iter().map(|s| sanitize_ident(s)).collect();
        segments.push(short_name.clone());

        // Depth = number of namespace segments (directories below module_dir).
        let depth = class_def.namespace.len();

        // Create nested directory.
        let mut file_dir = module_dir.clone();
        for seg in &class_def.namespace {
            file_dir = file_dir.join(sanitize_ident(seg));
        }
        fs::create_dir_all(&file_dir).map_err(CoreError::Io)?;

        let mut out = String::new();
        let class_funcs = || group.methods.iter().map(|&fid| &module.functions[fid]);
        let systems = collect_system_names_from_funcs(class_funcs());
        emit_runtime_imports_for(systems, &mut out, depth, runtime_config);
        let calls = collect_call_names_from_funcs(class_funcs(), engine);
        let func_prefix = "../".repeat(depth + 1);
        let func_prefix = func_prefix.trim_end_matches('/');
        emit_function_imports_with_prefix(&calls, &mut out, func_prefix, runtime_config);
        emit_free_function_imports(&calls, &free_func_names, depth, &mut out);
        if let Some(preamble) = runtime_config.and_then(|c| c.class_preamble.as_ref()) {
            let prefix = "../".repeat(depth + 1);
            let prefix = prefix.trim_end_matches('/');
            let _ = writeln!(
                out,
                "import {{ {} }} from \"{prefix}/runtime/{}\";",
                preamble.names.join(", "),
                preamble.path,
            );
            out.push('\n');
        }
        let qualified = qualified_class_name(&group.class_def);
        let empty_smo = HashMap::new();
        let static_method_owners = class_meta.static_method_owner_map.get(&qualified).unwrap_or(&empty_smo);
        let static_field_owners = class_meta.static_field_owner_map.get(&qualified).unwrap_or(&empty_smo);
        let late_bound = emit_intra_imports(group, module, &segments, &registry, static_method_owners, static_field_owners, &global_names, &mutable_global_names, module_exports, &transitive_value_imports, &short_to_qualified, depth, engine, &mut out);

        // Validate member accesses before emitting (warnings only).
        for &fid in &group.methods {
            validate_member_accesses(&module.functions[fid], Some(&qualified), &class_meta, &registry, &short_to_qualified, type_defs);
        }

        emit_class(group, module, &class_names, &class_meta, &mutable_global_names, &late_bound, &short_to_qualified, &known_classes, lowering_config, engine, &mut out)?;

        let path = file_dir.join(format!("{short_name}.ts"));
        fs::write(&path, &out).map_err(CoreError::Io)?;

        // Barrel export path: relative from module_dir.
        let export_path = segments.join("/");
        barrel_exports.push(export_path);
    }

    // Free functions → _init.ts (at module root, depth 0).
    if !free_funcs.is_empty() {
        let mut out = String::new();
        let free_fn_iter = || free_funcs.iter().map(|&fid| &module.functions[fid]);
        let systems = collect_system_names_from_funcs(free_fn_iter());
        emit_runtime_imports_for(systems, &mut out, 0, runtime_config);
        let calls = collect_call_names_from_funcs(free_fn_iter(), engine);
        emit_function_imports_with_prefix(&calls, &mut out, "..", runtime_config);
        if let Some(preamble) = runtime_config.and_then(|c| c.class_preamble.as_ref()) {
            let prefix = "../";
            let prefix = prefix.trim_end_matches('/');
            let _ = writeln!(
                out,
                "import {{ {} }} from \"{prefix}/runtime/{}\";",
                preamble.names.join(", "),
                preamble.path,
            );
            out.push('\n');
        }

        // Scan free functions for external class references.
        let mut refs = RefSets::default();
        for &fid in &free_funcs {
            let func = &module.functions[fid];
            collect_type_refs_from_function(
                func, "", &registry, &module.external_imports,
                &HashMap::new(), &HashMap::new(), &global_names, engine, &mut refs,
            );
        }
        emit_external_imports(&refs.ext_value_refs, &refs.ext_type_refs, &module.external_imports, module_exports, "..", &mut out);

        // Globals imports for free functions.
        if !refs.globals_used.is_empty() {
            let mut import_names: Vec<String> = Vec::new();
            for name in &refs.globals_used {
                import_names.push(sanitize_ident(name));
                if mutable_global_names.contains(name.as_str()) {
                    import_names.push(format!("$set_{}", sanitize_ident(name)));
                }
            }
            let _ = writeln!(out, "import {{ {} }} from \"./_globals\";", import_names.join(", "));
        }

        emit_imports(module, &mut out);
        for &fid in &free_funcs {
            emit_function(&mut module.functions[fid], &class_names, &known_classes, &mutable_global_names, lowering_config, engine, &mut out)?;
        }
        let path = module_dir.join("_init.ts");
        fs::write(&path, &out).map_err(CoreError::Io)?;
        barrel_exports.push("_init".to_string());
    }

    // Barrel file: index.ts
    let mut barrel = String::new();
    for export_path in &barrel_exports {
        let _ = writeln!(barrel, "export * from \"./{export_path}\";");
    }
    fs::write(module_dir.join("index.ts"), &barrel).map_err(CoreError::Io)?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Identifier sanitization
// ---------------------------------------------------------------------------

/// JS/TS reserved words that cannot be used as identifiers.
///
/// Includes ECMAScript reserved words, strict-mode reserved words, and
/// TypeScript contextual keywords that cause parse errors as identifiers.
const JS_RESERVED: &[&str] = &[
    "arguments",
    "async",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "eval",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "interface",
    "let",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "undefined",
    "var",
    "void",
    "while",
    "with",
    "yield",
];

/// Sanitize a name into a valid JavaScript identifier.
///
/// Replaces non-alphanumeric characters with `_` and prefixes with `_` if the
/// name starts with a digit or is a reserved word.
pub(crate) fn sanitize_ident(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' || ch == '$' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        return "_".to_string();
    }
    if out.starts_with(|c: char| c.is_ascii_digit()) {
        out.insert(0, '_');
    }
    if JS_RESERVED.contains(&out.as_str()) {
        out.insert(0, '_');
    }
    out
}
// ---------------------------------------------------------------------------
// Runtime imports (auto-detected from SystemCall ops)
// ---------------------------------------------------------------------------

fn collect_system_names_from_funcs<'a>(
    funcs: impl Iterator<Item = &'a Function>,
) -> BTreeSet<String> {
    let mut used = BTreeSet::new();
    for func in funcs {
        for (_inst_id, inst) in func.insts.iter() {
            if let Op::SystemCall { system, .. } = &inst.op {
                used.insert(system.clone());
            }
        }
    }
    used
}

/// Emit runtime imports for flat modules (files directly in `output_dir`).
fn emit_runtime_imports(module: &Module, out: &mut String, runtime_config: Option<&RuntimeConfig>, engine: EngineKind) {
    let all_funcs = || module.functions.iter().map(|(_id, f)| f);
    let systems = collect_system_names_from_funcs(all_funcs());
    emit_runtime_imports_with_prefix(systems, out, ".", runtime_config);
    let calls = collect_call_names_from_funcs(all_funcs(), engine);
    emit_function_imports_with_prefix(&calls, out, ".", runtime_config);
}

/// Emit runtime imports for files inside a module directory.
///
/// `depth` is the number of namespace directories below the module dir. The
/// module dir itself is one level inside `output_dir`, so the prefix traverses
/// `depth + 1` parent directories.
fn emit_runtime_imports_for(systems: BTreeSet<String>, out: &mut String, depth: usize, runtime_config: Option<&RuntimeConfig>) {
    let prefix = "../".repeat(depth + 1);
    let prefix = prefix.trim_end_matches('/');
    emit_runtime_imports_with_prefix(systems, out, prefix, runtime_config);
}

fn emit_runtime_imports_with_prefix(
    systems: BTreeSet<String>,
    out: &mut String,
    prefix: &str,
    runtime_config: Option<&RuntimeConfig>,
) {
    if systems.is_empty() {
        return;
    }
    let known: BTreeSet<&str> = SYSTEM_NAMES.iter().copied().collect();
    let mut generic: Vec<&str> = Vec::new();
    // Group engine-specific systems by their runtime sub-module path.
    let mut by_mod: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for sys in &systems {
        if known.contains(sys.as_str()) {
            generic.push(sys.as_str());
        } else if let Some(sm) = runtime_config.and_then(|c| c.system_modules.get(sys.as_str())) {
            by_mod
                .entry(sm.path.clone())
                .or_default()
                .push(sanitize_ident(sys));
        } else {
            // Fallback: derive module path from system name.
            let module = sys
                .split('.')
                .next_back()
                .unwrap_or(sys)
                .to_ascii_lowercase();
            by_mod
                .entry(module)
                .or_default()
                .push(sanitize_ident(sys));
        }
    }
    if !generic.is_empty() {
        let _ = writeln!(
            out,
            "import {{ {} }} from \"{prefix}/runtime\";",
            generic.join(", ")
        );
    }
    for (module, names) in &by_mod {
        // Namespace imports enable tree-shaking of individual methods.
        for name in names {
            let _ = writeln!(
                out,
                "import * as {name} from \"{prefix}/runtime/{module}\";",
            );
        }
    }
    if !generic.is_empty() || !by_mod.is_empty() {
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Function-level imports (runtime stdlib free functions)
// ---------------------------------------------------------------------------

/// Collect all direct `Call` function names from a set of IR functions,
/// plus any bare function names introduced by engine-specific rewrites
/// or backend cast printing (e.g. `int(x)` from `Coerce + Int(32)`).
fn collect_call_names_from_funcs<'a>(
    funcs: impl Iterator<Item = &'a Function>,
    engine: EngineKind,
) -> BTreeSet<String> {
    let mut used = BTreeSet::new();
    for func in funcs {
        for (_inst_id, inst) in func.insts.iter() {
            match &inst.op {
                Op::Call { func: name, .. } => {
                    used.insert(name.clone());
                }
                Op::SystemCall { system, method, .. } if engine == EngineKind::GameMaker => {
                    if let Some(name) = crate::rewrites::gamemaker::rewrite_introduced_call(system, method) {
                        used.insert(name.to_string());
                    }
                }
                // Coerce casts emit bare function calls: int(x), uint(x).
                Op::Cast(_, Type::Int(32), CastKind::Coerce) => {
                    used.insert("int".to_string());
                }
                Op::Cast(_, Type::UInt(32), CastKind::Coerce) => {
                    used.insert("uint".to_string());
                }
                _ => {}
            }
        }
    }
    used
}

/// Emit import statements for runtime-provided free functions.
///
/// Scans `call_names` against `function_modules` in the runtime config,
/// groups matches by module path, and emits one import per module.
fn emit_function_imports_with_prefix(
    call_names: &BTreeSet<String>,
    out: &mut String,
    prefix: &str,
    runtime_config: Option<&RuntimeConfig>,
) {
    let Some(cfg) = runtime_config else { return };
    if cfg.function_modules.is_empty() {
        return;
    }
    // Build reverse map: function_name → module_path.
    let mut func_to_module: HashMap<&str, &str> = HashMap::new();
    for group in &cfg.function_modules {
        for name in &group.names {
            func_to_module.insert(name.as_str(), group.path.as_str());
        }
    }
    // Group needed imports by module path.
    let mut by_mod: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
    for name in call_names {
        if let Some(&path) = func_to_module.get(name.as_str()) {
            by_mod.entry(path).or_default().insert(name.as_str());
        }
    }
    for (module, names) in &by_mod {
        let names: Vec<&str> = names.iter().copied().collect();
        let _ = writeln!(
            out,
            "import {{ {} }} from \"{prefix}/runtime/{module}\";",
            names.join(", "),
        );
    }
    if !by_mod.is_empty() {
        out.push('\n');
    }
}

/// Emit import statements for user-defined free functions that live in `_init.ts`.
///
/// Any `Op::Call` name that matches a free function in the same module gets an
/// `import { name } from "<prefix>/_init"` line.
fn emit_free_function_imports(
    call_names: &BTreeSet<String>,
    free_func_names: &HashSet<String>,
    depth: usize,
    out: &mut String,
) {
    let needed: BTreeSet<&str> = call_names
        .iter()
        .filter(|n| free_func_names.contains(n.as_str()))
        .map(|n| n.as_str())
        .collect();
    if needed.is_empty() {
        return;
    }
    let names: Vec<&str> = needed.into_iter().collect();
    // _init.ts lives in module_dir, so we go up `depth` levels (one per
    // namespace segment).  depth=0 → "./_init", depth=1 → "../_init", etc.
    let prefix = if depth == 0 {
        ".".to_string()
    } else {
        "../".repeat(depth).trim_end_matches('/').to_string()
    };
    let _ = writeln!(out, "import {{ {} }} from \"{prefix}/_init\";", names.join(", "));
    out.push('\n');
}

// ---------------------------------------------------------------------------
// Intra-module imports (class-to-class references)
// ---------------------------------------------------------------------------

/// Emit a categorized warning for an unmapped external reference, filtering
/// out known false positives (private/protected namespace accesses, `fl.*`
/// authoring library types).
fn warn_unmapped_reference(name: &str) {
    let (ns, _short) = name.rsplit_once("::").unwrap_or(("", name));
    // No namespace at all — bare name, not a qualified reference.
    if ns.is_empty() {
        return;
    }
    // Private/protected namespace member access (e.g. "classes:Monster::consumables").
    // These contain a colon within the namespace portion.
    if ns.contains(':') {
        return;
    }
    // Flash authoring library — not part of the runtime.
    if ns.starts_with("fl.") {
        return;
    }
    // Flash runtime stdlib — actionable: add to runtime.
    if let Some(pkg) = ns.strip_prefix("flash.") {
        eprintln!("warning: flash package '{pkg}' not in runtime stdlib (referenced: {name})");
        return;
    }
    eprintln!("warning: unmapped external reference: {name}");
}

/// Bundled output sets for collecting class/type references.
#[derive(Default)]
pub(crate) struct RefSets {
    /// Intra-module value refs (class constructor needed at runtime).
    pub(crate) value_refs: BTreeSet<String>,
    /// Intra-module type-only refs (erased at runtime).
    pub(crate) type_refs: BTreeSet<String>,
    /// Intra-module value refs from TypeCheck/AsType only (may be late-bound).
    pub(crate) typecheck_value_refs: BTreeSet<String>,
    /// External value refs (e.g. Flash stdlib runtime classes).
    pub(crate) ext_value_refs: BTreeSet<String>,
    /// External type-only refs.
    pub(crate) ext_type_refs: BTreeSet<String>,
    /// Module-level globals referenced via scope lookups.
    pub(crate) globals_used: BTreeSet<String>,
}

/// Collect type names referenced by a class group, split into value and type-only refs.
///
/// **Value refs** (class constructor needed at runtime):
/// - `super_class` — `extends X` is a runtime expression
/// - `Op::TypeCheck` — emits `instanceof X`
///
/// **Type refs** (erased at runtime):
/// - Struct field types, function signatures, `Op::Alloc`, `Op::Cast`, `value_types`
#[allow(clippy::too_many_arguments)]
fn collect_class_references(
    group: &ClassGroup,
    module: &Module,
    registry: &ClassRegistry,
    external_imports: &BTreeMap<String, ExternalImport>,
    static_method_owners: &HashMap<String, String>,
    static_field_owners: &HashMap<String, String>,
    global_names: &HashSet<String>,
    engine: EngineKind,
) -> RefSets {
    let self_name = &group.class_def.name;
    let mut refs = RefSets::default();

    // Super class reference — runtime value (extends).
    if let Some(sc) = &group.class_def.super_class {
        let short = sc.rsplit("::").next().unwrap_or(sc);
        if short != self_name {
            if let Some(entry) = registry.lookup(sc) {
                refs.value_refs.insert(entry.short_name.clone());
            } else if external_imports.contains_key(sc) {
                refs.ext_value_refs.insert(sc.to_string());
            }
        }
    }

    // Interface references — runtime value (registerInterface needs constructors).
    for iface in &group.class_def.interfaces {
        let short = iface.rsplit("::").next().unwrap_or(iface);
        if short != self_name {
            if let Some(entry) = registry.lookup(iface) {
                refs.value_refs.insert(entry.short_name.clone());
            } else if external_imports.contains_key(iface.as_str()) {
                refs.ext_value_refs.insert(iface.to_string());
            }
        }
    }

    // Struct fields (class instance fields) — type-only.
    for (_name, ty, _) in &group.struct_def.fields {
        collect_type_ref(ty, self_name, registry, external_imports, &mut refs.type_refs, &mut refs.ext_type_refs);
    }

    // Scan all method bodies for type references.
    for &fid in &group.methods {
        let func = &module.functions[fid];
        collect_type_refs_from_function(func, self_name, registry, external_imports, static_method_owners, static_field_owners, global_names, engine, &mut refs);
    }

    refs
}

/// Scan a function's instructions and signature for type references.
#[allow(clippy::too_many_arguments)]
fn collect_type_refs_from_function(
    func: &Function,
    self_name: &str,
    registry: &ClassRegistry,
    external_imports: &BTreeMap<String, ExternalImport>,
    static_method_owners: &HashMap<String, String>,
    static_field_owners: &HashMap<String, String>,
    global_names: &HashSet<String>,
    engine: EngineKind,
    refs: &mut RefSets,
) {
    use reincarnate_core::ir::Constant;

    // Return type and param types — type-only.
    collect_type_ref(&func.sig.return_ty, self_name, registry, external_imports, &mut refs.type_refs, &mut refs.ext_type_refs);
    for ty in &func.sig.params {
        collect_type_ref(ty, self_name, registry, external_imports, &mut refs.type_refs, &mut refs.ext_type_refs);
    }

    // Build ValueId → &str map for const strings (to resolve SystemCall args).
    let const_strings: HashMap<_, _> = func
        .insts
        .iter()
        .filter_map(|(_id, inst)| {
            if let Op::Const(Constant::String(s)) = &inst.op {
                inst.result.map(|v| (v, s.as_str()))
            } else {
                None
            }
        })
        .collect();

    // Instructions.
    for (_inst_id, inst) in func.insts.iter() {
        match &inst.op {
            // TypeCheck emits `isType()` — runtime value reference, collected
            // separately so circular imports can be detected and late-bound.
            Op::TypeCheck(_, ty) => {
                let is_struct_or_enum = matches!(ty, Type::Struct(_) | Type::Enum(_));
                if is_struct_or_enum {
                    collect_type_ref(ty, self_name, registry, external_imports, &mut refs.typecheck_value_refs, &mut refs.ext_value_refs);
                } else {
                    collect_type_ref(ty, self_name, registry, external_imports, &mut refs.value_refs, &mut refs.ext_value_refs);
                }
            }
            // Alloc is type-only. Cast with Struct/Enum: AsType needs runtime value, Coerce is type-only.
            Op::Alloc(ty) => {
                collect_type_ref(ty, self_name, registry, external_imports, &mut refs.type_refs, &mut refs.ext_type_refs);
            }
            Op::Cast(_, ty, kind) => {
                let is_struct_or_enum = matches!(ty, Type::Struct(_) | Type::Enum(_));
                if is_struct_or_enum && *kind == CastKind::AsType {
                    // AsType needs runtime constructor — collected separately
                    // so circular imports can be detected and late-bound.
                    collect_type_ref(ty, self_name, registry, external_imports, &mut refs.typecheck_value_refs, &mut refs.ext_value_refs);
                } else {
                    collect_type_ref(ty, self_name, registry, external_imports, &mut refs.type_refs, &mut refs.ext_type_refs);
                }
            }
            // GetField with a class name → runtime value reference (used with `new`).
            Op::GetField { field, .. } => {
                if registry.lookup(field).is_some() {
                    collect_type_ref(
                        &Type::Struct(field.clone()),
                        self_name,
                        registry,
                        external_imports,
                        &mut refs.value_refs,
                        &mut refs.ext_value_refs,
                    );
                } else {
                    let short = field.rsplit("::").next().unwrap_or(field);
                    if short != self_name {
                        if external_imports.contains_key(field.as_str()) {
                            refs.ext_value_refs.insert(field.to_string());
                        } else {
                            warn_unmapped_reference(field);
                        }
                    }
                }
            }
            // Engine-specific SystemCall import extraction — delegated to rewrite modules.
            Op::SystemCall { system, method, args } => {
                match engine {
                    EngineKind::Flash if system == "Flash.Scope"
                        && (method == "findPropStrict" || method == "findProperty") =>
                    {
                        crate::rewrites::flash::collect_flash_scope_refs(
                            args, &const_strings, self_name, registry,
                            static_method_owners, static_field_owners, global_names, refs,
                        );
                    }
                    EngineKind::GameMaker if system == "GameMaker.Instance"
                        && (method == "getOn" || method == "setOn")
                        && !args.is_empty() =>
                    {
                        crate::rewrites::gamemaker::collect_gamemaker_instance_refs(
                            args, &const_strings, self_name, registry, external_imports, refs,
                        );
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    // value_types — type-only.
    for (_vid, ty) in func.value_types.iter() {
        collect_type_ref(ty, self_name, registry, external_imports, &mut refs.type_refs, &mut refs.ext_type_refs);
    }
}

/// If a type references a class in the registry, add its short name.
/// If not in the registry but in `external_imports`, add to `ext_refs`.
fn collect_type_ref(
    ty: &Type,
    self_name: &str,
    registry: &ClassRegistry,
    external_imports: &BTreeMap<String, ExternalImport>,
    refs: &mut BTreeSet<String>,
    ext_refs: &mut BTreeSet<String>,
) {
    match ty {
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            if short != self_name {
                if let Some(entry) = registry.lookup(name) {
                    refs.insert(entry.short_name.clone());
                } else if external_imports.contains_key(name.as_str()) {
                    ext_refs.insert(name.to_string());
                } else {
                    warn_unmapped_reference(name);
                }
            }
        }
        Type::Array(inner) | Type::Option(inner) => {
            collect_type_ref(inner, self_name, registry, external_imports, refs, ext_refs);
        }
        Type::Map(k, v) => {
            collect_type_ref(k, self_name, registry, external_imports, refs, ext_refs);
            collect_type_ref(v, self_name, registry, external_imports, refs, ext_refs);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                collect_type_ref(elem, self_name, registry, external_imports, refs, ext_refs);
            }
        }
        Type::Function(sig) => {
            collect_type_ref(&sig.return_ty, self_name, registry, external_imports, refs, ext_refs);
            for p in &sig.params {
                collect_type_ref(p, self_name, registry, external_imports, refs, ext_refs);
            }
        }
        Type::Coroutine {
            yield_ty,
            return_ty,
        } => {
            collect_type_ref(yield_ty, self_name, registry, external_imports, refs, ext_refs);
            collect_type_ref(return_ty, self_name, registry, external_imports, refs, ext_refs);
        }
        Type::Union(types) => {
            for t in types {
                collect_type_ref(t, self_name, registry, external_imports, refs, ext_refs);
            }
        }
        _ => {}
    }
}

/// Collect short names of intra-module classes referenced by a type (for globals).
fn collect_global_type_imports(ty: &Type, registry: &ClassRegistry, refs: &mut BTreeSet<String>) {
    match ty {
        Type::Struct(name) | Type::Enum(name) => {
            if let Some(entry) = registry.lookup(name) {
                refs.insert(entry.short_name.clone());
            }
        }
        Type::Array(inner) | Type::Option(inner) => {
            collect_global_type_imports(inner, registry, refs);
        }
        Type::Map(k, v) => {
            collect_global_type_imports(k, registry, refs);
            collect_global_type_imports(v, registry, refs);
        }
        Type::Tuple(elems) | Type::Union(elems) => {
            for elem in elems {
                collect_global_type_imports(elem, registry, refs);
            }
        }
        Type::Function(sig) => {
            collect_global_type_imports(&sig.return_ty, registry, refs);
            for p in &sig.params {
                collect_global_type_imports(p, registry, refs);
            }
        }
        _ => {}
    }
}

/// Emit grouped `import` / `import type` statements for external runtime references.
///
/// Names in `ext_value_refs` / `ext_type_refs` are qualified keys into
/// `external_imports` (e.g. `"flash.text::TextFormat"` or `"stage"`).
fn emit_external_imports(
    ext_value_refs: &BTreeSet<String>,
    ext_type_refs: &BTreeSet<String>,
    external_imports: &BTreeMap<String, ExternalImport>,
    module_exports: &BTreeMap<String, Vec<String>>,
    prefix: &str,
    out: &mut String,
) {
    if ext_value_refs.is_empty() && ext_type_refs.is_empty() {
        return;
    }
    // Group value refs by module_path, resolving qualified → short names.
    // Validate each name against module_exports.
    let mut val_by_mod: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for name in ext_value_refs {
        if let Some(imp) = external_imports.get(name.as_str()) {
            validate_module_export(&imp.short_name, &imp.module_path, module_exports);
            val_by_mod
                .entry(&imp.module_path)
                .or_default()
                .push(&imp.short_name);
        }
    }
    for (module_path, names) in &val_by_mod {
        let _ = writeln!(
            out,
            "import {{ {} }} from \"{prefix}/runtime/{module_path}\";",
            names.join(", ")
        );
    }
    // Collect resolved short names from value refs for dedup.
    let val_short_names: HashSet<&str> = ext_value_refs
        .iter()
        .filter_map(|n| external_imports.get(n.as_str()).map(|i| i.short_name.as_str()))
        .collect();
    // Type-only imports (not already covered by value imports).
    let mut type_by_mod: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for name in ext_type_refs {
        if let Some(imp) = external_imports.get(name.as_str()) {
            if !val_short_names.contains(imp.short_name.as_str()) {
                validate_module_export(&imp.short_name, &imp.module_path, module_exports);
                type_by_mod
                    .entry(&imp.module_path)
                    .or_default()
                    .push(&imp.short_name);
            }
        }
    }
    for (module_path, names) in &type_by_mod {
        let _ = writeln!(
            out,
            "import type {{ {} }} from \"{prefix}/runtime/{module_path}\";",
            names.join(", ")
        );
    }
}

/// Warn if a short name is not listed in `module_exports` for the given module path.
fn validate_module_export(
    short_name: &str,
    module_path: &str,
    module_exports: &BTreeMap<String, Vec<String>>,
) {
    if module_exports.is_empty() {
        return;
    }
    match module_exports.get(module_path) {
        Some(exports) => {
            if !exports.iter().any(|e| e == short_name) {
                eprintln!(
                    "warning: '{short_name}' is not exported from runtime module '{module_path}'"
                );
            }
        }
        None => {
            eprintln!(
                "warning: runtime module '{module_path}' has no declared exports \
                 (referenced: {short_name})"
            );
        }
    }
}

/// Compute the transitive closure of the value-import graph.
///
/// For each class, find all classes reachable through value imports (extends,
/// interfaces, static method owners, etc.). Used to detect cycles: if target T
/// transitively imports source S, then adding S→T would create a circular
/// dependency.
fn compute_transitive_value_imports(
    direct_imports: &HashMap<String, BTreeSet<String>>,
) -> HashMap<String, HashSet<String>> {
    let mut result: HashMap<String, HashSet<String>> = HashMap::new();
    for start in direct_imports.keys() {
        let mut visited = HashSet::new();
        let mut stack = Vec::new();
        // Seed with direct imports of the start node.
        if let Some(direct) = direct_imports.get(start) {
            for dep in direct {
                if dep != start && visited.insert(dep.clone()) {
                    stack.push(dep.clone());
                }
            }
        }
        while let Some(current) = stack.pop() {
            if let Some(deps) = direct_imports.get(&current) {
                for dep in deps {
                    if dep != start && visited.insert(dep.clone()) {
                        stack.push(dep.clone());
                    }
                }
            }
        }
        result.insert(start.clone(), visited);
    }
    result
}

/// Emit `import` / `import type` statements for intra-module class references.
///
/// Returns the set of short names that are late-bound (circular TypeCheck/AsType
/// refs that should use `getDefinitionByName()` instead of a static import).
#[allow(clippy::too_many_arguments)]
fn emit_intra_imports(
    group: &ClassGroup,
    module: &Module,
    source_segments: &[String],
    registry: &ClassRegistry,
    static_method_owners: &HashMap<String, String>,
    static_field_owners: &HashMap<String, String>,
    global_names: &HashSet<String>,
    mutable_global_names: &HashSet<String>,
    module_exports: &BTreeMap<String, Vec<String>>,
    transitive_value_imports: &HashMap<String, HashSet<String>>,
    short_to_qualified: &HashMap<String, String>,
    depth: usize,
    engine: EngineKind,
    out: &mut String,
) -> HashSet<String> {
    let refs = collect_class_references(group, module, registry, &module.external_imports, static_method_owners, static_field_owners, global_names, engine);

    // Compute late-bound set: typecheck refs whose targets transitively import
    // this class (i.e. adding a static import would create a cycle), and that
    // are NOT also referenced as regular value refs (e.g. `new X()` or `extends X`).
    let self_short = sanitize_ident(&group.class_def.name);
    let mut late_bound: HashSet<String> = HashSet::new();
    for name in &refs.typecheck_value_refs {
        if !refs.value_refs.contains(name) {
            // Target T transitively imports self → adding self→T would be circular.
            let target_imports_self = transitive_value_imports
                .get(name.as_str())
                .is_some_and(|reachable| reachable.contains(&self_short));
            if target_imports_self {
                // Only late-bind if we have a qualified name to resolve at runtime.
                if short_to_qualified.contains_key(name.as_str()) {
                    late_bound.insert(name.clone());
                }
            }
        }
    }

    // Merge non-late-bound typecheck refs back into value_refs for import generation.
    let mut effective_value_refs = refs.value_refs.clone();
    for name in &refs.typecheck_value_refs {
        if !late_bound.contains(name) {
            effective_value_refs.insert(name.clone());
        }
    }

    let has_intra = !effective_value_refs.is_empty() || !refs.type_refs.is_empty();
    let has_ext = !refs.ext_value_refs.is_empty() || !refs.ext_type_refs.is_empty();
    let has_globals = !refs.globals_used.is_empty();
    if !has_intra && !has_ext && !has_globals {
        return late_bound;
    }

    // External runtime imports — grouped by sub-module.
    if has_ext {
        let prefix = "../".repeat(depth + 1);
        let prefix = prefix.trim_end_matches('/');
        emit_external_imports(&refs.ext_value_refs, &refs.ext_type_refs, &module.external_imports, module_exports, prefix, out);
    }

    // Intra-module value imports.
    for short_name in &effective_value_refs {
        if let Some(entry) = registry.classes.get(short_name) {
            let rel = relative_import_path(source_segments, &entry.path_segments);
            let _ = writeln!(out, "import {{ {short_name} }} from \"{rel}\";");
        }
    }
    // Intra-module type-only imports (names not already in value_refs).
    for short_name in &refs.type_refs {
        if effective_value_refs.contains(short_name) {
            continue;
        }
        if let Some(entry) = registry.classes.get(short_name) {
            let rel = relative_import_path(source_segments, &entry.path_segments);
            let _ = writeln!(out, "import type {{ {short_name} }} from \"{rel}\";");
        }
    }
    // Module-level globals.
    if has_globals {
        let globals_path = if depth == 0 {
            "./_globals".to_string()
        } else {
            let prefix = "../".repeat(depth);
            format!("{}_globals", prefix)
        };
        let mut import_names: Vec<String> = Vec::new();
        for name in &refs.globals_used {
            import_names.push(sanitize_ident(name));
            // Also import the setter for mutable globals.
            if mutable_global_names.contains(name.as_str()) {
                import_names.push(format!("$set_{}", sanitize_ident(name)));
            }
        }
        let _ = writeln!(out, "import {{ {} }} from \"{globals_path}\";", import_names.join(", "));
    }
    out.push('\n');
    late_bound
}

// ---------------------------------------------------------------------------
// Late-bound type check rewriting
// ---------------------------------------------------------------------------

/// Replace `isType(x, Foo)` / `asType(x, Foo)` with late-bound variants using
/// `getDefinitionByName("qualified::Name")` for types whose static import would
/// create a circular dependency.
fn rewrite_late_bound_types(
    body: &mut [JsStmt],
    late_bound: &HashSet<String>,
    short_to_qualified: &HashMap<String, String>,
) {
    if late_bound.is_empty() {
        return;
    }
    for stmt in body.iter_mut() {
        rewrite_late_bound_stmt(stmt, late_bound, short_to_qualified);
    }
}

fn rewrite_late_bound_stmt(
    stmt: &mut JsStmt,
    late_bound: &HashSet<String>,
    short_to_qualified: &HashMap<String, String>,
) {
    match stmt {
        JsStmt::VarDecl { init: Some(expr), .. } => {
            rewrite_late_bound_expr(expr, late_bound, short_to_qualified);
        }
        JsStmt::Assign { target, value } => {
            rewrite_late_bound_expr(target, late_bound, short_to_qualified);
            rewrite_late_bound_expr(value, late_bound, short_to_qualified);
        }
        JsStmt::CompoundAssign { target, value, .. } => {
            rewrite_late_bound_expr(target, late_bound, short_to_qualified);
            rewrite_late_bound_expr(value, late_bound, short_to_qualified);
        }
        JsStmt::Expr(expr) | JsStmt::Throw(expr) => {
            rewrite_late_bound_expr(expr, late_bound, short_to_qualified);
        }
        JsStmt::Return(Some(expr)) => {
            rewrite_late_bound_expr(expr, late_bound, short_to_qualified);
        }
        JsStmt::If { cond, then_body, else_body } => {
            rewrite_late_bound_expr(cond, late_bound, short_to_qualified);
            rewrite_late_bound_types(then_body, late_bound, short_to_qualified);
            rewrite_late_bound_types(else_body, late_bound, short_to_qualified);
        }
        JsStmt::While { cond, body } => {
            rewrite_late_bound_expr(cond, late_bound, short_to_qualified);
            rewrite_late_bound_types(body, late_bound, short_to_qualified);
        }
        JsStmt::For { init, cond, update, body } => {
            rewrite_late_bound_types(init, late_bound, short_to_qualified);
            rewrite_late_bound_expr(cond, late_bound, short_to_qualified);
            rewrite_late_bound_types(update, late_bound, short_to_qualified);
            rewrite_late_bound_types(body, late_bound, short_to_qualified);
        }
        JsStmt::Loop { body } => {
            rewrite_late_bound_types(body, late_bound, short_to_qualified);
        }
        JsStmt::ForOf { iterable, body, .. } => {
            rewrite_late_bound_expr(iterable, late_bound, short_to_qualified);
            rewrite_late_bound_types(body, late_bound, short_to_qualified);
        }
        JsStmt::Dispatch { blocks, .. } => {
            for (_, stmts) in blocks {
                rewrite_late_bound_types(stmts, late_bound, short_to_qualified);
            }
        }
        _ => {}
    }
}

fn rewrite_late_bound_expr(
    expr: &mut JsExpr,
    late_bound: &HashSet<String>,
    short_to_qualified: &HashMap<String, String>,
) {
    // First, check if this expression itself needs rewriting.
    let needs_rewrite = match expr {
        JsExpr::TypeCheck { ty: Type::Struct(name) | Type::Enum(name), .. } => {
            let short = name.rsplit("::").next().unwrap_or(name);
            late_bound.contains(short)
        }
        JsExpr::Cast { ty: Type::Struct(name) | Type::Enum(name), kind: CastKind::AsType, .. } => {
            let short = name.rsplit("::").next().unwrap_or(name);
            late_bound.contains(short)
        }
        _ => false,
    };

    if needs_rewrite {
        let dummy = JsExpr::Literal(Constant::Null);
        let old = std::mem::replace(expr, dummy);
        match old {
            JsExpr::TypeCheck { expr: inner, ty } => {
                let name = match &ty {
                    Type::Struct(n) | Type::Enum(n) => n,
                    _ => unreachable!(),
                };
                let short = name.rsplit("::").next().unwrap_or(name);
                let qualified = short_to_qualified.get(short).map_or_else(
                    || name.clone(),
                    |q| q.clone(),
                );
                *expr = JsExpr::Call {
                    callee: Box::new(JsExpr::Var("isType".into())),
                    args: vec![
                        *inner,
                        JsExpr::Call {
                            callee: Box::new(JsExpr::Var("getDefinitionByName".into())),
                            args: vec![JsExpr::Literal(Constant::String(qualified))],
                        },
                    ],
                };
            }
            JsExpr::Cast { expr: inner, ty, .. } => {
                let name = match &ty {
                    Type::Struct(n) | Type::Enum(n) => n,
                    _ => unreachable!(),
                };
                let short = name.rsplit("::").next().unwrap_or(name);
                let qualified = short_to_qualified.get(short).map_or_else(
                    || name.clone(),
                    |q| q.clone(),
                );
                *expr = JsExpr::Call {
                    callee: Box::new(JsExpr::Var("asType".into())),
                    args: vec![
                        *inner,
                        JsExpr::Call {
                            callee: Box::new(JsExpr::Var("getDefinitionByName".into())),
                            args: vec![JsExpr::Literal(Constant::String(qualified))],
                        },
                    ],
                };
            }
            _ => unreachable!(),
        }
        // Recurse into the newly created expression's children.
        rewrite_late_bound_expr(expr, late_bound, short_to_qualified);
        return;
    }

    // Recurse into child expressions.
    match expr {
        JsExpr::Binary { lhs, rhs, .. } | JsExpr::Cmp { lhs, rhs, .. }
        | JsExpr::LogicalOr { lhs, rhs } | JsExpr::LogicalAnd { lhs, rhs } => {
            rewrite_late_bound_expr(lhs, late_bound, short_to_qualified);
            rewrite_late_bound_expr(rhs, late_bound, short_to_qualified);
        }
        JsExpr::Unary { expr: inner, .. } | JsExpr::Not(inner)
        | JsExpr::PostIncrement(inner) | JsExpr::TypeOf(inner)
        | JsExpr::GeneratorResume(inner) => {
            rewrite_late_bound_expr(inner, late_bound, short_to_qualified);
        }
        JsExpr::Cast { expr: inner, .. } | JsExpr::TypeCheck { expr: inner, .. } => {
            rewrite_late_bound_expr(inner, late_bound, short_to_qualified);
        }
        JsExpr::Field { object, .. } => {
            rewrite_late_bound_expr(object, late_bound, short_to_qualified);
        }
        JsExpr::Index { collection, index } => {
            rewrite_late_bound_expr(collection, late_bound, short_to_qualified);
            rewrite_late_bound_expr(index, late_bound, short_to_qualified);
        }
        JsExpr::Call { callee, args } | JsExpr::New { callee, args } => {
            rewrite_late_bound_expr(callee, late_bound, short_to_qualified);
            for arg in args {
                rewrite_late_bound_expr(arg, late_bound, short_to_qualified);
            }
        }
        JsExpr::Ternary { cond, then_val, else_val } => {
            rewrite_late_bound_expr(cond, late_bound, short_to_qualified);
            rewrite_late_bound_expr(then_val, late_bound, short_to_qualified);
            rewrite_late_bound_expr(else_val, late_bound, short_to_qualified);
        }
        JsExpr::ArrayInit(elems) | JsExpr::TupleInit(elems) => {
            for elem in elems {
                rewrite_late_bound_expr(elem, late_bound, short_to_qualified);
            }
        }
        JsExpr::ObjectInit(pairs) => {
            for (_, val) in pairs {
                rewrite_late_bound_expr(val, late_bound, short_to_qualified);
            }
        }
        JsExpr::In { key, object } | JsExpr::Delete { object, key } => {
            rewrite_late_bound_expr(key, late_bound, short_to_qualified);
            rewrite_late_bound_expr(object, late_bound, short_to_qualified);
        }
        JsExpr::SuperCall(args) | JsExpr::GeneratorCreate { args, .. } => {
            for arg in args {
                rewrite_late_bound_expr(arg, late_bound, short_to_qualified);
            }
        }
        JsExpr::SuperMethodCall { args, .. } => {
            for arg in args {
                rewrite_late_bound_expr(arg, late_bound, short_to_qualified);
            }
        }
        JsExpr::SuperSet { value, .. } => {
            rewrite_late_bound_expr(value, late_bound, short_to_qualified);
        }
        JsExpr::Yield(Some(inner)) => {
            rewrite_late_bound_expr(inner, late_bound, short_to_qualified);
        }
        JsExpr::SystemCall { args, .. } => {
            for arg in args {
                rewrite_late_bound_expr(arg, late_bound, short_to_qualified);
            }
        }
        JsExpr::ArrowFunction { body, .. } => {
            rewrite_late_bound_types(body, late_bound, short_to_qualified);
        }
        // Leaf nodes — nothing to recurse into.
        JsExpr::Literal(_) | JsExpr::Var(_) | JsExpr::This
        | JsExpr::Activation | JsExpr::SuperGet(_) | JsExpr::Yield(None) => {}
    }
}

// ---------------------------------------------------------------------------
// Imports
// ---------------------------------------------------------------------------

fn emit_imports(module: &Module, out: &mut String) {
    for import in &module.imports {
        let name = match &import.alias {
            Some(alias) => {
                format!("{} as {}", sanitize_ident(&import.name), sanitize_ident(alias))
            }
            None => sanitize_ident(&import.name),
        };
        let _ = writeln!(out, "import {{ {name} }} from \"./{}\";", import.module);
    }
    if !module.imports.is_empty() {
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Structs → interfaces
// ---------------------------------------------------------------------------

fn emit_structs(module: &Module, out: &mut String) {
    for def in &module.structs {
        emit_struct(def, out);
    }
}

fn emit_struct(def: &StructDef, out: &mut String) {
    let vis = visibility_prefix(def.visibility);
    let _ = writeln!(out, "{vis}interface {} {{", sanitize_ident(&def.name));
    for (name, ty, _) in &def.fields {
        let _ = writeln!(out, "  {}: {};", sanitize_ident(name), ts_type(ty));
    }
    let _ = writeln!(out, "}}\n");
}

// ---------------------------------------------------------------------------
// Enums → discriminated union types
// ---------------------------------------------------------------------------

fn emit_enums(module: &Module, out: &mut String) {
    for def in &module.enums {
        let vis = visibility_prefix(def.visibility);
        let variants: Vec<String> = def
            .variants
            .iter()
            .map(|v| {
                if v.fields.is_empty() {
                    format!("{{ tag: \"{}\" }}", v.name)
                } else {
                    let fields: Vec<String> = v
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, t)| format!("field{i}: {}", ts_type(t)))
                        .collect();
                    format!("{{ tag: \"{}\", {} }}", v.name, fields.join(", "))
                }
            })
            .collect();
        let _ = writeln!(
            out,
            "{vis}type {} = {};",
            sanitize_ident(&def.name),
            variants.join(" | ")
        );
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Globals
// ---------------------------------------------------------------------------

fn emit_globals(module: &Module, out: &mut String) {
    for global in &module.globals {
        let vis = visibility_prefix(global.visibility);
        // const without initializer is invalid JS — demote to let.
        let kw = if global.mutable || global.init.is_none() { "let" } else { "const" };
        let ident = sanitize_ident(&global.name);
        let ts = ts_type(&global.ty);
        if let Some(val) = &global.init {
            let _ = writeln!(
                out,
                "{vis}{kw} {ident}: {ts} = {};",
                crate::ast_printer::emit_constant(val)
            );
        } else {
            let _ = writeln!(out, "{vis}{kw} {ident}: {ts};");
        }
    }
    if !module.globals.is_empty() {
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Global assignment rewriting (ESM compatibility)
// ---------------------------------------------------------------------------

/// Rewrite assignments to mutable globals into setter function calls.
///
/// ES modules make `import { x }` a read-only binding. To write to `x` from
/// another module, the exporting module provides `$set_x(v)`. This pass
/// rewrites `x = v` → `$set_x(v)` and `x op= v` → `$set_x(x op v)`.
fn rewrite_global_assignments(body: &mut [JsStmt], mutable_globals: &HashSet<String>) {
    if mutable_globals.is_empty() {
        return;
    }
    for stmt in body.iter_mut() {
        match stmt {
            JsStmt::Assign {
                target: JsExpr::Var(name),
                ..
            } if mutable_globals.contains(name.as_str()) => {
                // Replace: `name = value` → `$set_name(value)`
                let setter = format!("$set_{name}");
                let dummy = JsExpr::Literal(Constant::Null);
                if let JsStmt::Assign { value, .. } = std::mem::replace(stmt, JsStmt::Expr(dummy.clone())) {
                    *stmt = JsStmt::Expr(JsExpr::Call {
                        callee: Box::new(JsExpr::Var(setter)),
                        args: vec![value],
                    });
                }
            }
            JsStmt::CompoundAssign {
                target: JsExpr::Var(name),
                ..
            } if mutable_globals.contains(name.as_str()) => {
                // Replace: `name op= value` → `$set_name(name op value)`
                let var_name = name.clone();
                let setter = format!("$set_{var_name}");
                let dummy = JsExpr::Literal(Constant::Null);
                if let JsStmt::CompoundAssign { op, value, .. } = std::mem::replace(stmt, JsStmt::Expr(dummy.clone())) {
                    *stmt = JsStmt::Expr(JsExpr::Call {
                        callee: Box::new(JsExpr::Var(setter)),
                        args: vec![JsExpr::Binary {
                            op,
                            lhs: Box::new(JsExpr::Var(var_name)),
                            rhs: Box::new(value),
                        }],
                    });
                }
            }
            // Recurse into nested bodies.
            JsStmt::If { then_body, else_body, .. } => {
                rewrite_global_assignments(then_body, mutable_globals);
                rewrite_global_assignments(else_body, mutable_globals);
            }
            JsStmt::While { body, .. }
            | JsStmt::Loop { body }
            | JsStmt::ForOf { body, .. } => {
                rewrite_global_assignments(body, mutable_globals);
            }
            JsStmt::For { init, body, update, .. } => {
                rewrite_global_assignments(init, mutable_globals);
                rewrite_global_assignments(body, mutable_globals);
                rewrite_global_assignments(update, mutable_globals);
            }
            JsStmt::Dispatch { blocks, .. } => {
                for (_, stmts) in blocks {
                    rewrite_global_assignments(stmts, mutable_globals);
                }
            }
            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Functions
// ---------------------------------------------------------------------------

fn emit_functions(
    module: &mut Module,
    class_names: &HashMap<String, String>,
    known_classes: &HashSet<String>,
    mutable_global_names: &HashSet<String>,
    lowering_config: &LoweringConfig,
    engine: EngineKind,
    out: &mut String,
) -> Result<(), CoreError> {
    for id in module.functions.keys().collect::<Vec<_>>() {
        emit_function(&mut module.functions[id], class_names, known_classes, mutable_global_names, lowering_config, engine, out)?;
    }
    Ok(())
}

fn emit_function(
    func: &mut Function,
    class_names: &HashMap<String, String>,
    known_classes: &HashSet<String>,
    mutable_global_names: &HashSet<String>,
    lowering_config: &LoweringConfig,
    engine: EngineKind,
    out: &mut String,
) -> Result<(), CoreError> {
    use reincarnate_core::ir::linear;

    let shape = structurize::structurize(func);
    let ast = linear::lower_function_linear(func, &shape, lowering_config);
    let ctx = crate::lower::LowerCtx {
        self_param_name: None,
    };
    let js_func = crate::lower::lower_function(&ast, &ctx);
    let mut js_func = match engine {
        EngineKind::GameMaker => crate::rewrites::gamemaker::rewrite_gamemaker_function(js_func),
        EngineKind::Flash => {
            let rewrite_ctx = crate::rewrites::flash::FlashRewriteCtx {
                class_names: class_names.clone(),
                ancestors: HashSet::new(),
                method_names: HashSet::new(),
                instance_fields: HashSet::new(),
                has_self: false,
                suppress_super: false,
                is_cinit: false,
                static_fields: HashSet::new(),
                static_method_owners: HashMap::new(),
                static_field_owners: HashMap::new(),
                const_instance_fields: HashSet::new(),
                class_short_name: None,
                bindable_methods: HashSet::new(),
                closure_bodies: HashMap::new(),
                known_classes: known_classes.clone(),
            };
            crate::rewrites::flash::rewrite_flash_function(js_func, &rewrite_ctx)
        }
    };
    rewrite_global_assignments(&mut js_func.body, mutable_global_names);
    crate::ast_printer::print_function(&js_func, out);
    Ok(())
}

// ---------------------------------------------------------------------------
// Class grouping and emission
// ---------------------------------------------------------------------------

struct ClassGroup {
    class_def: ClassDef,
    struct_def: StructDef,
    methods: Vec<FuncId>,
}

/// Partition module contents into class groups and free functions.
///
/// Classes are returned in topological order (superclass before subclass) so
/// that barrel-file exports work correctly with bundlers like esbuild that
/// flatten modules into a single scope.
fn group_by_class(module: &Module) -> (Vec<ClassGroup>, Vec<FuncId>) {
    let mut claimed: HashSet<FuncId> = HashSet::new();
    let mut groups = Vec::new();

    for class in &module.classes {
        let struct_def = module.structs[class.struct_index].clone();
        let methods: Vec<FuncId> = class
            .methods
            .iter()
            .filter(|&&fid| {
                if module.functions.get(fid).is_some() {
                    claimed.insert(fid);
                    true
                } else {
                    false
                }
            })
            .copied()
            .collect();
        groups.push(ClassGroup {
            class_def: class.clone(),
            struct_def,
            methods,
        });
    }

    let free: Vec<FuncId> = module
        .functions
        .keys()
        .filter(|fid| !claimed.contains(fid))
        .collect();

    (groups, free)
}

/// Map an IR `Type` to the AS3-style type name used in describeType output.
fn as3_type_name(ty: &Type) -> String {
    match ty {
        Type::Void => "void".into(),
        Type::Bool => "Boolean".into(),
        Type::Int(_) => "int".into(),
        Type::UInt(_) => "uint".into(),
        Type::Float(_) => "Number".into(),
        Type::String => "String".into(),
        Type::Array(_) => "Array".into(),
        Type::Map(_, _) => "Object".into(),
        Type::Struct(name) | Type::Enum(name) => {
            name.rsplit("::").next().unwrap_or(name).into()
        }
        _ => "*".into(),
    }
}

/// Emit a `registerClassTraits(ClassName, [...instance], [...static])` call.
fn emit_register_class_traits(
    group: &ClassGroup,
    module: &Module,
    out: &mut String,
) {
    let class_name = sanitize_ident(&group.class_def.name);

    // Collect instance traits: fields from struct_def + instance methods/getters/setters
    let mut instance_traits = Vec::new();
    for (name, ty, _) in &group.struct_def.fields {
        let type_name = as3_type_name(ty);
        instance_traits.push(format!(
            "{{ name: \"{name}\", kind: \"variable\", type: \"{type_name}\" }}"
        ));
    }

    // Collect static traits: fields from class_def + static methods
    let mut static_traits = Vec::new();
    for (name, ty, _) in &group.class_def.static_fields {
        let type_name = as3_type_name(ty);
        static_traits.push(format!(
            "{{ name: \"{name}\", kind: \"variable\", type: \"{type_name}\" }}"
        ));
    }
    // Track getter/setter pairs to coalesce into accessors
    let mut instance_accessors: BTreeMap<String, (bool, bool)> = BTreeMap::new();
    for &fid in &group.methods {
        let func = &module.functions[fid];
        // Strip class prefix: "Enum::toString" → "toString"
        let short = func.name.rsplit("::").next().unwrap_or(&func.name);
        match func.method_kind {
            MethodKind::Constructor | MethodKind::Free | MethodKind::Closure => {}
            MethodKind::Instance => {
                instance_traits.push(format!(
                    "{{ name: \"{short}\", kind: \"method\" }}"
                ));
            }
            MethodKind::Static => {
                // Skip class initializer
                if short == "cinit" || short == "$cinit" {
                    continue;
                }
                static_traits.push(format!(
                    "{{ name: \"{short}\", kind: \"method\" }}"
                ));
            }
            MethodKind::Getter => {
                // Strip get_ prefix to match AS3 accessor names
                let acc_name = short.strip_prefix("get_").unwrap_or(short);
                let entry = instance_accessors.entry(acc_name.to_string()).or_insert((false, false));
                entry.0 = true;
            }
            MethodKind::Setter => {
                let acc_name = short.strip_prefix("set_").unwrap_or(short);
                let entry = instance_accessors.entry(acc_name.to_string()).or_insert((false, false));
                entry.1 = true;
            }
        }
    }

    // Emit coalesced accessors
    for (name, (has_get, has_set)) in &instance_accessors {
        let access = match (has_get, has_set) {
            (true, true) => "readwrite",
            (true, false) => "readonly",
            (false, true) => "writeonly",
            _ => "readwrite",
        };
        instance_traits.push(format!(
            "{{ name: \"{name}\", kind: \"accessor\", access: \"{access}\" }}"
        ));
    }

    let instance_arr = instance_traits.join(", ");
    let static_arr = static_traits.join(", ");
    let _ = writeln!(
        out,
        "registerClassTraits({class_name}, [{instance_arr}], [{static_arr}]);\n"
    );
}

/// Emit a TypeScript class from a `ClassGroup`.
#[allow(clippy::too_many_arguments)]
fn emit_class(
    group: &ClassGroup,
    module: &mut Module,
    class_names: &HashMap<String, String>,
    class_meta: &ClassMeta,
    mutable_global_names: &HashSet<String>,
    late_bound: &HashSet<String>,
    short_to_qualified: &HashMap<String, String>,
    known_classes: &HashSet<String>,
    lowering_config: &LoweringConfig,
    engine: EngineKind,
    out: &mut String,
) -> Result<(), CoreError> {
    let class_name = sanitize_ident(&group.class_def.name);
    let vis = visibility_prefix(group.class_def.visibility);

    let extends = match &group.class_def.super_class {
        Some(sc) => {
            let base = sc.rsplit("::").next().unwrap_or(sc);
            // `extends Object` is redundant in JS — all classes extend Object implicitly.
            if base == "Object" {
                String::new()
            } else {
                format!(" extends {}", sanitize_ident(base))
            }
        }
        None => String::new(),
    };

    let abstract_kw = if group.class_def.is_interface { "abstract " } else { "" };
    let _ = writeln!(out, "{vis}{abstract_kw}class {class_name}{extends} {{");
    let qualified = qualified_class_name(&group.class_def);
    if engine == EngineKind::Flash {
        let _ = writeln!(out, "  static [QN_KEY] = \"{qualified}\";");
    }

    // Static fields from ClassDef (class-level Slot/Const + promoted instance Consts).
    for (name, ty, default) in &group.class_def.static_fields {
        let ident = sanitize_ident(name);
        let ts = ts_type(ty);
        if let Some(val) = default {
            let _ = writeln!(out, "  static readonly {ident}: {ts} = {};", crate::ast_printer::emit_constant(val));
        } else {
            let _ = writeln!(out, "  static {ident}: {ts};");
        }
    }

    // Instance fields from struct def.
    for (name, ty, default) in &group.struct_def.fields {
        let ident = sanitize_ident(name);
        let ts = ts_type(ty);
        if let Some(val) = default {
            let _ = writeln!(out, "  {ident}: {ts} = {};", crate::ast_printer::emit_constant(val));
        } else {
            let _ = writeln!(out, "  {ident}: {ts};");
        }
    }
    let has_fields = !group.struct_def.fields.is_empty() || !group.class_def.static_fields.is_empty();
    if has_fields && !group.methods.is_empty() {
        out.push('\n');
    }

    // Methods — sorted: constructor first, then instance, static, getters, setters.
    // For interfaces, skip the constructor (AS3 interfaces have no constructor bodies).
    // Closures are separated and compiled for inlining as arrow functions.
    let mut sorted_methods: Vec<FuncId> = group.methods.iter()
        .copied()
        .filter(|&fid| {
            let mk = module.functions[fid].method_kind;
            if group.class_def.is_interface && mk == MethodKind::Constructor {
                return false;
            }
            mk != MethodKind::Closure
        })
        .collect();
    let closure_fids: Vec<FuncId> = group.methods.iter()
        .copied()
        .filter(|&fid| module.functions[fid].method_kind == MethodKind::Closure)
        .collect();
    sorted_methods.sort_by_key(|&fid| match module.functions[fid].method_kind {
        MethodKind::Constructor => 0,
        MethodKind::Instance => 1,
        MethodKind::Getter => 2,
        MethodKind::Setter => 3,
        MethodKind::Static => 4,
        MethodKind::Free => 5,
        MethodKind::Closure => 6,
    });

    let empty_set = HashSet::new();
    let empty_map = HashMap::new();
    let ancestors = class_meta.ancestor_sets.get(&qualified).unwrap_or(&empty_set);
    let method_names = class_meta.method_name_sets.get(&qualified).unwrap_or(&empty_set);
    let instance_fields = class_meta.instance_field_sets.get(&qualified).unwrap_or(&empty_set);
    let static_method_owners = class_meta.static_method_owner_map.get(&qualified).unwrap_or(&empty_map);
    let static_field_owners = class_meta.static_field_owner_map.get(&qualified).unwrap_or(&empty_map);
    let bindable_methods = class_meta.bindable_method_sets.get(&qualified).unwrap_or(&empty_set);
    let static_fields: HashSet<String> = group.class_def.static_fields.iter()
        .map(|(name, _, _)| name.clone())
        .collect();
    // Const instance fields promoted to static — entries with a value.
    let const_instance_fields: HashSet<String> = group.class_def.static_fields.iter()
        .filter(|(_, _, val)| val.is_some())
        .map(|(name, _, _)| name.clone())
        .collect();

    let suppress_super = extends.is_empty();

    // Compile closure bodies for inlining as arrow functions.
    let closure_bodies = compile_closures(
        &closure_fids,
        module,
        lowering_config,
    );

    for (i, &fid) in sorted_methods.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        emit_class_method(&mut module.functions[fid], class_names, ancestors, method_names, instance_fields, &static_fields, static_method_owners, static_field_owners, suppress_super, &const_instance_fields, &class_name, mutable_global_names, late_bound, short_to_qualified, bindable_methods, &closure_bodies, known_classes, lowering_config, engine, out)?;
    }

    let _ = writeln!(out, "}}\n");
    // Flash-specific class registration.
    if engine == EngineKind::Flash {
        let _ = writeln!(out, "registerClass({class_name});\n");
        // Skip registerClassTraits for interfaces (they have no runtime traits).
        if !group.class_def.is_interface {
            emit_register_class_traits(group, module, out);
        }
        // Emit registerInterface for implementing classes.
        if !group.class_def.interfaces.is_empty() {
            let iface_names: Vec<String> = group.class_def.interfaces.iter()
                .map(|name| {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    sanitize_ident(short)
                })
                .collect();
            let _ = writeln!(out, "registerInterface({class_name}, {});\n", iface_names.join(", "));
        }
    }
    Ok(())
}

/// Compile closure functions into JS AST form for inlining as arrow functions.
///
/// Closures are lowered through the same pipeline (structurize → linear → JS lower)
/// but WITHOUT the flash rewrite pass — that happens when the closure is inlined
/// into its parent method via the `newFunction` SystemCall rewrite.
fn compile_closures(
    closure_fids: &[FuncId],
    module: &mut Module,
    lowering_config: &LoweringConfig,
) -> HashMap<String, JsFunction> {
    use reincarnate_core::ir::linear;

    let mut result = HashMap::new();
    for &fid in closure_fids {
        let func = &mut module.functions[fid];
        let short = func
            .name
            .rsplit("::")
            .next()
            .unwrap_or(&func.name)
            .to_string();

        let shape = structurize::structurize(func);
        let ast = linear::lower_function_linear(func, &shape, lowering_config);

        // Closures: self_param_name = None — the first param is the activation
        // scope, NOT `this`. This prevents the lowering pass from substituting
        // it with JsExpr::This.
        let ctx = crate::lower::LowerCtx {
            self_param_name: None,
        };
        let js_func = crate::lower::lower_function(&ast, &ctx);
        result.insert(short, js_func);
    }
    result
}

/// Emit a single method inside a class body.
fn emit_class_method(
    func: &mut Function,
    class_names: &HashMap<String, String>,
    ancestors: &HashSet<String>,
    method_names: &HashSet<String>,
    instance_fields: &HashSet<String>,
    static_fields: &HashSet<String>,
    static_method_owners: &HashMap<String, String>,
    static_field_owners: &HashMap<String, String>,
    suppress_super: bool,
    const_instance_fields: &HashSet<String>,
    class_short_name: &str,
    mutable_global_names: &HashSet<String>,
    late_bound: &HashSet<String>,
    short_to_qualified: &HashMap<String, String>,
    bindable_methods: &HashSet<String>,
    closure_bodies: &HashMap<String, JsFunction>,
    known_classes: &HashSet<String>,
    lowering_config: &LoweringConfig,
    engine: EngineKind,
    out: &mut String,
) -> Result<(), CoreError> {
    #![allow(clippy::too_many_arguments)]
    use reincarnate_core::ir::linear;

    let raw_name = func
        .name
        .rsplit("::")
        .next()
        .unwrap_or(&func.name)
        .to_string();

    let skip_self = matches!(
        func.method_kind,
        MethodKind::Constructor
            | MethodKind::Instance
            | MethodKind::Getter
            | MethodKind::Setter
            | MethodKind::Static
            | MethodKind::Closure
    );

    let shape = structurize::structurize(func);
    let ast = linear::lower_function_linear(func, &shape, lowering_config);

    // Determine self_param_name for `this` substitution.
    let is_cinit = raw_name == "cinit" && matches!(func.method_kind, MethodKind::Static);
    let self_param_name = if is_cinit {
        ast.params.first().map(|(n, _)| n.clone())
    } else if skip_self && !ast.params.is_empty() {
        Some(ast.params[0].0.clone())
    } else {
        None
    };

    let ctx = crate::lower::LowerCtx { self_param_name };
    let js_func = crate::lower::lower_function(&ast, &ctx);
    let mut js_func = match engine {
        EngineKind::GameMaker => crate::rewrites::gamemaker::rewrite_gamemaker_function(js_func),
        EngineKind::Flash => {
            let rewrite_ctx = crate::rewrites::flash::FlashRewriteCtx {
                class_names: class_names.clone(),
                ancestors: ancestors.clone(),
                method_names: method_names.clone(),
                instance_fields: instance_fields.clone(),
                has_self: true,
                suppress_super,
                is_cinit,
                static_fields: static_fields.clone(),
                static_method_owners: static_method_owners.clone(),
                static_field_owners: static_field_owners.clone(),
                const_instance_fields: const_instance_fields.clone(),
                class_short_name: Some(class_short_name.to_string()),
                bindable_methods: if is_cinit || matches!(func.method_kind, MethodKind::Static | MethodKind::Closure) {
                    HashSet::new()
                } else {
                    bindable_methods.clone()
                },
                closure_bodies: closure_bodies.clone(),
                known_classes: known_classes.clone(),
            };
            let mut jf = crate::rewrites::flash::rewrite_flash_function(js_func, &rewrite_ctx);
            crate::rewrites::flash::eliminate_dead_activations(&mut jf.body);
            jf
        }
    };
    rewrite_global_assignments(&mut js_func.body, mutable_global_names);
    rewrite_late_bound_types(&mut js_func.body, late_bound, short_to_qualified);
    // Hoist super() to top of constructor body (after rewrite produces SuperCall nodes).
    if engine == EngineKind::Flash && func.method_kind == MethodKind::Constructor {
        crate::rewrites::flash::hoist_super_call(
            &mut js_func.body,
            Some(class_short_name),
        );
    }
    // Filter cinit: remove assignments that duplicate static readonly field defaults,
    // and skip emitting entirely if the body is empty after filtering.
    if is_cinit {
        js_func.body.retain(|stmt| {
            !is_redundant_static_assign(stmt, const_instance_fields)
        });
        if js_func.body.is_empty() {
            return Ok(());
        }
    }
    crate::ast_printer::print_class_method(&js_func, &raw_name, skip_self, out);
    Ok(())
}
/// Whether a cinit statement is a redundant assignment to a field that already
/// has a `static readonly` default value on the class.
fn is_redundant_static_assign(stmt: &JsStmt, const_fields: &HashSet<String>) -> bool {
    if let JsStmt::Assign {
        target: JsExpr::Field { object, field },
        ..
    } = stmt
    {
        matches!(**object, JsExpr::This) && const_fields.contains(field)
    } else {
        false
    }
}

fn visibility_prefix(vis: Visibility) -> &'static str {
    match vis {
        Visibility::Public => "export ",
        Visibility::Private | Visibility::Protected => "",
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use reincarnate_core::ir::builder::{FunctionBuilder, ModuleBuilder};
    use reincarnate_core::ir::{
        ClassDef, CmpKind, EnumDef, EnumVariant, FunctionSig, Global, Import, MethodKind,
        StructDef, Visibility,
    };

    fn build_and_emit(build: impl FnOnce(&mut ModuleBuilder)) -> String {
        let mut mb = ModuleBuilder::new("test");
        build(&mut mb);
        emit_module_to_string(&mut mb.build(), &LoweringConfig::default(), None).unwrap()
    }

    #[test]
    fn simple_add_function() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64), Type::Int(64)],
                return_ty: Type::Int(64), ..Default::default() };
            let mut fb = FunctionBuilder::new("add", sig, Visibility::Public);
            let a = fb.param(0);
            let b = fb.param(1);
            let sum = fb.add(a, b);
            fb.ret(Some(sum));
            mb.add_function(fb.build());
        });

        assert!(out.contains("export function add(v0: number, v1: number): number {"));
        // Single-use sum is inlined into return.
        assert!(out.contains("return v0 + v1;"), "Should inline sum into return:\n{out}");
        assert!(!out.contains("let v2"), "Single-use v2 should be inlined:\n{out}");
        // Single block → no dispatch loop.
        assert!(!out.contains("$block"));
    }

    #[test]
    fn branching_with_block_args() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
                return_ty: Type::Int(64), ..Default::default() };
            let mut fb = FunctionBuilder::new("choose", sig, Visibility::Public);

            let cond = fb.param(0);
            let x = fb.param(1);
            let y = fb.param(2);

            let (then_block, then_vals) = fb.create_block_with_params(&[Type::Int(64)]);
            let (else_block, else_vals) = fb.create_block_with_params(&[Type::Int(64)]);

            fb.br_if(cond, then_block, &[x], else_block, &[y]);

            fb.switch_to_block(then_block);
            fb.ret(Some(then_vals[0]));

            fb.switch_to_block(else_block);
            fb.ret(Some(else_vals[0]));

            mb.add_function(fb.build());
        });

        // Structured output: if/else instead of dispatch loop.
        assert!(!out.contains("$block"), "Should not use dispatch loop:\n{out}");
        assert!(out.contains("if (v0)"), "Should have if (v0):\n{out}");
        // Block-param vars folded away: return v1/v2 directly.
        assert!(out.contains("return v1;"), "Should return v1 directly:\n{out}");
        assert!(out.contains("return v2;"), "Should return v2 directly:\n{out}");
    }

    #[test]
    fn struct_emission() {
        let out = build_and_emit(|mb| {
            mb.add_struct(StructDef {
                name: "Point".into(),
                namespace: Vec::new(),
                fields: vec![
                    ("x".into(), Type::Float(64), None),
                    ("y".into(), Type::Float(64), None),
                ],
                visibility: Visibility::Public,
            });
        });

        assert!(out.contains("export interface Point {"));
        assert!(out.contains("  x: number;"));
        assert!(out.contains("  y: number;"));
    }

    #[test]
    fn enum_emission() {
        let out = build_and_emit(|mb| {
            mb.add_enum(EnumDef {
                name: "Shape".into(),
                variants: vec![
                    EnumVariant {
                        name: "Circle".into(),
                        fields: vec![Type::Float(64)],
                    },
                    EnumVariant {
                        name: "Rect".into(),
                        fields: vec![Type::Float(64), Type::Float(64)],
                    },
                ],
                visibility: Visibility::Public,
            });
        });

        assert!(out.contains("export type Shape ="));
        assert!(out.contains("tag: \"Circle\""));
        assert!(out.contains("tag: \"Rect\""));
    }

    #[test]
    fn global_variables() {
        let out = build_and_emit(|mb| {
            mb.add_global(Global {
                name: "counter".into(),
                ty: Type::Int(64),
                visibility: Visibility::Public,
                mutable: true,
                init: None,
            });
            mb.add_global(Global {
                name: "MAX_SIZE".into(),
                ty: Type::Int(64),
                visibility: Visibility::Private,
                mutable: false,
                init: None,
            });
        });

        assert!(out.contains("export let counter: number;"));
        // const without init demoted to let
        assert!(out.contains("let MAX_SIZE: number;"));
    }

    #[test]
    fn imports() {
        let out = build_and_emit(|mb| {
            mb.add_import(Import {
                module: "utils".into(),
                name: "helper".into(),
                alias: None,
            });
            mb.add_import(Import {
                module: "math".into(),
                name: "add".into(),
                alias: Some("mathAdd".into()),
            });
        });

        assert!(out.contains("import { helper } from \"./utils\";"));
        assert!(out.contains("import { add as mathAdd } from \"./math\";"));
    }

    #[test]
    fn system_call() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);
            let x = fb.const_int(100);
            let y = fb.const_int(200);
            fb.system_call("renderer", "clear", &[x, y], Type::Void);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        // Auto-injected runtime import.
        assert!(out.contains("import { renderer } from \"./runtime\";"));
        // Constants are inlined into the system call.
        assert!(
            out.contains("renderer.clear(100, 200);"),
            "Should inline consts into system call:\n{out}"
        );
    }

    #[test]
    fn multiple_system_imports() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("tick", sig, Visibility::Public);
            fb.system_call("timing", "tick", &[], Type::Void);
            fb.system_call("input", "update", &[], Type::Void);
            fb.system_call("renderer", "present", &[], Type::Void);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(out.contains("import { input, renderer, timing } from \"./runtime\";"));
    }

    #[test]
    fn no_runtime_import_without_system_calls() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("noop", sig, Visibility::Public);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(!out.contains("import"));
    }

    #[test]
    fn constants_all_types() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("constants", sig, Visibility::Public);
            // Each constant is passed to a call so it actually appears in output.
            let a = fb.const_null();
            fb.call("use_val", &[a], Type::Void);
            let b = fb.const_bool(true);
            fb.call("use_val", &[b], Type::Void);
            let c = fb.const_bool(false);
            fb.call("use_val", &[c], Type::Void);
            let d = fb.const_int(42);
            fb.call("use_val", &[d], Type::Void);
            let e = fb.const_float(3.125);
            fb.call("use_val", &[e], Type::Void);
            let f = fb.const_string("hello \"world\"\nnewline");
            fb.call("use_val", &[f], Type::Void);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(out.contains("null"), "Should contain null:\n{out}");
        assert!(out.contains("true"), "Should contain true:\n{out}");
        assert!(out.contains("false"), "Should contain false:\n{out}");
        assert!(out.contains("42"), "Should contain 42:\n{out}");
        assert!(out.contains("3.125"), "Should contain 3.125:\n{out}");
        assert!(out.contains(r#""hello \"world\"\nnewline""#), "Should contain escaped string:\n{out}");
    }

    #[test]
    fn array_and_struct_init() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);

            let a = fb.const_int(1);
            let b = fb.const_int(2);
            let arr = fb.array_init(&[a, b], Type::Int(64));
            fb.call("use_val", &[arr], Type::Void);

            let x = fb.const_float(10.0);
            let y = fb.const_float(20.0);
            let obj = fb.struct_init("Point", vec![("x".into(), x), ("y".into(), y)]);
            fb.call("use_val", &[obj], Type::Void);

            fb.ret(None);
            mb.add_function(fb.build());
        });

        // Constants are inlined into the aggregate expressions.
        assert!(out.contains("[1, 2]"), "Should inline consts into array:\n{out}");
        assert!(
            out.contains("{ x: 10.0, y: 20.0 }"),
            "Should inline consts into struct:\n{out}"
        );
    }

    #[test]
    fn mem2reg_plus_emit_eliminates_alloc_store_load() {
        use reincarnate_core::pipeline::Transform;
        use reincarnate_core::transforms::Mem2Reg;

        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("identity", sig, Visibility::Public);
        let param = fb.param(0);
        // Alloc → Store → Load chain (typical local variable pattern).
        let ptr = fb.alloc(Type::Int(64));
        fb.store(ptr, param);
        let loaded = fb.load(ptr, Type::Int(64));
        fb.ret(Some(loaded));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();

        // Run mem2reg IR pass, then emit.
        let mut result = Mem2Reg.apply(module).unwrap();
        let out = emit_module_to_string(&mut result.module, &LoweringConfig::default(), None).unwrap();

        // The alloc/store/load should be eliminated; return refers to the
        // original parameter directly.
        assert!(out.contains("return v0;"));
        assert!(!out.contains("undefined"));
    }

    #[test]
    fn sanitize_ident_handles_avm2_names() {
        assert_eq!(sanitize_ident("Flash.Object"), "Flash_Object");
        assert_eq!(sanitize_ident("flash.display::Loader"), "flash_display__Loader");
        assert_eq!(sanitize_ident("4l9JT7u2nN1ZFk+5"), "_4l9JT7u2nN1ZFk_5");
        assert_eq!(sanitize_ident("l/YEs377IakicDh/"), "l_YEs377IakicDh_");
        assert_eq!(sanitize_ident("normal_name"), "normal_name");
    }

    #[test]
    fn sanitize_ident_escapes_reserved_words() {
        assert_eq!(sanitize_ident("function"), "_function");
        assert_eq!(sanitize_ident("class"), "_class");
        assert_eq!(sanitize_ident("this"), "_this");
        assert_eq!(sanitize_ident("let"), "_let");
        // Non-reserved words pass through unchanged.
        assert_eq!(sanitize_ident("foo"), "foo");
    }

    #[test]
    fn bracket_notation_for_non_ident_fields() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Dynamic, ..Default::default() };
            let mut fb = FunctionBuilder::new("get_prop", sig, Visibility::Public);
            let obj = fb.param(0);
            // Qualified field → short name extraction.
            let result = fb.get_field(obj, "flash.display::Loader", Type::Dynamic);
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        // Qualified field should extract short name and use dot notation.
        assert!(out.contains(".Loader"), "Should use short name:\n{out}");
        assert!(
            !out.contains("flash.display::Loader"),
            "Should not have full qualified name:\n{out}"
        );
    }

    #[test]
    fn emit_structured_if_else() {
        //   entry: br_if cond, then, else
        //   then:  br merge
        //   else:  br merge
        //   merge: return
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("diamond", sig, Visibility::Public);
            let cond = fb.param(0);

            let then_block = fb.create_block();
            let else_block = fb.create_block();
            let merge_block = fb.create_block();

            fb.br_if(cond, then_block, &[], else_block, &[]);

            fb.switch_to_block(then_block);
            fb.br(merge_block, &[]);

            fb.switch_to_block(else_block);
            fb.br(merge_block, &[]);

            fb.switch_to_block(merge_block);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        // Both branches are empty — entire if should be omitted.
        assert!(!out.contains("$block"), "Should not use dispatch loop:\n{out}");
        assert!(!out.contains("if ("), "Empty diamond should omit entire if:\n{out}");
        // Trailing void return should be stripped.
        assert!(!out.contains("return;"), "Should not have trailing return:\n{out}");
    }

    #[test]
    fn emit_while_loop() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("while_loop", sig, Visibility::Public);
            let cond = fb.param(0);

            let header = fb.create_block();
            let body = fb.create_block();
            let exit = fb.create_block();

            fb.br(header, &[]);

            fb.switch_to_block(header);
            fb.br_if(cond, body, &[], exit, &[]);

            fb.switch_to_block(body);
            fb.br(header, &[]);

            fb.switch_to_block(exit);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        assert!(!out.contains("$block"), "Should not use dispatch loop:\n{out}");
        assert!(out.contains("while ("), "Should have while loop:\n{out}");
    }

    #[test]
    fn emit_for_loop() {
        use reincarnate_core::ir::CmpKind;

        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("for_loop", sig, Visibility::Public);

            let (header, header_vals) = fb.create_block_with_params(&[Type::Int(64)]);
            let body = fb.create_block();
            let exit = fb.create_block();

            let v_init = fb.const_int(0);
            fb.br(header, &[v_init]);

            fb.switch_to_block(header);
            let v_i = header_vals[0];
            let v_n = fb.const_int(10);
            let v_cond = fb.cmp(CmpKind::Lt, v_i, v_n);
            fb.br_if(v_cond, body, &[], exit, &[]);

            fb.switch_to_block(body);
            let v_one = fb.const_int(1);
            let v_next = fb.add(v_i, v_one);
            fb.br(header, &[v_next]);

            fb.switch_to_block(exit);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        assert!(!out.contains("$block"), "Should not use dispatch loop:\n{out}");
        // For-loop emits as while(cond) with init assigns before and
        // update assigns inside.
        assert!(out.contains("while ("), "Should have loop:\n{out}");
        // Init assigns header param v0 from inlined const 0 (merged into decl).
        assert!(out.contains("let v0: number = 0;"), "Should have init assign:\n{out}");
        // Update assigns header param v0 from inlined v0 + 1 (compound assignment).
        assert!(out.contains("v0 += 1;"), "Should have update assign:\n{out}");
    }

    #[test]
    fn emit_class_with_methods() {
        let mut mb = ModuleBuilder::new("test");

        // Struct for class fields.
        mb.add_struct(StructDef {
            name: "Fighter".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            fields: vec![("hp".into(), Type::Int(32), None)],
            visibility: Visibility::Public,
        });

        // Constructor: (this: dyn) -> void
        let ctor_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Fighter::new", ctor_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Fighter".into(),
            MethodKind::Constructor,
        );
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        // Instance method: (this: dyn, amount: i32) -> void
        let method_sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Int(32)],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Fighter::attack", method_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Fighter".into(),
            MethodKind::Instance,
        );
        let _this = fb.param(0);
        let _amount = fb.param(1);
        fb.ret(None);
        let method_id = mb.add_function(fb.build());

        // Static method: (self: dyn, amount: i32) -> i32
        // AVM2 register 0 is always reserved, so static methods include
        // a self/scope param that the emitter skips.
        let static_sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Int(32)],
            return_ty: Type::Int(32), ..Default::default() };
        let mut fb = FunctionBuilder::new("Fighter::create", static_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Fighter".into(),
            MethodKind::Static,
        );
        let _self = fb.param(0);
        let p = fb.param(1);
        fb.ret(Some(p));
        let static_id = mb.add_function(fb.build());

        // Getter: (this: dyn) -> i32
        let getter_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Int(32), ..Default::default() };
        let mut fb = FunctionBuilder::new("Fighter::get_health", getter_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Fighter".into(),
            MethodKind::Getter,
        );
        let this = fb.param(0);
        let hp = fb.get_field(this, "hp", Type::Int(32));
        fb.ret(Some(hp));
        let getter_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Fighter".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            struct_index: 0,
            methods: vec![ctor_id, method_id, static_id, getter_id],
            super_class: Some("Object".into()),
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        // Class declaration — `extends Object` is suppressed (redundant in JS).
        assert!(
            out.contains("export class Fighter {"),
            "Should have class decl without extends Object:\n{out}"
        );
        // Field.
        assert!(out.contains("  hp: number;"), "Should have field:\n{out}");
        // Constructor — no `this` param.
        assert!(
            out.contains("  constructor() {"),
            "Should have constructor:\n{out}"
        );
        // Instance method — skips `this`.
        assert!(
            out.contains("  attack(v1: number): void {"),
            "Should have instance method:\n{out}"
        );
        // Static method — skips self param (AVM2 register 0).
        assert!(
            out.contains("  static create(v1: number): number {"),
            "Should have static method:\n{out}"
        );
        // Getter — strips `get_` prefix, body uses `this`.
        assert!(
            out.contains("  get health(): number {"),
            "Should have getter:\n{out}"
        );
        assert!(
            out.contains("this.hp"),
            "Getter body should use `this.hp`:\n{out}"
        );
    }

    #[test]
    fn emit_class_and_free_functions() {
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Foo".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Foo::new", sig, Visibility::Public);
        fb.set_class(Vec::new(), "Foo".into(), MethodKind::Constructor);
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Foo".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![ctor_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        // Free function.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);
        fb.ret(None);
        mb.add_function(fb.build());

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(out.contains("export class Foo {"), "Should have class:\n{out}");
        assert!(
            out.contains("export function init(): void {"),
            "Should have free function:\n{out}"
        );
    }

    #[test]
    fn relative_import_path_same_dir() {
        let from = vec!["classes".into(), "Scenes".into(), "Swamp".into(), "Swamp".into()];
        let to = vec![
            "classes".into(),
            "Scenes".into(),
            "Swamp".into(),
            "CorruptedDriderScene".into(),
        ];
        assert_eq!(relative_import_path(&from, &to), "./CorruptedDriderScene");
    }

    #[test]
    fn relative_import_path_different_dir() {
        let from = vec!["classes".into(), "Scenes".into(), "Swamp".into(), "Swamp".into()];
        let to = vec!["classes".into(), "CoC".into()];
        assert_eq!(relative_import_path(&from, &to), "../../CoC");
    }

    #[test]
    fn relative_import_path_no_common() {
        let from = vec!["a".into(), "b".into()];
        let to = vec!["c".into(), "d".into()];
        assert_eq!(relative_import_path(&from, &to), "../c/d");
    }

    #[test]
    fn emit_nested_class_directory() {
        let dir = tempfile::tempdir().unwrap();
        let mut mb = ModuleBuilder::new("frame1");

        // Class with namespace → nested directory.
        mb.add_struct(StructDef {
            name: "Swamp".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            fields: vec![("hp".into(), Type::Int(32), None)],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Swamp::new", sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Swamp".into(),
            MethodKind::Constructor,
        );
        fb.system_call("renderer", "clear", &[], Type::Void);
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Swamp".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            struct_index: 0,
            methods: vec![ctor_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        emit_module_to_dir(&mut module, dir.path(), &LoweringConfig::default(), None).unwrap();

        // Check nested file exists.
        let class_file = dir
            .path()
            .join("frame1/classes/Scenes/Swamp.ts");
        assert!(class_file.exists(), "Nested class file should exist");

        let content = fs::read_to_string(&class_file).unwrap();

        // Runtime import should go up 3 levels (2 namespace segments + 1 module dir).
        assert!(
            content.contains("from \"../../../runtime\""),
            "Runtime import should use depth-relative path:\n{content}"
        );

        // Barrel file should have nested re-export.
        let barrel = fs::read_to_string(dir.path().join("frame1/index.ts")).unwrap();
        assert!(
            barrel.contains("export * from \"./classes/Scenes/Swamp\";"),
            "Barrel should have nested export path:\n{barrel}"
        );
    }

    #[test]
    fn emit_intra_module_imports() {
        let dir = tempfile::tempdir().unwrap();
        let mut mb = ModuleBuilder::new("frame1");

        // Two classes: Monster (root) and Swamp (nested), where Swamp references Monster.
        mb.add_struct(StructDef {
            name: "Monster".into(),
            namespace: vec!["classes".into()],
            fields: vec![],
            visibility: Visibility::Public,
        });
        mb.add_struct(StructDef {
            name: "Swamp".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            fields: vec![("boss".into(), Type::Struct("classes::Monster".into()), None)],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Monster::new", sig.clone(), Visibility::Public);
        fb.set_class(vec!["classes".into()], "Monster".into(), MethodKind::Constructor);
        fb.ret(None);
        let monster_ctor = mb.add_function(fb.build());

        let mut fb = FunctionBuilder::new("Swamp::new", sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Swamp".into(),
            MethodKind::Constructor,
        );
        fb.ret(None);
        let swamp_ctor = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Monster".into(),
            namespace: vec!["classes".into()],
            struct_index: 0,
            methods: vec![monster_ctor],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        mb.add_class(ClassDef {
            name: "Swamp".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            struct_index: 1,
            methods: vec![swamp_ctor],
            super_class: Some("classes::Monster".into()),
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        emit_module_to_dir(&mut module, dir.path(), &LoweringConfig::default(), None).unwrap();

        let swamp_file = dir.path().join("frame1/classes/Scenes/Swamp.ts");
        let content = fs::read_to_string(&swamp_file).unwrap();

        // Swamp extends Monster → should have import for Monster.
        assert!(
            content.contains("import { Monster } from \"../Monster\";"),
            "Should import Monster from parent dir:\n{content}"
        );
    }

    #[test]
    fn construct_super_emits_super_call() {
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Child".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Child::new", sig, Visibility::Public);
        fb.set_class(Vec::new(), "Child".into(), MethodKind::Constructor);
        let this = fb.param(0);
        // Place a field init before constructSuper — mimics AVM2 constructor order
        let val = fb.const_int(0);
        fb.set_field(this, "x", val);
        fb.system_call("Flash.Class", "constructSuper", &[this], Type::Void);
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Child".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![ctor_id],
            super_class: Some("Parent".into()),
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("super();"),
            "constructSuper should emit super():\n{out}"
        );
        assert!(
            !out.contains("constructSuper"),
            "Should not have raw constructSuper call:\n{out}"
        );
        // super() must come before any this.field access
        let super_pos = out.find("super();").expect("super() not found");
        let field_pos = out.find("this.x").expect("this.x not found");
        assert!(
            super_pos < field_pos,
            "super() must precede this.x access:\n{out}"
        );
    }

    #[test]
    fn find_prop_strict_get_field_construct_emits_new() {
        let mut mb = ModuleBuilder::new("test");

        // Two classes: Container and Widget. Container constructs a Widget.
        mb.add_struct(StructDef {
            name: "Container".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        mb.add_struct(StructDef {
            name: "Widget".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });

        // Container constructor does findPropStrict + getField + construct.
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Container::new", sig, Visibility::Public);
        fb.set_class(Vec::new(), "Container".into(), MethodKind::Constructor);
        let _this = fb.param(0);

        // findPropStrict("Widget")
        let name = fb.const_string("Widget");
        let scope = fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
        // getField(scope, "Widget")
        let ctor = fb.get_field(scope, "Widget", Type::Dynamic);
        // construct(ctor)
        let obj = fb.system_call("Flash.Object", "construct", &[ctor], Type::Dynamic);
        // Use the result so it's not dead code.
        fb.set_field(_this, "child", obj);
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        // Widget constructor (empty).
        let sig2 = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb2 = FunctionBuilder::new("Widget::new", sig2, Visibility::Public);
        fb2.set_class(Vec::new(), "Widget".into(), MethodKind::Constructor);
        fb2.ret(None);
        let widget_ctor_id = mb.add_function(fb2.build());

        mb.add_class(ClassDef {
            name: "Container".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![ctor_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        mb.add_class(ClassDef {
            name: "Widget".into(),
            namespace: Vec::new(),
            struct_index: 1,
            methods: vec![widget_ctor_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("new Widget()"),
            "Should emit new Widget():\n{out}"
        );
        assert!(
            !out.contains("Flash_Object.construct"),
            "Should not have Flash_Object.construct call:\n{out}"
        );
        assert!(
            !out.contains("Flash_Scope.findPropStrict"),
            "findPropStrict should be resolved away:\n{out}"
        );
    }

    #[test]
    fn qualified_call_emits_method_dispatch() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Dynamic, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let receiver = fb.param(0);
            let arg1 = fb.const_string("text");
            let arg2 = fb.const_bool(true);
            // Method call: receiver.outputText("text", true)
            let result = fb.call_method(
                receiver,
                "outputText",
                &[arg1, arg2],
                Type::Dynamic,
            );
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains(".outputText("),
            "Should emit method dispatch:\n{out}"
        );
        assert!(
            !out.contains("classes_BaseContent__outputText"),
            "Should not emit sanitized function call:\n{out}"
        );
    }

    #[test]
    fn qualified_get_field_emits_short_name() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Dynamic, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let obj = fb.param(0);
            let result = fb.get_field(obj, "classes:BaseContent::flags", Type::Dynamic);
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(out.contains(".flags"), "Should use short field name:\n{out}");
        assert!(
            !out.contains("BaseContent"),
            "Should not have qualified name:\n{out}"
        );
    }

    #[test]
    fn qualified_set_field_emits_short_name() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic, Type::Int(32)],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let obj = fb.param(0);
            let val = fb.param(1);
            fb.set_field(obj, "classes:BaseContent::flags", val);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(
            out.contains(".flags = "),
            "Should use short field name for set:\n{out}"
        );
        assert!(
            !out.contains("BaseContent"),
            "Should not have qualified name:\n{out}"
        );
    }

    #[test]
    fn find_prop_strict_resolves_to_this_for_own_class() {
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Hero".into(),
            namespace: vec!["classes".into()],
            fields: vec![("hp".into(), Type::Int(32), None)],
            visibility: Visibility::Public,
        });

        // Instance method that does findPropStrict("classes:Hero::hp") + getField.
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut fb = FunctionBuilder::new("Hero::getHp", sig, Visibility::Public);
        fb.set_class(vec!["classes".into()], "Hero".into(), MethodKind::Instance);
        let _this = fb.param(0);
        let name = fb.const_string("classes:Hero::hp");
        let scope = fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
        let val = fb.get_field(scope, "classes:Hero::hp", Type::Int(32));
        fb.ret(Some(val));
        let method_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Hero".into(),
            namespace: vec!["classes".into()],
            struct_index: 0,
            methods: vec![method_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("this.hp"),
            "findPropStrict for own class should resolve to this.hp:\n{out}"
        );
        assert!(
            !out.contains("findPropStrict"),
            "findPropStrict should be resolved away:\n{out}"
        );
    }

    #[test]
    fn find_prop_strict_resolves_to_this_for_ancestor() {
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Base".into(),
            namespace: vec!["classes".into()],
            fields: vec![("player".into(), Type::Dynamic, None)],
            visibility: Visibility::Public,
        });
        mb.add_struct(StructDef {
            name: "Child".into(),
            namespace: vec!["classes".into()],
            fields: vec![],
            visibility: Visibility::Public,
        });

        // Child instance method does findPropStrict("classes:Base::player") + getField.
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut fb = FunctionBuilder::new("Child::getPlayer", sig, Visibility::Public);
        fb.set_class(vec!["classes".into()], "Child".into(), MethodKind::Instance);
        let _this = fb.param(0);
        let name = fb.const_string("classes:Base::player");
        let scope = fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
        let val = fb.get_field(scope, "classes:Base::player", Type::Dynamic);
        fb.ret(Some(val));
        let method_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Base".into(),
            namespace: vec!["classes".into()],
            struct_index: 0,
            methods: vec![],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        mb.add_class(ClassDef {
            name: "Child".into(),
            namespace: vec!["classes".into()],
            struct_index: 1,
            methods: vec![method_id],
            super_class: Some("classes::Base".into()),
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("this.player"),
            "findPropStrict for ancestor class should resolve to this.player:\n{out}"
        );
        assert!(
            !out.contains("findPropStrict"),
            "findPropStrict should be resolved away:\n{out}"
        );
    }

    #[test]
    fn find_prop_strict_in_free_function_resolves_to_bare_name() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Dynamic, ..Default::default() };
            let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);
            let name = fb.const_string("classes:Hero::hp");
            let scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            let val = fb.get_field(scope, "classes:Hero::hp", Type::Dynamic);
            fb.ret(Some(val));
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("findPropStrict"),
            "findPropStrict should be resolved away:\n{out}"
        );
        assert!(
            out.contains("hp"),
            "Should resolve to bare field name:\n{out}"
        );
        assert!(
            !out.contains("this.hp"),
            "Free function should not resolve to this:\n{out}"
        );
    }

    #[test]
    fn find_prop_strict_non_ancestor_resolves_to_bare_name() {
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Hero".into(),
            namespace: vec!["classes".into()],
            fields: vec![],
            visibility: Visibility::Public,
        });
        mb.add_struct(StructDef {
            name: "Villain".into(),
            namespace: vec!["classes".into()],
            fields: vec![("power".into(), Type::Int(32), None)],
            visibility: Visibility::Public,
        });

        // Hero method does findPropStrict("classes:Villain::power") — unrelated class.
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut fb = FunctionBuilder::new("Hero::spy", sig, Visibility::Public);
        fb.set_class(vec!["classes".into()], "Hero".into(), MethodKind::Instance);
        let _this = fb.param(0);
        let name = fb.const_string("classes:Villain::power");
        let scope = fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
        let val = fb.get_field(scope, "classes:Villain::power", Type::Int(32));
        fb.ret(Some(val));
        let method_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Hero".into(),
            namespace: vec!["classes".into()],
            struct_index: 0,
            methods: vec![method_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        mb.add_class(ClassDef {
            name: "Villain".into(),
            namespace: vec!["classes".into()],
            struct_index: 1,
            methods: vec![],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            !out.contains("this.power"),
            "Non-ancestor should not resolve to this:\n{out}"
        );
        assert!(
            !out.contains("findPropStrict"),
            "findPropStrict should be resolved to bare name:\n{out}"
        );
        assert!(
            out.contains("power"),
            "Should resolve to bare field name:\n{out}"
        );
    }

    #[test]
    fn unqualified_find_prop_strict_resolves_to_bare_name() {
        // findPropStrict("rand") + getField("rand") → rand
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Dynamic, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let name = fb.const_string("rand");
            let scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            let rand_fn = fb.get_field(scope, "rand", Type::Dynamic);
            let result = fb.call_indirect(rand_fn, &[x], Type::Dynamic);
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("Flash_Scope.findPropStrict"),
            "findPropStrict call should be resolved away:\n{out}"
        );
        assert!(
            out.contains("rand("),
            "Should resolve to bare function call:\n{out}"
        );
    }

    #[test]
    fn find_property_set_field_resolves_to_bare_assignment() {
        // findProperty("X") + setField("X", 5) → X = 5
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let name = fb.const_string("X");
            let scope =
                fb.system_call("Flash.Scope", "findProperty", &[name], Type::Dynamic);
            let val = fb.const_int(5);
            fb.set_field(scope, "X", val);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("Flash_Scope.findProperty"),
            "findProperty call should be resolved away:\n{out}"
        );
        assert!(
            out.contains("X = 5"),
            "Should resolve to bare assignment:\n{out}"
        );
    }

    #[test]
    fn find_property_resolves_to_this_for_ancestor() {
        // findProperty("classes:Base::temp") in instance method → this
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Base".into(),
            namespace: vec!["classes".into()],
            fields: vec![("temp".into(), Type::Dynamic, None)],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Base::setTemp", sig, Visibility::Public);
        fb.set_class(vec!["classes".into()], "Base".into(), MethodKind::Instance);
        let _this = fb.param(0);
        let v = fb.param(1);
        let name = fb.const_string("classes:Base::temp");
        let scope =
            fb.system_call("Flash.Scope", "findProperty", &[name], Type::Dynamic);
        fb.set_field(scope, "classes:Base::temp", v);
        fb.ret(None);
        let method_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Base".into(),
            namespace: vec!["classes".into()],
            struct_index: 0,
            methods: vec![method_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("this.temp = "),
            "findProperty for own class should resolve to this.temp:\n{out}"
        );
        assert!(
            !out.contains("findProperty"),
            "findProperty should be resolved away:\n{out}"
        );
    }

    #[test]
    fn qualified_find_prop_strict_non_class_resolves_to_bare_name() {
        // findPropStrict("flash.events::Event") + getField("Event") → Event
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Dynamic, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let name = fb.const_string("flash.events::Event");
            let scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            let event_cls = fb.get_field(scope, "flash.events::Event", Type::Dynamic);
            let change = fb.get_field(event_cls, "CHANGE", Type::Dynamic);
            fb.ret(Some(change));
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("Flash_Scope.findPropStrict"),
            "findPropStrict call should be resolved away:\n{out}"
        );
        assert!(
            out.contains("Event"),
            "Should resolve to Event:\n{out}"
        );
    }

    #[test]
    fn call_unqualified_strips_scope_receiver() {
        // findPropStrict("rand") + call_method(scope, "rand", [x]) → rand(x)
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64)],
                return_ty: Type::Int(64), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let name = fb.const_string("rand");
            let scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            let result = fb.call_method(scope, "rand", &[x], Type::Int(64));
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("Flash_Scope.findPropStrict"),
            "findPropStrict call should be resolved away:\n{out}"
        );
        assert!(
            out.contains("rand(v0)"),
            "Should emit rand(v0) without scope arg:\n{out}"
        );
    }

    #[test]
    fn call_qualified_strips_scope_receiver() {
        // findPropStrict("flash.net::registerClassAlias") + call_method with qualified name
        // → registerClassAlias("Foo", Foo)
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let name = fb.const_string("flash.net::registerClassAlias");
            let scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            let alias = fb.const_string("Foo");
            let cls = fb.const_string("FooCls");
            fb.call_method(scope, "registerClassAlias", &[alias, cls], Type::Void);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("Flash_Scope.findPropStrict"),
            "findPropStrict call should be resolved away:\n{out}"
        );
        assert!(
            out.contains("registerClassAlias("),
            "Should emit bare registerClassAlias call:\n{out}"
        );
        assert!(
            !out.contains(".registerClassAlias("),
            "Should not emit method dispatch on scope:\n{out}"
        );
    }

    #[test]
    fn call_method_emits_receiver_dot_method() {
        // MethodCall with explicit receiver → receiver.method(args).
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64), Type::Int(64)],
                return_ty: Type::Int(64), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let a = fb.param(0);
            let b = fb.param(1);
            let result = fb.call_method(a, "add", &[b], Type::Int(64));
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("v0.add(v1)"),
            "Should emit receiver.method(args) for MethodCall:\n{out}"
        );
    }

    #[test]
    fn binop_add_strips_scope_operand() {
        // findPropStrict("int") + int(x) → int(x)
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Dynamic, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let name = fb.const_string("int");
            let scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            let int_fn = fb.get_field(scope, "int", Type::Dynamic);
            let casted = fb.call_indirect(int_fn, &[x], Type::Int(64));
            let sum = fb.add(scope, casted);
            fb.ret(Some(sum));
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("findPropStrict"),
            "findPropStrict should not appear in function body:\n{out}"
        );
        // The add should collapse to just the non-scope operand.
        assert!(
            out.contains("return int(v0);"),
            "Should resolve to int(x) without scope operand:\n{out}"
        );
    }

    #[test]
    fn standalone_scope_lookup_not_emitted() {
        // findPropStrict("rand") with no GetField/Call use → no output
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let name = fb.const_string("rand");
            let _scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("findPropStrict"),
            "Standalone scope lookup should not emit findPropStrict:\n{out}"
        );
    }

    #[test]
    fn scope_lookup_call_resolves_to_this_for_inherited_method() {
        // In a method context, an unqualified call with scope-lookup receiver
        // whose name matches a method in the class hierarchy should emit
        // this.method() instead of method().
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Base".into(),
            namespace: vec![],
            fields: vec![],
            visibility: Visibility::Public,
        });
        mb.add_struct(StructDef {
            name: "Child".into(),
            namespace: vec![],
            fields: vec![],
            visibility: Visibility::Public,
        });

        // Base class with isNaga method.
        let base_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("Base::isNaga", base_sig, Visibility::Public);
        fb.set_class(vec![], "Base".into(), MethodKind::Instance);
        let _this = fb.param(0);
        let c = fb.const_bool(false);
        fb.ret(Some(c));
        let base_method_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Base".into(),
            namespace: vec![],
            struct_index: 0,
            methods: vec![base_method_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        // Child class with a method that calls isNaga via scope lookup.
        let child_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("Child::check", child_sig, Visibility::Public);
        fb.set_class(vec![], "Child".into(), MethodKind::Instance);
        let _this = fb.param(0);
        let name = fb.const_string("isNaga");
        let scope =
            fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
        let result = fb.call_method(scope, "isNaga", &[], Type::Bool);
        fb.ret(Some(result));
        let child_method_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Child".into(),
            namespace: vec![],
            struct_index: 1,
            methods: vec![child_method_id],
            super_class: Some("Base".into()),
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("this.isNaga()"),
            "Should emit this.isNaga() for inherited method call:\n{out}"
        );
        assert!(
            !out.contains("findPropStrict"),
            "findPropStrict should be resolved away:\n{out}"
        );
    }

    #[test]
    fn unqualified_callproperty_emits_receiver_dot_method() {
        // MethodCall pattern: call_method(player, "isNaga", []) → player.isNaga()
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Bool, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let player = fb.param(0);
            let result = fb.call_method(player, "isNaga", &[], Type::Bool);
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("v0.isNaga()"),
            "Should emit receiver.method() for MethodCall:\n{out}"
        );
    }

    #[test]
    fn cast_elided_when_source_type_matches() {
        // Cast(v, Bool) where v is already Bool → no "as boolean".
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool],
                return_ty: Type::Bool, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let casted = fb.cast(x, Type::Bool);
            fb.ret(Some(casted));
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("as boolean"),
            "Redundant cast should be elided:\n{out}"
        );
    }

    #[test]
    fn cast_inlined_uses_as_t() {
        // Single-use cast (inlined into return) → needs "as T" wrapper.
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Bool, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let casted = fb.cast(x, Type::Bool);
            fb.ret(Some(casted));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("return v0 as boolean"),
            "Inlined cast should use 'as T':\n{out}"
        );
    }

    #[test]
    fn cast_binding_uses_type_annotation() {
        // Multi-use cast (assigned to variable) → type annotation instead of "as T".
        // Both uses must survive DCE to prevent single-use const folding.
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Bool, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let casted = fb.cast(x, Type::Bool);
            // Use the cast result in a non-dead side-effecting call to keep both uses alive.
            fb.system_call("console", "log", &[casted], Type::Void);
            fb.ret(Some(casted));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains(": boolean = v0"),
            "Multi-use cast should use type annotation:\n{out}"
        );
        assert!(
            !out.contains("as boolean"),
            "Multi-use cast should not use 'as T':\n{out}"
        );
    }

    #[test]
    fn if_else_empty_else_suppressed() {
        // if (cond) { body } else {} → if (cond) { body }
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let cond = fb.param(0);

            let then_block = fb.create_block();
            let else_block = fb.create_block();
            let merge = fb.create_block();

            fb.br_if(cond, then_block, &[], else_block, &[]);

            // then: has a call
            fb.switch_to_block(then_block);
            fb.system_call("renderer", "clear", &[], Type::Void);
            fb.br(merge, &[]);

            // else: empty — just branches to merge
            fb.switch_to_block(else_block);
            fb.br(merge, &[]);

            fb.switch_to_block(merge);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        assert!(
            out.contains("if (v0) {"),
            "Should have if block:\n{out}"
        );
        assert!(
            !out.contains("} else {"),
            "Empty else should be suppressed:\n{out}"
        );
    }

    #[test]
    fn if_else_empty_then_flips_condition() {
        // if (cond) {} else { body } → if (!cond) { body }
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let cond = fb.param(0);

            let then_block = fb.create_block();
            let else_block = fb.create_block();
            let merge = fb.create_block();

            fb.br_if(cond, then_block, &[], else_block, &[]);

            // then: empty
            fb.switch_to_block(then_block);
            fb.br(merge, &[]);

            // else: has a call
            fb.switch_to_block(else_block);
            fb.system_call("renderer", "clear", &[], Type::Void);
            fb.br(merge, &[]);

            fb.switch_to_block(merge);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        assert!(
            out.contains("if (!v0) {"),
            "Should flip condition when then is empty:\n{out}"
        );
        assert!(
            !out.contains("} else {"),
            "Should not have else branch:\n{out}"
        );
    }

    #[test]
    fn if_else_both_empty_omits_entire_if() {
        // if (cond) {} else {} → nothing
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let cond = fb.param(0);

            let then_block = fb.create_block();
            let else_block = fb.create_block();
            let merge = fb.create_block();

            fb.br_if(cond, then_block, &[], else_block, &[]);

            // both empty
            fb.switch_to_block(then_block);
            fb.br(merge, &[]);

            fb.switch_to_block(else_block);
            fb.br(merge, &[]);

            fb.switch_to_block(merge);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("if ("),
            "Both branches empty — entire if should be omitted:\n{out}"
        );
    }

    #[test]
    fn if_else_flip_unwraps_not_instead_of_double_negating() {
        // BrIf on Not(cond) with empty then → should emit if (cond), not if (!(!cond))
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let cond = fb.param(0);
            let not_cond = fb.not(cond);

            let then_block = fb.create_block();
            let else_block = fb.create_block();
            let merge = fb.create_block();

            fb.br_if(not_cond, then_block, &[], else_block, &[]);

            // then: empty
            fb.switch_to_block(then_block);
            fb.br(merge, &[]);

            // else: has a call
            fb.switch_to_block(else_block);
            fb.system_call("renderer", "clear", &[], Type::Void);
            fb.br(merge, &[]);

            fb.switch_to_block(merge);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        assert!(
            out.contains("if (v0) {"),
            "Should unwrap Not and use original condition:\n{out}"
        );
        assert!(
            !out.contains("!(!"),
            "Should not double-negate:\n{out}"
        );
    }

    #[test]
    fn if_else_empty_then_flips_cmp_operator() {
        // BrIf(Cmp(Ge, a, b), then=empty, else=body) → if (a < b) { body }
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64), Type::Int(64)],
                return_ty: Type::Void, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let a = fb.param(0);
            let b = fb.param(1);
            let cmp = fb.cmp(CmpKind::Ge, a, b);

            let then_block = fb.create_block();
            let else_block = fb.create_block();
            let merge = fb.create_block();

            fb.br_if(cmp, then_block, &[], else_block, &[]);

            // then: empty
            fb.switch_to_block(then_block);
            fb.br(merge, &[]);

            // else: has a call
            fb.switch_to_block(else_block);
            fb.system_call("renderer", "clear", &[], Type::Void);
            fb.br(merge, &[]);

            fb.switch_to_block(merge);
            fb.ret(None);

            mb.add_function(fb.build());
        });

        assert!(
            out.contains("if (v0 < v1) {"),
            "Should flip Cmp(Ge) to < when then is empty:\n{out}"
        );
        assert!(
            !out.contains("!("),
            "Should not wrap with !():\n{out}"
        );
    }

    #[test]
    fn cinit_scope_lookup_emits_this_dot_field() {
        // In cinit, findPropStrict that doesn't resolve to an ancestor should
        // still emit `this.field = value` (not bare `field = value`).
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "Settings".into(),
            namespace: vec!["classes".into()],
            fields: vec![],
            visibility: Visibility::Public,
        });

        // cinit: static initializer that sets a static field via scope lookup
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("Settings::cinit", sig, Visibility::Public);
        fb.set_class(vec!["classes".into()], "Settings".into(), MethodKind::Static);
        let _scope_param = fb.param(0);
        let name = fb.const_string("debugBuild");
        let scope =
            fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
        let val = fb.const_bool(true);
        fb.set_field(scope, "debugBuild", val);
        fb.ret(None);
        let cinit_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Settings".into(),
            namespace: vec!["classes".into()],
            struct_index: 0,
            methods: vec![cinit_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![("debugBuild".into(), Type::Bool, None)],
            is_interface: false,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();
        assert!(
            out.contains("this.debugBuild = true"),
            "cinit should emit this.field, not bare field:\n{out}"
        );
        assert!(
            !out.contains("Flash_Scope.findPropStrict"),
            "findPropStrict call should be resolved away:\n{out}"
        );
    }

    #[test]
    fn emit_interface_class() {
        let mut mb = ModuleBuilder::new("test");

        mb.add_struct(StructDef {
            name: "IEventListener".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });

        // Interface constructor (will be skipped).
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("IEventListener::new", sig, Visibility::Public);
        fb.set_class(Vec::new(), "IEventListener".into(), MethodKind::Constructor);
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "IEventListener".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![ctor_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: true,
            interfaces: vec![],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("export abstract class IEventListener {"),
            "Interface should emit abstract class:\n{out}"
        );
        assert!(
            !out.contains("constructor("),
            "Interface should not have constructor:\n{out}"
        );
        assert!(
            !out.contains("registerClassTraits("),
            "Interface should not have registerClassTraits:\n{out}"
        );
        assert!(
            out.contains("registerClass(IEventListener)"),
            "Interface should still have registerClass:\n{out}"
        );
    }

    #[test]
    fn emit_class_with_interfaces() {
        let mut mb = ModuleBuilder::new("test");

        // Interface.
        mb.add_struct(StructDef {
            name: "IClickable".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("IClickable::new", sig.clone(), Visibility::Public);
        fb.set_class(Vec::new(), "IClickable".into(), MethodKind::Constructor);
        fb.ret(None);
        let iface_ctor = mb.add_function(fb.build());
        mb.add_class(ClassDef {
            name: "IClickable".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![iface_ctor],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: true,
            interfaces: vec![],
        });

        // Implementing class.
        mb.add_struct(StructDef {
            name: "Button".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        let mut fb = FunctionBuilder::new("Button::new", sig, Visibility::Public);
        fb.set_class(Vec::new(), "Button".into(), MethodKind::Constructor);
        fb.ret(None);
        let button_ctor = mb.add_function(fb.build());
        mb.add_class(ClassDef {
            name: "Button".into(),
            namespace: Vec::new(),
            struct_index: 1,
            methods: vec![button_ctor],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec!["IClickable".into()],
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default(), None).unwrap();

        assert!(
            out.contains("registerInterface(Button, IClickable)"),
            "Implementing class should have registerInterface:\n{out}"
        );
    }

    #[test]
    fn type_check_struct_uses_is_type() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Bool, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let check = fb.type_check(x, Type::Struct("Monster".into()));
            fb.ret(Some(check));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("isType(v0, Monster)"),
            "TypeCheck with Struct should use isType():\n{out}"
        );
        assert!(
            !out.contains("instanceof"),
            "Should not use instanceof:\n{out}"
        );
    }

    #[test]
    fn cast_struct_uses_as_type() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Struct("Monster".into()), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let casted = fb.cast(x, Type::Struct("Monster".into()));
            fb.ret(Some(casted));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("asType(v0, Monster)"),
            "Cast with Struct should use asType():\n{out}"
        );
        assert!(
            !out.contains("as Monster"),
            "Should not use 'as Monster':\n{out}"
        );
    }

    #[test]
    fn coerce_int_emits_int_call() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Int(32), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let coerced = fb.coerce(x, Type::Int(32));
            fb.ret(Some(coerced));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("int(v0)"),
            "Coerce+Int(32) should emit int():\n{out}"
        );
    }

    #[test]
    fn coerce_float_emits_number_call() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Float(64), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let coerced = fb.coerce(x, Type::Float(64));
            fb.ret(Some(coerced));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("Number(v0)"),
            "Coerce+Float(64) should emit Number():\n{out}"
        );
    }

    #[test]
    fn coerce_uint_emits_uint_call() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::UInt(32), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let coerced = fb.coerce(x, Type::UInt(32));
            fb.ret(Some(coerced));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("uint(v0)"),
            "Coerce+UInt(32) should emit uint():\n{out}"
        );
    }

    #[test]
    fn coerce_string_emits_string_call() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::String, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let coerced = fb.coerce(x, Type::String);
            fb.ret(Some(coerced));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("String(v0)"),
            "Coerce+String should emit String():\n{out}"
        );
    }

    #[test]
    fn coerce_bool_emits_boolean_call() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Bool, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let coerced = fb.coerce(x, Type::Bool);
            fb.ret(Some(coerced));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("Boolean(v0)"),
            "Coerce+Bool should emit Boolean():\n{out}"
        );
    }

    #[test]
    fn coerce_struct_emits_ts_assertion() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Struct("Monster".into()), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let coerced = fb.coerce(x, Type::Struct("Monster".into()));
            fb.ret(Some(coerced));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("as Monster"),
            "Coerce+Struct should emit TS assertion:\n{out}"
        );
        assert!(
            !out.contains("asType"),
            "Coerce+Struct should NOT use asType():\n{out}"
        );
    }

    #[test]
    fn redundant_astype_eliminated() {
        // When value is already typed as the target, Cast should be eliminated.
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Struct("Monster".into())],
                return_ty: Type::Struct("Monster".into()), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            // Cast to same type — should be eliminated by linear lowering.
            let casted = fb.cast(x, Type::Struct("Monster".into()));
            fb.ret(Some(casted));
            mb.add_function(fb.build());
        });

        assert!(
            !out.contains("asType"),
            "Redundant asType should be eliminated:\n{out}"
        );
    }

}
