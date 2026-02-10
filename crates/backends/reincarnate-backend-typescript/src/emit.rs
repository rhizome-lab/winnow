use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Write;
use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{
    structurize, ClassDef, Expr, FuncId, Function, MethodKind, Module, Op, Stmt, StructDef, Type,
    Visibility,
};
use reincarnate_core::pipeline::LoweringConfig;

use crate::runtime::SYSTEM_NAMES;
use crate::types::ts_type;

/// Map a Flash runtime class/value name to its sub-module path under
/// `runtime/flash/`. Returns `None` for names not in the Flash stdlib.
fn flash_stdlib_module(name: &str) -> Option<&'static str> {
    Some(match name {
        // flash/events
        "Event" | "TextEvent" | "ErrorEvent" | "MouseEvent" | "KeyboardEvent"
        | "FocusEvent" | "ProgressEvent" | "IOErrorEvent" | "SecurityErrorEvent"
        | "HTTPStatusEvent" | "AsyncErrorEvent" | "TimerEvent" => "events",
        // flash/display
        "EventDispatcher" | "Graphics" | "DisplayObject" | "InteractiveObject"
        | "DisplayObjectContainer" | "Sprite" | "MovieClip" | "LoaderInfo"
        | "Loader" | "Stage" => "display",
        // flash/geom
        "Point" | "Rectangle" | "Matrix" => "geom",
        // flash/text
        "TextFieldType" | "TextFieldAutoSize" | "TextFormat" | "TextField"
        | "Font" => "text",
        // flash/net
        "URLRequest" | "SharedObject" => "net",
        // flash/utils
        "ByteArray" | "Timer" | "Proxy" => "utils",
        // flash/runtime
        "stage" | "flashTick" => "runtime",
        _ => return None,
    })
}

/// Emit a single module into `output_dir`.
///
/// If the module has classes, emits a directory with one file per class plus
/// a barrel `index.ts`. Otherwise emits a flat `.ts` file.
pub fn emit_module(
    module: &mut Module,
    output_dir: &Path,
    lowering_config: &LoweringConfig,
) -> Result<(), CoreError> {
    if module.classes.is_empty() {
        let out = emit_module_to_string(module, lowering_config)?;
        let path = output_dir.join(format!("{}.ts", module.name));
        fs::write(&path, &out).map_err(CoreError::Io)?;
    } else {
        emit_module_to_dir(module, output_dir, lowering_config)?;
    }
    Ok(())
}

/// Emit a module to a string (flat output — for testing or class-free modules).
pub fn emit_module_to_string(module: &mut Module, lowering_config: &LoweringConfig) -> Result<String, CoreError> {
    let mut out = String::new();
    let class_names = build_class_names(module);
    let class_meta = ClassMeta::build(module);

    emit_runtime_imports(module, &mut out);
    emit_imports(module, &mut out);
    emit_structs(module, &mut out);
    emit_enums(module, &mut out);
    emit_globals(module, &mut out);

    if module.classes.is_empty() {
        emit_functions(module, &class_names, lowering_config, &mut out)?;
    } else {
        let (class_groups, free_funcs) = group_by_class(module);
        for group in &class_groups {
            emit_class(group, module, &class_names, &class_meta, lowering_config, &mut out)?;
        }
        for &fid in &free_funcs {
            emit_function(&mut module.functions[fid], &class_names, lowering_config, &mut out)?;
        }
    }

    Ok(out)
}

// ---------------------------------------------------------------------------
// ClassRegistry — maps qualified names to filesystem paths for imports
// ---------------------------------------------------------------------------

struct ClassEntry {
    short_name: String,
    /// Path segments from module root, e.g. ["classes", "Scenes", "Swamp", "Swamp"].
    path_segments: Vec<String>,
}

struct ClassRegistry {
    /// Keyed by both qualified name and bare name (fallback).
    classes: HashMap<String, ClassEntry>,
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

    fn lookup(&self, name: &str) -> Option<&ClassEntry> {
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
}

impl ClassMeta {
    fn build(module: &Module) -> Self {
        Self {
            ancestor_sets: build_ancestor_sets(module),
            method_name_sets: build_method_name_sets(module),
            instance_field_sets: build_instance_field_sets(module),
        }
    }
}

/// Build a map from qualified class name to the set of ancestor short names.
///
/// For each class, the set includes the class's own short name and the short
/// names of all superclasses reachable via `super_class` links within the module.
fn build_ancestor_sets(module: &Module) -> HashMap<String, HashSet<String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut ancestors = HashSet::new();
        ancestors.insert(class.name.clone());
        let mut current = class;
        while let Some(ref sc) = current.super_class {
            let short = sc.rsplit("::").next().unwrap_or(sc);
            ancestors.insert(short.to_string());
            match class_by_short.get(short) {
                Some(parent) => current = parent,
                None => break,
            }
        }
        result.insert(qualified_class_name(class), ancestors);
    }
    result
}

/// Build a mapping from qualified class name → set of all method short names
/// visible through the class hierarchy (own methods + all ancestor methods).
fn build_method_name_sets(module: &Module) -> HashMap<String, HashSet<String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut names = HashSet::new();
        let mut current = class;
        loop {
            for &fid in &current.methods {
                if let Some(f) = module.functions.get(fid) {
                    if let Some(short) = f.name.rsplit("::").next() {
                        names.insert(short.to_string());
                    }
                }
            }
            match current.super_class {
                Some(ref sc) => {
                    let short = sc.rsplit("::").next().unwrap_or(sc);
                    match class_by_short.get(short) {
                        Some(parent) => current = parent,
                        None => break,
                    }
                }
                None => break,
            }
        }
        result.insert(qualified_class_name(class), names);
    }
    result
}

/// Build a mapping from qualified class name → set of all instance field short
/// names visible through the class hierarchy (own fields + all ancestor fields).
fn build_instance_field_sets(module: &Module) -> HashMap<String, HashSet<String>> {
    let class_by_short: HashMap<&str, &ClassDef> =
        module.classes.iter().map(|c| (c.name.as_str(), c)).collect();

    let mut result = HashMap::new();
    for class in &module.classes {
        let mut fields = HashSet::new();
        let mut current = class;
        loop {
            let struct_def = &module.structs[current.struct_index];
            for (name, _ty) in &struct_def.fields {
                fields.insert(name.clone());
            }
            match current.super_class {
                Some(ref sc) => {
                    let short = sc.rsplit("::").next().unwrap_or(sc);
                    match class_by_short.get(short) {
                        Some(parent) => current = parent,
                        None => break,
                    }
                }
                None => break,
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
pub fn emit_module_to_dir(module: &mut Module, output_dir: &Path, lowering_config: &LoweringConfig) -> Result<(), CoreError> {
    let module_dir = output_dir.join(&module.name);
    fs::create_dir_all(&module_dir).map_err(CoreError::Io)?;

    let (class_groups, free_funcs) = group_by_class(module);
    let registry = ClassRegistry::from_module(module);
    let class_names = build_class_names(module);
    let class_meta = ClassMeta::build(module);
    let mut barrel_exports: Vec<String> = Vec::new();

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
        let systems = collect_system_names_from_funcs(
            group.methods.iter().map(|&fid| &module.functions[fid]),
        );
        emit_runtime_imports_for(systems, &mut out, depth);
        let prefix = "../".repeat(depth + 1);
        let prefix = prefix.trim_end_matches('/');
        let _ = writeln!(out, "import {{ QN_KEY }} from \"{prefix}/runtime/flash/utils\";\n");
        emit_intra_imports(group, module, &segments, &registry, depth, &mut out);
        emit_class(group, module, &class_names, &class_meta, lowering_config, &mut out)?;

        let path = file_dir.join(format!("{short_name}.ts"));
        fs::write(&path, &out).map_err(CoreError::Io)?;

        // Barrel export path: relative from module_dir.
        let export_path = segments.join("/");
        barrel_exports.push(export_path);
    }

    // Free functions → _init.ts (at module root, depth 0).
    if !free_funcs.is_empty() {
        let mut out = String::new();
        let systems = collect_system_names_from_funcs(
            free_funcs.iter().map(|&fid| &module.functions[fid]),
        );
        emit_runtime_imports_for(systems, &mut out, 0);

        // Scan free functions for Flash stdlib class references.
        let mut ext_value_refs = BTreeSet::new();
        let mut ext_type_refs = BTreeSet::new();
        let mut _intra_val = BTreeSet::new();
        let mut _intra_ty = BTreeSet::new();
        for &fid in &free_funcs {
            let func = &module.functions[fid];
            collect_type_refs_from_function(
                func, "", &registry,
                &mut _intra_val, &mut _intra_ty,
                &mut ext_value_refs, &mut ext_type_refs,
            );
        }
        emit_flash_stdlib_imports(&ext_value_refs, &ext_type_refs, "..", &mut out);

        emit_imports(module, &mut out);
        emit_globals(module, &mut out);
        for &fid in &free_funcs {
            emit_function(&mut module.functions[fid], &class_names, lowering_config, &mut out)?;
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
fn emit_runtime_imports(module: &Module, out: &mut String) {
    let systems =
        collect_system_names_from_funcs(module.functions.iter().map(|(_id, f)| f));
    emit_runtime_imports_with_prefix(systems, out, ".");
}

/// Emit runtime imports for files inside a module directory.
///
/// `depth` is the number of namespace directories below the module dir. The
/// module dir itself is one level inside `output_dir`, so the prefix traverses
/// `depth + 1` parent directories.
fn emit_runtime_imports_for(systems: BTreeSet<String>, out: &mut String, depth: usize) {
    let prefix = "../".repeat(depth + 1);
    let prefix = prefix.trim_end_matches('/');
    emit_runtime_imports_with_prefix(systems, out, prefix);
}

/// Map a Flash system name (e.g. `"Flash.Object"`) to its sub-module path
/// under `runtime/flash/`. Returns `None` for non-Flash systems.
fn flash_system_module(system: &str) -> Option<&'static str> {
    Some(match system {
        "Flash.Object" => "object",
        "Flash.Class" => "class",
        "Flash.Scope" => "scope",
        "Flash.Exception" => "exception",
        "Flash.Iterator" => "iterator",
        "Flash.Memory" => "memory",
        "Flash.XML" => "xml",
        "Flash.Utils" => "utils",
        _ => return None,
    })
}

fn emit_runtime_imports_with_prefix(
    systems: BTreeSet<String>,
    out: &mut String,
    prefix: &str,
) {
    if systems.is_empty() {
        return;
    }
    let known: BTreeSet<&str> = SYSTEM_NAMES.iter().copied().collect();
    let mut generic: Vec<&str> = Vec::new();
    let mut flash_by_mod: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for sys in &systems {
        if known.contains(sys.as_str()) {
            generic.push(sys.as_str());
        } else {
            let module = flash_system_module(sys)
                .map(|s| s.to_string())
                .unwrap_or_else(|| {
                    sys.strip_prefix("Flash.")
                        .unwrap_or(sys)
                        .to_ascii_lowercase()
                });
            flash_by_mod
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
    for (module, names) in &flash_by_mod {
        let _ = writeln!(
            out,
            "import {{ {} }} from \"{prefix}/runtime/flash/{module}\";",
            names.join(", ")
        );
    }
    if !generic.is_empty() || !flash_by_mod.is_empty() {
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Intra-module imports (class-to-class references)
// ---------------------------------------------------------------------------

/// Collect type names referenced by a class group, split into value and type-only refs.
///
/// **Value refs** (class constructor needed at runtime):
/// - `super_class` — `extends X` is a runtime expression
/// - `Op::TypeCheck` — emits `instanceof X`
///
/// **Type refs** (erased at runtime):
/// - Struct field types, function signatures, `Op::Alloc`, `Op::Cast`, `value_types`
fn collect_class_references(
    group: &ClassGroup,
    module: &Module,
    registry: &ClassRegistry,
) -> (BTreeSet<String>, BTreeSet<String>, BTreeSet<String>, BTreeSet<String>) {
    let self_name = &group.class_def.name;
    let mut value_refs = BTreeSet::new();
    let mut type_refs = BTreeSet::new();
    let mut ext_value_refs = BTreeSet::new();
    let mut ext_type_refs = BTreeSet::new();

    // Super class reference — runtime value (extends).
    if let Some(sc) = &group.class_def.super_class {
        let short = sc.rsplit("::").next().unwrap_or(sc);
        if short != self_name {
            if let Some(entry) = registry.lookup(sc) {
                value_refs.insert(entry.short_name.clone());
            } else if flash_stdlib_module(short).is_some() {
                ext_value_refs.insert(short.to_string());
            }
        }
    }

    // Struct fields (class instance fields) — type-only.
    for (_name, ty) in &group.struct_def.fields {
        collect_type_ref(ty, self_name, registry, &mut type_refs, &mut ext_type_refs);
    }

    // Scan all method bodies for type references.
    for &fid in &group.methods {
        let func = &module.functions[fid];
        collect_type_refs_from_function(func, self_name, registry, &mut value_refs, &mut type_refs, &mut ext_value_refs, &mut ext_type_refs);
    }

    (value_refs, type_refs, ext_value_refs, ext_type_refs)
}

/// Scan a function's instructions and signature for type references.
fn collect_type_refs_from_function(
    func: &Function,
    self_name: &str,
    registry: &ClassRegistry,
    value_refs: &mut BTreeSet<String>,
    type_refs: &mut BTreeSet<String>,
    ext_value_refs: &mut BTreeSet<String>,
    ext_type_refs: &mut BTreeSet<String>,
) {
    // Return type and param types — type-only.
    collect_type_ref(&func.sig.return_ty, self_name, registry, type_refs, ext_type_refs);
    for ty in &func.sig.params {
        collect_type_ref(ty, self_name, registry, type_refs, ext_type_refs);
    }

    // Instructions.
    for (_inst_id, inst) in func.insts.iter() {
        match &inst.op {
            // TypeCheck emits `instanceof` — runtime value reference.
            Op::TypeCheck(_, ty) => {
                collect_type_ref(ty, self_name, registry, value_refs, ext_value_refs);
            }
            // Alloc and Cast are type assertions only — type-only.
            Op::Alloc(ty) | Op::Cast(_, ty) => {
                collect_type_ref(ty, self_name, registry, type_refs, ext_type_refs);
            }
            // GetField with a class name → runtime value reference (used with `new`).
            Op::GetField { field, .. } => {
                if registry.lookup(field).is_some() {
                    collect_type_ref(
                        &Type::Struct(field.clone()),
                        self_name,
                        registry,
                        value_refs,
                        ext_value_refs,
                    );
                } else {
                    // Check Flash stdlib (e.g. "flash.display::MovieClip").
                    let short = field.rsplit("::").next().unwrap_or(field);
                    if short != self_name && flash_stdlib_module(short).is_some() {
                        ext_value_refs.insert(short.to_string());
                    }
                }
            }
            _ => {}
        }
    }

    // value_types — type-only.
    for (_vid, ty) in func.value_types.iter() {
        collect_type_ref(ty, self_name, registry, type_refs, ext_type_refs);
    }
}

/// If a type references a class in the registry, add its short name.
/// If not in the registry but in the Flash stdlib, add to `ext_refs`.
fn collect_type_ref(
    ty: &Type,
    self_name: &str,
    registry: &ClassRegistry,
    refs: &mut BTreeSet<String>,
    ext_refs: &mut BTreeSet<String>,
) {
    match ty {
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            if short != self_name {
                if let Some(entry) = registry.lookup(name) {
                    refs.insert(entry.short_name.clone());
                } else if flash_stdlib_module(short).is_some() {
                    ext_refs.insert(short.to_string());
                }
            }
        }
        Type::Array(inner) | Type::Option(inner) => {
            collect_type_ref(inner, self_name, registry, refs, ext_refs);
        }
        Type::Map(k, v) => {
            collect_type_ref(k, self_name, registry, refs, ext_refs);
            collect_type_ref(v, self_name, registry, refs, ext_refs);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                collect_type_ref(elem, self_name, registry, refs, ext_refs);
            }
        }
        Type::Function(sig) => {
            collect_type_ref(&sig.return_ty, self_name, registry, refs, ext_refs);
            for p in &sig.params {
                collect_type_ref(p, self_name, registry, refs, ext_refs);
            }
        }
        Type::Coroutine {
            yield_ty,
            return_ty,
        } => {
            collect_type_ref(yield_ty, self_name, registry, refs, ext_refs);
            collect_type_ref(return_ty, self_name, registry, refs, ext_refs);
        }
        Type::Union(types) => {
            for t in types {
                collect_type_ref(t, self_name, registry, refs, ext_refs);
            }
        }
        _ => {}
    }
}

/// Emit grouped `import` / `import type` statements for Flash stdlib references.
fn emit_flash_stdlib_imports(
    ext_value_refs: &BTreeSet<String>,
    ext_type_refs: &BTreeSet<String>,
    prefix: &str,
    out: &mut String,
) {
    if ext_value_refs.is_empty() && ext_type_refs.is_empty() {
        return;
    }
    // Group value refs by module.
    let mut val_by_mod: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for name in ext_value_refs {
        if let Some(m) = flash_stdlib_module(name) {
            val_by_mod.entry(m).or_default().push(name);
        }
    }
    for (module, names) in &val_by_mod {
        let _ = writeln!(
            out,
            "import {{ {} }} from \"{prefix}/runtime/flash/{module}\";",
            names.join(", ")
        );
    }
    // Type-only imports (not already covered by value imports).
    let mut type_by_mod: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for name in ext_type_refs {
        if !ext_value_refs.contains(name) {
            if let Some(m) = flash_stdlib_module(name) {
                type_by_mod.entry(m).or_default().push(name);
            }
        }
    }
    for (module, names) in &type_by_mod {
        let _ = writeln!(
            out,
            "import type {{ {} }} from \"{prefix}/runtime/flash/{module}\";",
            names.join(", ")
        );
    }
}

/// Emit `import` / `import type` statements for intra-module class references.
fn emit_intra_imports(
    group: &ClassGroup,
    module: &Module,
    source_segments: &[String],
    registry: &ClassRegistry,
    depth: usize,
    out: &mut String,
) {
    let (value_refs, type_refs, ext_value_refs, ext_type_refs) =
        collect_class_references(group, module, registry);
    let has_intra = !value_refs.is_empty() || !type_refs.is_empty();
    let has_ext = !ext_value_refs.is_empty() || !ext_type_refs.is_empty();
    if !has_intra && !has_ext {
        return;
    }

    // External Flash stdlib imports — grouped by sub-module.
    if has_ext {
        let prefix = "../".repeat(depth + 1);
        let prefix = prefix.trim_end_matches('/');
        emit_flash_stdlib_imports(&ext_value_refs, &ext_type_refs, prefix, out);
    }

    // Intra-module value imports.
    for short_name in &value_refs {
        if let Some(entry) = registry.classes.get(short_name) {
            let rel = relative_import_path(source_segments, &entry.path_segments);
            let _ = writeln!(out, "import {{ {short_name} }} from \"{rel}\";");
        }
    }
    // Intra-module type-only imports (names not already in value_refs).
    for short_name in &type_refs {
        if value_refs.contains(short_name) {
            continue;
        }
        if let Some(entry) = registry.classes.get(short_name) {
            let rel = relative_import_path(source_segments, &entry.path_segments);
            let _ = writeln!(out, "import type {{ {short_name} }} from \"{rel}\";");
        }
    }
    out.push('\n');
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
    for (name, ty) in &def.fields {
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
        let kw = if global.mutable { "let" } else { "const" };
        let _ = writeln!(
            out,
            "{vis}{kw} {}: {};",
            sanitize_ident(&global.name),
            ts_type(&global.ty)
        );
    }
    if !module.globals.is_empty() {
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Functions
// ---------------------------------------------------------------------------

fn emit_functions(
    module: &mut Module,
    class_names: &HashMap<String, String>,
    lowering_config: &LoweringConfig,
    out: &mut String,
) -> Result<(), CoreError> {
    for id in module.functions.keys().collect::<Vec<_>>() {
        emit_function(&mut module.functions[id], class_names, lowering_config, out)?;
    }
    Ok(())
}

fn emit_function(
    func: &mut Function,
    class_names: &HashMap<String, String>,
    lowering_config: &LoweringConfig,
    out: &mut String,
) -> Result<(), CoreError> {
    use crate::ast_printer::{self, PrintCtx};
    use reincarnate_core::ir::linear;

    let shape = structurize::structurize(func);
    let ast = linear::lower_function_linear(func, &shape, lowering_config);
    let pctx = PrintCtx::for_function(class_names);
    ast_printer::print_function(&ast, &pctx, out);
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

/// Emit a TypeScript class from a `ClassGroup`.
fn emit_class(
    group: &ClassGroup,
    module: &mut Module,
    class_names: &HashMap<String, String>,
    class_meta: &ClassMeta,
    lowering_config: &LoweringConfig,
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

    let _ = writeln!(out, "{vis}class {class_name}{extends} {{");
    let qualified = qualified_class_name(&group.class_def);
    let _ = writeln!(out, "  static [QN_KEY] = \"{qualified}\";");

    // Fields from struct def.
    for (name, ty) in &group.struct_def.fields {
        let _ = writeln!(out, "  {}: {};", sanitize_ident(name), ts_type(ty));
    }
    if !group.struct_def.fields.is_empty() && !group.methods.is_empty() {
        out.push('\n');
    }

    // Methods — sorted: constructor first, then instance, static, getters, setters.
    let mut sorted_methods: Vec<FuncId> = group.methods.clone();
    sorted_methods.sort_by_key(|&fid| match module.functions[fid].method_kind {
        MethodKind::Constructor => 0,
        MethodKind::Instance => 1,
        MethodKind::Getter => 2,
        MethodKind::Setter => 3,
        MethodKind::Static => 4,
        MethodKind::Free => 5,
    });

    let empty = HashSet::new();
    let ancestors = class_meta.ancestor_sets.get(&qualified).unwrap_or(&empty);
    let method_names = class_meta.method_name_sets.get(&qualified).unwrap_or(&empty);
    let instance_fields = class_meta.instance_field_sets.get(&qualified).unwrap_or(&empty);

    let suppress_super = extends.is_empty();
    for (i, &fid) in sorted_methods.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        emit_class_method(&mut module.functions[fid], class_names, ancestors, method_names, instance_fields, suppress_super, lowering_config, out)?;
    }

    let _ = writeln!(out, "}}\n");
    Ok(())
}

/// In JS class constructors, `super()` must precede any `this` access.
/// AVM2 places constructSuper after field inits — hoist it to position 0.
fn hoist_super_call(body: &mut Vec<Stmt>) {
    let pos = body.iter().position(|s| {
        matches!(
            s,
            Stmt::Expr(Expr::SystemCall { system, method, .. })
            if system == "Flash.Class" && method == "constructSuper"
        )
    });
    if let Some(i) = pos {
        if i > 0 {
            let stmt = body.remove(i);
            body.insert(0, stmt);
        }
    }
}

/// Emit a single method inside a class body.
fn emit_class_method(
    func: &mut Function,
    class_names: &HashMap<String, String>,
    ancestors: &HashSet<String>,
    method_names: &HashSet<String>,
    instance_fields: &HashSet<String>,
    suppress_super: bool,
    lowering_config: &LoweringConfig,
    out: &mut String,
) -> Result<(), CoreError> {
    #![allow(clippy::too_many_arguments)]
    use crate::ast_printer::{self, PrintCtx};
    use reincarnate_core::ir::linear;

    let raw_name = func
        .name
        .rsplit("::")
        .next()
        .unwrap_or(&func.name)
        .to_string();

    let skip_self = matches!(
        func.method_kind,
        MethodKind::Constructor | MethodKind::Instance | MethodKind::Getter | MethodKind::Setter
    );

    let shape = structurize::structurize(func);
    let mut ast = linear::lower_function_linear(func, &shape, lowering_config);
    if func.method_kind == MethodKind::Constructor {
        hoist_super_call(&mut ast.body);
    }
    let is_cinit = raw_name == "cinit" && func.method_kind == MethodKind::Static;
    let mut pctx = if skip_self || is_cinit {
        PrintCtx::for_method(class_names, ancestors, method_names, instance_fields)
    } else {
        PrintCtx::for_function(class_names)
    };
    pctx.suppress_super = suppress_super;
    ast_printer::print_class_method(&ast, &raw_name, skip_self, &pctx, out);
    Ok(())
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
        emit_module_to_string(&mut mb.build(), &LoweringConfig::default()).unwrap()
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
                    ("x".into(), Type::Float(64)),
                    ("y".into(), Type::Float(64)),
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
            });
            mb.add_global(Global {
                name: "MAX_SIZE".into(),
                ty: Type::Int(64),
                visibility: Visibility::Private,
                mutable: false,
            });
        });

        assert!(out.contains("export let counter: number;"));
        assert!(out.contains("const MAX_SIZE: number;"));
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
        let out = emit_module_to_string(&mut result.module, &LoweringConfig::default()).unwrap();

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
            fields: vec![("hp".into(), Type::Int(32))],
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

        // Static method: (amount: i32) -> i32
        let static_sig = FunctionSig {
            params: vec![Type::Int(32)],
            return_ty: Type::Int(32), ..Default::default() };
        let mut fb = FunctionBuilder::new("Fighter::create", static_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Fighter".into(),
            MethodKind::Static,
        );
        let p = fb.param(0);
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
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
        // Static method — keeps all params.
        assert!(
            out.contains("  static create(v0: number): number {"),
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
        });

        // Free function.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);
        fb.ret(None);
        mb.add_function(fb.build());

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
            fields: vec![("hp".into(), Type::Int(32))],
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
        });

        let mut module = mb.build();
        emit_module_to_dir(&mut module, dir.path(), &LoweringConfig::default()).unwrap();

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
            fields: vec![("boss".into(), Type::Struct("classes::Monster".into()))],
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
        });
        mb.add_class(ClassDef {
            name: "Swamp".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            struct_index: 1,
            methods: vec![swamp_ctor],
            super_class: Some("classes::Monster".into()),
            visibility: Visibility::Public,
        });

        let mut module = mb.build();
        emit_module_to_dir(&mut module, dir.path(), &LoweringConfig::default()).unwrap();

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
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
        });
        mb.add_class(ClassDef {
            name: "Widget".into(),
            namespace: Vec::new(),
            struct_index: 1,
            methods: vec![widget_ctor_id],
            super_class: None,
            visibility: Visibility::Public,
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
            // Qualified call: receiver.outputText("text", true)
            let result = fb.call(
                "classes:BaseContent::outputText",
                &[receiver, arg1, arg2],
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
            fields: vec![("hp".into(), Type::Int(32))],
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
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
            fields: vec![("player".into(), Type::Dynamic)],
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
        });
        mb.add_class(ClassDef {
            name: "Child".into(),
            namespace: vec!["classes".into()],
            struct_index: 1,
            methods: vec![method_id],
            super_class: Some("classes::Base".into()),
            visibility: Visibility::Public,
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
            fields: vec![("power".into(), Type::Int(32))],
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
        });
        mb.add_class(ClassDef {
            name: "Villain".into(),
            namespace: vec!["classes".into()],
            struct_index: 1,
            methods: vec![],
            super_class: None,
            visibility: Visibility::Public,
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
            fields: vec![("temp".into(), Type::Dynamic)],
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
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
        // findPropStrict("rand") + call("rand", [scope, x]) → rand(x)
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64)],
                return_ty: Type::Int(64), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let name = fb.const_string("rand");
            let scope =
                fb.system_call("Flash.Scope", "findPropStrict", &[name], Type::Dynamic);
            let result = fb.call("rand", &[scope, x], Type::Int(64));
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
        // findPropStrict("flash.net::registerClassAlias") + call with qualified name
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
            fb.call("flash.net::registerClassAlias", &[scope, alias, cls], Type::Void);
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
    fn call_unqualified_non_scope_emits_method_dispatch() {
        // Unqualified call without scope lookup → receiver.method(rest) pattern
        // (matches AVM2 callproperty semantics where args[0] is the receiver).
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64), Type::Int(64)],
                return_ty: Type::Int(64), ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let a = fb.param(0);
            let b = fb.param(1);
            let result = fb.call("add", &[a, b], Type::Int(64));
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("v0.add(v1)"),
            "Should emit receiver.method(rest) for non-scope unqualified call:\n{out}"
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
        let result = fb.call("isNaga", &[scope], Type::Bool);
        fb.ret(Some(result));
        let child_method_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Child".into(),
            namespace: vec![],
            struct_index: 1,
            methods: vec![child_method_id],
            super_class: Some("Base".into()),
            visibility: Visibility::Public,
        });

        let mut module = mb.build();
        let out = emit_module_to_string(&mut module, &LoweringConfig::default()).unwrap();

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
        // AVM2 callproperty pattern: call "isNaga"(player) → player.isNaga()
        // No findPropStrict — args[0] is the real receiver.
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Bool, ..Default::default() };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let player = fb.param(0);
            let result = fb.call("isNaga", &[player], Type::Bool);
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        assert!(
            out.contains("v0.isNaga()"),
            "Should emit receiver.method() for callproperty pattern:\n{out}"
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

}
