use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Write;
use std::fs;
use std::path::Path;

use reincarnate_core::entity::EntityRef;
use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{
    structurize, Block, BlockArgAssign, BlockId, ClassDef, CmpKind, Constant, FuncId, Function,
    InstId, MethodKind, Module, Op, Shape, StructDef, Type, ValueId, Visibility,
};

use reincarnate_core::transforms::util::value_operands;

use crate::runtime::SYSTEM_NAMES;
use crate::types::ts_type;

/// Emit a single module into `output_dir`.
///
/// If the module has classes, emits a directory with one file per class plus
/// a barrel `index.ts`. Otherwise emits a flat `.ts` file.
pub fn emit_module(module: &Module, output_dir: &Path) -> Result<(), CoreError> {
    if module.classes.is_empty() {
        let out = emit_module_to_string(module)?;
        let path = output_dir.join(format!("{}.ts", module.name));
        fs::write(&path, &out).map_err(CoreError::Io)?;
    } else {
        emit_module_to_dir(module, output_dir)?;
    }
    Ok(())
}

/// Emit a module to a string (flat output — for testing or class-free modules).
pub fn emit_module_to_string(module: &Module) -> Result<String, CoreError> {
    let mut out = String::new();
    let class_names = build_class_names(module);
    let ancestor_sets = build_ancestor_sets(module);
    let method_name_sets = build_method_name_sets(module);

    emit_runtime_imports(module, &mut out);
    emit_imports(module, &mut out);
    emit_structs(module, &mut out);
    emit_enums(module, &mut out);
    emit_globals(module, &mut out);

    if module.classes.is_empty() {
        emit_functions(module, &class_names, &mut out)?;
    } else {
        let (class_groups, free_funcs) = group_by_class(module);
        for group in &class_groups {
            emit_class(group, module, &class_names, &ancestor_sets, &method_name_sets, &mut out)?;
        }
        for (_fid, func) in &free_funcs {
            emit_function(func, &class_names, &mut out)?;
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
pub fn emit_module_to_dir(module: &Module, output_dir: &Path) -> Result<(), CoreError> {
    let module_dir = output_dir.join(&module.name);
    fs::create_dir_all(&module_dir).map_err(CoreError::Io)?;

    let (class_groups, free_funcs) = group_by_class(module);
    let registry = ClassRegistry::from_module(module);
    let class_names = build_class_names(module);
    let ancestor_sets = build_ancestor_sets(module);
    let method_name_sets = build_method_name_sets(module);
    let mut barrel_exports: Vec<String> = Vec::new();

    for group in &class_groups {
        let class_def = group.class_def;
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
            group.methods.iter().map(|(_id, f)| *f),
        );
        emit_runtime_imports_for(systems, &mut out, depth);
        emit_intra_imports(group, &segments, &registry, &mut out);
        emit_class(group, module, &class_names, &ancestor_sets, &method_name_sets, &mut out)?;

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
            free_funcs.iter().map(|(_id, f)| *f),
        );
        emit_runtime_imports_for(systems, &mut out, 0);
        emit_imports(module, &mut out);
        emit_globals(module, &mut out);
        for (_fid, func) in &free_funcs {
            emit_function(func, &class_names, &mut out)?;
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
// Emit context — inline-let tracking
// ---------------------------------------------------------------------------

/// Per-function context for inline `let` declarations and expression inlining.
///
/// Copy propagation and alias resolution are handled by the `Mem2Reg` IR
/// transform pass. The emitter tracks which values get a `let` keyword at
/// their definition site and which single-use values can be inlined into
/// their sole consumer.
struct EmitCtx {
    /// Values that need a `let` keyword when first emitted.
    needs_let: HashSet<ValueId>,
    /// The `this` parameter for instance methods (emitted as `this` instead of `v0`).
    self_value: Option<ValueId>,
    /// Number of times each value is used as an operand.
    use_counts: HashMap<ValueId, usize>,
    /// Inlined expressions for single-use values: (expr, needs_parens).
    inline_exprs: HashMap<ValueId, (String, bool)>,
    /// Qualified class name → sanitized short name (e.g. "classes.Scenes::Foo" → "Foo").
    class_names: HashMap<String, String>,
    /// Values produced by scope lookups (`findPropStrict`/`findProperty`) — resolved to bare
    /// names in `GetField`/`SetField`.
    scope_lookups: HashSet<ValueId>,
    /// Short names of the current class and all its ancestors in the hierarchy.
    ancestors: HashSet<String>,
    /// Method short names visible in the class hierarchy (for resolving inherited calls).
    method_names: HashSet<String>,
    /// Debug names for values (from source-level variable/parameter names).
    value_names: HashMap<ValueId, String>,
}

impl EmitCtx {
    fn for_function(func: &Function, class_names: &HashMap<String, String>) -> Self {
        let needs_let = compute_needs_let(func);
        let use_counts = compute_use_counts(func);
        let value_names: HashMap<ValueId, String> = func
            .value_names
            .iter()
            .map(|(k, v)| (*k, sanitize_ident(v)))
            .collect();
        Self {
            needs_let,
            self_value: None,
            use_counts,
            inline_exprs: HashMap::new(),
            class_names: class_names.clone(),
            scope_lookups: HashSet::new(),
            ancestors: HashSet::new(),
            method_names: HashSet::new(),
            value_names,
        }
    }

    fn for_method(
        func: &Function,
        self_value: ValueId,
        class_names: &HashMap<String, String>,
        ancestors: &HashSet<String>,
        method_names: &HashSet<String>,
    ) -> Self {
        let needs_let = compute_needs_let(func);
        let use_counts = compute_use_counts(func);
        let value_names: HashMap<ValueId, String> = func
            .value_names
            .iter()
            .map(|(k, v)| (*k, sanitize_ident(v)))
            .collect();
        Self {
            needs_let,
            self_value: Some(self_value),
            use_counts,
            inline_exprs: HashMap::new(),
            class_names: class_names.clone(),
            scope_lookups: HashSet::new(),
            ancestors: ancestors.clone(),
            method_names: method_names.clone(),
            value_names,
        }
    }

    /// Whether a value should be inlined rather than assigned to a variable.
    fn should_inline(&self, v: ValueId) -> bool {
        self.use_counts.get(&v).copied().unwrap_or(0) == 1
    }

    /// Store an expression for a value to be inlined at its use site.
    fn store_inline(&mut self, v: ValueId, expr: String, needs_parens: bool) {
        self.inline_exprs.insert(v, (expr, needs_parens));
    }

    /// Format a value reference, checking for inlined expressions.
    fn val(&self, v: ValueId) -> String {
        if self.self_value == Some(v) {
            "this".into()
        } else if let Some((expr, _)) = self.inline_exprs.get(&v) {
            expr.clone()
        } else if let Some(name) = self.value_names.get(&v) {
            name.clone()
        } else {
            format!("v{}", v.index())
        }
    }

    /// Format a value reference for use as an operand where precedence matters.
    ///
    /// If the value is an inlined expression that needs parenthesization,
    /// wraps it in `(...)`.
    fn operand(&self, v: ValueId) -> String {
        if self.self_value == Some(v) {
            "this".into()
        } else if let Some((expr, needs_parens)) = self.inline_exprs.get(&v) {
            if *needs_parens {
                format!("({expr})")
            } else {
                expr.clone()
            }
        } else if let Some(name) = self.value_names.get(&v) {
            name.clone()
        } else {
            format!("v{}", v.index())
        }
    }

    /// Format a value name for the LHS of assignments (no inline lookup).
    fn val_name(&self, v: ValueId) -> String {
        if self.self_value == Some(v) {
            "this".into()
        } else if let Some(name) = self.value_names.get(&v) {
            name.clone()
        } else {
            format!("v{}", v.index())
        }
    }

    /// Returns `"const "` for instruction results, `""` for parameters and self.
    fn let_prefix(&self, v: ValueId) -> &'static str {
        if self.self_value == Some(v) {
            ""
        } else if self.needs_let.contains(&v) {
            "const "
        } else {
            ""
        }
    }
}

/// Determine which values should get an inline `let` declaration.
///
/// Entry-block parameters are declared in the function signature. Non-entry
/// block parameters are pre-declared before the dispatch loop. Everything else
/// (instruction results) gets `let` at its definition site.
fn compute_needs_let(func: &Function) -> HashSet<ValueId> {
    let entry_params: HashSet<ValueId> =
        func.blocks[func.entry].params.iter().map(|p| p.value).collect();

    let mut needs_let = HashSet::new();
    for (_inst_id, inst) in func.insts.iter() {
        if let Some(r) = inst.result {
            if !entry_params.contains(&r) {
                needs_let.insert(r);
            }
        }
    }
    needs_let
}

/// Count how many times each value is used as an operand across live instructions.
///
/// Only counts instructions that are actually in blocks — orphaned instructions
/// left in the arena by transforms (e.g. dead Copies from Mem2Reg) are excluded.
fn compute_use_counts(func: &Function) -> HashMap<ValueId, usize> {
    let mut counts = HashMap::new();
    for (_block_id, block) in func.blocks.iter() {
        for &inst_id in &block.insts {
            for v in value_operands(&func.insts[inst_id].op) {
                *counts.entry(v).or_insert(0) += 1;
            }
        }
    }
    counts
}

/// Adjust use counts for LogicalOr/LogicalAnd shapes.
///
/// In the IR, a BrIf uses `cond` twice (as condition + branch arg).
/// After folding into `cond || rhs` or `cond && rhs`, `cond` appears
/// only once. Decrement by 1 so the inline system can fold single-use
/// values through the `||`/`&&` expression.
///
/// Only adjusts when rhs_body will emit empty (clean `||`/`&&` path).
fn adjust_use_counts_for_logical_ops(ctx: &mut EmitCtx, func: &Function, shape: &Shape) {
    match shape {
        Shape::LogicalOr {
            cond, rhs_body, ..
        }
        | Shape::LogicalAnd {
            cond, rhs_body, ..
        } => {
            if logical_rhs_body_emits_empty(ctx, func, rhs_body) {
                if let Some(count) = ctx.use_counts.get_mut(cond) {
                    *count = count.saturating_sub(1);
                }
            }
            adjust_use_counts_for_logical_ops(ctx, func, rhs_body);
        }
        Shape::Seq(parts) => {
            for part in parts {
                adjust_use_counts_for_logical_ops(ctx, func, part);
            }
        }
        Shape::IfElse {
            then_body,
            else_body,
            ..
        } => {
            adjust_use_counts_for_logical_ops(ctx, func, then_body);
            adjust_use_counts_for_logical_ops(ctx, func, else_body);
        }
        Shape::WhileLoop { body, .. }
        | Shape::ForLoop { body, .. }
        | Shape::Loop { body, .. } => {
            adjust_use_counts_for_logical_ops(ctx, func, body);
        }
        _ => {}
    }
}

/// Check if a LogicalOr/LogicalAnd rhs_body will produce no visible output.
///
/// True when every non-terminator instruction in the body would be inlined
/// (single-use result) or the block is empty.
fn logical_rhs_body_emits_empty(ctx: &EmitCtx, func: &Function, shape: &Shape) -> bool {
    match shape {
        Shape::Seq(parts) => parts
            .iter()
            .all(|p| logical_rhs_body_emits_empty(ctx, func, p)),
        Shape::Block(b) => {
            let block = &func.blocks[*b];
            for &inst_id in &block.insts {
                let inst = &func.insts[inst_id];
                match &inst.op {
                    Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => break,
                    _ => {
                        if let Some(r) = inst.result {
                            if !ctx.should_inline(r) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                }
            }
            true
        }
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Identifier sanitization
// ---------------------------------------------------------------------------

/// Sanitize a name into a valid JavaScript identifier.
///
/// Replaces non-alphanumeric characters with `_` and prefixes with `_` if the
/// name starts with a digit.
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
    out
}

/// Check whether a string is a valid JS identifier (safe for dot-access).
fn is_valid_js_ident(name: &str) -> bool {
    !name.is_empty()
        && !name.starts_with(|c: char| c.is_ascii_digit())
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
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

fn emit_runtime_imports(module: &Module, out: &mut String) {
    let systems =
        collect_system_names_from_funcs(module.functions.iter().map(|(_id, f)| f));
    emit_runtime_imports_for(systems, out, 0);
}

/// Emit runtime imports with path prefix adjusted for directory depth.
///
/// `depth` is the number of directories below the module dir. From depth `d`,
/// the runtime path is `"../".repeat(d + 1) + "runtime"` (the `+1` accounts
/// for the module dir being one level inside `output_dir`).
fn emit_runtime_imports_for(systems: BTreeSet<String>, out: &mut String, depth: usize) {
    if systems.is_empty() {
        return;
    }
    let prefix = if depth == 0 {
        ".".to_string()
    } else {
        "../".repeat(depth).trim_end_matches('/').to_string()
    };
    let known: BTreeSet<&str> = SYSTEM_NAMES.iter().copied().collect();
    let mut generic: Vec<&str> = Vec::new();
    let mut flash: Vec<String> = Vec::new();
    for sys in &systems {
        if known.contains(sys.as_str()) {
            generic.push(sys.as_str());
        } else {
            flash.push(sanitize_ident(sys));
        }
    }
    if !generic.is_empty() {
        let _ = writeln!(
            out,
            "import {{ {} }} from \"{prefix}/runtime\";",
            generic.join(", ")
        );
    }
    if !flash.is_empty() {
        let _ = writeln!(
            out,
            "import {{ {} }} from \"{prefix}/runtime/flash\";",
            flash.join(", ")
        );
    }
    if !generic.is_empty() || !flash.is_empty() {
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
    group: &ClassGroup<'_>,
    registry: &ClassRegistry,
) -> (BTreeSet<String>, BTreeSet<String>) {
    let self_name = &group.class_def.name;
    let mut value_refs = BTreeSet::new();
    let mut type_refs = BTreeSet::new();

    // Super class reference — runtime value (extends).
    if let Some(sc) = &group.class_def.super_class {
        let short = sc.rsplit("::").next().unwrap_or(sc);
        if short != self_name {
            if let Some(entry) = registry.lookup(sc) {
                value_refs.insert(entry.short_name.clone());
            }
        }
    }

    // Struct fields (class instance fields) — type-only.
    for (_name, ty) in &group.struct_def.fields {
        collect_type_ref(ty, self_name, registry, &mut type_refs);
    }

    // Scan all method bodies for type references.
    for (_fid, func) in &group.methods {
        collect_type_refs_from_function(func, self_name, registry, &mut value_refs, &mut type_refs);
    }

    (value_refs, type_refs)
}

/// Scan a function's instructions and signature for type references.
fn collect_type_refs_from_function(
    func: &Function,
    self_name: &str,
    registry: &ClassRegistry,
    value_refs: &mut BTreeSet<String>,
    type_refs: &mut BTreeSet<String>,
) {
    // Return type and param types — type-only.
    collect_type_ref(&func.sig.return_ty, self_name, registry, type_refs);
    for ty in &func.sig.params {
        collect_type_ref(ty, self_name, registry, type_refs);
    }

    // Instructions.
    for (_inst_id, inst) in func.insts.iter() {
        match &inst.op {
            // TypeCheck emits `instanceof` — runtime value reference.
            Op::TypeCheck(_, ty) => {
                collect_type_ref(ty, self_name, registry, value_refs);
            }
            // Alloc and Cast are type assertions only — type-only.
            Op::Alloc(ty) | Op::Cast(_, ty) => {
                collect_type_ref(ty, self_name, registry, type_refs);
            }
            // GetField with a class name → runtime value reference (used with `new`).
            Op::GetField { field, .. } => {
                if registry.lookup(field).is_some() {
                    collect_type_ref(
                        &Type::Struct(field.clone()),
                        self_name,
                        registry,
                        value_refs,
                    );
                }
            }
            _ => {}
        }
    }

    // value_types — type-only.
    for (_vid, ty) in func.value_types.iter() {
        collect_type_ref(ty, self_name, registry, type_refs);
    }
}

/// If a type references a class in the registry, add its short name.
fn collect_type_ref(
    ty: &Type,
    self_name: &str,
    registry: &ClassRegistry,
    refs: &mut BTreeSet<String>,
) {
    match ty {
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            if short != self_name {
                if let Some(entry) = registry.lookup(name) {
                    refs.insert(entry.short_name.clone());
                }
            }
        }
        Type::Array(inner) | Type::Option(inner) => {
            collect_type_ref(inner, self_name, registry, refs);
        }
        Type::Map(k, v) => {
            collect_type_ref(k, self_name, registry, refs);
            collect_type_ref(v, self_name, registry, refs);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                collect_type_ref(elem, self_name, registry, refs);
            }
        }
        Type::Function(sig) => {
            collect_type_ref(&sig.return_ty, self_name, registry, refs);
            for p in &sig.params {
                collect_type_ref(p, self_name, registry, refs);
            }
        }
        Type::Coroutine {
            yield_ty,
            return_ty,
        } => {
            collect_type_ref(yield_ty, self_name, registry, refs);
            collect_type_ref(return_ty, self_name, registry, refs);
        }
        Type::Union(types) => {
            for t in types {
                collect_type_ref(t, self_name, registry, refs);
            }
        }
        _ => {}
    }
}

/// Emit `import` / `import type` statements for intra-module class references.
fn emit_intra_imports(
    group: &ClassGroup<'_>,
    source_segments: &[String],
    registry: &ClassRegistry,
    out: &mut String,
) {
    let (value_refs, type_refs) = collect_class_references(group, registry);
    if value_refs.is_empty() && type_refs.is_empty() {
        return;
    }

    // Value imports first (runtime-needed).
    for short_name in &value_refs {
        if let Some(entry) = registry.classes.get(short_name) {
            let rel = relative_import_path(source_segments, &entry.path_segments);
            let _ = writeln!(out, "import {{ {short_name} }} from \"{rel}\";");
        }
    }
    // Type-only imports (names not already in value_refs).
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
    module: &Module,
    class_names: &HashMap<String, String>,
    out: &mut String,
) -> Result<(), CoreError> {
    for (_id, func) in module.functions.iter() {
        emit_function(func, class_names, out)?;
    }
    Ok(())
}

fn emit_function(
    func: &Function,
    class_names: &HashMap<String, String>,
    out: &mut String,
) -> Result<(), CoreError> {
    let mut ctx = EmitCtx::for_function(func, class_names);
    let vis = visibility_prefix(func.visibility);
    let star = if func.coroutine.is_some() { "*" } else { "" };

    // Parameters from entry block.
    let entry = &func.blocks[func.entry];
    let params: Vec<String> = entry
        .params
        .iter()
        .map(|p| format!("{}: {}", ctx.val(p.value), ts_type(&p.ty)))
        .collect();
    let ret_ty = ts_type(&func.sig.return_ty);

    let _ = write!(
        out,
        "{vis}function{star} {}({params}): {ret_ty}",
        sanitize_ident(&func.name),
        params = params.join(", "),
    );

    let is_simple = func.blocks.len() == 1;

    let _ = writeln!(out, " {{");

    if is_simple {
        let insts = &func.blocks[func.entry].insts;
        let emit_count = if insts
            .last()
            .is_some_and(|&id| matches!(func.insts[id].op, Op::Return(None)))
        {
            insts.len() - 1
        } else {
            insts.len()
        };
        for &inst_id in &insts[..emit_count] {
            emit_inst(&mut ctx, func, inst_id, out, "  ")?;
        }
    } else {
        // Pre-declare only non-entry block parameters.
        emit_block_param_declarations(&ctx, func, out);

        let shape = structurize::structurize(func);
        adjust_use_counts_for_logical_ops(&mut ctx, func, &shape);
        emit_shape_strip_trailing_return(&mut ctx, func, &shape, out, "  ")?;
    }

    let _ = writeln!(out, "}}\n");
    Ok(())
}


/// Negate a condition for `if` emission, avoiding double negation.
///
/// If `cond` is the result of `Op::Not(inner)`, returns `inner` directly
/// instead of wrapping with `!`.
fn negate_cond(ctx: &EmitCtx, func: &Function, block: BlockId, cond: ValueId) -> String {
    for &inst_id in &func.blocks[block].insts {
        let inst = &func.insts[inst_id];
        if inst.result == Some(cond) {
            if let Op::Not(inner) = &inst.op {
                return ctx.val(*inner);
            }
            break;
        }
    }
    format!("!{}", ctx.operand(cond))
}

/// Emit a structured shape tree as TypeScript.
fn emit_shape(
    ctx: &mut EmitCtx,
    func: &Function,
    shape: &Shape,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    match shape {
        Shape::Block(block_id) => {
            emit_block_instructions(ctx, func, *block_id, out, indent)?;
        }

        Shape::Seq(parts) => {
            for part in parts {
                emit_shape(ctx, func, part, out, indent)?;
            }
        }

        Shape::IfElse {
            block,
            cond,
            then_assigns,
            then_body,
            then_trailing_assigns,
            else_assigns,
            else_body,
            else_trailing_assigns,
        } => {
            // Emit the block's non-terminator instructions first.
            emit_block_instructions(ctx, func, *block, out, indent)?;

            // Emit both branches to buffers so we can detect truly empty output.
            let inner = format!("{indent}  ");
            let mut then_buf = String::new();
            emit_arg_assigns(ctx, then_assigns, &mut then_buf, &inner);
            emit_shape(ctx, func, then_body, &mut then_buf, &inner)?;
            emit_arg_assigns(ctx, then_trailing_assigns, &mut then_buf, &inner);

            let mut else_buf = String::new();
            emit_arg_assigns(ctx, else_assigns, &mut else_buf, &inner);
            emit_shape(ctx, func, else_body, &mut else_buf, &inner)?;
            emit_arg_assigns(ctx, else_trailing_assigns, &mut else_buf, &inner);

            let then_empty = then_buf.trim().is_empty();
            let else_empty = else_buf.trim().is_empty();

            match (then_empty, else_empty) {
                (true, true) => {
                    // Both branches empty — skip the entire if.
                }
                (false, true) => {
                    // Only then has content — no else.
                    let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
                    out.push_str(&then_buf);
                    let _ = writeln!(out, "{indent}}}");
                }
                (true, false) => {
                    // Only else has content — flip condition.
                    let neg = negate_cond(ctx, func, *block, *cond);
                    let _ = writeln!(out, "{indent}if ({neg}) {{");
                    out.push_str(&else_buf);
                    let _ = writeln!(out, "{indent}}}");
                }
                (false, false) => {
                    // Both branches have content — full if/else.
                    let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
                    out.push_str(&then_buf);
                    let _ = writeln!(out, "{indent}}} else {{");
                    out.push_str(&else_buf);
                    let _ = writeln!(out, "{indent}}}");
                }
            }
        }

        Shape::WhileLoop {
            header,
            cond,
            cond_negated,
            body,
        } => {
            let _ = writeln!(out, "{indent}while (true) {{");
            let inner = format!("{indent}  ");
            emit_block_instructions(ctx, func, *header, out, &inner)?;
            // Break when loop condition is false.
            // cond_negated=false → loop while cond, break on !cond
            // cond_negated=true  → loop while !cond, break on cond
            let break_expr = if *cond_negated {
                ctx.val(*cond)
            } else {
                negate_cond(ctx, func, *header, *cond)
            };
            let _ = writeln!(out, "{inner}if ({break_expr}) break;");
            // Strip trailing Continue — while(true) loops back naturally.
            emit_shape_strip_trailing_continue(ctx, func, body, out, &inner)?;
            let _ = writeln!(out, "{indent}}}");
        }

        Shape::ForLoop {
            header,
            init_assigns,
            cond,
            cond_negated,
            update_assigns,
            body,
        } => {
            // Emit init assignments before the loop.
            emit_arg_assigns(ctx, init_assigns, out, indent);

            let _ = writeln!(out, "{indent}while (true) {{");
            let inner = format!("{indent}  ");
            emit_block_instructions(ctx, func, *header, out, &inner)?;
            let break_expr = if *cond_negated {
                ctx.val(*cond)
            } else {
                negate_cond(ctx, func, *header, *cond)
            };
            let _ = writeln!(out, "{inner}if ({break_expr}) break;");
            // Strip trailing Continue — update assigns + natural loop-back handle it.
            emit_shape_strip_trailing_continue(ctx, func, body, out, &inner)?;
            emit_arg_assigns(ctx, update_assigns, out, &inner);
            let _ = writeln!(out, "{indent}}}");
        }

        Shape::Loop { header: _, body } => {
            let _ = writeln!(out, "{indent}while (true) {{");
            let inner = format!("{indent}  ");
            emit_shape(ctx, func, body, out, &inner)?;
            let _ = writeln!(out, "{indent}}}");
        }

        Shape::Break => {
            let _ = writeln!(out, "{indent}break;");
        }

        Shape::Continue => {
            let _ = writeln!(out, "{indent}continue;");
        }

        Shape::LabeledBreak { depth } => {
            // Use label syntax: break L0; break L1; etc.
            let _ = writeln!(out, "{indent}break L{depth};");
        }

        Shape::LogicalOr {
            block,
            cond,
            phi,
            rhs_body,
            rhs,
        } => {
            emit_block_instructions(ctx, func, *block, out, indent)?;
            let inner = format!("{indent}  ");
            let mut body_buf = String::new();
            emit_shape(ctx, func, rhs_body, &mut body_buf, &inner)?;
            if body_buf.trim().is_empty() {
                let expr = format!("{} || {}", ctx.operand(*cond), ctx.operand(*rhs));
                emit_or_inline(ctx, *phi, expr, true, out, indent);
            } else {
                let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
                let _ = writeln!(out, "{inner}{} = {};", ctx.val(*phi), ctx.val(*cond));
                let _ = writeln!(out, "{indent}}} else {{");
                out.push_str(&body_buf);
                let _ = writeln!(out, "{inner}{} = {};", ctx.val(*phi), ctx.val(*rhs));
                let _ = writeln!(out, "{indent}}}");
            }
        }

        Shape::LogicalAnd {
            block,
            cond,
            phi,
            rhs_body,
            rhs,
        } => {
            emit_block_instructions(ctx, func, *block, out, indent)?;
            let inner = format!("{indent}  ");
            let mut body_buf = String::new();
            emit_shape(ctx, func, rhs_body, &mut body_buf, &inner)?;
            if body_buf.trim().is_empty() {
                let expr = format!("{} && {}", ctx.operand(*cond), ctx.operand(*rhs));
                emit_or_inline(ctx, *phi, expr, true, out, indent);
            } else {
                let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
                out.push_str(&body_buf);
                let _ = writeln!(out, "{inner}{} = {};", ctx.val(*phi), ctx.val(*rhs));
                let _ = writeln!(out, "{indent}}} else {{");
                let _ = writeln!(out, "{inner}{} = {};", ctx.val(*phi), ctx.val(*cond));
                let _ = writeln!(out, "{indent}}}");
            }
        }

        Shape::Dispatch { blocks, entry } => {
            // Fallback dispatch loop.
            let _ = writeln!(out, "{indent}let $block = {};", entry.index());
            let _ = writeln!(out, "{indent}while (true) {{");
            let _ = writeln!(out, "{indent}  switch ($block) {{");

            for &block_id in blocks {
                let block = &func.blocks[block_id];
                let _ = writeln!(out, "{indent}    case {}: {{", block_id.index());
                let case_indent = format!("{indent}      ");
                emit_block_body(ctx, func, block_id, block, out, &case_indent)?;
                let _ = writeln!(out, "{indent}    }}");
            }

            let _ = writeln!(out, "{indent}  }}");
            let _ = writeln!(out, "{indent}}}");
        }
    }

    Ok(())
}

/// Emit a shape, stripping a trailing `Continue` from `Seq` shapes.
///
/// Used inside `while(true)` loops where the natural loop-back makes an
/// explicit `continue` redundant.
fn emit_shape_strip_trailing_continue(
    ctx: &mut EmitCtx,
    func: &Function,
    shape: &Shape,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    match shape {
        Shape::Seq(parts) => {
            let len = parts.len();
            for (i, part) in parts.iter().enumerate() {
                if i == len - 1 && *part == Shape::Continue {
                    // Skip trailing Continue.
                    continue;
                }
                emit_shape(ctx, func, part, out, indent)?;
            }
            Ok(())
        }
        Shape::Continue => Ok(()), // Just a bare Continue — skip it.
        _ => emit_shape(ctx, func, shape, out, indent),
    }
}

/// Emit a shape, stripping a trailing `Return(None)` from the output.
///
/// Used at the top level of function/method bodies to omit unnecessary
/// `return;` at the end of void functions.
fn emit_shape_strip_trailing_return(
    ctx: &mut EmitCtx,
    func: &Function,
    shape: &Shape,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    match shape {
        Shape::Seq(parts) => {
            let len = parts.len();
            for (i, part) in parts.iter().enumerate() {
                if i == len - 1 {
                    emit_shape_strip_trailing_return(ctx, func, part, out, indent)?;
                } else {
                    emit_shape(ctx, func, part, out, indent)?;
                }
            }
            Ok(())
        }
        Shape::Block(block_id) => {
            // Emit block instructions, skip trailing Return(None).
            let block = &func.blocks[*block_id];
            for &inst_id in &block.insts {
                let inst = &func.insts[inst_id];
                match &inst.op {
                    Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => break,
                    Op::Return(None) => {
                        // Skip — trailing void return.
                    }
                    _ => {
                        emit_inst(ctx, func, inst_id, out, indent)?;
                    }
                }
            }
            Ok(())
        }
        Shape::IfElse {
            block,
            cond,
            then_assigns,
            then_body,
            then_trailing_assigns,
            else_assigns,
            else_body,
            else_trailing_assigns,
        } => {
            // Emit the block's non-terminator instructions first.
            emit_block_instructions(ctx, func, *block, out, indent)?;

            // Emit both branches to buffers so we can detect truly empty output.
            let inner = format!("{indent}  ");
            let mut then_buf = String::new();
            emit_arg_assigns(ctx, then_assigns, &mut then_buf, &inner);
            emit_shape_strip_trailing_return(ctx, func, then_body, &mut then_buf, &inner)?;
            emit_arg_assigns(ctx, then_trailing_assigns, &mut then_buf, &inner);

            let mut else_buf = String::new();
            emit_arg_assigns(ctx, else_assigns, &mut else_buf, &inner);
            emit_shape_strip_trailing_return(ctx, func, else_body, &mut else_buf, &inner)?;
            emit_arg_assigns(ctx, else_trailing_assigns, &mut else_buf, &inner);

            let then_empty = then_buf.trim().is_empty();
            let else_empty = else_buf.trim().is_empty();

            match (then_empty, else_empty) {
                (true, true) => {
                    // Both branches empty — skip the entire if.
                }
                (false, true) => {
                    let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
                    out.push_str(&then_buf);
                    let _ = writeln!(out, "{indent}}}");
                }
                (true, false) => {
                    let neg = negate_cond(ctx, func, *block, *cond);
                    let _ = writeln!(out, "{indent}if ({neg}) {{");
                    out.push_str(&else_buf);
                    let _ = writeln!(out, "{indent}}}");
                }
                (false, false) => {
                    let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
                    out.push_str(&then_buf);
                    let _ = writeln!(out, "{indent}}} else {{");
                    out.push_str(&else_buf);
                    let _ = writeln!(out, "{indent}}}");
                }
            }
            Ok(())
        }
        _ => emit_shape(ctx, func, shape, out, indent),
    }
}

/// Emit a block's non-terminator instructions.
///
/// Skips Br, BrIf, and Switch (handled by the shape tree). Stops after
/// encountering the first terminator — any instructions after it are dead
/// code from the frontend and should not be emitted. Return is kept since
/// it produces output (`return ...;`).
fn emit_block_instructions(
    ctx: &mut EmitCtx,
    func: &Function,
    block_id: BlockId,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    let block = &func.blocks[block_id];
    for &inst_id in &block.insts {
        let inst = &func.insts[inst_id];
        match &inst.op {
            Op::Br { .. } | Op::BrIf { .. } | Op::Switch { .. } => {
                // First terminator reached — stop emitting.
                break;
            }
            _ => {
                emit_inst(ctx, func, inst_id, out, indent)?;
            }
        }
    }
    Ok(())
}

/// Emit block argument assignments.
fn emit_arg_assigns(
    ctx: &EmitCtx,
    assigns: &[BlockArgAssign],
    out: &mut String,
    indent: &str,
) {
    for assign in assigns {
        let _ = writeln!(
            out,
            "{indent}{} = {};",
            ctx.val(assign.dst),
            ctx.val(assign.src)
        );
    }
}

/// Pre-declare `let` bindings for non-entry block parameters only.
fn emit_block_param_declarations(ctx: &EmitCtx, func: &Function, out: &mut String) {
    for (block_id, block) in func.blocks.iter() {
        if block_id == func.entry {
            continue;
        }
        for param in &block.params {
            let _ = writeln!(
                out,
                "  let {}: {};",
                ctx.val(param.value),
                ts_type(&param.ty)
            );
        }
    }
}

fn emit_block_body(
    ctx: &mut EmitCtx,
    func: &Function,
    _block_id: reincarnate_core::ir::BlockId,
    block: &Block,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    for &inst_id in &block.insts {
        emit_inst(ctx, func, inst_id, out, indent)?;
    }
    Ok(())
}

/// Either inline a single-use expression or emit it as a `let` binding.
fn emit_or_inline(
    ctx: &mut EmitCtx,
    r: ValueId,
    expr: String,
    needs_parens: bool,
    out: &mut String,
    indent: &str,
) {
    if ctx.should_inline(r) {
        ctx.store_inline(r, expr, needs_parens);
    } else if ctx.use_counts.get(&r).copied().unwrap_or(0) == 0 {
        let _ = writeln!(out, "{indent}{expr};");
    } else {
        let pfx = ctx.let_prefix(r);
        let _ = writeln!(out, "{indent}{pfx}{} = {expr};", ctx.val_name(r));
    }
}

fn emit_inst(
    ctx: &mut EmitCtx,
    func: &Function,
    inst_id: InstId,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    let inst = &func.insts[inst_id];
    let result = inst.result;

    match &inst.op {
        // -- Constants --
        Op::Const(c) => {
            let r = result.unwrap();
            let expr = emit_constant(c);
            emit_or_inline(ctx, r, expr, false, out, indent);
        }

        // -- Arithmetic --
        Op::Add(a, b) => emit_binop(ctx, out, indent, result, a, b, "+"),
        Op::Sub(a, b) => emit_binop(ctx, out, indent, result, a, b, "-"),
        Op::Mul(a, b) => emit_binop(ctx, out, indent, result, a, b, "*"),
        Op::Div(a, b) => emit_binop(ctx, out, indent, result, a, b, "/"),
        Op::Rem(a, b) => emit_binop(ctx, out, indent, result, a, b, "%"),
        Op::Neg(a) => {
            let r = result.unwrap();
            let expr = format!("-{}", ctx.operand(*a));
            emit_or_inline(ctx, r, expr, true, out, indent);
        }

        // -- Bitwise --
        Op::BitAnd(a, b) => emit_binop(ctx, out, indent, result, a, b, "&"),
        Op::BitOr(a, b) => emit_binop(ctx, out, indent, result, a, b, "|"),
        Op::BitXor(a, b) => emit_binop(ctx, out, indent, result, a, b, "^"),
        Op::BitNot(a) => {
            let r = result.unwrap();
            let expr = format!("~{}", ctx.operand(*a));
            emit_or_inline(ctx, r, expr, true, out, indent);
        }
        Op::Shl(a, b) => emit_binop(ctx, out, indent, result, a, b, "<<"),
        Op::Shr(a, b) => emit_binop(ctx, out, indent, result, a, b, ">>"),

        // -- Comparison --
        Op::Cmp(kind, a, b) => {
            let op = match kind {
                CmpKind::Eq => "===",
                CmpKind::Ne => "!==",
                CmpKind::Lt => "<",
                CmpKind::Le => "<=",
                CmpKind::Gt => ">",
                CmpKind::Ge => ">=",
            };
            emit_binop(ctx, out, indent, result, a, b, op);
        }

        // -- Logic --
        Op::Not(a) => {
            let r = result.unwrap();
            let expr = format!("!{}", ctx.operand(*a));
            emit_or_inline(ctx, r, expr, true, out, indent);
        }
        Op::Select {
            cond,
            on_true,
            on_false,
        } => {
            let r = result.unwrap();
            let expr = format!(
                "{} ? {} : {}",
                ctx.operand(*cond),
                ctx.operand(*on_true),
                ctx.operand(*on_false)
            );
            emit_or_inline(ctx, r, expr, true, out, indent);
        }

        // -- Control flow --
        Op::Br { target, args } => {
            emit_branch_args(ctx, func, *target, args, out, indent);
            let _ = writeln!(out, "{indent}$block = {}; continue;", target.index());
        }
        Op::BrIf {
            cond,
            then_target,
            then_args,
            else_target,
            else_args,
        } => {
            let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
            let inner = format!("{indent}  ");
            emit_branch_args(ctx, func, *then_target, then_args, out, &inner);
            let _ = writeln!(
                out,
                "{inner}$block = {}; continue;",
                then_target.index()
            );
            let _ = writeln!(out, "{indent}}} else {{");
            emit_branch_args(ctx, func, *else_target, else_args, out, &inner);
            let _ = writeln!(
                out,
                "{inner}$block = {}; continue;",
                else_target.index()
            );
            let _ = writeln!(out, "{indent}}}");
        }
        Op::Switch {
            value,
            cases,
            default,
        } => {
            let _ = writeln!(out, "{indent}switch ({}) {{", ctx.val(*value));
            for (constant, target, args) in cases {
                let _ = writeln!(out, "{indent}  case {}:", emit_constant(constant));
                let inner = format!("{indent}    ");
                emit_branch_args(ctx, func, *target, args, out, &inner);
                let _ = writeln!(
                    out,
                    "{inner}$block = {}; continue;",
                    target.index()
                );
            }
            let _ = writeln!(out, "{indent}  default:");
            let inner = format!("{indent}    ");
            emit_branch_args(ctx, func, default.0, &default.1, out, &inner);
            let _ = writeln!(
                out,
                "{inner}$block = {}; continue;",
                default.0.index()
            );
            let _ = writeln!(out, "{indent}}}");
        }
        Op::Return(v) => match v {
            Some(v) => {
                let _ = writeln!(out, "{indent}return {};", ctx.val(*v));
            }
            None => {
                let _ = writeln!(out, "{indent}return;");
            }
        },

        // -- Memory / fields --
        Op::Alloc(ty) => {
            // Alloc is a declaration, not an expression — always emit directly.
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(
                out,
                "{indent}{pfx}{}: {};",
                ctx.val_name(r),
                ts_type(ty)
            );
        }
        Op::Load(ptr) => {
            let r = result.unwrap();
            let expr = ctx.val(*ptr);
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
        Op::Store { ptr, value } => {
            let _ = writeln!(out, "{indent}{} = {};", ctx.val(*ptr), ctx.val(*value));
        }
        Op::GetField { object, field } => {
            let r = result.unwrap();
            // Resolve scope lookup + GetField → bare name.
            if ctx.scope_lookups.contains(object) {
                let resolved = if let Some(short) = ctx.class_names.get(field) {
                    short.clone()
                } else {
                    let effective = field.rsplit("::").next().unwrap_or(field);
                    sanitize_ident(effective)
                };
                emit_or_inline(ctx, r, resolved, false, out, indent);
                return Ok(());
            }
            // Extract short name from qualified fields (e.g. "ns::Class::prop" → "prop").
            let effective_field = if field.contains("::") {
                field.rsplit("::").next().unwrap_or(field)
            } else {
                field
            };
            let expr = if is_valid_js_ident(effective_field) {
                format!("{}.{effective_field}", ctx.operand(*object))
            } else {
                format!(
                    "{}[\"{}\"]",
                    ctx.operand(*object),
                    escape_js_string(effective_field)
                )
            };
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
        Op::SetField {
            object,
            field,
            value,
        } => {
            // Resolve scope lookup + SetField → bare assignment.
            if ctx.scope_lookups.contains(object) {
                let effective = field.rsplit("::").next().unwrap_or(field);
                let name = sanitize_ident(effective);
                let _ = writeln!(out, "{indent}{name} = {};", ctx.val(*value));
                return Ok(());
            }
            // Extract short name from qualified fields (e.g. "ns::Class::prop" → "prop").
            let effective_field = if field.contains("::") {
                field.rsplit("::").next().unwrap_or(field)
            } else {
                field
            };
            if is_valid_js_ident(effective_field) {
                let _ = writeln!(
                    out,
                    "{indent}{}.{effective_field} = {};",
                    ctx.operand(*object),
                    ctx.val(*value)
                );
            } else {
                let _ = writeln!(
                    out,
                    "{indent}{}[\"{}\"] = {};",
                    ctx.operand(*object),
                    escape_js_string(effective_field),
                    ctx.val(*value)
                );
            }
        }
        Op::GetIndex { collection, index } => {
            let r = result.unwrap();
            let expr = format!("{}[{}]", ctx.operand(*collection), ctx.val(*index));
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
        Op::SetIndex {
            collection,
            index,
            value,
        } => {
            let _ = writeln!(
                out,
                "{indent}{}[{}] = {};",
                ctx.operand(*collection),
                ctx.val(*index),
                ctx.val(*value)
            );
        }

        // -- Calls --
        Op::Call { func: fname, args } => {
            let expr = if fname.contains("::") && !args.is_empty() {
                // Qualified name → method dispatch: receiver.method(rest_args)
                let method = fname.rsplit("::").next().unwrap_or(fname);
                let receiver = args[0];
                let rest_args = args[1..]
                    .iter()
                    .map(|a| ctx.val(*a))
                    .collect::<Vec<_>>()
                    .join(", ");
                if ctx.scope_lookups.contains(&receiver) {
                    if ctx.self_value.is_some() && ctx.method_names.contains(method) {
                        // Inherited method via scope lookup → this.method()
                        format!("this.{}({rest_args})", sanitize_ident(method))
                    } else {
                        // True global scope lookup → bare function call
                        format!("{}({rest_args})", sanitize_ident(method))
                    }
                } else {
                    format!(
                        "{}.{}({rest_args})",
                        ctx.operand(receiver),
                        sanitize_ident(method)
                    )
                }
            } else if !args.is_empty() && ctx.scope_lookups.contains(&args[0]) {
                // Scope-lookup receiver — strip it; emit as this.method() or
                // bare global depending on whether fname is in the class hierarchy.
                let rest_args = args[1..]
                    .iter()
                    .map(|a| ctx.val(*a))
                    .collect::<Vec<_>>()
                    .join(", ");
                let safe_name = sanitize_ident(fname);
                if ctx.self_value.is_some() && ctx.method_names.contains(fname) {
                    format!("this.{safe_name}({rest_args})")
                } else {
                    format!("{safe_name}({rest_args})")
                }
            } else if !args.is_empty() {
                // Unqualified call with a real receiver (callproperty pattern):
                // args[0] is the receiver, args[1..] are the arguments.
                let receiver = args[0];
                let rest_args = args[1..]
                    .iter()
                    .map(|a| ctx.val(*a))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}.{}({rest_args})",
                    ctx.operand(receiver),
                    sanitize_ident(fname)
                )
            } else {
                // No args at all — bare function call.
                format!("{}()", sanitize_ident(fname))
            };
            if let Some(r) = result {
                emit_or_inline(ctx, r, expr, false, out, indent);
            } else {
                let _ = writeln!(out, "{indent}{expr};");
            }
        }
        Op::CallIndirect { callee, args } => {
            let args_str = args
                .iter()
                .map(|a| ctx.val(*a))
                .collect::<Vec<_>>()
                .join(", ");
            let expr = format!("{}({args_str})", ctx.val(*callee));
            if let Some(r) = result {
                emit_or_inline(ctx, r, expr, false, out, indent);
            } else {
                let _ = writeln!(out, "{indent}{expr};");
            }
        }
        Op::SystemCall {
            system,
            method,
            args,
        } => {
            // constructSuper(this, ...rest) → super(rest)
            if system == "Flash.Class" && method == "constructSuper" {
                let rest_args = args
                    .iter()
                    .skip(1)
                    .map(|a| ctx.val(*a))
                    .collect::<Vec<_>>()
                    .join(", ");
                let _ = writeln!(out, "{indent}super({rest_args});");
                return Ok(());
            }

            // construct(ctor, ...rest) → new ctor(rest)
            if system == "Flash.Object" && method == "construct" {
                if let Some((&ctor, rest)) = args.split_first() {
                    let rest_args = rest
                        .iter()
                        .map(|a| ctx.val(*a))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let expr = format!("new {}({rest_args})", ctx.val(ctor));
                    if let Some(r) = result {
                        emit_or_inline(ctx, r, expr, false, out, indent);
                    } else {
                        let _ = writeln!(out, "{indent}{expr};");
                    }
                    return Ok(());
                }
            }

            // findPropStrict/findProperty → resolve to `this` for class-member lookups,
            // otherwise record as scope lookup for bare-name resolution in GetField/SetField.
            if system == "Flash.Scope"
                && (method == "findPropStrict" || method == "findProperty")
            {
                if let Some(r) = result {
                    if ctx.self_value.is_some() {
                        if let Some(class_name) = class_from_scope_arg(func, args) {
                            if ctx.ancestors.contains(&class_name) {
                                ctx.store_inline(r, "this".to_string(), false);
                                return Ok(());
                            }
                        }
                    }
                    // Fallback: record for bare-name resolution in GetField/SetField/binop.
                    ctx.scope_lookups.insert(r);
                    return Ok(());
                }
            }

            let args_str = args
                .iter()
                .map(|a| ctx.val(*a))
                .collect::<Vec<_>>()
                .join(", ");
            let sys_ident = sanitize_ident(system);
            let safe_method = if is_valid_js_ident(method) {
                format!(".{method}")
            } else {
                format!("[\"{}\"]", escape_js_string(method))
            };
            let expr = format!("{sys_ident}{safe_method}({args_str})");
            if let Some(r) = result {
                emit_or_inline(ctx, r, expr, false, out, indent);
            } else {
                let _ = writeln!(out, "{indent}{expr};");
            }
        }

        // -- Type operations --
        Op::Cast(v, ty) => {
            let r = result.unwrap();
            if func.value_types[*v] == *ty {
                // Same type — elide the redundant cast.
                emit_or_inline(ctx, r, ctx.operand(*v), false, out, indent);
            } else if ctx.should_inline(r) {
                // Inlined into another expression — need `as T` wrapper.
                let expr = format!("{} as {}", ctx.operand(*v), ts_type(ty));
                ctx.store_inline(r, expr, true);
            } else if ctx.use_counts.get(&r).copied().unwrap_or(0) == 0 {
                // Unused — emit source as side-effect statement.
                let _ = writeln!(out, "{indent}{};", ctx.operand(*v));
            } else {
                // Assigned to a variable — type annotation instead of `as T`.
                let pfx = ctx.let_prefix(r);
                let _ = writeln!(
                    out,
                    "{indent}{pfx}{}: {} = {};",
                    ctx.val_name(r),
                    ts_type(ty),
                    ctx.operand(*v)
                );
            }
        }
        Op::TypeCheck(v, ty) => {
            let r = result.unwrap();
            let check = type_check_expr(ctx, *v, ty);
            emit_or_inline(ctx, r, check, false, out, indent);
        }

        // -- Aggregate construction --
        Op::StructInit { name: _, fields } => {
            let r = result.unwrap();
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(name, v)| {
                    if is_valid_js_ident(name) {
                        format!("{name}: {}", ctx.val(*v))
                    } else {
                        format!("\"{}\": {}", escape_js_string(name), ctx.val(*v))
                    }
                })
                .collect();
            let expr = format!("{{ {} }}", field_strs.join(", "));
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
        Op::ArrayInit(elems) => {
            let r = result.unwrap();
            let elems_str = elems
                .iter()
                .map(|v| ctx.val(*v))
                .collect::<Vec<_>>()
                .join(", ");
            let expr = format!("[{elems_str}]");
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
        Op::TupleInit(elems) => {
            let r = result.unwrap();
            let elems_str = elems
                .iter()
                .map(|v| ctx.val(*v))
                .collect::<Vec<_>>()
                .join(", ");
            let ty_str = ts_type(&func.value_types[r]);
            let expr = format!("[{elems_str}] as {ty_str}");
            emit_or_inline(ctx, r, expr, true, out, indent
            );
        }

        // -- Coroutines --
        Op::Yield(v) => {
            // Yield has side effects — always emit directly.
            if let Some(r) = result {
                let pfx = ctx.let_prefix(r);
                match v {
                    Some(yv) => {
                        let _ = writeln!(
                            out,
                            "{indent}{pfx}{} = yield {};",
                            ctx.val_name(r),
                            ctx.val(*yv)
                        );
                    }
                    None => {
                        let _ = writeln!(out, "{indent}{pfx}{} = yield;", ctx.val_name(r));
                    }
                }
            } else {
                match v {
                    Some(yv) => {
                        let _ = writeln!(out, "{indent}yield {};", ctx.val(*yv));
                    }
                    None => {
                        let _ = writeln!(out, "{indent}yield;");
                    }
                }
            }
        }
        Op::CoroutineCreate {
            func: fname,
            args,
        } => {
            let r = result.unwrap();
            let args_str = args
                .iter()
                .map(|a| ctx.val(*a))
                .collect::<Vec<_>>()
                .join(", ");
            let expr = format!("{}({args_str})", sanitize_ident(fname));
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
        Op::CoroutineResume(v) => {
            let r = result.unwrap();
            let expr = format!("{}.next()", ctx.val(*v));
            emit_or_inline(ctx, r, expr, false, out, indent);
        }

        // -- Misc --
        Op::GlobalRef(name) => {
            let r = result.unwrap();
            let expr = sanitize_ident(name);
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
        Op::Copy(src) => {
            let r = result.unwrap();
            let expr = ctx.val(*src);
            emit_or_inline(ctx, r, expr, false, out, indent);
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Class grouping and emission
// ---------------------------------------------------------------------------

struct ClassGroup<'a> {
    class_def: &'a ClassDef,
    struct_def: &'a StructDef,
    methods: Vec<(FuncId, &'a Function)>,
}

/// Partition module contents into class groups and free functions.
fn group_by_class(module: &Module) -> (Vec<ClassGroup<'_>>, Vec<(FuncId, &Function)>) {
    let mut claimed: HashSet<FuncId> = HashSet::new();
    let mut groups = Vec::new();

    for class in &module.classes {
        let struct_def = &module.structs[class.struct_index];
        let methods: Vec<(FuncId, &Function)> = class
            .methods
            .iter()
            .filter_map(|&fid| {
                let func = module.functions.get(fid)?;
                claimed.insert(fid);
                Some((fid, func))
            })
            .collect();
        groups.push(ClassGroup {
            class_def: class,
            struct_def,
            methods,
        });
    }

    let free: Vec<(FuncId, &Function)> = module
        .functions
        .iter()
        .filter(|(fid, _)| !claimed.contains(fid))
        .collect();

    (groups, free)
}

/// Emit a TypeScript class from a `ClassGroup`.
fn emit_class(
    group: &ClassGroup<'_>,
    _module: &Module,
    class_names: &HashMap<String, String>,
    ancestor_sets: &HashMap<String, HashSet<String>>,
    method_name_sets: &HashMap<String, HashSet<String>>,
    out: &mut String,
) -> Result<(), CoreError> {
    let class_name = sanitize_ident(&group.class_def.name);
    let vis = visibility_prefix(group.class_def.visibility);

    let extends = match &group.class_def.super_class {
        Some(sc) => {
            let base = sc.rsplit("::").next().unwrap_or(sc);
            format!(" extends {}", sanitize_ident(base))
        }
        None => String::new(),
    };

    let _ = writeln!(out, "{vis}class {class_name}{extends} {{");

    // Fields from struct def.
    for (name, ty) in &group.struct_def.fields {
        let _ = writeln!(out, "  {}: {};", sanitize_ident(name), ts_type(ty));
    }
    if !group.struct_def.fields.is_empty() && !group.methods.is_empty() {
        out.push('\n');
    }

    // Methods — sorted: constructor first, then instance, static, getters, setters.
    let mut sorted_methods: Vec<&(FuncId, &Function)> = group.methods.iter().collect();
    sorted_methods.sort_by_key(|(_, f)| match f.method_kind {
        MethodKind::Constructor => 0,
        MethodKind::Instance => 1,
        MethodKind::Getter => 2,
        MethodKind::Setter => 3,
        MethodKind::Static => 4,
        MethodKind::Free => 5,
    });

    let qualified = qualified_class_name(group.class_def);
    let empty_ancestors = HashSet::new();
    let ancestors = ancestor_sets.get(&qualified).unwrap_or(&empty_ancestors);
    let empty_methods = HashSet::new();
    let method_names = method_name_sets.get(&qualified).unwrap_or(&empty_methods);

    for (i, &(_fid, func)) in sorted_methods.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        emit_class_method(func, class_names, ancestors, method_names, out)?;
    }

    let _ = writeln!(out, "}}\n");
    Ok(())
}

/// Emit a single method inside a class body.
fn emit_class_method(
    func: &Function,
    class_names: &HashMap<String, String>,
    ancestors: &HashSet<String>,
    method_names: &HashSet<String>,
    out: &mut String,
) -> Result<(), CoreError> {
    // Extract bare method name from func.name (last `::` segment).
    let raw_name = func
        .name
        .rsplit("::")
        .next()
        .unwrap_or(&func.name);

    let entry = &func.blocks[func.entry];
    let ret_ty = ts_type(&func.sig.return_ty);

    // Determine which params to emit (skip `this` for instance/constructor/getter/setter).
    let skip_self = matches!(
        func.method_kind,
        MethodKind::Constructor | MethodKind::Instance | MethodKind::Getter | MethodKind::Setter
    );
    let param_start = if skip_self { 1.min(entry.params.len()) } else { 0 };

    // Build context — methods with a self parameter get `this` binding.
    let mut ctx = if skip_self && !entry.params.is_empty() {
        EmitCtx::for_method(func, entry.params[0].value, class_names, ancestors, method_names)
    } else {
        EmitCtx::for_function(func, class_names)
    };

    let params: Vec<String> = entry.params[param_start..]
        .iter()
        .map(|p| format!("{}: {}", ctx.val(p.value), ts_type(&p.ty)))
        .collect();

    let is_simple = func.blocks.len() == 1;
    let star = if func.coroutine.is_some() { "*" } else { "" };

    // Method signature.
    match func.method_kind {
        MethodKind::Constructor => {
            let _ = writeln!(out, "  constructor({}) {{", params.join(", "));
        }
        MethodKind::Getter => {
            let name = raw_name
                .strip_prefix("get_")
                .unwrap_or(raw_name);
            let _ = writeln!(
                out,
                "  get {name}(): {ret_ty} {{",
            );
        }
        MethodKind::Setter => {
            let name = raw_name
                .strip_prefix("set_")
                .unwrap_or(raw_name);
            let _ = writeln!(
                out,
                "  set {name}({}) {{",
                params.join(", ")
            );
        }
        MethodKind::Static => {
            let _ = writeln!(
                out,
                "  static {star}{}({}): {ret_ty} {{",
                sanitize_ident(raw_name),
                params.join(", ")
            );
        }
        _ => {
            let _ = writeln!(
                out,
                "  {star}{}({}): {ret_ty} {{",
                sanitize_ident(raw_name),
                params.join(", ")
            );
        }
    }

    // Method body.
    if is_simple {
        let insts = &func.blocks[func.entry].insts;
        let emit_count = if insts
            .last()
            .is_some_and(|&id| matches!(func.insts[id].op, Op::Return(None)))
        {
            insts.len() - 1
        } else {
            insts.len()
        };
        for &inst_id in &insts[..emit_count] {
            emit_inst(&mut ctx, func, inst_id, out, "    ")?;
        }
    } else {
        emit_block_param_declarations_indented(&ctx, func, out, "    ");
        let shape = structurize::structurize(func);
        adjust_use_counts_for_logical_ops(&mut ctx, func, &shape);
        emit_shape_strip_trailing_return(&mut ctx, func, &shape, out, "    ")?;
    }

    let _ = writeln!(out, "  }}");
    Ok(())
}

/// Pre-declare `let` bindings for non-entry block parameters at a given indent.
fn emit_block_param_declarations_indented(
    ctx: &EmitCtx,
    func: &Function,
    out: &mut String,
    indent: &str,
) {
    for (block_id, block) in func.blocks.iter() {
        if block_id == func.entry {
            continue;
        }
        for param in &block.params {
            let _ = writeln!(
                out,
                "{indent}let {}: {};",
                ctx.val(param.value),
                ts_type(&param.ty)
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn emit_binop(
    ctx: &mut EmitCtx,
    out: &mut String,
    indent: &str,
    result: Option<ValueId>,
    a: &ValueId,
    b: &ValueId,
    op: &str,
) {
    let r = result.unwrap();
    let a_is_scope = ctx.scope_lookups.contains(a);
    let b_is_scope = ctx.scope_lookups.contains(b);
    if a_is_scope || b_is_scope {
        let expr = if a_is_scope {
            ctx.operand(*b)
        } else {
            ctx.operand(*a)
        };
        emit_or_inline(ctx, r, expr, false, out, indent);
        return;
    }
    let expr = format!("{} {op} {}", ctx.operand(*a), ctx.operand(*b));
    emit_or_inline(ctx, r, expr, true, out, indent);
}

fn emit_constant(c: &Constant) -> String {
    match c {
        Constant::Null => "null".into(),
        Constant::Bool(b) => b.to_string(),
        Constant::Int(n) => n.to_string(),
        Constant::UInt(n) => n.to_string(),
        Constant::Float(f) => format_float(*f),
        Constant::String(s) => format!("\"{}\"", escape_js_string(s)),
    }
}

fn format_float(f: f64) -> String {
    if f.fract() == 0.0 && f.is_finite() {
        format!("{f:.1}")
    } else {
        format!("{f}")
    }
}

fn escape_js_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out
}

fn visibility_prefix(vis: Visibility) -> &'static str {
    match vis {
        Visibility::Public => "export ",
        Visibility::Private | Visibility::Protected => "",
    }
}

/// Assign branch args to the target block's parameter variables.
fn emit_branch_args(
    ctx: &EmitCtx,
    func: &Function,
    target: reincarnate_core::ir::BlockId,
    args: &[ValueId],
    out: &mut String,
    indent: &str,
) {
    let target_block = &func.blocks[target];
    for (param, arg) in target_block.params.iter().zip(args.iter()) {
        let _ = writeln!(
            out,
            "{indent}{} = {};",
            ctx.val(param.value),
            ctx.val(*arg)
        );
    }
}

/// Extract the defining-class short name from a findPropStrict argument string.
///
/// Class-member namespaces have the format "pkg:ClassName::member" where the
/// single colon distinguishes them from package-level namespaces ("pkg.sub::name").
/// Returns `Some("ClassName")` for class-member lookups, `None` otherwise.
fn class_from_scope_arg(func: &Function, args: &[ValueId]) -> Option<String> {
    let arg = args.first()?;
    let name = func.insts.iter().find_map(|(_, inst)| {
        if inst.result == Some(*arg) {
            if let Op::Const(Constant::String(s)) = &inst.op {
                return Some(s.as_str());
            }
        }
        None
    })?;
    // Must have :: (qualified) and the prefix must contain : (class-scoped namespace).
    let prefix = name.rsplit_once("::")?.0;
    let class_name = prefix.rsplit_once(':')?.1;
    Some(class_name.to_string())
}

/// Generate a TypeScript type-check expression.
fn type_check_expr(ctx: &EmitCtx, v: ValueId, ty: &Type) -> String {
    match ty {
        Type::Bool => format!("typeof {} === \"boolean\"", ctx.operand(v)),
        Type::Int(_) | Type::UInt(_) | Type::Float(_) => {
            format!("typeof {} === \"number\"", ctx.operand(v))
        }
        Type::String => format!("typeof {} === \"string\"", ctx.operand(v)),
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            format!("{} instanceof {}", ctx.operand(v), sanitize_ident(short))
        }
        Type::Union(types) => {
            let checks: Vec<_> = types.iter().map(|t| type_check_expr(ctx, v, t)).collect();
            format!("({})", checks.join(" || "))
        }
        _ => format!("typeof {} === \"object\"", ctx.operand(v)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use reincarnate_core::ir::builder::{FunctionBuilder, ModuleBuilder};
    use reincarnate_core::ir::{
        ClassDef, EnumDef, EnumVariant, FunctionSig, Global, Import, MethodKind, StructDef,
        Visibility,
    };

    fn build_and_emit(build: impl FnOnce(&mut ModuleBuilder)) -> String {
        let mut mb = ModuleBuilder::new("test");
        build(&mut mb);
        emit_module_to_string(&mb.build()).unwrap()
    }

    #[test]
    fn simple_add_function() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64), Type::Int(64)],
                return_ty: Type::Int(64),
            };
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
                return_ty: Type::Int(64),
            };
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
        // Block args assignment.
        assert!(out.contains("v3 = v1;"), "Should assign v3 = v1:\n{out}");
        assert!(out.contains("v4 = v2;"), "Should assign v4 = v2:\n{out}");
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Void,
            };
            let mut fb = FunctionBuilder::new("constants", sig, Visibility::Public);
            fb.const_null();
            fb.const_bool(true);
            fb.const_bool(false);
            fb.const_int(42);
            fb.const_float(3.125);
            fb.const_string("hello \"world\"\nnewline");
            fb.ret(None);
            mb.add_function(fb.build());
        });

        // Unused constants are emitted as bare statements (no binding).
        assert!(out.contains("  null;"));
        assert!(out.contains("  true;"));
        assert!(out.contains("  false;"));
        assert!(out.contains("  42;"));
        assert!(out.contains("  3.125;"));
        assert!(out.contains(r#"  "hello \"world\"\nnewline";"#));
    }

    #[test]
    fn array_and_struct_init() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void,
            };
            let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);

            let a = fb.const_int(1);
            let b = fb.const_int(2);
            fb.array_init(&[a, b], Type::Int(64));

            let x = fb.const_float(10.0);
            let y = fb.const_float(20.0);
            fb.struct_init("Point", vec![("x".into(), x), ("y".into(), y)]);

            fb.ret(None);
            mb.add_function(fb.build());
        });

        // Constants are inlined into the aggregate expressions; unused results have no binding.
        assert!(out.contains("[1, 2];"), "Should inline consts into array:\n{out}");
        assert!(
            out.contains("{ x: 10.0, y: 20.0 };"),
            "Should inline consts into struct:\n{out}"
        );
    }

    #[test]
    fn mem2reg_plus_emit_eliminates_alloc_store_load() {
        use reincarnate_core::pipeline::Transform;
        use reincarnate_core::transforms::Mem2Reg;

        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64),
        };
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
        let result = Mem2Reg.apply(module).unwrap();
        let out = emit_module_to_string(&result.module).unwrap();

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
    fn bracket_notation_for_non_ident_fields() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Dynamic,
            };
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Void,
            };
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
        assert!(out.contains("while (true)"), "Should have while loop:\n{out}");
    }

    #[test]
    fn emit_for_loop() {
        use reincarnate_core::ir::CmpKind;

        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void,
            };
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
        // For-loop emits as while(true) with init assigns before and
        // update assigns inside.
        assert!(out.contains("while (true)"), "Should have loop:\n{out}");
        // Init assigns header param v0 from inlined const 0.
        assert!(out.contains("v0 = 0;"), "Should have init assign:\n{out}");
        // Update assigns header param v0 from inlined v0 + 1.
        assert!(out.contains("v0 = v0 + 1;"), "Should have update assign:\n{out}");
    }

    #[test]
    fn emit_class_with_methods() {
        let mut mb = ModuleBuilder::new("test");

        // Struct for class fields.
        mb.add_struct(StructDef {
            name: "Phouka".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            fields: vec![("hp".into(), Type::Int(32))],
            visibility: Visibility::Public,
        });

        // Constructor: (this: dyn) -> void
        let ctor_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("Phouka::new", ctor_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Phouka".into(),
            MethodKind::Constructor,
        );
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        // Instance method: (this: dyn, amount: i32) -> void
        let method_sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Int(32)],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("Phouka::attack", method_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Phouka".into(),
            MethodKind::Instance,
        );
        let _this = fb.param(0);
        let _amount = fb.param(1);
        fb.ret(None);
        let method_id = mb.add_function(fb.build());

        // Static method: (amount: i32) -> i32
        let static_sig = FunctionSig {
            params: vec![Type::Int(32)],
            return_ty: Type::Int(32),
        };
        let mut fb = FunctionBuilder::new("Phouka::create", static_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Phouka".into(),
            MethodKind::Static,
        );
        let p = fb.param(0);
        fb.ret(Some(p));
        let static_id = mb.add_function(fb.build());

        // Getter: (this: dyn) -> i32
        let getter_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Int(32),
        };
        let mut fb = FunctionBuilder::new("Phouka::get_health", getter_sig, Visibility::Public);
        fb.set_class(
            vec!["classes".into(), "Scenes".into()],
            "Phouka".into(),
            MethodKind::Getter,
        );
        let this = fb.param(0);
        let hp = fb.get_field(this, "hp", Type::Int(32));
        fb.ret(Some(hp));
        let getter_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "Phouka".into(),
            namespace: vec!["classes".into(), "Scenes".into()],
            struct_index: 0,
            methods: vec![ctor_id, method_id, static_id, getter_id],
            super_class: Some("Object".into()),
            visibility: Visibility::Public,
        });

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

        // Class declaration with extends.
        assert!(
            out.contains("export class Phouka extends Object {"),
            "Should have class decl:\n{out}"
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
            return_ty: Type::Void,
        };
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
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);
        fb.ret(None);
        mb.add_function(fb.build());

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

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
            return_ty: Type::Void,
        };
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

        let module = mb.build();
        emit_module_to_dir(&module, dir.path()).unwrap();

        // Check nested file exists.
        let class_file = dir
            .path()
            .join("frame1/classes/Scenes/Swamp.ts");
        assert!(class_file.exists(), "Nested class file should exist");

        let content = fs::read_to_string(&class_file).unwrap();

        // Runtime import should go up 2 levels (depth = 2 namespace segments).
        assert!(
            content.contains("from \"../../runtime\""),
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
            return_ty: Type::Void,
        };
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

        let module = mb.build();
        emit_module_to_dir(&module, dir.path()).unwrap();

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
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("Child::new", sig, Visibility::Public);
        fb.set_class(Vec::new(), "Child".into(), MethodKind::Constructor);
        let this = fb.param(0);
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

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

        assert!(
            out.contains("super();"),
            "constructSuper should emit super():\n{out}"
        );
        assert!(
            !out.contains("constructSuper"),
            "Should not have raw constructSuper call:\n{out}"
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
            return_ty: Type::Void,
        };
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
            return_ty: Type::Void,
        };
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

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

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
                return_ty: Type::Dynamic,
            };
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
                return_ty: Type::Dynamic,
            };
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
                return_ty: Type::Void,
            };
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
            return_ty: Type::Dynamic,
        };
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

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

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
            return_ty: Type::Dynamic,
        };
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

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

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
                return_ty: Type::Dynamic,
            };
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
            return_ty: Type::Dynamic,
        };
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

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

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
                return_ty: Type::Dynamic,
            };
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
                return_ty: Type::Void,
            };
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
            return_ty: Type::Void,
        };
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

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

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
                return_ty: Type::Dynamic,
            };
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
                return_ty: Type::Int(64),
            };
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Int(64),
            };
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
                return_ty: Type::Dynamic,
            };
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
                return_ty: Type::Void,
            };
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
            return_ty: Type::Bool,
        };
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
            return_ty: Type::Bool,
        };
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

        let module = mb.build();
        let out = emit_module_to_string(&module).unwrap();

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
                return_ty: Type::Bool,
            };
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
                return_ty: Type::Bool,
            };
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
                return_ty: Type::Bool,
            };
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
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Dynamic],
                return_ty: Type::Bool,
            };
            let mut fb = FunctionBuilder::new("test_fn", sig, Visibility::Public);
            let x = fb.param(0);
            let casted = fb.cast(x, Type::Bool);
            // Use the cast result twice to force a variable binding.
            let _sum = fb.add(casted, casted);
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Void,
            };
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
                return_ty: Type::Void,
            };
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

}
