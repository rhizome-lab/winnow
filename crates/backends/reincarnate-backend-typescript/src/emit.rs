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

    emit_runtime_imports(module, &mut out);
    emit_imports(module, &mut out);
    emit_structs(module, &mut out);
    emit_enums(module, &mut out);
    emit_globals(module, &mut out);

    if module.classes.is_empty() {
        emit_functions(module, &mut out)?;
    } else {
        let (class_groups, free_funcs) = group_by_class(module);
        for group in &class_groups {
            emit_class(group, module, &mut out)?;
        }
        for (_fid, func) in &free_funcs {
            emit_function(func, &mut out)?;
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
        emit_runtime_imports_at_depth(module, &mut out, depth);
        emit_intra_imports(group, &segments, &registry, &mut out);
        emit_class(group, module, &mut out)?;

        let path = file_dir.join(format!("{short_name}.ts"));
        fs::write(&path, &out).map_err(CoreError::Io)?;

        // Barrel export path: relative from module_dir.
        let export_path = segments.join("/");
        barrel_exports.push(export_path);
    }

    // Free functions → _init.ts (at module root, depth 0).
    if !free_funcs.is_empty() {
        let mut out = String::new();
        emit_runtime_imports_at_depth(module, &mut out, 0);
        emit_imports(module, &mut out);
        emit_globals(module, &mut out);
        for (_fid, func) in &free_funcs {
            emit_function(func, &mut out)?;
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

/// Per-function context for inline `let` declarations.
///
/// Copy propagation and alias resolution are handled by the `Mem2Reg` IR
/// transform pass, so the emitter only needs to track which values get a
/// `let` keyword at their definition site.
struct EmitCtx {
    /// Values that need a `let` keyword when first emitted.
    needs_let: HashSet<ValueId>,
}

impl EmitCtx {
    fn for_function(func: &Function) -> Self {
        let needs_let = compute_needs_let(func);
        Self { needs_let }
    }

    /// Format a value reference.
    fn val(&self, v: ValueId) -> String {
        format!("v{}", v.index())
    }

    /// Returns `"let "` for instruction results, `""` for parameters.
    fn let_prefix(&self, v: ValueId) -> &'static str {
        if self.needs_let.contains(&v) {
            "let "
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

fn collect_system_names(module: &Module) -> BTreeSet<String> {
    let mut used = BTreeSet::new();
    for (_id, func) in module.functions.iter() {
        for (_inst_id, inst) in func.insts.iter() {
            if let Op::SystemCall { system, .. } = &inst.op {
                used.insert(system.clone());
            }
        }
    }
    used
}

fn emit_runtime_imports(module: &Module, out: &mut String) {
    emit_runtime_imports_at_depth(module, out, 0);
}

/// Emit runtime imports with path prefix adjusted for directory depth.
///
/// `depth` is the number of directories below the module dir. From depth `d`,
/// the runtime path is `"../".repeat(d + 1) + "runtime"` (the `+1` accounts
/// for the module dir being one level inside `output_dir`).
fn emit_runtime_imports_at_depth(module: &Module, out: &mut String, depth: usize) {
    let systems = collect_system_names(module);
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

/// Collect type names referenced by a class group that exist in the registry.
fn collect_class_references(group: &ClassGroup<'_>, registry: &ClassRegistry) -> BTreeSet<String> {
    let self_name = &group.class_def.name;
    let mut refs = BTreeSet::new();

    // Super class reference.
    if let Some(sc) = &group.class_def.super_class {
        let short = sc.rsplit("::").next().unwrap_or(sc);
        if short != self_name {
            if let Some(entry) = registry.lookup(sc) {
                refs.insert(entry.short_name.clone());
            }
        }
    }

    // Struct fields (class instance fields).
    for (_name, ty) in &group.struct_def.fields {
        collect_type_ref(ty, self_name, registry, &mut refs);
    }

    // Scan all method bodies for type references.
    for (_fid, func) in &group.methods {
        collect_type_refs_from_function(func, self_name, registry, &mut refs);
    }

    refs
}

/// Scan a function's instructions and signature for type references.
fn collect_type_refs_from_function(
    func: &Function,
    self_name: &str,
    registry: &ClassRegistry,
    refs: &mut BTreeSet<String>,
) {
    // Check return type and param types.
    collect_type_ref(&func.sig.return_ty, self_name, registry, refs);
    for ty in &func.sig.params {
        collect_type_ref(ty, self_name, registry, refs);
    }

    // Check instructions.
    for (_inst_id, inst) in func.insts.iter() {
        match &inst.op {
            Op::Alloc(ty) | Op::Cast(_, ty) | Op::TypeCheck(_, ty) => {
                collect_type_ref(ty, self_name, registry, refs);
            }
            _ => {}
        }
    }

    // Check value_types for Struct/Enum references.
    for (_vid, ty) in func.value_types.iter() {
        collect_type_ref(ty, self_name, registry, refs);
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
        _ => {}
    }
}

/// Emit `import { X } from "..."` statements for intra-module class references.
fn emit_intra_imports(
    group: &ClassGroup<'_>,
    source_segments: &[String],
    registry: &ClassRegistry,
    out: &mut String,
) {
    let needed = collect_class_references(group, registry);
    if needed.is_empty() {
        return;
    }

    for short_name in &needed {
        if let Some(entry) = registry.classes.get(short_name) {
            let rel = relative_import_path(source_segments, &entry.path_segments);
            let _ = writeln!(out, "import {{ {short_name} }} from \"{rel}\";");
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

fn emit_functions(module: &Module, out: &mut String) -> Result<(), CoreError> {
    for (_id, func) in module.functions.iter() {
        emit_function(func, out)?;
    }
    Ok(())
}

fn emit_function(func: &Function, out: &mut String) -> Result<(), CoreError> {
    let ctx = EmitCtx::for_function(func);
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
        emit_block_body(&ctx, func, func.entry, &func.blocks[func.entry], out, "  ")?;
    } else {
        // Pre-declare only non-entry block parameters.
        emit_block_param_declarations(&ctx, func, out);

        let shape = structurize::structurize(func);
        emit_shape(&ctx, func, &shape, out, "  ")?;
    }

    let _ = writeln!(out, "}}\n");
    Ok(())
}

/// Emit a structured shape tree as TypeScript.
fn emit_shape(
    ctx: &EmitCtx,
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
            else_assigns,
            else_body,
        } => {
            // Emit the block's non-terminator instructions first.
            emit_block_instructions(ctx, func, *block, out, indent)?;

            let _ = writeln!(out, "{indent}if ({}) {{", ctx.val(*cond));
            let inner = format!("{indent}  ");
            emit_arg_assigns(ctx, then_assigns, out, &inner);
            emit_shape(ctx, func, then_body, out, &inner)?;
            let _ = writeln!(out, "{indent}}} else {{");
            emit_arg_assigns(ctx, else_assigns, out, &inner);
            emit_shape(ctx, func, else_body, out, &inner)?;
            let _ = writeln!(out, "{indent}}}");
        }

        Shape::WhileLoop {
            header,
            cond,
            cond_negated,
            body,
        } => {
            let cond_expr = if *cond_negated {
                format!("!{}", ctx.val(*cond))
            } else {
                ctx.val(*cond)
            };
            let _ = writeln!(out, "{indent}while (true) {{");
            let inner = format!("{indent}  ");
            emit_block_instructions(ctx, func, *header, out, &inner)?;
            let _ = writeln!(out, "{inner}if (!{cond_expr}) break;");
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
            let cond_expr = if *cond_negated {
                format!("!{}", ctx.val(*cond))
            } else {
                ctx.val(*cond)
            };

            // Emit init assignments before the loop.
            emit_arg_assigns(ctx, init_assigns, out, indent);

            let _ = writeln!(out, "{indent}while (true) {{");
            let inner = format!("{indent}  ");
            emit_block_instructions(ctx, func, *header, out, &inner)?;
            let _ = writeln!(out, "{inner}if (!{cond_expr}) break;");
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
    ctx: &EmitCtx,
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

/// Emit a block's non-terminator instructions.
///
/// Skips Br, BrIf, and Switch (handled by the shape tree). Stops after
/// encountering the first terminator — any instructions after it are dead
/// code from the frontend and should not be emitted. Return is kept since
/// it produces output (`return ...;`).
fn emit_block_instructions(
    ctx: &EmitCtx,
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
    ctx: &EmitCtx,
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

fn emit_inst(
    ctx: &EmitCtx,
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
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(out, "{indent}{pfx}{} = {};", ctx.val(r), emit_constant(c));
        }

        // -- Arithmetic --
        Op::Add(a, b) => emit_binop(ctx, out, indent, result, a, b, "+"),
        Op::Sub(a, b) => emit_binop(ctx, out, indent, result, a, b, "-"),
        Op::Mul(a, b) => emit_binop(ctx, out, indent, result, a, b, "*"),
        Op::Div(a, b) => emit_binop(ctx, out, indent, result, a, b, "/"),
        Op::Rem(a, b) => emit_binop(ctx, out, indent, result, a, b, "%"),
        Op::Neg(a) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(out, "{indent}{pfx}{} = -{};", ctx.val(r), ctx.val(*a));
        }

        // -- Bitwise --
        Op::BitAnd(a, b) => emit_binop(ctx, out, indent, result, a, b, "&"),
        Op::BitOr(a, b) => emit_binop(ctx, out, indent, result, a, b, "|"),
        Op::BitXor(a, b) => emit_binop(ctx, out, indent, result, a, b, "^"),
        Op::BitNot(a) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(out, "{indent}{pfx}{} = ~{};", ctx.val(r), ctx.val(*a));
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
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(out, "{indent}{pfx}{} = !{};", ctx.val(r), ctx.val(*a));
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
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = undefined as unknown as {};",
                ctx.val(r),
                ts_type(ty)
            );
        }
        Op::Load(ptr) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(out, "{indent}{pfx}{} = {};", ctx.val(r), ctx.val(*ptr));
        }
        Op::Store { ptr, value } => {
            let _ = writeln!(out, "{indent}{} = {};", ctx.val(*ptr), ctx.val(*value));
        }
        Op::GetField { object, field } => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            if is_valid_js_ident(field) {
                let _ = writeln!(
                    out,
                    "{indent}{pfx}{} = {}.{field};",
                    ctx.val(r),
                    ctx.val(*object)
                );
            } else {
                let _ = writeln!(
                    out,
                    "{indent}{pfx}{} = {}[\"{}\"];",
                    ctx.val(r),
                    ctx.val(*object),
                    escape_js_string(field)
                );
            }
        }
        Op::SetField {
            object,
            field,
            value,
        } => {
            if is_valid_js_ident(field) {
                let _ = writeln!(
                    out,
                    "{indent}{}.{field} = {};",
                    ctx.val(*object),
                    ctx.val(*value)
                );
            } else {
                let _ = writeln!(
                    out,
                    "{indent}{}[\"{}\"] = {};",
                    ctx.val(*object),
                    escape_js_string(field),
                    ctx.val(*value)
                );
            }
        }
        Op::GetIndex { collection, index } => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = {}[{}];",
                ctx.val(r),
                ctx.val(*collection),
                ctx.val(*index)
            );
        }
        Op::SetIndex {
            collection,
            index,
            value,
        } => {
            let _ = writeln!(
                out,
                "{indent}{}[{}] = {};",
                ctx.val(*collection),
                ctx.val(*index),
                ctx.val(*value)
            );
        }

        // -- Calls --
        Op::Call { func: fname, args } => {
            let args_str = args
                .iter()
                .map(|a| ctx.val(*a))
                .collect::<Vec<_>>()
                .join(", ");
            let safe_name = sanitize_ident(fname);
            if let Some(r) = result {
                let pfx = ctx.let_prefix(r);
                let _ = writeln!(
                    out,
                    "{indent}{pfx}{} = {safe_name}({args_str});",
                    ctx.val(r)
                );
            } else {
                let _ = writeln!(out, "{indent}{safe_name}({args_str});");
            }
        }
        Op::CallIndirect { callee, args } => {
            let args_str = args
                .iter()
                .map(|a| ctx.val(*a))
                .collect::<Vec<_>>()
                .join(", ");
            if let Some(r) = result {
                let pfx = ctx.let_prefix(r);
                let _ = writeln!(
                    out,
                    "{indent}{pfx}{} = {}({args_str});",
                    ctx.val(r),
                    ctx.val(*callee)
                );
            } else {
                let _ = writeln!(out, "{indent}{}({args_str});", ctx.val(*callee));
            }
        }
        Op::SystemCall {
            system,
            method,
            args,
        } => {
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
            if let Some(r) = result {
                let pfx = ctx.let_prefix(r);
                let _ = writeln!(
                    out,
                    "{indent}{pfx}{} = {sys_ident}{safe_method}({args_str});",
                    ctx.val(r)
                );
            } else {
                let _ = writeln!(
                    out,
                    "{indent}{sys_ident}{safe_method}({args_str});",
                );
            }
        }

        // -- Type operations --
        Op::Cast(v, ty) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = {} as {};",
                ctx.val(r),
                ctx.val(*v),
                ts_type(ty)
            );
        }
        Op::TypeCheck(v, ty) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let check = type_check_expr(ctx, *v, ty);
            let _ = writeln!(out, "{indent}{pfx}{} = {check};", ctx.val(r));
        }

        // -- Aggregate construction --
        Op::StructInit { name: _, fields } => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
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
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = {{ {} }};",
                ctx.val(r),
                field_strs.join(", ")
            );
        }
        Op::ArrayInit(elems) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let elems_str = elems
                .iter()
                .map(|v| ctx.val(*v))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(out, "{indent}{pfx}{} = [{elems_str}];", ctx.val(r));
        }
        Op::TupleInit(elems) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let elems_str = elems
                .iter()
                .map(|v| ctx.val(*v))
                .collect::<Vec<_>>()
                .join(", ");
            let ty_str = ts_type(&func.value_types[r]);
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = [{elems_str}] as {ty_str};",
                ctx.val(r)
            );
        }

        // -- Coroutines --
        Op::Yield(v) => {
            if let Some(r) = result {
                let pfx = ctx.let_prefix(r);
                match v {
                    Some(yv) => {
                        let _ = writeln!(
                            out,
                            "{indent}{pfx}{} = yield {};",
                            ctx.val(r),
                            ctx.val(*yv)
                        );
                    }
                    None => {
                        let _ = writeln!(out, "{indent}{pfx}{} = yield;", ctx.val(r));
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
            let pfx = ctx.let_prefix(r);
            let args_str = args
                .iter()
                .map(|a| ctx.val(*a))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = {}({args_str});",
                ctx.val(r),
                sanitize_ident(fname)
            );
        }
        Op::CoroutineResume(v) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = {}.next();",
                ctx.val(r),
                ctx.val(*v)
            );
        }

        // -- Misc --
        Op::GlobalRef(name) => {
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(
                out,
                "{indent}{pfx}{} = {};",
                ctx.val(r),
                sanitize_ident(name)
            );
        }
        Op::Copy(src) => {
            // Not skipped (multi-use or other reason).
            let r = result.unwrap();
            let pfx = ctx.let_prefix(r);
            let _ = writeln!(out, "{indent}{pfx}{} = {};", ctx.val(r), ctx.val(*src));
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

    for (i, &(_fid, func)) in sorted_methods.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        emit_class_method(func, out)?;
    }

    let _ = writeln!(out, "}}\n");
    Ok(())
}

/// Emit a single method inside a class body.
fn emit_class_method(func: &Function, out: &mut String) -> Result<(), CoreError> {
    let ctx = EmitCtx::for_function(func);

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
        emit_block_body(&ctx, func, func.entry, &func.blocks[func.entry], out, "    ")?;
    } else {
        emit_block_param_declarations_indented(&ctx, func, out, "    ");
        let shape = structurize::structurize(func);
        emit_shape(&ctx, func, &shape, out, "    ")?;
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
    ctx: &EmitCtx,
    out: &mut String,
    indent: &str,
    result: Option<ValueId>,
    a: &ValueId,
    b: &ValueId,
    op: &str,
) {
    let r = result.unwrap();
    let pfx = ctx.let_prefix(r);
    let _ = writeln!(
        out,
        "{indent}{pfx}{} = {} {op} {};",
        ctx.val(r),
        ctx.val(*a),
        ctx.val(*b)
    );
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

/// Generate a TypeScript type-check expression.
fn type_check_expr(ctx: &EmitCtx, v: ValueId, ty: &Type) -> String {
    match ty {
        Type::Bool => format!("typeof {} === \"boolean\"", ctx.val(v)),
        Type::Int(_) | Type::UInt(_) | Type::Float(_) => {
            format!("typeof {} === \"number\"", ctx.val(v))
        }
        Type::String => format!("typeof {} === \"string\"", ctx.val(v)),
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            format!("{} instanceof {}", ctx.val(v), sanitize_ident(short))
        }
        _ => format!("typeof {} === \"object\"", ctx.val(v)),
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
        assert!(out.contains("let v2 = v0 + v1;"));
        assert!(out.contains("return v2;"));
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
        assert!(out.contains("renderer.clear(v0, v1);"));
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

        assert!(out.contains("let v0 = null;"));
        assert!(out.contains("let v1 = true;"));
        assert!(out.contains("let v2 = false;"));
        assert!(out.contains("let v3 = 42;"));
        assert!(out.contains("let v4 = 3.125;"));
        assert!(out.contains(r#"let v5 = "hello \"world\"\nnewline";"#));
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

        assert!(out.contains("let v2 = [v0, v1];"));
        assert!(out.contains("let v5 = { x: v3, y: v4 };"));
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
            let result = fb.get_field(obj, "flash.display::Loader", Type::Dynamic);
            fb.ret(Some(result));
            mb.add_function(fb.build());
        });

        // Non-ident field name should use bracket notation.
        assert!(out.contains("[\"flash.display::Loader\"]"));
        assert!(!out.contains(".flash.display::Loader"));
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

        // Should have if/else, no dispatch loop.
        assert!(!out.contains("$block"), "Should not use dispatch loop:\n{out}");
        assert!(out.contains("if (v0)"), "Should have if/else:\n{out}");
        assert!(out.contains("} else {"), "Should have else branch:\n{out}");
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
        // Init assigns header param v0 from const v1 (which is 0).
        assert!(out.contains("v0 = v1;"), "Should have init assign:\n{out}");
        // Update assigns header param v0 from computed v5 (v0 + 1).
        assert!(out.contains("v0 = v5;"), "Should have update assign:\n{out}");
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
        // Getter — strips `get_` prefix.
        assert!(
            out.contains("  get health(): number {"),
            "Should have getter:\n{out}"
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
}
