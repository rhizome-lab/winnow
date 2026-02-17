//! Generate project scaffold files: index.html, tsconfig.json, and main entry point.

use std::collections::BTreeMap;
use std::fmt::Write;
use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{EntryPoint, FuncId, MethodKind, Module, Visibility};
use reincarnate_core::project::{AssetCatalog, AssetKind, PersistenceConfig, RuntimeConfig};

use crate::emit::sanitize_ident;

/// Write all scaffold files into `output_dir`.
pub fn emit_scaffold(modules: &[Module], output_dir: &Path, runtime_config: Option<&RuntimeConfig>, assets: &AssetCatalog, persistence: Option<&PersistenceConfig>) -> Result<(), CoreError> {
    let html = if is_twine(modules) {
        generate_twine_index_html(modules, assets)
    } else {
        generate_index_html(modules)
    };
    fs::write(output_dir.join("index.html"), html)?;
    fs::write(output_dir.join("tsconfig.json"), TSCONFIG)?;
    fs::write(output_dir.join("main.ts"), generate_main(modules, runtime_config, persistence))?;
    fs::write(output_dir.join("package.json"), generate_package_json(runtime_config))?;
    Ok(())
}

/// Detect Twine projects by the presence of passage_names in any module.
fn is_twine(modules: &[Module]) -> bool {
    modules.iter().any(|m| !m.passage_names.is_empty())
}

fn generate_index_html(modules: &[Module]) -> String {
    let title = modules
        .first()
        .map(|m| m.name.as_str())
        .unwrap_or("Reincarnate App");

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{title}</title>
  <style>
    body {{
      margin: 0;
      background: #000;
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
    }}
    canvas {{
      image-rendering: pixelated;
    }}
  </style>
</head>
<body>
  <canvas id="reincarnate-canvas" width="800" height="600"></canvas>
  <script type="module" src="./dist/bundle.js"></script>
</body>
</html>
"#
    )
}

fn generate_twine_index_html(modules: &[Module], assets: &AssetCatalog) -> String {
    let title = modules
        .first()
        .map(|m| m.name.as_str())
        .unwrap_or("Reincarnate App");

    // Detect Harlowe by presence of format CSS asset (SugarCube has none)
    let is_harlowe = assets
        .assets
        .iter()
        .any(|a| a.id.starts_with("format_css:"));

    // Collect stylesheet asset paths for <link> tags
    let mut style_links = String::new();
    for asset in &assets.assets {
        if asset.kind == AssetKind::Stylesheet && !asset.data.is_empty() {
            let path = asset.path.display();
            let _ = writeln!(style_links, "  <link rel=\"stylesheet\" href=\"./{path}\">");
        }
    }

    if is_harlowe {
        // Harlowe: minimal reset, format CSS handles all styling via <tw-story>
        format!(
            r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{title}</title>
  <style>
    * {{ box-sizing: border-box; }}
    body {{ margin: 0; }}
  </style>
{style_links}</head>
<body>
  <tw-story></tw-story>
  <script type="module" src="./dist/bundle.js"></script>
</body>
</html>
"#
        )
    } else {
        // SugarCube: generic dark theme with #passages container
        format!(
            r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{title}</title>
  <style>
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      padding: 2em;
      background: #111;
      color: #eee;
      font-family: Georgia, "Times New Roman", serif;
      font-size: 18px;
      line-height: 1.6;
      max-width: 54em;
      margin-left: auto;
      margin-right: auto;
    }}
    #passages {{
      min-height: 50vh;
    }}
    #passages a {{
      color: #4ea6ca;
      cursor: pointer;
      text-decoration: none;
    }}
    #passages a:hover {{
      color: #8cc8e0;
      text-decoration: underline;
    }}
    input, textarea, select, button {{
      font-family: inherit;
      font-size: inherit;
      color: #eee;
      background: #222;
      border: 1px solid #555;
      padding: 0.3em 0.6em;
      border-radius: 3px;
    }}
    button {{
      cursor: pointer;
    }}
    button:hover {{
      background: #333;
    }}
  </style>
{style_links}</head>
<body>
  <div id="passages"></div>
  <script type="module" src="./dist/bundle.js"></script>
</body>
</html>
"#
        )
    }
}

/// Build passage map entries: `"Name": func_name` for each passage.
fn build_passage_map(modules: &[Module]) -> String {
    let mut entries = Vec::new();
    for module in modules {
        for (display_name, func_name) in &module.passage_names {
            let escaped = display_name.replace('\\', "\\\\").replace('"', "\\\"");
            entries.push(format!("  \"{escaped}\": {}", sanitize_ident(func_name)));
        }
    }
    if entries.is_empty() {
        "{}".to_string()
    } else {
        format!("{{\n{}\n}}", entries.join(",\n"))
    }
}

/// Build passage tag map: `{ "Name": ["tag1", "tag2"], ... }`.
fn build_passage_tags(modules: &[Module]) -> String {
    let mut entries = Vec::new();
    for module in modules {
        for (display_name, tags) in &module.passage_tags {
            let escaped = display_name.replace('\\', "\\\\").replace('"', "\\\"");
            let tag_list: Vec<String> = tags
                .iter()
                .map(|t| {
                    let t_escaped = t.replace('\\', "\\\\").replace('"', "\\\"");
                    format!("\"{t_escaped}\"")
                })
                .collect();
            entries.push(format!("  \"{escaped}\": [{}]", tag_list.join(", ")));
        }
    }
    if entries.is_empty() {
        "{}".to_string()
    } else {
        format!("{{\n{}\n}}", entries.join(",\n"))
    }
}

/// Build user script call statements (e.g. `__user_script_0();`).
///
/// Detects functions named `__user_script_*` in the module and emits
/// direct calls so they run before the story starts.
fn build_user_script_calls(modules: &[Module]) -> String {
    let mut calls = Vec::new();
    for module in modules {
        for func in module.functions.values() {
            if func.name.starts_with("__user_script_") && func.visibility == Visibility::Public {
                calls.push(format!("{}(_rt);", sanitize_ident(&func.name)));
            }
        }
    }
    calls.join("\n")
}

/// Find the root class name from `EntryPoint::ConstructClass` metadata.
///
/// Returns the sanitized short class name (e.g. "CoC") or an empty string
/// if no module has a ConstructClass entry point.
fn find_root_class(modules: &[Module]) -> String {
    for module in modules {
        if let Some(EntryPoint::ConstructClass(fqn)) = &module.entry_point {
            return resolve_class_short_name(modules, fqn);
        }
    }
    String::new()
}

/// Find the start passage display name from the entry point metadata.
///
/// Looks up the `EntryPoint::CallFunction` FuncId in the module's
/// `passage_names` to find the display name (e.g. "Start").
fn find_start_passage(modules: &[Module]) -> String {
    for module in modules {
        if let Some(EntryPoint::CallFunction(fid)) = &module.entry_point {
            if let Some(func) = module.functions.get(*fid) {
                // Reverse-lookup: find passage name whose func_name matches
                for (display_name, func_name) in &module.passage_names {
                    if *func_name == func.name {
                        return format!("\"{}\"", display_name.replace('\\', "\\\\").replace('"', "\\\""));
                    }
                }
            }
        }
    }
    // Default to "Start" if no entry point found
    "\"Start\"".to_string()
}

const TSCONFIG: &str = r#"{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ES2020",
    "moduleResolution": "bundler",
    "strict": true,
    "esModuleInterop": true,
    "outDir": "dist",
    "rootDir": ".",
    "lib": ["ES2020", "DOM", "DOM.Iterable"]
  },
  "include": ["**/*.ts"]
}
"#;

/// Schema for the generated `package.json`.
#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct PackageJson {
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    private: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    license: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    r#type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    main: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    scripts: Option<PackageScripts>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    dependencies: BTreeMap<String, String>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    dev_dependencies: BTreeMap<String, String>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    peer_dependencies: BTreeMap<String, String>,
}

#[derive(serde::Serialize)]
struct PackageScripts {
    #[serde(skip_serializing_if = "Option::is_none")]
    build: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    serve: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    dev: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    start: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    test: Option<String>,
}

/// Generate `package.json` from runtime config dependencies.
fn generate_package_json(runtime_config: Option<&RuntimeConfig>) -> String {
    let dependencies = runtime_config
        .map(|c| c.scaffold.dependencies.clone())
        .unwrap_or_default();

    let mut dev_dependencies = BTreeMap::new();
    dev_dependencies.insert("esbuild".into(), "^0.24.0".into());
    dev_dependencies.insert("typescript".into(), "^5.0.0".into());

    let pkg = PackageJson {
        name: None,
        version: None,
        private: Some(true),
        description: None,
        license: None,
        r#type: Some("module".into()),
        main: None,
        scripts: Some(PackageScripts {
            build: Some("esbuild main.ts --bundle --outfile=dist/bundle.js --format=esm".into()),
            serve: Some("esbuild main.ts --bundle --outfile=dist/bundle.js --format=esm --serve --servedir=.".into()),
            dev: None,
            start: None,
            test: None,
        }),
        dependencies,
        dev_dependencies,
        peer_dependencies: BTreeMap::new(),
    };

    let mut json = serde_json::to_string_pretty(&pkg).unwrap();
    json.push('\n');
    json
}

/// Generate `main.ts` — imports all modules and calls the entry point in a
/// requestAnimationFrame game loop.
fn generate_main(modules: &[Module], runtime_config: Option<&RuntimeConfig>, persistence: Option<&PersistenceConfig>) -> String {
    let mut out = String::new();

    let has_custom_entry = runtime_config
        .and_then(|c| c.scaffold.entry.as_deref())
        .is_some();

    // Only import timing for standard rAF loop entries.
    if !has_custom_entry {
        let _ = writeln!(out, "import {{ timing }} from \"./runtime\";");
    }
    if let Some(cfg) = runtime_config {
        for group in &cfg.scaffold.imports {
            let _ = writeln!(
                out,
                "import {{ {} }} from \"./runtime/{}\";",
                group.names.join(", "),
                group.path,
            );
        }
        for group in &cfg.scaffold.data_imports {
            let _ = writeln!(
                out,
                "import {{ {} }} from \"./{}\";",
                group.names.join(", "),
                group.path,
            );
        }
    }

    // Collect imports for all modules (same logic as before).
    let mut heuristic_entry: Option<String> = None;
    let mut class_names: Vec<String> = Vec::new();
    for module in modules {
        emit_module_imports(module, &mut out, &mut heuristic_entry);
        // Collect all public class names for the {classes} placeholder.
        for class in &module.classes {
            if class.visibility == Visibility::Public {
                class_names.push(sanitize_ident(&class.name));
            }
        }
    }

    out.push('\n');

    // If the scaffold has a custom entry template, use it.
    if let Some(entry) = runtime_config.and_then(|c| c.scaffold.entry.as_deref()) {
        let class_list = class_names.join(", ");
        let room_creation_code = build_room_creation_code_array(modules);
        let passage_map = build_passage_map(modules);
        let passage_tags = build_passage_tags(modules);
        let user_scripts = build_user_script_calls(modules);
        let start_passage = find_start_passage(modules);
        let root_class = find_root_class(modules);
        let persistence_json = persistence
            .map(|p| serde_json::to_string(p).unwrap_or_else(|_| "{}".into()))
            .unwrap_or_else(|| "undefined".into());
        let expanded = entry
            .replace("{classes}", &class_list)
            .replace("{roomCreationCode}", &room_creation_code)
            .replace("{passages}", &passage_map)
            .replace("{passageTags}", &passage_tags)
            .replace("{userScripts}", &user_scripts)
            .replace("{startPassage}", &start_passage)
            .replace("{rootClass}", &root_class)
            .replace("{persistence}", &persistence_json);
        let _ = writeln!(out, "{}", expanded);
    } else if let Some(code) = metadata_entry_code(modules, runtime_config) {
        // Prefer metadata-based entry point over heuristic.
        out.push_str(&code);
    } else if let Some(func_name) = heuristic_entry {
        emit_game_loop(&mut out, &func_name, runtime_config);
    } else {
        let _ = writeln!(
            out,
            "// No entry point detected. Module-level code will run on import."
        );
    }

    out
}

/// Emit import statements for a single module, collecting heuristic entry
/// point candidates along the way.
fn emit_module_imports(module: &Module, out: &mut String, heuristic_entry: &mut Option<String>) {
    if module.classes.is_empty() {
        // Flat module — import public functions directly.
        let public_funcs: Vec<_> = module
            .functions
            .values()
            .filter(|f| f.visibility == Visibility::Public)
            .collect();
        if public_funcs.is_empty() {
            return;
        }
        let sanitized: Vec<String> = public_funcs
            .iter()
            .map(|f| sanitize_ident(&f.name))
            .collect();
        let _ = writeln!(
            out,
            "import {{ {} }} from \"./{}\";",
            sanitized.join(", "),
            module.name,
        );
        if heuristic_entry.is_none() {
            for func in &public_funcs {
                if is_entry_candidate(&func.name) {
                    *heuristic_entry = Some(sanitize_ident(&func.name));
                    break;
                }
            }
        }
    } else {
        // Class-based module — import from barrel file.
        let mut imports = Vec::new();
        for class in &module.classes {
            if class.visibility == Visibility::Public {
                imports.push(sanitize_ident(&class.name));
            }
        }
        // Also import free (non-class) public functions.
        let class_methods: std::collections::HashSet<_> = module
            .classes
            .iter()
            .flat_map(|c| c.methods.iter().copied())
            .collect();
        for (fid, func) in module.functions.iter() {
            if !class_methods.contains(&fid) && func.visibility == Visibility::Public {
                imports.push(sanitize_ident(&func.name));
                if heuristic_entry.is_none() && is_entry_candidate(&func.name) {
                    *heuristic_entry = Some(sanitize_ident(&func.name));
                }
            }
        }
        if !imports.is_empty() {
            let _ = writeln!(
                out,
                "import {{ {} }} from \"./{}\";",
                imports.join(", "),
                module.name,
            );
        }
        // Detect entry points in class methods (e.g., Main::init).
        if heuristic_entry.is_none() {
            for class in &module.classes {
                for &fid in &class.methods {
                    if let Some(func) = module.functions.get(fid) {
                        if func.method_kind == MethodKind::Static
                            && is_entry_candidate(
                                func.name.rsplit("::").next().unwrap_or(&func.name),
                            )
                        {
                            let class_name = sanitize_ident(&class.name);
                            let method_name = sanitize_ident(
                                func.name.rsplit("::").next().unwrap_or(&func.name),
                            );
                            *heuristic_entry = Some(format!("{class_name}.{method_name}"));
                            break;
                        }
                    }
                }
                if heuristic_entry.is_some() {
                    break;
                }
            }
        }
    }
}

/// Build entry-point code from module metadata (entry_point).
/// Returns `None` if no module has metadata, falling back to heuristic.
fn metadata_entry_code(modules: &[Module], runtime_config: Option<&RuntimeConfig>) -> Option<String> {
    // Find the first module with entry point metadata.
    let module = modules.iter().find(|m| m.entry_point.is_some())?;

    let mut code = String::new();

    // Emit entry point.
    match module.entry_point.as_ref()? {
        EntryPoint::ConstructClass(fqn) => {
            let ident = resolve_class_short_name(modules, fqn);
            if let Some(func) = runtime_config.and_then(|c| c.scaffold.construct_class_fn.as_deref()) {
                let _ = writeln!(code, "const app = {func}({ident});");
            } else {
                let _ = writeln!(code, "const app = new {ident}();");
            }
            if let Some(init) = runtime_config.and_then(|c| c.scaffold.construct_class_init.as_deref()) {
                let _ = writeln!(code, "{init}");
            }
        }
        EntryPoint::CallFunction(fid) => {
            if let Some(name) = func_call_name(module, *fid) {
                let _ = writeln!(code, "{name}();");
            }
        }
    }

    // Frame loop.
    let _ = writeln!(code, "\nfunction loop() {{");
    let _ = writeln!(code, "  timing.tick();");
    if let Some(tick) = runtime_config.and_then(|c| c.scaffold.tick.as_deref()) {
        let _ = writeln!(code, "  {tick}");
    }
    let _ = writeln!(code, "  requestAnimationFrame(loop);");
    let _ = writeln!(code, "}}");
    let _ = writeln!(code);
    let _ = writeln!(code, "requestAnimationFrame(loop);");

    Some(code)
}

/// Resolve a fully-qualified class name (e.g. `classes.CoC`) to the short
/// class name from the module's ClassDef entries (e.g. `CoC`).
fn resolve_class_short_name(modules: &[Module], fqn: &str) -> String {
    // Split FQN into segments: "classes.CoC" → ["classes", "CoC"]
    let segments: Vec<&str> = fqn.split('.').collect();
    let short = segments.last().copied().unwrap_or(fqn);

    // Try to match against ClassDef entries for precision.
    for module in modules {
        for class in &module.classes {
            // Match by short name + namespace.
            if class.name == short {
                let fqn_from_class = if class.namespace.is_empty() {
                    class.name.clone()
                } else {
                    format!("{}.{}", class.namespace.join("."), class.name)
                };
                if fqn_from_class == fqn {
                    return sanitize_ident(&class.name);
                }
            }
        }
    }
    // Fallback: just use the last segment.
    sanitize_ident(short)
}

/// Get the sanitized call expression for a function by FuncId.
fn func_call_name(module: &Module, fid: FuncId) -> Option<String> {
    let func = module.functions.get(fid)?;
    Some(sanitize_ident(&func.name))
}

/// Emit the standard game loop calling `func_name` each frame.
fn emit_game_loop(out: &mut String, func_name: &str, runtime_config: Option<&RuntimeConfig>) {
    let _ = writeln!(out, "function loop() {{");
    let _ = writeln!(out, "  timing.tick();");
    if let Some(tick) = runtime_config.and_then(|c| c.scaffold.tick.as_deref()) {
        let _ = writeln!(out, "  {tick}");
    }
    let _ = writeln!(out, "  {func_name}();");
    let _ = writeln!(out, "  requestAnimationFrame(loop);");
    let _ = writeln!(out, "}}");
    let _ = writeln!(out);
    let _ = writeln!(out, "requestAnimationFrame(loop);");
}

/// Build a sparse array string from room_creation_code mappings across all modules.
///
/// E.g. `{0: "roomInitCreate", 3: "roomFooCreate"}` becomes
/// `"roomInitCreate, undefined, undefined, roomFooCreate"`.
fn build_room_creation_code_array(modules: &[Module]) -> String {
    // Merge all modules' room_creation_code maps (typically only one module).
    let mut merged = std::collections::BTreeMap::new();
    for module in modules {
        for (&idx, name) in &module.room_creation_code {
            merged.insert(idx, sanitize_ident(name));
        }
    }
    if merged.is_empty() {
        return String::new();
    }
    let max_idx = *merged.keys().next_back().unwrap();
    let mut entries = Vec::with_capacity(max_idx + 1);
    for i in 0..=max_idx {
        if let Some(name) = merged.get(&i) {
            entries.push(name.as_str());
        } else {
            entries.push("undefined");
        }
    }
    entries.join(", ")
}

fn is_entry_candidate(name: &str) -> bool {
    matches!(
        name,
        "main"
            | "init"
            | "start"
            | "run"
            | "update"
            | "tick"
            | "frame"
            | "enterFrame"
            | "onEnterFrame"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use reincarnate_core::ir::builder::{FunctionBuilder, ModuleBuilder};
    use reincarnate_core::ir::{FunctionSig, Type};

    #[test]
    fn main_with_entry_point() {
        let mut mb = ModuleBuilder::new("game");
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("update", sig.clone(), Visibility::Public);
        fb.ret(None);
        mb.add_function(fb.build());
        let mut fb2 = FunctionBuilder::new("helper", sig, Visibility::Private);
        fb2.ret(None);
        mb.add_function(fb2.build());
        let module = mb.build();

        let main = generate_main(&[module], None, None);
        assert!(main.contains("import { update } from \"./game\";"));
        assert!(main.contains("update();"));
        assert!(main.contains("requestAnimationFrame(loop);"));
        // Private function not imported.
        assert!(!main.contains("helper"));
    }

    #[test]
    fn main_no_entry_point() {
        let mut mb = ModuleBuilder::new("utils");
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("compute", sig, Visibility::Public);
        fb.ret(None);
        mb.add_function(fb.build());
        let module = mb.build();

        let main = generate_main(&[module], None, None);
        assert!(main.contains("import { compute } from \"./utils\";"));
        assert!(main.contains("No entry point detected"));
        assert!(!main.contains("requestAnimationFrame"));
    }

    #[test]
    fn main_with_class_entry_point() {
        use reincarnate_core::ir::{ClassDef, StructDef};

        let mut mb = ModuleBuilder::new("game");
        mb.add_struct(StructDef {
            name: "App".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("App::init", sig.clone(), Visibility::Public);
        fb.set_class(Vec::new(), "App".into(), MethodKind::Static);
        fb.ret(None);
        let init_id = mb.add_function(fb.build());

        let mut fb2 = FunctionBuilder::new("App::render", sig, Visibility::Public);
        fb2.set_class(Vec::new(), "App".into(), MethodKind::Instance);
        fb2.ret(None);
        let render_id = mb.add_function(fb2.build());

        mb.add_class(ClassDef {
            name: "App".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![init_id, render_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        let module = mb.build();
        let main = generate_main(&[module], None, None);
        assert!(
            main.contains("import { App } from \"./game\";"),
            "Should import class from barrel:\n{main}"
        );
        assert!(
            main.contains("App.init();"),
            "Should call static entry point:\n{main}"
        );
        assert!(
            main.contains("requestAnimationFrame(loop);"),
            "Should have game loop:\n{main}"
        );
    }

    #[test]
    fn main_with_construct_class_entry() {
        use reincarnate_core::ir::{ClassDef, EntryPoint, StructDef};

        let mut mb = ModuleBuilder::new("game");
        mb.add_struct(StructDef {
            name: "MyApp".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("MyApp::new", sig, Visibility::Public);
        fb.set_class(Vec::new(), "MyApp".into(), MethodKind::Constructor);
        fb.ret(None);
        let ctor_id = mb.add_function(fb.build());

        mb.add_class(ClassDef {
            name: "MyApp".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![ctor_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        mb.set_entry_point(EntryPoint::ConstructClass("MyApp".into()));
        let module = mb.build();

        let main = generate_main(&[module], None, None);
        assert!(
            main.contains("const app = new MyApp();"),
            "Should construct class:\n{main}"
        );
        assert!(
            main.contains("requestAnimationFrame(loop);"),
            "Should have frame loop:\n{main}"
        );
        assert!(
            !main.contains("No entry point detected"),
            "Should not fall back to heuristic:\n{main}"
        );
    }

    #[test]
    fn main_with_call_function_entry() {
        use reincarnate_core::ir::EntryPoint;

        let mut mb = ModuleBuilder::new("game");
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("start_game", sig, Visibility::Public);
        fb.ret(None);
        let fid = mb.add_function(fb.build());

        mb.set_entry_point(EntryPoint::CallFunction(fid));
        let module = mb.build();

        let main = generate_main(&[module], None, None);
        assert!(
            main.contains("start_game();"),
            "Should call function:\n{main}"
        );
        assert!(
            main.contains("requestAnimationFrame(loop);"),
            "Should have frame loop:\n{main}"
        );
    }

    #[test]
    fn main_with_construct_class_no_init_order() {
        use reincarnate_core::ir::{ClassDef, EntryPoint, StructDef};

        let mut mb = ModuleBuilder::new("game");
        mb.add_struct(StructDef {
            name: "App".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });

        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };

        let mut fb_ctor = FunctionBuilder::new("App::new", sig, Visibility::Public);
        fb_ctor.set_class(Vec::new(), "App".into(), MethodKind::Constructor);
        fb_ctor.ret(None);
        let ctor_id = mb.add_function(fb_ctor.build());

        mb.add_class(ClassDef {
            name: "App".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![ctor_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });

        mb.set_entry_point(EntryPoint::ConstructClass("App".into()));
        let module = mb.build();

        let main = generate_main(&[module], None, None);
        assert!(
            main.contains("const app = new App();"),
            "Should construct App:\n{main}"
        );
        assert!(
            !main.contains("script"),
            "Should have no script init calls:\n{main}"
        );
        assert!(
            main.contains("requestAnimationFrame(loop);"),
            "Should have frame loop:\n{main}"
        );
    }

    #[test]
    fn index_html_has_canvas() {
        let mb = ModuleBuilder::new("my_game");
        let module = mb.build();

        let html = generate_index_html(&[module]);
        assert!(html.contains("reincarnate-canvas"));
        assert!(html.contains("<title>my_game</title>"));
        assert!(html.contains("dist/bundle.js"));
    }
}
