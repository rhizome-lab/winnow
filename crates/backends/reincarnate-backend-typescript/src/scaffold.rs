//! Generate project scaffold files: index.html, tsconfig.json, and main entry point.

use std::fmt::Write;
use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{EntryPoint, FuncId, MethodKind, Module, Visibility};

use crate::emit::sanitize_ident;

/// Write all scaffold files into `output_dir`.
pub fn emit_scaffold(modules: &[Module], output_dir: &Path) -> Result<(), CoreError> {
    fs::write(
        output_dir.join("index.html"),
        generate_index_html(modules),
    )?;
    fs::write(output_dir.join("tsconfig.json"), TSCONFIG)?;
    fs::write(output_dir.join("main.ts"), generate_main(modules))?;
    fs::write(output_dir.join("package.json"), PACKAGE_JSON)?;
    Ok(())
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

const PACKAGE_JSON: &str = r#"{
  "private": true,
  "type": "module",
  "scripts": {
    "build": "esbuild main.ts --bundle --outfile=dist/bundle.js --format=esm",
    "serve": "esbuild main.ts --bundle --outdir=dist --format=esm --serve --servedir=."
  },
  "devDependencies": {
    "esbuild": "^0.24.0",
    "typescript": "^5.0.0"
  }
}
"#;

/// Generate `main.ts` — imports all modules and calls the entry point in a
/// requestAnimationFrame game loop.
fn generate_main(modules: &[Module]) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "import {{ timing }} from \"./runtime\";");

    // Collect imports for all modules (same logic as before).
    let mut heuristic_entry: Option<String> = None;
    for module in modules {
        emit_module_imports(module, &mut out, &mut heuristic_entry);
    }

    out.push('\n');

    // Prefer metadata-based entry point over heuristic.
    if let Some(code) = metadata_entry_code(modules) {
        out.push_str(&code);
    } else if let Some(func_name) = heuristic_entry {
        emit_game_loop(&mut out, &func_name);
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

/// Build entry-point code from module metadata (entry_point + init_order).
/// Returns `None` if no module has metadata, falling back to heuristic.
fn metadata_entry_code(modules: &[Module]) -> Option<String> {
    // Find the first module with entry point metadata.
    let module = modules.iter().find(|m| m.entry_point.is_some())?;

    let mut code = String::new();

    // Emit init_order calls (from any module that has them).
    for m in modules {
        for &fid in &m.init_order {
            if let Some(name) = func_call_name(m, fid) {
                let _ = writeln!(code, "{name}();");
            }
        }
    }

    // Emit entry point.
    match module.entry_point.as_ref()? {
        EntryPoint::ConstructClass(name) => {
            let ident = sanitize_ident(name);
            let _ = writeln!(code, "const app = new {ident}();");
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
    let _ = writeln!(code, "  requestAnimationFrame(loop);");
    let _ = writeln!(code, "}}");
    let _ = writeln!(code);
    let _ = writeln!(code, "requestAnimationFrame(loop);");

    Some(code)
}

/// Get the sanitized call expression for a function by FuncId.
fn func_call_name(module: &Module, fid: FuncId) -> Option<String> {
    let func = module.functions.get(fid)?;
    Some(sanitize_ident(&func.name))
}

/// Emit the standard game loop calling `func_name` each frame.
fn emit_game_loop(out: &mut String, func_name: &str) {
    let _ = writeln!(out, "function loop() {{");
    let _ = writeln!(out, "  timing.tick();");
    let _ = writeln!(out, "  {func_name}();");
    let _ = writeln!(out, "  requestAnimationFrame(loop);");
    let _ = writeln!(out, "}}");
    let _ = writeln!(out);
    let _ = writeln!(out, "requestAnimationFrame(loop);");
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

        let main = generate_main(&[module]);
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

        let main = generate_main(&[module]);
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
        });

        let module = mb.build();
        let main = generate_main(&[module]);
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
        });

        mb.set_entry_point(EntryPoint::ConstructClass("MyApp".into()));
        let module = mb.build();

        let main = generate_main(&[module]);
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

        let main = generate_main(&[module]);
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
    fn main_with_init_order() {
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

        let mut fb0 = FunctionBuilder::new("script0__init", sig.clone(), Visibility::Public);
        fb0.ret(None);
        let s0 = mb.add_function(fb0.build());

        let mut fb1 = FunctionBuilder::new("script1__init", sig.clone(), Visibility::Public);
        fb1.ret(None);
        let s1 = mb.add_function(fb1.build());

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
        });

        mb.set_init_order(vec![s0, s1]);
        mb.set_entry_point(EntryPoint::ConstructClass("App".into()));
        let module = mb.build();

        let main = generate_main(&[module]);
        // Init calls should appear before the constructor.
        let init0_pos = main.find("script0__init();").expect("should have script0__init");
        let init1_pos = main.find("script1__init();").expect("should have script1__init");
        let ctor_pos = main.find("const app = new App();").expect("should construct App");
        assert!(
            init0_pos < init1_pos,
            "script0 before script1:\n{main}"
        );
        assert!(
            init1_pos < ctor_pos,
            "inits before constructor:\n{main}"
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
