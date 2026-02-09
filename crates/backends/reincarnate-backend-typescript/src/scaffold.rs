//! Generate project scaffold files: index.html, tsconfig.json, and main entry point.

use std::fmt::Write;
use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{MethodKind, Module, Visibility};

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

    let mut entry_func: Option<String> = None;

    for module in modules {
        if module.classes.is_empty() {
            // Flat module — import public functions directly.
            let public_funcs: Vec<_> = module
                .functions
                .values()
                .filter(|f| f.visibility == Visibility::Public)
                .collect();
            if public_funcs.is_empty() {
                continue;
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
            if entry_func.is_none() {
                for func in &public_funcs {
                    if is_entry_candidate(&func.name) {
                        entry_func = Some(sanitize_ident(&func.name));
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
                if !class_methods.contains(&fid)
                    && func.visibility == Visibility::Public
                {
                    imports.push(sanitize_ident(&func.name));
                    if entry_func.is_none() && is_entry_candidate(&func.name) {
                        entry_func = Some(sanitize_ident(&func.name));
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
            if entry_func.is_none() {
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
                                entry_func =
                                    Some(format!("{class_name}.{method_name}"));
                                break;
                            }
                        }
                    }
                    if entry_func.is_some() {
                        break;
                    }
                }
            }
        }
    }

    out.push('\n');

    match entry_func {
        Some(func_name) => {
            let _ = writeln!(out, "function loop() {{");
            let _ = writeln!(out, "  timing.tick();");
            let _ = writeln!(out, "  {func_name}();");
            let _ = writeln!(out, "  requestAnimationFrame(loop);");
            let _ = writeln!(out, "}}");
            let _ = writeln!(out);
            let _ = writeln!(out, "requestAnimationFrame(loop);");
        }
        None => {
            let _ = writeln!(
                out,
                "// No entry point detected. Module-level code will run on import."
            );
        }
    }

    out
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
    fn index_html_has_canvas() {
        let mb = ModuleBuilder::new("my_game");
        let module = mb.build();

        let html = generate_index_html(&[module]);
        assert!(html.contains("reincarnate-canvas"));
        assert!(html.contains("<title>my_game</title>"));
        assert!(html.contains("dist/bundle.js"));
    }
}
