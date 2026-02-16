pub mod ast_passes;
pub mod ast_printer;
pub mod emit;
pub mod js_ast;
pub mod lower;
pub mod rewrites;
pub mod runtime;
pub mod scaffold;
pub mod types;

use std::fs;

use reincarnate_core::error::CoreError;
use reincarnate_core::pipeline::{Backend, BackendInput};

/// TypeScript codegen backend.
pub struct TypeScriptBackend;

impl Backend for TypeScriptBackend {
    fn name(&self) -> &str {
        "typescript"
    }

    fn emit(&self, mut input: BackendInput) -> Result<(), CoreError> {
        fs::create_dir_all(&input.output_dir)?;

        if let Some(ref runtime_pkg) = input.runtime {
            runtime::emit_runtime(&input.output_dir, &runtime_pkg.source_dir, &runtime_pkg.config)?;
        }

        let runtime_config = input.runtime.as_ref().map(|p| &p.config);
        for module in &mut input.modules {
            emit::emit_module(module, &input.output_dir, &input.lowering_config, runtime_config, &input.debug)?;
        }

        scaffold::emit_scaffold(&input.modules, &input.output_dir, runtime_config, &input.assets)?;

        // Write extracted assets to disk.
        for asset in &input.assets.assets {
            if asset.data.is_empty() {
                continue;
            }
            let path = input.output_dir.join(&asset.path);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&path, &asset.data)?;
        }

        Ok(())
    }
}
