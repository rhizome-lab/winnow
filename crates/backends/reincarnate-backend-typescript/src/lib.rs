pub mod ast_printer;
pub mod emit;
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

        if let Some(ref runtime_src) = input.runtime_dir {
            runtime::emit_runtime(&input.output_dir, runtime_src)?;
        }

        for module in &mut input.modules {
            emit::emit_module(module, &input.output_dir, &input.lowering_config)?;
        }

        scaffold::emit_scaffold(&input.modules, &input.output_dir)?;

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
