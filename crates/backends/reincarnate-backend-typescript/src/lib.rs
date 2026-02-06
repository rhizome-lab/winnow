pub mod emit;
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

    fn emit(&self, input: BackendInput) -> Result<(), CoreError> {
        fs::create_dir_all(&input.output_dir)?;

        for module in &input.modules {
            emit::emit_module(module, &input.output_dir)?;
        }

        Ok(())
    }
}
