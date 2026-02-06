use std::path::PathBuf;

use crate::error::CoreError;
use crate::ir::Module;
use crate::project::AssetCatalog;

/// Input to a backend.
pub struct BackendInput {
    /// The typed, optimized IR modules to compile.
    pub modules: Vec<Module>,
    /// Extracted assets to include in the output.
    pub assets: AssetCatalog,
    /// Output directory for generated code.
    pub output_dir: PathBuf,
}

/// Backend trait — emits target code from IR.
pub trait Backend {
    /// Name of this backend (e.g., "rust", "typescript").
    fn name(&self) -> &str;

    /// Generate code from the IR modules.
    fn emit(&self, input: BackendInput) -> Result<(), CoreError>;
}
