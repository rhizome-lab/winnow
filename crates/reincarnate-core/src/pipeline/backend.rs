use std::path::PathBuf;

use crate::error::CoreError;
use crate::ir::Module;
use super::LoweringConfig;
use crate::project::{AssetCatalog, RuntimeConfig};

/// A resolved runtime package: source directory + parsed config.
pub struct RuntimePackage {
    /// Path to the runtime source directory (copied into output).
    pub source_dir: PathBuf,
    /// Parsed `runtime.json` configuration.
    pub config: RuntimeConfig,
}

/// Input to a backend.
pub struct BackendInput {
    /// The typed, optimized IR modules to compile.
    pub modules: Vec<Module>,
    /// Extracted assets to include in the output.
    pub assets: AssetCatalog,
    /// Output directory for generated code.
    pub output_dir: PathBuf,
    /// Configuration for AST lowering optimizations.
    pub lowering_config: LoweringConfig,
    /// Engine-specific runtime package.
    /// When `Some`, the backend copies the runtime and uses its config for codegen.
    /// When `None`, runtime emission is skipped.
    pub runtime: Option<RuntimePackage>,
}

/// Backend trait — emits target code from IR.
pub trait Backend {
    /// Name of this backend (e.g., "rust", "typescript").
    fn name(&self) -> &str;

    /// Generate code from the IR modules.
    fn emit(&self, input: BackendInput) -> Result<(), CoreError>;
}
