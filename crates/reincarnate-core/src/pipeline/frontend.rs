use std::path::PathBuf;

use crate::error::CoreError;
use crate::ir::Module;
use crate::project::{AssetCatalog, EngineOrigin};

/// Input to a frontend.
pub struct FrontendInput {
    /// Path to the source binary/project.
    pub source: PathBuf,
    /// Engine origin hint (from manifest).
    pub engine: EngineOrigin,
}

/// Output from a frontend.
pub struct FrontendOutput {
    /// The IR modules extracted from the source.
    pub modules: Vec<Module>,
    /// Assets extracted alongside the code.
    pub assets: AssetCatalog,
}

/// Frontend trait — parses engine-specific formats and emits IR.
pub trait Frontend {
    /// Which engine(s) this frontend supports.
    fn supported_engines(&self) -> &[EngineOrigin];

    /// Parse the source and produce IR modules + extracted assets.
    fn extract(&self, input: FrontendInput) -> Result<FrontendOutput, CoreError>;
}
