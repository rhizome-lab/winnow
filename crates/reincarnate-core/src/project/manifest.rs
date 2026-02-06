use std::path::PathBuf;

use serde::{Deserialize, Serialize};

/// The source engine a project was extracted from.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum EngineOrigin {
    Flash,
    Director,
    Vb6,
    JavaApplet,
    Silverlight,
    HyperCard,
    ToolBook,
    RenPy,
    RpgMakerVxAce,
    RpgMakerMv,
    RpgMakerMz,
    GameMaker,
    Twine,
    Inform,
    Other(String),
}

/// Codegen backend target.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TargetBackend {
    Rust,
    TypeScript,
}

/// Configuration for a build target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetConfig {
    pub backend: TargetBackend,
    pub output_dir: PathBuf,
    /// Additional backend-specific options.
    pub options: serde_json::Value,
}

/// Top-level project manifest (reincarnate.json).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectManifest {
    pub name: String,
    pub version: String,
    pub engine: EngineOrigin,
    /// Path to the source binary/project.
    pub source: PathBuf,
    pub targets: Vec<TargetConfig>,
}
