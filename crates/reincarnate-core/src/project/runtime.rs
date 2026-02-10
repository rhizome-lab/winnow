use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

/// Describes an engine-specific runtime package.
///
/// Loaded from `runtime.json` inside the runtime source directory.
/// The backend uses this to generate imports and scaffold code
/// without hardcoding engine-specific paths.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuntimeConfig {
    /// System call name → module path mapping for import generation.
    /// e.g. `"Flash.Object"` → `{ path: "flash/object" }`
    pub system_modules: BTreeMap<String, SystemModule>,
    /// Scaffold configuration for the main entry point file.
    pub scaffold: ScaffoldConfig,
    /// Per-class-file imports (e.g. class registration helpers).
    /// When `None`, no class preamble is emitted.
    #[serde(default)]
    pub class_preamble: Option<ImportGroup>,
}

/// A single system module mapping.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemModule {
    /// Submodule path relative to runtime root (e.g. `"flash/object"`).
    pub path: String,
    /// Use named import (`import { X }`) instead of namespace (`import * as X`).
    #[serde(default)]
    pub named_import: bool,
}

/// Configuration for the generated entry point (`main.ts`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScaffoldConfig {
    /// Additional imports for the entry point file.
    #[serde(default)]
    pub imports: Vec<ImportGroup>,
    /// Code after `new ClassName()` for ConstructClass entries
    /// (e.g. `"stage.addChild(app);"`).
    #[serde(default)]
    pub construct_class_init: Option<String>,
    /// Code called each frame in the game loop (e.g. `"flashTick();"`).
    #[serde(default)]
    pub tick: Option<String>,
}

/// A group of named imports from a single path.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportGroup {
    pub names: Vec<String>,
    pub path: String,
}
