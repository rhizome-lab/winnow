use std::path::PathBuf;

use serde::{Deserialize, Deserializer, Serialize};

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
    #[serde(default)]
    pub options: serde_json::Value,
}

/// An asset entry in the manifest: either a plain path (copied as-is) or
/// a `{ "src": "...", "dest": "..." }` object for path remapping.
#[derive(Debug, Clone, Serialize)]
pub struct AssetMapping {
    /// Source path (relative to manifest, resolved to absolute by CLI).
    pub src: PathBuf,
    /// Output path (relative to the output directory). If `None`, uses the
    /// source's filename/dirname.
    pub dest: Option<PathBuf>,
}

impl AssetMapping {
    /// The output-relative path for this asset.
    pub fn dest_name(&self) -> &std::path::Path {
        self.dest
            .as_deref()
            .unwrap_or_else(|| std::path::Path::new(self.src.file_name().unwrap_or_default()))
    }
}

impl<'de> Deserialize<'de> for AssetMapping {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Raw {
            Plain(PathBuf),
            Mapped { src: PathBuf, dest: Option<PathBuf> },
        }
        match Raw::deserialize(deserializer)? {
            Raw::Plain(src) => Ok(AssetMapping { src, dest: None }),
            Raw::Mapped { src, dest } => Ok(AssetMapping { src, dest }),
        }
    }
}

/// Persistence configuration for save/load behavior.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersistenceConfig {
    /// Enable continuous autosave after each passage transition.
    #[serde(default = "PersistenceConfig::default_autosave")]
    pub autosave: bool,
    /// Resume behavior: "auto" (silent), "prompt", or "ignore".
    #[serde(default = "PersistenceConfig::default_resume")]
    pub resume: String,
    /// History strategy: "snapshot" or "diff".
    #[serde(default = "PersistenceConfig::default_history")]
    pub history: String,
    /// Number of save slots.
    #[serde(default = "PersistenceConfig::default_slot_count")]
    pub slot_count: u32,
    /// Debounce interval for autosave writes (milliseconds). 0 = no debounce.
    #[serde(default)]
    pub debounce_ms: u32,
}

impl Default for PersistenceConfig {
    fn default() -> Self {
        Self {
            autosave: true,
            resume: "auto".into(),
            history: "snapshot".into(),
            slot_count: 8,
            debounce_ms: 0,
        }
    }
}

impl PersistenceConfig {
    fn default_autosave() -> bool {
        true
    }
    fn default_resume() -> String {
        "auto".into()
    }
    fn default_history() -> String {
        "snapshot".into()
    }
    fn default_slot_count() -> u32 {
        8
    }
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
    /// Asset directories/files to copy into the build output.
    ///
    /// Each entry is either a plain string path (copied preserving its name)
    /// or `{ "src": "...", "dest": "..." }` for path remapping.
    #[serde(default)]
    pub assets: Vec<AssetMapping>,
    /// Persistence configuration (save/load/autosave behavior).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub persistence: Option<PersistenceConfig>,
    /// Frontend-specific options passed verbatim to the frontend.
    ///
    /// Known keys by engine:
    /// - `Twine/Harlowe`: `"hal_audio": false` disables HAL macro translation
    ///   (macros fall back to `unknown_macro()`; default: `true`).
    #[serde(default, skip_serializing_if = "serde_json::Value::is_null")]
    pub frontend_options: serde_json::Value,
}
