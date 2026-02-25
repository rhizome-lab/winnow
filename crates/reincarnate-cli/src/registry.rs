use std::collections::BTreeMap;
use std::fs;
use std::io::BufReader;
use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};

const REGISTRY_VERSION: u32 = 1;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectEntry {
    /// Absolute path to the manifest file.
    pub manifest: String,
    /// Engine name extracted from the manifest at add-time (e.g. "GameMaker", "Flash").
    pub engine: Option<String>,
    /// ISO 8601 UTC timestamp when this project was added to the registry.
    pub added_at: String,
    /// ISO 8601 UTC timestamp of the last successful `emit`, or null.
    pub last_emitted_at: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProjectRegistry {
    pub version: u32,
    /// Projects keyed by name (BTreeMap for stable sort order in serialized JSON).
    pub projects: BTreeMap<String, ProjectEntry>,
}

impl Default for ProjectRegistry {
    fn default() -> Self {
        Self {
            version: REGISTRY_VERSION,
            projects: BTreeMap::new(),
        }
    }
}

/// Returns `~/.config/reincarnate/projects.json` (XDG config dir on Linux/macOS).
pub fn registry_path() -> Result<PathBuf> {
    let config_dir = dirs::config_dir().context("could not determine config directory")?;
    Ok(config_dir.join("reincarnate").join("projects.json"))
}

/// Load the registry from disk.  Returns an empty registry if the file does not exist.
/// Errors if the file exists but cannot be parsed, or if its version is newer than supported.
pub fn load_registry() -> Result<ProjectRegistry> {
    let path = registry_path()?;
    if !path.exists() {
        return Ok(ProjectRegistry::default());
    }
    let file = fs::File::open(&path)
        .with_context(|| format!("failed to read registry: {}", path.display()))?;
    let reg: ProjectRegistry = serde_json::from_reader(BufReader::new(file))
        .with_context(|| format!("failed to parse registry: {}", path.display()))?;
    if reg.version > REGISTRY_VERSION {
        bail!(
            "registry version {} is not supported — please upgrade reincarnate \
             (this build supports up to version {})",
            reg.version,
            REGISTRY_VERSION
        );
    }
    Ok(reg)
}

/// Write the registry to disk, creating parent directories as needed.
pub fn save_registry(reg: &ProjectRegistry) -> Result<()> {
    let path = registry_path()?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create registry dir: {}", parent.display()))?;
    }
    let content = serde_json::to_string_pretty(reg)?;
    fs::write(&path, content)
        .with_context(|| format!("failed to write registry: {}", path.display()))?;
    Ok(())
}

/// Read only the `engine` field from a manifest file without full parsing.
pub fn read_engine_from_manifest(path: &Path) -> Option<String> {
    let file = fs::File::open(path).ok()?;
    let val: serde_json::Value = serde_json::from_reader(BufReader::new(file)).ok()?;
    val.get("engine")?.as_str().map(|s| s.to_string())
}

/// Return the current UTC time formatted as an ISO 8601 string.
pub fn now_iso8601() -> String {
    chrono::Utc::now().to_rfc3339()
}
