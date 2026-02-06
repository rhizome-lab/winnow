use std::path::PathBuf;

use serde::{Deserialize, Serialize};

/// Kind of asset extracted from a source binary.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssetKind {
    Image,
    Audio,
    Video,
    Font,
    Script,
    Data,
    Shader,
    Other(String),
}

/// A single extracted asset.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Asset {
    /// Unique identifier within the project.
    pub id: String,
    pub kind: AssetKind,
    /// Original name/path in the source binary.
    pub original_name: String,
    /// Path to the extracted file on disk.
    pub path: PathBuf,
    /// Size in bytes.
    pub size: u64,
}

/// Catalog of all assets extracted from a project.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AssetCatalog {
    pub assets: Vec<Asset>,
}

impl AssetCatalog {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, asset: Asset) {
        self.assets.push(asset);
    }

    pub fn find_by_id(&self, id: &str) -> Option<&Asset> {
        self.assets.iter().find(|a| a.id == id)
    }

    pub fn find_by_kind(&self, kind: &AssetKind) -> Vec<&Asset> {
        self.assets.iter().filter(|a| &a.kind == kind).collect()
    }
}
