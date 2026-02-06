use std::path::PathBuf;

/// Core error type for the reincarnate framework.
#[derive(Debug, thiserror::Error)]
pub enum CoreError {
    #[error("unsupported format: {format}")]
    UnsupportedFormat { format: String },

    #[error("parse error in {file}: {message}")]
    Parse { file: PathBuf, message: String },

    #[error("type error: {0}")]
    Type(String),

    #[error("unresolved import: {module}::{name}")]
    UnresolvedImport { module: String, name: String },

    #[error("codegen error: {0}")]
    Codegen(String),

    #[error("project error: {0}")]
    Project(String),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
}
