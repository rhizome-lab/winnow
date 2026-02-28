use std::path::PathBuf;

use crate::error::CoreError;

/// A single diagnostic from a language-level type checker.
#[derive(Debug, Clone, serde::Serialize)]
pub struct Diagnostic {
    pub file: String,
    pub line: u32,
    pub col: u32,
    /// Error code, e.g. "TS2304".
    pub code: String,
    pub severity: Severity,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum Severity {
    Error,
    Warning,
}

/// Input to a checker — the output directory to typecheck.
pub struct CheckerInput {
    pub output_dir: PathBuf,
}

/// Output from a checker.
#[derive(Debug, Clone, serde::Serialize)]
pub struct CheckerOutput {
    pub diagnostics: Vec<Diagnostic>,
    pub summary: CheckSummary,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CheckSummary {
    pub output_dir: String,
    pub total_errors: usize,
    pub total_warnings: usize,
    /// Error codes sorted by count descending.
    pub by_code: Vec<(String, usize)>,
    /// Unique messages sorted by count descending: (message, code, count).
    #[serde(default)]
    pub by_message: Vec<(String, String, usize)>,
}

/// Checker trait — validates emitted code using an external type checker.
pub trait Checker {
    fn name(&self) -> &str;
    fn check(&self, input: CheckerInput) -> Result<CheckerOutput, CoreError>;
}
