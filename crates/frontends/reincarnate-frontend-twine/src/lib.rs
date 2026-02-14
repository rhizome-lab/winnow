pub mod extract;
mod harlowe;
pub mod sugarcube;

use std::fs;

use reincarnate_core::error::CoreError;
use reincarnate_core::pipeline::{Frontend, FrontendInput, FrontendOutput};
use reincarnate_core::project::EngineOrigin;

/// Twine frontend — extracts stories from compiled Twine HTML files.
///
/// Supports SugarCube and Harlowe story formats. The story format is
/// auto-detected from the `format` attribute of `<tw-storydata>`.
pub struct TwineFrontend;

impl Frontend for TwineFrontend {
    fn supported_engines(&self) -> &[EngineOrigin] {
        &[EngineOrigin::Twine]
    }

    fn extract(&self, input: FrontendInput) -> Result<FrontendOutput, CoreError> {
        let html = fs::read_to_string(&input.source)?;

        let story = extract::extract_story(&html).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: e.to_string(),
        })?;

        match story.format.as_str() {
            "SugarCube" => {
                // Parse all passages, collect diagnostics
                let mut total_errors = 0;
                for passage in &story.passages {
                    let ast = sugarcube::parse_passage(&passage.source);
                    total_errors += ast.errors.len();
                }

                // TODO: SugarCube passage ASTs → IR modules
                Err(CoreError::Parse {
                    file: input.source,
                    message: format!(
                        "SugarCube IR lowering not yet implemented \
                         (parsed {} passages, {} errors)",
                        story.passages.len(),
                        total_errors,
                    ),
                })
            }
            "Harlowe" => {
                // TODO: Harlowe passage parsing → IR
                Err(CoreError::Parse {
                    file: input.source,
                    message: format!(
                        "Harlowe parser not yet implemented (story has {} passages)",
                        story.passages.len()
                    ),
                })
            }
            other => Err(CoreError::Parse {
                file: input.source,
                message: format!(
                    "unsupported Twine story format: {other:?} \
                     (supported: SugarCube, Harlowe)"
                ),
            }),
        }
    }
}
