pub mod extract;
mod harlowe;
pub mod sugarcube;

use std::fs;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{EntryPoint, ModuleBuilder};
use reincarnate_core::pipeline::{Frontend, FrontendInput, FrontendOutput};
use reincarnate_core::project::{AssetCatalog, EngineOrigin};

use sugarcube::translate;

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
            "SugarCube" => self.extract_sugarcube(&story),
            "Harlowe" => {
                // TODO: Harlowe passage parsing → IR
                Err(CoreError::Parse {
                    file: Default::default(),
                    message: format!(
                        "Harlowe parser not yet implemented (story has {} passages)",
                        story.passages.len()
                    ),
                })
            }
            other => Err(CoreError::Parse {
                file: Default::default(),
                message: format!(
                    "unsupported Twine story format: {other:?} \
                     (supported: SugarCube, Harlowe)"
                ),
            }),
        }
    }
}

impl TwineFrontend {
    fn extract_sugarcube(
        &self,
        story: &extract::Story,
    ) -> Result<FrontendOutput, CoreError> {
        let mut mb = ModuleBuilder::new(&story.name);
        let mut start_func_id = None;

        // Find start passage name
        let start_passage_name = story
            .passages
            .iter()
            .find(|p| p.pid == story.start_pid)
            .map(|p| p.name.clone());

        // Translate each passage → Function
        for passage in &story.passages {
            // Skip special tag passages (stylesheet, script, etc.)
            if passage.tags.iter().any(|t| {
                matches!(
                    t.as_str(),
                    "stylesheet" | "Twine.private" | "annotation"
                )
            }) {
                continue;
            }

            let ast = sugarcube::parse_passage(&passage.source);

            let (func, widgets) = translate::translate_passage(&passage.name, &ast);
            let func_id = mb.add_function(func);

            // Track start passage
            if Some(&passage.name) == start_passage_name.as_ref() {
                start_func_id = Some(func_id);
            }

            // Translate extracted widgets as separate functions
            for (widget_name, widget_body) in &widgets {
                let widget_func = translate::translate_widget(widget_name, widget_body);
                mb.add_function(widget_func);
            }
        }

        // Handle user scripts: store as metadata via a special init function
        for (i, script) in story.user_scripts.iter().enumerate() {
            if script.trim().is_empty() {
                continue;
            }
            let func = translate::translate_user_script(i, script);
            mb.add_function(func);
        }

        // Set entry point to the start passage
        if let Some(fid) = start_func_id {
            mb.set_entry_point(EntryPoint::CallFunction(fid));
        }

        let module = mb.build();

        Ok(FrontendOutput {
            modules: vec![module],
            assets: AssetCatalog::new(),
        })
    }
}
