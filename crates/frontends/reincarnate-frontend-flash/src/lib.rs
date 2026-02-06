//! Flash/AVM2 frontend for Reincarnate.
//!
//! Reads SWF files, extracts AVM2 (ActionScript 3) bytecode from DoABC tags,
//! and translates the stack-based AVM2 instruction set into register-based
//! reincarnate IR.

pub mod abc;
pub mod assets;
pub mod class;
pub mod multiname;
pub mod scope;
pub mod translate;

use std::fs;

use reincarnate_core::error::CoreError;
use reincarnate_core::pipeline::{Frontend, FrontendInput, FrontendOutput};
use reincarnate_core::project::EngineOrigin;

use swf::avm2::read::Reader;

/// Flash/AVM2 frontend implementation.
pub struct FlashFrontend;

impl Frontend for FlashFrontend {
    fn supported_engines(&self) -> &[EngineOrigin] {
        &[EngineOrigin::Flash]
    }

    fn extract(&self, input: FrontendInput) -> Result<FrontendOutput, CoreError> {
        let data = fs::read(&input.source)?;

        let swf_buf = swf::decompress_swf(&data[..]).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: format!("SWF decompression failed: {e}"),
        })?;

        let swf = swf::parse_swf(&swf_buf).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: format!("SWF parsing failed: {e}"),
        })?;

        let mut modules = Vec::new();
        let assets = assets::extract_assets(&swf.tags);

        // Extract ABC bytecode from DoAbc / DoAbc2 tags.
        for (tag_idx, tag) in swf.tags.iter().enumerate() {
            let abc_data = match tag {
                swf::Tag::DoAbc(data) => Some((*data, format!("abc{tag_idx}"))),
                swf::Tag::DoAbc2(abc2) => {
                    let name = abc2.name.to_string_lossy(swf::UTF_8).to_string();
                    let name = if name.is_empty() {
                        format!("abc{tag_idx}")
                    } else {
                        name
                    };
                    Some((abc2.data, name))
                }
                _ => None,
            };

            if let Some((data, module_name)) = abc_data {
                let mut reader = Reader::new(data);
                let abc = reader.read().map_err(|e| CoreError::Parse {
                    file: input.source.clone(),
                    message: format!("ABC parsing failed in tag {tag_idx}: {e}"),
                })?;

                let module = class::translate_abc_to_module(&abc, &module_name).map_err(|e| {
                    CoreError::Parse {
                        file: input.source.clone(),
                        message: format!("translation failed in {module_name}: {e}"),
                    }
                })?;

                modules.push(module);
            }
        }

        Ok(FrontendOutput { modules, assets })
    }
}
