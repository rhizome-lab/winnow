pub mod extract;
pub mod harlowe;
pub mod html_util;
pub mod sugarcube;

use std::fs;
use std::path::PathBuf;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{EntryPoint, ModuleBuilder};
use reincarnate_core::pipeline::{Frontend, FrontendInput, FrontendOutput};
use reincarnate_core::project::{Asset, AssetCatalog, AssetKind, EngineOrigin};

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
            "Harlowe" => self.extract_harlowe(&story),
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
        let mut assets = AssetCatalog::new();

        // Find start passage name
        let start_passage_name = story
            .passages
            .iter()
            .find(|p| p.pid == story.start_pid)
            .map(|p| p.name.clone());

        // Build custom macro registry from user scripts before parsing passages.
        let custom_macro_registry = story
            .user_scripts
            .iter()
            .fold(sugarcube::CustomMacroRegistry::new(), |mut reg, script| {
                reg.extend(sugarcube::extract_custom_macros(script));
                reg
            });

        // Translate each passage → Function
        for passage in &story.passages {
            // Collect stylesheet passages as assets
            if passage.tags.iter().any(|t| t == "stylesheet") {
                if !passage.source.trim().is_empty() {
                    let safe_name = passage.name.replace(['/', '\\', ' '], "_");
                    assets.add(Asset {
                        id: format!("stylesheet:{}", passage.name),
                        kind: AssetKind::Stylesheet,
                        original_name: passage.name.clone(),
                        path: PathBuf::from(format!("assets/styles/{safe_name}.css")),
                        size: passage.source.len() as u64,
                        data: passage.source.as_bytes().to_vec(),
                    });
                }
                continue;
            }

            // Skip other special tag passages
            if passage
                .tags
                .iter()
                .any(|t| matches!(t.as_str(), "Twine.private" | "annotation"))
            {
                continue;
            }

            let ast = sugarcube::parse_passage_with_registry(
                &passage.source,
                &custom_macro_registry,
            );

            let func_name = translate::passage_func_name(&passage.name);
            let result = translate::translate_passage(&passage.name, &ast);
            let func_id = mb.add_function(result.func);

            // Register passage name → function name mapping for the passage registry
            mb.add_passage_name(passage.name.clone(), func_name);
            mb.add_passage_tags(passage.name.clone(), passage.tags.clone());

            // Track start passage
            if Some(&passage.name) == start_passage_name.as_ref() {
                start_func_id = Some(func_id);
            }

            // Add setter callback functions
            for setter_func in result.setter_callbacks {
                mb.add_function(setter_func);
            }

            // Translate extracted widgets as separate functions
            for (widget_name, widget_body, widget_source) in &result.widgets {
                let (widget_func, widget_callbacks) =
                    translate::translate_widget(widget_name, widget_body, widget_source);
                let widget_func_name = format!("widget_{widget_name}");
                mb.add_passage_name(widget_name.clone(), widget_func_name);
                mb.add_function(widget_func);
                for cb in widget_callbacks {
                    mb.add_function(cb);
                }
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

        // Collect user stylesheets as assets
        for (i, style) in story.user_styles.iter().enumerate() {
            if style.trim().is_empty() {
                continue;
            }
            assets.add(Asset {
                id: format!("user_style:{i}"),
                kind: AssetKind::Stylesheet,
                original_name: format!("user_style_{i}"),
                path: PathBuf::from(format!("assets/styles/user_{i}.css")),
                size: style.len() as u64,
                data: style.as_bytes().to_vec(),
            });
        }

        // Set entry point to the start passage
        if let Some(fid) = start_func_id {
            mb.set_entry_point(EntryPoint::CallFunction(fid));
        }

        let module = mb.build();

        Ok(FrontendOutput {
            modules: vec![module],
            assets,
            runtime_variant: None,
        })
    }

    fn extract_harlowe(
        &self,
        story: &extract::Story,
    ) -> Result<FrontendOutput, CoreError> {
        let mut mb = ModuleBuilder::new(&story.name);
        let mut start_func_id = None;
        let mut assets = AssetCatalog::new();
        let mut parse_errors = 0;

        // Find start passage name
        let start_passage_name = story
            .passages
            .iter()
            .find(|p| p.pid == story.start_pid)
            .map(|p| p.name.clone());

        // Translate each passage → Function
        for passage in &story.passages {
            // Skip special tag passages
            if passage
                .tags
                .iter()
                .any(|t| matches!(t.as_str(), "Twine.private" | "annotation"))
            {
                continue;
            }

            let ast = harlowe::parse_passage(&passage.source);

            // Log parse errors but continue
            for err in &ast.errors {
                eprintln!(
                    "warning: parse error in passage '{}': {}",
                    passage.name, err
                );
                parse_errors += 1;
            }

            let func_name = harlowe::translate::passage_func_name(&passage.name);
            let result = harlowe::translate::translate_passage(&passage.name, &ast);
            let func_id = mb.add_function(result.func);

            // Register passage name → function name mapping
            mb.add_passage_name(passage.name.clone(), func_name);
            mb.add_passage_tags(passage.name.clone(), passage.tags.clone());

            // Track start passage
            if Some(&passage.name) == start_passage_name.as_ref() {
                start_func_id = Some(func_id);
            }

            // Add callback functions (link hooks, live intervals, etc.)
            for cb_func in result.callbacks {
                mb.add_function(cb_func);
            }
        }

        if parse_errors > 0 {
            eprintln!("warning: {parse_errors} parse error(s) in Harlowe story");
        }

        // Add format CSS (Harlowe built-in stylesheet) as an asset
        if let Some(css) = &story.format_css {
            assets.add(Asset {
                id: "format_css:harlowe".to_string(),
                kind: AssetKind::Stylesheet,
                original_name: "format_harlowe".to_string(),
                path: PathBuf::from("assets/styles/format_harlowe.css"),
                size: css.len() as u64,
                data: css.as_bytes().to_vec(),
            });
        }

        // Collect user stylesheets as assets
        for (i, style) in story.user_styles.iter().enumerate() {
            if style.trim().is_empty() {
                continue;
            }
            assets.add(Asset {
                id: format!("user_style:{i}"),
                kind: AssetKind::Stylesheet,
                original_name: format!("user_style_{i}"),
                path: PathBuf::from(format!("assets/styles/user_{i}.css")),
                size: style.len() as u64,
                data: style.as_bytes().to_vec(),
            });
        }

        // Set entry point to the start passage
        if let Some(fid) = start_func_id {
            mb.set_entry_point(EntryPoint::CallFunction(fid));
        }

        let module = mb.build();

        Ok(FrontendOutput {
            modules: vec![module],
            assets,
            runtime_variant: Some("harlowe".to_string()),
        })
    }
}
