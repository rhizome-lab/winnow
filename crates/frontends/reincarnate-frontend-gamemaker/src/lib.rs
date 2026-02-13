use std::fs;

use reincarnate_core::error::CoreError;
use reincarnate_core::ir::builder::ModuleBuilder;
use reincarnate_core::pipeline::{Frontend, FrontendInput, FrontendOutput};
use reincarnate_core::project::{AssetCatalog, EngineOrigin};

/// GameMaker frontend — translates data.win files into reincarnate IR.
pub struct GameMakerFrontend;

impl Frontend for GameMakerFrontend {
    fn supported_engines(&self) -> &[EngineOrigin] {
        &[EngineOrigin::GameMaker]
    }

    fn extract(&self, input: FrontendInput) -> Result<FrontendOutput, CoreError> {
        let data = fs::read(&input.source)?;
        let dw = datawin::DataWin::parse(data).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: e.to_string(),
        })?;

        let gen8 = dw.gen8().map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: e.to_string(),
        })?;
        let game_name = dw.resolve_string(gen8.name).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: format!("failed to resolve game name: {e}"),
        })?;

        eprintln!("[gamemaker] extracting: {game_name}");

        let mb = ModuleBuilder::new(&game_name);
        let module = mb.build();

        Ok(FrontendOutput {
            modules: vec![module],
            assets: AssetCatalog::default(),
        })
    }
}
