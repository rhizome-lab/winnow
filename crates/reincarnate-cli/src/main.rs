use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use reincarnate_core::ir::Module;
use reincarnate_core::pipeline::{Frontend, FrontendInput};
use reincarnate_core::project::{EngineOrigin, ProjectManifest};

#[derive(Parser)]
#[command(name = "reincarnate", about = "Legacy software lifting framework")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Display project manifest info.
    Info {
        /// Path to the project manifest.
        #[arg(long, default_value = "reincarnate.json")]
        manifest: PathBuf,
    },
    /// Print a JSON-serialized IR module in human-readable form.
    PrintIr {
        /// Path to a JSON IR module file.
        file: PathBuf,
    },
    /// Extract IR from a project's source files.
    Extract {
        /// Path to the project manifest.
        #[arg(long, default_value = "reincarnate.json")]
        manifest: PathBuf,
    },
}

fn load_manifest(path: &PathBuf) -> Result<ProjectManifest> {
    let file = File::open(path).with_context(|| format!("failed to open manifest: {}", path.display()))?;
    let reader = BufReader::new(file);
    let manifest: ProjectManifest =
        serde_json::from_reader(reader).with_context(|| format!("failed to parse manifest: {}", path.display()))?;
    Ok(manifest)
}

fn find_frontend(engine: &EngineOrigin) -> Option<Box<dyn Frontend>> {
    match engine {
        EngineOrigin::Flash => Some(Box::new(reincarnate_flash::FlashFrontend)),
        _ => None,
    }
}

fn cmd_info(manifest_path: &PathBuf) -> Result<()> {
    let manifest = load_manifest(manifest_path)?;
    println!("Project: {}", manifest.name);
    println!("Version: {}", manifest.version);
    println!("Engine:  {:?}", manifest.engine);
    println!("Source:  {}", manifest.source.display());
    println!("Targets:");
    for target in &manifest.targets {
        println!("  - {:?} -> {}", target.backend, target.output_dir.display());
    }
    Ok(())
}

fn cmd_print_ir(file: &PathBuf) -> Result<()> {
    let f = File::open(file).with_context(|| format!("failed to open IR file: {}", file.display()))?;
    let reader = BufReader::new(f);
    let module: Module =
        serde_json::from_reader(reader).with_context(|| format!("failed to parse IR file: {}", file.display()))?;
    println!("{module}");
    Ok(())
}

fn cmd_extract(manifest_path: &PathBuf) -> Result<()> {
    let manifest = load_manifest(manifest_path)?;
    let frontend = find_frontend(&manifest.engine);
    let Some(frontend) = frontend else {
        bail!(
            "no frontend available for engine {:?}",
            manifest.engine
        );
    };

    let input = FrontendInput {
        source: manifest.source.clone(),
        engine: manifest.engine.clone(),
    };
    let output = frontend
        .extract(input)
        .map_err(|e| anyhow::anyhow!("{e}"))?;

    for module in &output.modules {
        println!("{module}");
    }
    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Command::Info { manifest } => cmd_info(manifest),
        Command::PrintIr { file } => cmd_print_ir(file),
        Command::Extract { manifest } => cmd_extract(manifest),
    }
}
