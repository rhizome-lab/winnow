use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use reincarnate_core::ir::Module;
use reincarnate_core::pipeline::{Backend, BackendInput, Frontend, FrontendInput, Linker, PassConfig, Preset, RuntimePackage};
use reincarnate_core::project::{EngineOrigin, ProjectManifest, TargetBackend};
use reincarnate_core::transforms::default_pipeline;

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
        /// Transform passes to skip (e.g. "type-inference", "constant-folding").
        #[arg(long = "skip-pass")]
        skip_passes: Vec<String>,
    },
    /// Run the full pipeline: extract, transform, and emit target code.
    Emit {
        /// Path to the project manifest.
        #[arg(long, default_value = "reincarnate.json")]
        manifest: PathBuf,
        /// Transform passes to skip on top of the preset.
        #[arg(long = "skip-pass")]
        skip_passes: Vec<String>,
        /// Pipeline preset: "literal" (1:1 translation) or "optimized" (default).
        #[arg(long, default_value = "optimized")]
        preset: String,
    },
}

fn load_manifest(path: &PathBuf) -> Result<ProjectManifest> {
    let file = File::open(path).with_context(|| format!("failed to open manifest: {}", path.display()))?;
    let reader = BufReader::new(file);
    let mut manifest: ProjectManifest =
        serde_json::from_reader(reader).with_context(|| format!("failed to parse manifest: {}", path.display()))?;

    // Resolve relative paths against the manifest's parent directory.
    if let Some(base) = path.canonicalize()?.parent() {
        if manifest.source.is_relative() {
            manifest.source = base.join(&manifest.source);
        }
        for target in &mut manifest.targets {
            if target.output_dir.is_relative() {
                target.output_dir = base.join(&target.output_dir);
            }
        }
    }

    Ok(manifest)
}

fn find_frontend(engine: &EngineOrigin) -> Option<Box<dyn Frontend>> {
    match engine {
        EngineOrigin::Flash => Some(Box::new(reincarnate_frontend_flash::FlashFrontend)),
        EngineOrigin::GameMaker => Some(Box::new(reincarnate_frontend_gamemaker::GameMakerFrontend)),
        _ => None,
    }
}

/// Resolve the engine-specific runtime package.
///
/// Uses `CARGO_MANIFEST_DIR` (compile-time) to locate `runtime/{engine}/{lang}/`
/// relative to the workspace root. Loads `runtime.json` from the directory.
/// Returns `None` if the directory or config file doesn't exist.
fn resolve_runtime(engine: &EngineOrigin, backend: &TargetBackend) -> Option<RuntimePackage> {
    let engine_name = match engine {
        EngineOrigin::Flash => "flash",
        EngineOrigin::GameMaker => "gamemaker",
        _ => return None,
    };
    let lang = match backend {
        TargetBackend::TypeScript => "ts",
        _ => return None,
    };
    // CARGO_MANIFEST_DIR points to crates/reincarnate-cli/
    let cli_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let runtime_dir = cli_dir.join("../../runtime").join(engine_name).join(lang);
    let source_dir = match runtime_dir.canonicalize() {
        Ok(p) if p.is_dir() => p,
        _ => return None,
    };
    let config_path = source_dir.join("runtime.json");
    let config_file = std::fs::File::open(&config_path).ok()?;
    let config = serde_json::from_reader(std::io::BufReader::new(config_file))
        .unwrap_or_else(|e| panic!("failed to parse {}: {e}", config_path.display()));
    Some(RuntimePackage { source_dir, config })
}

fn find_backend(backend: &TargetBackend) -> Option<Box<dyn Backend>> {
    match backend {
        TargetBackend::TypeScript => {
            Some(Box::new(reincarnate_backend_typescript::TypeScriptBackend))
        }
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

fn cmd_extract(manifest_path: &PathBuf, skip_passes: &[String]) -> Result<()> {
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

    let skip_refs: Vec<&str> = skip_passes.iter().map(|s| s.as_str()).collect();
    let config = PassConfig::from_skip_list(&skip_refs);
    let pipeline = default_pipeline(&config);
    for module in output.modules {
        let module = pipeline.run(module).map_err(|e| anyhow::anyhow!("{e}"))?;
        println!("{module}");
    }
    Ok(())
}

fn cmd_emit(manifest_path: &PathBuf, skip_passes: &[String], preset: &str) -> Result<()> {
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

    // Resolve runtime early so we can attach type_definitions to modules
    // before running transforms (type inference needs them).
    let first_backend = manifest.targets.first().map(|t| &t.backend);
    let early_runtime = first_backend.and_then(|b| resolve_runtime(&manifest.engine, b));
    let external_type_defs = early_runtime
        .as_ref()
        .map(|rt| rt.config.type_definitions.clone())
        .unwrap_or_default();
    let external_function_sigs = early_runtime
        .as_ref()
        .map(|rt| rt.config.function_signatures.clone())
        .unwrap_or_default();

    let skip_refs: Vec<&str> = skip_passes.iter().map(|s| s.as_str()).collect();
    let (pass_config, lowering_config) = Preset::resolve(preset, &skip_refs)
        .ok_or_else(|| anyhow::anyhow!("unknown preset: {preset:?} (valid: \"literal\", \"optimized\")"))?;
    let pipeline = default_pipeline(&pass_config);

    let mut modules = Vec::new();
    for mut module in output.modules {
        eprintln!("[emit] transforming module: {}", module.name);
        module.external_type_defs = external_type_defs.clone();
        module.external_function_sigs = external_function_sigs.clone();
        let module = pipeline.run(module).map_err(|e| anyhow::anyhow!("{e}"))?;
        modules.push(module);
    }
    eprintln!("[emit] transforms done, linking...");

    // Cross-module linking: validate all imports resolve.
    Linker::link(&modules).map_err(|errors| {
        let msgs: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
        anyhow::anyhow!("linking failed:\n  {}", msgs.join("\n  "))
    })?;
    eprintln!("[emit] linking done, emitting...");

    for target in &manifest.targets {
        let backend = find_backend(&target.backend);
        let Some(backend) = backend else {
            bail!("no backend available for {:?}", target.backend);
        };

        let input = BackendInput {
            modules: modules.clone(),
            assets: output.assets.clone(),
            output_dir: target.output_dir.clone(),
            lowering_config: lowering_config.clone(),
            runtime: resolve_runtime(&manifest.engine, &target.backend),
        };
        eprintln!("[emit] emitting to {}...", target.output_dir.display());
        backend
            .emit(input)
            .map_err(|e| anyhow::anyhow!("{e}"))?;
        println!(
            "Emitted {} output to {}",
            backend.name(),
            target.output_dir.display()
        );
    }

    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Command::Info { manifest } => cmd_info(manifest),
        Command::PrintIr { file } => cmd_print_ir(file),
        Command::Extract {
            manifest,
            skip_passes,
        } => cmd_extract(manifest, skip_passes),
        Command::Emit {
            manifest,
            skip_passes,
            preset,
        } => cmd_emit(manifest, skip_passes, preset),
    }
}
