use std::fs::{self, File};
use std::io::BufReader;
use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use reincarnate_core::ir::Module;
use reincarnate_core::pipeline::{Backend, BackendInput, DebugConfig, Frontend, FrontendInput, Linker, PassConfig, Preset, RuntimePackage};
use reincarnate_core::project::{AssetMapping, EngineOrigin, ProjectManifest, TargetBackend};
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
        /// Dump post-transform IR to stderr before structurization.
        #[arg(long)]
        dump_ir: bool,
        /// Dump raw AST to stderr before AST-to-AST passes.
        #[arg(long)]
        dump_ast: bool,
        /// Filter IR/AST dumps to functions whose name contains this substring.
        #[arg(long = "dump-function")]
        dump_function: Option<String>,
    },
}

/// Find `reincarnate.json` by walking up from `start` through ancestor directories.
/// Returns the first path that exists, or `None`.
fn find_manifest_upward(start: &Path) -> Option<PathBuf> {
    let mut dir = if start.is_dir() {
        start.to_path_buf()
    } else {
        start.parent()?.to_path_buf()
    };
    loop {
        let candidate = dir.join("reincarnate.json");
        if candidate.exists() {
            return Some(candidate);
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// Resolve the manifest path: if the given path exists, use it directly;
/// otherwise search ancestor directories from cwd.
fn resolve_manifest_path(path: &Path) -> Result<PathBuf> {
    if path.exists() {
        return Ok(path.to_path_buf());
    }
    // Only search ancestors when using the default filename.
    if path.file_name().and_then(|f| f.to_str()) == Some("reincarnate.json") {
        let cwd = std::env::current_dir().context("failed to get current directory")?;
        if let Some(found) = find_manifest_upward(&cwd) {
            eprintln!("[manifest] found {}", found.display());
            return Ok(found);
        }
    }
    bail!("manifest not found: {}", path.display())
}

fn load_manifest(path: &Path) -> Result<ProjectManifest> {
    let path = resolve_manifest_path(path)?;
    let file = File::open(&path).with_context(|| format!("failed to open manifest: {}", path.display()))?;
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
        for asset in &mut manifest.assets {
            if asset.src.is_relative() {
                asset.src = base.join(&asset.src);
            }
        }
    }

    Ok(manifest)
}

/// Recursively copy a directory tree from `src` to `dst`.
fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<u64> {
    let mut count = 0u64;
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src).with_context(|| format!("reading asset dir: {}", src.display()))? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let dest = dst.join(entry.file_name());
        if ty.is_dir() {
            count += copy_dir_recursive(&entry.path(), &dest)?;
        } else {
            fs::copy(entry.path(), &dest)?;
            count += 1;
        }
    }
    Ok(count)
}

/// Copy manifest-declared assets into each target output directory.
fn copy_manifest_assets(assets: &[AssetMapping], output_dir: &Path) -> Result<()> {
    for mapping in assets {
        if !mapping.src.exists() {
            eprintln!(
                "[warn] manifest asset not found, skipping: {}",
                mapping.src.display()
            );
            continue;
        }
        let dest = output_dir.join(mapping.dest_name());
        if mapping.src.is_dir() {
            let count = copy_dir_recursive(&mapping.src, &dest)?;
            eprintln!(
                "[emit] copied {} files from {} -> {}",
                count,
                mapping.src.display(),
                dest.display()
            );
        } else {
            if let Some(parent) = dest.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::copy(&mapping.src, &dest)?;
            eprintln!("[emit] copied {} -> {}", mapping.src.display(), dest.display());
        }
    }
    Ok(())
}

fn find_frontend(engine: &EngineOrigin) -> Option<Box<dyn Frontend>> {
    match engine {
        EngineOrigin::Flash => Some(Box::new(reincarnate_frontend_flash::FlashFrontend)),
        EngineOrigin::GameMaker => Some(Box::new(reincarnate_frontend_gamemaker::GameMakerFrontend)),
        EngineOrigin::Twine => Some(Box::new(reincarnate_frontend_twine::TwineFrontend)),
        _ => None,
    }
}

/// In release builds, embed the entire runtime/ tree so the binary is self-contained.
#[cfg(not(debug_assertions))]
static EMBEDDED_RUNTIME: include_dir::Dir = include_dir::include_dir!("$CARGO_MANIFEST_DIR/../../runtime");

/// Release: extract embedded runtime to a temp directory.
#[cfg(not(debug_assertions))]
fn runtime_base_dir() -> Option<PathBuf> {
    let tmp = std::env::temp_dir().join("reincarnate-runtime");
    EMBEDDED_RUNTIME.extract(&tmp).ok()?;
    Some(tmp)
}

/// Debug: walk up from the executable to find the workspace root's `runtime/` dir.
#[cfg(debug_assertions)]
fn runtime_base_dir() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let mut dir = exe.parent()?;
    loop {
        let cargo_toml = dir.join("Cargo.toml");
        if cargo_toml.exists() {
            let content = std::fs::read_to_string(&cargo_toml).ok()?;
            if content.contains("[workspace]") {
                return Some(dir.join("runtime"));
            }
        }
        dir = dir.parent()?;
    }
}

/// Resolve the engine-specific runtime package.
///
/// In debug builds, locates `runtime/` by walking up from the executable to find
/// the workspace root. In release builds, extracts embedded runtime files to a
/// temp directory. Then loads `runtime.json` from `{engine}/{lang}/`.
///
/// When `variant` is `Some("harlowe")`, tries `runtime.harlowe.json` first,
/// falling back to `runtime.json`. This lets a single engine directory serve
/// multiple sub-formats with different scaffold and system module configs.
fn resolve_runtime(engine: &EngineOrigin, backend: &TargetBackend, variant: Option<&str>) -> Option<RuntimePackage> {
    let engine_name = match engine {
        EngineOrigin::Flash => "flash",
        EngineOrigin::GameMaker => "gamemaker",
        EngineOrigin::Twine => "twine",
        _ => return None,
    };
    let lang = match backend {
        TargetBackend::TypeScript => "ts",
        _ => return None,
    };
    let runtime_base = runtime_base_dir()?;
    let source_dir = runtime_base.join(engine_name).join(lang);
    if !source_dir.is_dir() {
        return None;
    }
    // Try variant-specific config first (e.g. runtime.harlowe.json),
    // then fall back to runtime.json.
    let config_path = if let Some(v) = variant {
        let variant_path = source_dir.join(format!("runtime.{v}.json"));
        if variant_path.exists() { variant_path } else { source_dir.join("runtime.json") }
    } else {
        source_dir.join("runtime.json")
    };
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

fn cmd_info(manifest_path: &Path) -> Result<()> {
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

fn cmd_print_ir(file: &Path) -> Result<()> {
    let f = File::open(file).with_context(|| format!("failed to open IR file: {}", file.display()))?;
    let reader = BufReader::new(f);
    let module: Module =
        serde_json::from_reader(reader).with_context(|| format!("failed to parse IR file: {}", file.display()))?;
    println!("{module}");
    Ok(())
}

fn cmd_extract(manifest_path: &Path, skip_passes: &[String]) -> Result<()> {
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
        options: manifest.frontend_options.clone(),
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

fn cmd_emit(manifest_path: &Path, skip_passes: &[String], preset: &str, debug: &DebugConfig) -> Result<()> {
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
        options: manifest.frontend_options.clone(),
    };
    let output = frontend
        .extract(input)
        .map_err(|e| anyhow::anyhow!("{e}"))?;

    // Resolve runtime early so we can attach type_definitions to modules
    // before running transforms (type inference needs them).
    let runtime_variant = output.runtime_variant.as_deref();
    let first_backend = manifest.targets.first().map(|t| &t.backend);
    let early_runtime = first_backend.and_then(|b| resolve_runtime(&manifest.engine, b, runtime_variant));
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
            runtime: resolve_runtime(&manifest.engine, &target.backend, runtime_variant),
            debug: debug.clone(),
            persistence: manifest.persistence.clone(),
        };
        eprintln!("[emit] emitting to {}...", target.output_dir.display());
        backend
            .emit(input)
            .map_err(|e| anyhow::anyhow!("{e}"))?;

        // Copy manifest-declared asset directories into the output.
        if !manifest.assets.is_empty() {
            copy_manifest_assets(&manifest.assets, &target.output_dir)?;
        }

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
            dump_ir,
            dump_ast,
            dump_function,
        } => {
            let debug = DebugConfig {
                dump_ir: *dump_ir,
                dump_ast: *dump_ast,
                function_filter: dump_function.clone(),
            };
            cmd_emit(manifest, skip_passes, preset, &debug)
        }
    }
}
