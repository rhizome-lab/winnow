use std::collections::hash_map::DefaultHasher;
use std::fs::{self, File};
use std::hash::{Hash, Hasher};
use std::io::BufReader;
use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use reincarnate_core::ir::Module;
use std::collections::HashMap;
use reincarnate_core::pipeline::{Backend, BackendInput, Checker, CheckerInput, CheckerOutput, CheckSummary, DebugConfig, Diagnostic, Frontend, FrontendInput, Linker, PassConfig, PipelineOutput, Preset, RuntimePackage, VALID_PASS_NAMES};
use reincarnate_core::project::{AssetMapping, EngineOrigin, ProjectManifest, TargetBackend};
#[cfg(feature = "checker-typescript")]
use reincarnate_checker_typescript::TsChecker;
use reincarnate_core::transforms::default_pipeline;

mod registry;
use registry::{load_registry, now_iso8601, read_engine_from_manifest, save_registry, ProjectEntry};

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
        /// Registry name, path to manifest file, or directory containing reincarnate.json.
        /// Defaults to searching ancestor directories from the current directory.
        #[arg(conflicts_with = "manifest")]
        target: Option<String>,
        /// Path to the project manifest (legacy flag; prefer positional target).
        #[arg(long, conflicts_with = "target")]
        manifest: Option<PathBuf>,
    },
    /// Print a JSON-serialized IR module in human-readable form.
    PrintIr {
        /// Path to a JSON IR module file.
        file: PathBuf,
    },
    /// Extract IR from a project's source files.
    Extract {
        /// Registry name, path to manifest file, or directory containing reincarnate.json.
        /// Defaults to searching ancestor directories from the current directory.
        #[arg(conflicts_with = "manifest")]
        target: Option<String>,
        /// Path to the project manifest (legacy flag; prefer positional target).
        #[arg(long, conflicts_with = "target")]
        manifest: Option<PathBuf>,
        /// Transform passes to skip (e.g. "type-inference", "constant-folding").
        #[arg(long = "skip-pass")]
        skip_passes: Vec<String>,
    },
    /// Run the full pipeline: extract, transform, and emit target code.
    Emit {
        /// Registry name, path to manifest file, or directory containing reincarnate.json.
        /// Defaults to searching ancestor directories from the current directory.
        /// Conflicts with --manifest and --all.
        #[arg(conflicts_with_all = ["manifest", "all"])]
        target: Option<String>,
        /// Path to the project manifest (legacy flag; prefer positional target).
        #[arg(long, conflicts_with_all = ["target", "all"])]
        manifest: Option<PathBuf>,
        /// Emit all registered projects sequentially.
        #[arg(long, conflicts_with_all = ["target", "manifest"])]
        all: bool,
        /// Run --all projects concurrently (caution: high memory usage).
        #[arg(long, requires = "all")]
        parallel: bool,
        /// Transform passes to skip on top of the preset.
        #[arg(long = "skip-pass")]
        skip_passes: Vec<String>,
        /// Pipeline preset: "literal" (1:1 translation) or "optimized" (default).
        #[arg(long, default_value = "optimized")]
        preset: String,
        /// Enable fixpoint iteration: repeat all passes until stable.
        #[arg(long)]
        fixpoint: bool,
        /// Dump post-transform IR to stderr before structurization.
        #[arg(long)]
        dump_ir: bool,
        /// Dump raw AST to stderr before AST-to-AST passes.
        #[arg(long)]
        dump_ast: bool,
        /// Filter IR/AST dumps to functions whose name contains this substring.
        #[arg(long = "dump-function")]
        dump_function: Option<String>,
        /// Run transforms up through the named pass, dump IR, then exit without
        /// emitting code. Pass name is kebab-case (e.g. "type-inference",
        /// "mem2reg"). Use "frontend" to dump raw IR before any transforms.
        #[arg(long = "dump-ir-after")]
        dump_ir_after: Option<String>,
    },
    /// Emit and type-check the output.
    Check {
        /// Registry name, path to manifest file, or directory containing reincarnate.json.
        /// Conflicts with --manifest and --all.
        #[arg(conflicts_with_all = ["manifest", "all"])]
        target: Option<String>,
        /// Path to the project manifest (legacy flag; prefer positional target).
        #[arg(long, conflicts_with_all = ["target", "all"])]
        manifest: Option<PathBuf>,
        /// Check all registered projects sequentially.
        #[arg(long, conflicts_with_all = ["target", "manifest"])]
        all: bool,
        /// Skip emission and check existing output.
        #[arg(long)]
        no_emit: bool,
        /// Output results as JSON.
        #[arg(long)]
        json: bool,
        /// Transform passes to skip on top of the preset.
        #[arg(long = "skip-pass")]
        skip_passes: Vec<String>,
        /// Pipeline preset: "literal" (1:1 translation) or "optimized" (default).
        #[arg(long, default_value = "optimized")]
        preset: String,
        /// Save check results as a baseline to the given file.
        #[arg(long)]
        save_baseline: Option<PathBuf>,
        /// Compare check results against a previously saved baseline.
        #[arg(long)]
        baseline: Option<PathBuf>,
        /// Number of representative example messages to show per error code.
        /// 0 = counts only, -1 = show all. Default: 3.
        #[arg(long, default_value = "3")]
        examples: i32,
        /// Only show diagnostics with this error code (e.g. TS2345); case-insensitive.
        #[arg(long = "filter-code")]
        filter_code: Option<String>,
        /// Only show diagnostics where the file path contains this substring.
        #[arg(long = "filter-file")]
        filter_file: Option<String>,
        /// Only show diagnostics where the message contains this substring; case-insensitive.
        #[arg(long = "filter-message")]
        filter_message: Option<String>,
    },
    /// Add a project to the registry.
    Add {
        /// Path to manifest file, directory containing reincarnate.json, or omit to
        /// search ancestor directories from the current directory.
        path: Option<PathBuf>,
        /// Registry name (defaults to the parent directory name of the manifest).
        name: Option<String>,
        /// Overwrite an existing entry with the same name.
        #[arg(long)]
        force: bool,
    },
    /// Remove a project from the registry.
    Remove {
        /// Registry name to remove.
        name: String,
    },
    /// List registered projects.
    List {
        /// Sort order.
        #[arg(long, value_enum, default_value = "name")]
        sort: SortOrder,
        /// Output as JSON array (one object per project).
        #[arg(long)]
        json: bool,
    },
    /// Run the transform pipeline N times and report whether it reaches fixpoint or oscillates.
    Stress {
        /// Registry name, path to manifest file, or directory containing reincarnate.json.
        /// Defaults to searching ancestor directories from the current directory.
        #[arg(conflicts_with = "manifest")]
        target: Option<String>,
        /// Path to the project manifest (legacy flag; prefer positional target).
        #[arg(long, conflicts_with = "target")]
        manifest: Option<PathBuf>,
        /// Number of pipeline runs to perform (default: 5).
        #[arg(long, default_value = "5")]
        runs: usize,
        /// Transform passes to skip (e.g. "type-inference", "constant-folding").
        #[arg(long = "skip-pass")]
        skip_passes: Vec<String>,
        /// Pipeline preset: "literal" (1:1 translation) or "optimized" (default).
        #[arg(long, default_value = "optimized")]
        preset: String,
    },
    /// List all IR function names in a project (useful for verifying --dump-function strings).
    ListFunctions {
        /// Registry name, path to manifest file, or directory containing reincarnate.json.
        /// Defaults to searching ancestor directories from the current directory.
        #[arg(conflicts_with = "manifest")]
        target: Option<String>,
        /// Path to the project manifest (legacy flag; prefer positional target).
        #[arg(long, conflicts_with = "target")]
        manifest: Option<PathBuf>,
        /// Only show function names matching this filter (same logic as --dump-function:
        /// case-insensitive substring, or split-part matching on `.`/`::` separators).
        #[arg(long)]
        filter: Option<String>,
    },
}

#[derive(clap::ValueEnum, Clone)]
enum SortOrder {
    Name,
    Engine,
    LastEmitted,
}

// ---------------------------------------------------------------------------
// Manifest resolution helpers
// ---------------------------------------------------------------------------

/// Find `reincarnate.json` by walking up from `start` through ancestor directories.
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

/// Resolve a path argument to an existing manifest file.
/// - If the path is a `.json` file that exists, return it directly.
/// - If it is a directory, look for `reincarnate.json` inside it.
/// - If using the default filename and not found, search ancestor directories.
fn resolve_manifest_path(path: &Path) -> Result<PathBuf> {
    if path.exists() && path.is_file() {
        return Ok(path.to_path_buf());
    }
    if path.is_dir() {
        let candidate = path.join("reincarnate.json");
        if candidate.exists() {
            return Ok(candidate);
        }
        bail!("no reincarnate.json found in directory: {}", path.display());
    }
    // Only ancestor-search when using the default filename (no explicit path given).
    if path.file_name().and_then(|f| f.to_str()) == Some("reincarnate.json") {
        let cwd = std::env::current_dir().context("failed to get current directory")?;
        if let Some(found) = find_manifest_upward(&cwd) {
            eprintln!("[manifest] found {}", found.display());
            return Ok(found);
        }
    }
    bail!("manifest not found: {}", path.display())
}

/// Resolve the manifest for commands that accept a positional target (name or path)
/// plus an optional legacy `--manifest` flag.
///
/// Resolution order:
/// 1. `--manifest <path>` → resolve directly as a path.
/// 2. Positional `target` → if it looks like a path (contains `/`, or exists on disk),
///    resolve as a path; otherwise try as a registry name.
/// 3. Neither given → ancestor scan from cwd.
fn resolve_target(target: Option<&str>, manifest: Option<&Path>) -> Result<PathBuf> {
    if let Some(m) = manifest {
        return resolve_manifest_path(m);
    }
    if let Some(t) = target {
        let p = Path::new(t);
        // Treat as a path if it exists or looks like one (contains a separator).
        if p.exists() || t.contains('/') || t.contains('\\') {
            return resolve_manifest_path(p);
        }
        // Try as registry name.
        let reg = load_registry()?;
        if let Some(entry) = reg.projects.get(t) {
            return Ok(PathBuf::from(&entry.manifest));
        }
        bail!("'{}' is not a known project name or a valid path", t);
    }
    // Default: ancestor scan.
    resolve_manifest_path(Path::new("reincarnate.json"))
}

fn load_manifest(path: &Path) -> Result<ProjectManifest> {
    let path = path.canonicalize().with_context(|| format!("failed to canonicalize: {}", path.display()))?;
    let file = File::open(&path).with_context(|| format!("failed to open manifest: {}", path.display()))?;
    let reader = BufReader::new(file);
    let mut manifest: ProjectManifest =
        serde_json::from_reader(reader).with_context(|| format!("failed to parse manifest: {}", path.display()))?;

    // Resolve relative paths against the manifest's parent directory.
    if let Some(base) = path.parent() {
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

// ---------------------------------------------------------------------------
// Asset copying
// ---------------------------------------------------------------------------

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

fn copy_manifest_assets(assets: &[AssetMapping], output_dir: &Path) -> Result<()> {
    for mapping in assets {
        if !mapping.src.exists() {
            eprintln!("[warn] manifest asset not found, skipping: {}", mapping.src.display());
            continue;
        }
        let dest = output_dir.join(mapping.dest_name());
        if mapping.src.is_dir() {
            let count = copy_dir_recursive(&mapping.src, &dest)?;
            eprintln!("[emit] copied {} files from {} -> {}", count, mapping.src.display(), dest.display());
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

// ---------------------------------------------------------------------------
// Frontend / backend / runtime resolution
// ---------------------------------------------------------------------------

fn find_frontend(engine: &EngineOrigin) -> Option<Box<dyn Frontend>> {
    match engine {
        #[cfg(feature = "frontend-flash")]
        EngineOrigin::Flash => Some(Box::new(reincarnate_frontend_flash::FlashFrontend)),
        #[cfg(feature = "frontend-gamemaker")]
        EngineOrigin::GameMaker => Some(Box::new(reincarnate_frontend_gamemaker::GameMakerFrontend)),
        #[cfg(feature = "frontend-twine")]
        EngineOrigin::Twine => Some(Box::new(reincarnate_frontend_twine::TwineFrontend)),
        _ => None,
    }
}

/// In release builds, embed the entire runtime/ tree so the binary is self-contained.
#[cfg(not(debug_assertions))]
static EMBEDDED_RUNTIME: include_dir::Dir = include_dir::include_dir!("$CARGO_MANIFEST_DIR/../../runtime");

#[cfg(not(debug_assertions))]
fn runtime_base_dir() -> Option<PathBuf> {
    let tmp = std::env::temp_dir().join("reincarnate-runtime");
    EMBEDDED_RUNTIME.extract(&tmp).ok()?;
    Some(tmp)
}

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
        #[cfg(feature = "backend-typescript")]
        TargetBackend::TypeScript => Some(Box::new(reincarnate_backend_typescript::TypeScriptBackend)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Command implementations
// ---------------------------------------------------------------------------

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
    let Some(frontend) = find_frontend(&manifest.engine) else {
        bail!("no frontend available for engine {:?}", manifest.engine);
    };

    let input = FrontendInput {
        source: manifest.source.clone(),
        engine: manifest.engine.clone(),
        options: manifest.frontend_options.clone(),
    };
    let output = frontend.extract(input).map_err(|e| anyhow::anyhow!("{e}"))?;

    let skip_refs: Vec<&str> = skip_passes.iter().map(|s| s.as_str()).collect();
    let config = PassConfig::from_skip_list(&skip_refs);
    let mut pipeline = default_pipeline(&config);
    for extra in output.extra_passes {
        pipeline.add(extra);
    }
    for module in output.modules {
        let module = pipeline.run(module).map_err(|e| anyhow::anyhow!("{e}"))?;
        println!("{module}");
    }
    Ok(())
}

/// Run emit and return the list of output directories produced.
fn cmd_emit(manifest_path: &Path, skip_passes: &[String], preset: &str, fixpoint: bool, debug: &DebugConfig) -> Result<Vec<PathBuf>> {
    let manifest = load_manifest(manifest_path)?;
    let Some(frontend) = find_frontend(&manifest.engine) else {
        bail!("no frontend available for engine {:?}", manifest.engine);
    };

    let input = FrontendInput {
        source: manifest.source.clone(),
        engine: manifest.engine.clone(),
        options: manifest.frontend_options.clone(),
    };
    let output = frontend.extract(input).map_err(|e| anyhow::anyhow!("{e}"))?;

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
    let (mut pass_config, lowering_config) = Preset::resolve(preset, &skip_refs)
        .ok_or_else(|| anyhow::anyhow!("unknown preset: {preset:?} (valid: \"literal\", \"optimized\")"))?;
    if fixpoint {
        pass_config.fixpoint = true;
    }
    let mut pipeline = default_pipeline(&pass_config);
    for extra in output.extra_passes {
        pipeline.add(extra);
    }

    let mut modules = Vec::new();
    let mut stopped_early = false;
    for mut module in output.modules {
        eprintln!("[emit] transforming module: {}", module.name);
        module.external_type_defs = external_type_defs.clone();
        module.external_function_sigs = external_function_sigs.clone();
        let PipelineOutput { module, stopped_early: early } =
            pipeline.run_with_debug(module, debug).map_err(|e| anyhow::anyhow!("{e}"))?;
        modules.push(module);
        if early {
            stopped_early = true;
            break;
        }
    }

    if stopped_early {
        return Ok(Vec::new());
    }

    eprintln!("[emit] transforms done, linking...");

    Linker::link(&modules).map_err(|errors| {
        let msgs: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
        anyhow::anyhow!("linking failed:\n  {}", msgs.join("\n  "))
    })?;
    eprintln!("[emit] linking done, emitting...");

    let mut output_dirs = Vec::new();
    for target in &manifest.targets {
        let Some(backend) = find_backend(&target.backend) else {
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
        backend.emit(input).map_err(|e| anyhow::anyhow!("{e}"))?;

        if !manifest.assets.is_empty() {
            copy_manifest_assets(&manifest.assets, &target.output_dir)?;
        }

        println!("Emitted {} output to {}", backend.name(), target.output_dir.display());
        output_dirs.push(target.output_dir.clone());
    }

    Ok(output_dirs)
}

/// After a successful emit, update `last_emitted_at` for any registry entry whose
/// manifest path matches the one we just emitted.  Best-effort — does not fail the emit.
fn try_update_last_emitted(manifest_path: &Path) {
    let Ok(mut reg) = load_registry() else { return };
    let manifest_str = manifest_path.to_string_lossy();
    let mut updated = false;
    for entry in reg.projects.values_mut() {
        if entry.manifest == manifest_str.as_ref() {
            entry.last_emitted_at = Some(now_iso8601());
            updated = true;
            break;
        }
    }
    if updated {
        let _ = save_registry(&reg);
    }
}

fn cmd_emit_all(skip_passes: &[String], preset: &str, debug: &DebugConfig) -> Result<()> {
    let reg = load_registry()?;
    if reg.projects.is_empty() {
        println!("No projects in registry. Use `reincarnate add` to register a project.");
        return Ok(());
    }

    let projects: Vec<(&String, &ProjectEntry)> = reg.projects.iter().collect();
    let total = projects.len();
    let mut failures: Vec<(String, anyhow::Error)> = Vec::new();

    for (i, (name, entry)) in projects.iter().enumerate() {
        let engine_label = entry.engine.as_deref().unwrap_or("unknown");
        println!("\n[{}/{}] {} ({})", i + 1, total, name, engine_label);
        println!("  manifest: {}", entry.manifest);

        let manifest_path = PathBuf::from(&entry.manifest);
        match cmd_emit(&manifest_path, skip_passes, preset, false, debug) {
            Ok(_) => {
                try_update_last_emitted(&manifest_path);
            }
            Err(e) => {
                eprintln!("  [error] {e:#}");
                failures.push((name.to_string(), e));
            }
        }
    }

    if failures.is_empty() {
        println!("\nAll {total} project(s) emitted successfully.");
        Ok(())
    } else {
        println!("\n{} of {} project(s) failed:", failures.len(), total);
        for (name, err) in &failures {
            println!("  {name}: {err:#}");
        }
        bail!("{} project(s) failed to emit", failures.len())
    }
}

// ---------------------------------------------------------------------------
// Check command
// ---------------------------------------------------------------------------

/// Options shared by all check invocations.
struct CheckConfig<'a> {
    json: bool,
    save_baseline: Option<&'a Path>,
    baseline: Option<&'a Path>,
    examples: i32,
    filter_code: Option<&'a str>,
    filter_file: Option<&'a str>,
    filter_message: Option<&'a str>,
}

fn find_checker(backend: &TargetBackend) -> Option<Box<dyn Checker>> {
    match backend {
        #[cfg(feature = "checker-typescript")]
        TargetBackend::TypeScript => Some(Box::new(TsChecker)),
        _ => None,
    }
}

/// Collect output dirs from the manifest without emitting.
fn collect_output_dirs(manifest_path: &Path) -> Result<Vec<(PathBuf, TargetBackend)>> {
    let manifest = load_manifest(manifest_path)?;
    Ok(manifest
        .targets
        .iter()
        .map(|t| (t.output_dir.clone(), t.backend.clone()))
        .collect())
}

fn run_checks(
    targets: &[(PathBuf, TargetBackend)],
    cfg: &CheckConfig<'_>,
) -> Result<()> {
    let json = cfg.json;
    let save_baseline = cfg.save_baseline;
    let baseline = cfg.baseline;
    let examples = cfg.examples;
    let filter_code = cfg.filter_code.map(|s| s.to_ascii_lowercase());
    let filter_file = cfg.filter_file;
    let filter_message = cfg.filter_message.map(|s| s.to_ascii_lowercase());
    let has_filters = filter_code.is_some() || filter_file.is_some() || filter_message.is_some();
    let mut all_outputs: Vec<CheckerOutput> = Vec::new();
    let mut has_errors = false;

    for (output_dir, backend) in targets {
        let Some(checker) = find_checker(backend) else {
            eprintln!("[check] no checker for {:?}, skipping", backend);
            continue;
        };

        eprintln!("[check] running {} checker on {}...", checker.name(), output_dir.display());
        let result = checker
            .check(CheckerInput {
                output_dir: output_dir.clone(),
            })
            .map_err(|e| anyhow::anyhow!("{e}"))?;

        if result.summary.total_errors > 0 {
            has_errors = true;
        }
        all_outputs.push(result);
    }

    let summaries: Vec<CheckSummary> = all_outputs.iter().map(|o| o.summary.clone()).collect();

    // Save baseline if requested (always uses unfiltered summaries).
    if let Some(path) = save_baseline {
        let json_str = serde_json::to_string_pretty(&summaries)?;
        fs::write(path, json_str).with_context(|| format!("failed to write baseline: {}", path.display()))?;
        eprintln!("[check] baseline saved to {}", path.display());
    }

    // Load and compare against baseline if requested (always uses unfiltered).
    let baseline_diff = if let Some(path) = baseline {
        let file = File::open(path).with_context(|| format!("failed to open baseline: {}", path.display()))?;
        let reader = BufReader::new(file);
        let old: Vec<CheckSummary> = serde_json::from_reader(reader)
            .with_context(|| format!("failed to parse baseline: {}", path.display()))?;
        Some(compute_baseline_diff(&old, &summaries))
    } else {
        None
    };

    // Helper: does a diagnostic match all active filters?
    let matches_filters = |d: &Diagnostic| -> bool {
        if let Some(fc) = &filter_code {
            if d.code.to_ascii_lowercase() != *fc {
                return false;
            }
        }
        if let Some(ff) = filter_file {
            if !d.file.contains(ff) {
                return false;
            }
        }
        if let Some(fm) = &filter_message {
            if !d.message.to_ascii_lowercase().contains(fm.as_str()) {
                return false;
            }
        }
        true
    };

    if json {
        if has_filters {
            // Filtered JSON: emit only matching diagnostics from each output.
            #[derive(serde::Serialize)]
            struct FilteredOutput<'a> {
                output_dir: &'a str,
                diagnostics: Vec<&'a Diagnostic>,
            }
            let filtered: Vec<FilteredOutput<'_>> = all_outputs
                .iter()
                .map(|o| FilteredOutput {
                    output_dir: &o.summary.output_dir,
                    diagnostics: o.diagnostics.iter().filter(|d| matches_filters(d)).collect(),
                })
                .collect();
            println!("{}", serde_json::to_string_pretty(&filtered)?);
        } else if let Some(diff) = &baseline_diff {
            #[derive(serde::Serialize)]
            struct JsonOutput<'a> {
                summaries: &'a [CheckSummary],
                baseline_diff: &'a BaselineDiff,
            }
            let output = JsonOutput { summaries: &summaries, baseline_diff: diff };
            println!("{}", serde_json::to_string_pretty(&output)?);
        } else {
            println!("{}", serde_json::to_string_pretty(&summaries)?);
        }
    } else {
        for output in &all_outputs {
            let s = &output.summary;
            println!("\n[check] {}", s.output_dir);

            if has_filters {
                // Collect all diagnostics matching the filters.
                let unfiltered_total = output.diagnostics.len();
                let mut matched: Vec<&Diagnostic> =
                    output.diagnostics.iter().filter(|d| matches_filters(d)).collect();
                matched.sort_by(|a, b| a.file.cmp(&b.file).then(a.line.cmp(&b.line)));

                // Build a description of the active filter(s).
                let mut filter_desc = Vec::new();
                if let Some(fc) = cfg.filter_code {
                    filter_desc.push(format!("--filter-code {fc}"));
                }
                if let Some(ff) = filter_file {
                    filter_desc.push(format!("--filter-file {ff}"));
                }
                if let Some(fm) = cfg.filter_message {
                    filter_desc.push(format!("--filter-message {fm}"));
                }
                println!(
                    "Showing {} of {} diagnostics matching {}",
                    matched.len(),
                    unfiltered_total,
                    filter_desc.join(", ")
                );

                if matched.is_empty() {
                    continue;
                }

                // Determine whether to show all or apply --examples limit.
                // When --filter-code is the only active filter (one code), default to showing all.
                let single_code_filter =
                    filter_code.is_some() && filter_file.is_none() && filter_message.is_none();
                let limit: Option<usize> = if examples == 0 {
                    Some(0) // counts only — but there's no "by code" table here, so just skip bodies
                } else if examples < 0 || single_code_filter {
                    None // show all
                } else {
                    Some(examples as usize)
                };

                if examples == 0 {
                    // Just show the count header (already printed above), no detail.
                    continue;
                }

                let mut seen_messages: std::collections::HashSet<&str> =
                    std::collections::HashSet::new();
                let mut shown = 0usize;
                for diag in &matched {
                    if !seen_messages.insert(diag.message.as_str()) {
                        continue;
                    }
                    println!(
                        "  {}:{}:{} [{}] \u{2013} {}",
                        diag.file, diag.line, diag.col, diag.code, diag.message
                    );
                    shown += 1;
                    if let Some(lim) = limit {
                        if shown >= lim {
                            let remaining = matched.len() - shown;
                            if remaining > 0 {
                                println!("  ... and {} more (use --examples -1 to show all)", remaining);
                            }
                            break;
                        }
                    }
                }
                continue;
            }

            // Unfiltered output path (original behavior).
            if s.total_errors == 0 && s.total_warnings == 0 {
                println!("No errors.");
                continue;
            }
            println!(
                "Total: {} error(s), {} warning(s)",
                s.total_errors, s.total_warnings
            );

            if !s.by_code.is_empty() {
                println!("\nBy error code:");
                for (code, count) in &s.by_code {
                    println!("  {count:>5}  {code}");
                    if examples != 0 {
                        // Collect diagnostics for this code, deduplicated by message text,
                        // sorted by file+line for reproducibility.
                        let mut seen_messages: std::collections::HashSet<&str> =
                            std::collections::HashSet::new();
                        let mut sorted: Vec<&Diagnostic> = output
                            .diagnostics
                            .iter()
                            .filter(|d| &d.code == code)
                            .collect();
                        sorted.sort_by(|a, b| {
                            a.file.cmp(&b.file).then(a.line.cmp(&b.line))
                        });
                        let mut shown = 0usize;
                        for diag in &sorted {
                            if !seen_messages.insert(diag.message.as_str()) {
                                continue;
                            }
                            println!(
                                "             {}:{}:{} \u{2013} {}",
                                diag.file, diag.line, diag.col, diag.message
                            );
                            shown += 1;
                            if examples > 0 && shown >= examples as usize {
                                break;
                            }
                        }
                    }
                }
            }

            if !s.by_file.is_empty() {
                let max_files = 20;
                println!("\nBy file (top {max_files}):");
                for (file, count) in s.by_file.iter().take(max_files) {
                    println!("  {count:>5}  {file}");
                }
                if s.by_file.len() > max_files {
                    println!("  ... and {} more file(s)", s.by_file.len() - max_files);
                }
            }
        }

        if let Some(diff) = &baseline_diff {
            print_baseline_diff(diff, baseline.unwrap());
        }
    }

    if let Some(diff) = &baseline_diff {
        if diff.total_delta > 0 {
            bail!(
                "regression detected: {} more error(s) than baseline ({} → {})",
                diff.total_delta, diff.old_total, diff.new_total
            );
        }
    }

    if has_errors && baseline.is_none() {
        // When filters are active, still exit non-zero based on unfiltered total (original behavior
        // is to fail if there are errors). Filters affect display only; the overall pass/fail
        // decision is based on the full result set.
        let total: usize = summaries.iter().map(|s| s.total_errors).sum();
        bail!("type checking found {total} error(s)");
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Baseline comparison
// ---------------------------------------------------------------------------

#[derive(Debug, serde::Serialize)]
struct BaselineDiff {
    old_total: usize,
    new_total: usize,
    total_delta: isize,
    by_code: Vec<(String, usize, usize, isize)>,
    by_file: Vec<(String, usize, usize, isize)>,
}

fn compute_baseline_diff(old: &[CheckSummary], new: &[CheckSummary]) -> BaselineDiff {
    let old_total: usize = old.iter().map(|s| s.total_errors).sum();
    let new_total: usize = new.iter().map(|s| s.total_errors).sum();

    // Aggregate by_code across all summaries.
    let mut old_codes: HashMap<&str, usize> = HashMap::new();
    let mut new_codes: HashMap<&str, usize> = HashMap::new();
    for s in old {
        for (code, count) in &s.by_code {
            *old_codes.entry(code.as_str()).or_default() += count;
        }
    }
    for s in new {
        for (code, count) in &s.by_code {
            *new_codes.entry(code.as_str()).or_default() += count;
        }
    }

    let mut all_codes: Vec<&str> = old_codes.keys().chain(new_codes.keys()).copied().collect();
    all_codes.sort();
    all_codes.dedup();

    let mut by_code: Vec<(String, usize, usize, isize)> = Vec::new();
    for code in &all_codes {
        let o = old_codes.get(code).copied().unwrap_or(0);
        let n = new_codes.get(code).copied().unwrap_or(0);
        if o != n {
            by_code.push((code.to_string(), o, n, n as isize - o as isize));
        }
    }
    by_code.sort_by(|a, b| b.3.unsigned_abs().cmp(&a.3.unsigned_abs()));

    // Aggregate by_file across all summaries.
    let mut old_files: HashMap<&str, usize> = HashMap::new();
    let mut new_files: HashMap<&str, usize> = HashMap::new();
    for s in old {
        for (file, count) in &s.by_file {
            *old_files.entry(file.as_str()).or_default() += count;
        }
    }
    for s in new {
        for (file, count) in &s.by_file {
            *new_files.entry(file.as_str()).or_default() += count;
        }
    }

    let mut all_files: Vec<&str> = old_files.keys().chain(new_files.keys()).copied().collect();
    all_files.sort();
    all_files.dedup();

    let mut by_file: Vec<(String, usize, usize, isize)> = Vec::new();
    for file in &all_files {
        let o = old_files.get(file).copied().unwrap_or(0);
        let n = new_files.get(file).copied().unwrap_or(0);
        if o != n {
            by_file.push((file.to_string(), o, n, n as isize - o as isize));
        }
    }
    by_file.sort_by(|a, b| b.3.unsigned_abs().cmp(&a.3.unsigned_abs()));

    BaselineDiff {
        old_total,
        new_total,
        total_delta: new_total as isize - old_total as isize,
        by_code,
        by_file,
    }
}

fn print_baseline_diff(diff: &BaselineDiff, baseline_path: &Path) {
    println!("\n[check] Comparing against baseline: {}", baseline_path.display());

    let sign = if diff.total_delta > 0 { "+" } else { "" };
    println!(
        "Total: {} → {} errors ({}{})",
        diff.old_total, diff.new_total, sign, diff.total_delta
    );

    if !diff.by_code.is_empty() {
        println!("\nBy error code (changes only):");
        for (code, old, new, delta) in &diff.by_code {
            let s = if *delta > 0 { "+" } else { "" };
            let marker = if *delta > 0 { "  <- regression" } else { "" };
            println!("  {code}: {old} → {new} ({s}{delta}){marker}");
        }
    }

    if !diff.by_file.is_empty() {
        let max_files = 30;
        println!("\nBy file (changes only, top {max_files}):");
        for (file, old, new, delta) in diff.by_file.iter().take(max_files) {
            let s = if *delta > 0 { "+" } else { "" };
            println!("  {s}{delta}  {file} ({old} → {new})");
        }
        if diff.by_file.len() > max_files {
            println!("  ... and {} more file(s)", diff.by_file.len() - max_files);
        }
    }
}

fn cmd_check(
    manifest_path: &Path,
    no_emit: bool,
    skip_passes: &[String],
    preset: &str,
    cfg: &CheckConfig<'_>,
) -> Result<()> {
    let targets = if no_emit {
        collect_output_dirs(manifest_path)?
    } else {
        let debug = DebugConfig::default();
        let output_dirs = cmd_emit(manifest_path, skip_passes, preset, false, &debug)?;
        try_update_last_emitted(manifest_path);
        // Pair output dirs with backends from the manifest.
        let manifest = load_manifest(manifest_path)?;
        output_dirs
            .into_iter()
            .zip(manifest.targets.iter().map(|t| t.backend.clone()))
            .collect()
    };

    run_checks(&targets, cfg)
}

fn cmd_check_all(
    no_emit: bool,
    skip_passes: &[String],
    preset: &str,
    cfg: &CheckConfig<'_>,
) -> Result<()> {
    let reg = load_registry()?;
    if reg.projects.is_empty() {
        println!("No projects in registry. Use `reincarnate add` to register a project.");
        return Ok(());
    }

    let projects: Vec<(&String, &ProjectEntry)> = reg.projects.iter().collect();
    let total = projects.len();
    let mut failures: Vec<(String, anyhow::Error)> = Vec::new();

    for (i, (name, entry)) in projects.iter().enumerate() {
        let engine_label = entry.engine.as_deref().unwrap_or("unknown");
        println!("\n[{}/{}] {} ({})", i + 1, total, name, engine_label);

        let manifest_path = PathBuf::from(&entry.manifest);
        if let Err(e) = cmd_check(&manifest_path, no_emit, skip_passes, preset, cfg) {
            eprintln!("  [error] {e:#}");
            failures.push((name.to_string(), e));
        }
    }

    if failures.is_empty() {
        println!("\nAll {total} project(s) checked successfully.");
        Ok(())
    } else {
        println!("\n{} of {} project(s) had errors:", failures.len(), total);
        for (name, err) in &failures {
            println!("  {name}: {err:#}");
        }
        bail!("{} project(s) had check errors", failures.len())
    }
}

fn cmd_list_functions(manifest_path: &Path, filter: Option<&str>) -> Result<()> {
    let manifest = load_manifest(manifest_path)?;
    let Some(frontend) = find_frontend(&manifest.engine) else {
        bail!("no frontend available for engine {:?}", manifest.engine);
    };

    let input = FrontendInput {
        source: manifest.source.clone(),
        engine: manifest.engine.clone(),
        options: manifest.frontend_options.clone(),
    };
    let output = frontend.extract(input).map_err(|e| anyhow::anyhow!("{e}"))?;

    let debug_config = DebugConfig {
        dump_ir: false,
        dump_ast: false,
        function_filter: filter.map(|s| s.to_string()),
        dump_ir_after: None,
    };

    for module in &output.modules {
        for (_, func) in module.functions.iter() {
            if debug_config.should_dump(&func.name) {
                println!("{}", func.name);
            }
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Stress command
// ---------------------------------------------------------------------------

/// Hash a string using `DefaultHasher` and return the u64 hash value.
fn hash_str(s: &str) -> u64 {
    let mut h = DefaultHasher::new();
    s.hash(&mut h);
    h.finish()
}

/// Count how many functions differ between two serialized modules.
///
/// Both maps are `function_name → JSON string`.  Returns the number of
/// function names whose serialized form changed (including functions that
/// appeared or disappeared).
fn count_differing_functions(
    prev: &HashMap<String, String>,
    curr: &HashMap<String, String>,
) -> usize {
    let mut count = 0usize;
    for (name, curr_json) in curr {
        match prev.get(name) {
            Some(prev_json) if prev_json == curr_json => {}
            _ => count += 1,
        }
    }
    // Functions present in prev but absent in curr also differ.
    for name in prev.keys() {
        if !curr.contains_key(name) {
            count += 1;
        }
    }
    count
}

/// Serialize each function in the module to its own JSON string, returning a
/// map from function name to JSON.
fn serialize_functions(module: &Module) -> Result<HashMap<String, String>> {
    let mut map = HashMap::new();
    for func in module.functions.values() {
        let json = serde_json::to_string(func)?;
        map.insert(func.name.clone(), json);
    }
    Ok(map)
}

fn cmd_stress(
    manifest_path: &Path,
    runs: usize,
    skip_passes: &[String],
    preset: &str,
) -> Result<()> {
    if runs == 0 {
        bail!("--runs must be at least 1");
    }

    let manifest = load_manifest(manifest_path)?;
    let Some(frontend) = find_frontend(&manifest.engine) else {
        bail!("no frontend available for engine {:?}", manifest.engine);
    };

    let skip_refs: Vec<&str> = skip_passes.iter().map(|s| s.as_str()).collect();
    let (pass_config, _lowering_config) = Preset::resolve(preset, &skip_refs)
        .ok_or_else(|| anyhow::anyhow!("unknown preset: {preset:?} (valid: \"literal\", \"optimized\")"))?;

    // Extract the initial modules once.
    let initial_input = FrontendInput {
        source: manifest.source.clone(),
        engine: manifest.engine.clone(),
        options: manifest.frontend_options.clone(),
    };
    let initial_output = frontend.extract(initial_input).map_err(|e| anyhow::anyhow!("{e}"))?;

    let runtime_variant = initial_output.runtime_variant.as_deref();
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

    let debug = DebugConfig::none();

    // Add external defs and collect module names before consuming initial_output.
    let module_names: Vec<String> = initial_output.modules.iter().map(|m| m.name.clone()).collect();
    let mut initial_modules: Vec<Module> = initial_output.modules;
    // extra_passes from the initial extraction are consumed by run 0's pipeline.
    // For runs 1+, we re-extract to obtain fresh Box<dyn Transform> instances
    // (they are not Clone).  We discard the re-extracted modules.
    let mut first_extra_passes: Option<Vec<Box<dyn reincarnate_core::pipeline::Transform>>> =
        Some(initial_output.extra_passes);

    for module in &mut initial_modules {
        module.external_type_defs = external_type_defs.clone();
        module.external_function_sigs = external_function_sigs.clone();
    }

    // Stress-test each module independently; collect overall result.
    let mut any_not_converged = false;

    for (mod_idx, module_name) in module_names.iter().enumerate() {
        eprintln!("[stress] module: {module_name}");

        let mut module = std::mem::replace(
            &mut initial_modules[mod_idx],
            Module::new(String::new()),
        );

        // History of (whole-module hash, per-function JSON map) for each run.
        let mut run_hashes: Vec<u64> = Vec::with_capacity(runs);
        let mut run_func_maps: Vec<HashMap<String, String>> = Vec::with_capacity(runs);

        let mut fixpoint_run: Option<usize> = None;

        for run_idx in 0..runs {
            let run_num = run_idx + 1;

            // Obtain extra_passes for this run.  On the very first module's first
            // run we use the pool from the initial extract; on every other run we
            // re-extract to get fresh Box<dyn Transform> instances.
            let extra_passes: Vec<Box<dyn reincarnate_core::pipeline::Transform>> =
                if run_idx == 0 && mod_idx == 0 {
                    first_extra_passes.take().unwrap_or_default()
                } else {
                    let re_input = FrontendInput {
                        source: manifest.source.clone(),
                        engine: manifest.engine.clone(),
                        options: manifest.frontend_options.clone(),
                    };
                    frontend
                        .extract(re_input)
                        .map_err(|e| anyhow::anyhow!("{e}"))?
                        .extra_passes
                };

            // Build a fresh pipeline for this run.
            let mut pipeline = reincarnate_core::transforms::default_pipeline(&pass_config);
            for extra in extra_passes {
                pipeline.add(extra);
            }

            let PipelineOutput { module: transformed, stopped_early: _ } =
                pipeline.run_with_debug(module, &debug).map_err(|e| anyhow::anyhow!("{e}"))?;
            module = transformed;

            let module_json = serde_json::to_string(&module)?;
            let module_hash = hash_str(&module_json);
            let func_map = serialize_functions(&module)?;

            eprint!("[stress] run {run_num}/{runs} — ");

            // Check against all previous runs for oscillation / fixpoint.
            let mut matched_run: Option<usize> = None;
            for (prev_idx, &prev_hash) in run_hashes.iter().enumerate() {
                if prev_hash == module_hash {
                    matched_run = Some(prev_idx + 1);
                    break;
                }
            }

            if run_idx == 0 {
                eprintln!("hashing IR...");
            } else if let Some(matched) = matched_run {
                if matched == run_idx {
                    // Identical to the immediately preceding run — fixpoint.
                    eprintln!("identical to run {matched}. Fixpoint reached after {} run(s).", run_idx);
                    fixpoint_run = Some(run_idx);
                    run_hashes.push(module_hash);
                    run_func_maps.push(func_map);
                    break;
                } else {
                    // Matches an earlier run but not the immediately preceding one — oscillating.
                    eprintln!("same as run {matched} — oscillating!");
                }
            } else {
                // Different from all previous runs.
                let prev_func_map = &run_func_maps[run_idx - 1];
                let diff_count = count_differing_functions(prev_func_map, &func_map);
                if diff_count > 0 {
                    eprintln!("changed ({diff_count} function(s) differ)");
                } else {
                    eprintln!("changed");
                }
            }

            run_hashes.push(module_hash);
            run_func_maps.push(func_map);
        }

        if fixpoint_run.is_none() {
            eprintln!("[stress] WARNING: pipeline did not reach fixpoint after {runs} run(s)");
            any_not_converged = true;
        }
    }

    if any_not_converged {
        bail!("oscillating or slow to converge after {runs} runs");
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Registry commands
// ---------------------------------------------------------------------------

/// Resolve the manifest path for `add`: accepts a directory, a .json file, or None
/// (ancestor scan from cwd).
fn resolve_add_manifest(path: Option<&Path>) -> Result<PathBuf> {
    match path {
        Some(p) if p.is_file() => Ok(p.to_path_buf()),
        Some(p) if p.is_dir() => {
            let candidate = p.join("reincarnate.json");
            if candidate.exists() {
                Ok(candidate)
            } else {
                bail!("no reincarnate.json found in directory: {}", p.display());
            }
        }
        Some(p) if p.extension().map(|e| e == "json").unwrap_or(false) => {
            bail!("manifest file not found: {}", p.display())
        }
        Some(p) => {
            bail!("'{}' is not a directory or .json file", p.display())
        }
        None => {
            let cwd = std::env::current_dir().context("failed to get current directory")?;
            find_manifest_upward(&cwd)
                .ok_or_else(|| anyhow::anyhow!("no reincarnate.json found in current directory or ancestors"))
        }
    }
}

fn cmd_add(path: Option<&Path>, name: Option<&str>, force: bool) -> Result<()> {
    let manifest_path = resolve_add_manifest(path)?;
    let manifest_abs = manifest_path
        .canonicalize()
        .with_context(|| format!("failed to canonicalize: {}", manifest_path.display()))?;

    // Derive default name from the manifest's parent directory.
    let default_name = manifest_abs
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .ok_or_else(|| anyhow::anyhow!("could not derive project name from manifest path"))?
        .to_string();
    let project_name = name.unwrap_or(&default_name);

    let engine = read_engine_from_manifest(&manifest_abs);

    let mut reg = load_registry()?;
    if reg.projects.contains_key(project_name) && !force {
        bail!(
            "project '{}' already exists in registry — use --force to overwrite",
            project_name
        );
    }

    reg.projects.insert(
        project_name.to_string(),
        ProjectEntry {
            manifest: manifest_abs.to_string_lossy().into_owned(),
            engine: engine.clone(),
            added_at: now_iso8601(),
            last_emitted_at: None,
        },
    );
    save_registry(&reg)?;

    println!(
        "Added '{}' ({}) → {}",
        project_name,
        engine.as_deref().unwrap_or("unknown engine"),
        manifest_abs.display()
    );
    Ok(())
}

fn cmd_remove(name: &str) -> Result<()> {
    let mut reg = load_registry()?;
    if reg.projects.remove(name).is_none() {
        bail!("no project named '{}' in registry", name);
    }
    save_registry(&reg)?;
    println!("Removed '{name}' from registry.");
    Ok(())
}

fn cmd_list(sort: &SortOrder, json: bool) -> Result<()> {
    let reg = load_registry()?;

    if reg.projects.is_empty() {
        if json {
            println!("[]");
        } else {
            println!("No projects registered. Use `reincarnate add` to register a project.");
        }
        return Ok(());
    }

    // Collect and sort entries.
    let mut entries: Vec<(&String, &ProjectEntry)> = reg.projects.iter().collect();
    match sort {
        SortOrder::Name => {} // BTreeMap iteration is already alphabetical.
        SortOrder::Engine => entries.sort_by(|(_, a), (_, b)| {
            a.engine.as_deref().unwrap_or("").cmp(b.engine.as_deref().unwrap_or(""))
        }),
        SortOrder::LastEmitted => entries.sort_by(|(_, a), (_, b)| {
            b.last_emitted_at.as_deref().unwrap_or("").cmp(a.last_emitted_at.as_deref().unwrap_or(""))
        }),
    }

    if json {
        // Emit a JSON array for scripting.
        #[derive(serde::Serialize)]
        struct JsonEntry<'a> {
            name: &'a str,
            engine: Option<&'a str>,
            manifest: &'a str,
            added_at: &'a str,
            last_emitted_at: Option<&'a str>,
        }
        let json_entries: Vec<_> = entries
            .iter()
            .map(|(name, e)| JsonEntry {
                name,
                engine: e.engine.as_deref(),
                manifest: &e.manifest,
                added_at: &e.added_at,
                last_emitted_at: e.last_emitted_at.as_deref(),
            })
            .collect();
        println!("{}", serde_json::to_string_pretty(&json_entries)?);
        return Ok(());
    }

    // Compute column widths.
    let col_name_w = entries.iter().map(|(n, _)| n.len()).max().unwrap_or(4).max(4);
    let col_engine_w = entries
        .iter()
        .map(|(_, e)| e.engine.as_deref().unwrap_or("-").len())
        .max()
        .unwrap_or(6)
        .max(6);
    let col_emitted_w = "LAST EMITTED".len();

    // Header.
    println!(
        "{:<col_name_w$}  {:<col_engine_w$}  {:<col_emitted_w$}  MANIFEST",
        "NAME", "ENGINE", "LAST EMITTED"
    );
    println!(
        "{}  {}  {}  {}",
        "-".repeat(col_name_w),
        "-".repeat(col_engine_w),
        "-".repeat(col_emitted_w),
        "-".repeat(40)
    );

    for (name, entry) in &entries {
        let engine = entry.engine.as_deref().unwrap_or("-");
        // Trim ISO 8601 timestamp to a readable form: "2026-02-25 12:34" (drop seconds + tz).
        let last_emitted = entry
            .last_emitted_at
            .as_deref()
            .map(|s| s.get(..16).unwrap_or(s).replace('T', " "))
            .unwrap_or_else(|| "never".to_string());
        println!(
            "{:<col_name_w$}  {:<col_engine_w$}  {:<col_emitted_w$}  {}",
            name, engine, last_emitted, entry.manifest
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

fn main() -> Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Command::Info { target, manifest } => {
            let path = resolve_target(target.as_deref(), manifest.as_deref())?;
            cmd_info(&path)
        }
        Command::PrintIr { file } => cmd_print_ir(file),
        Command::Extract { target, manifest, skip_passes } => {
            let path = resolve_target(target.as_deref(), manifest.as_deref())?;
            cmd_extract(&path, skip_passes)
        }
        Command::Emit { target, manifest, all, parallel: _, skip_passes, preset, fixpoint, dump_ir, dump_ast, dump_function, dump_ir_after } => {
            // Validate --dump-ir-after pass name early so the error is clear.
            if let Some(pass) = dump_ir_after.as_deref() {
                if !VALID_PASS_NAMES.contains(&pass) {
                    bail!(
                        "unknown pass {:?} for --dump-ir-after\nValid pass names: {}",
                        pass,
                        VALID_PASS_NAMES.join(", ")
                    );
                }
            }
            let debug = DebugConfig {
                dump_ir: *dump_ir,
                dump_ast: *dump_ast,
                function_filter: dump_function.clone(),
                dump_ir_after: dump_ir_after.clone(),
            };
            if *all {
                cmd_emit_all(skip_passes, preset, &debug)
            } else {
                let path = resolve_target(target.as_deref(), manifest.as_deref())?;
                let result = cmd_emit(&path, skip_passes, preset, *fixpoint, &debug);
                if result.is_ok() {
                    try_update_last_emitted(&path);
                }
                result.map(|_| ())
            }
        }
        Command::Check { target, manifest, all, no_emit, json, skip_passes, preset, save_baseline, baseline, examples, filter_code, filter_file, filter_message } => {
            let cfg = CheckConfig {
                json: *json,
                save_baseline: save_baseline.as_deref(),
                baseline: baseline.as_deref(),
                examples: *examples,
                filter_code: filter_code.as_deref(),
                filter_file: filter_file.as_deref(),
                filter_message: filter_message.as_deref(),
            };
            if *all {
                cmd_check_all(*no_emit, skip_passes, preset, &cfg)
            } else {
                let path = resolve_target(target.as_deref(), manifest.as_deref())?;
                cmd_check(&path, *no_emit, skip_passes, preset, &cfg)
            }
        }
        Command::Add { path, name, force } => {
            cmd_add(path.as_deref(), name.as_deref(), *force)
        }
        Command::Remove { name } => cmd_remove(name),
        Command::List { sort, json } => cmd_list(sort, *json),
        Command::Stress { target, manifest, runs, skip_passes, preset } => {
            let path = resolve_target(target.as_deref(), manifest.as_deref())?;
            cmd_stress(&path, *runs, skip_passes, preset)
        }
        Command::ListFunctions { target, manifest, filter } => {
            let path = resolve_target(target.as_deref(), manifest.as_deref())?;
            cmd_list_functions(&path, filter.as_deref())
        }
    }
}
