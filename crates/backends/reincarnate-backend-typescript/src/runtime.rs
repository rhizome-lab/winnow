use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;
use reincarnate_core::project::RuntimeConfig;

/// All known generic system names that the runtime provides.
pub const SYSTEM_NAMES: &[&str] = &["renderer", "audio", "input", "timing", "save", "ui"];

/// Recursively copy `src_dir` into `dst_dir`, preserving directory structure.
fn copy_dir_recursive(src_dir: &Path, dst_dir: &Path) -> Result<(), CoreError> {
    fs::create_dir_all(dst_dir)?;
    for entry in fs::read_dir(src_dir)? {
        let entry = entry?;
        let src_path = entry.path();
        let dst_path = dst_dir.join(entry.file_name());
        if src_path.is_dir() {
            copy_dir_recursive(&src_path, &dst_path)?;
        } else {
            fs::copy(&src_path, &dst_path)?;
        }
    }
    Ok(())
}

/// Collect the set of top-level directory names referenced by the config
/// (from system_modules paths and scaffold import paths).
fn referenced_dirs(config: &RuntimeConfig) -> BTreeSet<String> {
    let mut dirs = BTreeSet::new();
    // platform/ is always needed — engine modules import from ../platform
    dirs.insert("platform".to_string());
    for module in config.system_modules.values() {
        if let Some(top) = module.path.split('/').next() {
            dirs.insert(top.to_string());
        }
    }
    for imp in &config.scaffold.imports {
        if let Some(top) = imp.path.split('/').next() {
            dirs.insert(top.to_string());
        }
    }
    for imp in &config.scaffold.data_imports {
        if let Some(top) = imp.path.split('/').next() {
            dirs.insert(top.to_string());
        }
    }
    for imp in &config.function_modules {
        if let Some(top) = imp.path.split('/').next() {
            dirs.insert(top.to_string());
        }
    }
    if let Some(ref preamble) = config.class_preamble {
        if let Some(top) = preamble.path.split('/').next() {
            dirs.insert(top.to_string());
        }
    }
    dirs
}

/// Copy the engine-specific runtime into `output_dir/runtime/`.
///
/// Only copies top-level directories that are referenced by the config
/// (system_modules, scaffold imports, etc.), plus all top-level files.
/// This prevents unused variant directories (e.g. sugarcube/ in a Harlowe
/// project) from being copied.
pub fn emit_runtime(
    output_dir: &Path,
    runtime_src: &Path,
    config: &RuntimeConfig,
) -> Result<(), CoreError> {
    let dest = output_dir.join("runtime");
    fs::create_dir_all(&dest)?;

    let needed = referenced_dirs(config);

    for entry in fs::read_dir(runtime_src)? {
        let entry = entry?;
        let src_path = entry.path();
        let dst_path = dest.join(entry.file_name());
        if src_path.is_dir() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy();
            if needed.contains(name_str.as_ref()) {
                copy_dir_recursive(&src_path, &dst_path)?;
            }
        } else {
            // Always copy top-level files (index.ts, runtime.json, etc.)
            fs::copy(&src_path, &dst_path)?;
        }
    }
    Ok(())
}
