use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;

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

/// Copy the engine-specific runtime into `output_dir/runtime/`.
pub fn emit_runtime(output_dir: &Path, runtime_src: &Path) -> Result<(), CoreError> {
    let dest = output_dir.join("runtime");
    copy_dir_recursive(runtime_src, &dest)
}
