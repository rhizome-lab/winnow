use std::fs;
use std::path::Path;

use reincarnate_core::error::CoreError;

// Generic runtime modules.
const RENDERER_TS: &str = include_str!("../runtime/renderer.ts");
const AUDIO_TS: &str = include_str!("../runtime/audio.ts");
const INPUT_TS: &str = include_str!("../runtime/input.ts");
const TIMING_TS: &str = include_str!("../runtime/timing.ts");
const SAVE_TS: &str = include_str!("../runtime/save.ts");
const UI_TS: &str = include_str!("../runtime/ui.ts");
const INDEX_TS: &str = include_str!("../runtime/index.ts");

// Flash-specific runtime modules.
const FLASH_INDEX_TS: &str = include_str!("../runtime/flash.ts");
const FLASH_OBJECT_TS: &str = include_str!("../runtime/flash_object.ts");
const FLASH_CLASS_TS: &str = include_str!("../runtime/flash_class.ts");
const FLASH_SCOPE_TS: &str = include_str!("../runtime/flash_scope.ts");
const FLASH_EXCEPTION_TS: &str = include_str!("../runtime/flash_exception.ts");
const FLASH_ITERATOR_TS: &str = include_str!("../runtime/flash_iterator.ts");
const FLASH_MEMORY_TS: &str = include_str!("../runtime/flash_memory.ts");
const FLASH_XML_TS: &str = include_str!("../runtime/flash_xml.ts");

/// All known generic system names that the runtime provides.
pub const SYSTEM_NAMES: &[&str] = &["renderer", "audio", "input", "timing", "save", "ui"];

/// Write the runtime TypeScript files into `output_dir/runtime/`.
pub fn emit_runtime(output_dir: &Path) -> Result<(), CoreError> {
    let runtime_dir = output_dir.join("runtime");
    fs::create_dir_all(&runtime_dir)?;

    // Generic systems.
    fs::write(runtime_dir.join("renderer.ts"), RENDERER_TS)?;
    fs::write(runtime_dir.join("audio.ts"), AUDIO_TS)?;
    fs::write(runtime_dir.join("input.ts"), INPUT_TS)?;
    fs::write(runtime_dir.join("timing.ts"), TIMING_TS)?;
    fs::write(runtime_dir.join("save.ts"), SAVE_TS)?;
    fs::write(runtime_dir.join("ui.ts"), UI_TS)?;
    fs::write(runtime_dir.join("index.ts"), INDEX_TS)?;

    // Flash-specific systems.
    fs::write(runtime_dir.join("flash.ts"), FLASH_INDEX_TS)?;
    fs::write(runtime_dir.join("flash_object.ts"), FLASH_OBJECT_TS)?;
    fs::write(runtime_dir.join("flash_class.ts"), FLASH_CLASS_TS)?;
    fs::write(runtime_dir.join("flash_scope.ts"), FLASH_SCOPE_TS)?;
    fs::write(runtime_dir.join("flash_exception.ts"), FLASH_EXCEPTION_TS)?;
    fs::write(runtime_dir.join("flash_iterator.ts"), FLASH_ITERATOR_TS)?;
    fs::write(runtime_dir.join("flash_memory.ts"), FLASH_MEMORY_TS)?;
    fs::write(runtime_dir.join("flash_xml.ts"), FLASH_XML_TS)?;

    Ok(())
}
