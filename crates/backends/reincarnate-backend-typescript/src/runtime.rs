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
const FLASH_OBJECT_TS: &str = include_str!("../runtime/flash/object.ts");
const FLASH_CLASS_TS: &str = include_str!("../runtime/flash/class.ts");
const FLASH_SCOPE_TS: &str = include_str!("../runtime/flash/scope.ts");
const FLASH_EXCEPTION_TS: &str = include_str!("../runtime/flash/exception.ts");
const FLASH_ITERATOR_TS: &str = include_str!("../runtime/flash/iterator.ts");
const FLASH_MEMORY_TS: &str = include_str!("../runtime/flash/memory.ts");
const FLASH_XML_TS: &str = include_str!("../runtime/flash/xml.ts");
const FLASH_GEOM_TS: &str = include_str!("../runtime/flash/geom.ts");
const FLASH_EVENTS_TS: &str = include_str!("../runtime/flash/events.ts");
const FLASH_DISPLAY_TS: &str = include_str!("../runtime/flash/display.ts");
const FLASH_TEXT_TS: &str = include_str!("../runtime/flash/text.ts");
const FLASH_NET_TS: &str = include_str!("../runtime/flash/net.ts");
const FLASH_AMF_TS: &str = include_str!("../runtime/flash/amf.ts");
const FLASH_UTILS_TS: &str = include_str!("../runtime/flash/utils.ts");
const FLASH_RUNTIME_TS: &str = include_str!("../runtime/flash/runtime.ts");

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
    let flash_dir = runtime_dir.join("flash");
    fs::create_dir_all(&flash_dir)?;
    fs::write(flash_dir.join("object.ts"), FLASH_OBJECT_TS)?;
    fs::write(flash_dir.join("class.ts"), FLASH_CLASS_TS)?;
    fs::write(flash_dir.join("scope.ts"), FLASH_SCOPE_TS)?;
    fs::write(flash_dir.join("exception.ts"), FLASH_EXCEPTION_TS)?;
    fs::write(flash_dir.join("iterator.ts"), FLASH_ITERATOR_TS)?;
    fs::write(flash_dir.join("memory.ts"), FLASH_MEMORY_TS)?;
    fs::write(flash_dir.join("xml.ts"), FLASH_XML_TS)?;
    fs::write(flash_dir.join("geom.ts"), FLASH_GEOM_TS)?;
    fs::write(flash_dir.join("events.ts"), FLASH_EVENTS_TS)?;
    fs::write(flash_dir.join("display.ts"), FLASH_DISPLAY_TS)?;
    fs::write(flash_dir.join("text.ts"), FLASH_TEXT_TS)?;
    fs::write(flash_dir.join("net.ts"), FLASH_NET_TS)?;
    fs::write(flash_dir.join("amf.ts"), FLASH_AMF_TS)?;
    fs::write(flash_dir.join("utils.ts"), FLASH_UTILS_TS)?;
    fs::write(flash_dir.join("runtime.ts"), FLASH_RUNTIME_TS)?;

    Ok(())
}
