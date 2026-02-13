use std::path::PathBuf;

use datawin::DataWin;
use reincarnate_core::project::{Asset, AssetCatalog, AssetKind};

/// Extract assets (textures, audio) from a data.win file.
pub fn extract_assets(dw: &DataWin) -> AssetCatalog {
    let mut catalog = AssetCatalog::new();

    extract_textures(dw, &mut catalog);
    extract_audio(dw, &mut catalog);

    catalog
}

/// Extract texture pages from TXTR as PNG images.
fn extract_textures(dw: &DataWin, catalog: &mut AssetCatalog) {
    let txtr = match dw.txtr() {
        Ok(t) => t,
        Err(_) => return,
    };

    for (i, _entry) in txtr.textures.iter().enumerate() {
        let data = match txtr.texture_data(i, dw.data()) {
            Some(d) => d,
            None => continue,
        };

        // Texture pages are stored as PNG in the data.win file.
        let name = format!("texture_{i}");
        catalog.add(Asset {
            id: format!("txtr_{i}"),
            kind: AssetKind::Image,
            original_name: name.clone(),
            path: PathBuf::from(format!("assets/textures/{name}.png")),
            size: data.len() as u64,
            data: data.to_vec(),
        });
    }
}

/// Extract audio entries from AUDO, named via SOND.
fn extract_audio(dw: &DataWin, catalog: &mut AssetCatalog) {
    let audo = match dw.audo() {
        Ok(a) => a,
        Err(_) => return,
    };

    // Build audio_id → sound name mapping from SOND.
    let sound_names = build_sound_names(dw);

    for (i, _entry) in audo.entries.iter().enumerate() {
        let data = match audo.audio_data(i, dw.data()) {
            Some(d) => d,
            None => continue,
        };

        let name = sound_names
            .get(&(i as i32))
            .cloned()
            .unwrap_or_else(|| format!("audio_{i}"));

        // Detect format from magic bytes.
        let ext = detect_audio_extension(data);

        catalog.add(Asset {
            id: format!("audo_{i}"),
            kind: AssetKind::Audio,
            original_name: name.clone(),
            path: PathBuf::from(format!("assets/audio/{name}.{ext}")),
            size: data.len() as u64,
            data: data.to_vec(),
        });
    }
}

/// Build audio_id → sound name from SOND chunk.
fn build_sound_names(dw: &DataWin) -> std::collections::HashMap<i32, String> {
    let mut names = std::collections::HashMap::new();
    let sond = match dw.sond() {
        Ok(s) => s,
        Err(_) => return names,
    };

    for sound in &sond.sounds {
        if sound.audio_id >= 0 {
            if let Ok(name) = dw.resolve_string(sound.name) {
                names.insert(sound.audio_id, name);
            }
        }
    }
    names
}

/// Detect audio format from magic bytes.
fn detect_audio_extension(data: &[u8]) -> &'static str {
    if data.starts_with(b"OggS") {
        "ogg"
    } else if data.starts_with(b"RIFF") {
        "wav"
    } else if data.starts_with(b"ID3")
        || (data.len() >= 2 && data[0] == 0xFF && (data[1] & 0xE0) == 0xE0)
    {
        "mp3"
    } else {
        "bin"
    }
}
