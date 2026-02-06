//! Extract image, sound, and binary assets from SWF tags.

use reincarnate_core::project::{Asset, AssetCatalog, AssetKind};

/// Walk SWF tags and extract assets into the catalog.
pub fn extract_assets<'a>(tags: &[swf::Tag<'a>]) -> AssetCatalog {
    let mut catalog = AssetCatalog::new();

    // Collect JPEG tables (shared across DefineBits tags).
    let jpeg_tables: Option<&[u8]> = tags.iter().find_map(|tag| match tag {
        swf::Tag::JpegTables(data) => Some(*data),
        _ => None,
    });

    for tag in tags {
        match tag {
            // JPEG images (v1 — needs JPEG tables prepended).
            swf::Tag::DefineBits { id, jpeg_data } => {
                let data = match jpeg_tables {
                    Some(tables) => {
                        let mut combined = tables.to_vec();
                        combined.extend_from_slice(jpeg_data);
                        combined
                    }
                    None => jpeg_data.to_vec(),
                };
                let size = data.len() as u64;
                catalog.add(Asset {
                    id: format!("char_{id}"),
                    kind: AssetKind::Image,
                    original_name: format!("char_{id}"),
                    path: format!("assets/image_{id}.jpg").into(),
                    size,
                    data,
                });
            }

            // JPEG images (v2 — self-contained).
            swf::Tag::DefineBitsJpeg2 { id, jpeg_data } => {
                let data = jpeg_data.to_vec();
                let size = data.len() as u64;
                catalog.add(Asset {
                    id: format!("char_{id}"),
                    kind: AssetKind::Image,
                    original_name: format!("char_{id}"),
                    path: format!("assets/image_{id}.jpg").into(),
                    size,
                    data,
                });
            }

            // JPEG images (v3/v4 — with alpha or deblocking).
            swf::Tag::DefineBitsJpeg3(jpeg3) => {
                let data = jpeg3.data.to_vec();
                let size = data.len() as u64;
                let ext = detect_image_ext(&data);
                catalog.add(Asset {
                    id: format!("char_{}", jpeg3.id),
                    kind: AssetKind::Image,
                    original_name: format!("char_{}", jpeg3.id),
                    path: format!("assets/image_{}.{ext}", jpeg3.id).into(),
                    size,
                    data,
                });
            }

            // Lossless bitmaps (PNG-like, zlib-compressed).
            swf::Tag::DefineBitsLossless(lossless) => {
                let data = lossless.data.to_vec();
                let size = data.len() as u64;
                catalog.add(Asset {
                    id: format!("char_{}", lossless.id),
                    kind: AssetKind::Image,
                    original_name: format!("char_{}", lossless.id),
                    path: format!("assets/image_{}.bin", lossless.id).into(),
                    size,
                    data,
                });
            }

            // Sound assets.
            swf::Tag::DefineSound(sound) => {
                let data = sound.data.to_vec();
                let size = data.len() as u64;
                let ext = sound_ext(&sound.format.compression);
                catalog.add(Asset {
                    id: format!("char_{}", sound.id),
                    kind: AssetKind::Audio,
                    original_name: format!("char_{}", sound.id),
                    path: format!("assets/sound_{}.{ext}", sound.id).into(),
                    size,
                    data,
                });
            }

            // Binary data blobs.
            swf::Tag::DefineBinaryData(bin) => {
                let data = bin.data.to_vec();
                let size = data.len() as u64;
                catalog.add(Asset {
                    id: format!("char_{}", bin.id),
                    kind: AssetKind::Data,
                    original_name: format!("char_{}", bin.id),
                    path: format!("assets/data_{}.bin", bin.id).into(),
                    size,
                    data,
                });
            }

            _ => {}
        }
    }

    catalog
}

/// Detect image format from magic bytes.
fn detect_image_ext(data: &[u8]) -> &'static str {
    if data.starts_with(&[0x89, b'P', b'N', b'G']) {
        "png"
    } else if data.starts_with(b"GIF") {
        "gif"
    } else {
        "jpg"
    }
}

/// Map SWF audio compression to a file extension.
fn sound_ext(compression: &swf::AudioCompression) -> &'static str {
    match compression {
        swf::AudioCompression::Mp3 => "mp3",
        swf::AudioCompression::Aac => "aac",
        _ => "bin",
    }
}
