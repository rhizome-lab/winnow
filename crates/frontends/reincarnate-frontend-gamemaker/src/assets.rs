use std::path::{Path, PathBuf};

use reincarnate_datawin::DataWin;
use reincarnate_core::project::{Asset, AssetCatalog, AssetKind};

/// Extract assets (textures, audio, icon) from a data.win file.
///
/// `source_dir` is the directory containing `data.win`; used to locate a
/// sibling Windows `.exe` for icon extraction.
pub fn extract_assets(dw: &DataWin, source_dir: &Path) -> AssetCatalog {
    let mut catalog = AssetCatalog::new();

    extract_textures(dw, &mut catalog);
    extract_audio(dw, &mut catalog);
    extract_icon_from_exe(source_dir, &mut catalog);

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
pub(crate) fn detect_audio_extension(data: &[u8]) -> &'static str {
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

// ---------------------------------------------------------------------------
// Icon extraction from Windows PE executable
// ---------------------------------------------------------------------------

/// Scan `source_dir` for a Windows `.exe`, extract its icon resources, and
/// add an `AssetKind::Icon` asset to the catalog.
///
/// GameMaker games ship `data.win` alongside a Windows runner `.exe`.  The
/// game icon is stored as `RT_GROUP_ICON` / `RT_ICON` PE resources in that
/// exe — it is **not** embedded in `data.win` itself.  We locate the first
/// sibling `.exe`, extract the best icon group, and reconstruct a valid
/// Windows `.ico` blob.
///
/// On failure (no exe found, not a PE file, no icon resources) this function
/// silently returns without adding an asset — the caller continues normally
/// and the manifest `icon` field can be used as a fallback.
fn extract_icon_from_exe(source_dir: &Path, catalog: &mut AssetCatalog) {
    let exe = match find_exe_in_dir(source_dir) {
        Some(p) => p,
        None => return,
    };

    let data = match std::fs::read(&exe) {
        Ok(d) => d,
        Err(_) => return,
    };

    if let Some(ico) = extract_ico_from_pe(&data) {
        let size = ico.len() as u64;
        catalog.add(Asset {
            id: "icon".into(),
            kind: AssetKind::Icon,
            original_name: "icon".into(),
            path: PathBuf::from("favicon.ico"),
            size,
            data: ico,
        });
    }
}

/// Return the first `.exe` found directly in `dir` (non-recursive).
fn find_exe_in_dir(dir: &Path) -> Option<PathBuf> {
    let rd = std::fs::read_dir(dir).ok()?;
    for entry in rd.flatten() {
        let path = entry.path();
        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext.eq_ignore_ascii_case("exe") {
                    return Some(path);
                }
            }
        }
    }
    None
}

/// Parse a Windows PE binary and reconstruct a `.ico` blob from its icon
/// resources.  Returns `None` if the binary is not a valid PE, has no icon
/// section, or the icon data cannot be assembled.
///
/// The PE format stores icons in two resource types:
/// - `RT_ICON` (type 3): raw icon bitmap/PNG data for each resolution.
/// - `RT_GROUP_ICON` (type 14): directory that lists which `RT_ICON` entries
///   belong to one logical icon and their dimensions/colour depth.
///
/// We locate the first `RT_GROUP_ICON` entry, read its directory, collect the
/// referenced `RT_ICON` entries, and write them out as a standard `.ico` file.
fn extract_ico_from_pe(data: &[u8]) -> Option<Vec<u8>> {
    // Verify MZ header.
    if data.len() < 0x40 || &data[0..2] != b"MZ" {
        return None;
    }

    // PE header offset is at 0x3C.
    let pe_off = u32::from_le_bytes(data.get(0x3C..0x40)?.try_into().ok()?) as usize;
    if data.len() < pe_off + 4 || &data[pe_off..pe_off + 4] != b"PE\0\0" {
        return None;
    }

    // COFF header: number of sections at pe_off+6, optional header size at pe_off+20.
    let num_sections = u16::from_le_bytes(data.get(pe_off + 6..pe_off + 8)?.try_into().ok()?) as usize;
    let opt_header_size = u16::from_le_bytes(data.get(pe_off + 20..pe_off + 22)?.try_into().ok()?) as usize;

    // Optional header magic: 0x10b = PE32, 0x20b = PE32+.
    let magic = u16::from_le_bytes(data.get(pe_off + 24..pe_off + 26)?.try_into().ok()?);

    // Resource data directory index is 2 (zero-based).
    // For PE32:  data directories start at optional header offset +96.
    // For PE32+: data directories start at optional header offset +112.
    let dd_base = match magic {
        0x010b => pe_off + 24 + 96,
        0x020b => pe_off + 24 + 112,
        _ => return None,
    };
    let rsrc_rva = u32::from_le_bytes(data.get(dd_base + 16..dd_base + 20)?.try_into().ok()?) as usize;
    if rsrc_rva == 0 {
        return None;
    }

    // Sections start right after the optional header.
    let sections_off = pe_off + 24 + opt_header_size;

    // Find the section that contains rsrc_rva.
    let (rsrc_raw_off, rsrc_va) = find_section(data, sections_off, num_sections, rsrc_rva)?;

    // Helper: RVA → file offset within resource section.
    let rva_to_off = |rva: usize| -> Option<usize> {
        let offset = rva.checked_sub(rsrc_va)?;
        rsrc_raw_off.checked_add(offset)
    };

    // Build map: icon_id → raw icon data.
    let rt_icon_map = collect_rt_icon(data, rsrc_raw_off, rsrc_raw_off, rsrc_va, 3)?;

    // Find the first RT_GROUP_ICON entry and decode its directory.
    let group_data = first_rt_group_icon_data(data, rsrc_raw_off, rsrc_raw_off, rsrc_va, 14, &rva_to_off)?;

    build_ico(group_data, &rt_icon_map)
}

/// Find the PE section containing `target_rva`.  Returns `(raw_offset, virtual_address)`.
fn find_section(data: &[u8], sections_off: usize, count: usize, target_rva: usize) -> Option<(usize, usize)> {
    for i in 0..count {
        let base = sections_off + i * 40;
        if base + 40 > data.len() {
            break;
        }
        let vsize = u32::from_le_bytes(data[base + 8..base + 12].try_into().ok()?) as usize;
        let vaddr = u32::from_le_bytes(data[base + 12..base + 16].try_into().ok()?) as usize;
        let raw_size = u32::from_le_bytes(data[base + 16..base + 20].try_into().ok()?) as usize;
        let raw_off = u32::from_le_bytes(data[base + 20..base + 24].try_into().ok()?) as usize;
        if target_rva >= vaddr && target_rva < vaddr + vsize.max(raw_size) {
            return Some((raw_off + (target_rva - vaddr), vaddr));
        }
    }
    None
}

/// Walk a PE resource directory and collect all leaf data for resources of
/// `target_type`.  Returns a map of `resource_id → &[u8]` data slices.
///
/// The PE resource directory is a three-level tree:
///   Level 1: resource type (RT_ICON = 3, RT_GROUP_ICON = 14, …)
///   Level 2: resource name/id
///   Level 3: language variant
///
/// We descend only into the matching type subtree and collect the first
/// language variant for each name/id.
fn collect_rt_icon(
    data: &[u8],
    dir_off: usize,    // offset of current directory node within data
    rsrc_raw: usize,   // raw file offset of the start of the .rsrc section
    rsrc_va: usize,    // virtual address of the .rsrc section
    target_type: u32,
) -> Option<std::collections::HashMap<u32, &[u8]>> {
    let mut map = std::collections::HashMap::new();

    // Parse level-1 directory (resource types).
    let named = u16::from_le_bytes(data.get(dir_off + 12..dir_off + 14)?.try_into().ok()?) as usize;
    let id_entries = u16::from_le_bytes(data.get(dir_off + 14..dir_off + 16)?.try_into().ok()?) as usize;
    let total = named + id_entries;

    for e in 0..total {
        let entry_off = dir_off + 16 + e * 8;
        let id_or_name = u32::from_le_bytes(data.get(entry_off..entry_off + 4)?.try_into().ok()?);
        let subdiroff_raw = u32::from_le_bytes(data.get(entry_off + 4..entry_off + 8)?.try_into().ok()?);
        let is_dir = subdiroff_raw & 0x8000_0000 != 0;

        // Skip named entries (high bit set in id_or_name) and non-matching type IDs.
        if id_or_name & 0x8000_0000 != 0 || id_or_name != target_type {
            continue;
        }
        if !is_dir {
            continue;
        }

        let type_dir_off = rsrc_raw + (subdiroff_raw & 0x7FFF_FFFF) as usize;
        // Level 2: name/id entries under this type.
        let named2 = u16::from_le_bytes(data.get(type_dir_off + 12..type_dir_off + 14)?.try_into().ok()?) as usize;
        let id2 = u16::from_le_bytes(data.get(type_dir_off + 14..type_dir_off + 16)?.try_into().ok()?) as usize;
        let total2 = named2 + id2;

        for e2 in 0..total2 {
            let e2_off = type_dir_off + 16 + e2 * 8;
            let icon_id = u32::from_le_bytes(data.get(e2_off..e2_off + 4)?.try_into().ok()?);
            // Strip high bit from named entries for ID lookup (they're named, we skip those).
            if icon_id & 0x8000_0000 != 0 {
                continue; // skip named entries
            }
            let subdiroff2_raw = u32::from_le_bytes(data.get(e2_off + 4..e2_off + 8)?.try_into().ok()?);
            if subdiroff2_raw & 0x8000_0000 == 0 {
                continue; // expect a subdirectory (language level)
            }
            let lang_dir_off = rsrc_raw + (subdiroff2_raw & 0x7FFF_FFFF) as usize;

            // Level 3: take the first language entry's data leaf.
            if data.len() < lang_dir_off + 16 {
                continue;
            }
            let leaf_off_raw = u32::from_le_bytes(data.get(lang_dir_off + 20..lang_dir_off + 24)?.try_into().ok()?);
            if leaf_off_raw & 0x8000_0000 != 0 {
                continue; // unexpected sub-directory
            }
            let leaf_off = rsrc_raw + leaf_off_raw as usize;
            // IMAGE_RESOURCE_DATA_ENTRY: RVA (4), Size (4), CodePage (4), Reserved (4).
            let data_rva = u32::from_le_bytes(data.get(leaf_off..leaf_off + 4)?.try_into().ok()?) as usize;
            let data_size = u32::from_le_bytes(data.get(leaf_off + 4..leaf_off + 8)?.try_into().ok()?) as usize;
            let file_off = rsrc_raw + data_rva.checked_sub(rsrc_va)?;
            let slice = data.get(file_off..file_off + data_size)?;
            map.insert(icon_id, slice);
        }
        break; // found target type, no need to continue
    }

    Some(map)
}

/// Find the first `RT_GROUP_ICON` resource and return its raw data bytes.
fn first_rt_group_icon_data<'a>(
    data: &'a [u8],
    dir_off: usize,
    rsrc_raw: usize,
    _rsrc_va: usize,
    target_type: u32,
    rva_to_off: &impl Fn(usize) -> Option<usize>,
) -> Option<&'a [u8]> {
    // Level-1: find the target type directory.
    let named = u16::from_le_bytes(data.get(dir_off + 12..dir_off + 14)?.try_into().ok()?) as usize;
    let id_entries = u16::from_le_bytes(data.get(dir_off + 14..dir_off + 16)?.try_into().ok()?) as usize;
    let total = named + id_entries;

    for e in 0..total {
        let entry_off = dir_off + 16 + e * 8;
        let id_or_name = u32::from_le_bytes(data.get(entry_off..entry_off + 4)?.try_into().ok()?);
        let subdiroff_raw = u32::from_le_bytes(data.get(entry_off + 4..entry_off + 8)?.try_into().ok()?);
        let is_dir = subdiroff_raw & 0x8000_0000 != 0;
        if id_or_name & 0x8000_0000 != 0 || id_or_name != target_type || !is_dir {
            continue;
        }

        let type_dir_off = rsrc_raw + (subdiroff_raw & 0x7FFF_FFFF) as usize;
        let named2 = u16::from_le_bytes(data.get(type_dir_off + 12..type_dir_off + 14)?.try_into().ok()?) as usize;
        let id2 = u16::from_le_bytes(data.get(type_dir_off + 14..type_dir_off + 16)?.try_into().ok()?) as usize;
        let total2 = named2 + id2;

        // Take the first name/id entry.
        if total2 == 0 {
            break;
        }
        let e2_off = type_dir_off + 16;
        let subdiroff2_raw = u32::from_le_bytes(data.get(e2_off + 4..e2_off + 8)?.try_into().ok()?);
        if subdiroff2_raw & 0x8000_0000 == 0 {
            break;
        }
        let lang_dir_off = rsrc_raw + (subdiroff2_raw & 0x7FFF_FFFF) as usize;
        if data.len() < lang_dir_off + 24 {
            break;
        }
        let leaf_off_raw = u32::from_le_bytes(data.get(lang_dir_off + 20..lang_dir_off + 24)?.try_into().ok()?);
        if leaf_off_raw & 0x8000_0000 != 0 {
            break;
        }
        let leaf_off = rsrc_raw + leaf_off_raw as usize;
        let data_rva = u32::from_le_bytes(data.get(leaf_off..leaf_off + 4)?.try_into().ok()?) as usize;
        let data_size = u32::from_le_bytes(data.get(leaf_off + 4..leaf_off + 8)?.try_into().ok()?) as usize;
        let file_off = rva_to_off(data_rva)?;
        return data.get(file_off..file_off + data_size);
    }
    None
}

/// Build a `.ico` file from a `GRPICONDIR` blob and the RT_ICON map.
///
/// `GRPICONDIR` layout (mirrors ICONDIR but uses `wId` instead of `dwImageOffset`):
///
/// ```text
/// u16 idReserved (0)
/// u16 idType (1)
/// u16 idCount
/// [idCount × GRPICONDIRENTRY]
///
/// GRPICONDIRENTRY:
///   u8  bWidth
///   u8  bHeight
///   u8  bColorCount
///   u8  bReserved
///   u16 wPlanes
///   u16 wBitCount
///   u32 dwBytesInRes
///   u16 nId  ← RT_ICON resource ID
/// ```
///
/// The `.ico` format uses the same layout but replaces `nId` with a `u32`
/// file offset to the image data.
fn build_ico(
    group: &[u8],
    icons: &std::collections::HashMap<u32, &[u8]>,
) -> Option<Vec<u8>> {
    if group.len() < 6 {
        return None;
    }
    let count = u16::from_le_bytes(group[4..6].try_into().ok()?) as usize;
    if count == 0 {
        return None;
    }

    // Each GRPICONDIRENTRY is 14 bytes.
    let entries_end = 6 + count * 14;
    if group.len() < entries_end {
        return None;
    }

    // Collect entries and validate that we have the icon data for each.
    struct IcoEntry<'a> {
        width: u8,
        height: u8,
        color_count: u8,
        planes: u16,
        bit_count: u16,
        data: &'a [u8],
    }

    let mut entries: Vec<IcoEntry<'_>> = Vec::with_capacity(count);
    for i in 0..count {
        let base = 6 + i * 14;
        let width = group[base];
        let height = group[base + 1];
        let color_count = group[base + 2];
        // base+3: bReserved
        let planes = u16::from_le_bytes(group[base + 4..base + 6].try_into().ok()?);
        let bit_count = u16::from_le_bytes(group[base + 6..base + 8].try_into().ok()?);
        // base+8..base+12: dwBytesInRes (u32) — we use actual slice length instead
        let nid = u16::from_le_bytes(group[base + 12..base + 14].try_into().ok()?) as u32;
        let icon_data = icons.get(&nid)?;
        entries.push(IcoEntry { width, height, color_count, planes, bit_count, data: icon_data });
    }

    // Build the .ico file:
    //   6-byte ICONDIR header
    //   count × 16-byte ICONDIRENTRY
    //   image data blobs
    let header_size = 6 + entries.len() * 16;
    let total_size = header_size + entries.iter().map(|e| e.data.len()).sum::<usize>();
    let mut out = Vec::with_capacity(total_size);

    // ICONDIR header: idReserved=0, idType=1, idCount=N
    out.extend_from_slice(&[0u8, 0, 1, 0]);
    out.extend_from_slice(&(entries.len() as u16).to_le_bytes());

    // ICONDIRENTRY array (image data offsets will be filled after).
    let dir_start = out.len();
    // Reserve space for directory entries (16 bytes each).
    out.resize(dir_start + entries.len() * 16, 0u8);

    // Append image data and back-fill directory entries.
    let mut offset = header_size as u32;
    for (i, entry) in entries.iter().enumerate() {
        let dir_entry_off = dir_start + i * 16;
        out[dir_entry_off] = entry.width;
        out[dir_entry_off + 1] = entry.height;
        out[dir_entry_off + 2] = entry.color_count;
        out[dir_entry_off + 3] = 0; // reserved
        out[dir_entry_off + 4..dir_entry_off + 6].copy_from_slice(&entry.planes.to_le_bytes());
        out[dir_entry_off + 6..dir_entry_off + 8].copy_from_slice(&entry.bit_count.to_le_bytes());
        let size = entry.data.len() as u32;
        out[dir_entry_off + 8..dir_entry_off + 12].copy_from_slice(&size.to_le_bytes());
        out[dir_entry_off + 12..dir_entry_off + 16].copy_from_slice(&offset.to_le_bytes());
        offset += size;
        out.extend_from_slice(entry.data);
    }

    Some(out)
}
