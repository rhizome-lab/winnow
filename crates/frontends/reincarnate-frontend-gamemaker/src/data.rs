use std::fmt::Write;
use std::path::PathBuf;

use datawin::DataWin;
use reincarnate_core::project::{Asset, AssetCatalog, AssetKind};

use crate::assets::detect_audio_extension;
use crate::naming;

/// Return true if `s` is a valid JavaScript identifier.
///
/// JS identifiers use Unicode ID_Start for the first character and
/// ID_Continue for subsequent characters, plus `$` which JS allows as
/// both a start and continue character (it is not in the Unicode
/// ID_Start/ID_Continue sets).
fn is_valid_js_ident(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if unicode_ident::is_xid_start(c) || c == '_' || c == '$' => {}
        _ => return false,
    }
    chars.all(|c| unicode_ident::is_xid_continue(c) || c == '$')
}

/// Generate TypeScript data files from parsed chunks and add them to the catalog.
pub fn generate_data_files(dw: &DataWin, catalog: &mut AssetCatalog, obj_names: &[String]) {
    generate_textures(dw, catalog);
    generate_sprites(dw, catalog);
    generate_fonts(dw, catalog);
    generate_sounds(dw, catalog);
    generate_shaders(dw, catalog);
    generate_rooms(dw, catalog, obj_names);
    generate_objects(catalog, obj_names);
    generate_asset_ids(dw, catalog);
}

/// Generate `data/textures.ts` from TPAG entries.
fn generate_textures(dw: &DataWin, catalog: &mut AssetCatalog) {
    let tpag = match dw.tpag() {
        Ok(t) => t,
        Err(_) => return,
    };

    let mut out = String::new();
    out.push_str("export interface Texture {\n");
    out.push_str("  src: { x: number; y: number; w: number; h: number };\n");
    out.push_str("  dest: { w: number; h: number };\n");
    out.push_str("  sheetId: number;\n");
    out.push_str("}\n\n");
    out.push_str("export const textures: Texture[] = [\n");

    for item in &tpag.items {
        let _ = writeln!(
            out,
            "  {{ src: {{ x: {}, y: {}, w: {}, h: {} }}, dest: {{ w: {}, h: {} }}, sheetId: {} }},",
            item.source_x, item.source_y, item.source_width, item.source_height,
            item.render_width, item.render_height,
            item.texture_page_id,
        );
    }

    out.push_str("];\n");

    catalog.add(Asset {
        id: "data_textures".into(),
        kind: AssetKind::Data,
        original_name: "textures".into(),
        path: PathBuf::from("data/textures.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}

/// Generate `data/sprites.ts` from SPRT entries.
fn generate_sprites(dw: &DataWin, catalog: &mut AssetCatalog) {
    let sprt = match dw.sprt() {
        Ok(s) => s,
        Err(_) => return,
    };

    let mut out = String::new();
    out.push_str("export interface Sprite {\n");
    out.push_str("  name: string;\n");
    out.push_str("  size: { width: number; height: number };\n");
    out.push_str("  origin: { x: number; y: number };\n");
    out.push_str("  bbox: { left: number; right: number; top: number; bottom: number };\n");
    out.push_str("  textures: number[];\n");
    out.push_str("}\n\n");
    out.push_str("export const sprites: Sprite[] = [\n");

    for sprite in &sprt.sprites {
        let name = dw.resolve_string(sprite.name).unwrap_or_else(|_| "???".into());
        let tpag_str: Vec<String> = sprite.tpag_indices.iter().map(|i| i.to_string()).collect();
        let _ = writeln!(
            out,
            "  {{ name: {:?}, size: {{ width: {}, height: {} }}, origin: {{ x: {}, y: {} }}, bbox: {{ left: {}, right: {}, top: {}, bottom: {} }}, textures: [{}] }},",
            name,
            sprite.width, sprite.height,
            sprite.origin_x, sprite.origin_y,
            sprite.bbox_left, sprite.bbox_right, sprite.bbox_top, sprite.bbox_bottom,
            tpag_str.join(", "),
        );
    }

    out.push_str("];\n\n");

    // Sprites enum: PascalCase name → index.
    // Use `as const` (no explicit type) so property access returns the exact literal
    // type (e.g. `322`) rather than `number | undefined` under noUncheckedIndexedAccess.
    out.push_str("export const Sprites = {\n");
    for (i, sprite) in sprt.sprites.iter().enumerate() {
        let raw = dw.resolve_string(sprite.name).unwrap_or_else(|_| format!("spr_{i}"));
        let key = naming::sprite_name_to_pascal(&raw);
        // Emit as a plain identifier when valid, otherwise as a quoted string key.
        // A valid JS identifier starts with [a-zA-Z_$] and continues with [a-zA-Z0-9_$].
        // serde_json::to_string provides correct JSON string escaping for quoted keys.
        let key_token = if is_valid_js_ident(&key) {
            key
        } else {
            serde_json::to_string(&key).expect("string serialization cannot fail")
        };
        let _ = writeln!(out, "  {key_token}: {i},");
    }
    out.push_str("} as const;\n");

    catalog.add(Asset {
        id: "data_sprites".into(),
        kind: AssetKind::Data,
        original_name: "sprites".into(),
        path: PathBuf::from("data/sprites.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}

/// Extract PascalCase sprite names indexed by sprite ID.
pub fn extract_sprite_names(dw: &DataWin) -> Vec<String> {
    let sprt = match dw.sprt() {
        Ok(s) => s,
        Err(_) => return Vec::new(),
    };
    sprt.sprites
        .iter()
        .enumerate()
        .map(|(i, sprite)| {
            let raw = dw.resolve_string(sprite.name).unwrap_or_else(|_| format!("spr_{i}"));
            naming::sprite_name_to_pascal(&raw)
        })
        .collect()
}

/// Generate `data/fonts.ts` from FONT entries.
fn generate_fonts(dw: &DataWin, catalog: &mut AssetCatalog) {
    let font = match dw.font() {
        Ok(f) => f,
        Err(_) => return,
    };
    let tpag = match dw.tpag() {
        Ok(t) => t,
        Err(_) => return,
    };

    let mut out = String::new();
    out.push_str("export interface FontGlyph {\n");
    out.push_str("  char: number;\n");
    out.push_str("  frame: { x: number; y: number; width: number; height: number };\n");
    out.push_str("  shift: number;\n");
    out.push_str("  offset: number;\n");
    out.push_str("}\n\n");
    out.push_str("export interface Font {\n");
    out.push_str("  name: string;\n");
    out.push_str("  size: number;\n");
    out.push_str("  texture: number;\n");
    out.push_str("  chars: FontGlyph[];\n");
    out.push_str("}\n\n");
    out.push_str("export const fonts: Font[] = [\n");

    for entry in &font.fonts {
        let name = dw.resolve_string(entry.name).unwrap_or_else(|_| "???".into());
        // The font's tpag_index points into TPAG which tells us which texture sheet.
        let texture_idx = entry.tpag_index;

        let _ = writeln!(out, "  {{ name: {:?}, size: {}, texture: {texture_idx}, chars: [", name, entry.size);
        for glyph in &entry.glyphs {
            // Font glyphs have their own x,y on the texture — offset from the font's TPAG region.
            // The reference uses the TPAG src as a base offset for the font texture.
            let _ = writeln!(
                out,
                "    {{ char: {}, frame: {{ x: {}, y: {}, width: {}, height: {} }}, shift: {}, offset: {} }},",
                glyph.character,
                glyph.x, glyph.y, glyph.width, glyph.height,
                glyph.shift, glyph.offset,
            );
        }
        out.push_str("  ] },\n");
    }

    out.push_str("];\n");

    // Suppress unused variable warning for tpag
    let _ = &tpag;

    catalog.add(Asset {
        id: "data_fonts".into(),
        kind: AssetKind::Data,
        original_name: "fonts".into(),
        path: PathBuf::from("data/fonts.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}

/// Generate `data/sounds.ts` from SOND/AUDO entries.
fn generate_sounds(dw: &DataWin, catalog: &mut AssetCatalog) {
    let sond = match dw.sond() {
        Ok(s) => s,
        Err(_) => return,
    };
    let audo = match dw.audo() {
        Ok(a) => a,
        Err(_) => return,
    };

    let mut out = String::new();
    out.push_str("export interface Sound {
");
    out.push_str("  name: string;
");
    out.push_str("  url: string;
");
    out.push_str("}

");
    out.push_str("export const sounds: Sound[] = [
");

    for entry in &sond.sounds {
        let name = dw.resolve_string(entry.name).unwrap_or_else(|_| "???".into());
        if entry.audio_id >= 0 {
            let idx = entry.audio_id as usize;
            let data = audo.audio_data(idx, dw.data()).unwrap_or(&[]);
            let ext = detect_audio_extension(data);
            let _ = writeln!(out, "  {{ name: {:?}, url: {:?} }},",
                name, format!("assets/audio/{name}.{ext}"));
        } else {
            // External audio (not embedded in data.win) — no URL available.
            let _ = writeln!(out, "  {{ name: {:?}, url: \"\" }},", name);
        }
    }

    out.push_str("];
");

    catalog.add(Asset {
        id: "data_sounds".into(),
        kind: AssetKind::Data,
        original_name: "sounds".into(),
        path: PathBuf::from("data/sounds.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}

/// Generate `data/shaders.ts` from SHDR entries (embeds GLSL source strings).
fn generate_shaders(dw: &DataWin, catalog: &mut AssetCatalog) {
    let shdr = match dw.shdr() {
        Ok(s) => s,
        Err(_) => return,
    };

    let mut out = String::new();
    out.push_str("// GML shader data — auto-generated, do not edit.\n");
    out.push_str("export interface GmlShader { name: string; vertex: string; fragment: string; }\n");
    out.push_str("export const shaders: GmlShader[] = [\n");

    for entry in &shdr.shaders {
        let name = dw.resolve_string(entry.name).unwrap_or_else(|_| "???".into());
        let vertex = dw.resolve_string(entry.vertex).unwrap_or_default();
        let fragment = dw.resolve_string(entry.fragment).unwrap_or_default();
        let _ = writeln!(
            out,
            "  {{ name: {}, vertex: {}, fragment: {} }},",
            serde_json::to_string(&name).unwrap_or_else(|_| "\"\"".into()),
            serde_json::to_string(&vertex).unwrap_or_else(|_| "\"\"".into()),
            serde_json::to_string(&fragment).unwrap_or_else(|_| "\"\"".into()),
        );
    }

    out.push_str("];\n");

    // Also emit `Shaders` as-const enum for named access by index.
    out.push_str("\nexport const Shaders: Record<string, number> = {\n");
    for (i, entry) in shdr.shaders.iter().enumerate() {
        let name = dw.resolve_string(entry.name).unwrap_or_else(|_| format!("shader_{i}"));
        if is_valid_js_ident(&name) {
            let _ = writeln!(out, "  {name}: {i},");
        }
    }
    out.push_str("};\n");

    catalog.add(Asset {
        id: "data_shaders".into(),
        kind: AssetKind::Data,
        original_name: "shaders".into(),
        path: PathBuf::from("data/shaders.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}

/// Generate `data/rooms.ts` from ROOM entries.
fn generate_rooms(dw: &DataWin, catalog: &mut AssetCatalog, obj_names: &[String]) {
    let room = match dw.room() {
        Ok(r) => r,
        Err(_) => return,
    };

    let mut out = String::new();
    out.push_str("export interface RoomObj {\n");
    out.push_str("  obj: number;\n");
    out.push_str("  pos: { x: number; y: number };\n");
    out.push_str("}\n\n");
    out.push_str("export interface Room {\n");
    out.push_str("  name: string;\n");
    out.push_str("  size: { width: number; height: number };\n");
    out.push_str("  speed: number;\n");
    out.push_str("  objs: RoomObj[];\n");
    out.push_str("}\n\n");
    out.push_str("export const rooms: Room[] = [\n");

    for entry in &room.rooms {
        let name = dw.resolve_string(entry.name).unwrap_or_else(|_| "???".into());
        let _ = writeln!(
            out,
            "  {{ name: {:?}, size: {{ width: {}, height: {} }}, speed: {}, objs: [",
            name, entry.width, entry.height, entry.speed,
        );
        for obj in &entry.objects {
            if obj.object_id >= 0 {
                let _ = writeln!(
                    out,
                    "    {{ obj: {}, pos: {{ x: {}, y: {} }} }},",
                    obj.object_id, obj.x, obj.y,
                );
            }
        }
        out.push_str("  ] },\n");
    }

    out.push_str("];\n\n");

    // Rooms enum: PascalCase name → index (as const for exact literal types).
    out.push_str("export const Rooms = {\n");
    for (i, entry) in room.rooms.iter().enumerate() {
        let raw = dw.resolve_string(entry.name).unwrap_or_else(|_| format!("room_{i}"));
        let key = naming::room_name_to_pascal(&raw);
        let _ = writeln!(out, "  {key}: {i},");
    }
    out.push_str("} as const;\n");

    // Suppress unused variable warning
    let _ = obj_names;

    catalog.add(Asset {
        id: "data_rooms".into(),
        kind: AssetKind::Data,
        original_name: "rooms".into(),
        path: PathBuf::from("data/rooms.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}

/// Generate `data/objects.ts` — Classes name→index enum.
fn generate_objects(catalog: &mut AssetCatalog, obj_names: &[String]) {
    let mut out = String::new();
    out.push_str("export const Classes = {\n");
    for (i, name) in obj_names.iter().enumerate() {
        let _ = writeln!(out, "  {name}: {i},");
    }
    out.push_str("} as const;\n");

    catalog.add(Asset {
        id: "data_objects".into(),
        kind: AssetKind::Data,
        original_name: "objects".into(),
        path: PathBuf::from("data/objects.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}

/// Generate `data/asset_ids.d.ts` — ambient declarations for all named GML assets.
///
/// GML code references asset names (sprites, sounds, fonts, shaders, etc.) as
/// bare identifiers (e.g. `spr_player`, `snd_jump`). In the lifted TypeScript these
/// become global constant references that need `declare const` declarations so the
/// TypeScript compiler knows they exist and have type `number`.
fn generate_asset_ids(dw: &DataWin, catalog: &mut AssetCatalog) {
    let mut out = String::new();
    out.push_str("// GML asset ID declarations — auto-generated, do not edit.\n");
    out.push_str("// Each constant is the numeric asset index used by the GameMaker runtime.\n\n");

    // Helper: emit `declare const <name>: number;` for each resolved name.
    let mut emit_group = |label: &str, names: Vec<String>| {
        if names.is_empty() {
            return;
        }
        let _ = writeln!(out, "// {label}");
        for name in &names {
            if is_valid_js_ident(name) {
                let _ = writeln!(out, "declare const {name}: number;");
            }
        }
        out.push('\n');
    };

    // Sprites.
    let sprite_names: Vec<String> = dw.sprt()
        .map(|sprt| {
            sprt.sprites.iter().filter_map(|e| dw.resolve_string(e.name).ok()).collect()
        })
        .unwrap_or_default();
    emit_group("Sprites", sprite_names);

    // Sounds.
    let sound_names: Vec<String> = dw.sond()
        .map(|sond| {
            sond.sounds.iter().filter_map(|e| dw.resolve_string(e.name).ok()).collect()
        })
        .unwrap_or_default();
    emit_group("Sounds", sound_names);

    // Rooms (type=3 in pushref encoding).
    let room_names: Vec<String> = dw.room()
        .map(|room| {
            room.rooms.iter().filter_map(|e| dw.resolve_string(e.name).ok()).collect()
        })
        .unwrap_or_default();
    emit_group("Rooms", room_names);

    // Fonts (type=6).
    let font_names: Vec<String> = dw.font()
        .map(|font| {
            font.fonts.iter().filter_map(|e| dw.resolve_string(e.name).ok()).collect()
        })
        .unwrap_or_default();
    emit_group("Fonts", font_names);

    // Shaders (type=8 in pushref encoding).
    let shdr_names: Vec<String> = dw.shdr()
        .map(|shdr| {
            shdr.shaders.iter().filter_map(|e| dw.resolve_string(e.name).ok()).collect()
        })
        .unwrap_or_default();
    emit_group("Shaders", shdr_names);

    // Sequences (type=9; SEQN chunk; GMS2.3+ only).
    let seqn_names: Vec<String> = dw.seqn()
        .ok()
        .flatten()
        .map(|seqn| {
            seqn.sequences.iter().filter_map(|e| dw.resolve_string(e.name).ok()).collect()
        })
        .unwrap_or_default();
    emit_group("Sequences", seqn_names);

    catalog.add(Asset {
        id: "data_asset_ids".into(),
        kind: AssetKind::Data,
        original_name: "asset_ids".into(),
        path: PathBuf::from("data/asset_ids.d.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}
