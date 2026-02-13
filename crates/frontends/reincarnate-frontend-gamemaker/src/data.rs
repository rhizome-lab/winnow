use std::fmt::Write;
use std::path::PathBuf;

use datawin::DataWin;
use reincarnate_core::project::{Asset, AssetCatalog, AssetKind};

use crate::naming;

/// Generate TypeScript data files from parsed chunks and add them to the catalog.
pub fn generate_data_files(dw: &DataWin, catalog: &mut AssetCatalog, obj_names: &[String]) {
    generate_textures(dw, catalog);
    generate_sprites(dw, catalog);
    generate_fonts(dw, catalog);
    generate_rooms(dw, catalog, obj_names);
    generate_objects(catalog, obj_names);
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

    // Sprites enum: PascalCase name → index
    out.push_str("export const Sprites: Record<string, number> = {\n");
    for (i, sprite) in sprt.sprites.iter().enumerate() {
        let raw = dw.resolve_string(sprite.name).unwrap_or_else(|_| format!("spr_{i}"));
        let key = naming::sprite_name_to_pascal(&raw);
        let _ = writeln!(out, "  {key}: {i},");
    }
    out.push_str("};\n");

    catalog.add(Asset {
        id: "data_sprites".into(),
        kind: AssetKind::Data,
        original_name: "sprites".into(),
        path: PathBuf::from("data/sprites.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
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

    // Rooms enum: PascalCase name → index
    out.push_str("export const Rooms: Record<string, number> = {\n");
    for (i, entry) in room.rooms.iter().enumerate() {
        let raw = dw.resolve_string(entry.name).unwrap_or_else(|_| format!("room_{i}"));
        let key = naming::room_name_to_pascal(&raw);
        let _ = writeln!(out, "  {key}: {i},");
    }
    out.push_str("};\n");

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
    out.push_str("export const Classes: Record<string, number> = {\n");
    for (i, name) in obj_names.iter().enumerate() {
        let _ = writeln!(out, "  {name}: {i},");
    }
    out.push_str("};\n");

    catalog.add(Asset {
        id: "data_objects".into(),
        kind: AssetKind::Data,
        original_name: "objects".into(),
        path: PathBuf::from("data/objects.ts"),
        size: out.len() as u64,
        data: out.into_bytes(),
    });
}
