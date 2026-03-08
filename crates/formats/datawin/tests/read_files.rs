use reincarnate_datawin::bytecode::decode;
use reincarnate_datawin::bytecode::encode;
use reincarnate_datawin::chunks::audo::Audo;
use reincarnate_datawin::chunks::code::Code;
use reincarnate_datawin::chunks::font::Font;
use reincarnate_datawin::chunks::func::Func;
use reincarnate_datawin::chunks::gen8::Gen8;
use reincarnate_datawin::chunks::glob::Glob;
use reincarnate_datawin::chunks::lang::Lang;
use reincarnate_datawin::chunks::objt::Objt;
use reincarnate_datawin::chunks::optn::Optn;
use reincarnate_datawin::chunks::room::Room;
use reincarnate_datawin::chunks::scpt::Scpt;
use reincarnate_datawin::chunks::sond::Sond;
use reincarnate_datawin::chunks::sprt::Sprt;
use reincarnate_datawin::chunks::tpag::Tpag;
use reincarnate_datawin::chunks::txtr::Txtr;
use reincarnate_datawin::chunks::vari::Vari;
use reincarnate_datawin::reader::ChunkIndex;
use reincarnate_datawin::string_table::StringTable;
use reincarnate_datawin::version::BytecodeVersion;
use reincarnate_datawin::DataWin;

fn load_if_exists(path: &str) -> Option<Vec<u8>> {
    std::fs::read(path).ok()
}

fn bounty_path() -> String {
    format!("{}/Bounty/data.win", env!("HOME"))
}

const UNDERTALE_PATH: &str = "/mnt/ssd/steam/steamapps/common/Undertale/assets/game.unx";
const CHRONICON_PATH: &str = "/mnt/ssd/steam/steamapps/common/Chronicon/data.win";

// ── Phase 1: ChunkIndex ─────────────────────────────────────────────

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn parse_bounty_chunks() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping: Bounty/data.win not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse Bounty data.win");
    assert_eq!(index.len(), 22);

    let magics: Vec<&str> = index.chunks().iter().map(|c| c.magic_str()).collect();
    assert_eq!(
        magics,
        [
            "GEN8", "OPTN", "EXTN", "SOND", "AGRP", "SPRT", "BGND", "PATH", "SCPT", "SHDR",
            "FONT", "TMLN", "OBJT", "ROOM", "DAFL", "TPAG", "CODE", "VARI", "FUNC", "STRG",
            "TXTR", "AUDO",
        ]
    );

    let gen8 = index.find(b"GEN8").expect("GEN8 not found");
    assert_eq!(gen8.offset, 8);
    assert_eq!(gen8.size, 252);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn parse_undertale_chunks() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping: Undertale game.unx not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse Undertale game.unx");
    assert_eq!(index.len(), 24);
    assert!(index.find(b"LANG").is_some());
    assert!(index.find(b"GLOB").is_some());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn parse_chronicon_chunks() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping: Chronicon data.win not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse Chronicon data.win");
    assert_eq!(index.len(), 31);
    assert!(index.find(b"TGIN").is_some());
    assert!(index.find(b"FEAT").is_some());
    assert!(index.find(b"FEDS").is_some());
    assert!(index.find(b"EMBI").is_some());
    assert!(index.find(b"CODE").is_none());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chunk_data_extraction() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping: Bounty/data.win not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse");
    let gen8_data = index.chunk_data(&data, b"GEN8").expect("GEN8 data");
    assert_eq!(gen8_data.len(), 252);
    assert_eq!(gen8_data[0], 1); // debug
    assert_eq!(gen8_data[1], 15); // bytecode version
}

// ── Phase 2: String Table + GEN8 ────────────────────────────────────

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_string_table() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let strg_entry = index.find(b"STRG").unwrap();
    let strg_data = index.chunk_data(&data, b"STRG").unwrap();
    let table = StringTable::parse(strg_data, strg_entry.data_offset()).unwrap();

    assert_eq!(table.len(), 2281);

    // First few strings from hex verification
    assert_eq!(table.get(0, &data).unwrap(), "prototype");
    assert_eq!(table.get(1, &data).unwrap(), "@@array@@");
    assert_eq!(table.get(2, &data).unwrap(), "arguments");
    assert_eq!(table.get(3, &data).unwrap(), "active");
    assert_eq!(table.get(4, &data).unwrap(), "mouse_check_button_pressed");
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_string_table() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let strg_entry = index.find(b"STRG").unwrap();
    let strg_data = index.chunk_data(&data, b"STRG").unwrap();
    let table = StringTable::parse(strg_data, strg_entry.data_offset()).unwrap();

    // Undertale should have many strings
    assert!(table.len() > 1000, "expected >1000 strings, got {}", table.len());

    // First string should be "prototype" (same GMS convention)
    assert_eq!(table.get(0, &data).unwrap(), "prototype");
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_gen8() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let gen8_data = index.chunk_data(&data, b"GEN8").unwrap();
    let gen8 = Gen8::parse(gen8_data).unwrap();

    assert_eq!(gen8.bytecode_version, BytecodeVersion::V15);
    assert_eq!(gen8.major, 1);
    assert_eq!(gen8.default_window_width, 640);
    assert_eq!(gen8.default_window_height, 480);
    assert_eq!(gen8.room_order.len(), 30);
    assert!(gen8.gms2_data.is_empty());

    // Resolve game name
    let name = gen8.name.resolve(&data).unwrap();
    assert!(!name.is_empty(), "game name should not be empty");
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_gen8() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let gen8_data = index.chunk_data(&data, b"GEN8").unwrap();
    let gen8 = Gen8::parse(gen8_data).unwrap();

    assert_eq!(gen8.bytecode_version, BytecodeVersion::V16);
    assert_eq!(gen8.major, 1);
    assert_eq!(gen8.default_window_width, 640);
    assert_eq!(gen8.default_window_height, 480);
    assert_eq!(gen8.room_order.len(), 336);
    assert!(gen8.gms2_data.is_empty());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chronicon_gen8() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let gen8_data = index.chunk_data(&data, b"GEN8").unwrap();
    let gen8 = Gen8::parse(gen8_data).unwrap();

    assert_eq!(gen8.bytecode_version, BytecodeVersion::V17);
    assert_eq!(gen8.major, 2); // GMS2
    assert_eq!(gen8.default_window_width, 960);
    assert_eq!(gen8.default_window_height, 540);
    assert_eq!(gen8.room_order.len(), 6);
    assert!(!gen8.gms2_data.is_empty(), "GMS2 data should be present");
    assert_eq!(gen8.gms2_data.len(), 68);
}

// ── Phase 3: CODE + Bytecode Decoder ────────────────────────────────

fn parse_code_for(data: &[u8]) -> (Code, Gen8) {
    let index = ChunkIndex::parse(data).unwrap();
    let gen8_data = index.chunk_data(data, b"GEN8").unwrap();
    let gen8 = Gen8::parse(gen8_data).unwrap();
    let code_entry = index.find(b"CODE").unwrap();
    let code_data = index.chunk_data(data, b"CODE").unwrap();
    let code = Code::parse(code_data, code_entry.data_offset(), gen8.bytecode_version).unwrap();
    (code, gen8)
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_code_entries() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let (code, _gen8) = parse_code_for(&data);

    assert_eq!(code.entries.len(), 197);

    // First entry should be a gml_Script
    let first = &code.entries[0];
    let name = first.name.resolve(&data).unwrap();
    assert_eq!(name, "gml_Script_button_click");
    assert_eq!(first.length, 324);
    assert_eq!(first.locals_count, 1);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_decode_all_bytecode() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let (code, _gen8) = parse_code_for(&data);

    let mut total_instructions = 0;
    for (i, entry) in code.entries.iter().enumerate() {
        let bc = code
            .entry_bytecode(i, &data)
            .unwrap_or_else(|| panic!("bytecode for entry {}", i));
        let instructions = decode::decode(bc)
            .unwrap_or_else(|e| {
                let name = entry.name.resolve(&data).unwrap_or_default();
                panic!("decode entry {} ({}): {}", i, name, e)
            });
        total_instructions += instructions.len();
    }

    // Bounty should have a reasonable number of instructions
    assert!(
        total_instructions > 1000,
        "expected >1000 instructions total, got {}",
        total_instructions
    );
    eprintln!(
        "Bounty: {} entries, {} total instructions",
        code.entries.len(),
        total_instructions
    );
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_code_entries() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let (code, _gen8) = parse_code_for(&data);

    // Undertale should have many code entries
    assert!(
        code.entries.len() > 100,
        "expected >100 code entries, got {}",
        code.entries.len()
    );
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_decode_all_bytecode() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let (code, _gen8) = parse_code_for(&data);

    let mut total_instructions = 0;
    let mut errors = 0;
    for (i, entry) in code.entries.iter().enumerate() {
        let bc = code
            .entry_bytecode(i, &data)
            .unwrap_or_else(|| panic!("bytecode for entry {}", i));
        match decode::decode(bc) {
            Ok(insts) => total_instructions += insts.len(),
            Err(e) => {
                let name = entry.name.resolve(&data).unwrap_or_default();
                eprintln!("  decode error in {}: {}", name, e);
                errors += 1;
            }
        }
    }

    eprintln!(
        "Undertale: {} entries, {} total instructions, {} errors",
        code.entries.len(),
        total_instructions,
        errors
    );
    assert_eq!(errors, 0, "all entries should decode without errors");
}

// ── Phase 4: FUNC + VARI ───────────────────────────────────────────

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_func() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let gen8 = Gen8::parse(index.chunk_data(&data, b"GEN8").unwrap()).unwrap();
    let func_data = index.chunk_data(&data, b"FUNC").unwrap();
    let func = Func::parse(func_data, gen8.bytecode_version).unwrap();

    assert_eq!(func.functions.len(), 101);
    assert_eq!(func.code_locals.len(), 197);

    // First function
    let f0 = &func.functions[0];
    let name = f0.name.resolve(&data).unwrap();
    assert_eq!(name, "mouse_check_button_pressed");
    assert_eq!(f0.occurrences, 2);

    // First code locals entry
    let cl0 = &func.code_locals[0];
    let cl_name = cl0.name.resolve(&data).unwrap();
    assert_eq!(cl_name, "gml_Script_button_click");
    assert_eq!(cl0.locals.len(), 1);
    assert_eq!(cl0.locals[0].name.resolve(&data).unwrap(), "arguments");
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_vari() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let gen8 = Gen8::parse(index.chunk_data(&data, b"GEN8").unwrap()).unwrap();
    let vari_data = index.chunk_data(&data, b"VARI").unwrap();
    let vari = Vari::parse(vari_data, gen8.bytecode_version).unwrap();

    assert_eq!(vari.variables.len(), 610);
    assert_eq!(vari.instance_var_count_max, 206);
    assert_eq!(vari.max_local_var_count, 12);

    // First variable
    let v0 = &vari.variables[0];
    assert_eq!(v0.name.resolve(&data).unwrap(), "prototype");
    assert_eq!(v0.instance_type, -1); // self
    assert_eq!(v0.var_id, 0);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_func() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let gen8 = Gen8::parse(index.chunk_data(&data, b"GEN8").unwrap()).unwrap();
    let func_data = index.chunk_data(&data, b"FUNC").unwrap();
    let func = Func::parse(func_data, gen8.bytecode_version).unwrap();

    assert!(
        func.functions.len() > 100,
        "expected >100 functions, got {}",
        func.functions.len()
    );
    // Code locals should match code entry count
    assert!(func.code_locals.len() > 100);

    eprintln!(
        "Undertale: {} functions, {} code_locals entries",
        func.functions.len(),
        func.code_locals.len()
    );
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_vari() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let gen8 = Gen8::parse(index.chunk_data(&data, b"GEN8").unwrap()).unwrap();
    let vari_data = index.chunk_data(&data, b"VARI").unwrap();
    let vari = Vari::parse(vari_data, gen8.bytecode_version).unwrap();

    assert!(
        vari.variables.len() > 100,
        "expected >100 variables, got {}",
        vari.variables.len()
    );
    eprintln!(
        "Undertale: {} variables, instance_var_max={}, max_local={}",
        vari.variables.len(),
        vari.instance_var_count_max,
        vari.max_local_var_count
    );
}

// ── Phase 5: SCPT + OBJT ─────────────────────────────────────────

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_scpt() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let scpt_entry = index.find(b"SCPT").unwrap();
    let scpt_data = index.chunk_data(&data, b"SCPT").unwrap();
    let scpt = Scpt::parse(scpt_data, scpt_entry.data_offset(), &data).unwrap();

    assert_eq!(scpt.scripts.len(), 61);

    // First script
    let s0 = &scpt.scripts[0];
    assert_eq!(s0.name.resolve(&data).unwrap(), "button_click");
    assert_eq!(s0.code_id, 0);

    // Scripts should map to sequential code IDs
    for (i, s) in scpt.scripts.iter().enumerate() {
        assert_eq!(
            s.code_id, i as u32,
            "script {} code_id mismatch",
            s.name.resolve(&data).unwrap_or_default()
        );
    }
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_objt() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let objt_data = index.chunk_data(&data, b"OBJT").unwrap();
    let gen8 = Gen8::parse(index.chunk_data(&data, b"GEN8").unwrap()).unwrap();
    let objt = Objt::parse(objt_data, &data, gen8.bytecode_version).unwrap();

    assert_eq!(objt.objects.len(), 86);

    // First object
    let obj0 = &objt.objects[0];
    assert_eq!(obj0.name.resolve(&data).unwrap(), "obj_button_base");
    assert_eq!(obj0.sprite_index, 0);
    assert!(obj0.visible);
    assert!(!obj0.solid);
    assert_eq!(obj0.depth, 0);
    assert!(!obj0.persistent);
    assert_eq!(obj0.parent_index, -100);
    assert_eq!(obj0.mask_index, -1);

    // Default physics values
    assert!(!obj0.physics_enabled);
    assert!((obj0.physics_density - 0.5).abs() < f32::EPSILON);
    assert!((obj0.physics_restitution - 0.1).abs() < f32::EPSILON);
    assert!((obj0.physics_friction - 0.2).abs() < f32::EPSILON);
    assert!(obj0.physics_awake);
    assert!(!obj0.physics_kinematic);
    assert!(obj0.physics_vertices.is_empty());

    // Event structure
    assert_eq!(obj0.events.len(), 12);

    // Create event (type 0): 1 sub-entry with subtype 0
    assert_eq!(obj0.events[0].len(), 1);
    assert_eq!(obj0.events[0][0].subtype, 0);
    assert_eq!(obj0.events[0][0].actions.len(), 1);

    // Mouse event (type 6): 2 sub-entries (mouse enter=11, mouse leave=10)
    assert_eq!(obj0.events[6].len(), 2);
    assert_eq!(obj0.events[6][0].subtype, 11); // mouse enter
    assert_eq!(obj0.events[6][1].subtype, 10); // mouse leave

    // All events should have valid code IDs
    let (code, _) = parse_code_for(&data);
    for event_list in &obj0.events {
        for event in event_list {
            for action in &event.actions {
                assert!(
                    (action.code_id as usize) < code.entries.len(),
                    "code_id {} out of range (max {})",
                    action.code_id,
                    code.entries.len()
                );
            }
        }
    }
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_objt_code_linkage() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let objt_data = index.chunk_data(&data, b"OBJT").unwrap();
    let gen8 = Gen8::parse(index.chunk_data(&data, b"GEN8").unwrap()).unwrap();
    let objt = Objt::parse(objt_data, &data, gen8.bytecode_version).unwrap();
    let (code, _) = parse_code_for(&data);

    // Verify code entry names match object+event naming convention
    let obj0 = &objt.objects[0];
    let obj_name = obj0.name.resolve(&data).unwrap();

    let event_type_names = [
        "Create", "Destroy", "Alarm", "Step", "Collision", "Keyboard", "Mouse", "Other", "Draw",
        "KeyPress", "KeyRelease", "Trigger",
    ];

    for (type_idx, event_list) in obj0.events.iter().enumerate() {
        for event in event_list {
            for action in &event.actions {
                let code_name = code.entries[action.code_id as usize]
                    .name
                    .resolve(&data)
                    .unwrap();
                let expected_suffix = format!(
                    "gml_Object_{}_{}_{}",
                    obj_name, event_type_names[type_idx], event.subtype
                );
                assert_eq!(
                    code_name, expected_suffix,
                    "code entry name mismatch for {}.{}.{}",
                    obj_name, event_type_names[type_idx], event.subtype
                );
            }
        }
    }
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_scpt() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let scpt_entry = index.find(b"SCPT").unwrap();
    let scpt_data = index.chunk_data(&data, b"SCPT").unwrap();
    let scpt = Scpt::parse(scpt_data, scpt_entry.data_offset(), &data).unwrap();

    assert!(
        scpt.scripts.len() > 100,
        "expected >100 scripts, got {}",
        scpt.scripts.len()
    );
    eprintln!("Undertale: {} scripts", scpt.scripts.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_objt() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let objt_data = index.chunk_data(&data, b"OBJT").unwrap();
    let gen8 = Gen8::parse(index.chunk_data(&data, b"GEN8").unwrap()).unwrap();
    let objt = Objt::parse(objt_data, &data, gen8.bytecode_version).unwrap();

    assert!(
        objt.objects.len() > 100,
        "expected >100 objects, got {}",
        objt.objects.len()
    );

    // All objects should have at least 12 event types (13 for v16+)
    for (i, obj) in objt.objects.iter().enumerate() {
        assert!(
            obj.events.len() >= 12,
            "object {} ({}) has {} event types, expected >= 12",
            i,
            obj.name.resolve(&data).unwrap_or_default(),
            obj.events.len()
        );
    }

    eprintln!("Undertale: {} objects", objt.objects.len());
}

// ── Phase 6: Asset Chunks ─────────────────────────────────────────

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_sprt() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let sprt_data = index.chunk_data(&data, b"SPRT").unwrap();
    let sprt = Sprt::parse(sprt_data, &data).unwrap();

    assert_eq!(sprt.sprites.len(), 41);

    let s0 = &sprt.sprites[0];
    assert_eq!(s0.width, 200);
    assert_eq!(s0.height, 40);
    assert_eq!(s0.tpag_indices.len(), 5);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_tpag() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let tpag_data = index.chunk_data(&data, b"TPAG").unwrap();
    let tpag = Tpag::parse(tpag_data, &data).unwrap();

    assert_eq!(tpag.items.len(), 118);

    let t0 = &tpag.items[0];
    assert_eq!(t0.source_width, 200);
    assert_eq!(t0.source_height, 40);
    assert_eq!(t0.texture_page_id, 0);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_txtr() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let txtr_data = index.chunk_data(&data, b"TXTR").unwrap();
    let txtr = Txtr::parse(txtr_data, &data).unwrap();

    assert_eq!(txtr.textures.len(), 1);

    // Texture data should start with PNG magic
    let tex_data = txtr.texture_data(0, &data).unwrap();
    assert_eq!(&tex_data[..4], b"\x89PNG");
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_font() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let font_data = index.chunk_data(&data, b"FONT").unwrap();
    let font = Font::parse(font_data, &data).unwrap();

    assert_eq!(font.fonts.len(), 3);

    let f0 = &font.fonts[0];
    assert_eq!(f0.size, 12);
    assert_eq!(f0.range_start, 0x20);
    assert_eq!(f0.range_end, 0x7F);
    assert_eq!(f0.glyphs.len(), 96); // 0x20..=0x7F
    assert!((f0.scale_x - 1.0).abs() < f32::EPSILON);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_room() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let room_data = index.chunk_data(&data, b"ROOM").unwrap();
    let room = Room::parse(room_data, &data).unwrap();

    assert_eq!(room.rooms.len(), 30);

    let r0 = &room.rooms[0];
    assert_eq!(r0.width, 640);
    assert_eq!(r0.height, 480);
    assert_eq!(r0.speed, 60);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_optn() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let optn_data = index.chunk_data(&data, b"OPTN").unwrap();
    let optn = Optn::parse(optn_data).unwrap();

    // Bounty has no user-defined constants
    assert_eq!(optn.constants.len(), 0);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_optn() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let optn_data = index.chunk_data(&data, b"OPTN").unwrap();
    let optn = Optn::parse(optn_data).unwrap();

    assert_eq!(optn.constants.len(), 2);
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_sprt() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let sprt_data = index.chunk_data(&data, b"SPRT").unwrap();
    let sprt = Sprt::parse(sprt_data, &data).unwrap();

    assert_eq!(sprt.sprites.len(), 2583);
    eprintln!("Undertale: {} sprites", sprt.sprites.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_tpag() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let tpag_data = index.chunk_data(&data, b"TPAG").unwrap();
    let tpag = Tpag::parse(tpag_data, &data).unwrap();

    assert_eq!(tpag.items.len(), 6550);
    eprintln!("Undertale: {} texture page items", tpag.items.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_txtr() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let txtr_data = index.chunk_data(&data, b"TXTR").unwrap();
    let txtr = Txtr::parse(txtr_data, &data).unwrap();

    assert_eq!(txtr.textures.len(), 26);

    // All textures should have PNG data
    for i in 0..txtr.textures.len() {
        let tex_data = txtr.texture_data(i, &data).unwrap();
        assert_eq!(
            &tex_data[..4],
            b"\x89PNG",
            "texture {} should be PNG",
            i
        );
    }
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_sond() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let sond_data = index.chunk_data(&data, b"SOND").unwrap();
    let sond = Sond::parse(sond_data, &data).unwrap();

    assert_eq!(sond.sounds.len(), 443);

    // Check volume is in valid range
    for s in &sond.sounds {
        assert!(
            (0.0..=1.0).contains(&s.volume),
            "sound volume {} out of range",
            s.volume
        );
    }
    eprintln!("Undertale: {} sounds", sond.sounds.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_audo() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let audo_entry = index.find(b"AUDO").unwrap();
    let audo_data = index.chunk_data(&data, b"AUDO").unwrap();
    let audo = Audo::parse(audo_data, audo_entry.data_offset()).unwrap();

    assert_eq!(audo.entries.len(), 231);

    // First audio entry should have RIFF/WAV header
    let audio = audo.audio_data(0, &data).unwrap();
    assert_eq!(&audio[..4], b"RIFF", "first audio entry should be WAV");
    eprintln!("Undertale: {} audio entries", audo.entries.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_font() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let font_data = index.chunk_data(&data, b"FONT").unwrap();
    let font = Font::parse(font_data, &data).unwrap();

    assert_eq!(font.fonts.len(), 20);
    eprintln!("Undertale: {} fonts", font.fonts.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_room() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let room_data = index.chunk_data(&data, b"ROOM").unwrap();
    let room = Room::parse(room_data, &data).unwrap();

    assert_eq!(room.rooms.len(), 336);

    let r0 = &room.rooms[0];
    assert_eq!(r0.width, 640);
    assert_eq!(r0.height, 480);
    assert_eq!(r0.speed, 30);
    eprintln!("Undertale: {} rooms", room.rooms.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_glob() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let glob_data = index.chunk_data(&data, b"GLOB").unwrap();
    let glob = Glob::parse(glob_data).unwrap();

    // Undertale's GLOB is empty
    eprintln!("Undertale: {} global scripts", glob.script_ids.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_lang() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let lang_data = index.chunk_data(&data, b"LANG").unwrap();
    let lang = Lang::parse(lang_data).unwrap();

    assert_eq!(lang.entry_count, 1);
    eprintln!(
        "Undertale: {} language entries (count={})",
        lang.entries.len(),
        lang.entry_count
    );
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chronicon_sprt() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let sprt_data = index.chunk_data(&data, b"SPRT").unwrap();
    let sprt = Sprt::parse(sprt_data, &data).unwrap();

    assert_eq!(sprt.sprites.len(), 1569);
    eprintln!("Chronicon: {} sprites", sprt.sprites.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chronicon_tpag() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let tpag_data = index.chunk_data(&data, b"TPAG").unwrap();
    let tpag = Tpag::parse(tpag_data, &data).unwrap();

    assert_eq!(tpag.items.len(), 16989);
    eprintln!("Chronicon: {} texture page items", tpag.items.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chronicon_txtr() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let txtr_data = index.chunk_data(&data, b"TXTR").unwrap();
    let txtr = Txtr::parse(txtr_data, &data).unwrap();

    assert_eq!(txtr.textures.len(), 10);
    eprintln!("Chronicon: {} textures", txtr.textures.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chronicon_sond() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let sond_data = index.chunk_data(&data, b"SOND").unwrap();
    let sond = Sond::parse(sond_data, &data).unwrap();

    assert_eq!(sond.sounds.len(), 1357);
    eprintln!("Chronicon: {} sounds", sond.sounds.len());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chronicon_room() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let room_data = index.chunk_data(&data, b"ROOM").unwrap();
    let room = Room::parse(room_data, &data).unwrap();

    assert_eq!(room.rooms.len(), 6);

    let r0 = &room.rooms[0];
    assert_eq!(r0.width, 4000);
    assert_eq!(r0.height, 4000);
    eprintln!("Chronicon: {} rooms", room.rooms.len());
}

// ── Phase 7: DataWin Lazy Wrapper + Round-Trip Writer ─────────────

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_datawin_lazy() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let dw = DataWin::parse(data).unwrap();

    // Lazy access — each chunk parsed on first call
    assert_eq!(dw.gen8().unwrap().bytecode_version, BytecodeVersion::V15);
    assert_eq!(dw.bytecode_version().unwrap(), BytecodeVersion::V15);
    assert_eq!(dw.strings().unwrap().len(), 2281);
    assert_eq!(dw.code().unwrap().entries.len(), 197);
    assert_eq!(dw.func().unwrap().functions.len(), 101);
    assert_eq!(dw.vari().unwrap().variables.len(), 610);
    assert_eq!(dw.objt().unwrap().objects.len(), 86);
    assert_eq!(dw.scpt().unwrap().scripts.len(), 61);
    assert_eq!(dw.sprt().unwrap().sprites.len(), 41);
    assert_eq!(dw.tpag().unwrap().items.len(), 118);
    assert_eq!(dw.txtr().unwrap().textures.len(), 1);
    assert_eq!(dw.font().unwrap().fonts.len(), 3);
    assert_eq!(dw.room().unwrap().rooms.len(), 30);
    assert_eq!(dw.optn().unwrap().constants.len(), 0);

    // Optional chunks
    assert!(dw.glob().unwrap().is_none());
    assert!(dw.lang().unwrap().is_none());

    // String resolution
    let gen8 = dw.gen8().unwrap();
    let name = dw.resolve_string(gen8.name).unwrap();
    assert!(!name.is_empty());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_datawin_lazy() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let dw = DataWin::parse(data).unwrap();

    assert_eq!(dw.bytecode_version().unwrap(), BytecodeVersion::V16);
    assert_eq!(dw.index().len(), 24);
    assert!(dw.has_chunk(b"LANG"));
    assert!(dw.has_chunk(b"GLOB"));
    assert!(dw.glob().unwrap().is_some());
    assert!(dw.lang().unwrap().is_some());
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_round_trip() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let chunks = reincarnate_datawin::writer::extract_chunks(&index, &data);
    let reassembled = reincarnate_datawin::writer::assemble_form(&chunks);

    assert_eq!(
        data.len(),
        reassembled.len(),
        "round-trip size mismatch: {} vs {}",
        data.len(),
        reassembled.len()
    );
    assert_eq!(data, reassembled, "round-trip byte mismatch");
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_round_trip() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let chunks = reincarnate_datawin::writer::extract_chunks(&index, &data);
    let reassembled = reincarnate_datawin::writer::assemble_form(&chunks);

    assert_eq!(
        data.len(),
        reassembled.len(),
        "round-trip size mismatch"
    );
    assert_eq!(data, reassembled, "round-trip byte mismatch");
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn chronicon_round_trip() {
    let Some(data) = load_if_exists(CHRONICON_PATH) else {
        eprintln!("skipping");
        return;
    };
    let index = ChunkIndex::parse(&data).unwrap();
    let chunks = reincarnate_datawin::writer::extract_chunks(&index, &data);
    let reassembled = reincarnate_datawin::writer::assemble_form(&chunks);

    assert_eq!(
        data.len(),
        reassembled.len(),
        "round-trip size mismatch"
    );
    assert_eq!(data, reassembled, "round-trip byte mismatch");
}

// ── Phase 8: Bytecode Encoder Round-Trip ──────────────────────────

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn bounty_bytecode_round_trip() {
    let Some(data) = load_if_exists(&bounty_path()) else {
        eprintln!("skipping");
        return;
    };
    let (code, _gen8) = parse_code_for(&data);

    let mut total = 0;
    let mut matched = 0;
    for (i, entry) in code.entries.iter().enumerate() {
        let bc = code.entry_bytecode(i, &data).unwrap();
        let instructions = decode::decode(bc).unwrap();
        let encoded = encode::encode(&instructions);

        total += 1;
        if bc == encoded.as_slice() {
            matched += 1;
        } else {
            let name = entry.name.resolve(&data).unwrap_or_default();
            for (j, (a, b)) in bc.iter().zip(encoded.iter()).enumerate() {
                if a != b {
                    panic!(
                        "bytecode round-trip mismatch in {} at byte {:#x}: expected {:#04x}, got {:#04x}",
                        name, j, a, b
                    );
                }
            }
            if bc.len() != encoded.len() {
                panic!(
                    "bytecode round-trip length mismatch in {}: {} vs {}",
                    name,
                    bc.len(),
                    encoded.len()
                );
            }
        }
    }

    assert_eq!(matched, total, "not all entries matched");
    eprintln!(
        "Bounty bytecode round-trip: {}/{} entries matched",
        matched, total
    );
}

#[test]
#[ignore = "requires local game files; run with --include-ignored"]
fn undertale_bytecode_round_trip() {
    let Some(data) = load_if_exists(UNDERTALE_PATH) else {
        eprintln!("skipping");
        return;
    };
    let (code, _gen8) = parse_code_for(&data);

    let mut total = 0;
    let mut matched = 0;
    let mut first_error = None;
    for (i, entry) in code.entries.iter().enumerate() {
        let bc = code.entry_bytecode(i, &data).unwrap();
        let instructions = decode::decode(bc).unwrap();
        let encoded = encode::encode(&instructions);

        total += 1;
        if bc == encoded.as_slice() {
            matched += 1;
        } else if first_error.is_none() {
            let name = entry.name.resolve(&data).unwrap_or_default();
            first_error = Some(format!(
                "first mismatch in {} (entry {}): orig={} bytes, encoded={} bytes",
                name,
                i,
                bc.len(),
                encoded.len()
            ));
        }
    }

    if let Some(err) = first_error {
        panic!(
            "bytecode round-trip: {}/{} matched, {}",
            matched, total, err
        );
    }
    eprintln!(
        "Undertale bytecode round-trip: {}/{} entries matched",
        matched, total
    );
}
