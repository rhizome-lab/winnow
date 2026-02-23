//! Fixture generator for datawin unit tests.
//!
//! Generates small synthetic `data.win`-format binary files **and** paired
//! JSON expected-value files into `tests/fixtures/`. Both are committed to
//! the repo and serve dual purposes:
//!
//!   1. **Regression tests** for the Rust parser (`tests/fixture_tests.rs`).
//!   2. **Reference inputs** for the Kaitai Struct specs:
//!      `game_maker_data.ksy` and `gml_bytecode.ksy`.
//!
//! The JSON files are the cross-language contract:
//!   - Fields without `_` prefix: validated by both the Kaitai Python
//!     validator (`tests/kaitai_validate.py`) and the Rust fixture tests.
//!   - Fields with `_` prefix: Rust-only (resolved strings, decoded operand
//!     values that require full-file context or imperative logic).
//!
//! # Usage
//!
//! ```sh
//! cargo run -p datawin --bin gen_fixtures
//! ```
//!
//! # Kaitai validation workflow
//!
//! ```sh
//! cd crates/formats/datawin
//! ksc -t python game_maker_data.ksy gml_bytecode.ksy
//! pip install kaitaistruct
//! python3 tests/kaitai_validate.py
//! ```
//!
//! Or open a `.bin` file in the interactive Kaitai Web IDE at
//! <https://ide.kaitai.io/> together with the matching `.ksy`.

use datawin::bytecode::decode::{Instruction, Operand};
use datawin::bytecode::opcode::Opcode;
use datawin::bytecode::types::{ComparisonKind, DataType, VariableRef};
use datawin::bytecode::{decode, encode};
use datawin::cursor::Writer;
use datawin::writer::{assemble_form, OutputChunk};

const FIXTURES_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");

/// Kaitai Struct limitations documented in every JSON file.
const KAITAI_LIMITATIONS: &str = r#"[
    "push_body: 'size: 0' placeholder — type1-conditional operand length is not declaratively expressible in Kaitai 0.11",
    "vari_body / func_body in game_maker_data.ksy: stored as raw size-eos blob — version-conditional layout requires cross-chunk GEN8.bytecode_version",
    "string_ref resolution: Kaitai can read the raw u32 offset but cannot follow the STRG pointer without _root._io seek",
    "shared_blob per-entry length: requires two-pass gap algorithm (sort offsets, compute gaps) — not expressible in Kaitai"
  ]"#;

fn main() -> std::io::Result<()> {
    std::fs::create_dir_all(FIXTURES_DIR)?;

    write_fixture("v15_minimal", build_v15_minimal())?;
    write_fixture("v15_bytecode_variety", build_v15_bytecode_variety())?;
    write_fixture("v15_break_signals", build_v15_break_signals())?;
    write_fixture("v14_minimal", build_v14_minimal())?;
    write_fixture("v15_vari_func", build_v15_vari_func())?;
    write_fixture("v15_more_opcodes", build_v15_more_opcodes())?;
    write_fixture("v15_scpt", build_v15_scpt())?;
    write_fixture("v15_shared_blob", build_v15_shared_blob())?;

    Ok(())
}

fn write_fixture(name: &str, (bin, json): (Vec<u8>, String)) -> std::io::Result<()> {
    let bin_path = format!("{FIXTURES_DIR}/{name}.bin");
    let json_path = format!("{FIXTURES_DIR}/{name}.json");
    std::fs::write(&bin_path, &bin)?;
    std::fs::write(&json_path, json.as_bytes())?;
    println!("wrote {name}.bin ({} bytes) + {name}.json", bin.len());
    Ok(())
}

// ── String pool ──────────────────────────────────────────────────────────────

/// Build STRG chunk data.
///
/// Layout: count(u32) + pointer_list(n × u32) + strings(u32 len + bytes + NUL).
/// Pointer list entries are absolute file offsets of each string's `u32 len`
/// prefix, as expected by `StringTable::get`.
fn build_strg(strings: &[&str], chunk_data_abs: usize) -> Vec<u8> {
    let n = strings.len();
    let mut w = Writer::new();

    w.write_u32(n as u32);

    let ptr_base = w.position();
    for _ in 0..n {
        w.write_u32(0);
    }

    for (i, s) in strings.iter().enumerate() {
        let abs = (chunk_data_abs + w.position()) as u32;
        w.patch_u32(ptr_base + i * 4, abs);
        w.write_gm_string(s);
    }

    w.into_bytes()
}

/// Compute the `StringRef` value (absolute offset of char data) for string
/// `index`. This is what GEN8, CODE, etc. store as `name_ref` fields.
fn str_ref(index: usize, strings: &[&str], strg_data_abs: usize) -> u32 {
    // Layout: count(4) + ptrs(4*n) + strings…
    let ptr_section = 4 + 4 * strings.len();
    let mut len_prefix_abs = strg_data_abs + ptr_section;
    for s in strings[..index].iter() {
        len_prefix_abs += 4 + s.len() + 1;
    }
    // StringRef points to char data = len_prefix + 4.
    (len_prefix_abs + 4) as u32
}

// ── GEN8 chunk ───────────────────────────────────────────────────────────────

/// Build a GEN8 chunk with the given bytecode version and zero rooms.
///
/// Fixed size: 132 bytes (valid for bc_version >= 14).
fn build_gen8(
    bc_version: u8,
    filename_ref: u32,
    config_ref: u32,
    name_ref: u32,
    display_name_ref: u32,
) -> Vec<u8> {
    let mut w = Writer::new();

    w.write_u8(0); // is_debug_disabled = false
    w.write_u8(bc_version);
    w.write_u16(0); // padding

    w.write_u32(filename_ref);
    w.write_u32(config_ref);
    w.write_u32(0); // last_obj
    w.write_u32(0); // last_tile
    w.write_u32(1); // game_id = 1
    w.write_bytes(&[0u8; 16]); // guid
    w.write_u32(name_ref);
    w.write_u32(1); // major
    w.write_u32(0); // minor
    w.write_u32(0); // release
    w.write_u32(0); // build
    w.write_u32(1024); // default_window_width
    w.write_u32(768); // default_window_height
    w.write_u32(0); // info
    w.write_u32(0); // license_crc32
    w.write_bytes(&[0u8; 16]); // license_md5
    w.write_u64(0); // timestamp
    w.write_u32(display_name_ref);
    w.write_u64(0); // active_targets
    w.write_u64(0); // function_classifications
    w.write_i32(0); // steam_app_id
    w.write_u32(0); // debugger_port (bc >= 14)
    w.write_u32(0); // room_count = 0

    let data = w.into_bytes();
    assert_eq!(data.len(), 132, "GEN8 size mismatch");
    data
}

// ── CODE chunk ───────────────────────────────────────────────────────────────

/// Build a BC >= 15 CODE chunk (extended entry format).
///
/// Each entry has its bytecode blob inline (immediately after the 20-byte
/// header), so `bc_rel_addr = 8`.
fn build_code_v15(entries: &[(u32, &[u8])], code_data_abs: usize) -> Vec<u8> {
    let n = entries.len();
    let mut w = Writer::new();

    w.write_u32(n as u32);
    let ptr_base = w.position();
    for _ in 0..n {
        w.write_u32(0);
    }

    for (i, (name_ref, bytecode)) in entries.iter().enumerate() {
        let abs = (code_data_abs + w.position()) as u32;
        w.patch_u32(ptr_base + i * 4, abs);

        w.write_u32(*name_ref);
        w.write_u32(bytecode.len() as u32);
        w.write_u16(0); // locals_count
        w.write_u16(0); // args_count
        w.write_i32(8); // bc_rel_addr: blob follows 8 bytes past this field
        w.write_u32(0); // offset_in_blob
        w.write_bytes(bytecode);
    }

    w.into_bytes()
}

/// Build a BC >= 15 CODE chunk with one entry having explicit locals/args.
fn build_code_v15_with_meta(
    name_ref: u32,
    bytecode: &[u8],
    locals_count: u16,
    args_count: u16,
    code_data_abs: usize,
) -> Vec<u8> {
    let mut w = Writer::new();

    w.write_u32(1); // count
    let ptr_pos = w.position();
    w.write_u32(0); // ptr[0]

    let abs = (code_data_abs + w.position()) as u32;
    w.patch_u32(ptr_pos, abs);

    w.write_u32(name_ref);
    w.write_u32(bytecode.len() as u32);
    w.write_u16(locals_count);
    w.write_u16(args_count);
    w.write_i32(8);
    w.write_u32(0);
    w.write_bytes(bytecode);

    w.into_bytes()
}

/// Build a BC >= 15 CODE chunk with a GMS2.3+ shared blob (two entries).
///
/// Parent and child share one blob; parent's code starts at offset 0, child's
/// at offset `parent_bc.len()`. The two-pass length algorithm in the parser
/// correctly splits the blob.
fn build_code_v15_shared_blob(
    parent_name_ref: u32,
    parent_bc: &[u8],
    child_name_ref: u32,
    child_bc: &[u8],
    code_data_abs: usize,
) -> Vec<u8> {
    let parent_len = parent_bc.len() as u32;
    let child_len = child_bc.len() as u32;
    let total_blob_len = parent_len + child_len;

    let mut w = Writer::new();

    w.write_u32(2); // count
    let ptr_base = w.position(); // = 4
    w.write_u32(0); // ptr[0]
    w.write_u32(0); // ptr[1]

    // Entry 0 (parent): starts at position 12 within the data.
    let entry0_pos = w.position(); // = 12
    let abs0 = (code_data_abs + entry0_pos) as u32;
    w.patch_u32(ptr_base, abs0);

    // The shared blob begins immediately after the 20-byte entry-0 header.
    let blob_abs = code_data_abs + entry0_pos + 20;

    w.write_u32(parent_name_ref);
    w.write_u32(total_blob_len); // blob_length = total (both entries share it)
    w.write_u16(0); // locals_count
    w.write_u16(0); // args_count
    // bc_rel_addr field is at entry0 + 12; blob is at entry0 + 20 → rel = 8.
    w.write_i32(8);
    w.write_u32(0); // offset_in_blob = 0 (parent starts at blob[0])

    // Write the shared blob (parent bytecode followed by child bytecode).
    w.write_bytes(parent_bc);
    w.write_bytes(child_bc);

    // Entry 1 (child): starts after the blob.
    let entry1_pos = w.position();
    let abs1 = (code_data_abs + entry1_pos) as u32;
    w.patch_u32(ptr_base + 4, abs1);

    w.write_u32(child_name_ref);
    w.write_u32(total_blob_len); // same blob_length
    w.write_u16(0); // locals_count
    w.write_u16(0); // args_count
    // bc_rel_addr field is at entry1 + 12; it must point back to blob_abs.
    let bc_rel_addr_abs = code_data_abs + entry1_pos + 12;
    let rel1 = blob_abs as i64 - bc_rel_addr_abs as i64;
    w.write_i32(rel1 as i32);
    w.write_u32(parent_len); // offset_in_blob = parent_len (child starts after parent)

    w.into_bytes()
}

/// Build a BC < 15 CODE chunk (simple format: name + length + inline bytecode).
fn build_code_v14(entries: &[(u32, &[u8])], code_data_abs: usize) -> Vec<u8> {
    let n = entries.len();
    let mut w = Writer::new();

    w.write_u32(n as u32);
    let ptr_base = w.position();
    for _ in 0..n {
        w.write_u32(0);
    }

    for (i, (name_ref, bytecode)) in entries.iter().enumerate() {
        let abs = (code_data_abs + w.position()) as u32;
        w.patch_u32(ptr_base + i * 4, abs);
        w.write_u32(*name_ref); // name
        w.write_u32(bytecode.len() as u32); // length
        w.write_bytes(bytecode); // inline bytecode (bc_abs = ptr + 8)
    }

    w.into_bytes()
}

// ── VARI chunk ───────────────────────────────────────────────────────────────

/// Build a BC < 15 VARI chunk (no header, 12-byte entries).
fn build_vari_v14(entries: &[(u32, u32, i32)]) -> Vec<u8> {
    let mut w = Writer::new();
    for (name, occ, fa) in entries {
        w.write_u32(*name);
        w.write_u32(*occ);
        w.write_i32(*fa);
    }
    w.into_bytes()
}

/// Build a BC >= 15 VARI chunk (3-field header + 20-byte entries).
fn build_vari_v15(
    instance_var_count: u32,
    instance_var_count_max: u32,
    max_local_var_count: u32,
    entries: &[(u32, i32, i32, u32, i32)], // (name, inst_type, var_id, occ, fa)
) -> Vec<u8> {
    let mut w = Writer::new();
    w.write_u32(instance_var_count);
    w.write_u32(instance_var_count_max);
    w.write_u32(max_local_var_count);
    for (name, inst_type, var_id, occ, fa) in entries {
        w.write_u32(*name);
        w.write_i32(*inst_type);
        w.write_i32(*var_id);
        w.write_u32(*occ);
        w.write_i32(*fa);
    }
    w.into_bytes()
}

// ── FUNC chunk ───────────────────────────────────────────────────────────────

/// Build a BC < 15 FUNC chunk (flat list of 12-byte entries, no count prefix).
fn build_func_v14(functions: &[(u32, u32, i32)]) -> Vec<u8> {
    let mut w = Writer::new();
    for (name, occ, fa) in functions {
        w.write_u32(*name);
        w.write_u32(*occ);
        w.write_i32(*fa);
    }
    w.into_bytes()
}

/// Build a BC >= 15 FUNC chunk.
///
/// Format: func_count(u32) + functions + locals_count(u32) + code_locals.
///
/// `code_locals`: list of (entry_name_ref, locals: &[(index, var_name_ref)]).
fn build_func_v15(
    functions: &[(u32, u32, i32)],
    code_locals: &[(u32, &[(u32, u32)])],
) -> Vec<u8> {
    let mut w = Writer::new();

    w.write_u32(functions.len() as u32);
    for (name, occ, fa) in functions {
        w.write_u32(*name);
        w.write_u32(*occ);
        w.write_i32(*fa);
    }

    w.write_u32(code_locals.len() as u32);
    for (entry_name, locals) in code_locals {
        w.write_u32(locals.len() as u32); // var_count
        w.write_u32(*entry_name); // code entry name ref
        for (idx, var_name) in *locals {
            w.write_u32(*idx);
            w.write_u32(*var_name);
        }
    }

    w.into_bytes()
}

// ── SCPT chunk ───────────────────────────────────────────────────────────────

/// Build a SCPT chunk.
///
/// `entries`: (name_ref, code_id) pairs. The pointer list uses absolute
/// file offsets into the scpt data; entries follow the pointer list.
fn build_scpt(entries: &[(u32, u32)], scpt_data_abs: usize) -> Vec<u8> {
    let n = entries.len();
    let mut w = Writer::new();

    w.write_u32(n as u32);
    let ptr_base = w.position();
    for _ in 0..n {
        w.write_u32(0);
    }

    for (i, (name_ref, code_id)) in entries.iter().enumerate() {
        let abs = (scpt_data_abs + w.position()) as u32;
        w.patch_u32(ptr_base + i * 4, abs);
        w.write_u32(*name_ref);
        w.write_u32(*code_id);
    }

    w.into_bytes()
}

// ── Fixture 1: v15_minimal ───────────────────────────────────────────────────

pub fn build_v15_minimal() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["", "default", "test_game", "gml_Script_test"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        15,
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(2, strings, strg_data_abs),
        str_ref(2, strings, strg_data_abs),
    );

    let bytecode = encode_instructions(&[
        instr(0, Opcode::PushI, DataType::Int16, DataType::Double, Operand::Int16(42)),
        instr(4, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(3, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );
    let code_size = code_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v15_minimal",
  "description": "Minimal valid data.win: GEN8 + STRG (4 strings) + CODE (1 function, PushI Int16(42) + Ret)",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}}
  ],
  "gen8": {{
    "bytecode_version": 15,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_filename": "default",
    "_config": "default",
    "_name": "test_game",
    "_display_name": "test_game"
  }},
  "strg": {{
    "count": 4,
    "_strings": ["", "default", "test_game", "gml_Script_test"]
  }},
  "code": {{
    "count": 1,
    "entries": [
      {{
        "_name": "gml_Script_test",
        "locals_count": 0,
        "args_count": 0,
        "_bc_byte_count": 8,
        "_instructions": [
          {{"opcode": "PushI", "type1": "Int16", "_value_i16": 42}},
          {{"opcode": "Ret"}}
        ]
      }}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
    );

    (bin, json)
}

// ── Fixture 2: v15_bytecode_variety ──────────────────────────────────────────

pub fn build_v15_bytecode_variety() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["hello", "variety_game", "gml_Script_variety"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        15,
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    let bytecode = encode_instructions(&[
        instr(0, Opcode::Push, DataType::Double, DataType::Double, Operand::Double(1.5_f64)),
        instr(12, Opcode::Push, DataType::Int32, DataType::Double, Operand::Int32(100)),
        instr(20, Opcode::PushI, DataType::Int16, DataType::Double, Operand::Int16(-1)),
        instr(24, Opcode::Push, DataType::String, DataType::Double, Operand::StringIndex(0)),
        instr(
            32,
            Opcode::Push,
            DataType::Variable,
            DataType::Double,
            Operand::Variable {
                var_ref: VariableRef { variable_id: 0, ref_type: 0 },
                instance: -1,
            },
        ),
        instr(
            40,
            Opcode::Cmp,
            DataType::Double,
            DataType::Double,
            Operand::Comparison(ComparisonKind::Less),
        ),
        instr(44, Opcode::Bf, DataType::Double, DataType::Double, Operand::Branch(8)),
        instr(48, Opcode::Dup, DataType::Variable, DataType::Double, Operand::Dup(0)),
        instr(52, Opcode::Call, DataType::Int32, DataType::Double, Operand::Call { function_id: 7, argc: 0 }),
        instr(60, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(2, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );
    let code_size = code_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v15_bytecode_variety",
  "description": "Diverse instruction mix: Double, Int32, Int16 (PushI), String, Variable, Cmp, Bf, Dup, Call, Ret",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}}
  ],
  "gen8": {{
    "bytecode_version": 15,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_name": "variety_game"
  }},
  "strg": {{
    "count": 3,
    "_strings": ["hello", "variety_game", "gml_Script_variety"]
  }},
  "code": {{
    "count": 1,
    "entries": [
      {{
        "_name": "gml_Script_variety",
        "locals_count": 0,
        "args_count": 0,
        "_bc_byte_count": 64,
        "_instructions": [
          {{"opcode": "Push",  "type1": "Double",   "_value_f64": 1.5}},
          {{"opcode": "Push",  "type1": "Int32",    "_value_i32": 100}},
          {{"opcode": "PushI", "type1": "Int16",    "_value_i16": -1}},
          {{"opcode": "Push",  "type1": "String",   "_value_string_idx": 0}},
          {{"opcode": "Push",  "type1": "Variable", "_instance": -1, "_variable_id": 0}},
          {{"opcode": "Cmp",   "type1": "Double",   "_cmp": "Less"}},
          {{"opcode": "Bf",    "_branch_bytes": 8}},
          {{"opcode": "Dup",   "type1": "Variable", "_dup_val": 0}},
          {{"opcode": "Call",  "_function_id": 7, "_argc": 0}},
          {{"opcode": "Ret"}}
        ]
      }}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
    );

    (bin, json)
}

// ── Fixture 3: v15_break_signals ─────────────────────────────────────────────

pub fn build_v15_break_signals() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["", "break_signals_game", "gml_Script_signals"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        15,
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    let bytecode = encode_instructions(&[
        instr(0, Opcode::Break, DataType::Double, DataType::Double, Operand::Break { signal: 0xFFF6, extra: None }),
        instr(4, Opcode::Break, DataType::Double, DataType::Double, Operand::Break { signal: 0xFFFA, extra: None }),
        instr(8, Opcode::Break, DataType::Int32, DataType::Double, Operand::Break { signal: 0xFFF5, extra: Some(5) }),
        instr(16, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(2, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );
    let code_size = code_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v15_break_signals",
  "description": "GMS2.3+ Break signals: chknullish (0xFFF6), isstaticok (0xFFFA), pushref (0xFFF5, type1=Int32, extra=5)",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}}
  ],
  "gen8": {{
    "bytecode_version": 15,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_name": "break_signals_game"
  }},
  "strg": {{
    "count": 3,
    "_strings": ["", "break_signals_game", "gml_Script_signals"]
  }},
  "code": {{
    "count": 1,
    "entries": [
      {{
        "_name": "gml_Script_signals",
        "locals_count": 0,
        "args_count": 0,
        "_bc_byte_count": 20,
        "_instructions": [
          {{"opcode": "Break", "type1": "Double", "_signal": "0xFFF6", "_extra": null}},
          {{"opcode": "Break", "type1": "Double", "_signal": "0xFFFA", "_extra": null}},
          {{"opcode": "Break", "type1": "Int32",  "_signal": "0xFFF5", "_extra": 5}},
          {{"opcode": "Ret"}}
        ]
      }}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
    );

    (bin, json)
}

// ── Fixture 4: v14_minimal ───────────────────────────────────────────────────

/// BC = 14 fixture: exercises v14-specific CODE / VARI / FUNC parse paths.
///
/// CODE v14: simple format — no `locals_count`/`args_count`/`bc_rel_addr`;
///           `bytecode_offset = ptr + 8` (after name+length).
/// VARI v14: no 3-field header; 12-byte entries (name+occurrences+first_addr).
/// FUNC v14: flat list, no count prefix; 12-byte entries.
///
/// The bytecode bytes use the v14 `Ret` opcode (0x9D). The Rust decoder only
/// understands v15 opcodes, so bytecode decode is NOT tested for v14 — only
/// chunk structure.
pub fn build_v14_minimal() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["", "v14_game", "gml_Script_init", "x", "y"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        14,
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    // v14 bytecode: just a raw Ret (opcode 0x9D in v14 encoding).
    // We do NOT run encode_instructions here since the decoder uses v15 opcodes.
    let bc_v14_ret: &[u8] = &[0x9D, 0x00, 0x00, 0x00];

    let code_data = build_code_v14(
        &[(str_ref(2, strings, strg_data_abs), bc_v14_ret)],
        code_data_abs,
    );
    let code_size = code_data.len();
    let vari_data_abs = code_data_abs + code_size + CHUNK_HDR;

    // Two v14 VARI entries: "x" (occ=2, fa=-1) and "y" (occ=1, fa=-1).
    let vari_data = build_vari_v14(&[
        (str_ref(3, strings, strg_data_abs), 2, -1),
        (str_ref(4, strings, strg_data_abs), 1, -1),
    ]);
    let vari_size = vari_data.len();
    let _func_data_abs = vari_data_abs + vari_size + CHUNK_HDR;

    // One v14 FUNC entry: "gml_Script_init" (occ=1, fa=-1).
    let func_data = build_func_v14(&[(str_ref(2, strings, strg_data_abs), 1, -1)]);
    let func_size = func_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
        OutputChunk { magic: *b"VARI", data: vari_data },
        OutputChunk { magic: *b"FUNC", data: func_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v14_minimal",
  "description": "BC=14: v14 CODE (simple 8-byte header), VARI (no header, 12-byte entries), FUNC (flat list, no count)",
  "_note": "Bytecode uses v14 opcode 0x9D (Ret); not decodable by the v15 decoder.",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}},
    {{"magic": "VARI", "data_size": {vari_size}}},
    {{"magic": "FUNC", "data_size": {func_size}}}
  ],
  "gen8": {{
    "bytecode_version": 14,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_name": "v14_game"
  }},
  "strg": {{
    "count": 5,
    "_strings": ["", "v14_game", "gml_Script_init", "x", "y"]
  }},
  "code": {{
    "count": 1,
    "_format": "v14: bytecode_offset = ptr + 8 (no extended header)",
    "entries": [
      {{
        "_name": "gml_Script_init",
        "_bc_byte_count": 4,
        "_raw_bc_hex": "9d000000"
      }}
    ]
  }},
  "vari": {{
    "_format": "v14: no header, 12-byte entries",
    "count": 2,
    "entries": [
      {{"_name": "x", "occurrences": 2, "first_address": -1}},
      {{"_name": "y", "occurrences": 1, "first_address": -1}}
    ]
  }},
  "func": {{
    "_format": "v14: flat list (no count prefix), 12-byte entries",
    "count": 1,
    "functions": [
      {{"_name": "gml_Script_init", "occurrences": 1, "first_address": -1}}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
        vari_size = vari_size,
        func_size = func_size,
    );

    (bin, json)
}

// ── Fixture 5: v15_vari_func ─────────────────────────────────────────────────

/// Exercises BC >= 15 VARI (3-field header + 20-byte entries) and FUNC
/// (count-prefixed functions + CodeLocals section).
pub fn build_v15_vari_func() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    // strings[0]="" [1]="vf_game" [2]="gml_Script_vf" [3]="x" [4]="my_func" [5]="i"
    let strings: &[&str] = &["", "vf_game", "gml_Script_vf", "x", "my_func", "i"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        15,
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    // CODE: 1 entry with locals_count=1.
    let bytecode = encode_instructions(&[
        instr(0, Opcode::PushI, DataType::Int16, DataType::Double, Operand::Int16(0)),
        instr(4, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);
    let code_data = build_code_v15_with_meta(
        str_ref(2, strings, strg_data_abs),
        &bytecode,
        1, // locals_count
        0, // args_count
        code_data_abs,
    );
    let code_size = code_data.len();
    let _vari_data_abs = code_data_abs + code_size + CHUNK_HDR;

    // VARI: header + 1 variable "x" (instance_type=-1/Own, var_id=0, occ=1, fa=-1).
    let vari_data = build_vari_v15(
        1, // instance_var_count
        1, // instance_var_count_max
        1, // max_local_var_count
        &[(str_ref(3, strings, strg_data_abs), -1, 0, 1, -1)],
    );
    let vari_size = vari_data.len();

    // FUNC: 1 function "my_func" + 1 code_locals entry for "gml_Script_vf"
    //       with 1 local "i" at index 0.
    let code_locals_entry: &[(u32, u32)] =
        &[(0, str_ref(5, strings, strg_data_abs))]; // index=0, name="i"
    let func_data = build_func_v15(
        &[(str_ref(4, strings, strg_data_abs), 1, -1)],
        &[(str_ref(2, strings, strg_data_abs), code_locals_entry)],
    );
    let func_size = func_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
        OutputChunk { magic: *b"VARI", data: vari_data },
        OutputChunk { magic: *b"FUNC", data: func_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v15_vari_func",
  "description": "BC=15 VARI (3-field header + 20-byte entries) and FUNC (count + functions + CodeLocals)",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}},
    {{"magic": "VARI", "data_size": {vari_size}}},
    {{"magic": "FUNC", "data_size": {func_size}}}
  ],
  "gen8": {{
    "bytecode_version": 15,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_name": "vf_game"
  }},
  "strg": {{
    "count": 6,
    "_strings": ["", "vf_game", "gml_Script_vf", "x", "my_func", "i"]
  }},
  "code": {{
    "count": 1,
    "entries": [
      {{
        "_name": "gml_Script_vf",
        "locals_count": 1,
        "args_count": 0,
        "_bc_byte_count": 8
      }}
    ]
  }},
  "vari": {{
    "_format": "v15: 12-byte header + 20-byte entries",
    "instance_var_count": 1,
    "instance_var_count_max": 1,
    "max_local_var_count": 1,
    "count": 1,
    "entries": [
      {{
        "_name": "x",
        "instance_type": -1,
        "var_id": 0,
        "occurrences": 1,
        "first_address": -1
      }}
    ]
  }},
  "func": {{
    "func_count": 1,
    "functions": [
      {{"_name": "my_func", "occurrences": 1, "first_address": -1}}
    ],
    "code_locals_count": 1,
    "code_locals": [
      {{
        "_entry_name": "gml_Script_vf",
        "var_count": 1,
        "locals": [{{"index": 0, "_name": "i"}}]
      }}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
        vari_size = vari_size,
        func_size = func_size,
    );

    (bin, json)
}

// ── Fixture 6: v15_more_opcodes ───────────────────────────────────────────────

/// Exercises opcode variants not covered by the first three fixtures:
///   - Push.Float, Push.Int64, Push.Bool  (additional push types)
///   - PushLoc, PushGlb, PushBltn         (scope-qualified push variants)
///   - Pop.Variable                        (variable write)
///   - Backward Bf branch                  (negative 23-bit signed offset)
pub fn build_v15_more_opcodes() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["", "more_game", "gml_Script_more"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        15,
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    // Instruction layout (all byte offsets from function start):
    //  0: Push.Float  2.5                   ( 8 bytes)
    //  8: Push.Int64  999                   (12 bytes)
    // 20: Push.Bool   true                  ( 8 bytes)
    // 28: PushLoc.Variable  Own[var=0]      ( 8 bytes)
    // 36: PushGlb.Variable  Global[var=0]   ( 8 bytes)
    // 44: PushBltn.Variable Builtin[var=0]  ( 8 bytes)
    // 52: Pop.Variable      Own[var=1]      ( 8 bytes)
    // 60: Bf  -60  (backward branch to offset 0)  ( 4 bytes)
    // 64: Ret                               ( 4 bytes)
    // Total: 68 bytes

    let bytecode = encode_instructions(&[
        instr(0, Opcode::Push, DataType::Float, DataType::Double, Operand::Float(2.5_f32)),
        instr(8, Opcode::Push, DataType::Int64, DataType::Double, Operand::Int64(999)),
        instr(20, Opcode::Push, DataType::Bool, DataType::Double, Operand::Bool(true)),
        instr(
            28,
            Opcode::PushLoc,
            DataType::Variable,
            DataType::Double,
            Operand::Variable {
                var_ref: VariableRef { variable_id: 0, ref_type: 0 },
                instance: -1, // Own
            },
        ),
        instr(
            36,
            Opcode::PushGlb,
            DataType::Variable,
            DataType::Double,
            Operand::Variable {
                var_ref: VariableRef { variable_id: 0, ref_type: 0 },
                instance: -5, // Global
            },
        ),
        instr(
            44,
            Opcode::PushBltn,
            DataType::Variable,
            DataType::Double,
            Operand::Variable {
                var_ref: VariableRef { variable_id: 0, ref_type: 0 },
                instance: -6, // Builtin
            },
        ),
        instr(
            52,
            Opcode::Pop,
            DataType::Variable,
            DataType::Double,
            Operand::Variable {
                var_ref: VariableRef { variable_id: 1, ref_type: 0 },
                instance: -1, // Own
            },
        ),
        // Backward branch: Bf at offset 60 jumping to offset 0 = byte_offset -60.
        // -60 / 4 = -15 words; encoded as 23-bit two's complement → type1=Int16, type2=Raw(7).
        backward_branch_instr(60, Opcode::Bf, -60),
        instr(64, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(2, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );
    let code_size = code_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v15_more_opcodes",
  "description": "Push.Float, Push.Int64, Push.Bool, PushLoc/Glb/Bltn (Variable), Pop.Variable, backward Bf branch",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}}
  ],
  "gen8": {{
    "bytecode_version": 15,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_name": "more_game"
  }},
  "strg": {{
    "count": 3,
    "_strings": ["", "more_game", "gml_Script_more"]
  }},
  "code": {{
    "count": 1,
    "entries": [
      {{
        "_name": "gml_Script_more",
        "locals_count": 0,
        "args_count": 0,
        "_bc_byte_count": 68,
        "_instructions": [
          {{"opcode": "Push",    "type1": "Float",    "_value_f32": 2.5}},
          {{"opcode": "Push",    "type1": "Int64",    "_value_i64": 999}},
          {{"opcode": "Push",    "type1": "Bool",     "_value_bool": true}},
          {{"opcode": "PushLoc", "type1": "Variable", "_instance": -1, "_variable_id": 0}},
          {{"opcode": "PushGlb", "type1": "Variable", "_instance": -5, "_variable_id": 0}},
          {{"opcode": "PushBltn","type1": "Variable", "_instance": -6, "_variable_id": 0}},
          {{"opcode": "Pop",     "type1": "Variable", "_instance": -1, "_variable_id": 1}},
          {{"opcode": "Bf", "_branch_bytes": -60, "_note": "backward branch; type1=Int16, type2=Raw(7) carry upper offset bits"}},
          {{"opcode": "Ret"}}
        ]
      }}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
    );

    (bin, json)
}

// ── Fixture 7: v15_scpt ───────────────────────────────────────────────────────

/// Exercises the SCPT chunk parser.
///
/// SCPT uses a pointer list (absolute file offsets); each entry contains
/// a script name StringRef and a zero-based CODE chunk index.
pub fn build_v15_scpt() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    // strings[0]="" [1]="scpt_game" [2]="gml_Script_foo" [3]="Script_foo"
    let strings: &[&str] = &["", "scpt_game", "gml_Script_foo", "Script_foo"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        15,
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    let bytecode = encode_instructions(&[
        instr(0, Opcode::PushI, DataType::Int16, DataType::Double, Operand::Int16(0)),
        instr(4, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(2, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );
    let code_size = code_data.len();
    let scpt_data_abs = code_data_abs + code_size + CHUNK_HDR;

    // SCPT: 1 script "Script_foo" mapping to code entry 0.
    let scpt_data = build_scpt(
        &[(str_ref(3, strings, strg_data_abs), 0)],
        scpt_data_abs,
    );
    let scpt_size = scpt_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
        OutputChunk { magic: *b"SCPT", data: scpt_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v15_scpt",
  "description": "SCPT chunk: pointer list of (name_ref, code_id) pairs",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}},
    {{"magic": "SCPT", "data_size": {scpt_size}}}
  ],
  "gen8": {{
    "bytecode_version": 15,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_name": "scpt_game"
  }},
  "strg": {{
    "count": 4,
    "_strings": ["", "scpt_game", "gml_Script_foo", "Script_foo"]
  }},
  "code": {{
    "count": 1,
    "entries": [
      {{"_name": "gml_Script_foo", "locals_count": 0, "args_count": 0}}
    ]
  }},
  "scpt": {{
    "count": 1,
    "entries": [
      {{"_name": "Script_foo", "code_id": 0}}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
        scpt_size = scpt_size,
    );

    (bin, json)
}

// ── Fixture 8: v15_shared_blob ────────────────────────────────────────────────

/// GMS2.3+ shared blob: two CODE entries (parent + child) sharing one
/// bytecode blob at different `offset_in_blob` values.
///
/// Tests the two-pass gap-based length computation in `Code::parse`.
pub fn build_v15_shared_blob() -> (Vec<u8>, String) {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] =
        &["", "shared_game", "gml_Script_parent", "gml_Script_child"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8(
        15,
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    // Parent: PushI Int16(1) + Ret = 8 bytes.
    let parent_bc = encode_instructions(&[
        instr(0, Opcode::PushI, DataType::Int16, DataType::Double, Operand::Int16(1)),
        instr(4, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);
    // Child: Ret = 4 bytes.
    let child_bc = encode_instructions(&[instr(
        0,
        Opcode::Ret,
        DataType::Double,
        DataType::Double,
        Operand::None,
    )]);

    let parent_len = parent_bc.len();
    let child_len = child_bc.len();

    let code_data = build_code_v15_shared_blob(
        str_ref(2, strings, strg_data_abs),
        &parent_bc,
        str_ref(3, strings, strg_data_abs),
        &child_bc,
        code_data_abs,
    );
    let code_size = code_data.len();

    let bin = assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ]);

    let json = format!(
        r#"{{
  "_kaitai_limitations": {limitations},
  "fixture": "v15_shared_blob",
  "description": "GMS2.3+ shared blob: parent + child share one bytecode blob; two-pass gap algorithm splits lengths",
  "file_size": {file_size},
  "chunks": [
    {{"magic": "GEN8", "data_size": {gen8_size}}},
    {{"magic": "STRG", "data_size": {strg_size}}},
    {{"magic": "CODE", "data_size": {code_size}}}
  ],
  "gen8": {{
    "bytecode_version": 15,
    "is_debug_disabled": false,
    "game_id": 1,
    "major": 1,
    "minor": 0,
    "room_count": 0,
    "_name": "shared_game"
  }},
  "strg": {{
    "count": 4,
    "_strings": ["", "shared_game", "gml_Script_parent", "gml_Script_child"]
  }},
  "code": {{
    "count": 2,
    "_note": "Both entries have blob_length={total_blob} (total); actual per-entry lengths are computed by the two-pass gap algorithm.",
    "entries": [
      {{
        "_name": "gml_Script_parent",
        "locals_count": 0,
        "args_count": 0,
        "_offset_in_blob": 0,
        "_actual_bc_bytes": {parent_len},
        "_instructions": [
          {{"opcode": "PushI", "type1": "Int16", "_value_i16": 1}},
          {{"opcode": "Ret"}}
        ]
      }},
      {{
        "_name": "gml_Script_child",
        "locals_count": 0,
        "args_count": 0,
        "_offset_in_blob": {parent_len},
        "_actual_bc_bytes": {child_len},
        "_instructions": [
          {{"opcode": "Ret"}}
        ]
      }}
    ]
  }}
}}"#,
        limitations = KAITAI_LIMITATIONS,
        file_size = bin.len(),
        gen8_size = GEN8_SIZE,
        strg_size = strg_size,
        code_size = code_size,
        total_blob = parent_len + child_len,
        parent_len = parent_len,
        child_len = child_len,
    );

    (bin, json)
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn instr(
    offset: usize,
    opcode: Opcode,
    type1: DataType,
    type2: DataType,
    operand: Operand,
) -> Instruction {
    Instruction { offset, opcode, type1, type2, operand }
}

/// Build a branch instruction with the correct type1/type2 nibbles for the
/// given signed byte offset. For backward (negative) branches the upper bits
/// of the 23-bit two's-complement offset overflow into the type nibbles.
fn backward_branch_instr(offset: usize, opcode: Opcode, byte_offset: i32) -> Instruction {
    let offset_words = byte_offset / 4;
    let raw23 = (offset_words as u32) & 0x007F_FFFF;
    let type1 = DataType::from_u8(((raw23 >> 16) & 0xF) as u8);
    let type2 = DataType::from_u8(((raw23 >> 20) & 0xF) as u8);
    Instruction { offset, opcode, type1, type2, operand: Operand::Branch(byte_offset) }
}

fn encode_instructions(instructions: &[Instruction]) -> Vec<u8> {
    let encoded = encode::encode(instructions);
    let decoded = decode::decode(&encoded).expect("fixture bytecode failed to decode");
    assert_eq!(decoded.len(), instructions.len(), "round-trip instruction count mismatch");
    for (i, (orig, rt)) in instructions.iter().zip(decoded.iter()).enumerate() {
        assert_eq!(
            orig.opcode, rt.opcode,
            "instruction {i}: opcode mismatch ({:?} vs {:?})",
            orig.opcode, rt.opcode
        );
    }
    encoded
}
