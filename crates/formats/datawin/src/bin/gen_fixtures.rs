//! Fixture generator for datawin unit tests.
//!
//! Generates small synthetic `data.win`-format binary files into
//! `tests/fixtures/`. These are committed to the repo and served as:
//!
//!   1. **Regression tests** for the Rust parser (`tests/fixture_tests.rs`).
//!   2. **Reference inputs** for validating the Kaitai Struct specs:
//!      `crates/formats/datawin/game_maker_data.ksy` and
//!      `crates/formats/datawin/gml_bytecode.ksy`
//!
//! # Usage
//!
//! ```
//! cargo run -p datawin --bin gen_fixtures
//! ```
//!
//! # Kaitai validation workflow
//!
//! Install the Kaitai Struct compiler and Python runtime:
//!
//! ```sh
//! pip install kaitaistruct
//! # Download ksc from https://github.com/kaitai-io/kaitai_struct_compiler/releases
//! # (or: nix-env -i kaitai-struct-compiler)
//! ```
//!
//! Compile the specs and parse a fixture:
//!
//! ```sh
//! cd crates/formats/datawin
//! ksc -t python game_maker_data.ksy
//! python3 - <<'EOF'
//! import sys, os
//! sys.path.insert(0, '.')
//! from kaitaistruct import KaitaiStream, BytesIO
//! import game_maker_data
//! data = open('tests/fixtures/v15_minimal.bin', 'rb').read()
//! gmd = game_maker_data.GameMakerData(KaitaiStream(BytesIO(data)))
//! print("chunks:", [c.magic.decode() for c in gmd.chunks])
//! # navigate into gen8, strg, code as documented in the .ksy
//! EOF
//! ```
//!
//! Or use the interactive Kaitai Web IDE at <https://ide.kaitai.io/>.

use datawin::bytecode::decode::{Instruction, Operand};
use datawin::bytecode::opcode::Opcode;
use datawin::bytecode::types::{ComparisonKind, DataType, VariableRef};
use datawin::bytecode::{decode, encode};
use datawin::cursor::Writer;
use datawin::writer::{assemble_form, OutputChunk};

const FIXTURES_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");

fn main() -> std::io::Result<()> {
    std::fs::create_dir_all(FIXTURES_DIR)?;

    write("v15_minimal.bin", build_v15_minimal())?;
    write("v15_bytecode_variety.bin", build_v15_bytecode_variety())?;
    write("v15_break_signals.bin", build_v15_break_signals())?;

    Ok(())
}

fn write(name: &str, data: Vec<u8>) -> std::io::Result<()> {
    let path = format!("{FIXTURES_DIR}/{name}");
    std::fs::write(&path, &data)?;
    println!("wrote {name} ({} bytes)", data.len());
    Ok(())
}

// ── String pool ──────────────────────────────────────────────────────────────

/// Compute the STRG chunk data given the absolute offset of the chunk's data
/// start (i.e. the 8-byte header has already been accounted for).
///
/// Pointer list entries point to each string's `u32 len` prefix, as expected
/// by `StringTable::get`. `StringRef` values are `ptr + 4` (char data).
fn build_strg(strings: &[&str], chunk_data_abs: usize) -> Vec<u8> {
    let n = strings.len();
    let mut w = Writer::new();

    w.write_u32(n as u32);

    // Reserve space for the pointer list (will be backpatched below).
    let ptr_base = w.position(); // = 4
    for _ in 0..n {
        w.write_u32(0);
    }

    // Write string data and backpatch the corresponding pointer.
    for (i, s) in strings.iter().enumerate() {
        let abs = (chunk_data_abs + w.position()) as u32;
        w.patch_u32(ptr_base + i * 4, abs);
        w.write_gm_string(s);
    }

    w.into_bytes()
}

/// Compute the `StringRef` value (absolute offset of the **char** data) for
/// the string at `index` within `strings`, given the absolute offset of the
/// STRG chunk's data.
fn str_ref(index: usize, strings: &[&str], strg_data_abs: usize) -> u32 {
    // Layout: count(4) + ptrs(4*n) + string_0 + string_1 + ...
    let ptr_section = 4 + 4 * strings.len();
    let mut len_prefix_abs = strg_data_abs + ptr_section;
    for s in strings[..index].iter() {
        len_prefix_abs += 4 + s.len() + 1; // u32 len + chars + null
    }
    // StringRef.0 points to char data = len_prefix + 4.
    (len_prefix_abs + 4) as u32
}

// ── GEN8 chunk ───────────────────────────────────────────────────────────────

/// Build a minimal GEN8 chunk with bytecode version 15, `major=1`, no rooms.
///
/// Fixed size: 132 bytes.
fn build_gen8_v15(
    filename_ref: u32,
    config_ref: u32,
    name_ref: u32,
    display_name_ref: u32,
) -> Vec<u8> {
    let mut w = Writer::new();

    // Byte 0: is_debug_disabled; byte 1: bytecode_version; bytes 2-3: padding.
    w.write_u8(0); // is_debug_disabled = false
    w.write_u8(15); // bytecode_version = 15
    w.write_u16(0); // padding

    w.write_u32(filename_ref);
    w.write_u32(config_ref);
    w.write_u32(0); // last_obj
    w.write_u32(0); // last_tile
    w.write_u32(1); // game_id

    w.write_bytes(&[0u8; 16]); // guid (zeroed)

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

    w.write_u32(0); // room_count = 0 (no rooms)

    let data = w.into_bytes();
    assert_eq!(data.len(), 132, "GEN8 size mismatch");
    data
}

// ── CODE chunk ───────────────────────────────────────────────────────────────

/// Build a BC >= 15 CODE chunk with a sequence of non-shared entries.
///
/// Each entry has its bytecode blob immediately following the entry header,
/// so `bc_rel_addr = 8` (blob is 8 bytes past the `bc_rel_addr` field).
fn build_code_v15(entries: &[(u32, &[u8])], code_data_abs: usize) -> Vec<u8> {
    let n = entries.len();
    let mut w = Writer::new();

    w.write_u32(n as u32);

    let ptr_base = w.position(); // = 4
    for _ in 0..n {
        w.write_u32(0);
    }

    for (i, (name_ref, bytecode)) in entries.iter().enumerate() {
        // Backpatch: ptr[i] = absolute offset of this entry.
        let abs = (code_data_abs + w.position()) as u32;
        w.patch_u32(ptr_base + i * 4, abs);

        // Entry header (20 bytes).
        w.write_u32(*name_ref); // name
        w.write_u32(bytecode.len() as u32); // blob_length
        w.write_u16(0); // locals_count
        w.write_u16(0); // args_count
        // bc_rel_addr: relative to this field (offset +12 within entry).
        // Bytecode follows at entry_start+20, so rel = 20 - 12 = 8.
        w.write_i32(8);
        w.write_u32(0); // offset_in_blob

        // Bytecode blob.
        w.write_bytes(bytecode);
    }

    w.into_bytes()
}

// ── Fixture 1: v15_minimal ───────────────────────────────────────────────────

/// Minimal valid `data.win`: GEN8 + STRG (4 strings) + CODE (1 function).
///
/// Bytecode: `PushI Int16(42)` then `Ret`.
///
/// Validates:
/// - FORM envelope + chunk index parsing
/// - GEN8 field layout (bytecode version, StringRef resolution)
/// - STRG pointer list and string resolution
/// - CODE entry format (bc ≥ 15)
/// - Basic bytecode decoding
pub fn build_v15_minimal() -> Vec<u8> {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["", "default", "test_game", "gml_Script_test"];

    // Compute absolute offsets of each chunk's data (after the 8-byte header).
    let gen8_data_abs = FORM_HDR + CHUNK_HDR; // 16
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR; // 156

    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();

    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8_v15(
        str_ref(1, strings, strg_data_abs), // filename = "default"
        str_ref(1, strings, strg_data_abs), // config = "default"
        str_ref(2, strings, strg_data_abs), // name = "test_game"
        str_ref(2, strings, strg_data_abs), // display_name = "test_game"
    );

    let bytecode = encode_instructions(&[
        instr(0, Opcode::PushI, DataType::Int16, DataType::Double, Operand::Int16(42)),
        instr(4, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(3, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );

    assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ])
}

// ── Fixture 2: v15_bytecode_variety ──────────────────────────────────────────

/// Diverse instruction mix exercising all operand formats.
///
/// Instruction sequence (offsets are byte offsets within the function):
///
/// ```text
///  0: Push.d  3.14            (push f64, 12 bytes)
/// 12: Push.i  100             (push i32, 8 bytes)
/// 20: PushI.e -1              (push i16 inline, 4 bytes)
/// 24: Push.s  0               (push string index, 8 bytes)
/// 32: Push.v  Own[0]          (push variable, 8 bytes)
/// 40: Cmp.dd  Less            (compare, 4 bytes)
/// 44: Bf      +8              (branch-false, skip 1 word, 4 bytes)
/// 48: Dup.v   0               (dup, 4 bytes)
/// 52: Call    func=7, argc=0  (call, 8 bytes)
/// 60: Ret                     (return, 4 bytes)
/// ```
///
/// Validates:
/// - Extended push operands (Double, Int32, String, Variable)
/// - Branch offset encoding (23-bit signed, units of 4 bytes)
/// - Comparison encoding
/// - Call operand (function_id word)
/// - Dup encoding
pub fn build_v15_bytecode_variety() -> Vec<u8> {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["hello", "variety_game", "gml_Script_variety"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8_v15(
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    let bytecode = encode_instructions(&[
        // Push f64 3.14 (12 bytes: 4 header + 8 f64)
        instr(0, Opcode::Push, DataType::Double, DataType::Double, Operand::Double(1.5_f64)),
        // Push i32 100 (8 bytes: 4 header + 4 i32)
        instr(12, Opcode::Push, DataType::Int32, DataType::Double, Operand::Int32(100)),
        // PushI i16 -1 inline (4 bytes)
        instr(20, Opcode::PushI, DataType::Int16, DataType::Double, Operand::Int16(-1)),
        // Push String index 0 (8 bytes: 4 header + 4 string index)
        instr(24, Opcode::Push, DataType::String, DataType::Double, Operand::StringIndex(0)),
        // Push Variable Own[var=0, ref_type=0] (8 bytes: 4 header + 4 var ref)
        instr(
            32,
            Opcode::Push,
            DataType::Variable,
            DataType::Double,
            Operand::Variable {
                var_ref: VariableRef { variable_id: 0, ref_type: 0 },
                instance: -1, // Own
            },
        ),
        // Cmp.dd Less (4 bytes)
        instr(
            40,
            Opcode::Cmp,
            DataType::Double,
            DataType::Double,
            Operand::Comparison(ComparisonKind::Less),
        ),
        // Bf +8 bytes (skip 1 instruction, 4 bytes)
        instr(44, Opcode::Bf, DataType::Double, DataType::Double, Operand::Branch(8)),
        // Dup.Variable(0) (4 bytes)
        instr(48, Opcode::Dup, DataType::Variable, DataType::Double, Operand::Dup(0)),
        // Call func=7, argc=0 (8 bytes: 4 header + 4 function_id)
        instr(52, Opcode::Call, DataType::Int32, DataType::Double, Operand::Call { function_id: 7, argc: 0 }),
        // Ret (4 bytes)
        instr(60, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(2, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );

    assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ])
}

// ── Fixture 3: v15_break_signals ─────────────────────────────────────────────

/// GMS2.3+ `Break` signal instructions.
///
/// Instruction sequence:
///
/// ```text
///  0: Break  signal=-10                         (chknullish, 4 bytes)
///  4: Break  signal=-6                          (isstaticok, 4 bytes)
///  8: Break  signal=-11, extra=(tag=0, idx=5)   (pushref, 8 bytes: type1=Int32)
/// 16: Ret                                        (4 bytes)
/// ```
///
/// Signal encoding (i16 reinterpreted as u16):
///   -10 → 0xFFF6 (chknullish)
///   -6  → 0xFFFA (isstaticok)
///   -11 → 0xFFF5 (pushref, requires `type1 = Int32` + extra i32 word)
///
/// Extra word for pushref: `(type_tag << 24) | asset_index`.
/// Here type_tag=0 (FUNC) and asset_index=5.
///
/// Validates: Break signal decoding, pushref extra-word handling.
pub fn build_v15_break_signals() -> Vec<u8> {
    const FORM_HDR: usize = 8;
    const CHUNK_HDR: usize = 8;
    const GEN8_SIZE: usize = 132;

    let strings: &[&str] = &["", "break_signals_game", "gml_Script_signals"];

    let gen8_data_abs = FORM_HDR + CHUNK_HDR;
    let strg_data_abs = gen8_data_abs + GEN8_SIZE + CHUNK_HDR;
    let strg_data = build_strg(strings, strg_data_abs);
    let strg_size = strg_data.len();
    let code_data_abs = strg_data_abs + strg_size + CHUNK_HDR;

    let gen8_data = build_gen8_v15(
        str_ref(0, strings, strg_data_abs),
        str_ref(0, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
        str_ref(1, strings, strg_data_abs),
    );

    let bytecode = encode_instructions(&[
        // Break chknullish: signal = 0xFFF6 (-10 as u16), no extra word.
        instr(0, Opcode::Break, DataType::Double, DataType::Double, Operand::Break { signal: 0xFFF6, extra: None }),
        // Break isstaticok: signal = 0xFFFA (-6 as u16), no extra word.
        instr(4, Opcode::Break, DataType::Double, DataType::Double, Operand::Break { signal: 0xFFFA, extra: None }),
        // Break pushref: signal = 0xFFF5 (-11 as u16), type1=Int32, extra = (tag=0, idx=5).
        instr(8, Opcode::Break, DataType::Int32, DataType::Double, Operand::Break { signal: 0xFFF5, extra: Some(5) }),
        // Ret.
        instr(16, Opcode::Ret, DataType::Double, DataType::Double, Operand::None),
    ]);

    let code_data = build_code_v15(
        &[(str_ref(2, strings, strg_data_abs), &bytecode)],
        code_data_abs,
    );

    assemble_form(&[
        OutputChunk { magic: *b"GEN8", data: gen8_data },
        OutputChunk { magic: *b"STRG", data: strg_data },
        OutputChunk { magic: *b"CODE", data: code_data },
    ])
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

fn encode_instructions(instructions: &[Instruction]) -> Vec<u8> {
    let encoded = encode::encode(instructions);
    // Verify round-trip: decoding must produce structurally identical instructions.
    let decoded = decode::decode(&encoded).expect("fixture bytecode failed to decode");
    assert_eq!(
        decoded.len(),
        instructions.len(),
        "round-trip instruction count mismatch"
    );
    for (i, (orig, rt)) in instructions.iter().zip(decoded.iter()).enumerate() {
        assert_eq!(
            orig.opcode, rt.opcode,
            "instruction {i}: opcode mismatch ({:?} vs {:?})",
            orig.opcode, rt.opcode
        );
    }
    encoded
}
