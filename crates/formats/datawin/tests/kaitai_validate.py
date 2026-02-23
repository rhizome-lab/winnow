#!/usr/bin/env python3
"""Kaitai Struct fixture validator for the datawin crate.

Parses each `.bin` fixture with the Kaitai-compiled Python parser and validates
the structural fields that Kaitai CAN check against the paired `.json` expected-
value file.  Fields prefixed with `_` in the JSON are Rust-only (require full-
file context or imperative logic) and are SKIPPED here; those fields are
validated in `tests/fixture_tests.rs` instead.

Prerequisites
-------------
1.  Compile the .ksy specs to Python (run from `crates/formats/datawin/`):

        ksc -t python game_maker_data.ksy gml_bytecode.ksy

2.  Install the Python runtime:

        pip install kaitaistruct

3.  Run this script (from `crates/formats/datawin/`):

        python3 tests/kaitai_validate.py

The script exits non-zero if any assertion fails.
"""

import json
import os
import sys

FIXTURES_DIR = os.path.join(os.path.dirname(__file__), "fixtures")
KSY_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# ── Kaitai import ─────────────────────────────────────────────────────────────

def load_kaitai():
    """Import the kaitai-compiled game_maker_data module.

    ksc places the generated .py file in the current working directory.
    We look there first, then fall back to the ksy source directory.
    """
    for search_dir in [os.getcwd(), KSY_DIR]:
        if os.path.exists(os.path.join(search_dir, "game_maker_data.py")):
            sys.path.insert(0, search_dir)
            break
    else:
        print("ERROR: game_maker_data.py not found.")
        print("  Run: ksc -t python game_maker_data.ksy gml_bytecode.ksy")
        sys.exit(1)

    try:
        from kaitaistruct import KaitaiStream, BytesIO
        import game_maker_data as gmd_mod
        return gmd_mod, KaitaiStream, BytesIO
    except ImportError as e:
        print(f"ERROR: {e}")
        print("  Run: pip install kaitaistruct")
        sys.exit(1)


# ── Validation helpers ────────────────────────────────────────────────────────

PASS = "\033[32mPASS\033[0m"
FAIL = "\033[31mFAIL\033[0m"
SKIP = "\033[33mSKIP\033[0m"

failures = []


def check(label, actual, expected, skip=False):
    if skip:
        print(f"  {SKIP}  {label}")
        return
    if actual == expected:
        print(f"  {PASS}  {label}: {actual!r}")
    else:
        msg = f"{label}: expected {expected!r}, got {actual!r}"
        print(f"  {FAIL}  {msg}")
        failures.append(msg)


def check_chunks(gmd, expected_chunks):
    """Validate chunk count, magics, and data sizes."""
    actual_count = len(gmd.chunks)
    exp_count = len(expected_chunks)
    check("chunk count", actual_count, exp_count)

    for i, exp in enumerate(expected_chunks):
        if i >= actual_count:
            break
        chunk = gmd.chunks[i]
        magic = chunk.magic if isinstance(chunk.magic, str) else chunk.magic.decode("ascii")
        check(f"chunks[{i}].magic", magic, exp["magic"])
        check(f"chunks[{i}].data_size", chunk.size, exp["data_size"])


def check_gen8(gen8_kaitai, exp_gen8):
    """Validate GEN8 numeric fields (Kaitai can read all of these directly)."""
    check("gen8.bytecode_version", gen8_kaitai.bytecode_version, exp_gen8["bytecode_version"])
    check("gen8.is_debug_disabled", bool(gen8_kaitai.is_debug_disabled), exp_gen8["is_debug_disabled"])
    check("gen8.game_id", gen8_kaitai.game_id, exp_gen8["game_id"])
    # ksy uses ide_version_major/minor; JSON uses major/minor
    if "major" in exp_gen8:
        check("gen8.ide_version_major", gen8_kaitai.ide_version_major, exp_gen8["major"])
    if "minor" in exp_gen8:
        check("gen8.ide_version_minor", gen8_kaitai.ide_version_minor, exp_gen8["minor"])
    if "room_count" in exp_gen8:
        check("gen8.room_count", gen8_kaitai.room_count, exp_gen8["room_count"])


def check_strg(strg_kaitai, exp_strg):
    """Validate STRG string count."""
    # strg_kaitai.strings is a pointer_list; count is stored in .strings.count
    check("strg.count", strg_kaitai.strings.count, exp_strg["count"])
    # NOTE: string content resolution requires following the pointer to STRG char
    # data — this is listed in _kaitai_limitations.  The _strings field is
    # validated in Rust fixture tests instead.
    print(f"  {SKIP}  strg._strings (requires StringRef resolution — Kaitai limitation)")


def check_code(code_kaitai, exp_code):
    """Validate CODE chunk entry count.

    code_body.entries is a pointer_list (count + raw offsets).  The actual
    code_entry_v14 / code_entry_v15 structs are accessed via absolute seek —
    Kaitai stores only the count and raw offset array here, not parsed entries.
    Per-entry fields (locals_count, args_count, instructions) are validated in
    the Rust fixture tests instead.
    """
    check("code.count", code_kaitai.entries.count, exp_code["count"])
    # Per-entry validation requires following pointer_list offsets (not Kaitai-native):
    for i, exp_entry in enumerate(exp_code.get("entries", [])):
        prefix = f"code.entries[{i}]"
        if "locals_count" in exp_entry:
            print(f"  {SKIP}  {prefix}.locals_count (pointer-based entry — Kaitai limitation)")
        if "args_count" in exp_entry:
            print(f"  {SKIP}  {prefix}.args_count (pointer-based entry — Kaitai limitation)")
        if "_instructions" in exp_entry:
            print(f"  {SKIP}  {prefix}._instructions (push_body size:0 — Kaitai limitation)")


def check_vari(vari_kaitai, exp_vari):
    """Validate VARI chunk structural metadata where accessible."""
    # The VARI body is stored as a raw size-eos blob in game_maker_data.ksy
    # (Kaitai limitation: needs cross-chunk GEN8.bytecode_version to decode).
    print(f"  {SKIP}  vari fields (stored as raw blob — Kaitai limitation)")


def check_func(func_kaitai, exp_func):
    """Validate FUNC chunk structural metadata where accessible."""
    print(f"  {SKIP}  func fields (stored as raw blob — Kaitai limitation)")


def check_scpt(scpt_kaitai, exp_scpt):
    """Validate SCPT chunk entry count.

    scpt_body.scripts is a pointer_list; actual script entries are accessed via
    absolute seek (same limitation as CODE entries).  We can only check count here.
    """
    check("scpt.count", scpt_kaitai.scripts.count, exp_scpt["count"])
    for i, exp_entry in enumerate(exp_scpt.get("entries", [])):
        prefix = f"scpt.entries[{i}]"
        if "code_id" in exp_entry:
            print(f"  {SKIP}  {prefix}.code_id (pointer-based entry — Kaitai limitation)")
        print(f"  {SKIP}  {prefix}._name (StringRef — Kaitai limitation)")


# ── Per-fixture validation ────────────────────────────────────────────────────

def validate_fixture(fixture_name, gmd_mod, KaitaiStream, BytesIO):
    bin_path = os.path.join(FIXTURES_DIR, f"{fixture_name}.bin")
    json_path = os.path.join(FIXTURES_DIR, f"{fixture_name}.json")

    if not os.path.exists(bin_path):
        print(f"SKIP  {fixture_name}: {bin_path} not found")
        return
    if not os.path.exists(json_path):
        print(f"SKIP  {fixture_name}: {json_path} not found")
        return

    with open(bin_path, "rb") as f:
        raw = f.read()
    with open(json_path) as f:
        exp = json.load(f)

    print(f"\n── {fixture_name} ({len(raw)} bytes) ──")

    # File size
    check("file_size", len(raw), exp["file_size"])

    # Parse with Kaitai
    try:
        gmd = gmd_mod.GameMakerData(KaitaiStream(BytesIO(raw)))
    except Exception as e:
        failures.append(f"{fixture_name}: Kaitai parse failed: {e}")
        print(f"  {FAIL}  Kaitai parse failed: {e}")
        return

    # Chunks
    if "chunks" in exp:
        check_chunks(gmd, exp["chunks"])

    # Find specific chunks by magic
    chunk_map = {(c.magic if isinstance(c.magic, str) else c.magic.decode("ascii")): c for c in gmd.chunks}

    if "gen8" in exp and "GEN8" in chunk_map:
        check_gen8(chunk_map["GEN8"].body, exp["gen8"])

    if "strg" in exp and "STRG" in chunk_map:
        check_strg(chunk_map["STRG"].body, exp["strg"])

    if "code" in exp and "CODE" in chunk_map:
        check_code(chunk_map["CODE"].body, exp["code"])

    if "vari" in exp and "VARI" in chunk_map:
        check_vari(chunk_map["VARI"].body, exp["vari"])

    if "func" in exp and "FUNC" in chunk_map:
        check_func(chunk_map["FUNC"].body, exp["func"])

    if "scpt" in exp and "SCPT" in chunk_map:
        check_scpt(chunk_map["SCPT"].body, exp["scpt"])


# ── Main ──────────────────────────────────────────────────────────────────────

FIXTURES = [
    "v15_minimal",
    "v15_bytecode_variety",
    "v15_break_signals",
    "v14_minimal",
    "v15_vari_func",
    "v15_more_opcodes",
    "v15_scpt",
    "v15_shared_blob",
]


def main():
    gmd_mod, KaitaiStream, BytesIO = load_kaitai()

    for name in FIXTURES:
        validate_fixture(name, gmd_mod, KaitaiStream, BytesIO)

    print()
    if failures:
        print(f"FAILED: {len(failures)} assertion(s) failed:")
        for f in failures:
            print(f"  - {f}")
        sys.exit(1)
    else:
        print(f"All checks passed ({len(FIXTURES)} fixtures).")


if __name__ == "__main__":
    main()
