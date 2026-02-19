# Inform (Z-machine / Glulx)

**Status: Planned** — No implementation started.

## Background

Inform is the dominant compiler for Interactive Fiction. Two major versions:
- **Inform 6** — C-like source language, been around since 1993
- **Inform 7** — natural language source ("The ball is on the table.")

Both compile to one of two VMs:
- **Z-machine** — classic 8-bit-era IF VM with 6 standard versions (z5 most common, z8 for large games)
- **Glulx** — 32-bit extended VM that removes Z-machine's size and address-space limits

The VM specifications are exhaustively documented and stable. Many open-source interpreters exist.

## Formats

### Z-machine (`.z3`, `.z5`, `.z8`, `.zblorb`)

- Fixed-size header with version, entry point, object table address, dictionary address
- Static game data (object tree, property tables, dictionary) in low memory
- Dynamic game data (object attributes, variables) in high memory
- Z-machine bytecode in high memory
- Optional Blorb wrapper (`.zblorb`) adds audio, graphics, metadata

Z-machine v5/v8 add:
- `@aread` / `@sread` — line input with optional timed callbacks
- `@print_unicode` / `@check_unicode` — Unicode support
- Extended instruction set (`@je`, `@jl`, etc. in z5+)

### Glulx (`.ulx`, `.gblorb`)

- 32-bit addressing throughout (removes Z-machine's 64KB/128KB/256KB limits)
- Glk I/O library for window management, file I/O, and sound
- Stack-based VM with register-indirect addressing
- Optional Blorb wrapper (`.gblorb`) for multimedia

Glulx is documented in the [Glulx specification](https://eblong.com/zarf/glulx/glulx-spec.html) by Andrew Plotkin.

## Lifting Strategy

Full recompilation (Tier 2). The goal is emitted TypeScript (or Rust) per routine — not bundling an interpreter.

The Z-machine and Glulx are well-specified VMs with manageable opcode sets. Lifting means:
1. Parse the story file header to identify VM type, version, and entry points
2. Decode bytecode in reachable routines (Z-machine: variable-length instructions with 1-4 operands; Glulx: similar)
3. Recover the object tree and property tables (Z-machine stores the world model as a tree structure with 31/255 attributes and properties per object)
4. Emit IR per routine
5. Replace Glk I/O calls with web UI equivalents

### Z-machine Specifics

Z-machine programs use a global variable array (240 globals), a local variable stack per routine (up to 15 locals), and a call stack. The object tree (up to 255 objects in v1-3, 65535 in v5+) stores the game world. Objects have attributes (bitmask), properties (variable-length), and a parent/sibling/child tree.

Key opcodes to model:
- `call` / `call_1n` / `call_2n` — routine calls with 0-3 args
- `storew` / `storeb` / `loadw` / `loadb` — word/byte memory access
- `put_prop` / `get_prop` / `get_prop_addr` / `get_next_prop` — object property access
- `set_attr` / `clear_attr` / `test_attr` — object attribute manipulation
- `insert_obj` / `remove_obj` / `move` — object tree manipulation
- `print` / `print_addr` / `print_ret` / `print_num` / `print_char` — output
- `read` / `aread` — line input
- `save` / `restore` / `save_undo` / `restore_undo` — save state

### Glulx Specifics

Glulx uses a simpler, more regular instruction set. The main complexity is Glk — the I/O abstraction layer. Glk provides windows (text, graphics, pair), streams, and file references. The web implementation maps:
- Text window → `<div>` with scrollable text
- Status window → fixed-height header
- Graphics window → `<canvas>`
- File I/O → localStorage or IndexedDB

## What Needs Building

### Format Parser (new crate: `reincarnate-frontend-inform`)

- [ ] Z-machine header parser (version detection, address table locations)
- [ ] Z-machine bytecode decoder (full opcode table for v5 and v8)
- [ ] Object tree extractor (objects, attributes, properties, parent/sibling/child)
- [ ] Dictionary extractor (word → Z-machine word code)
- [ ] Glulx header parser and bytecode decoder
- [ ] Glk call identification
- [ ] Blorb resource extractor (`.zblorb` / `.gblorb` — Blorb is an IFF-like container)

### IR Mapping

- Z-machine routine → IR function
- Local variables → IR function parameters + temporaries
- Global variables → module-level state
- Object tree → structured data (options: static JS object map, or dynamic object with attribute/property arrays)
- `print` calls → `SystemCall("Inform.Print", text)`
- `read` / `aread` → `SystemCall("Inform.ReadLine")` + `Yield` (coroutine)
- `save_undo` / `restore_undo` → `SystemCall("Inform.SaveUndo")` / `SystemCall("Inform.RestoreUndo")`

### Replacement Runtime (`runtime/inform/ts/`)

- [ ] Glk window system: text buffer, text grid, graphics, pair windows
- [ ] Text output with style attributes (bold, italic, fixed-width, etc.)
- [ ] Line input with command history and timeout
- [ ] Character input
- [ ] Save/restore (serialize interpreter state)
- [ ] Undo stack
- [ ] Sound playback (Blorb sound resources)
- [ ] Image display (Blorb image resources)
- [ ] File I/O (Glk streams → localStorage / File API)
- [ ] Unicode support (Z-machine v5+ `@print_unicode`)
- [ ] Z-machine-specific: object tree operations (insert_obj, remove_obj, move), property get/set

## Known Challenges

- **Inform 7 "natural language"** — Inform 7 source is in English prose. The I7 compiler translates it to I6 (the older Inform 6 language) which then compiles to Z-machine/Glulx. The bytecode output is already I6-generated code, so decompiling to I6 is feasible. Recovering the original I7 source prose is not realistic (and not necessary for lifting).
- **Compressed text** — Z-machine uses Zscii encoding and a 3-character alphabet table compression scheme. Must be decoded to Unicode.
- **Complex object model** — The Z-machine object tree encodes the IF world model (rooms, things, actors). Some optimizations assume knowledge of which objects are rooms vs items vs NPCs — this information isn't directly in the bytecode.
- **Undo granularity** — Z-machine `@save_undo` / `@restore_undo` must save/restore the entire interpreter state. The approach (copy-on-write snapshots vs diff-based) affects memory and performance.

## Optionally: Lifting from Inform 7 Source

If the game's `.inform` project directory is available (not just the compiled story file), the I7 source can be used directly. This would require an Inform 7 → IR translation pipeline bypassing the VM entirely. Given the complexity of the I7 language (natural language grammar, extension mechanism), this is a separate, much larger project.

## Note: Existing Interpreters

[Parchment](https://github.com/curiousdannii/parchment) (Z-machine + Glulx via WASM) and [Quixe](https://github.com/erkyrath/quixe) (pure-JS Glulx) exist and work well for rapid deployment. But they ship an interpreter loop — the output is not emitted code, it's not analysable, and it can't target Rust. The reincarnate approach compiles routines to functions.

## References

- [Z-machine Standards Document](https://inform-fiction.org/zmachine/standards/z1point1/)
- [Glulx Specification](https://eblong.com/zarf/glulx/glulx-spec.html)
- [Glk API Specification](https://eblong.com/zarf/glk/glk-spec-075.html)
- [Blorb Specification](https://eblong.com/zarf/blorb/)
- [Inform 6 Designer's Manual](https://inform-fiction.org/manual/html/)
- [Inform 7 Documentation](https://ganelson.github.io/inform-website/)
- [Frotz (Z-machine interpreter)](https://github.com/DavidGriffith/frotz)
- [Glulxe (Glulx interpreter)](https://github.com/erkyrath/glulxe)
