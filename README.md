# Reincarnate

Legacy software lifting framework.

Part of the [rhi.zone](https://rhi.zone) ecosystem.

## Overview

Reincarnate extracts and transforms applications from obsolete runtimes into modern web-based equivalents. It targets bytecode and script-based runtimes — engines where user logic can be extracted and recompiled.

### Supported Targets

| Category | Targets | Format | Status |
|----------|---------|--------|--------|
| Interactive Media | Flash (AVM2) | ABC bytecode | ✅ Active |
| Interactive Media | Director/Shockwave | Lingo bytecode, RIFX | Planned |
| Enterprise | Visual Basic 6 | P-Code | Planned |
| Enterprise | Silverlight | .NET IL | Planned |
| Enterprise | Java Applets | JVM bytecode | Planned |
| No-Code Ancestors | HyperCard, ToolBook | Stack formats, HyperTalk | Planned |
| Game Engines | GameMaker (GMS1/GMS2) | GML bytecode | ✅ Active |
| Game Engines | RPG Maker VX Ace / MV / MZ | Ruby/RGSS, JSON events | Planned |
| Game Engines | Ren'Py | .rpyc AST pickle / .rpa archive | Planned |
| Game Engines | WolfRPG | Custom binary commands | Planned |
| Game Engines | SRPG Studio | JS + NW.js | Planned |
| Interactive Fiction | Twine SugarCube | HTML + macro DSL | ✅ Active |
| Interactive Fiction | Twine Harlowe | HTML + hook/macro syntax | ✅ Active |
| Interactive Fiction | Inform 7 | Z-machine / Glulx bytecode | Planned |
| Interactive Fiction | Ink by Inkle | .ink / compiled JSON | Planned |
| Interactive Fiction | RAGS | NRBF / SQL CE binary | Planned |
| Interactive Fiction | QSP | XOR-scrambled text/binary | Planned |
| Puzzle Games | PuzzleScript | Plain text DSL | Planned |

### Approach

**Tier 1 (Native Patching)**: For binaries you can't fully lift — pointer relocation, font replacement, hex editing.

**Tier 2 (Runtime Replacement)**: For engines you can shim — extract bytecode/script, decompile to IR, transform, and emit modern TypeScript with a replacement runtime.

## Architecture

The pipeline: **Frontend** (extract + decompile) → **IR** (SSA-like, block arguments) → **Transform passes** → **Backend** (emit TypeScript).

### Crates

| Crate | Description |
|-------|-------------|
| `reincarnate-core` | Core IR, entity arenas, system traits, transform pipeline |
| `reincarnate-cli` | CLI binary |
| `reincarnate-frontend-flash` | Flash/SWF frontend (ABC bytecode) |
| `reincarnate-frontend-gamemaker` | GameMaker frontend (GMS1/GMS2 data.win) |
| `reincarnate-frontend-twine` | Twine frontend (SugarCube + Harlowe) |
| `reincarnate-backend-typescript` | TypeScript code emitter |
| `datawin` | GameMaker data.win format parser |

### Runtimes

Each engine has a TypeScript runtime in `runtime/<engine>/ts/` that provides engine-specific API shims on top of a swappable platform layer:

- `runtime/flash/ts/` — Flash display list, events, timeline
- `runtime/gamemaker/ts/` — GameMaker instances, rooms, drawing
- `runtime/twine/ts/` — Twine SugarCube + Harlowe engines

## Usage

```bash
# Full pipeline: extract → IR → transform → emit TypeScript
cargo run -p reincarnate-cli -- emit --manifest path/to/reincarnate.json

# Print human-readable IR (for debugging)
cargo run -p reincarnate-cli -- print-ir <ir-json-file>

# Extract IR only (no transforms/emit)
cargo run -p reincarnate-cli -- extract --manifest path/to/reincarnate.json

# Show project manifest info
cargo run -p reincarnate-cli -- info --manifest path/to/reincarnate.json
```

The `--manifest` flag defaults to `reincarnate.json` in the current directory. Use `--skip-pass` to disable specific transform passes.

## Building

```bash
cargo build
cargo clippy --all-targets --all-features -- -D warnings && cargo test
```

## License

[MIT](LICENSE)
