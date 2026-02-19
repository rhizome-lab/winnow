# Supported Targets

Reincarnate lifts applications from **bytecode and script-based runtimes** — not native binaries. Each engine target has a frontend (extraction + decompilation), a set of IR transform passes, a backend (TypeScript or Rust codegen), and a replacement runtime.

## Implementation Status

| Engine | Frontend | Runtime | Status |
|--------|----------|---------|--------|
| [Flash (AVM2)](./flash) | ✅ | ✅ | Active |
| [GameMaker (GML)](./gamemaker) | ✅ | ✅ | Active |
| [Twine SugarCube](./sugarcube) | ✅ | ✅ | Active |
| [Twine Harlowe](./harlowe) | ✅ | ✅ | Active |
| [Ren'Py](./renpy) | — | — | Planned |
| [RPG Maker](./rpgmaker) | — | — | Planned |
| [Director/Shockwave](./director) | — | — | Planned |
| [Inform (Z-machine/Glulx)](./inform) | — | — | Planned |
| [Ink by Inkle](./ink) | — | — | Planned |
| [Visual Basic 6](./vb6) | — | — | Planned |
| [Java Applets](./java-applets) | — | — | Planned |
| [Silverlight](./silverlight) | — | — | Planned |
| [HyperCard / ToolBook](./hypercard) | — | — | Planned |
| [WolfRPG](./wolfrpg) | — | — | Planned |
| [SRPG Studio](./srpg-studio) | — | — | Planned |
| [RAGS](./rags) | — | — | Planned |
| [QSP](./qsp) | — | — | Planned |
| [PuzzleScript](./puzzlescript) | — | — | Planned |

## Scope

Reincarnate targets **bytecode and script** runtimes where the user logic can be extracted and recompiled. Native code targets (C/C++, DirectX games with no scripting layer) are not a current priority — they are much harder to lift and require a different approach (disassembly + decompilation rather than bytecode decoding). Native decompilation is not planned but is not ruled out in the long run.

Two formats are deliberately excluded from the Twine frontend:

- **Snowman** — Passages are raw JavaScript with `<% %>` templates. There is no macro DSL to lift; the source is already the target.
- **Chapbook** — Minimal adoption and a niche "inserts + modifiers" syntax with no significant ecosystem. Not worth the investment without demonstrated demand.

## Common Pipeline

All Tier 2 targets share the same pipeline:

```
Source binary / scripts
        │
        ▼
   Frontend          (engine-specific: parse bytecode, extract scripts)
        │  untyped IR
        ▼
   Type Inference    (recover concrete types from annotated bytecodes)
        │  typed IR
        ▼
   Transform Passes  (constant folding, mem2reg, DCE, coroutine lowering, …)
        │  optimized IR
        ▼
   Backend           (TypeScript or Rust codegen)
        │
        ▼
   Emitted code + Replacement runtime
```

Each target's roadmap page describes what remains to complete that pipeline.
