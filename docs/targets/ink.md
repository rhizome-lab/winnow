# Ink (by Inkle Studios)

**Status: Planned** — No implementation started. Web runtime already exists; primary value is extraction from host games.

## Format

- **`.ink`** — plain-text source in the Ink markup language (knots, stitches, choices, conditional logic, variables, functions)
- **`.json`** — compiled runtime format produced by `inklecate`. This is the distribution artifact and what the runtime consumes.

The JSON format is **fully documented** in [ink_JSON_runtime_format.md](https://github.com/inkle/ink/blob/master/Documentation/ink_JSON_runtime_format.md). It represents a tree of **Containers** (JSON arrays). Array elements are: prefixed string content (`^text`), control commands (`"ev"`, `"/ev"`, `"out"`, `"pop"`, `"->->"`, `"~ret"`), diverts (jumps/calls), integers, floats, and variable references. Paths use dot-separated notation (`knot.stitch.0`). The last element of each container is either `null` or an object with named sub-containers and metadata flags.

## Runtime

Stack-based VM. Steps through container elements maintaining:
- An output buffer (text and tags)
- An evaluation stack
- Variable state (story variables + temporary variables)
- Visit counts per container (built-in — `"visit"` opcode)
- Divert targets (knots, stitches, tunnels, threads)

Control commands manipulate the stack. Diverts implement function calls, tunnels (subroutines with `->->` return), and thread cloning. Choice points are collected during traversal and presented to the player. The runtime is intentionally non-prescriptive about UI.

## Lifting Strategy

Full recompilation (Tier 2). The goal is emitted TypeScript (or Rust) per knot/stitch — not bundling an interpreter.

1. Parse the `.json` container format (or `.ink` source if available)
2. Emit IR — each knot/stitch becomes a function; diverts become branches/calls; choice points become `Yield` ops
3. The existing transform pipeline (constant folding, DCE, etc.) runs over the IR
4. The TypeScript backend emits clean per-function code

This produces maintainable, backend-agnostic output. It also enables future Rust codegen and cross-engine integration (e.g., an ink story embedded in a lifted GameMaker or Flash game).

### Extraction

Many commercial games bundle ink `.json` files in their data directory (Unity games: `Assets/` or `StreamingAssets/`). The extraction path:
1. Locate `.json` ink files in the game package
2. Parse the container format
3. Emit IR → run transforms → emit TypeScript

For games that compile ink into a native C# runtime (no `.json` files present), binary extraction is required — the ink runtime is embedded in the Unity IL2CPP build.

## What Needs Building

- [ ] `.ink` source parser (for games with source present)
- [ ] `.json` reader → IR emitter:
  - Containers → IR functions
  - Diverts → `Op::Br` / `Op::Call`
  - Choices → `SystemCall("Ink.ShowChoices", ...)` + `Yield`
  - `^text` content → `SystemCall("Ink.Output", text)`
  - Tags → `SystemCall("Ink.Tag", tag)`
  - Visit counts → `SystemCall("Ink.Visits", path)`
- [ ] `SystemCall` namespace: `Ink.Output`, `Ink.ShowChoices`, `Ink.Tag`, `Ink.Visits`
- [ ] Replacement runtime (`runtime/ink/ts/`) — thin UI layer:
  - Text output buffer
  - Choice presentation + player input
  - Variable state
  - Visit count tracking
  - Save/load

## Note: Existing Interpreters

[inkjs](https://github.com/inkle/inkjs) (official JS port) and the `inklecate` compiler exist and work. For rapid one-off deployment these are fine. But they don't produce emitted code — the game runs inside an interpreter loop, which is opaque to further analysis, slower than compiled output, and tied to a single runtime target.

## References

- [ink source (MIT)](https://github.com/inkle/ink)
- [inkjs (official JS port)](https://github.com/inkle/inkjs)
- [ink JSON runtime format documentation](https://github.com/inkle/ink/blob/master/Documentation/ink_JSON_runtime_format.md)
- [Inky editor](https://github.com/inkle/inky)
