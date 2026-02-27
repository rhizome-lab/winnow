# TODO

Completed items archived in [COMPLETED.md](COMPLETED.md).

Per-engine roadmaps (gaps, runtime coverage, open work) live in [`docs/targets/`](docs/targets/). This file tracks in-flight and near-term work across all active engines.

## Planned Engines (not yet started)

Full roadmaps in `docs/targets/<engine>.md`. Summary of where each stands:

| Engine | Blocker / next step |
|--------|---------------------|
| [GameMaker 8.x](docs/targets/gamemaker8.md) | New container parser for `.wad`/GM8 format + opcode adjustments; reuses GMS1 translator — test game: Hotline Miami (`~/reincarnate/gamemaker/hotlinemiami/`) |
| [GameMaker 5/6](docs/targets/gamemaker5.md) | Unpack data from PE exe, then parse GM5/6 format; older/simpler opcode set — test game: Seiklus (`~/reincarnate/gamemaker/seiklus/`) |
| GameMaker YYC | YYC-compiled games have no CODE chunk — logic is in native binary. Requires native decompiler pipeline (out of scope for now). Affects: Katana Zero, Picayune Dreams |
| [Director/Shockwave](docs/targets/director.md) | Format parsing (RIFX/Lingo bytecode) — ProjectorRays and ScummVM are references |
| [Ren'Py](docs/targets/renpy.md) | `.rpa` extractor → `.rpyc` decompile (unrpyc) → Ren'Py AST → IR |
| [RPG Maker VX Ace](docs/targets/rpgmaker.md) | `Scripts.rvdata2` extractor (Ruby Marshal) → Ruby AST → IR |
| [RPG Maker MV/MZ](docs/targets/rpgmaker.md) | JSON event command compiler → IR |
| [Inform (Z-machine/Glulx)](docs/targets/inform.md) | Story file parser → bytecode decoder → IR; well-documented specs |
| [Ink by Inkle](docs/targets/ink.md) | `.json` container reader → IR (knots/stitches → functions, choices → Yield) |
| [Visual Basic 6](docs/targets/vb6.md) | PE/VB6 header parser → P-Code decoder → IR |
| [Java Applets](docs/targets/java-applets.md) | JAR/class file parser → JVM bytecode decoder → IR; JVM spec is thorough |
| [Silverlight](docs/targets/silverlight.md) | XAP extractor → PE/CLI parser → IL decoder → IR + XAML parser |
| [HyperCard](docs/targets/hypercard.md) | Stack binary parser → HyperTalk text parser → IR (scripts are source, not bytecode) |
| [WolfRPG](docs/targets/wolfrpg.md) | `.wolf` decryption → event command compiler; `wolfrpg-map-parser` Rust crate exists |
| [SRPG Studio](docs/targets/srpg-studio.md) | `data.dts` decryption → NW.js API shim (engine is already JS) |
| [RAGS](docs/targets/rags.md) | NRBF/SDF decryption → game data extractor; `rags2html` is the reference impl |
| [QSP](docs/targets/qsp.md) | `.qsp` decoder → QSP-lang parser → IR; open-source `libqsp` is reference |
| [PuzzleScript](docs/targets/puzzlescript.md) | Source parser → rule compiler → IR (rule semantics are formally specified) |

---

## TODO.md Staleness Audit (HIGH PRIORITY)

- [ ] **Audit TODO.md for stale items** — Many items were written months ago and may already be implemented, superseded, or no longer relevant. Go through each open `[ ]` item, check against the codebase, and either mark `[x]` with a note, update the description, or delete if obsolete. Known example: `IR-level closure representation` was stale (now marked done). `withBegin`/withEnd design debt references "no closure construct" which is now resolved but the debt itself is still live.

---

## End-to-End Regression Tests

- [ ] **Snapshot tests for both frontends** — No snapshot infrastructure yet.
  - **Flash**: 15 new vN identifiers regressed in `91fe86e` (MethodCall
    refactor). Pre-existing 5 vN (hasNext2 one-shot, split-path phi).
  - **GML**: Reference decompilation at `~/git/bounty/`.

## Unit Test Infrastructure

### GML Translator Tests

- [ ] **Narrow `DataWin` dependency in `TranslateCtx`** — `translate_code_entry`
  takes `ctx.dw: &DataWin` but only uses it for string resolution (3 call sites:
  local var name resolution × 2, push-string operand × 1). Replace with a
  `resolve_string: &dyn Fn(u32) -> &str` field (or equivalent) so tests can
  construct a `TranslateCtx` without a real `DataWin`.

- [ ] **GML translator unit tests** — Once `DataWin` dependency is narrowed,
  add regression tests for:
  - 2D array write stack pop order (`ref_type==0` VARI pops must check
    `is_2d_array_access` before popping — `dim1` on top, not value)
  - AVM2 `ref_type==0xA0` singleton field access (no pops)
  - `argument` variable mapping (`dim1=-1, dim2=N` → `fb.param(offset + N)`)

### Flash Translator Tests

- [ ] **Flash translator unit tests** — The `swf` crate (v0.2.2) has
  `avm2::write::Writer` + `write_op()`, so AVM2 bytecode is constructable.
  Still need a minimal `AbcFile` builder to avoid constructing a full SWF.
  Add regression tests for:
  - `SetProperty` operand order (receiver pushed before value, not after)
  - `Debug` register layout (register names extracted from `DebugFile`/`DebugLine`)

- [ ] **Minimal `AbcFile` test builder** — Helper that constructs a bare-minimum
  `AbcFile` (empty constant pool, one method body with supplied ops) so Flash
  translator tests don't require real SWF files.

## Type System

### Open

- [ ] **GML instance ID type propagation** — When `instance_create_depth(x, y, d, OFoo)` is
  called, the return type should be inferred as `OFoo` (or `InstanceRef<OFoo>`), not `any`.
  This type must flow through assignments and field accesses — `let enemy = instance_create(..., OEnemy);
  enemy.health -= 1` should type `health` as a field of `OEnemy`. Also: `withInstances(inst, cb)`
  should type `_self` in `cb` as the same type as `inst`. This is critical for emitting maintainable
  code — without it, every cross-instance field access is `any`-typed. Requires the type inference
  pass to understand `instance_create_*` return types, and to propagate the object class through
  the type system as `Struct(className)`.

- [ ] **Flow-sensitive narrowing** — Narrow types after guards
  (`if (x instanceof Foo)` → `x: Foo` in then-branch). Requires per-block type
  environments rather than the current single `value_types` map.
- [ ] **Flash frontend: emit concrete types** — AVM2 bytecode has type
  annotations on locals, parameters, fields, return types. `resolve_type`
  failures cause unnecessary `Dynamic` entries.
- [ ] **Untyped frontend validation** — Test the inference pipeline against a
  fully-untyped IR (simulating Lingo/HyperCard).

### Remaining `:any` analysis (Flash cc-project, 541 total)

Measured after TypeInference + ConstraintSolve + Alloc refinement.

| Category | Count | Root cause |
|----------|-------|------------|
| `any[]` arrays | 185 | Array element type unknown — no element-type inference yet |
| Parameter `: any` | 186 | Untyped function params from ABC metadata gaps |
| Return `: any` | 79 | Functions returning Dynamic (unresolved return types) |
| Field `: any` | 80 | Struct fields without type info (empty class defs, external supers) |
| `let` locals | 11 | Block params where incoming args don't all agree |
| `const` locals | 9 | Genuinely untyped values from calls/block params |

### Known Issues

- **Multi-typed locals** — Some Flash locals are assigned different types in
  different branches (e.g. `race` initialized to `0.0` as a sentinel, then
  assigned `this.player.race()` which returns `string`). These correctly stay
  `Dynamic` / `:any` today. For TypeScript this is ugly but functional. For
  Rust emit this is a hard blocker. Options:
  - **Split into separate variables** per SSA def when types disagree
  - **Enum wrapper** — tagged union for the specific types observed
  - **Sentinel elimination** — recognize sentinel-then-overwrite → `Option<T>`
  - Emit union type annotation (`number | string`) instead of `any`

## Runtime Audits (Evergreen)

These are recurring health-checks, not one-off fixes. Run them periodically and update the "last audited" date.

### Module-level mutable state — last audited 2026-02-22

State at module scope prevents two game instances from coexisting on the same page. Any `let` or lowercase-named singleton `const` at the top level of a `.ts` file is a smell.

```bash
# Top-level let declarations (mutable by definition)
grep -rn "^let \|^export let " runtime/ --include="*.ts" | grep -v node_modules

# Top-level singleton instances (lowercase name + new = likely singleton)
grep -rn "^const [a-z].* = new " runtime/ --include="*.ts" | grep -v node_modules
```

Known violations as of 2026-02-22 (see high-priority issues below for details):
- `runtime/flash/ts/flash/display.ts` — `export let _dragTarget/Bounds/LockCenter/OffsetX/OffsetY`
- `runtime/flash/ts/audio.ts` — `_audio` (AudioState singleton)
- `runtime/flash/ts/input.ts` — `state` (InputState singleton)
- `runtime/flash/ts/timing.ts` — `state` (TimingState singleton)
- `runtime/flash/ts/renderer.ts` — `canvas`, `ctx` (hardcoded DOM element)
- `runtime/twine/ts/platform/save.ts` — `state`, `backend`, `gotoFn`, `slotPrefix`, `autosaveEnabled`
- `runtime/twine/ts/platform/_overlay.ts` — `dialog` (DialogManager singleton)
- `runtime/twine/ts/platform/input.ts` — `input` (InputManager singleton)
- `runtime/twine/ts/platform/layout.ts` — `layout` (LayoutManager singleton)
- `runtime/twine/ts/platform/save-ui.ts` — `saveUI` (SaveUI singleton)
- `runtime/twine/ts/platform/settings-ui.ts` — `settingsUI` (SettingsUI singleton)

### GML runtime stubs — silent returns audit — last audited: (never)

Many GML built-in stubs added in 2026-02 silently return `0`, `""`, `false`, or `-1` for
functions that require real implementations (collision, path-finding, particle systems,
video, vertex buffers, DS operations, etc.). Per the CLAUDE.md rule, these should throw
`Error("name: not yet implemented")` instead of returning wrong values silently.

```bash
# Find suspicious silent-return stubs in GML runtime
grep -n "{ return 0; }\|{ return \"\"; }\|{ return false; }\|{ return -1; }" \
  runtime/gamemaker/ts/gamemaker/runtime.ts | grep -v "// genuine"
```

Functions to audit (partial list):
- `mp_grid_*` — pathfinding (returns -1/void, but caller uses return value for grid ID)
- `collision_point/circle/ellipse` — collision (returns -4 but caller may iterate over results)
- `path_start/path_get_length` — path following
- `part_*` — particle system state
- `instance_deactivate_all`, `instance_furthest`, `instance_position`
- `ds_priority_*` — DS priority queue (partially implemented)
- `buffer_*` async variants
- `surface_copy`, `vertex_*` — graphics
- `layer_get_depth`, `layer_x`, `layer_y` — layer state queries

### Runtime type widening — last audited: (never)

Review runtime files for type signatures widened to silence TypeScript errors from game code (e.g. `string` → `any`, required param made optional). Such changes hide real bugs.

```bash
# Hunt for any-typed params/returns that shouldn't be
grep -rn ": any\b" runtime/ --include="*.ts" | grep -v node_modules | grep -v "Record<string, any>"
```

Audit targets: `runtime/twine/ts/harlowe/`, `runtime/twine/ts/sugarcube/`.

The rule: never widen types to accommodate buggy game code — TypeScript catching game author mistakes is correct behavior.

---

## Runtime Architecture — High Priority

### Flash runtime: module-level singletons (multi-instance blocker)

All mutable state in the Flash runtime lives at module scope, which means two Flash games cannot run on the same page and a second `createFlashRuntime()` call would stomp on the first. All of the following need to move onto the `FlashRuntime` instance (analogous to how `GameRuntime` was designed from the start).

- [x] **`flash/display.ts` — exported drag state** (`_dragTarget`, `_dragBounds`, `_dragLockCenter`, `_dragOffsetX`, `_dragOffsetY`). Replaced with `_dragStateByStage: WeakMap<Stage, DragState | null>` — now keyed by Stage so each FlashRuntime has isolated drag state.
- [ ] **`flash/display.ts` — `_displayState` singleton** (line ~1209). Single-threaded JS means two concurrent `constructRoot()` can't interleave, so this is safe in practice but still wrong in principle. Move to a WeakMap keyed by Stage or pass stage directly.
- [ ] **`flash/timing.ts` — `TimingState` singleton** — `state = new TimingState()` at module scope (line 9); `timing` object wraps it. Move to `FlashRuntime.timing` instance field; same pattern as `GameRuntime.onTick`.
- [ ] **`flash/input.ts` — `InputState` singleton** — `state = new InputState()` at module scope (line 12); DOM event listeners registered unconditionally on import. Move to `FlashRuntime.input`; attach/detach listeners in `start()`/`stop()`.
- [ ] **`flash/audio.ts` — `AudioState` singleton** — `_audio = new AudioState()` at module scope. Move to `FlashRuntime.audio`; `_ensureInit()` becomes part of `FlashRuntime.start()`.
- [ ] **`flash/renderer.ts` — hardcoded canvas** — `canvas = document.getElementById("reincarnate-canvas")` at module scope (lines 1–2). Move to `FlashRuntime.renderer`; accept canvas element (or ID) as a constructor arg.
- [ ] **`flash/memory.ts` — shared heap** — `heap = new ArrayBuffer(HEAP_SIZE)` and typed array views at module scope (lines 4–13). Unlikely to matter in practice (single game per page for Flash), but still wrong in principle. Move to `FlashRuntime`.

### Twine platform: module-level singletons (multi-instance blocker)

The Twine platform modules (`save`, `input`, `layout`, `save-ui`, `settings-ui`, `_overlay`) all follow the same pattern: define a class, instantiate it once at module scope, export methods that close over the singleton. They should export the class (or a factory) and let the runtime wire instances together — same as how GML's `GameRuntime` works.

- [ ] **`platform/save.ts`** — five `let` declarations at module scope (`state`, `backend`, `gotoFn`, `slotPrefix`, `autosaveEnabled`). Export a `SaveManager` class; `SugarCubeRuntime`/`HarloweRuntime` constructs it.
- [ ] **`platform/_overlay.ts`** — `dialog = new DialogManager()` singleton. Export the class.
- [ ] **`platform/input.ts`** — `input = new InputManager()` singleton. Export the class.
- [ ] **`platform/layout.ts`** — `layout = new LayoutManager()` singleton. Export the class.
- [ ] **`platform/save-ui.ts`** — `saveUI = new SaveUI()` singleton. Export the class.
- [ ] **`platform/settings-ui.ts`** — `settingsUI = new SettingsUI()` singleton. Export the class.

### `flash/utils.ts` — module-level class registries (multi-instance, emitter change required)

`_interfaceRegistry` (WeakMap<Function, Set<Function>>) and `_traitRegistry` (WeakMap<Function, ClassTraits>) are already keyed by constructor identity and thus effectively per-game. But `_classRegistry` (Map<string, Function>) is keyed by qualified name string, meaning `getDefinitionByName("com.example::Foo")` would return whichever game registered that name last.

- [ ] **`flash/utils.ts` — `_classRegistry` string-keyed map** (`getDefinitionByName` returns wrong class if two games use the same qualified name). Fix requires the emitter to generate per-runtime registration calls (`_rt.registerClass(Foo)` instead of module-level `registerClass(Foo)`) — non-trivial emitter change. Low priority: Flash games running simultaneously is rare.
- [ ] **`flash/utils.ts` — `_bindCache` WeakMap** — `as3Bind()` caches bound functions globally across all games. Since it's keyed by `(function, thisArg)` pairs which are game-specific objects, no cross-game pollution occurs in practice. Safe to leave.

### `flash/vector.ts` — `Array.prototype` mutation

- [ ] **Prototype pollution** — `vector.ts` adds `removeAt` and `insertAt` to `Array.prototype` (lines 17–27) as a side effect of import. This bleeds across the entire page. Correct fix: either use a typed `Vector<T>` wrapper class (correct but invasive), or at minimum scope the side effect to only apply when the Flash runtime is initialized rather than on module load.

---

## Third-Party Engine Libraries

Many real-world games embed third-party macro/plugin libraries alongside the engine.
These appear as `unknown_macro` warnings (Twine) or unresolved calls (other engines)
but are *not* missing built-ins — they are authored libraries distributed alongside
the game. Each needs to be identified, understood, and either implemented against the
platform layer or stubbed with a clear diagnostic.

**General strategy:** For each library, read its source embedded in the game, understand
its API, and decide:
1. **Implement** — map to the platform layer (e.g. audio library → platform audio)
2. **Stub** — no-op with a runtime warning (purely visual effects, dev tools)
3. **Warn at compile time** — emit a named diagnostic instead of per-call-site noise

**Detection:** During extraction, scan for library registration patterns
(e.g. `Chapel.Macros.add(` in Harlowe, `Macro.add(` in SugarCube) and record
discovered library names + macro lists in `FrontendOutput` metadata. This enables
precise warnings ("uses HAL audio — implement platform audio shim") rather than
generic unknown-call spam.

**Known libraries observed in the wild:**

- [ ] **HAL (Harlowe Audio Library)** — `(masteraudio:)`, `(track:)`, `(newtrack:)`,
  `(newplaylist:)`, `(newgroup:)`, `(playlist:)`, `(group:)`. Registered via
  `Chapel.Macros.add()`. Maps naturally to the platform audio layer.
  Observed in: Artifact v0.76 (Harlowe).


## Format Spec (game_maker_data.ksy)

- [ ] **Submit to kaitai_struct_formats** — PR to `kaitai-io/kaitai_struct_formats`
  under `game/game_maker_data.ksy`. This is the "never RE it again" step — the spec
  is only findable by the community once it's in the format gallery.
  *Low priority — gated on polish (full chunk coverage, fixture-validated, clean doc strings).*

- [x] **gml_bytecode.ksy** — Separate Kaitai spec for GML instruction encoding.
  Covers: opcode layout (v14 vs v15+ numbering), operand formats (Double/
  Int32/Int64/String/Variable/Int16), Break signal encoding (GMS2.3+ extended
  signals including pushref/chknullish/isstaticok), Dup type-size semantics,
  variable_ref bit layout, instance_type enum, branch offset encoding.
  Lives at `crates/formats/datawin/gml_bytecode.ksy`.

- [ ] **datawin fixture tests (ongoing — goal: 100% format coverage)** — 15 synthetic
  fixtures covering all 20 chunk types now exist in `crates/formats/datawin/tests/fixtures/`
  (57 Rust tests, validated by Kaitai). The goal is complete coverage of every variant the
  format specifies: every field, every conditional branch, every version difference. Keep
  expanding when there's time: GMS2 TXTR entries (currently only GMS1 format tested),
  OBJT with actual events + event actions, ROOM with object instances, physics vertices,
  BC≥17 OBJT `_managed` field, SPRT with multiple tpag entries, multi-entry FONT with
  multiple glyphs, SEQN with GMS2.3+ full entries, etc. Each new case: add builder in
  `gen_fixtures.rs`, regenerate (`cargo run -p datawin --bin gen_fixtures`), add tests in
  `fixture_tests.rs`, update `kaitai_validate.py`. Real-game tests in `tests/read_files.rs`
  still provide broader coverage when run with `--include-ignored`.

## CLI — Project Registry

- [ ] **Project registry** — Persistent project registry so you don't have to pass `--manifest` every time.

  **Fully decided:**

  **Registry storage**
  - File: `~/.config/reincarnate/projects.json` (global, XDG config dir)
  - Schema: `{ "version": 1, "projects": { "<name>": { "manifest": "<abs-path>", "added_at": "<iso8601>", "last_emitted_at": "<iso8601> | null" } } }`
  - `last_emitted_at` enables future `--stale` / `--since` filters on `--all`
  - Load-time version check: if `version > 1`, error with "registry version N not supported — please upgrade reincarnate"; no auto-migration
  - Registry loaded from disk at startup when a registry-aware subcommand runs, passed as a value — no global state

  **`reincarnate add [path] [name]`**
  - `path` accepts: a directory (searches for `reincarnate.json` inside), a direct `.json` file, or omitted (searches ancestors of cwd — same upward-walk as current `resolve_manifest_path`)
  - Name defaults to the folder name of the manifest's parent directory
  - Error on collision with message hinting `--force`; `--force` overwrites
  - Verifies manifest file exists at add-time (defers parseability to emit-time)

  **`reincarnate remove <name>`** — remove entry

  **`reincarnate list`**
  - Tabular output: name | engine | manifest path | last emitted
  - Default sort: alphabetical by name; `--sort=engine|last-emitted` flags
  - `--json` flag for scripting (JSON array of project objects)
  - Output format modelled after normalize's `OutputFormat` (Compact/Pretty/Json) — use `--json` for Json mode; TTY auto-detects Pretty vs Compact

  **`reincarnate emit <name>`** — registry lookup → manifest path → existing pipeline
  **`reincarnate emit <path>`** — bare path accepted as positional; no `--manifest` required
  **`reincarnate emit --manifest <path>`** — existing behaviour unchanged
  **`reincarnate emit --all`** — sequential by default; `--parallel` flag for concurrent
  - Output format: per-project sections (`[1/3] bounty (gamemaker)\n  ...`)
  - Error handling: continue-and-collect — finish all projects, print failure summary at end

  **All commands accept bare path as positional arg** (not just `--manifest <path>`) when they take a manifest input. Commands: `emit`, `extract`, `info`.

  **`reincarnate info <name-or-path>`** — unified: accepts registry name, directory, or `.json` path; replaces old `--manifest`-only form

## CLI — Build Configuration

- [x] **Feature-gate frontends, backends, and checkers in the CLI** — All 5 plugin crates are behind
  Cargo feature flags (`frontend-flash`, `frontend-gamemaker`, `frontend-twine`, `backend-typescript`,
  `checker-typescript`), all enabled by default. Build with `--no-default-features` for a minimal binary.

## Future

- [ ] **Split OPFS out of localStorage backend** — `runtime/gamemaker/ts/shared/platform/persistence.ts` currently bakes OPFS in as a fire-and-forget side effect of localStorage writes. Per the persistence design, OPFS should be its own backend composed via `tee(localStorage, opfs)`. Split into `persistence/localstorage.ts` + `persistence/opfs.ts`, wire with `tee()` in `platform/index.ts`. Twine platform (`runtime/twine/ts/platform/persistence.ts`) is localStorage-only and will benefit from the same split.

- [ ] **Cloud save backends** — Platform persistence interface already abstracts save/load/remove; swapping the backend is just a different platform implementation. Candidates: OneDrive, Google Drive, Dropbox, S3/R2/B2. Design: `platform/onedrive.ts`, `platform/gdrive.ts`, etc., each re-exporting the same persistence interface. Config: deployer switches backend by changing re-export in `platform/index.ts`.

- [x] **IR-level closure representation** — `Op::MakeClosure { func, captures }`, `CaptureMode::ByValue`/`ByRef`, `CaptureParam`, and `add_capture_params` on `FunctionBuilder` are all implemented. SugarCube, Harlowe, and GML frontends emit `MakeClosure` with explicit capture lists. TypeScript backend rewrites to IIFE-with-captures (by value) or plain arrow (no captures). DCE tracks captures as uses. `<<capture>>` is a correct no-op — our IIFE pattern already snapshots by value, making SugarCube's workaround unnecessary. Remaining gaps: (1) Flash closures still use `MethodKind::Closure` + TS lexical closure rather than `Op::MakeClosure` — see "Inline closures" below; (2) `CaptureMode::ByRef` is defined but unused.
- [x] **GML default argument recovery pass** — `GmlDefaultArgRecovery` detects the GMS2.3+
  `if (arg === self.undefined) arg = default` IR pattern and folds constant defaults into
  `FunctionSig.defaults`. Also sets type-matched defaults for variadic script params
  (post-inference, reads narrowed types from `value_types`): `""` for string, `false` for
  bool, `0` for number. Dead Estate TS2554: 2069→859, TS2555: 149→0. Bounty TS2555: 251→0.
- [x] **GML param type inference gaps — cross-function call-site narrowing** — 76% of GML
  function `argumentN` params remain `: any` after inference. Dead Estate: 1407/1855 `any`.
  Bounty: 53/77 `any`. Root cause: ConstraintSolve runs per-function and only propagates
  callee param types → caller arg types, never the reverse. Call sites are the biggest
  untapped source of type info.
  **Implemented**: `CallSiteTypeFlow` pass in `transforms/call_site_flow.rs`. Runs between
  TypeInference and ConstraintSolve. Collects argument types from all `Op::Call` and
  `Op::MethodCall` sites, narrows `Dynamic` params when all callers agree on a concrete type.
  Skips self-calls, `CallIndirect`, `SystemCall`, and `Dynamic` args.
- [ ] **CallSiteTypeFlow: union type narrowing (intentionally deferred)** — When callers
  disagree on types (e.g. one passes `string`, another `number`), the param stays `Dynamic`
  rather than being narrowed to `string | number`. Adding union narrowing would require
  downstream work in ConstraintSolve (union unification) and emitter (union type annotations).
  Deferred indefinitely — the single-type narrowing covers the majority of cases where all
  callers agree, and disagreement genuinely suggests `any` is appropriate.
- [ ] **GML fixpoint + CallSiteTypeFlow: instance ID narrowing causes TS2339 spike** —
  With `--fixpoint` enabled on Dead Estate, TS2339 "Property does not exist" errors jump from
  44 (baseline) to 217. Root cause: GML instance IDs are `Float(64)` at the call site (returned
  by `instance_create`, `instance_find`, etc.), so fixpoint propagates `Float(64)` → `number`
  into callee `self` params via ConstraintSolve. The callee then accesses `self.field` directly,
  which TypeScript rejects on `number`. `run_once` on CallSiteTypeFlow helps slightly (217 → 217,
  same pattern) but the bulk comes from ConstraintSolve re-propagating the first round's
  narrowing in subsequent iterations. Two potential fixes:
  1. CallSiteTypeFlow: skip narrowing `Float(64)` → `Float(64)` for GML (instance IDs are
     semantically opaque handles, not plain numbers). But this would block all float narrowing.
  2. Deeper fix: GML instance IDs need a distinct IR type (e.g. `InstanceId`) separate from
     `Float(64)`, so they don't conflate "this is a float" with "this is an object handle".
  The remaining 16 new TS2339s are `string.push` errors — array params narrowed to `string`
  from a control-flow path that initialises as a string. Likely game author bugs surfaced by
  narrowing.
- [ ] **Frontend-controlled pass ordering** — `extra_passes` are currently appended after the
  entire default pipeline. Frontends should be able to specify where their passes run (e.g.
  "after constraint-solve but before mem2reg"). Current approach works for IntToBoolPromotion
  and GmlLogicalOpNormalize which are fine running last, but won't scale.
- [ ] Rust codegen backend (emit `.rs` files from typed IR — **blocked on multi-typed locals**)
- [ ] wgpu + winit renderer system implementation
- [ ] Web Audio system implementation
- [ ] Native binary decompilation (C/C++, DirectX games with no scripting layer) — requires disassembly + decompilation pipeline rather than bytecode decoding; far harder than any current target; no timeline

## CLI — `reincarnate check` (language-agnostic output validation)

- [x] **`reincarnate check` subcommand** — Implemented: Checker trait in core, TsChecker crate
  (tsgo via bunx), CLI wiring with `--no-emit`, `--json`, `--all` flags. Supports registry names,
  paths, and bare positional args. Prints per-code and per-file breakdown.
- [x] **`--baseline <file>` for check** — `--save-baseline <path>` saves check results as JSON;
  `--baseline <path>` compares against it and reports per-code/per-file deltas. Exits non-zero on regression.

## Diagnostics

- [ ] **External type member validation** — 90 member warnings from types
  inheriting Flash stdlib classes. Need structured member metadata from runtime
  type definitions to validate these.

### Discarded AVM2 Metadata

The Flash frontend discards several categories of ABC metadata that could
improve output fidelity:

- [ ] **Exception handler metadata** — `from`/`to` byte offsets,
  `variable_name`, `type_name`. No try/catch in IR yet.
- [ ] **Class flags** — `is_sealed`, `is_final`.
- [ ] **Protected namespace** — Per-class protected namespace for `protected`
  member visibility.
- [ ] **Trait metadata annotations** — `[Embed]`, `[Bindable]`, custom.
- [ ] **Trait `is_override` / `is_final` flags** — `override` keyword in TS.
- [ ] **Slot/dispatch IDs** — AVM2 vtable layout, irrelevant to decompilation.
- [ ] **`DebugLine` source line info** — Could emit `// line N` or source maps.

## Flash Output Quality

### Correctness

- [ ] **Complex loop decompilation** — Some while-loop bodies have unreachable
  code after `continue`, wrong variable assignments.

### Optimizations — Safe (no semantic change)

- [x] **Redundant type casts** — `strip_redundant_casts` AST pass eliminates
  `as number` when VarDecl/param type already matches. 584 → 108 in Flash.
  Remaining 108 are field accesses where field types aren't tracked.
- [ ] **Constant `rand(n)` where n <= 1** — `rand(1)` always returns 0. Only 1
  known instance (PhoukaScene).
- [ ] **Dead store elimination** — Remove assignments whose values are never
  read. Requires liveness analysis.
- [ ] **Condition inversion** — Structurizer sometimes inverts conditions.
  Heuristic to match original branch polarity.

### Optimizations — Requires alias/purity analysis

- [ ] **Cross-side-effect const sinking** — Sink `const v = expr` past
  side-effecting statements when `expr` is provably pure and unaliased.
- [ ] **Method reference inlining** — `const v = this.method; ... v(args)` →
  `this.method(args)`. Only safe if `method` is not a getter.
- [ ] **Field read deduplication** — `this.x` read twice → read once, reuse.

### Optimizations — Requires control flow analysis

- [ ] **Inline closures** — Some Flash closures still fall back to
  `this.$closureN` field references when `compile_closures()` fails to
  compile them (e.g. dynamic features). These should be diagnosed and fixed
  case-by-case. Twine closures are now fully inlined as of `26ecc6a`.
- [x] **Loop variable promotion** — Fixed `match_compound_assign` and
  `is_var_update` to look through `AsType` casts. Flash: ~65 additional
  while→for promotions. Remaining while-loops use class fields, parameters,
  pre-increment patterns, or complex multi-step increments.

## GameMaker — Version-Gating Audit (HIGH PRIORITY)

The GML frontend does not pass `BytecodeVersion` to any translator, decoder, or rewrite.
Every behavior runs unconditionally regardless of whether the game is GMS1, GMS2, or GMS2.3+.
Many behaviors are version-specific and applying them to the wrong version can produce silent
wrong output. The `BytecodeVersion` is already extracted from GEN8 and available on `DataWin`
(`dw.bytecode_version()`). It needs to be threaded into `TranslateCtx` and all relevant code
paths that make version-sensitive decisions.

**What to audit** — every file under `crates/frontends/reincarnate-frontend-gamemaker/src/`:

- [ ] **`lib.rs`** — `gml_GlobalScript_` skip (added for GMS2.3+ migration pattern; safe for
  GMS1/GMS2 since those games don't have `gml_GlobalScript_*` CODE entries in SCPT, but should
  be documented and guarded with a version assertion). Also: FUNC chunk translation, GLOB chunk
  translation, `scan_code_refs` — all may need version guards.

- [ ] **`decode.rs`** — Dup swap mode no-op (`dup_extra != 0` branch), Break signal decoding
  (signals -10/-11 etc. are GMS2.3+ only; older games don't have them). The `DataType` and
  `OpCode` decoding may differ between v14 and v15+ instruction formats — confirm `has_new_instruction_format` is respected.

- [ ] **`translate.rs`** — All code that assumes GMS2.3+ behaviour:
  - Shared bytecode blobs (`filter_reachable`) — only needed for GMS2.3+ shared CODE entries
  - `scan_body_argument_indices` + `argument` captures in with-body — may not apply to GMS1 where `argument` is always global
  - `InstanceType::Stacktop` (-9) as struct method self-reference — GMS2.3+ construct; in GMS1 `-9` is always a genuine stack pop
  - `args_count & 0x7FFF` masking — 0x8000 flag meaning differs between versions
  - Negative instance IDs below -9 (e.g. -16 for `Arg`) — confirm range is version-stable

- [ ] **`object.rs`** — Event type encoding may differ between GMS1 and GMS2; object/event
  structure differences (e.g. `persistent`, `visible` fields, parent indices).

- [ ] **`data.rs`** — Sprite/texture/audio asset structures differ between versions. TXTR
  external textures (GMS2.3+), SEQN/TAGS/ACRV/FEDS chunks — guard against parsing these on
  older versions.

**Action**: Add `bytecode_version: BytecodeVersion` to `TranslateCtx`; add version-check
helper methods (`is_gms23_plus()`, `is_gms2_plus()`) to `BytecodeVersion`; replace any
implicit version assumptions with explicit version guards. Log a warning when a GMS2.3+ feature
is detected on a game that reports an older version.

## GameMaker — New Game Failures (discovered 2026-02-22)

Batch-emitting 7 new games from the Steam library exposed 4 distinct bugs:

### 1. `argument` inside `with`-body panics — blocks 10SecNinjaX, 12BetterThan6, VA-11 HALL-A

- [x] **`argument[N]` accessed inside a `with`-body closes over wrong param index** — **FIXED**.
  Guards added to `translate_push_variable` and `translate_pop` for both the named (`argument0`)
  and stacktop (`argument[N]`) forms: check `locals` map for `_argumentN` capture first (with-body
  case), then bounds-check before calling `fb.param()`. Unblocked 10SecNinjaX, 12BetterThan6.

### 2. TXTR external textures panic — blocks Downwell

- [x] **`txtr.rs:102` slice-end underflow when textures are stored externally** — **FIXED**.
  `texture_data()` now returns `None` when `end < start || end > data.len()`. Downwell now emits
  (remaining errors are runtime API gaps, not parse bugs).

### 3. PE-embedded `data.win` not supported — blocks Momodora RUtM

- [x] **Reader requires FORM at offset 0, but Momodora embeds it in a PE exe** — **FIXED**.
  `DataWin::parse` detects MZ magic and scans all FORM occurrences for the first one whose
  declared size fits within the file (avoids false positives in PE sections). Also fixed 0-size
  CODE/VARI/FUNC chunks for YYC-compiled games (early-return empty structs). Momodora now emits.

### 4. Forager parse error at EOF / Risk of Rain CODE chunk empty

- [ ] **Forager `game.unx` hits unexpected EOF while parsing** — the reader reaches absolute
  file offset 81446624 (= EOF) and attempts a 4-byte read. All top-level chunks parse correctly;
  the failure is inside a chunk's content parser. Likely the CODE chunk bytecode decoder reading
  a function entry whose stated length extends to exactly EOF, then attempting to read past.
  Needs `--dump-ir` + targeted investigation.

- [x] **Risk of Rain `game.unx` has empty CODE/VARI/FUNC chunks (YYC-compiled)** — **FIXED**
  by the same early-return guards added for Momodora (Bug 3). Both GMS1 and GMS2 YYC games now
  parse correctly with empty bytecode chunks.

### 5. Sprite name bracket notation missing for access side — blocks Nubby's, Mindwave, MaxManos2

- [x] **`Sprites.3DPegBase` emitted instead of `Sprites["3DPegBase"]`** — **FIXED** in commit
  `9e1b5d7`. Both `resolve_sprite_constant` (emit.rs) and `try_resolve_sprite_assign` (rewrites/
  gamemaker.rs) apply `is_valid_js_ident` and use bracket notation when false. Remaining errors
  in Nubby's/MINDWAVE/MaxManos2 are Bug 7 (struct field access on Number) and runtime API gaps.

### 6. pushac/popaf array capture coerces array to int — Schism syntax errors

- [ ] **`int(argument0)[FxDoomApply.gunMod] = 0` produces TS1005 syntax error** — `pushac`
  captures `int(argument0)` as the "array reference", but `argument0` is actually an array
  passed by reference. The `coerce v1, i32` from a preceding `Push Variable(argument0,
  type=Int32)` converts the array to an integer before `pushac` saves it. `popaf` then calls
  `set_index(int_value, array_value, 0)` which the TS printer emits as `int(argument0)[array]
  = 0` — invalid JS. Root cause: type mismatch between the Int32 push type and the actual
  array type of argument0. Fix requires either: (a) not coercing when the value is used as a
  pushac target, or (b) the TS printer detecting integer-as-collection in SetIndex and routing
  to a GameMaker.setIndex runtime call. Only 6 errors in Schism, low priority.

### 7. Dead Estate remaining TS errors — 1622 as of 2026-02-27

Progress: 12350 → 4151 → 3341 → 2112 → 879 → 743 → 2927 → 1622. GmlDefaultArgRecovery
pass folds GMS2.3+ `if (arg === undefined) arg = val` patterns into FunctionSig.defaults;
variadic scripts get `= 0.0` on fixed params. TS2554: 2069→859, TS2555: 149→0.

| Code | Count | Root cause |
|------|-------|------------|
| TS2554 | 859 | Wrong argument count — remaining are non-default-related mismatches |
| TS2345 | 330 | Argument type mismatch — 176 are `number→GMLObject` at `instance_destroy` (see Bug 7e below) |
| TS2322 | 189 | Type not assignable — `boolean→number` (GML no-bool-type) + misc |
| TS2367 | 53 | Comparison always false — type mismatch in `===` (game author errors) |
| TS2339 | 44 | Property doesn't exist |
| TS7027 | 32 | Unreachable code — **structurizer/emitter bug** (see Bug 7c below) |
| TS2365 | 27 | Operator not applicable — bitwise/arithmetic on wrong type |
| TS2304 | 25 | Cannot find name — **linearizer/structurizer bugs** (see Bug 7d below) |
| TS2362 | 20 | Left side of `**`/arithmetic must be number |
| TS18050 | 19 | `null` in non-null position — Mem2Reg null sentinels used in comparisons/arithmetic |
| TS7053 | 8 | Element implicitly has `any` type |
| TS2363 | 6 | Right side of `**` must be number |
| TS2872 | 3 | Comparison appears unintentional |
| TS2308 | 3 | Duplicate re-export (`___struct___127/128`, `TextEffect`) in barrel `index.ts` |
| TS2300 | 2 | Duplicate identifier |
| TS2552 | 1 | Cannot find name (with-body self-reference bug) |
| TS2307 | 1 | Cannot find module — `runtime/argument` (out-of-range GML `argument` access) |
| TS18047 | 1 | Object is possibly null |

#### TS2345 Detailed Breakdown (2043 errors, by type pair)

| Count | Source type → Expected type | Root cause |
|------:|----------------------------|------------|
| 505 | `(_rt, self) => void` → `number` | **pushref name resolution (Bug 7a)**: GMS2.3+ constructor refs resolved to imported function values instead of numeric asset indices |
| 432 | `number` → `boolean` | **GML calling convention (Bug 7b)**: GML has no boolean type; `0`/`1` used everywhere for boolean params (`audio_play_sound(..., loop)`, `sprite_create_from_surface(..., removeback, smooth)`) |
| 116 | `() => string` → `number` | **pushref name resolution (Bug 7a)**: e.g. `instance_exists(steam_current_game_language)` — `steam_current_game_language` is a runtime function but should be a numeric object ID |
| 111 | `(_rt, self, arg0?, arg1?) => any` → `number` | **pushref name resolution (Bug 7a)**: multi-arg function refs where object index expected |
| 58 | `(_rt, self) => void` → `GMLObject` | Same pushref issue, different target type |
| 55 | `(_rt, self, ...) => void` → `number` | Same pushref issue, various arities |
| 50 | `(_rt, self, arg0?) => any` → `number` | Same |
| 47 | `(_rt, self, arg0?) => void` → `number` | Same |
| 41 | `(_result: number) => void` → `number` | **pushref name resolution**: e.g. `collision_line(..., steam_inventory_result_destroy, ...)` — clearly wrong name for a collision object |
| 33 | `string` → `number` | Mixed: some genuine type mismatches, some asset name resolution |
| 28 | `(_rt, self, ..4 args) => void` → `number` | Same pushref issue |
| 24 | `(_rt, self, ..4 args) => void` → `GMLObject` | Same |
| 23 | `(font: number) => void` → `number` | Same pushref issue |
| 22 | `Record<string, any>` → `number` | Struct objects used as numeric IDs |
| 22 | `GMLObject \| null` → `number` | Instance refs used as numeric IDs |
| 21 | `() => void` → `number` | Same pushref issue |
| 17 | `(s: string) => number` → `number` | Same |
| 16 | `boolean` → `number` | Reverse of Bug 7b |
| 14 | `() => number` → `number` | Same pushref issue |
| ~400 | (remaining long tail) | Mix of pushref, type inference gaps, game author errors |

**~900 errors are pushref name resolution (Bug 7a)**, **~450 are number↔boolean (Bug 7b)**.

#### Bug 7a: FIXED — pushref type_tag=0 is OBJT, not FUNC (2026-02-26)

Root cause: pushref type_tag=0 was treated as FUNC chunk index, but UndertaleModTool confirms
type 0 = Object (OBJT) in all GameMaker versions. Fix: added OBJT names to `build_asset_ref_names`
at type_tag=0, removed special-case FUNC lookup, added Objects group to `generate_asset_ids`.
Result: TS2345 2043 → 846, total errors 4151 → 2112.

**Remaining risk:** pushref type_tag mapping is **version-dependent** (UndertaleModTool
`AdaptAssetType`). Our code uses the GM 2024.4+ layout. Pre-2024.4 games have a different
mapping (e.g. type 4=Background instead of Path, type 6=Script instead of Font, type 8=Timeline
instead of Shader, type 10=Shader instead of AnimCurve). Types 0–3 are the same in both versions.
Need version detection + dual mapping for pre-2024.4 games. See `lib.rs` comment on
`build_asset_ref_names` for full table.

#### Bug 7b: FIXED — GML number↔boolean calling convention (2026-02-26)

Root cause: GML has no boolean type — `0`/`1` used everywhere for boolean params. The IR
represented these as `Int(0)`/`Int(1)` but the constraint solver skipped non-Dynamic values.

Fix: new `IntToBoolPromotion` transform pass that identifies values demanded as Bool (via
`external_function_sigs` param types, `BrIf` conditions, `Not` operands, internal function
sig param types) and traces backward through SSA. If all leaves are Int(0/1)/Bool, the
chain is promoted to Bool. Also subsumes `BoolLiteralReturn` (return type inference).
Result: TS2345 846 → 325, total errors 2112 → 879.

#### Bug 7b2: FIXED — IntToBoolPromotion bare-exit + SetField gaps (2026-02-27)

Two bugs in `IntToBoolPromotion`:
1. `infer_bool_return` set return type to `Bool` even when the function had `Op::Return(None)`
   (bare `exit;` paths), causing 121 TS2322 `undefined → boolean` errors.
2. `collect_bool_demands` didn't check `SetField` operations, so `this.persistent = 1` stayed
   as `number` even though the field was `Type::Bool`, causing ~12 TS2322 `boolean → number` errors.
Result: TS2322 319 → 187, total errors 879 → 743.

#### Bug 7e: HIGH PRIORITY — Object indices exposed as bare numbers (176 TS2345)

All 176 `number → GMLObject` errors are at `instance_destroy(objName)` call sites. The object
constants (from pushref type_tag=0 OBJT) are declared as `number` in `asset_ids.d.ts`. GML's
`instance_destroy` accepts both instances and object indices.

**Design decision:** Object indices should NOT be exposed as bare numbers. Passing numbers
around instead of objects is bad for maintainability and type safety. Instead:
- [ ] Pushref OBJT references should resolve to class references (constructor functions), not numeric indices
- [ ] Runtime functions (`instance_destroy`, `instance_create_depth`, `instance_exists`,
  `instance_find`, `instance_nearest`, etc.) should accept class constructors, not numeric indices
- [ ] `asset_ids.d.ts` object entries should be typed as class refs, not `number`
- [ ] The runtime internally can still use numeric indices, but the external API hides them

This is a significant refactor touching: pushref resolution in translator, asset_ids generation,
runtime function signatures, and emitter import generation for object classes.

#### Bug 7c: Unreachable code after return/continue (207 TS7027)

Two distinct patterns:

1. **`break` after `return`/`continue` in switch cases** — The structurizer/emitter emits both
   a `continue`/`return` AND a trailing `break` for the same switch case. The `break` is dead code.
   Example: `case 2: ... continue; break;` — the `break` is unreachable. This is a code quality
   bug in the switch-case emitter: when a case body ends with `return`/`continue`, the `break`
   should be suppressed.

2. **`return 0 as any;` after all-paths-return functions** — The `ends_with_terminal` fallback
   adds a trailing return to satisfy TS2366, but when all switch/if branches already return,
   it's unreachable. This is cosmetic — the trailing return is a safety net, not a semantic error.

Fix for pattern 1: in the switch-case emitter, check whether the last statement in a case body
is a `return`, `continue`, or `break`, and suppress the auto-generated `break` if so.

#### Bug 7d: Undeclared variables — `vNNN` and named vars (17 TS2304)

Three distinct sub-patterns:

1. **SSA register names in ternary chains** (10 errors) — `v33`, `v29`, `v12`, `v14`, `v418`, `v40`,
   `v247`, `v264`, `v4`, `v2` appear in complex conditional expressions. The structurizer creates
   ternary expressions that reference SSA values from different CFG branches, but those values were
   never declared in the emitted scope. Example: `chance(0.05 * ...) ? v33 : (chance(0.5 * ...) ?
   ... : v29)` — `v33` and `v29` are values from predecessor blocks that weren't properly hoisted.
   Root cause: the structurizer's ternary/select lowering doesn't ensure all referenced values have
   declarations in the enclosing scope.

2. **`spd` in with-body** (2 errors, Barnacle.ts:142-143) — `lengthdir_x(spd, d)` where `spd` was
   never declared. The preceding line `random_range(2, 4)` is a bare expression (result discarded).
   In the original bytecode, this was likely `spd = random_range(2, 4)` but the linearizer dropped
   the assignment, leaving only the bare function call. The with-body closure then references `spd`
   which doesn't exist. Root cause: linearizer or translator lost the assignment target.

3. **`pass` in controller objects** (3 errors, OAnya*Controller.ts) — `pass` used as a variable
   but never declared. Same category as `spd` — assignment dropped by linearizer.

All 17 are **correctness bugs**, not cosmetic issues — the emitted code references variables
that don't exist, meaning these functions would crash at runtime.

#### TS7053: `int(x)[field]` indexing number (454 errors)

Pattern: `int(global.argument0)[AceDoll.instances[0]!.items] = 0` — `int()` returns `number`,
which can't be indexed. These are 2D array access patterns where the array variable is coerced
to `int` before being used as an array target. Root cause: the 2D array access codegen wraps
the array in `int()` (from a `coerce i32` instruction) when it should be accessing the array directly.

#### TS2339: Missing particle system properties (107 of 149)

`part_system_depth` (55) and `part_system_automatic_draw` (52) are not declared on the particle
system type. These are standard GML particle system properties that need to be added to the
GMLObject type definition or the particle system type.

Fixed in previous sessions (2026-02-24):
- TS2393 (14→0): Duplicate method blocks in runtime.ts removed
- TS2454 (204→0): `let v!: T;` definite-assignment assertion in ast_printer.rs
- TS2552 (150→1): Added ~50 missing runtime stubs + registered in runtime.json
- TS2366 (114→0): Added `ends_with_terminal()` + trailing `return 0 as any;` in ast_printer.rs
- TS2769 (417→0): Reclassified as TS2345 after adding type signatures to runtime.json
- TS2554 (512→7): Variadic scripts get rest param; game-script shadowing fix; ~80 signature corrections

**Highest-leverage next targets (by error reduction potential):**

- [x] **Bug 7a: pushref type_tag=0 is OBJT (~1200 TS2345 fixed)** — Fixed 2026-02-26.

- [x] **Bug 7b: number→boolean casting (~450 TS2345 fixed)** — Fixed 2026-02-26/27. `IntToBoolPromotion`
  pass + bare-exit fix + SetField demands.

- [ ] **TS7053 (454): int() wrapping array access targets** — The `coerce i32` instruction
  before a 2D array access should not wrap the array variable in `int()`. Fix in GML translator
  or as a rewrite pass.

- [ ] **TS7053 (518): struct field access on number-typed vars** — `v['fieldName']` where `v` is
  typed as `number` rather than `any`. Root cause: instance variables that hold object references
  (e.g. created by `instance_create_depth`) are typed as `number` (the raw instance ID) rather
  than as the class type. Fix requires GML instance ID type propagation (see Type System section).
  This was the original "102k error" regression from `7c4dc61` — it had briefly been worse but
  type inference improvements have since reduced it.

- [ ] **TS2339 (107): particle system properties** — Add `part_system_depth` and
  `part_system_automatic_draw` to the GMLObject type definition or particle system type.

- [ ] **TS2554 (5): Remaining runtime signature mismatches** — 5 remaining. All are either
  game-author errors (`gamepad_set_axis_deadzone` called with 2 args, `draw_roundrect_color` called
  with 7 args) or decompiler arg-count mismatches (functions that read fewer args than callers pass;
  e.g. `getScreenType`, `loadSetting`, `getPiecesWidth`/`getPiecesHeight`/`drawTextPieces`). The
  decompiler issue requires a new emit to fix (arg count determined by reads in function body).

- [x] **TS2454 (204→0): used before assigned** — Fixed with `let v!: T;` definite-assignment assertion in ast_printer.rs.

- [x] **TS2552 (150→1): missing runtime stubs** — Added ~50 stubs + registered in runtime.json.
  Remaining 1: `sarr` self-reference bug in with-body translation (see below).

- [ ] **TS2552 (1): `sarr` in with-body** — `sarr.length` on `_init.ts:6198` should be `self.sarr.length`.
  Adjacent references correctly use `self.sarr[...]` — one reference dropped the `self.` prefix.
  This is a with-body codegen bug; needs investigation in GML translator.

### 8. RunLoader::step stack underflow on Popz at 0x19c — Dead Estate translation error

- [ ] **`compute_block_stack_depths` uses `or_insert` — first path to reach a join point wins** —
  `RunLoader::step` fails with `0x19c: stack underflow on Popz`. The depth pre-computation walk
  sets `terminated = true` on `Bt`/`Bf`, which is correct since fall-through is always a block
  start. But `or_insert` means if two converging paths have different stack depths, only the first
  recorded depth is used. If the true runtime path arrives with fewer items than the recorded depth,
  subsequent pops underflow. Investigation needs bytecode dump of RunLoader::step to see which
  join point has inconsistent depths. Consider: (a) asserting stack depth consistency at join
  points, (b) using a worklist-based depth propagation instead of linear walk.

### 9. Pushref type_tag mapping is version-dependent — wrong for pre-2024.4 games

`build_asset_ref_names` currently uses the GM 2024.4+ type_tag layout. Pre-2024.4 games use a
different mapping where types 4–13 are shuffled (e.g. 4=Background, 6=Script, 8=Timeline,
10=Shader). Types 0–3 (Object, Sprite, Sound, Room) are the same in both versions, so games
using only those assets are unaffected. Need:
- [ ] Version detection (check `IsVersionAtLeast(2024, 4)` equivalent — likely in GEN8 chunk)
- [ ] Dual mapping in `build_asset_ref_names` based on detected version
- [ ] Same dual mapping in `generate_asset_ids`

Reference: UndertaleModTool `AdaptAssetType` / `AdaptAssetTypeId` in `UndertaleCode.cs`.

### New game inventory

| Game | Source | Status |
|------|--------|--------|
| 10 Second Ninja X | `data.win` 134MB | ⚠️ emits (TS errors TBD) |
| 12 is Better Than 6 | `game.unx` 179MB | ⚠️ emits (TS errors TBD) |
| Cauldron | `data.win` 169MB | ❌ YYC |
| CookServeDelicious2 | `game.unx` 805MB | ❌ EOF parse error in CODE (same as Forager) |
| Dead Estate | `data.win` 192MB | ⚠️ 879 TS errors + 1 translation error (2026-02-26) |
| Downwell | `data.win` 27MB | ❌ TXTR external textures |
| Forager | `game.unx` 78MB | ❌ EOF parse error in CODE |
| Just Hit The Button | `data.win` 1MB | ✅ emits (TS errors TBD) |
| Max Manos | `data.win` 47MB | ⚠️ 2 TS errors (local var pop raw index) |
| Max Manos 2 | `data.win` 10MB | ⚠️ 4 TS errors (local var pop raw index) |
| MINDWAVE Demo | `data.win` 324MB | ⚠️ ~26k TS errors (runtime API gaps) |
| Momodora RUtM | `.exe` 36MB | ❌ PE-embedded FORM |
| Nova Drift | `data.win` 415MB | ❌ YYC |
| Nubby's Number Factory | `data.win` 66MB | ⚠️ ~77k TS errors (runtime API gaps) |
| Risk of Rain | `game.unx` 34MB | ❌ YYC (empty CODE chunk) |
| Rocket Rats | `data.win` 2MB | ❌ YYC |
| Schism | `data.win` 77MB | ⚠️ emits (TS errors TBD) |
| Shelldiver | `data.win` 2MB | ❌ YYC |
| Soulknight Survivor | `data.win` 35MB | ❌ YYC |
| VA-11 HALL-A | `game.unx` 212MB | ⚠️ emits (TS errors TBD) |

---

## GameMaker — Runtime Platform Layer (HIGH PRIORITY)

The GameMaker runtime has several major API families that need platform-layer implementations:

### Audio (`platform/audio.ts`)
All `audio_*` functions are currently unimplemented and throw. Audio belongs in the platform
layer per the three-layer architecture. Needs:
- `platform/audio.ts` — Web Audio API implementation (AudioContext, AudioBufferSourceNode)
- Wire audio asset loading from `GameConfig` (asset table → audio buffer)
- `GameRuntime.audio_play_sound` → delegates to platform audio
- `GameRuntime.audio_is_playing` / `audio_stop_sound` / `audio_stop_all` / etc.

### Surfaces (`platform/surface.ts` or in draw layer)
`surface_*` / `draw_surface*` require off-screen rendering via WebGL or OffscreenCanvas.
Currently throw. Dead Estate uses surfaces extensively for post-processing effects.

### Shaders / GPU State
`shader_*` / `gpu_*` require WebGL shader compilation and state management. Currently throw.
Dead Estate uses shaders for visual effects (fog, color grading, etc.).

### Particle System
`part_*` require a real particle simulation system. Currently throw.
Dead Estate uses particles for effects (dust, sparks, etc.).

### Import side: free-function and asset references
`loadAnyaDataExt`, `AnyaSticker2A`, etc. appear as bare TS2304 names because the import
generator only adds imports for `Op::Call` callee positions — not for function references
used as values (via `@@pushref@@` / `GlobalRef`). Fix: scan all `GlobalRef` / `JsExpr::Var`
nodes in the emitted function body and add any that resolve to `_init.ts` exports or
asset-table names to the file's import set.

## GameMaker Frontend

### GML `with` Statement Bugs — FIXED (2026-02-22)

Discovered via Bounty reference comparison (2026-02-22). The GML `with(obj) { ... }` statement
had two distinct bugs in the frontend translator, both now fixed:

- [x] **`with` callback uses outer `self` instead of iterated instance** — Fixed by adding
  `_withSelf: any` parameter to the arrow function and replacing all `JsExpr::This` in the
  body with `JsExpr::Var("_withSelf")` via `replace_this_in_stmts` in `rewrites/gamemaker.rs`.
  Commit: 3ba5814. Note: field accesses on the iterated instance that went through `v0`
  (IR-level) are correctly replaced because `v0` (named "self") emits as TypeScript `this`,
  which is then caught by the replacement.

- [x] **Post-`with` code not captured** — Fixed by changing `PopEnv`'s loop-back case from
  `resolve_branch_target` (emits back-edge to body, leaving fall-through unreachable) to
  `resolve_fallthrough` (falls through to continuation). The GML iteration is handled entirely
  by `withInstances()` in the runtime — the IR doesn't need to model the loop. Both the
  loop-back case (sentinel >= 0) and the break-out case (sentinel < 0) now fall through.
  Commit: 7832b3a.

**Design debt — RESOLVED**: The `withBegin`/`withEnd` bracket anti-pattern turned out to already be fixed — the GML frontend already emitted `Op::MakeClosure` + `withInstances` directly (not `withBegin`/`withEnd`). `collapse_with_blocks` was dead code only exercised by its own tests. Deleted `collapse_with_blocks`, `try_extract_with_loop_body`, `make_with_instances`, `replace_this_in_stmts`, `replace_this_in_expr`, the `withBegin` canonicalization map entry, and all associated tests.

### GML Short-Circuit AND Condition Bug — FIXED (2026-02-22)

- [x] **`if (a && b && c)` emits ternary `a ? b : c` instead of conjunction** — Root cause was
  `GmlLogicalOpNormalize` incorrectly normalizing the shared else-block when multiple BrIf
  instructions share the same else target. The pass replaced `br merge(const_0)` with
  `br merge(cond_outer)`, making the condition `(cond_inner) ? (cond_innermost) : cond_outer`
  instead of the correct falsy short-circuit. Fix: skip normalization when the trivial block has
  more than one BrIf predecessor (commit 41671f2). Output is now semantically correct:
  `(pressed === 2) ? (locked === 0) : 0` (equivalent to `pressed===2 && locked===0`).
  The switch detection also fires correctly now, producing `switch(this.type)` blocks.

### GML 2D Array Compound-Assignment Bug — FIXED (commit b3db317)

Discovered via Bounty reference comparison (2026-02-22). Fixed 2026-02-22.

- [x] **2D array `+=` wrote `inventory[sum] = const` instead of `inventory[idx] = sum`** —
  Root cause: compound assignment uses the Dup pattern: `push dim2, push dim1, Dup, VARI-read,
  arithmetic, VARI-write`. After the read+arithmetic, the stack is `[dim2, dim1, new_value]`
  with new_value on top — opposite of simple assignment `[value, dim2, dim1]` with dim1 on top.
  Fix: added `compound_2d_pending` flag, set by `translate_push_variable` when originals remain
  after 2D VARI read (`stack.len() >= 2`). `translate_pop` uses reversed pop order when set.
  Output: `self.inventory[(int(i)*32000)+1] += argument1` (correct compound assignment).

### Other GML Logic Bugs Found in Bounty Comparison

- ~~**Missing `do_gangbang_encounter` function**~~ — Confirmed custom code added after the port
  (`~/git/bounty/scripts/main6.js`). Not a missing translation. N/A.

- [ ] **`save_game` missing INI writes** — The following fields are not written to the save file:
  `name`, all `a_*` appearance fields (`a_eye_color`, `a_skin_color`, `a_height`, `a_weight`,
  `a_hair_color`, `a_hair_length`, `a_hair_straightness`, `a_hair_style`, `a_other`, `a_racial`),
  `o_prostitution`, `o_self_defense`. Reference is `~/git/bounty/scripts/main.js:save_game`.

- [ ] **`roll_d6` hardcoded Y-coordinate** — Reference: `y = 480 - 20 * size` (scales with
  dice size). Emitted: hardcoded `460`. Dice render at wrong Y position when dice size ≠ 1.

- [ ] **Stats::create broken advantages/inventory loop** — The loop initializer in create()
  emits `i = i < 20` (assigns boolean comparison result to counter) instead of the loop
  running properly. All 20 advantages slots and inventory entries may not initialize correctly.
  Also missing: `negotiate_*` fields (6 bool fields), `o_prostitution`, `o_self_defense`,
  and all 12 `a_*` appearance fields. Also missing `user0()`/`user1()` events and debug
  `keypress70`/`keypress72`/`keypress76`/`keypress77` handlers.

- [ ] **Location::create scroll condition inverted** — Reference: `if (i > 440)` sets
  `scroll = true` and skips destroying LocationScroll; `else` destroys LocationScroll.
  Emitted: condition is `<= 440` (inverted) and `scroll` is set to 1 unconditionally after
  the branch. Also text width for scale computation is 380 instead of 390.

- [ ] **Location::step/draw debug check missing** — Reference: `if (instance_exists(obj_location_scroll) && obj_stats.debug)` before drawing debug overlay. Emitted: missing the `&& obj_stats.debug` guard.

- [ ] **Cross-object writes inside `with`-bodies emitted to `_self` instead of outer self** —
  When a `with(OtherObj)` body writes to the enclosing instance (e.g. `obj_race_reader.advantage = self.number`),
  the emitted code incorrectly assigns to `_self.advantage` (the iterated instance) instead of
  `outerSelf.advantage` (the captured outer self). Root cause: the with-body closure translator
  doesn't distinguish `InstanceType::Other` (outer self) from `InstanceType::Own/_self` (iterated
  instance). Fix: inside `translate_with_body`, capture the outer `self` parameter as an extra
  capture, and emit writes with `instance == other / outer-object-id` as `captured_self.field`
  rather than `_self.field`.
  Affects: `EquipReader::step` (`_self.type = _self.type` → should be `this.type = _self.type`),
  `RaceReader::step` (`_self.advantage = _self.number`), `OptionsReader::step` (`_self.type = _self.number`),
  `MainLocMain::step` (`_self.no_use = 1`).

- [ ] **GeneralGotoMain::step missing debug mode cleanup** — Reference sets `obj_stats.debug = false`
  and deletes `obj_stats.alarm[0]` when navigating from the debug room. Both ops are absent.
  Additionally TS has an extra `save_config()` call not in the reference.

- [ ] **`a && b` compiled as `a ? b : 0` ternary instead of logical AND** — When GML bytecode
  encodes `a && b` as a diamond CFG (eval `a`; if false skip; eval `b`; merge), the structurizer
  sometimes emits `a ? b : 0` instead of `a && b`. The two are semantically equivalent in a
  boolean `if`-condition context (both falsy when `a` is false), but the ternary form is harder
  to read and should ideally be normalized to `&&`. The `GmlLogicalOpNormalize` pass handles
  the simple 2-operand case but misses the 3+ operand case (two consecutive BrIf instructions
  where the second guard shares the else-target of the first). A post-structurize AST pass
  should detect `(cond ? inner_cond : 0)` / `(cond ? inner_cond : false)` and rewrite to
  `cond && inner_cond`. Affected: `StoreButton`, `TravelButton`, `TravelMain` in Bounty.
  See also: `GmlLogicalOpNormalize` in `rewrites/gamemaker.rs` commit 41671f2.

### Boolean / Short-Circuit Detection (open)

- [ ] **Numeric booleans: `=== 1` / `=== 0` instead of boolean tests** —
  GML compiles `if (self.active)` as `push self.active; pushi 1; cmp.eq; bf`.
  Requires heuristics to identify fields only assigned 0/1/true/false across
  all functions, then replace `=== 1` with bare test and `=== 0` with `!`.

- [ ] **Enum detection (string and numeric)** — Many GML games use string
  constants as enum values. Could extract into `const` objects during type
  inference. The reference code uses `Advantages.none`, `MouseButtons.pressed`,
  etc., showing these were originally named constants.

### Missing Runtime Functions

All previously listed functions have been implemented. Check `function_modules`
in runtime.json for any newly referenced but unimplemented functions.

## IR Architecture

- [ ] **Closure support with variable capture** — The IR has
  `MethodKind::Closure` for marking closures, and the backend can inline them
  as `JsExpr::ArrowFunction`, but there's no mechanism for closures to capture
  variables from the enclosing scope. Currently Twine arrows work because all
  state is runtime-managed (`State.get`/`State.set`), so there's nothing to
  capture. Proper closure support would need:
  - `Op::MakeClosure { func_name, captures: Vec<ValueId> }` — creates a
    closure binding captured values from the current scope
  - `Function.captures: Vec<CaptureInfo>` — records which parent values
    each closure parameter maps to
  - Backend: when inlining, strip captured params and use parent variable
    names directly (JS native closures handle the rest)
  - Touches every transform pass (new Op variant requires match arms)

## Twine Frontend

- [x] **Use a proper HTML parser for extraction** — Replaced `extract_tagged_blocks` and `extract_format_css` with `extract_script_style_blocks`, a tokenizer-based implementation using `html5ever`. Returns `TokenSinkResult::RawData(ScriptData/Rawtext)` on script/style start tags, so the tokenizer handles raw content exactly as a browser would — no manual closing-tag search, no cross-element contamination.

- [ ] **Passage rendering strategy** — Implement `passage_rendering`
  manifest option (`auto`/`compiled`/`wikifier`). In `wikifier` mode,
  Rust emitter emits passage source as string constants instead of compiled
  functions. `auto` mode scans scripts for `Wikifier.Parser` references.

### SugarCube Runtime Errors (DOL)

Two runtime errors block DOL (Degrees of Lewdity) from running:

- [ ] **User script eval failure** — The emitted `__user_script_0` is ~45k
  lines compiled into a single `new Function(...)` call. If this eval fails
  (SyntaxError or runtime error), all subsequent `window.X = X` assignments
  inside it are lost. `resolve("allClothesSetup")` returns `undefined` because
  `allClothesSetup` was defined inside the eval but the eval died before
  reaching the `window.allClothesSetup = allClothesSetup` assignment. The
  improved error reporting in `evalCode()` (split SyntaxError vs runtime
  errors, logged to console) should surface the root cause when run in the
  browser — **needs testing**.
  - Cascading failure: `get("NPCNameList")` returns `undefined` in
    `widget_npcPregnancyUpdater` because StoryInit didn't complete.
  - Potential fix directions: split user scripts into smaller eval chunks,
    or identify the specific code pattern that causes the eval to fail.

- [x] **Un-parenthesized single-param arrow parsing** — Fixed in `06a0dce`.
  The parser now handles `x => expr` in addition to `(x) => expr`. DOL:
  596 → 817 inline arrows (+221 previously broken).

### SugarCube Translator Bugs

- [ ] **CRITICAL: eliminate per-function runtime service aliases** — Every exported function
  currently emits 7 `const SugarCube_X = _rt.X;` aliasing lines at the top (DOM, Engine,
  Input, Navigation, Output, State, Widget). These aliases are unnecessary — all call sites
  should use `_rt.State`, `_rt.Output`, etc. directly. The aliases add noise to every function,
  make diffs harder to read, and cause TypeScript to allocate extra locals for every call frame.
  Fix: emit `_rt.State.get(...)`, `_rt.Output.break()`, etc. at every call site; drop the
  alias-emission block entirely. This is a backend emitter change (TypeScript backend, `emit.rs`
  or the SugarCube-specific emitter).

- [ ] **`_rt` parameter threading in trc (1935 errors)** — Passage functions are emitted as
  `(_rt: SugarCubeRuntime) => void` but some callback sites (e.g. link targets, widget calls)
  pass them without the `_rt` argument, producing TS2345 "not assignable to `() => void`".
  Root cause: the translator threads `_rt` through passage closures but the call-site types
  don't reflect it. Either passage callbacks must always accept `_rt` consistently, or the
  closure form must be `() => void` with `_rt` captured from outer scope.

- [x] **`radiobutton` emits 1 arg instead of 2 in trc** — Fixed in `3079adb`. Input macros
  (`textbox`, `textarea`, `numberbox`, `checkbox`, `radiobutton`) now use `split_case_values()`
  → `MacroArgs::CaseValues` so each arg is a discrete token. Also fixed `checkbox` arg order
  (was `checkedValue, uncheckedValue`; SugarCube is `uncheckedValue, checkedValue`) and added
  `...flags` rest param to handle `checked`/`autocheck` bareword flags.

### SugarCube Type Emission Bugs (DoL)

- **`never[]` errors in DoL (game author errors)** — `[][_namecontroller] = x` (writing
  to a throw-away empty array literal), `[].pushUnique(...)`, `[].pluck(...)`, and
  `traits: never[]` from TypeScript's union inference on inline push args with empty
  `traits: []` in objects with inconsistent shapes. All are game author bugs in the original
  SugarCube source — reincarnate faithfully reproduces them. No fix warranted.

- [x] **`Property '0'/'1' does not exist on type '{}'` — fixed** — `Record<string, unknown>`
  object literal annotation made field reads return `unknown`; null-check narrowing then
  produced `{}`, and `{}[numeric]` is TS7053. Fixed by annotating object literals as
  `Record<string, any>` instead of `Record<string, unknown>`. Game objects are inherently
  dynamic — `any` is the accurate annotation when no source-level type info exists.

- [x] **jQuery `@types` missing in DoL runtime** — Already in `dev_dependencies` in `runtime.json`; was a stale entry.

### SugarCube Remaining Stubs

- [ ] **Scripting.parse()** — Returns code unchanged (identity function).
- [ ] **L10n.get()** — Returns key as-is. Low impact.
- [ ] **SimpleAudio.select()** — AudioRunner returned is a no-op stub.
- [ ] **Engine.forward()** — No-op (deprecated in SugarCube v2).
- [x] **SCEngine.clone()** — Fixed in `8e17415`. `clone(x)` now rewrites to standalone pure function import; was emitted as `SCEngine.clone(x)` method call. Eliminated 643 TS2339 errors.
- [x] **SCEngine.iterate() / iterator_has_next() / iterator_next_value() / iterator_next_key()** — Fixed in `8e17415`. Same fix; standalone pure function imports. Eliminated ~969 TS2339 errors.
- **TS2447 `|` on booleans** (1610 errors in DoL) — DoL game authors use `|` where they meant `||`: `(v === "Kylar") | (v === "Bailey")`. This is a **game author error** in the original source. Reincarnate correctly emits the game's code. These errors are expected and no fix is appropriate — changing `|` to `||` would alter semantics, and any other suppression would hide a real bug in the game.

### SugarCube oxc Parse Errors

Status: DoL 290→4, TRC 974→0. Fixed: default-to-Raw, LinkTarget, preprocessor context
(identifiers, property names, object literals), HTML entity decoding (html-escape crate),
UTF-8 preservation in preprocessor, template literal `${...}` preprocessing, `<<run>>`
statement parsing, trailing comma stripping in case values.

- [ ] **`settings.x` / `setup.x` in `<<case>>` args** — SugarCube's `parseArgs()`
  evaluates `settings.x` and `setup.x` barewords via `evalTwineScript()`, not as string
  constants. Currently falls through to `Bareword` in `classify_case_token`.

- [ ] **`[[...]]` SquareBracket token in `<<case>>` args** — SugarCube converts `[[text|passage]]`
  in arg position to a link object. Currently not handled by `classify_case_token` at all.
  Rare in practice as a case value (object identity comparison with `===` would never match).

- [ ] **`parseArgs()` semantics for media/audio macros** — When `<<audio>>`, `<<playlist>>`,
  `<<cacheaudio>>`, `<<track>>` etc. are eventually lowered to the platform audio layer, their
  args must use `parseArgs()` token semantics (discrete values: track ID, command string, volume)
  not the full-expression path. Currently fall to `Raw` which is safe but means audio is not
  preserved in output.

- [x] **CRITICAL: Extract `Macro.add()` from JavaScript passages** — Implemented in
  `sugarcube/custom_macros.rs`. Scanner extracts block/self-closing kind (`tags:` property)
  and `skipArgs: true` semantics. Registry built from user scripts before passage parsing.
  Custom entries shadow built-ins (DoL redefines `button`, `link`). Dynamic registrations
  (variable names) skipped silently. No `<<switch>>` override exists in DoL.

- [x] **CRITICAL: `assets/styles/user_0.css` for DoL contains JavaScript** — Fixed in `57e02a9`.
  `extract_tagged_blocks` preferred `</script>` unconditionally over `</style>`, so the user
  stylesheet block captured 1.5 MB of JS. Fix: pick whichever closing tag appears first.

### Harlowe Correctness Bugs

- [ ] **`(sorted: via lambda)` — `$dm's (it)` inside via lambda** — e.g.
  `(sorted: via $players's (it), ...(dm-names: $players))` uses `(it)` as a
  property key inside a `via` lambda. Our translator lowers `(it)` as a variable
  reference. Need to verify `$dm's (it)` translates correctly in lambda context
  (should produce `get_property(dm, it)`).

- [x] **`(sorted:)` via-lambda `its X` and implicit any (rogue-time-agent, 1 error)** —
  Two bugs: (1) `its year` was parsed as `Ident("its")` → `const_string("its")` instead
  of `Possessive(It, "year")`. Fixed in `expr.rs`: `its` desugars to `it's`. (2) `sorted`
  was in `is_predicate_op` causing `infer_param_types: true` on the via-lambda, but
  `Collections.sorted` takes `...args: unknown[]` so TS can't infer the item type → TS7006.
  Fixed by removing `sorted` from `is_predicate_op`. rogue-time-agent: 1 → 0 errors.

- [ ] **Unreachable code after `(goto:)`/`(stop:)` (equivalent-exchange, 8 errors)** —
  Code emitted after `goto`/`stop` IR returns is flagged as TS7027 unreachable by
  TypeScript. The structurizer or emitter should suppress statements following a
  guaranteed-terminating call. Alternatively, the frontend should mark blocks after
  unconditional goto/stop as dead and DCE should prune them.

- [ ] **TS2304 `vNNNN` across Dispatch case boundaries (DoL, 1501 errors)** —
  Values (e.g., `iterate()` results) are declared in one `case {}` block of a
  Dispatch/switch and used in a sibling `case {}`. TypeScript sees them as
  undeclared because each case is a separate block scope. Repro: `v5242` declared in
  case 742 (`const v5242 = iterate(...)`) used in case 745 (`iterator_next_value(v5242)`).
  Fix: during Dispatch emission, detect values defined in one case but referenced in
  another, and hoist their declarations (without init, with `Assign` at definition site)
  to before the switch statement — same approach as `block_params_preamble()`.

- [ ] **Unresolved temp vars / used-before-assigned (equivalent-exchange, DoL)** —
  Two related TS errors from temp var scoping gaps:
  - **TS2304 "Cannot find name `_x`"** — `_enemycockz`/`_cockz` (equivalent-exchange),
    `_swarmamounts`/`_arrayClothes`/`_clothing` (DoL): temp vars referenced in passage
    bodies but not declared in scope at the reference site. Likely a closure capture
    ordering problem — the lambda captures the var before its alloc is visible.
    Also: `_slot`/`_tentacleColour`/`_ii`/`_outfit`/`_kylar`/`__part` etc. (21 errors).
  - **Root cause for `_args = State.get("_args")` without `let` (TS2304)** —
    When a single-store alloc has 2+ loads (e.g. `_args` loaded in both a display
    expression and an if condition), after single-store promotion the stored value (v1)
    gets use_count >= 2. `emit_or_inline(v1, count≥2)` takes the `Assign` path +
    adds to `referenced_block_params`. But v1 is NOT a block param, so
    `collect_block_param_decls` never emits `let _args;`. Fix: in `emit_or_inline`,
    when count >= 2 AND value has a name AND is not yet in referenced_block_params,
    emit `VarDecl { name, init: Some(expr) }` (not bare Assign). Track that the
    first emission was a VarDecl so subsequent uses just reference the name.
  - **TS2454 "Variable `_x` used before being assigned"** — `_hooks`/`_them` (DoL):
    var IS declared (hoisted by `hoist_allocs()`) but has no initializer, and a code
    path reaches the use before the `(set:)` assignment. Fix: initialize hoisted allocs
    to `undefined` (or the Harlowe undefined sentinel) so no path is "unassigned".

- **TS2872 in artifact (game author error)** — `(set: $cond to (cond: ...))` where
  `(cond:)` is used as a value macro. This is a game author misuse of `(cond:)` —
  Harlowe's `(cond:)` is a value macro that picks between two values, not a
  condition object. Reincarnate correctly emits the call; the TS error reflects the
  type mismatch in the source. No fix warranted.

- **TS2363/TS2367 game author comparison errors** — Boolean or type-mismatched operand
  in a comparison that TypeScript rejects. Observed in the-national-pokedexxx (TS2363)
  and equivalent-exchange line 44703 (TS2367: `boolean` vs `number`). Game author errors
  in the original source. No fix warranted.

- [x] **Temp variable block-scope leak** — Fixed in `62bcd79`. Harlowe temp vars (`_var`) have
  passage-level scope, but `Op::Alloc` was placed inside nested blocks producing block-scoped `let`.
  Fix: `Function::hoist_allocs()` moves all allocs to the entry block before structurize. Resolved
  `_twelve` (artifact), `_sex`/`_victory` (equivalent-exchange).

- [x] **Backtick verbatim spans not handled in parser** — Fixed in `cfb1796`.
  Parser now handles backtick-delimited verbatim spans; `]` inside backticks
  no longer closes hooks. arceus-garden: 203 → 3 unknown_macro calls.

- [x] **`is ... or ...` shorthand not expanded** — Fixed in `d4bcf29`.
  `maybe_distribute_comparison()` wraps bare values in matching comparison
  when `or`/`and` follows `is`/`is not`.

- [x] **Changer `+` composition emits JS `+`** — Fixed in `155af06`.
  All Harlowe `+` routed through `Harlowe.Engine.plus()` runtime call
  which dispatches by type (changers, arrays, datamaps, numbers).

- [x] **`it` in `(set:)` context** — Fixed in `5223050`. Confirmed via
  Harlowe source (`setIt()` calls `.get()` on target VarRef): `it` inside
  `(set:)` refers to the target variable's current value. `set_target` field
  on TranslateCtx substitutes `It` with a read of the target variable.
  arceus-garden: 244 `get_it()` calls → 0.

- [x] **Missing macros** — `(obviously)` was prose misparsed as macro (parser
  fix: require colon). `(forget-undos:)` implemented in engine.ts + state.ts.

### Harlowe Output Quality

- [x] **Text coalescing** — Fixed in `66ade45`. New `coalesce_text_calls`
  AST pass merges adjacent string-literal text() calls into a single call.
  arceus-garden: 2,974 → 1,874 text() calls (-37%), 16,329 → 15,429 lines.

### Harlowe Performance

- [x] **O(n²) AST pass regression (12x → 2.4x)** — The declarative content
  tree refactor created more AST statements, exposing O(n²) behavior in
  `fold_single_use_consts` and `narrow_var_scope`. Both used a
  one-at-a-time loop pattern (scan body per candidate). Fixed with batch
  passes that precompute variable reference counts in one O(n) pass.
  arceus-garden: 1.1s → 0.22s (release). Remaining 2.4x vs baseline is
  from higher statement count (content-as-values), not algorithmic.

### Harlowe DOM Fidelity — Missing `tw-*` Custom Elements

Format CSS is now extracted from `<style title="Twine CSS">` in the story HTML
and emitted as `assets/styles/format_harlowe.css`. Scaffold uses `<tw-story>`
root for Harlowe (SugarCube keeps `<div id="passages">`).

**Structural (done):**
- [x] `<tw-story>` — root container (scaffold)
- [x] `<tw-passage>` — wraps current passage (navigation.ts, `tags` attribute)
- [x] `<tw-sidebar>` — sidebar with undo/redo (navigation.ts)
- [x] `<tw-icon>` — clickable sidebar icons (navigation.ts)

**Content (done):**
- [x] `<tw-link>` — clickable links (context.ts)
- [x] `<tw-broken-link>` — links to nonexistent passages (context.ts)
- [x] `<tw-hook>` — changer-styled content wrapper (context.ts `styled()`)
- [x] `<tw-expression>` — macro output wrapper (context.ts `live()`)
- [x] `<tw-collapsed>` — collapsed whitespace sections (context.ts `collapse()`)
- [x] `<tw-align>` — aligned content (context.ts `align()`)
- [x] `<tw-consecutive-br>` — consecutive line break normalization (context.ts `br()`)
- [x] `<tw-include>` — embedded passage content via `(display:)` (context.ts)

**Content (done — macro support added, untested against real games):**
- [x] `<tw-verbatim>` — raw/verbatim text via `(verbatim:)` changer (context.ts)
- [x] `<tw-enchantment>` — enchanted element wrapper via `(enchant:)`/`(enchant-in:)` (engine.ts)
- [x] `<tw-columns>` / `<tw-column>` — column layout via `(columns:)`/`(column:)` (engine.ts, context.ts)
- [x] `<tw-meter>` — progress meter via `(meter:)` (engine.ts)
- [x] `<tw-colour>` — color swatch display in `printVal` (context.ts)

**Dialog system (done — untested against real games):**
- [x] `<tw-dialog>` — modal dialog container via `(dialog:)` (engine.ts)
- [x] `<tw-backdrop>` — dialog backdrop overlay (engine.ts)
- [x] `<tw-dialog-links>` — dialog close link container (engine.ts)

**Content (done — transition wrapping):**
- [x] `<tw-transition-container>` — wraps hook children when `(transition:)` changer is applied (context.ts)

**Testing:** None of the 21 current test games use these macros. To verify
correctness, find a Harlowe game on IFDB/itch.io that exercises these macros
and add it to `~/reincarnate/twine/`.

**CSS animation keyframes:** Now extracted from format CSS (no runtime injection).

**Error/debug (skip):** `tw-error`, `tw-debugger`, `tw-eval-*`, etc.

**Data (extraction input only):** `tw-storydata`, `tw-passagedata`, `tw-tag`

### Harlowe Phase 2 (Advanced Features)

- [x] **`(for: each _item, ...$arr)[hook]`** — Loop lowering (done)
- [x] **`(live: Ns)[hook]` + `(stop:)`** — Timed interval (IR lowering, runtime `live()`/`stopLive`, navigation cleanup all done; regression test present)
- [x] **`(click: ?hook)[hook]`** — Event handler targeting named hooks (done — `click_macro` in engine.ts handles selector + hook callback)
- [x] **Collection constructors** — `(a:)`, `(dm:)`, `(ds:)` (done — runtime + frontend complete)
- [x] **Collection operators** — `contains`, `is in`, `'s`, `of` with full Harlowe semantics (done)
- [x] **Lambda expressions in collection ops** — `each _x where _x > 5` as predicate callback for
  `(find:)`, `(some-pass:)`, `(all-pass:)`, `(none-pass:)` etc. (done — `build_lambda_callback`)
- [x] **Lambda `via` expressions + `(sorted-by:)` + fold lambdas** — `via expr` tokenized and lowered
  as `ViaLambda`; `each _x making _acc via expr` parsed as `FoldLambda` for `(folded:)`;
  `(sorted-by:)` added to frontend + runtime; `interlaced`, `repeated`, `folded` runtime functions added.
- [x] **Dynamic macro calls** — `($varName: args)` expression-position calls; `(macro: type _p, [body])`
  closures; `ExprKind::DynCall`; `ExprKind::MacroDef`; `extract_macro_definition()` in lexer;
  `parse_macro_definition_args()` in parser; `build_macro_closure()` in translator.
- [x] **Changer composition with `+`** — `(color: red) + (text-style: "bold")` (fixed in `155af06`)
- [x] **`(save-game:)` / `(load-game:)`** — Save integration (basic runtime done; already implemented)
- [x] **`(replace:)`, `(show:)`, `(hide:)`** — DOM manipulation hooks (done)
- [x] **`(meter:)`, `(dialog:)`, `(dropdown:)`, `(checkbox:)`, `(input-box:)`** — UI macros (implemented)
- [x] **`(verbatim:)[...]`** — Raw text pass-through via `<tw-verbatim>` element
- [x] **`(enchant:)` / `(enchant-in:)`** — Apply changers to matching elements via `<tw-enchantment>`
- [x] **Named hooks** — `|name>[hook content]` and `?name` hook references (done)
- [x] **Complex `'s` possessive chains** — `$obj's (str-nth: $idx)` nested macro in possessive
  (already handled: `parse_prefix` falls to `try_parse_inline_macro` after `'s`)



