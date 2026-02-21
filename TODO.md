# TODO

Completed items archived in [COMPLETED.md](COMPLETED.md).

Per-engine roadmaps (gaps, runtime coverage, open work) live in [`docs/targets/`](docs/targets/). This file tracks in-flight and near-term work across all active engines.

## Planned Engines (not yet started)

Full roadmaps in `docs/targets/<engine>.md`. Summary of where each stands:

| Engine | Blocker / next step |
|--------|---------------------|
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

## Runtime Type Audit

- [ ] **Periodic `git blame` audit of runtime type signatures** — Review Harlowe/SugarCube runtime files
  (`engine.ts`, `context.ts`, `state.ts`, etc.) for any type signatures that were widened (e.g. `string`
  → `any`, required param made optional) to silence TypeScript errors from game code. Such changes hide
  real bugs. The rule: never widen types to accommodate buggy game code — TypeScript catching game author
  mistakes is correct behavior. Audit files: `runtime/twine/ts/harlowe/`, `runtime/twine/ts/sugarcube/`.

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


## Future

- [ ] **IR-level closure representation** — `MethodKind::Closure` exists as a tag but captures are implicit (lexical scoping in TS handles it today). Design: `Op::MakeClosure { func: FuncId, captures: Vec<(ValueId, CaptureMode)> }` with `CaptureMode` = `ByValue | ByRef`; closure function gets capture params prepended to its signature. Also needed for correct DCE (currently can't see that a closure body keeps an outer-scope value live). **Prerequisite for Rust backend AND for correct SugarCube `<<capture>>` semantics** — without explicit capture lists, the SugarCube frontend's lift-then-inline round-trip can't model captured temp vars (outer-scope ValueIds are inaccessible from a lifted function). `<<capture _i>>` maps directly: snapshot `_i` → `ValueId`, any `Op::MakeClosure` inside the block lists it as a `ByValue` capture.
- [ ] Rust codegen backend (emit `.rs` files from typed IR — **blocked on multi-typed locals**)
- [ ] wgpu + winit renderer system implementation
- [ ] Web Audio system implementation
- [ ] Native binary decompilation (C/C++, DirectX games with no scripting layer) — requires disassembly + decompilation pipeline rather than bytecode decoding; far harder than any current target; no timeline

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

## GameMaker Frontend

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



