# TODO

Completed items archived in [COMPLETED.md](COMPLETED.md).

## End-to-End Regression Tests

- [ ] **Snapshot tests for both frontends** — No snapshot infrastructure yet.
  - **Flash**: 15 new vN identifiers regressed in `91fe86e` (MethodCall
    refactor). Pre-existing 5 vN (hasNext2 one-shot, split-path phi).
  - **GML**: Reference decompilation at `~/git/bounty/`.

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

## Future

- [ ] Rust codegen backend (emit `.rs` files from typed IR — **blocked on multi-typed locals**)
- [ ] wgpu + winit renderer system implementation
- [ ] Web Audio system implementation

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

- [ ] **Use a proper HTML parser for extraction** — `extract_tagged_blocks` uses manual string search which is fragile. The bug where `</script>` was preferred over an earlier `</style>` is one example of what hand-rolled HTML parsing gets wrong. Switch to `html5ever` or `scraper` crate for robust extraction of `<script id="...">` / `<style id="...">` blocks.

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

### SugarCube Remaining Stubs

- [ ] **Scripting.parse()** — Returns code unchanged (identity function).
- [ ] **L10n.get()** — Returns key as-is. Low impact.
- [ ] **SimpleAudio.select()** — AudioRunner returned is a no-op stub.
- [ ] **Engine.forward()** — No-op (deprecated in SugarCube v2).

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

- [ ] **`(for: each _item, ...$arr)[hook]`** — Loop lowering
- [ ] **`(live: Ns)[hook]` + `(stop:)`** — Timed interval (basic IR done, runtime impl present)
- [ ] **`(click: ?hook)[hook]`** — Event handler targeting named hooks
- [ ] **Collection constructors** — `(a:)`, `(dm:)`, `(ds:)` (runtime done, parser handles basic cases)
- [ ] **Collection operators** — `contains`, `is in`, `'s`, `of` with full Harlowe semantics
- [ ] **Lambda expressions** — `_x where _x > 5` syntax in parser/translator
- [x] **Changer composition with `+`** — `(color: red) + (text-style: "bold")` (fixed in `155af06`)
- [ ] **`(save-game:)` / `(load-game:)`** — Save integration (basic runtime done)
- [ ] **`(replace:)`, `(show:)`, `(hide:)`** — DOM manipulation hooks
- [x] **`(meter:)`, `(dialog:)`** — UI macros (implemented, `(dropdown:)` still open)
- [x] **`(verbatim:)[...]`** — Raw text pass-through via `<tw-verbatim>` element
- [x] **`(enchant:)` / `(enchant-in:)`** — Apply changers to matching elements via `<tw-enchantment>`
- [ ] **Named hooks** — `|name>[hook content]` and `?name` hook references
- [ ] **Complex `'s` possessive chains** — `$obj's (str-nth: $idx)` nested macro in possessive


