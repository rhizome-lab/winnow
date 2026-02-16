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

- [ ] **Inline closures** — Filter/map callbacks extracted as named function
  references instead of being inlined as arrow functions.
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

## Twine Frontend

- [ ] **Passage rendering strategy** — Implement `passage_rendering`
  manifest option (`auto`/`compiled`/`wikifier`). In `wikifier` mode,
  Rust emitter emits passage source as string constants instead of compiled
  functions. `auto` mode scans scripts for `Wikifier.Parser` references.

### SugarCube Remaining Stubs

- [ ] **Scripting.parse()** — Returns code unchanged (identity function).
- [ ] **L10n.get()** — Returns key as-is. Low impact.
- [ ] **SimpleAudio.select()** — AudioRunner returned is a no-op stub.
- [ ] **Engine.forward()** — No-op (deprecated in SugarCube v2).

### Harlowe Phase 2 (Advanced Features)

- [ ] **`(for: each _item, ...$arr)[hook]`** — Loop lowering
- [ ] **`(live: Ns)[hook]` + `(stop:)`** — Timed interval (basic IR done, runtime impl present)
- [ ] **`(click: ?hook)[hook]`** — Event handler targeting named hooks
- [ ] **Collection constructors** — `(a:)`, `(dm:)`, `(ds:)` (runtime done, parser handles basic cases)
- [ ] **Collection operators** — `contains`, `is in`, `'s`, `of` with full Harlowe semantics
- [ ] **Lambda expressions** — `_x where _x > 5` syntax in parser/translator
- [ ] **Changer composition with `+`** — `(color: red) + (text-style: "bold")`
- [ ] **`(save-game:)` / `(load-game:)`** — Save integration (basic runtime done)
- [ ] **`(replace:)`, `(show:)`, `(hide:)`** — DOM manipulation hooks
- [ ] **`(meter:)`, `(dialog:)`, `(dropdown:)`** — UI macros
- [ ] **`(verbatim:)[...]`** — Raw text pass-through (no macro processing)
- [ ] **`(enchant:)` / `(enchant-in:)`** — Apply changers to matching elements
- [ ] **Named hooks** — `|name>[hook content]` and `?name` hook references
- [ ] **Complex `'s` possessive chains** — `$obj's (str-nth: $idx)` nested macro in possessive

## AST Pass Design Gaps (from audit)

- [ ] **Unify variable reference counting** — `count_var_refs_in_stmt` (reads only),
  `stmt_references_var` (reads+writes), and `var_is_reassigned` (writes only) are three
  near-identical tree walks with subtly different semantics. 5+ bugs from picking the wrong
  one. Unify into `count_var_refs(stmt, name, mode: ReadOnly | ReadWrite)` or similar.
- [ ] **Scope-aware flush in linearizer** — `flush_protected` bolts scope-awareness onto
  `flush_pending_reads` via a HashSet. Could break for any new nested construct (switch
  cases, loop bodies) that isn't explicitly protected. The flush model needs inherent scope
  awareness.
- [ ] **Automatic write cascade on VarDecl removal** — `remove_dead_assigns(body, &name)`
  is called manually at two sites after VarDecl removal. A third removal path that forgets
  to call it reintroduces undeclared-variable bugs.
- [ ] **Prevent redundant cinit emission** — The emitter generates both
  `static readonly FIELD = value;` and a `static { this.FIELD = value; }` block for the
  same field, then filters the duplicate after the fact. Prevent dual emission instead.
