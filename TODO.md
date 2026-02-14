# TODO

## Next Up

- [x] **IR builder API** — Convenience layer in `reincarnate-core` for constructing functions, blocks, and instructions without manually managing entity IDs. Every frontend needs this.
- [x] **IR printer** — Human-readable text format for dumping IR (like LLVM `.ll` or Cranelift CLIF). Essential for debugging frontends and transforms.
- [x] **CLI scaffolding** — `reincarnate-cli` crate with clap. Parse a project manifest, load source files, print info. Wire up the pipeline trait plumbing.
- [x] **Flash frontend** — `reincarnate-frontend-flash` crate. AVM2 bytecode extraction and decompilation using Ruffle's `swf` crate (MIT/Apache-2.0). First real target.

## Critical — Transform Pass Test Coverage

- [x] **Unit tests for each pass** — Identity, edge cases, boundary conditions,
  side-effect preservation. 18 identity/idempotency + 24 edge case tests.
- [x] **Adversarial tests** — Shared constants, cyclic block params, diamond
  CFGs, deep chains, overflow/NaN, escape analysis, constraint cycles.
  30 adversarial tests; 2 bugs found and documented (#[ignore] + BUG).
- [x] **Pass interaction tests** — 6 cross-pass tests (fold→DCE, infer→cast-elim,
  mem2reg→DCE, cfg-simplify→mem2reg, full pipeline well-formed + idempotent).
- [x] **Round-trip invariant tests** — Well-formedness validator + idempotency
  harness in util.rs; every test verifies both properties.
- [x] **Stress tests** — 13 tests exercising all passes on varied IR shapes
  (linear chains, diamonds, loops, nested diamonds) × 6 type variants.
- [x] **Regression tests** — DCE branch-arg chain liveness (f0ac828).

Still open (not transform-pass scope):
- [ ] **End-to-end regression tests** — Snapshot tests for both frontends.
  - **Flash**: 15 new vN identifiers regressed in `91fe86e` (MethodCall
    refactor). Pre-existing 5 vN (hasNext2 one-shot, split-path phi).
  - **GML**: ~~`get_race` body wrong~~ — Fixed in two rounds:
    (1) GML frontend bugs: 2D array access and `Dup(N)` duplication count.
    (2) `forward_substitute` adjacency check used `stmt_references_var`
    (counts writes) but total-ref check used `count_var_refs_in_stmt`
    (reads only). When adjacent stmt only *wrote* the var in a nested
    assign target, substitute found no read to replace, silently dropping
    the value. Fix: use `count_var_refs_in_stmt` for adjacency too.
- [x] **GML 2D array access + `argument[N]` parameter access** — Fixed.
  In GM:S bytecode, `ref_type == 0` with `instance >= 0` indicates a 2D
  array access that pops 2 indices from the stack. The frontend was treating
  these as plain field accesses on numbered objects (e.g. `ButtonBase`).
  Fix: `is_2d_array_access()` helper detects the pattern; Push pops 2
  indices and pushes value, Pop pops value + 2 indices. For `argument`
  specifically, the 2D index maps to `fb.param(offset + N)`. Also fixed
  `Dup(N)` to correctly duplicate N+1 values (was always duplicating 1).

## Known Bugs (found by adversarial tests)

- [ ] **Mem2Reg: no escape analysis** — Allocs passed to calls are still promoted. If the callee modifies the pointed-to value, the Load after the call returns stale data. Need to mark allocs as escaped when their ValueId appears as an argument to Call/SystemCall. (`mem2reg.rs::alloc_escapes_via_call`, `#[ignore]`)
- [ ] **TypeInference: circular block params stay Dynamic** — Has an internal fixpoint loop, but `infer_common_type` short-circuits to Dynamic when ANY incoming arg is Dynamic. Back-edge args depend on the header param's type (chicken-and-egg), so circular block params never converge. Fix: skip Dynamic args during block-param join. ConstraintSolve handles this in practice. (`type_infer.rs::circular_block_params`, `#[ignore]`)

## Future

- [x] Type inference pass — forward dataflow (refine `Dynamic` via propagation)
- [x] Receiver-aware method resolution (class hierarchy walk, unique bare name fallback)
- [x] Redundant cast elimination pass (`Cast(v, ty)` → `Copy(v)` when types match)
- [x] Coroutine lowering transform (IR coroutine ops → state machines)
- [ ] Rust codegen backend (emit `.rs` files from typed IR — **blocked on alloc type refinement + multi-typed locals**)
- [x] TypeScript codegen backend
- [x] Dead code elimination pass
- [x] Constant folding pass
- [x] CFG simplification pass (merge redundant blocks, thread jumps)
- [x] Mem2Reg pass (promote single-store alloc/store/load chains, eliminate copies)
- [x] Structured control flow reconstruction (if/else, while, for from block CFG)
- [x] Transform pipeline fixpoint iteration (re-run until no changes)
- [x] Cross-module linking pass (resolve string imports, build global symbol table)
- [x] Asset extraction pipeline (images, audio, fonts from SWF/etc.)
- [ ] wgpu + winit renderer system implementation
- [ ] Web Audio system implementation

## Type System — Constraint-Based Inference

The current type inference is forward dataflow with fixed-point iteration. It
refines `Dynamic` when it can see the answer locally (constants, struct fields,
known function return types). This is enough for Flash (AVM2 has type
annotations) but insufficient for untyped frontends (Lingo, HyperCard, VB6
P-Code) where the IR starts as all-`Dynamic`.

A Rust backend makes this critical — Rust has no `any` escape hatch, so every
value needs a concrete type. This isn't a polish pass; it's a prerequisite.

### What exists today
- [x] Forward dataflow with fixed-point iteration
- [x] Receiver-aware method resolution (class hierarchy walk)
- [x] Cross-function return type propagation (module-level method index)
- [x] Select type inference
- [x] Redundant cast elimination

### What's needed
- [x] **Constraint-based solving** — `ConstraintSolve` pass generates equality
  constraints from operations and solves via union-find unification. Runs after
  forward `TypeInference` to propagate types backward (e.g., call argument used
  as `number` constrains the caller's variable). Reduced `:any` in Flash test
  output from 454 → 445.
- [ ] **Flow-sensitive narrowing** — narrow types after guards
  (`if (x instanceof Foo)` → `x: Foo` in then-branch). Requires per-block type
  environments rather than the current single `value_types` map. SSA form helps
  here — the BrIf arms can carry different type contexts.
- [ ] **Flash frontend: emit concrete types** — AVM2 bytecode has type
  annotations on locals, parameters, fields, return types. `resolve_type`
  failures cause unnecessary `Dynamic` entries. Fix the frontend to preserve
  what the source already knows, so inference only needs to handle what's
  genuinely untyped.
- [x] **Flash frontend: extract local variable names** — Done. Extracts from
  `Op::Debug` opcodes (not HAS_PARAM_NAMES, which has stale indices in this
  SWF). Register offset corrected for instance methods (`this` skipped).
  Names propagate through Mem2Reg and appear in TypeScript output.
- [ ] **Alloc type refinement** — The single biggest remaining `:any` source
  (~390 of 445). The Flash frontend creates `alloc dyn` for all locals. Even
  when every Store to an alloc writes the same concrete type (e.g. `Function`,
  `f64`), the alloc's own value_type stays `Dynamic`. The emitter declares
  locals using the alloc's type. Fix: if all stores to an `alloc dyn` agree on
  type, refine the alloc value_type to that type. Could live in the forward
  pass (`build_alloc_types` already computes the info but only uses it for Load
  refinement) or as a dedicated micro-pass.
- [ ] **Untyped frontend validation** — test the inference pipeline against a
  fully-untyped IR (simulating Lingo/HyperCard) to verify it can reconstruct
  useful types from usage patterns alone.

### Remaining `:any` analysis (Flash test, 445 total)

Measured on CoC.ts (36k lines) after TypeInference + ConstraintSolve.

| Category | Count | Root cause |
|----------|-------|------------|
| `const` locals | ~390 | Multi-store `alloc dyn` — alloc value_type stays Dynamic even when all stores agree. Emitter uses alloc type for declaration. |
| `let` locals | ~34 | Same as above but for Mem2Reg-promoted block params where incoming args don't all agree yet. |
| `any[]` arrays | 6 | Array element type unknown because elements are Dynamic (cascading from alloc issue). |
| Struct fields | 4 | Empty struct definitions (e.g. `struct Camp {}`) — fields accessed via GetField have no type info. Flash frontend doesn't populate all struct fields. |
| Multi-typed locals | ~5 | Genuinely different types assigned in different branches (e.g. `race = 0.0` then `race = "human"`). See Known Issues below. |

### Known Issues

- **Multi-typed locals** — Some Flash locals are assigned different types in
  different branches (e.g. `race` initialized to `0.0` as a sentinel, then
  assigned `this.player.race()` which returns `string`). These correctly stay
  `Dynamic` / `:any` today. For TypeScript this is ugly but functional. For
  Rust emit this is a hard blocker — Rust has no `any` type. Options:
  - **Split into separate variables** — SSA already distinguishes the defs, but
    the emitter coalesces them back into one mutable local. Could emit separate
    variables for each SSA def when types disagree.
  - **Enum wrapper** — generate a `Value2<A, B>` or tagged union for the
    specific types observed. Heavy, but correct.
  - **Sentinel elimination** — many cases are `0` or `null` sentinels followed
    by the real value. A pass that recognizes sentinel-then-overwrite patterns
    could use `Option<T>` instead of a union.
  - For TypeScript, the pragmatic fix is to emit a union type annotation
    (`number | string`) instead of `any`, which at least preserves type safety.

## Diagnostics & Validation

- [x] **Warning categorization** — Unmapped external reference warnings now
  filter private namespace member accesses and `fl.*` authoring library types.
  Flash stdlib references get specific package-level warnings.
- [x] **Per-export validation for flash packages** — `module_exports` in
  `runtime.json` maps each module path to its exported names. At emit time,
  `emit_external_imports` validates that each imported short name exists in the
  module's declared exports and warns if not. Catches types like `SimpleButton`
  that are referenced in SWFs but not yet implemented in the runtime.
- [x] **Script globals extraction** — AVM2 script `Slot`/`Const` traits
  extracted as IR `Global` entries via `ModuleBuilder.add_global()`. Class
  traits filtered by name to avoid duplicates. Emitted as `_globals.ts`.
- [x] **Global import detection** — `findPropStrict` scope lookups checked
  against `global_names`; class files import from `_globals.ts` as needed.
- [x] **Member access validation** — `GetField`/`SetField` ops validated
  against class hierarchy (instance fields, methods, getters/setters via
  `get_`/`set_` prefix, static fields/methods). 90 remaining warnings from
  external superclass members (Flash DisplayObject, Sprite, etc.).
- [ ] **External type member validation** — The remaining 90 member warnings
  are from types inheriting Flash stdlib classes. Need structured member
  metadata from runtime type definitions to validate these.

### Discarded AVM2 Metadata

The Flash frontend currently extracts core structure (classes, methods, fields,
globals) but discards several categories of ABC metadata that could improve
output fidelity:

- [ ] **Exception handler metadata** — `from`/`to` byte offsets,
  `variable_name`, `type_name`. Currently exception handlers are not modeled
  in the IR (no try/catch).
- [ ] **Class flags** — `is_sealed`, `is_final`. Could emit `final` modifier
  or sealed-class patterns in TypeScript.
- [ ] **Protected namespace** — Per-class protected namespace from
  `Instance.protected_namespace`. Used for `protected` member visibility in
  AS3.
- [ ] **Trait metadata annotations** — `TraitKind::Metadata` entries
  (attributes like `[Embed]`, `[Bindable]`, custom annotations).
- [ ] **Trait `is_override` / `is_final` flags** — Method trait attributes.
  `is_override` maps to TypeScript `override` keyword; `is_final` has no
  direct TS equivalent but documents intent.
- [ ] **Slot/dispatch IDs** — Trait `slot_id` and `disp_id` values. Used for
  AVM2 vtable layout but irrelevant to source-level decompilation.
- [ ] **`DebugLine` source line info** — `Op::DebugLine` opcodes carry
  original source line numbers. Could emit `// line N` comments or source
  maps.

## Output Quality — FFDec Comparison

Compared our TypeScript output against JPEXS FFDec's ActionScript decompilation
of the same SWF. Parameter names now match. Detailed notes with specific method
examples in the test project (`~/cc-project/comparison-notes.md`).

### High Priority (correctness)

- [x] **`["rt:?"]` runtime property access** — Fixed. Runtime multinames now
  resolve to proper indexed access (`array[index]`).
- [x] **Instruction reordering** — Fixed. Side-effecting inline expressions
  (Call/SystemCall results) are now flushed at block boundaries to preserve
  evaluation order.
- [ ] **Negative constant resolution** — At least one `Math.max` clamp emits
  a wrong positive constant instead of the correct negative value.
- [x] **Non-deterministic output** — Fixed. Sorted all HashMap/HashSet
  iterations by key before processing in structurize.rs (detect_loops,
  find_exit_in_body) and linear.rs (flush_pending_reads,
  flush_side_effecting_inlines, collect_block_param_decls, name coalescing).

### Medium Priority (output quality)

- [x] **Early returns via control flow inversion** — Done. Guard clause
  detection flattens `if/else` when one branch terminates.
- [x] **Default parameter values** — Done. HAS_OPTIONAL defaults emitted.
- [x] **Dead variable declarations** — Fixed. DCE Phase 5 eliminates unused
  block parameters at the IR level (iterative non-branch-arg analysis). Emitter
  buffers output and skips declaring params not referenced in the body. 31%
  reduction (12k → 8.4k) across the test project.
- [ ] **Complex loop decompilation** — Some while-loop bodies have unreachable
  code after `continue`, wrong variable assignments, and confused array
  accesses. Related to the `["rt:?"]` bug.

### Medium-High Priority (readability)

These are the main gaps between our output and ffdec-quality decompilation,
identified by comparing `takeDamage` / `reduceDamage` in Player.ts.

- [x] **Out-of-SSA variable coalescing** — Done. Mem2Reg propagates alloc debug
  names to stored values. Structurizer uses ValueId-only identity skip (not name
  matching). Lowerer detects shared names and emits assignments instead of const
  declarations, with self-assignment detection in branch-arg handlers.

- [x] **SE inline flush architecture** — Solved via AST-level single-use const
  folding (`fold_single_use_consts`). Instead of fixing the lowerer's flush
  mechanism, we let it produce conservative named variables, then fold
  single-use `const x = expr; use(x)` → `use(expr)` as a post-pass. This
  trivially recovers all inline opportunities lost by block-boundary flushing.

- [x] **Compound assignment detection** — AST-to-AST pass to rewrite
  `x = x + y` → `x += y`, `x = x - 1` → `x -= 1`, etc. Straightforward
  pattern match on `Stmt::Assign` where the value is a `Binary` with one
  operand equal to the target. Also `HP = HP - v` → `HP -= v`.

- [x] **Block-param decl/init merging** — When the ternary rewrite converts
  `if (c) { x = a } else { x = b }` → `x = c ? a : b`, the variable `x`
  is already declared as `let x: T;` at the top (block-param decl system).
  The result is a split `let x; ... x = c ? a : b` instead of a combined
  `let x = c ? a : b`. Post-pass merges uninit decls with their first
  dominating assignment. 43% of split let decls merged (1768/4081).

### Remaining `vN` Identifiers

0 unique vN identifiers remain across emitted TypeScript (down from 683 → 77
→ 28 → 19 → 4 → 1 → 0). Pass order: self_assigns → dup_assigns → forwarding_stubs →
invert_empty_then → eliminate_unreachable → ternary → minmax →
[fixpoint: forward_sub → ternary → ternary_to_logical → absorb_phi → narrow →
merge → fold] → compound_assign → post_increment. Debug name propagation through
Cast/Copy fixed 3 named variables (item, hug, returnDamage, itype).
absorb_phi_condition Case C eliminated the last (v115 in Mutations).

#### Pattern 1: Non-adjacent const before side-effect (13 vars)

```typescript
const v94 = (this.rand(2) === 0) && (this.player.findStatusAffect(...) < 0);
damage = this.player.takeDamage(damage);   // side-effecting intervening stmt
if (v94) { ...
```

Files: AntsScene, Bazaar, GooGirlScene, Helspawn, Katherine(x2), Kiha,
Kitsune, LivingStatue, Loppe(x2), MarbleScene, Owca.

AVM2 evaluates the condition before the side-effecting statement (operand stack
semantics). Can't inline without proving the init doesn't alias the mutation.

**Fix**: Alias/purity analysis on method calls, or accept. A rename pass (e.g.
`isStunnable`) would improve readability without elimination.

**Effort**: Hard (alias analysis) or Cosmetic (rename).

#### Pattern 2: Split-path phi boolean (6 vars)

```typescript
let v58: boolean;
if (condition) {
    v58 = complex_expr;    // assigned in one branch
} else {
    earlyReturn();         // other branch exits or has different logic
}
if (v58) { ...             // read after merge
```

Files: Camp, CoC, MarbleScene, MinervaScene, Mutations, Roxanne.

Phi variable assigned in one if-branch, left undefined in the other (which has
early returns or different control flow). Read after the merge point.

**Fix**: If the else branch always returns/breaks, the code after the if is
only reachable from the then-branch — the assign dominates the use. Could
hoist to `const v58 = expr` after the if. Requires AST-level dominator check.

**Effort**: Medium (~40 lines).

#### Pattern 3: Dup alias — object captured then field-accessed (4 vars)

```typescript
const v52 = this.emphasizedBorder;
this.emphasizedBorder.y = _loc4;
v52.x = _loc4;
```

Files: BaseScrollPane(x2), Button, ScrollPane.

AVM2 `dup` opcode materializes an object alias for consecutive `.x`/`.y`
field sets. The alias is the same object reference.

**Fix**: Relax `fold_single_use_consts` to allow sinking pure path expressions
(Var/Field chains, no calls) past field assignments when the assignment target
is a different path. Safe because field-set doesn't change the object reference.

**Effort**: Medium (~30 lines in `try_fold_one_const`).

#### Pattern 4: Operand-stack pre-increment capture (4 vars)

```typescript
const v24 = this._availableCheatControlMethods;
const v16 = this._cheatControlMethods;
this._availableCheatControlMethods = (v24 as number) + 1;
v16.push(new BoundControlMethod(func, name, desc, v24));
```

File: InputManager (2 branches, 2 vars each).

Captures pre-increment value and array reference from the operand stack.
`v24` preserves the old counter value for use in the constructor after the
increment. Same eval-order preservation as Pattern 1.

**Fix**: Same as Pattern 1 — alias analysis or accept.

**Effort**: Hard or Cosmetic.

#### Pattern 5: Property capture before side effect (2 vars)

```typescript
const v16 = storage[slotNum].itype;
_loc5.quantity -= 1.0;
this.inventory.takeItem(v16 as ItemType, ...);
```

Files: Inventory, Mutations.

Field read captured before an intervening mutation. Same root cause as
Pattern 1 but with pure inits.

**Fix**: Same relaxed sinking as Pattern 3 — pure path expressions past
non-overlapping assignments.

**Effort**: Medium (same fix as Pattern 3).

#### Pattern 6: Method reference capture for call args (2 vars)

```typescript
const v24 = this.hugTheShitOutOfYourHam;
const v19 = this.giveLottieAnItem;
// ... 160+ lines later ...
this.choices("Appearance", ..., "Give Item", v19, ..., "Hug", v24, ...);
```

File: Lottie.

Method references pushed to operand stack at function entry, consumed far
later. Fold refuses due to many intervening side effects.

**Fix**: Accept — these are correct temporaries. Large distance makes alias
analysis impractical.

**Effort**: N/A (correct as-is).

#### Pattern 7: Return value capture before body (1 var)

```typescript
let v24: number = ((damage > 0) && (damage < 1)) ? 1 : (damage as number);
if (damage > 0) { HP = HP - damage; ... }
return v24 as number;
```

File: Player.

Pre-computes the return value before the function body mutates state.
Correct — the return needs the pre-mutation value.

**Fix**: Accept. This is a genuine named variable.

**Effort**: N/A (correct as-is).

#### Pattern 8: Constant `rand(1)` (1 var)

```typescript
const v15 = this.rand(1);       // always returns 0
postCombat = postCombat && (...);
this.dynStats("cor", v15 + (postCombat ? 1 : 3));
```

File: PhoukaScene.

`rand(1)` always returns 0 (integer range [0,1)). The variable is always 0,
making the addition a no-op.

**Fix**: Teach constant folder that `rand(n)` where n <= 1 is always 0. Or
accept — it's a single case and also non-adjacent (Pattern 1 applies too).

**Effort**: Easy but narrow (1 case).

#### Summary

| Pattern | Vars | Fix | Effort | Status |
|---------|------|-----|--------|--------|
| 1. Non-adjacent const before side-effect | 13 | Sink past local-only assigns | Medium | Done |
| 2. Split-path phi boolean | 6 | Absorb into assigning branch | Medium | Done (all cases including C) |
| 3. Dup alias (object field set) | 4 | Relax sinking for pure paths | Medium | Done |
| 4. Operand-stack pre-increment | 4 | Alias analysis or accept | Hard | Accept for now |
| 5. Property capture before side effect | 2 | Same fix as Pattern 3 | Medium | Done |
| 6. Method ref capture (far use) | 2 | Accept | N/A | Correct as-is |
| 7. Return value capture | 1 | Accept | N/A | Correct as-is |
| 8. Constant rand(1) | 1 | Constant fold | Easy | Accept (also non-adj) |
| **Total** | **0** *(was 291)* | | | |

All patterns resolved. 0 unique vN remain from clean emit (683 → 0).
Debug name propagation through Cast/Copy fixed item, hug, returnDamage, itype.
absorb_phi_condition Case C (with else-body duplication) eliminated v115.

Cross-SWF vN counts: cc-project 0, utg-project 0, tln-project 16,
nff-project 2, mvol-project 6. The `simplify_ternary_to_logical` pass
eliminated v78 from tln (was 17 → 16).

### Architecture — Three-Layer Runtime (HLE)

See `docs/architecture.md` for the full design. The current runtime has two
structural problems:

1. **No platform abstraction** — `display.ts` calls `ctx.fillRect()` directly.
   Can't swap Canvas 2D for WebGL, can't test without a browser, can't share
   engine logic between TS and Rust runtimes.
2. **Backend has frontend knowledge** — `emit.rs` parses `flash.*::` namespace
   strings to route imports. The backend should not know about Flash namespaces.

**Migration plan (ordered):**

- [x] **Platform interface for TS runtime** — Extracted browser API calls from
  `display.ts`, `runtime.ts`, `text.ts`, `net.ts`, `utils.ts` into a
  `platform/` module. `platform/browser.ts` implements Canvas 2D init, DOM
  events, fetch, localStorage, timers, image loading. `platform/index.ts`
  re-exports from browser. Swap target by changing the re-export source.

- [ ] **Redesign system traits** — Replace `Renderer` (sprite-level) with
  `Graphics` (2D drawing primitives). Remove `Ui` (engine-level, not
  platform-level). Keep Audio, Input, Timing, Persistence. The Rust traits
  in `core/system/` define the canonical platform interface.

- [x] **Move runtime out of backend** — Runtime moved from
  `reincarnate-backend-typescript/runtime/` to `runtime/flash/ts/` at the
  workspace root. The backend receives `runtime_dir` via `BackendInput`
  and copies it into output generically, without knowing what's inside it.

- [x] **IR import metadata** — Add `external_imports` to `Module` mapping
  qualified names → `{ short_name, module_path }`. Flash frontend populates
  during extraction. Backend reads metadata instead of parsing `flash.*::`
  namespace strings. Eliminates `flash_pkg_module`, `flash_stdlib_module`,
  `emit_flash_stdlib_imports` from the backend.

- [x] **Runtime package config** — Runtime declares its import paths, scaffold
  config, and class preamble in `runtime.json`. CLI loads the manifest and
  passes `RuntimePackage` to the backend. Backend reads config instead of
  hardcoding Flash paths. `named_import` flag removed — all system modules
  use uniform namespace imports.

- [x] **Backend-local JS AST + scoped rewrites** — Done. `JsStmt`/`JsExpr`
  types in `js_ast.rs` with JS-specific variants (Throw, New, TypeOf, In,
  Delete, SuperCall, SuperMethodCall, SuperGet, SuperSet, Activation, This).
  Mechanical lowering pass in `lower.rs` converts core `Vec<Stmt>` →
  `Vec<JsStmt>` with SystemCalls passed through as-is. Flash-specific
  rewrites in `rewrites/flash.rs` (FlashRewriteCtx, scope resolution,
  SystemCall → JS construct mapping) run as a post-lowering `JsExpr → JsExpr`
  tree walk. Printer in `ast_printer.rs` handles `JsStmt`/`JsExpr`
  faithfully with zero engine knowledge. Object literal keys now unquoted
  for valid JS identifiers; `in` expressions correctly parenthesized.

### Architecture — Hybrid Lowering via Structured IR

The current `lower_ast.rs` (~1000 lines) interleaves three concerns: control
flow lowering, expression inlining, and side-effect ordering. This causes
cascading bugs — inlining decisions interact with ternary detection, name
coalescing, self-assignment elimination, and shape processing in ways that are
hard to reason about.

Replace with a three-phase hybrid pipeline:

- [x] **Phase 1: Shape → `LinearStmt`** — Walk the Shape tree and produce a
  flat `Vec<LinearStmt>` where every instruction is a `Def(ValueId, Op)`,
  control flow comes from shapes (`If(ValueId, Vec, Vec)`, `While`, etc.),
  and branch args become `Assign(ValueId, ValueId)`. No inlining decisions.
  Trivial ~200-line shape walk.

- [x] **Phase 2: Pure resolution on `LinearStmt`** — Single pass over the
  structured IR. Pure single-use values (`use_count == 1 && is_pure`) are
  substituted into their consumer (ValueId → expression tree). Constants
  always substituted. Scope lookups + cascading GetField marked as
  always-rebuild. Dead pure code dropped. Self-assignments detected via
  ValueId equality. Name coalescing annotated (shared ValueIds → mutable
  assignment). This handles 90% of inlining with zero side-effect concerns.

- [x] **Phase 3: `LinearStmt` → AST** — Resolve remaining ValueIds to
  variable names. Side-effecting single-use values inlined if no
  intervening side effects (the only hard case, now isolated to ~10% of
  values). Multi-use values get `const`/`let` declarations. Produces
  `Vec<Stmt>` for existing AST passes.

Existing AST passes (ternary, compound assign, const fold, decl/init merge,
self-assign elimination, empty-then inversion, unreachable elimination,
ternary-to-logical) continue unchanged on the output AST.

Benefits: eliminates `lazy_inlines`, `side_effecting_inlines`,
`always_inlines`, `se_flush_declared`, `skip_loop_init_assigns`, and the
flush mechanisms. Pure inlining is provably correct (no ordering concerns).
Side-effect handling is isolated. `LinearStmt` is thinner than the AST
(ValueId refs vs String names) — net memory reduction vs current `LowerCtx`.

### Flash Runtime Elimination

Replace `Flash_*` helper calls with native JS equivalents. Each of these is
a `SystemCall` in the IR that the TypeScript backend resolves to a runtime
module call. Most can be inlined as native syntax or simple expressions,
eliminating the runtime module entirely.

Measured on cc-project output. Count = call sites across all emitted `.ts`.

#### Tier 1 — Trivial inlines (AST rewrite or backend print)

These are 1:1 syntax replacements. No new AST nodes needed for most; some
need a `Stmt::Throw` or `Expr::Delete`/`Expr::TypeOf`/`Expr::In`.

- [x] **`Flash_Iterator.hasNext2` / `nextValue` / `nextName`** (was ~56 loops) →
  `for (const x of Object.values/keys(...))`. Done via `rewrite_foreach_loops`
  AST pass. Two non-loop one-shot checks remain (correct).
- [x] **`Flash_Exception.throw(x)`** (21) → `throw x;`. Inlined in backend
  printer's `Stmt::Expr` handler.
- [x] **`Flash_Scope.newActivation()`** (52) → `({})`. Inlined in backend
  printer as parenthesized empty object (parens needed when const-folded
  into field access targets like `({}).slot1`).
- [x] **`Flash_Object.typeOf(x)`** (4) → `typeof x`. Inlined in backend
  printer's `print_system_call`.
- [x] **`Flash_Object.hasProperty(obj, k)`** (14) → `k in obj`. Inlined in
  backend printer's `print_system_call`.
- [x] **`Flash_Object.deleteProperty(obj, k)`** (14) → `delete obj[k]`.
  Inlined in backend printer's `print_system_call`.

#### Tier 2 — Simple expression rewrites

- [x] **`Flash_Object.newObject(k1, v1, k2, v2, ...)`** (58) → `{ k1: v1, k2: v2 }`.
  Inlined in backend printer — pairs of string-key + value arguments emitted
  as object literal.
- [x] **`Flash_Object.newFunction("name")`** (318) → closure reference.
  Frontend now recursively translates closure method bodies from
  `Op::NewFunction` into private instance methods (`$closureN`). Backend
  inlines the call as `this.$closureN`. Runtime stub removed.

#### Tier 3 — Structural (super calls)

These need the backend to emit `super.x` syntax instead of function calls.

- [x] **`Flash_Class.callSuper(this, "method", ...args)`** (94) →
  `super.method(...args)`. Inlined in backend printer's `print_system_call`.
- [x] **`Flash_Class.getSuper(this, "prop")`** (30) → `super.prop`. Inlined
  in backend printer's `print_system_call`.
- [x] **`Flash_Class.setSuper(this, "prop", value)`** (21) → `super.prop = value`.
  Inlined in both `print_system_call` (expression) and `print_stmt`
  (statement context for clean `super.prop = value;` without parens).

#### Tier 4 — Keep as runtime

These legitimately need runtime support and shouldn't be inlined.

- **`Flash_Object.applyType(base, ...typeArgs)`** (8) — Generic type
  application (`Vector.<int>`). Runtime already returns `base` (no-op).
  Low priority; could emit bare `base` but the callsites are complex.
- **`Flash_Utils.describeType(x)`** (3) — Reflection. Needs runtime.
- **`Flash_Utils.getDefinitionByName(name)`** (3) — Class registry lookup.
- **`Flash_Utils.getQualifiedClassName(x)`** (10) — Reflection.
- **`Flash_Utils.getQualifiedSuperclassName(x)`** (1) — Reflection.
- **`Flash_XML.getDescendants(xml, name)`** (3) — E4X XML operations.

### Optimizations — Future Directions

The current pipeline focuses on faithful decompilation: reproduce what the
original source did, with clean variable names and readable structure. Further
optimization passes could improve output quality but each carries correctness
risks that need careful analysis.

#### Safe (no semantic change)

- [ ] **Redundant type casts** — Eliminate `as number` etc. when the expression
  already has the target type. Pure type-level, no runtime effect.
- [x] **Separate AsType from Coerce/Convert in IR** — `Op::Cast` now carries
  `CastKind` (AsType | Coerce). Coerce emits `Number()`, `int()`, `uint()`,
  `String()`, `Boolean()`. AsType+Struct emits `asType()`. Redundant casts
  eliminated by `red_cast_elim` + `linear.rs` same-type check.
- [x] **Demote `asType()` to compile-time `as T`** — Already handled:
  `red_cast_elim` converts `Cast(v, ty, _)` → `Copy(v)` when value type matches,
  and `linear.rs` drops Cast entirely when types match. No `asType()` emitted
  for same-type casts.
- [ ] **Constant `rand(n)` where n <= 1** — `rand(1)` always returns 0 (integer
  range `[0, n)`). Could fold to literal 0. Only 1 known instance
  (PhoukaScene).
- [ ] **Dead store elimination** — Remove assignments whose values are never
  read. Already partially done (dead decl removal); extending to non-decl
  assigns requires liveness analysis.
- [ ] **Condition inversion** — Structurizer sometimes inverts conditions.
  Not a bug but reads backward vs the original source. Heuristic to match
  original branch polarity.

#### Requires alias/purity analysis

These optimizations reorder or eliminate reads through object references. In
AS3, property access can invoke getters (`get foo(): T`), making `this.foo`
potentially side-effecting. Inlining `const v = this.foo; ... use(v)` as
`use(this.foo)` is only safe if:
1. `foo` is a plain field (not a getter), AND
2. Nothing between the capture and use mutates `foo`

For (1), we'd need class hierarchy analysis to determine whether a property
access hits a getter. The ABC metadata has trait definitions that distinguish
`TRAIT_SLOT` (plain field) from `TRAIT_GETTER` (accessor), but virtual dispatch
means a subclass could override a slot with a getter.

For (2), we'd need alias analysis — does an intervening call (e.g.
`takeDamage()`) mutate the field being read? In general this requires whole-
program analysis or conservative assumptions.

- [ ] **Cross-side-effect const sinking** — Sink `const v = expr` past
  side-effecting statements when `expr` is provably pure and unaliased.
  Would eliminate Patterns 1, 4, 5, 6 (the remaining 4 vN). Requires
  purity analysis for method calls and field accesses.
- [ ] **Method reference inlining** — `const v = this.method; ... v(args)` →
  `this.method(args)`. Only safe if `method` is not a getter. The Lottie
  case captures `this.giveLottieAnItem` and `this.hugTheShitOutOfYourHam`
  160 lines before use — inlining changes evaluation order of the property
  access.
- [ ] **Field read deduplication** — `this.x` read twice → read once, reuse.
  Unsafe if intervening code could mutate `this.x` or if `x` is a getter
  with side effects.

#### Requires control flow analysis

- [x] **Pattern 2 Case C (absorb_phi_condition)** — Done. Duplicates the
  use-site's else body into the outer else branch. Safety: checks that the
  phi variable has no refs between its declaration and the outer if, to
  avoid incorrectly matching general-purpose variables.
- [ ] **Inline closures** — Filter/map callbacks extracted as named function
  references instead of being inlined as arrow functions. Requires knowing
  the function is only passed once.
- [ ] **Loop variable promotion** — Some for-loop induction variables could
  be declared in the `for` header (`for (let i = 0; ...)`) rather than
  as separate `let` declarations. Requires scope analysis.

#### Naming & readability (non-semantic)

- [x] **All vN eliminated** — 0 unique vN remain. No renaming needed.
- [x] **Op::Debug name propagation** — Fixed. 3 of 4 remaining vN were
  actually named variables (`item`, `hug`, `returnDamage`, `itype`). Root
  cause: Mem2Reg transferred names to Cast results, but the emitter lazily
  inlined the Cast and materialized the source (GetField/block-param) without
  the name. Fix: propagate names through Cast/Copy in flush_pending_reads
  and EmitCtx::new.

## GameMaker Frontend

### Reference Project

The decompiled GML reference for Bounty is at `~/git/bounty/`. Scripts in
`scripts/main*.js`, object event handlers in `classes/`. Compare emitted
`~/Bounty/out/` against this reference when validating output.

### Critical

- [x] **`Op::MethodCall` IR node + remove `receiver_is_first_arg`** — Done.
  `Op::MethodCall { receiver, method, args }` added to core IR. Flash frontend
  uses `fb.call_method()` for CallProperty/CallPropVoid/CallPropLex/CallStatic/
  CallMethod. GameMaker uses plain `Op::Call` (no method dispatch). Backend
  lowers `MethodCall` to `receiver.method(args)` without any per-engine flag.

- [x] **2D array write: stack pop order** — Fixed. The GML bytecode stack
  layout for 2D array Pop instructions is `[value, dim2, dim1]` with dim1 on
  top. The translator was popping `value` first (getting dim1), then `index`
  (getting dim2), then `_dim1` (getting the actual value). This caused array
  writes like `advantages[i] = argument0` to emit as `advantages[3] = int(i)`.
  Fixed by restructuring the Pop handler to check `is_2d_array_access` before
  the first pop, then popping all three in correct order (dim1, dim2, value).

- [x] **`button_click` structural corruption** — Fixed. The post-dominator
  computation didn't recognize empty blocks (no instructions, no successors)
  as function exits. GML frontends produce empty implicit-return blocks that
  were invisible in the reverse CFG, corrupting the post-dominator tree. This
  caused block6's else branch (the `released(2)` check) to be identified as
  the merge point rather than a branch target, flattening the entire else-if
  structure. Fixed in `structurize.rs` `compute_post_dominators`.

### High Priority (correctness)

- [x] **Branch offset encoding** — Fixed. GML bytecode branch offsets use
  23-bit signed values in bits 0-22 of the instruction word. Bit 23 is not part
  of the offset (it encodes type/flag info). The decoder was using 24-bit sign
  extension, causing all backward branches (loop back-edges) to resolve to
  invalid targets. Every `for`/`while` loop in GML output was lost — the loop
  body executed once then fell through. Fixed in `datawin/bytecode/decode.rs`.
  `DataType` also gained a `Raw(u8)` variant to preserve unknown nibble values
  for bytecode round-trip fidelity.
- [x] **Switch statement reconstruction** — Done. Frontend detects
  `Dup` + `Cmp(Eq)` + `BrIf` chains and rewrites them as `Op::Switch`.
  Core pipeline and backend support Switch through structurize → linear → emit.
- [x] **Named object singleton accessors** — Done. Numeric instance IDs
  resolved to named singletons (e.g. `Stats.instances[0].dice_type`)
  using object names from OBJT chunk.
- [x] **Self-referencing instance types** — Done. The GM compiler uses the
  owning object's index as instance type for self-variable access. Normalized
  to -1 (Own) when instance matches `self_object_index`.
- [x] **Object persistent/visible flags** — Done. Emit `persistent` and
  `visible` from OBJT as class instance fields when they differ from defaults.
- [ ] **Parent class self-references** — When an object inherits from
  a parent, bytecode may use the parent's object index as instance type for
  inherited field accesses. Currently these produce cross-object `getOn/setOn`
  instead of self-access. Need to walk the parent chain in TranslateCtx.
- [x] **Stale namespace imports** — Fixed. Post-process
  `strip_unused_namespace_imports()` removes `import * as NAME` lines
  where `NAME.` never appears in the output body. Runs after all rewrite
  passes, so engine-specific rewrites that eliminate namespace usages are
  handled correctly.
- [x] **Empty switch cases (shared target blocks)** — Fixed. When multiple
  switch cases share the same target block, the structurizer now reorders
  secondary cases before the primary case. The printer emits empty cases
  as fall-through labels (no body, no break).
- [ ] **ButtonBase alarm/advantages/inventory** — Stats.create() references
  `ButtonBase.instances[0].alarm`, `.advantages`, `.inventory` with
  instance type 0 (ButtonBase). In the original GML these may be genuine
  cross-object references or may need parent-chain normalization.

### GML Boolean / Short-Circuit Detection

Several related issues around boolean handling in GML output. GML has no
dedicated boolean type — `true`/`false` are 1/0 at runtime. The bytecode
preserves these as integers, losing boolean semantics.

- [x] **Stacktop-via-ref_type stack imbalance** — Fixed. In pre-GMS2 bytecode,
  `ref_type=0x80` with `instance >= 0` encodes a stacktop reference (the
  target instance is on the operand stack). The translator was treating these
  as normal field access, causing Pop to consume 1 value instead of 2 and
  Push to consume 0 instead of 1. This left the stack unbalanced — the
  `repeat` loop counter would get buried under unconsumed values, making
  the decrement instruction operate on the wrong value.

- [x] **Empty `while (true) {}` loop bodies (general loop structurizer)** —
  Fixed. The structurizer's `emitted` set prevents exponential blowup by
  skipping already-visited blocks. When a general loop's header was also its
  body entry, the header was marked as emitted before `structurize_loop`
  could process it, producing empty loop bodies. Fixed by removing the
  header from `emitted` in `structurize_general_loop` before re-entering.
  Restored 25 `repeat`/`with` loop bodies in Bounty.

- [x] **Guard clause drops continuation branch assigns** — Fixed. When the
  structurizer emits a guard clause (one branch terminates, the other
  continues), block-param assignments for the continuation branch were
  lost. Now when the continuation has assigns, it's nested inside the
  else body instead of flattened as a sibling.

- [ ] **General loop block-param counter uses initial value** — In `repeat(N)`
  loops emitted as `while (true) { ... }`, the loop counter decrement
  `v10 - 1` renders as `v100 - 1` (using the initial entry value instead
  of the mutable loop variable). The IR is correct (`sub v10, v120` where
  v10 is a block param), but the emitter resolves v10 to v100 somewhere
  in the build_val → name_coalescing → forward_substitute pipeline. The
  root cause is not yet identified. Affects all `repeat` loops with counters.

- [x] **Short-circuit `||`/`&&` emitted as nested ternaries** — Fixed. The IR
  encodes boolean short-circuit evaluation as BrIf chains with block params
  carrying 0/1. The structurizer emits these as `(A) ? 1 : ((B) ? (C) : 0)`
  instead of `A || (B && C)`. Example in `advantage_add`: the condition
  `advantages[i] === "None" || (advantages[i] === "Free" && arg !== "Free")`
  emits as `(... === "None") ? 1 : ((... === "Free") ? (arg !== "Free") : 0)`.
  The pattern is: BrIf with one branch passing const 1/0 to a merge block,
  the other branch computing the actual condition. This is different from the
  Flash LogicalOr/LogicalAnd detection (which uses `try_logical_op` on
  IfElse shapes). The GML pattern likely needs its own detection pass or an
  extension to the existing logical-op detection.

- [ ] **Numeric booleans: `=== 1` / `=== 0` instead of boolean tests** —
  GML compiles `if (self.active)` as `push self.active; pushi 1; cmp.eq; bf`.
  This produces IR `cmp.eq(self.active, 1)` which emits as
  `self.active === 1` instead of the idiomatic `self.active`. Similarly,
  `!self.active` becomes `self.active === 0`. Fixing this requires heuristics
  to recognize when a comparison against 0/1 is really a boolean test.
  Approach: during type inference or as a post-pass, identify fields/variables
  that are only ever assigned 0/1/true/false, then replace `=== 1` with bare
  test and `=== 0` with `!`. Requires investigating all usage sites across
  all functions to build the boolean field set — can't just pattern-match
  locally because a variable might be assigned numeric values elsewhere.

- [ ] **Enum detection (string and numeric)** — Many GML games use string
  constants as enum values (e.g. `"None"`, `"Free"`, `"Well-Off"` for
  advantages; `"pressed"`, `"left"`, `"right"` for mouse buttons). These
  could be extracted into `const` objects during type inference by tracking
  switch case values and equality comparisons. String enums are easier
  (unique strings). Numeric enums (e.g. MouseButtons: 0=none, 1=pressed,
  2=left, 3=right) are harder — requires cross-referencing assignment sites
  with switch cases to infer the mapping. The reference code
  (`~/git/bounty/scripts/main.js`) uses `Advantages.none`, `Advantages.free`,
  `MouseButtons.pressed` etc., showing these were originally named constants.

### Type Inference Results

GML bytecode has zero type metadata — everything enters the IR as `Dynamic`.
With `function_signatures` in `runtime.json` (~160 GML standard library
functions), `type_definitions` for `GMLObject` (44 built-in fields), and
`Struct(class_name)` self parameter typing:

| Metric | Before | After |
|--------|--------|-------|
| `let: any` | 1736 → 8 | 2 |
| `(): any` return types | 205 → 10 | 7 |
| Total `: any` | ~1950 | 9 |

Remaining 7 `(): any` are scripts that return `variable_global_get()` which
legitimately returns `*` (Dynamic). Remaining 2 `let: any` are variables
assigned from `variable_global_get()` or untyped argument passthrough.

### Medium Priority (output quality)

- [x] **While → for loop promotion** — Fixed. AST-level rewrite pass
  `promote_while_to_for` detects `init; while (cond) { body; update; }`
  patterns and rewrites as `for (init; cond; update) { body }`. Handles both
  VarDecl and Assign inits, looks backwards past non-related declarations,
  and supports tail-increment and else-continue-increment patterns.
- [ ] **Sprite constant resolution** — `this.sprite_index = 34` should be
  `this.sprite_index = Sprites.spr_dice`. Requires mapping sprite indices to
  enum names at emission time (the Sprites enum already exists in data output).
- [x] **Duplicate variable declarations** — Fixed: locals map was keyed by
  `local.index` (CodeLocals slot) but looked up by `var_ref.variable_id` (VARI
  index). Keyed by variable name instead.

### GML Runtime — Missing Function Implementations

Functions listed in `function_modules` (runtime.json) but not yet implemented
in the corresponding TypeScript modules. Generated imports will reference these,
causing compile errors if any emitted code calls them.

**draw.ts** — missing draw_text variants:
- [ ] `draw_text_ext_transformed` — draw text with line sep, width, and transform
- [ ] `draw_text_transformed_color` — draw text with transform and per-corner colors
- [ ] `draw_text_ext_transformed_color` — draw text with all options combined

**color.ts** — missing color functions from gml.js reference:
- [ ] `color_get_hue` — extract hue component (0–255) from a GML color
- [ ] `make_color_hsv` — create color from hue, saturation, value
