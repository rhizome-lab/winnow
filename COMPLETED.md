# Completed

Items resolved from TODO.md, moved here to keep the active list focused.

## Initial Milestones

- [x] **IR builder API** — Convenience layer in `reincarnate-core` for constructing functions, blocks, and instructions without manually managing entity IDs. Every frontend needs this.
- [x] **IR printer** — Human-readable text format for dumping IR (like LLVM `.ll` or Cranelift CLIF). Essential for debugging frontends and transforms.
- [x] **CLI scaffolding** — `reincarnate-cli` crate with clap. Parse a project manifest, load source files, print info. Wire up the pipeline trait plumbing.
- [x] **Flash frontend** — `reincarnate-frontend-flash` crate. AVM2 bytecode extraction and decompilation using Ruffle's `swf` crate (MIT/Apache-2.0). First real target.

## Transform Pass Test Coverage

- [x] **Unit tests for each pass** — Identity, edge cases, boundary conditions,
  side-effect preservation. 18 identity/idempotency + 24 edge case tests.
- [x] **Adversarial tests** — Shared constants, cyclic block params, diamond
  CFGs, deep chains, overflow/NaN, escape analysis, constraint cycles.
  30 adversarial tests; 2 bugs found and documented.
- [x] **Pass interaction tests** — 6 cross-pass tests (fold→DCE, infer→cast-elim,
  mem2reg→DCE, cfg-simplify→mem2reg, full pipeline well-formed + idempotent).
- [x] **Round-trip invariant tests** — Well-formedness validator + idempotency
  harness in util.rs; every test verifies both properties.
- [x] **Stress tests** — 13 tests exercising all passes on varied IR shapes
  (linear chains, diamonds, loops, nested diamonds) × 6 type variants.
- [x] **Regression tests** — DCE branch-arg chain liveness (f0ac828).
- [x] **GML 2D array access + `argument[N]` parameter access** — Fixed.
  In GM:S bytecode, `ref_type == 0` with `instance >= 0` indicates a 2D
  array access that pops 2 indices from the stack. The frontend was treating
  these as plain field accesses on numbered objects (e.g. `ButtonBase`).
  Fix: `is_2d_array_access()` helper detects the pattern; Push pops 2
  indices and pushes value, Pop pops value + 2 indices. For `argument`
  specifically, the 2D index maps to `fb.param(offset + N)`. Also fixed
  `Dup(N)` to correctly duplicate N+1 values (was always duplicating 1).

## Known Bugs (found by adversarial tests)

- [x] **Mem2Reg: no escape analysis** — Fixed. `promote_single_store` and
  `promote_multi_store` both use `value_operands()` to detect alloc pointers
  passed as Call/SystemCall/MethodCall arguments and mark them as escaped.
  Test `alloc_escapes_via_call` passes.
- [x] **TypeInference: circular block params stay Dynamic** — Fixed. Forward
  pass skips Dynamic args during block-param join (line 628: `.filter(|ty|
  **ty != Type::Dynamic)`). Test `circular_block_params` passes.

## Completed Future Items

- [x] Type inference pass — forward dataflow (refine `Dynamic` via propagation)
- [x] Receiver-aware method resolution (class hierarchy walk, unique bare name fallback)
- [x] Redundant cast elimination pass (`Cast(v, ty)` → `Copy(v)` when types match)
- [x] Coroutine lowering transform (IR coroutine ops → state machines)
- [x] TypeScript codegen backend
- [x] Dead code elimination pass
- [x] Constant folding pass
- [x] CFG simplification pass (merge redundant blocks, thread jumps)
- [x] Mem2Reg pass (promote single-store alloc/store/load chains, eliminate copies)
- [x] Structured control flow reconstruction (if/else, while, for from block CFG)
- [x] Transform pipeline fixpoint iteration (re-run until no changes)
- [x] Cross-module linking pass (resolve string imports, build global symbol table)
- [x] Asset extraction pipeline (images, audio, fonts from SWF/etc.)

## Type System — Completed Items

- [x] Forward dataflow with fixed-point iteration
- [x] Receiver-aware method resolution (class hierarchy walk)
- [x] Cross-function return type propagation (module-level method index)
- [x] Select type inference
- [x] Redundant cast elimination
- [x] **Constraint-based solving** — `ConstraintSolve` pass generates equality
  constraints from operations and solves via union-find unification. Runs after
  forward `TypeInference` to propagate types backward (e.g., call argument used
  as `number` constrains the caller's variable). Reduced `:any` in Flash test
  output from 454 → 445.
- [x] **Flash frontend: extract local variable names** — Done. Extracts from
  `Op::Debug` opcodes (not HAS_PARAM_NAMES, which has stale indices in this
  SWF). Register offset corrected for instance methods (`this` skipped).
  Names propagate through Mem2Reg and appear in TypeScript output.
- [x] **Alloc type refinement** — Fixed. `build_alloc_types` scans all Store
  instructions and unions stored types. After the forward inference loop,
  `infer_function` refines `Op::Alloc(Dynamic)` → `Op::Alloc(concrete)` when
  all stores agree (type_infer.rs:644-662). Local `:any` reduced from ~390 to
  ~53 (remaining are genuinely untyped values from calls/block params).

## Diagnostics & Validation — Completed

- [x] **Warning categorization** — Unmapped external reference warnings now
  filter private namespace member accesses and `fl.*` authoring library types.
  Flash stdlib references get specific package-level warnings.
- [x] **Per-export validation for flash packages** — `module_exports` in
  `runtime.json` maps each module path to its exported names. At emit time,
  `emit_external_imports` validates that each imported short name exists in the
  module's declared exports and warns if not.
- [x] **Script globals extraction** — AVM2 script `Slot`/`Const` traits
  extracted as IR `Global` entries via `ModuleBuilder.add_global()`. Class
  traits filtered by name to avoid duplicates. Emitted as `_globals.ts`.
- [x] **Global import detection** — `findPropStrict` scope lookups checked
  against `global_names`; class files import from `_globals.ts` as needed.
- [x] **Member access validation** — `GetField`/`SetField` ops validated
  against class hierarchy (instance fields, methods, getters/setters via
  `get_`/`set_` prefix, static fields/methods). 90 remaining warnings from
  external superclass members (Flash DisplayObject, Sprite, etc.).

## Output Quality — FFDec Comparison (Completed Items)

- [x] **`["rt:?"]` runtime property access** — Fixed. Runtime multinames now
  resolve to proper indexed access (`array[index]`).
- [x] **Instruction reordering** — Fixed. Side-effecting inline expressions
  (Call/SystemCall results) are now flushed at block boundaries to preserve
  evaluation order.
- [x] **Non-deterministic output** — Fixed. Sorted all HashMap/HashSet
  iterations by key before processing.
- [x] **Early returns via control flow inversion** — Done. Guard clause
  detection flattens `if/else` when one branch terminates.
- [x] **Default parameter values** — Done. HAS_OPTIONAL defaults emitted.
- [x] **Dead variable declarations** — Fixed. DCE Phase 5 eliminates unused
  block parameters at the IR level. 31% reduction (12k → 8.4k).
- [x] **Out-of-SSA variable coalescing** — Done.
- [x] **SE inline flush architecture** — Solved via AST-level single-use const
  folding (`fold_single_use_consts`).
- [x] **Compound assignment detection** — AST-to-AST pass to rewrite
  `x = x + y` → `x += y`, etc.
- [x] **Block-param decl/init merging** — Post-pass merges uninit decls with
  their first dominating assignment. 43% of split let decls merged.

## Remaining `vN` Identifiers — All Resolved

0 unique vN identifiers remain across emitted TypeScript (down from 683 → 0).

Pass order: self_assigns → dup_assigns → forwarding_stubs →
invert_empty_then → eliminate_unreachable → ternary → minmax →
[fixpoint: forward_sub → ternary → ternary_to_logical → absorb_phi → narrow →
merge → fold] → compound_assign → post_increment.

| Pattern | Vars | Fix | Status |
|---------|------|-----|--------|
| 1. Non-adjacent const before side-effect | 13 | Sink past local-only assigns | Done |
| 2. Split-path phi boolean | 6 | Absorb into assigning branch | Done |
| 3. Dup alias (object field set) | 4 | Relax sinking for pure paths | Done |
| 4. Operand-stack pre-increment | 4 | Alias analysis or accept | Accept |
| 5. Property capture before side effect | 2 | Same fix as Pattern 3 | Done |
| 6. Method ref capture (far use) | 2 | Accept | Correct as-is |
| 7. Return value capture | 1 | Accept | Correct as-is |
| 8. Constant rand(1) | 1 | Constant fold | Accept |

Cross-SWF vN counts: cc-project 0, utg-project 0, tln-project 16,
nff-project 2, mvol-project 6.

## Architecture — Three-Layer Runtime (HLE) Migration

All migration steps completed. See `docs/architecture.md` for the design.

- [x] **Platform interface for TS runtime** — Extracted browser API calls from
  `display.ts`, `runtime.ts`, `text.ts`, `net.ts`, `utils.ts` into a
  `platform/` module. `platform/browser.ts` implements Canvas 2D init, DOM
  events, fetch, localStorage, timers, image loading. Swap target by changing
  the re-export source.

- [x] **Redesign system traits** — Renamed `Renderer` → `Graphics`,
  `SaveLoad` → `Persistence`. Split `Ui` into `Dialog`, `SaveUi`,
  `SettingsUi`. Added `Network`, `Images`, `Files`, `Layout` traits.

- [x] **Move runtime out of backend** — Runtime moved from
  `reincarnate-backend-typescript/runtime/` to `runtime/flash/ts/` at the
  workspace root.

- [x] **IR import metadata** — `external_imports` on `Module` mapping
  qualified names → `{ short_name, module_path }`. Backend reads metadata
  instead of parsing `flash.*::` namespace strings.

- [x] **Runtime package config** — Runtime declares its import paths, scaffold
  config, and class preamble in `runtime.json`. CLI loads the manifest and
  passes `RuntimePackage` to the backend.

- [x] **Backend-local JS AST + scoped rewrites** — `JsStmt`/`JsExpr`
  types in `js_ast.rs`. Mechanical lowering in `lower.rs`. Engine-specific
  rewrites in `rewrites/flash.rs` as a post-lowering `JsExpr → JsExpr` walk.

## Hybrid Lowering via Structured IR — Completed

Replaced the interleaved `lower_ast.rs` with a three-phase pipeline:

- [x] **Phase 1: Shape → `LinearStmt`** — Walk the Shape tree and produce a
  flat `Vec<LinearStmt>`.
- [x] **Phase 2: Pure resolution on `LinearStmt`** — Single pass. Pure
  single-use values substituted, constants always substituted.
- [x] **Phase 3: `LinearStmt` → AST** — Resolve remaining ValueIds to variable
  names, produce `Vec<Stmt>` for existing AST passes.

## Flash Runtime Elimination — Completed

| SystemCall | Count | Replacement | Status |
|------------|-------|-------------|--------|
| `Flash_Iterator.hasNext2/nextValue/nextName` | ~56 | `for (const x of Object.values/keys(...))` | Done |
| `Flash_Exception.throw(x)` | 21 | `throw x;` | Done |
| `Flash_Scope.newActivation()` | 52 | `({})` | Done |
| `Flash_Object.typeOf(x)` | 4 | `typeof x` | Done |
| `Flash_Object.hasProperty(obj, k)` | 14 | `k in obj` | Done |
| `Flash_Object.deleteProperty(obj, k)` | 14 | `delete obj[k]` | Done |
| `Flash_Object.newObject(...)` | 58 | Object literal | Done |
| `Flash_Object.newFunction("name")` | 318 | Closure reference | Done |
| `Flash_Class.callSuper(...)` | 94 | `super.method(...)` | Done |
| `Flash_Class.getSuper(...)` | 30 | `super.prop` | Done |
| `Flash_Class.setSuper(...)` | 21 | `super.prop = value` | Done |

Kept as runtime (legitimately need it):
- `Flash_Object.applyType` (8), `Flash_Utils.describeType` (3),
  `Flash_Utils.getDefinitionByName` (3), `Flash_Utils.getQualifiedClassName` (10),
  `Flash_Utils.getQualifiedSuperclassName` (1), `Flash_XML.getDescendants` (3).

## Completed Optimizations

- [x] **Separate AsType from Coerce/Convert in IR** — `Op::Cast` now carries
  `CastKind`. Redundant casts eliminated by `red_cast_elim` + `linear.rs`.
- [x] **Demote `asType()` to compile-time `as T`** — Already handled.
- [x] **Pattern 2 Case C (absorb_phi_condition)** — Done. Duplicates the
  use-site's else body into the outer else branch.
- [x] **All vN eliminated** — 0 unique vN remain. No renaming needed.
- [x] **Op::Debug name propagation** — Fixed. Names propagate through Cast/Copy.

## GameMaker Frontend — Completed Items

### Critical

- [x] **`Op::MethodCall` IR node** — `Op::MethodCall { receiver, method, args }`
  added to core IR.
- [x] **2D array write: stack pop order** — Fixed. Stack layout `[value, dim2, dim1]`
  with dim1 on top.
- [x] **`button_click` structural corruption** — Post-dominator computation now
  recognizes empty blocks as function exits.

### High Priority (correctness)

- [x] **Break instruction Int32 extra operand** — All Break signals -1 through -11
  handled. Dead Estate: 3,477 → 981 errors.
- [x] **GMS2.3+ struct operations** — Dead Estate errors 2,167 → 81 (96%
  reduction). Child function lengths, type-aware Dup, spurious blocks, stack effects.
- [x] **GMS2.3+ remaining 81 errors** — `filter_reachable()` worklist fix +
  Bt/Bf end-of-function handling. Dead Estate: 2,167 → 0 errors.
- [x] **Branch offset encoding** — 23-bit signed offsets (was 24-bit).
- [x] **Switch statement reconstruction** — Frontend detects Dup+Cmp+BrIf chains.
- [x] **Named object singleton accessors** — Numeric IDs → named singletons.
- [x] **Self-referencing instance types** — Normalized to -1 (Own).
- [x] **Object persistent/visible flags** — Emit from OBJT as class fields.
- [x] **Parent class self-references** — `ancestor_indices` normalization.
- [x] **Stale namespace imports** — `strip_unused_namespace_imports()` post-process.
- [x] **Empty switch cases** — Reordered as fall-through labels.
- [x] **ButtonBase alarm/advantages/inventory** — Genuine cross-object references.

### Boolean / Short-Circuit Detection (completed)

- [x] **Stacktop-via-ref_type stack imbalance** — Fixed ref_type=0x80 handling.
- [x] **Empty `while (true) {}` loop bodies** — Fixed emitted set vs header.
- [x] **Guard clause drops continuation branch assigns** — Nested continuation.
- [x] **General loop block-param counter uses initial value** — BrIf self-loops
  + `var_is_reassigned` guard.
- [x] **Short-circuit `||`/`&&` emitted as nested ternaries** — Fixed via
  `try_logical_op` in structurizer.

### Type Inference Results

GML bytecode starts all-Dynamic. With `function_signatures` (~160 stdlib),
`type_definitions` (44 built-in fields), and `Struct(class_name)` self typing:

| Metric | Before | After |
|--------|--------|-------|
| `let: any` | 1736 → 8 | 2 |
| `(): any` return types | 205 → 10 | 7 |
| Total `: any` | ~1950 | 9 |

### Medium Priority (output quality)

- [x] **While → for loop promotion** — AST-level rewrite pass.
- [x] **Sprite constant resolution** — Both field defaults and dynamic assignments.
- [x] **Duplicate variable declarations** — Fixed locals map key.

## Twine Frontend — Completed Items

- [x] **Double-bracket array wrapping in navigation calls** — Fixed. (was 1211
  TRC, 62 DoL → 0)
- [x] **Dialog.wiki() lacks wikification** — Runtime Wikifier implemented.
- [x] **`<<nobr>>` compile-time bug** — `suppress_line_breaks` flag.
- [x] **`\n` → `<br>` fidelity bug** — Parser emits `NodeKind::LineBreak`.
- [x] **`NodeKind::LineBreak` is dead code** — No longer dead.
- [x] **Per-concern platform pluggability** — Split monolithic `browser.ts`
  into per-concern modules across all three runtimes.
- [x] **Link setter timing** — Setter callback functions built during extraction.
- [x] **Unparsed setter in passage name** — Parser bracket-depth tracking fixed.
- [x] **Raw SugarCube markup in text output** — `[img[...]]` parsed as
  `NodeKind::Image`.
- [x] **Widget call is async** — Static import of `getPassage`.
- [x] **Missing globals: Has, LoadScreen, Fullscreen, SimpleAudio** — Added.
- [x] **Passage.render() returns empty fragment** — Uses pushBuffer/popBuffer.
- [x] **MacroContext shadow methods are no-ops** — addShadow/createShadowWrapper
  implemented.
