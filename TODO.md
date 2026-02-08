# TODO

## Next Up

- [x] **IR builder API** — Convenience layer in `reincarnate-core` for constructing functions, blocks, and instructions without manually managing entity IDs. Every frontend needs this.
- [x] **IR printer** — Human-readable text format for dumping IR (like LLVM `.ll` or Cranelift CLIF). Essential for debugging frontends and transforms.
- [x] **CLI scaffolding** — `reincarnate-cli` crate with clap. Parse a project manifest, load source files, print info. Wire up the pipeline trait plumbing.
- [x] **Flash frontend** — `reincarnate-frontend-flash` crate. AVM2 bytecode extraction and decompilation using Ruffle's `swf` crate (MIT/Apache-2.0). First real target.

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

- [ ] **Out-of-SSA variable coalescing** — SSA creates a new ValueId for each
  definition (`v11 = Math.round(damage)` instead of `damage = Math.round(damage)`).
  Multiple SSA values sharing the same debug name should be coalesced back into a
  single mutable variable when they don't interfere (aren't live simultaneously).
  This is the single biggest readability gap vs ffdec. The information is all in
  `value_names` — we just need to use it during AST lowering. Standard out-of-SSA
  (phi elimination + copy coalescing via interference graph).

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

### Low Priority (polish)

- [ ] **Redundant type casts** — Eliminate `as number` etc. when the expression
  already has the target type.
- [ ] **Inline closures** — Filter/map callbacks extracted as named function
  references instead of being inlined as arrow functions.
- [ ] **Condition inversion** — Structurizer sometimes inverts conditions.
  Not a bug but reads backward vs the original source.
