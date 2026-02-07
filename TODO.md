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
  `MethodParam.name` and `Op::Debug` opcodes. Names propagate through Mem2Reg
  and appear in TypeScript output.
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
