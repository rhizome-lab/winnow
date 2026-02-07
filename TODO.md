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
- [ ] Rust codegen backend (emit `.rs` files from typed IR — **blocked on constraint-based inference**)
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
- [ ] **Constraint-based solving** — generate type constraints from operations
  (e.g., `Add(a, b)` → `a: Numeric, b: Numeric, result: Numeric`), solve via
  unification. Replaces the ad-hoc forward propagation with a principled system
  that handles backward flow (e.g., argument used as `number` constrains the
  caller's variable).
- [ ] **Flow-sensitive narrowing** — narrow types after guards
  (`if (x instanceof Foo)` → `x: Foo` in then-branch). Requires per-block type
  environments rather than the current single `value_types` map. SSA form helps
  here — the BrIf arms can carry different type contexts.
- [ ] **Flash frontend: emit concrete types** — AVM2 bytecode has type
  annotations on locals, parameters, fields, return types. `resolve_type`
  failures cause unnecessary `Dynamic` entries. Fix the frontend to preserve
  what the source already knows, so inference only needs to handle what's
  genuinely untyped.
- [ ] **Flash frontend: extract local variable names** — We're throwing away
  two sources of name information that the swf crate already parses:
  (1) `MethodParam.name` — parameter names, gated behind `HAS_PARAM_NAMES`;
  (2) `Op::Debug` opcodes — map register indices to variable names
  (e.g., register 3 = `"player"`). Currently both are discarded. The output
  uses synthesized names like `v42` instead of the original `player`, `score`,
  etc. This is the single highest-impact change for output readability.
- [ ] **Untyped frontend validation** — test the inference pipeline against a
  fully-untyped IR (simulating Lingo/HyperCard) to verify it can reconstruct
  useful types from usage patterns alone.
