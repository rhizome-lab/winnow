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
ternary → minmax → [fixpoint: forward_sub → ternary → absorb_phi → narrow → merge →
fold] → compound_assign → post_increment. Debug name propagation through
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
self-assign elimination) continue unchanged on the output AST.

Benefits: eliminates `lazy_inlines`, `side_effecting_inlines`,
`always_inlines`, `se_flush_declared`, `skip_loop_init_assigns`, and the
flush mechanisms. Pure inlining is provably correct (no ordering concerns).
Side-effect handling is isolated. `LinearStmt` is thinner than the AST
(ValueId refs vs String names) — net memory reduction vs current `LowerCtx`.

### Optimizations — Future Directions

The current pipeline focuses on faithful decompilation: reproduce what the
original source did, with clean variable names and readable structure. Further
optimization passes could improve output quality but each carries correctness
risks that need careful analysis.

#### Safe (no semantic change)

- [ ] **Redundant type casts** — Eliminate `as number` etc. when the expression
  already has the target type. Pure type-level, no runtime effect.
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
