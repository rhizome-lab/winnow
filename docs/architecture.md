# Architecture

## Philosophy

Reincarnate performs **full recompilation**, not interpretation. Source bytecode and scripts are decompiled into a typed intermediate representation, optimized, and compiled to native targets. The goal is accurate reproduction — preserve fidelity, don't redesign. When rendering can't be fully lifted, overlay a modern UI layer over the original rather than patching internal rendering.

## Pipeline

```
Source binary/scripts
        │
        ▼
   ┌──────────┐
   │ Frontend  │   (per-engine: Flash, Ren'Py, RPG Maker, etc.)
   └────┬─────┘
        │  untyped IR
        ▼
   ┌──────────┐
   │   Type    │
   │ Inference │
   └────┬─────┘
        │  typed IR
        ▼
   ┌──────────┐
   │ Transform │   (optimization passes, coroutine lowering, etc.)
   │  Passes   │
   └────┬─────┘
        │  optimized typed IR
        ▼
   ┌──────────┐
   │ Backend   │   (Rust source, TypeScript, etc.)
   └──────────┘
```

Frontends parse engine-specific formats and emit untyped IR. Type inference recovers concrete types. Transform passes optimize and lower (e.g., coroutines to state machines). Backends emit target code.

## Supported Engines

| Engine | Format | Strategy |
|--------|--------|----------|
| **Flash (AVM2)** | ABC bytecode | Full recompilation (first target) |
| **Ren'Py** | RPY scripts / RPYC bytecode | Full recompilation |
| **RPG Maker VX Ace** | Ruby/RGSS scripts | Full recompilation |
| **RPG Maker MV/MZ** | JSON event scripts + JS engine | Patch-in-place (compile JSON events, optimize engine, inject plugins) |
| **GameMaker** | GML bytecode | Full recompilation |
| **Director/Shockwave** | Lingo scripts | Full recompilation |
| **Twine / Inform** | Story formats | Full recompilation |
| **VB6** | P-Code | Full recompilation |
| **Java Applets** | JVM bytecode | Full recompilation |
| **Silverlight** | .NET IL | Full recompilation |
| **HyperCard / ToolBook** | Stack formats | Full recompilation |

RPG Maker MV/MZ is a special case: since the engine already runs in JavaScript, it may be more practical to compile event scripts and optimize the existing engine rather than full recompilation.

## Codegen Backends

### Rust Source (primary)

Emits `.rs` files that compile with `rustc`. Benefits:
- Monomorphization eliminates all generic overhead
- Const evaluation resolves static data at compile time
- LLVM optimizations apply automatically
- Native desktop and mobile via standard Rust targets
- WASM via `rustc --target wasm32-unknown-unknown`

Desktop is truly native — no Tauri, no WebView, no embedded browser. The generated Rust code links against `wgpu` + `winit` for rendering and windowing.

### TypeScript (secondary)

Emits `.ts` files for web deployment. Useful when:
- The target is a browser game/app
- WASM binary size is a concern
- Integration with existing web infrastructure is needed

## Type Inference

Flow-sensitive, Hindley-Milner-ish type recovery. Source languages (ActionScript 3, Lingo, GML, etc.) are often dynamically typed — inference recovers concrete types where possible.

When inference fails, the type becomes `Dynamic`: a tagged union that backends emit as an enum with runtime dispatch. The goal is to minimize `Dynamic` usage — most well-typed ActionScript 3 code should infer fully.

Key properties:
- **Flow-sensitive**: types narrow through conditionals and casts
- **Constraint-based**: unification over type variables
- **Fallback**: `Dynamic` when constraints are unsatisfiable or insufficient
- **Per-function**: inference runs per function, cross-function via signatures

## System Architecture

Pluggable systems via generic traits that monomorphize at compile time:

```rust
trait Renderer {
    type Texture;
    type Surface;
    fn draw_sprite(&mut self, texture: &Self::Texture, x: f32, y: f32);
    // ...
}
```

Systems are generic parameters on the game/app entry point. The compiler monomorphizes each combination — zero virtual dispatch at runtime. Systems can be swapped (e.g., inject touch controls for mobile, replace save/load UX, add accessibility overlays) without changing the core translated code.

### System Traits

| System | Responsibility |
|--------|---------------|
| **Renderer** | Sprite/shape/text drawing, display list |
| **Audio** | Sound/music playback, mixing |
| **Input** | Keyboard, mouse, touch, gamepad |
| **SaveLoad** | Persistence (save files, local storage) |
| **UI** | Dialogue boxes, menus, HUD overlays |
| **Timing** | Frame pacing, delta time, timers |

## IR Design

### Entity-Based Arenas

All IR nodes live in typed arenas (`PrimaryMap<K, V>`) and are referenced by `u32` indices:

```
FuncId(0) → Function { name: "main", blocks: [BlockId(0), BlockId(1)] }
BlockId(0) → Block { params: [ValueId(0)], insts: [InstId(0), InstId(1)] }
InstId(0) → Inst { op: Op::Const(42), result: ValueId(1) }
```

Cache-friendly, trivially serializable, no lifetimes or `Rc`/`Arc`.

### Block Arguments (not Phi Nodes)

Following Cranelift and MLIR, the IR uses block arguments instead of phi nodes. Simpler to construct from frontends, easier to reason about.

```
block0(v0: i32):
    v1 = add v0, 1
    br block1(v1)

block1(v2: i32):
    return v2
```

### Operations

The `Op` enum covers:
- **Constants**: integers, floats, strings, booleans, null
- **Arithmetic/logic**: standard ALU operations
- **Control flow**: branch, conditional branch, switch, return
- **Memory**: load, store, alloc, field access
- **Calls**: direct call, indirect call, system call
- **Type operations**: cast, type check, dynamic dispatch
- **Coroutines**: yield, create, resume
- **Aggregates**: struct/array/tuple construction and access

### SystemCall

```
SystemCall { system: "Renderer", method: "draw_sprite", args: [v0, v1, v2] }
```

String-based at IR level, resolved to concrete trait method calls at codegen. Keeps the `Op` enum from exploding with per-engine operations.

### Coroutines as First-Class

`Yield`, `CoroutineCreate`, `CoroutineResume` are IR operations. A transform pass lowers them to state machines before codegen. Different backends may lower differently:
- Rust: state machine enum or async/await
- TypeScript: generator functions

### Module Scope

Entity IDs are module-scoped: `FuncId(0)` in Module A ≠ `FuncId(0)` in Module B. Cross-module references use string-based imports. A linking pass builds the global symbol table.

## AST Normalization Passes

After the backend lowers IR to an AST (`Vec<Stmt>`), a pipeline of rewrite passes normalizes the output for readability. Passes run in a fixed order; some are in a fixpoint loop that iterates until no further changes occur.

### Cleanup Phase (one-shot, before fixpoint)

| Pass | Effect |
|------|--------|
| **eliminate_self_assigns** | Remove `x = x` |
| **eliminate_duplicate_assigns** | Collapse consecutive identical assignments |
| **eliminate_forwarding_stubs** | Remove uninit phi + immediate read into another phi |
| **invert_empty_then** | `if (x) {} else { B }` → `if (!x) { B }` |
| **eliminate_unreachable_after_exit** | Truncate dead code after return/break/continue or if-else where both branches exit |
| **rewrite_ternary** | `if (c) { x = a } else { x = b }` → `x = c ? a : b` |
| **rewrite_minmax** | `(a >= b) ? a : b` → `Math.max(a, b)` |

### Fixpoint Phase

Runs in a loop until statement count stabilizes:

| Pass | Effect |
|------|--------|
| **forward_substitute** | Inline single-use adjacent assignments |
| **rewrite_ternary** | (re-run to catch newly exposed patterns) |
| **simplify_ternary_to_logical** | `c ? x : c` → `c && x`, `c ? c : x` → `c \|\| x` |
| **absorb_phi_condition** | Merge split-path phi booleans into their assigning branch |
| **narrow_var_scope** | Push uninit `let` into the single child scope where all refs live |
| **merge_decl_init** | `let x; ... x = v` → `let x = v` |
| **fold_single_use_consts** | Inline single-use const/let declarations |

### Final Phase (one-shot, after fixpoint)

| Pass | Effect |
|------|--------|
| **rewrite_compound_assign** | `x = x + 1` → `x += 1` |
| **rewrite_post_increment** | Read-modify-write → `x++` |

## Multi-Platform Strategy

| Platform | Rendering | Windowing | Audio |
|----------|-----------|-----------|-------|
| Desktop (Linux/macOS/Windows) | wgpu | winit | TBD |
| Mobile (iOS/Android) | wgpu | winit | TBD |
| WASM | wgpu (WebGPU) | winit (canvas) | Web Audio API |
| Web (TypeScript backend) | Canvas/WebGL | DOM | Web Audio API |

Platform differences are abstracted at the system trait level. The same IR compiles to any target by swapping system implementations.

## Dependencies

- **`swf` crate** (from Ruffle, MIT/Apache-2.0): SWF file parsing for the Flash frontend
- **`thiserror`**: Structured error types in core
- **`serde` / `serde_json`**: Project manifest and asset catalog serialization
