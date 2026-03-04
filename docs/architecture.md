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
| **Twine (SugarCube)** | SugarCube macro DSL in HTML | Full recompilation |
| **Twine (Harlowe)** | Harlowe hook/macro syntax in HTML | Full recompilation |
| **Inform** | Story formats | Full recompilation |
| **VB6** | P-Code | Full recompilation |
| **Java Applets** | JVM bytecode | Full recompilation |
| **Silverlight** | .NET IL | Full recompilation |
| **HyperCard / ToolBook** | Stack formats | Full recompilation |

RPG Maker MV/MZ is a special case: since the engine already runs in JavaScript, it may be more practical to compile event scripts and optimize the existing engine rather than full recompilation.

### Twine story format coverage

The Twine frontend targets **SugarCube** and **Harlowe** only. Two other story
formats exist but are not supported:

- **Snowman** — Not a scripting language. Passages contain raw JavaScript with
  `<% %>` template tags and jQuery. There is no macro DSL to parse — it's
  already JavaScript. Lifting it would be pointless: the source is the target.
  Very few published games use it.

- **Chapbook** — Minimal adoption. Uses a unique "inserts + modifiers" syntax
  with no meaningful ecosystem. Community forum threads are mostly people
  looking for example games, which speaks to the format's obscurity. Not worth
  the parser investment until there's real demand.

SugarCube and Harlowe cover the overwhelming majority of Twine games in the
wild. SugarCube dominates large/complex projects; Harlowe is the Twine 2
default and is common for shorter works. Harlowe is arguably the higher-value
target — its runtime is slower, its save system is barely functional, and its
syntax actively resists extension and debugging.

## Library Replacement (HLE)

Reincarnate works like a console recompiler: user logic is faithfully
translated, but **original runtime libraries are detected and replaced** with
native equivalents. This is the HLE (High-Level Emulation) approach — instead
of emulating the original runtime instruction-by-instruction, we recognize
API boundaries and swap in modern implementations.

```
┌─────────────────────────────────────────────────────┐
│                   Original binary                    │
│  ┌──────────────┐  ┌─────────────────────────────┐  │
│  │  User logic   │  │  Runtime libraries           │  │
│  │  (game code)  │  │  (flash.display, flash.text) │  │
│  └──────┬───────┘  └──────────────┬──────────────┘  │
│         │                         │                  │
└─────────┼─────────────────────────┼──────────────────┘
          │                         │
          ▼                         ▼
   Recompiled to IR          Detected at boundary,
   → transforms              replaced with native
   → codegen                 implementation
          │                         │
          ▼                         ▼
   ┌──────────────┐  ┌─────────────────────────────┐
   │  Translated   │  │  Replacement runtime         │
   │  user code    │──│  (canvas renderer, DOM text,  │
   │  (.ts / .rs)  │  │   Web Audio, etc.)           │
   └──────────────┘  └─────────────────────────────┘
```

### Why this matters

Every legacy runtime has a standard library: Flash has `flash.display`,
`flash.events`, `flash.text`; Director has `Lingo` built-ins; VB6 has COM
controls and the VB runtime. These libraries define the app's interaction
with rendering, input, persistence, etc.

The recompiler's job is two-fold:
1. **Translate user logic** — faithful recompilation of the app's own code
2. **Replace runtime libraries** — swap the original runtime with a modern
   implementation that provides the same API surface

This separation is what makes the output actually *run*. Translating
`MovieClip.gotoAndStop(3)` to TypeScript is useless without a `MovieClip`
class that does the right thing. The replacement runtime IS the optimization.

### Library boundary detection

The frontend is responsible for identifying library boundaries. For Flash,
the `flash.*::` namespace cleanly separates stdlib from user code. The
frontend marks these references in the IR so downstream passes know which
calls cross the boundary.

What the frontend produces:

```
IR instruction:   GetField "flash.text::TextFormatAlign"
IR metadata:      external_lib = "flash.text", short_name = "TextFormatAlign"
```

The backend consumes the metadata to emit imports. It never parses namespace
strings — that's the frontend's domain knowledge.

### Three-layer runtime architecture

The replacement runtime has three distinct layers:

```
Emitted user code (.ts / .rs)
        │
        │  imports Flash API classes
        ▼
┌──────────────────────────────────────────────┐
│  API Shim Layer (engine-specific)            │
│  MovieClip, EventDispatcher, Loader, ...     │
│  Implements original engine semantics        │
│  flash/display.ts, flash/events.ts, ...      │
│                                              │
│  Owned by: engine × target language          │
│  (runtime/flash/ts/, runtime/flash/rs/)      │
└────────────────┬─────────────────────────────┘
                 │
                 │  calls platform abstraction
                 ▼
┌──────────────────────────────────────────────┐
│  Platform Interface                          │
│  draw_image(), fill_rect(), play_sound()     │
│  load_bytes(), save_persistent(), ...        │
│                                              │
│  Rust: generic traits (monomorphized)        │
│  TS: module re-exports (tree-shaken)         │
└────────────────┬─────────────────────────────┘
                 │
                 │  implemented by
                 ▼
┌──────────────────────────────────────────────┐
│  Platform Implementation (per-target)        │
│  BrowserCanvas2D, WebAudio, fetch/localStorage│
│  WgpuRenderer, CpalAudio, winit input        │
│  NullPlatform (testing)                      │
│                                              │
│  Selected at build time                      │
└──────────────────────────────────────────────┘
```

**Layer 1: API Shim** — Implements the original engine's class hierarchy and
semantics in the target language. Flash's `MovieClip` has frame timelines,
display list children, and event dispatching. Director's `Sprite` has cast
members and score-based animation. These are completely different APIs but
both call the same platform layer to draw, play sounds, and handle input.
The API shim is engine-specific but platform-agnostic.

**Layer 2: Platform Interface** — A thin abstraction over platform
capabilities: 2D rendering, audio playback, input polling, network I/O,
persistence. This is the boundary where swapping happens. The interface is
engine-agnostic — Flash and Director both call `draw_image()` to put pixels
on screen. In Rust, these are generic traits that monomorphize (zero virtual
dispatch). In TypeScript, these are module-level function re-exports that the
bundler resolves at build time (zero indirection — the unused implementation
is tree-shaken away entirely).

**Layer 3: Platform Implementation** — Concrete implementations of the
platform interface for each deployment target. Browser: Canvas 2D + Web
Audio + fetch + localStorage. Desktop: wgpu + cpal + winit + filesystem.
Testing: null platform that no-ops all I/O.

### Why three layers

The naive approach (API shim directly calls browser APIs) is what the current
runtime does. This breaks down when:

- **Multiple deployment targets** — The same Flash game should run in a
  browser (Canvas 2D) and as a native desktop app (wgpu). Without the
  platform interface, you'd need separate API shim implementations per
  target, duplicating all the engine logic.
- **Testing** — You can't unit-test a display list implementation that calls
  `ctx.fillRect()` without a browser. With the platform interface, inject a
  null platform and test pure logic.
- **Progressive optimization** — Swap `BrowserCanvas2D` for `BrowserWebGL`
  or `BrowserWebGPU` without touching the API shim. Swap the full-fidelity
  `flash.events` (capture/target/bubble) for a flat-dispatch version. Each
  layer swaps independently.

### Swappable runtime packages

Each engine's API shim can have multiple implementations at different fidelity
levels:

| Package | Stub (correctness) | Full (faithful) | Optimized (native) |
|---------|-------------------|-----------------|-------------------|
| `flash.display` | JS class hierarchy | Full display list | Canvas2D direct draw, skip display list |
| `flash.text` | DOM `<div>` measurement | TextField with format runs | Pre-measured glyph atlas |
| `flash.events` | Simple listener map | Full capture/bubble | Flat listener dispatch |
| `flash.net` | localStorage wrapper | SharedObject + URLLoader | IndexedDB with sync API |

A project starts with stub implementations and progressively replaces
individual packages with optimized versions — same user code, better runtime.
The platform interface ensures implementations only change what's below the
boundary.

### Runtime ownership

The replacement runtime is **neither frontend nor backend**. It's a third
concern:

- The **frontend** extracts bytecode and emits IR. It identifies library
  boundaries (`flash.*::` namespaces) and attaches metadata. It does not
  implement any runtime classes.
- The **backend** emits target code and wires up imports to the runtime
  package. It does not know what `MovieClip` does — it just knows the import
  path.
- The **runtime** implements engine semantics in the target language, calling
  the platform interface for I/O. It is standalone — usable independently of
  the recompiler pipeline.

File layout:

```
runtime/
  flash/
    ts/                    ← Flash replacement runtime (TypeScript)
      flash/
        display.ts         ← MovieClip, Stage, DisplayObject, ...
        events.ts          ← EventDispatcher, Event subclasses
        text.ts            ← TextField, TextFormat, ...
        net.ts             ← URLLoader, SharedObject, ...
        utils.ts           ← ByteArray, Timer, Dictionary, ...
      platform/
        interface.ts       ← Platform interface (exported functions)
        browser.ts         ← Browser implementation (Canvas 2D, fetch, ...)
        null.ts            ← Null implementation (testing)
    rs/                    ← Flash replacement runtime (Rust, future)
      src/
        display.rs
        events.rs
        ...
```

### Pipeline with library replacement

```
Source binary
      │
      ▼
┌──────────┐
│ Frontend  │ → IR + library boundary metadata
└────┬─────┘
     │
     ▼
┌──────────┐
│ Transforms│ → optimized IR (user logic only; library calls untouched)
└────┬─────┘
     │
     ▼
┌──────────┐     ┌───────────────────┐
│ Backend   │ ←── │ Replacement runtime │  (selected per engine × target)
└────┬─────┘     └───────────────────┘
     │
     ▼
  Output (.ts/.rs) + runtime package
```

The backend's job: emit user code + wire up imports to the replacement
runtime. The frontend's job: detect library boundaries and attach metadata.
The runtime's job: make `MovieClip.gotoAndStop(3)` actually work.

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

## Platform Interface

The platform interface is the boundary between engine-specific logic (display
lists, event systems, animation timelines) and target-specific I/O (canvas
drawing, audio playback, keyboard input, file access). It must be:

- **Zero-cost** — In Rust, generic traits monomorphize to direct calls. In
  TypeScript, module-level function re-exports resolve at build time (the
  bundler tree-shakes the unused implementation).
- **Swappable** — Different implementations for different deployment targets.
  The same Flash runtime runs on a browser (Canvas 2D + Web Audio) or native
  desktop (wgpu + cpal) by swapping the platform implementation.
- **Low-level** — The interface provides 2D drawing primitives, not engine
  concepts. It has `draw_image()` and `fill_rect()`, not `add_to_display_list()`.
  Engine semantics (display lists, event bubbling, frame timelines) live in
  the API shim layer above, not here.

### Platform capabilities

The platform interface is a **cross-language contract** — TypeScript, Rust, C#/Unity, SDL all implement the same conceptual interface. Names below are the canonical snake_case form; TypeScript implementations camelCase them. API surfaces use only primitive types (int, float, bool, string, opaque handles) — no language-specific types (`AudioBuffer`, `HTMLImageElement`) in exported signatures.

#### Graphics (2D)

**Opaque handle types** (u32, defined in shared types module): `CanvasHandle`, `FontHandle`, `PathHandle`

All hot-tier draw operations take an explicit `CanvasHandle` — there is no implicit "active canvas" state. Multiple canvases can be drawn to in the same frame without set/restore ceremony.

Colors are RGBA packed as `0xRRGGBBAA`. Matches WebGL convention, maps directly to GPU formats with no swizzling. Engines using other packing (e.g. Flash ARGB) convert in the shim.

Per-draw effect state (`set_alpha`, `set_blend_mode`, `set_color_transform`, `set_image_smoothing`) lives on the canvas state stack — saved and restored with `save_state`/`restore_state`. Not parameters on draw calls (avoids draw_image becoming a bag of effects). `set_transform` follows the same pattern.

Text alignment and baseline are parameters on `draw_text`, not state — they are intrinsic to where text lands, not an effect layered on top.

**Named constant types** (closed sets — Rust `enum`, TS const union, C# `enum`):
- `BlendMode`: `normal | additive | multiply | screen | erase`
- `TextAlign`: `left | center | right`
- `TextBaseline`: `top | middle | bottom | alphabetic`

Note: Graphics 3D uses `Blend` (not `BlendMode`) since the valid set differs: `none | alpha | additive | premultiplied`.

**Setup tier:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `init_canvas` | `(id: str) → CanvasHandle` | Bind to existing canvas element by ID |
| `create_canvas` | `(w, h: int) → CanvasHandle` | Create offscreen canvas |
| `resize_canvas` | `(canvas: CanvasHandle, w, h: int) → void` | |
| `canvas_width` | `(canvas: CanvasHandle) → int` | Current canvas width in pixels |
| `canvas_height` | `(canvas: CanvasHandle) → int` | Current canvas height in pixels |
| `load_font` | `(url: str) → FontHandle` (async) | Load and register a font |
| `create_path` | `() → PathHandle` | Begin recording a reusable path |
| `path_move_to` | `(path: PathHandle, x, y: float) → void` | |
| `path_line_to` | `(path: PathHandle, x, y: float) → void` | |
| `path_bezier_to` | `(path: PathHandle, cp1x, cp1y, cp2x, cp2y, x, y: float) → void` | Cubic bezier |
| `path_quadratic_to` | `(path: PathHandle, cpx, cpy, x, y: float) → void` | Quadratic bezier |
| `path_arc` | `(path: PathHandle, x, y, r, start, end: float, ccw: bool) → void` | |
| `path_close` | `(path: PathHandle) → void` | |
| `destroy_path` | `(path: PathHandle) → void` | Free path resources |
| `destroy_canvas` | `(canvas: CanvasHandle) → void` | Free canvas resources |
| `destroy_font` | `(font: FontHandle) → void` | Free font resources |

**Hot tier — state (all saved/restored with save_state/restore_state):**

| Function | Signature | Notes |
|----------|-----------|-------|
| `set_transform` | `(canvas: CanvasHandle, a,b,c,d,e,f: float) → void` | Set CTM (6-element affine) |
| `set_alpha` | `(canvas: CanvasHandle, alpha: float) → void` | Global opacity multiplier (0..1) |
| `set_blend_mode` | `(canvas: CanvasHandle, mode: BlendMode) → void` | |
| `set_color_transform` | `(canvas: CanvasHandle, matrix: float[20]) → void` | 4×5 RGBA multiply+add matrix. Implementation note: Canvas 2D has no native color transform. Implement via an LRU cache of color-transformed images (see reference implementation in `~/git/bounty`). |
| `set_image_smoothing` | `(canvas: CanvasHandle, enabled: bool) → void` | false = nearest-neighbor (pixel art) |
| `save_state` | `(canvas: CanvasHandle) → void` | Push transform + clip + effect state |
| `restore_state` | `(canvas: CanvasHandle) → void` | Pop state |

**Hot tier — drawing:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `clear_canvas` | `(canvas: CanvasHandle, color: int) → void` | Fill entire canvas with color; resets clip |
| `fill_rect` | `(canvas: CanvasHandle, x,y,w,h: float, color: int) → void` | RGBA `0xRRGGBBAA` |
| `draw_image` | `(canvas: CanvasHandle, img: ImageHandle, sx,sy,sw,sh, dx,dy,dw,dh: float) → void` | Source and dest rects |
| `draw_canvas` | `(dst: CanvasHandle, src: CanvasHandle, sx,sy,sw,sh, dx,dy,dw,dh: float) → void` | Composite offscreen canvas onto another |
| `draw_text` | `(canvas: CanvasHandle, text: str, x,y: float, font: FontHandle, size: float, color: int, align: TextAlign, baseline: TextBaseline) → void` | |
| `measure_text` | `(text: str, font: FontHandle, size: float) → float` | Text width; no canvas needed |

**Hot tier — one-off paths (canvas-local, not reusable):**

| Function | Signature | Notes |
|----------|-----------|-------|
| `begin_path` | `(canvas: CanvasHandle) → void` | Start a new path |
| `move_to` | `(canvas: CanvasHandle, x, y: float) → void` | |
| `line_to` | `(canvas: CanvasHandle, x, y: float) → void` | |
| `bezier_to` | `(canvas: CanvasHandle, cp1x, cp1y, cp2x, cp2y, x, y: float) → void` | |
| `quadratic_to` | `(canvas: CanvasHandle, cpx, cpy, x, y: float) → void` | |
| `arc` | `(canvas: CanvasHandle, x, y, r, start, end: float, ccw: bool) → void` | |
| `close_path` | `(canvas: CanvasHandle) → void` | |
| `fill_path` | `(canvas: CanvasHandle, color: int) → void` | Fill current path |
| `stroke_path` | `(canvas: CanvasHandle, color: int, width: float) → void` | Stroke current path |
| `clip` | `(canvas: CanvasHandle) → void` | Clip to current path |
| `begin_text_path` | `(canvas: CanvasHandle, text: str, x, y: float, font: FontHandle, size: float) → void` | Add glyph outlines to current path; then fill/stroke/clip as normal |

**Hot tier — reusable paths:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `fill_path_handle` | `(canvas: CanvasHandle, path: PathHandle, color: int) → void` | |
| `stroke_path_handle` | `(canvas: CanvasHandle, path: PathHandle, color: int, width: float) → void` | |
| `clip_path_handle` | `(canvas: CanvasHandle, path: PathHandle) → void` | |

#### Graphics 3D

A **separate platform concern** from `graphics` (2D). A 2D-only engine pays nothing for 3D init; a headless test renderer can omit it entirely. Never import from or into the 2D graphics concern.

**Named constant types** (closed sets — Rust `enum`, TS const union, C# `enum`):
- `LoadOp`: `clear | load`
- `StoreOp`: `store | discard`
- `Primitive`: `triangles | lines | points`
- `Blend`: `none | alpha | additive | premultiplied` (distinct from 2D `BlendMode` — different valid set)
- `CullMode`: `none | back | front`

**Opaque handle types** (u32, cross-language safe):
- `GpuHandle` — a 3D rendering context (one per game instance; surface binding is implementation-defined)
- `ShaderHandle` — compiled shader program (identified by name; impl provides source)
- `LayoutHandle` — interned vertex layout descriptor
- `MeshHandle` — uploaded vertex + index buffer pair
- `TextureHandle` — GPU texture
- `RenderTargetHandle` — framebuffer / render texture (0 = main framebuffer)
- `UniformHandle` — resolved uniform location (name resolved at setup time; used on hot path)

**Design decisions:**
- **Uniforms**: named string API at the call site; implementation resolves name→(block, offset) at shader compile time via reflection and batches into a UBO. Callers never see binding indices.
- **Uniform lifetime**: persistent per shader object — set once, reused across draws until changed.
- **Shader creation**: sync ID, async compile. `create_shader` returns immediately; compilation happens in the background. Poll `shader_ready`. Draw calls against an unready shader are silently no-oped — the engine is responsible for not drawing until ready. Supports streaming (shaders not known at init time).
- **Vertex layout**: interned via `create_vertex_layout(format: str) → LayoutHandle`. Format string parsed once; `LayoutHandle` used everywhere else. Format: attribute letter + component count, e.g. `"p3n3uv2"` = position(3) + normal(3) + uv(2).
- **Render passes**: explicit `begin_pass`/`end_pass` with `load_op`/`store_op` declared upfront — required for tile-based GPU efficiency on mobile. Implicit `set_render_target` silently assumes load+store, which wastes bandwidth on tile GPUs.
- **Context handle**: `GpuHandle` is returned by `init_gfx3d()`. What surface or window it binds to is implementation-defined — on browsers it is implicit (e.g. WebGL context on the document canvas); on native targets it is a window handle passed at construction. Callers never see the surface; they hold only the opaque handle.

**Setup tier:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `init_gfx3d` | `() → GpuHandle` | Create a 3D context; surface/window binding is implementation-defined |
| `create_shader` | `(gpu: GpuHandle, name: str) → ShaderHandle` | Sync ID; async compile in background |
| `shader_ready` | `(gpu: GpuHandle, id: ShaderHandle) → bool` | Poll until true before drawing |
| `get_uniform` | `(gpu: GpuHandle, shader: ShaderHandle, name: str) → UniformHandle` | Resolve name at setup time; hot-path uses the returned handle |
| `create_vertex_layout` | `(gpu: GpuHandle, format: str) → LayoutHandle` | Intern once; use ID everywhere |
| `create_mesh` | `(gpu: GpuHandle, layout: LayoutHandle, vertices: float[], indices: int[]) → MeshHandle` | Upload geometry |
| `update_mesh` | `(gpu: GpuHandle, id: MeshHandle, vertices: float[]) → void` | Dynamic geometry update |
| `destroy_mesh` | `(gpu: GpuHandle, id: MeshHandle) → void` | |
| `upload_image` | `(gpu: GpuHandle, img: ImageHandle) → TextureHandle` | Upload a CPU-side image to GPU texture |
| `create_texture` | `(gpu: GpuHandle, w: int, h: int, data: int[]) → TextureHandle` | RGBA8 pixels |
| `destroy_texture` | `(gpu: GpuHandle, id: TextureHandle) → void` | |
| `create_render_target` | `(gpu: GpuHandle, w: int, h: int) → RenderTargetHandle` | |
| `destroy_render_target` | `(gpu: GpuHandle, rt: RenderTargetHandle) → void` | |
| `render_target_texture` | `(gpu: GpuHandle, id: RenderTargetHandle) → TextureHandle` | Read result as texture |
| `destroy_shader` | `(gpu: GpuHandle, shader: ShaderHandle) → void` | |

**Hot tier:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `begin_pass` | `(gpu: GpuHandle, rt: RenderTargetHandle, color_load: LoadOp, color_store: StoreOp, depth_load: LoadOp, depth_store: StoreOp) → void` | |
| `end_pass` | `(gpu: GpuHandle) → void` | |
| `set_uniform_float` | `(gpu: GpuHandle, uniform: UniformHandle, v: float) → void` | |
| `set_uniform_vec2` | `(gpu: GpuHandle, uniform: UniformHandle, x,y: float) → void` | |
| `set_uniform_vec3` | `(gpu: GpuHandle, uniform: UniformHandle, x,y,z: float) → void` | |
| `set_uniform_vec4` | `(gpu: GpuHandle, uniform: UniformHandle, x,y,z,w: float) → void` | |
| `set_uniform_mat4` | `(gpu: GpuHandle, uniform: UniformHandle, m: float[16]) → void` | Column-major |
| `set_uniform_texture` | `(gpu: GpuHandle, uniform: UniformHandle, tex: TextureHandle) → void` | |
| `draw_mesh` | `(gpu: GpuHandle, mesh: MeshHandle, shader: ShaderHandle, prim: Primitive) → void` | |
| `set_viewport` | `(gpu: GpuHandle, x,y,w,h: int) → void` | |
| `set_depth_test` | `(gpu: GpuHandle, enabled: bool) → void` | |
| `set_depth_write` | `(gpu: GpuHandle, enabled: bool) → void` | |
| `set_blend` | `(gpu: GpuHandle, mode: Blend) → void` | |
| `set_cull` | `(gpu: GpuHandle, mode: CullMode) → void` | |

#### Audio

The audio platform is a **node graph**. The signal path is:

```
[BufferHandle] → [Voice (gain+pan)] → [NodeGraph: user DAG] → [Master (node 0)]
```

Voices are per-play instances; the node graph is a user-defined DAG of DSP nodes
established at init time. Playing a voice routes its output to a designated sink node.
Fixed topologies (Buffer→Bus→Master) are special cases of the graph, not the interface.

**Opaque handle types** (u32, cross-language safe):
- `BufferHandle` — decoded audio data (`load_audio` assigns IDs = SOND index)
- `NodeHandle` — a DSP node in the graph (0 = master output, always valid)
- `VoiceHandle` — a playing voice instance (0 = invalid). VoiceHandles are monotonically increasing and never reused. Any operation on an invalid or stopped VoiceHandle is a silent no-op.
- `GroupHandle` — a voice group for bulk operations

**Node kinds**: `gain | pan | low_pass | high_pass | band_pass | notch | compressor | reverb | delay | mixer`

**`ParamKind`** — all param values are floats; no wrapper struct needed:

| ParamKind | Unit | Notes |
|-----------|------|-------|
| `gain` | linear (0..∞) | amplitude multiplier |
| `pan` | -1..1 | -1=L, 0=C, 1=R |
| `cutoff` | Hz | filter cutoff frequency |
| `resonance` | Q | filter resonance |
| `wet_mix` | 0..1 | reverb/delay wet blend |
| `decay` | seconds | reverb tail length |
| `delay_time` | seconds | echo delay |
| `feedback` | 0..1 | echo feedback |
| `threshold` | dBFS | compressor threshold (negative) |
| `ratio` | x:1 | compressor ratio |
| `attack` | seconds | compressor/envelope attack |
| `release` | seconds | compressor/envelope release |
| `knee` | dB | compressor knee width |

Each node kind accepts only its own params; others throw at runtime.

**Setup tier** (graph construction, init-time, not perf-critical):

| Function | Signature | Notes |
|----------|-----------|-------|
| `load_audio` | `(sounds: [{name,url}]) → void` (async) | Decode all sounds; assigns BufferHandles |
| `audio_ready` | `() → bool` | True when AudioContext is running and playback is possible; poll before first play call |
| `create_node` | `(kind: NodeKind) → NodeHandle` | Create a DSP node |
| `create_voice_group` | `() → GroupHandle` | Create a group for bulk voice operations |
| `connect` | `(from: NodeHandle, to: NodeHandle) → void` | Add DAG edge: from.output → to.input |
| `disconnect` | `(from: NodeHandle, to: NodeHandle) → void` | Remove edge |
| `set_node_param` | `(node: NodeHandle, kind: ParamKind, value: float, fade_ms: float) → void` | Set/animate a node param |
| `get_node_param` | `(node: NodeHandle, kind: ParamKind) → float` | Read current param value |
| `destroy_node` | `(node: NodeHandle) → void` | |
| `destroy_group` | `(group: GroupHandle) → void` | |

**Hot tier** (per-frame voice control, all args are primitives — zero object allocation at call site):

| Function | Signature | Notes |
|----------|-----------|-------|
| `play` | `(buffer: BufferHandle, sink: NodeHandle, loop: bool, gain: float, pitch: float, pan: float, offset: float) → VoiceHandle` | Play, route through sink |
| `stop` | `(voice: VoiceHandle) → void` | Stop and discard |
| `stop_all` | `() → void` | Stop all voices |
| `pause` | `(voice: VoiceHandle) → void` | Pause, remember position |
| `resume` | `(voice: VoiceHandle) → void` | Resume from pause position |
| `resume_all` | `() → void` | Resume all paused voices |
| `is_playing` | `(voice: VoiceHandle) → bool` | True if playing (not paused, not ended) |
| `is_paused` | `(voice: VoiceHandle) → bool` | |
| `on_voice_end` | `(voice: VoiceHandle, cb: () → void) → void` | Register a one-shot callback fired when the voice finishes or is stopped |
| `set_voice_gain` | `(voice: VoiceHandle, gain: float, fade_ms: float) → void` | Per-voice gain |
| `get_voice_gain` | `(voice: VoiceHandle) → float` | |
| `set_voice_pitch` | `(voice: VoiceHandle, pitch: float, fade_ms: float) → void` | Playback rate |
| `get_voice_pitch` | `(voice: VoiceHandle) → float` | |
| `set_voice_pan` | `(voice: VoiceHandle, pan: float) → void` | Stereo pan |
| `get_voice_pan` | `(voice: VoiceHandle) → float` | |
| `set_master_gain` | `(gain: float) → void` | Shorthand for set_node_param(0, gain) |
| `get_position` | `(voice: VoiceHandle) → float` | Playback position in seconds |
| `set_position` | `(voice: VoiceHandle, pos: float) → void` | Seek |
| `buffer_duration` | `(buffer: BufferHandle) → float` | Duration in seconds |
| `stop_node` | `(node: NodeHandle) → void` | Stop all voices routed to node |
| `pause_node` | `(node: NodeHandle) → void` | Pause all voices routed to node |
| `resume_node` | `(node: NodeHandle) → void` | Resume all paused voices routed to node |
| `add_to_group` | `(group: GroupHandle, voice: VoiceHandle) → void` | Add voice to group |
| `remove_from_group` | `(group: GroupHandle, voice: VoiceHandle) → void` | Remove voice from group |
| `stop_group` | `(group: GroupHandle) → void` | Stop all voices in group |
| `pause_group` | `(group: GroupHandle) → void` | Pause all voices in group |
| `resume_group` | `(group: GroupHandle) → void` | Resume all paused voices in group |
| `set_group_gain` | `(group: GroupHandle, gain: float, fade_ms: float) → void` | Set gain on all voices in group |

Engine-specific audio behaviors (exclusive channels, BGM crossfade, voice stealing by priority,
named channels) are shim concerns — implemented by composing the above primitives, not baked
into the platform interface.

**Non-web backends**: the node graph maps directly to Web Audio. For SDL/cpal/Unity targets, implement with [miniaudio](https://miniaud.io) as the DSP engine and a lightweight graph runner on top. The graph interface is intentionally kept as the canonical surface — expressivity for our own frontends outweighs the porting cost of writing the backend once.

#### Input

**Named constant types**:
- `DeviceKind`: `keyboard | mouse | touch | gamepad`
- `CursorShape`: `default | pointer | text | crosshair | move | resize_ns | resize_ew | none`

Every input source carries a `device: int` ID. Device 0 is the primary/default for each kind. Multiple devices of the same kind (two keyboards, two mice, split controllers) are distinguished by ID. Use `devices(kind)` to enumerate on init; `on_device_connect`/`on_device_disconnect` handle changes after init.

Gamepad buttons surface through the keyboard callbacks with synthetic codes (`"Button0"`, `"Button1"`, etc.) on their device ID — engines that just want "button pressed" get it for free. Raw analog state uses `device_axis`.

`on_mouse_move` delivers absolute canvas coordinates. When pointer-locked, use `on_mouse_delta` for relative motion — these are distinct concerns and must not be conflated.

**Device enumeration:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `devices` | `(kind: DeviceKind) → int[]` | IDs of connected devices of that kind |
| `on_device_connect` | `(cb: (device: int, kind: DeviceKind) → void) → void` | |
| `on_device_disconnect` | `(cb: (device: int) → void) → void` | |

**Keyboard:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `on_key_down` | `(cb: (device: int, code: str, key: str) → void) → void` | `code` = physical key position; `key` = character produced |
| `on_key_up` | `(cb: (device: int, code: str, key: str) → void) → void` | |
| `is_key_down` | `(device: int, code: str) → bool` | Poll current state |

**Mouse:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `on_mouse_down` | `(cb: (device: int, button: int) → void) → void` | |
| `on_mouse_up` | `(cb: (device: int, button: int) → void) → void` | |
| `on_mouse_move` | `(cb: (device: int, x, y: float) → void) → void` | Absolute canvas coordinates |
| `on_scroll` | `(cb: (device: int, dx, dy: float) → void) → void` | Both axes |
| `is_mouse_down` | `(device: int, button: int) → bool` | |
| `mouse_x` | `(device: int) → float` | |
| `mouse_y` | `(device: int) → float` | |

**Pointer lock:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `request_pointer_lock` | `() → void` | |
| `release_pointer_lock` | `() → void` | |
| `is_pointer_locked` | `() → bool` | |
| `on_mouse_delta` | `(cb: (device: int, dx, dy: float) → void) → void` | Relative motion; only meaningful while locked |

**Touch:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `on_touch_start` | `(cb: (device: int, id: int, x, y: float) → void) → void` | `id` = touch point ID within device |
| `on_touch_move` | `(cb: (device: int, id: int, x, y: float) → void) → void` | |
| `on_touch_end` | `(cb: (device: int, id: int, x, y: float) → void) → void` | |
| `touch_count` | `(device: int) → int` | Active touch points |
| `touch_x` | `(device: int, id: int) → float` | |
| `touch_y` | `(device: int, id: int) → float` | |

**Gamepad (analog):**

| Function | Signature | Notes |
|----------|-----------|-------|
| `device_axis` | `(device: int, axis: int) → float` | Analog axis value (-1..1); buttons use keyboard callbacks |

**Text input:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `on_text_input` | `(cb: (text: str) → void) → void` | Fired with composed text (post-IME); separate from key events — use for text entry fields |

**Window focus and visibility:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `on_focus_change` | `(cb: (focused: bool) → void) → void` | Fired when the game window gains or loses focus |
| `on_visibility_change` | `(cb: (visible: bool) → void) → void` | Fired when the page/tab becomes hidden or visible; use to pause the game loop |

**Cursor:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `set_cursor` | `(shape: CursorShape) → void` | Set the OS cursor appearance; `none` hides the cursor |

#### Images

`ImageHandle` is an opaque u32. It is defined in a shared types module — not owned by the images concern or the graphics concern. This is what allows `graphics_3d` to accept `ImageHandle` in `upload_image` without importing from the images concern.

Sub-images are views into a parent — no copy. `image_width`/`image_height` on a sub-image return the sub-region dimensions. `destroy_image` on a sub-image releases the view, not the parent. `destroy_image` on a **parent** that still has live sub-images throws — callers must destroy sub-images before the parent.

`format` in `load_image_bytes` is a MIME type string (`"image/png"`, `"image/webp"`, etc.), or `null` to request format sniffing from magic bytes. Use explicit format when known; `null` as an escape hatch for raw blobs with no format information.

**Setup tier** (async):

| Function | Signature | Notes |
|----------|-----------|-------|
| `load_image_url` | `(url: str) → ImageHandle` | Decode image from URL |
| `load_image_bytes` | `(data: bytes, format: str \| null) → ImageHandle` | Decode from raw bytes; `null` format = sniff |
| `create_sub_image` | `(parent: ImageHandle, x, y, w, h: int) → ImageHandle` | View into parent; no copy |

**Query** (sync):

| Function | Signature | Notes |
|----------|-----------|-------|
| `image_width` | `(handle: ImageHandle) → int` | |
| `image_height` | `(handle: ImageHandle) → int` | |
| `read_pixels` | `(handle: ImageHandle, x, y, w, h: int) → bytes` | RGBA8; throws if unsupported by backend |

**Lifecycle:**

| Function | Signature | Notes |
|----------|-----------|-------|
| `destroy_image` | `(handle: ImageHandle) → void` | Releases view (sub-image) or backing data (root image) |

#### Persistence

A key-value byte store. "Save" is a shim-level concept; the platform is just storage. Naming reflects that: `store`/`fetch`/`remove`, not `save`/`load`.

**Contract:**
- `init` is async — preloads cache, initialises backing store. After `init` returns, all reads are sync.
- `store` and `remove` are **atomic** — on failure, the old value remains intact. Never a partial write. Implementations use write-to-temp-then-rename (OPFS, filesystem) or the natural atomicity of `localStorage.setItem`.
- `store` **throws on failure** — never swallows errors silently. Callers decide how to handle quota exceeded, permission denied, etc.
- Data is **bytes**, not strings — strings are just UTF-8 bytes; the more general interface subsumes the string case.
- Backend composition (OPFS + localStorage tee, fallback, debounce, rolling history) is above the platform layer. A `fallback(primary, secondary)` utility wrapper handles degradation — the platform interface itself does not decide which backend to use.

| Function | Signature | Notes |
|----------|-----------|-------|
| `init` | `() → void` (async) | Preload cache from backing store |
| `store` | `(key: str, data: bytes) → void` | Atomic replace; throws on failure |
| `fetch` | `(key: str) → bytes \| null` | Sync read from in-memory cache |
| `remove` | `(key: str) → void` | Atomic; throws on failure |
| `list` | `(prefix: str) → str[]` | Enumerate keys with given prefix |

#### Timing

Handles are typed opaque u32s — distinct types prevent mixing delayed/recurring/frame handles. Zero runtime cost (branded types in TS, newtype structs in Rust).

`request_frame` is the game loop driver — register once, fires every vsync until cancelled. The callback receives a monotonic timestamp so the loop can compute delta time without a separate call.

**Handle types**: `DelayHandle`, `RecurringHandle`, `FrameHandle` (all u32)

| Function | Signature | Notes |
|----------|-----------|-------|
| `schedule_delayed` | `(cb: () → void, delay_ms: float) → DelayHandle` | One-shot; cb fires once after delay |
| `cancel_delayed` | `(handle: DelayHandle) → void` | No-op if already fired or invalid |
| `schedule_recurring` | `(cb: () → void, interval_ms: float) → RecurringHandle` | Repeating; fires until cancelled |
| `cancel_recurring` | `(handle: RecurringHandle) → void` | |
| `request_frame` | `(cb: (time_ms: float) → void) → FrameHandle` | Vsync driver; fires every frame until cancelled; `time_ms` is monotonic |
| `cancel_frame` | `(handle: FrameHandle) → void` | |
| `current_time_ms` | `() → float` | Monotonic clock — for game timing, delta computation |
| `current_wall_time_ms` | `() → float` | Wall clock — for save file timestamps |

#### Window

Window and display management. Fullscreen state is async on browsers (requires user gesture); poll `is_fullscreen` or use the callback.

| Function | Signature | Notes |
|----------|-----------|-------|
| `request_fullscreen` | `() → void` | Request fullscreen; may be deferred until next user gesture |
| `exit_fullscreen` | `() → void` | |
| `is_fullscreen` | `() → bool` | |
| `on_fullscreen_change` | `(cb: (fullscreen: bool) → void) → void` | Fired when fullscreen state changes |
| `window_width` | `() → int` | Current window/viewport width in pixels |
| `window_height` | `() → int` | Current window/viewport height in pixels |

#### Clipboard

Text-only clipboard access. `read_clipboard` is async and may require a user permission grant on browsers.

| Function | Signature | Notes |
|----------|-----------|-------|
| `read_clipboard` | `() → str` (async) | Read current clipboard text; throws if permission denied |
| `write_clipboard` | `(text: str) → void` | Write text to clipboard |

#### Network

Minimal HTTP client. Sufficient for leaderboards, analytics, and asset streaming. More complex patterns (WebSockets, SSE) are composed above this layer.

| Function | Signature | Notes |
|----------|-----------|-------|
| `fetch_url` | `(url: str, method: str, headers: {str: str}, body: bytes \| null) → bytes` (async) | HTTP request; throws on network error or non-2xx status |

### Rust: generic traits

```rust
trait Graphics {
    type Image;
    fn set_transform(&mut self, matrix: [f32; 6]);
    fn fill_rect(&mut self, x: f32, y: f32, w: f32, h: f32, color: u32);
    fn draw_image(&mut self, image: &Self::Image, x: f32, y: f32, w: f32, h: f32);
    fn draw_text(&mut self, text: &str, x: f32, y: f32, font: &str, size: f32);
    // ...
}

trait Audio {
    fn create_node(&mut self, kind: NodeKind) -> NodeHandle;
    fn connect(&mut self, from: NodeHandle, to: NodeHandle);
    fn set_node_param(&mut self, node: NodeHandle, kind: ParamKind, value: f32, fade_ms: f32);
    fn play(&mut self, params: PlayParams) -> VoiceHandle;  // struct avoids 8-arg limit
    fn stop(&mut self, voice: VoiceHandle);
    fn set_voice_gain(&mut self, voice: VoiceHandle, gain: f32, fade_ms: f32);
    // ...
}

trait PersistenceBackend {
    async fn store(&self, key: &str, value: &[u8]) -> Result<()>;
    async fn fetch(&self, key: &str) -> Result<Option<Vec<u8>>>;
    async fn remove(&self, key: &str) -> Result<()>;
    async fn list(&self, prefix: &str) -> Result<Vec<String>>;
}
```

The Flash API shim is generic over the platform:

```rust
struct MovieClip<G: Graphics, A: Audio> {
    children: Vec<DisplayObjectRef>,
    current_frame: u32,
    // ...
}
```

The compiler monomorphizes each `<G, A>` combination — zero virtual dispatch.
Platform implementations (`WgpuGraphics`, `Canvas2DGraphics`) are concrete
types that implement the traits.

### TypeScript: module re-exports

```typescript
// platform/index.ts — resolved at build time
export { setTransform, fillRect, drawImage, drawText, ... } from "./browser";
```

The API shim imports platform functions directly:

```typescript
// flash/display.ts
import { setTransform, fillRect, drawImage } from "../platform";

export class DisplayObject {
    render(): void {
        setTransform(this.transform.matrix);
        // ... draw children
    }
}
```

The bundler resolves `platform/index.ts` to the chosen implementation at
build time. Tree-shaking eliminates the unused one. The result is direct
function calls with zero indirection — equivalent to inlining the browser
API calls, but with a clean abstraction boundary.

To swap implementations, change the re-export source or use bundler path
aliasing (Vite, webpack, esbuild all support this). No runtime cost.

### Relationship to current system traits

The `system/` traits in `reincarnate-core` (Renderer, Audio, Input, SaveLoad,
Timing, Ui) were designed as the platform interface but at the wrong
abstraction level. `Renderer::draw_sprite()` assumes a sprite-based engine,
but Flash uses a display list. `Ui::show_message()` is an engine-level
concept, not a platform capability.

The platform interface should be redesigned as:
- **Graphics** replaces **Renderer** — 2D drawing primitives, not sprite
  operations. The display list is the API shim's concern.
- **Audio** stays as-is — sound buffer management is platform-level.
- **Input** stays as-is — key/mouse state is platform-level.
- **Persistence** replaces **SaveLoad** — simpler key-value API without
  serde dependency (serialization is the API shim's concern).
- **Timing** stays as-is — frame pacing is platform-level.
- **Ui** is removed — dialogue boxes and menus are engine-level, not
  platform-level. Each engine's API shim implements its own UI system
  using the graphics and input capabilities.

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

## Backend AST Design

Each backend defines its own AST types (e.g. `JsStmt`/`JsExpr` for TypeScript) separate from the core `Stmt`/`Expr`. This two-type design is intentional:

**Why not a single unified AST?** Backends need language-specific constructs (`new`, `typeof`, `in`, `delete`, `throw`, `super.*` for JS) that don't belong in core. Polluting core with JS-isms would violate engine neutrality.

**Why not a generic/parameterized AST?** `Expr<Ext>` parameterized over an extension type still requires a deep copy when converting between `Expr<CoreExt>` and `Expr<JsExt>`, since every `Box` child changes type. No zero-copy win.

**Why not a builder trait?** If the linearizer constructed through `trait AstSink`, it could produce backend AST directly — no intermediate core AST. But the AST normalization passes (forward substitution, ternary rewrite, constant folding, etc.) need to pattern-match and transform a concrete tree. The builder is write-only. Since the core AST is needed for the passes regardless, the builder can't eliminate it.

**Current design:** IR → core AST → normalization passes → mechanical lowering → backend AST → engine-specific rewrites → printer. The mechanical lowering (`lower.rs`) is a boring O(n) tree copy — the cost of having two type hierarchies. Engine-specific rewrites (e.g. Flash) operate on the backend AST (`JsExpr` → `JsExpr`), keeping them decoupled from both the core types and the printer.

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

| Platform | Graphics | Audio | Input | Persistence |
|----------|----------|-------|-------|-------------|
| Desktop (Linux/macOS/Windows) | wgpu | cpal | winit | filesystem |
| Mobile (iOS/Android) | wgpu | cpal | winit | filesystem |
| WASM (Rust backend) | wgpu (WebGPU) | Web Audio | winit (canvas) | IndexedDB |
| Web (TypeScript backend) | Canvas 2D / WebGL | Web Audio | DOM events | localStorage |

Platform differences are abstracted at the platform interface level. The same
API shim (engine runtime) runs on any target by swapping the platform
implementation. For Rust targets, the platform traits monomorphize. For
TypeScript, the bundler resolves the platform module at build time.

## Persistence & Save System

See [Persistence & Saving](persistence.md) for the full design. Key points:

- **Save state** (current moment) and **history** (undo stack) are separate
  concerns with independent persistence and retention policies.
- The engine calls `commit(state)` after each transition; the platform handles
  persistence strategy (when, where, how many copies).
- Three composable axes: **state transforms** (what to save), **timing** (when
  to save), **backends** (where to save). Deployers compose these at init.
- History supports **diff-based** (store only changed variables per transition)
  or **snapshot-based** (full clone) strategies, chosen per game's state size.
- Default: continuous autosave with debouncing, upgrading every engine's
  original save system.

## Architecture Decision Records

See [adr/](adr/) for design decisions:

- [ADR 001: Harlowe Content Emission via `h` Parameter](adr/001-harlowe-h-parameter.md)

## Dependencies

- **`swf` crate** (from Ruffle, MIT/Apache-2.0): SWF file parsing for the Flash frontend
- **`thiserror`**: Structured error types in core
- **`serde` / `serde_json`**: Project manifest and asset catalog serialization
