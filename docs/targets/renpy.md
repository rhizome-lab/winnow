# Ren'Py

**Status: Planned** — No implementation started.

## Format

Ren'Py is a Python-based visual novel engine. Games ship with:
- **`.rpy`** — source scripts (human-readable)
- **`.rpyc`** — compiled bytecode (Python marshal format — essentially Python `.pyc`)
- **`game/`** — directory containing scripts, images, audio, video, fonts
- **`renpy/`** — bundled copy of the Ren'Py engine (Python source)

Compiled `.rpyc` files contain Python bytecode (CPython marshal). They can be decompiled with unrpyc (open source). Source `.rpy` files are often present alongside compiled files in distributed games.

The Ren'Py script language is a DSL layered on Python:
- `label name:` — passage/scene entry points
- `scene bg_name` / `show character_name` — display commands
- `play music "file"` / `play sound "file"` — audio commands
- `menu:` / `"Choice text":` — player choice menus
- `jump label` / `call label` / `return` — control flow
- `$ python_expression` — inline Python
- `[python_expression]` — interpolated expression in dialogue
- `with transition_name` — visual transitions
- `define` / `default` — variable declarations

## Lifting Strategy

Full recompilation (Tier 2).

1. Prefer `.rpy` source if available; fall back to decompiled `.rpyc`
2. Parse the Ren'Py DSL — it's a superset of Python indented block syntax
3. Emit IR per `label` (each label is a function with jump/call/return)
4. Identify `renpy.*` API boundaries (display, audio, store variables)
5. Model Ren'Py's execution model: statement-by-statement coroutine with rollback

The key complexity is Ren'Py's **rollback system** — the engine can roll back through arbitrary script history for undo. This is implemented via Python generators in the original; in the lifted version it maps to the IR coroutine model (`Yield`/`CoroutineCreate`/`CoroutineResume`).

## What Needs Building

### Format Parser (new crate: `reincarnate-frontend-renpy`)

- [ ] `.rpy` source parser — Ren'Py DSL (statement grammar, expression grammar)
  - Script statements: `label`, `scene`, `show`, `hide`, `play`, `stop`, `queue`, `menu`, `jump`, `call`, `return`, `if/elif/else`, `while`, `for`, `with`, `image`, `define`, `default`, `init`, `python`
  - ATL (Animation and Transformation Language) — `at` clauses, transform definitions
  - Inline Python (`$` and `python:` blocks) — need Python expression parser or delegate to Python AST
- [ ] `.rpyc` decoder — Python marshal format for CPython 2.x/3.x
- [ ] Asset catalog extraction (images, audio, video references from script)

### IR Mapping

- `label name:` → function
- `jump label` → `Op::Call` or `Op::Br` to target function
- `call label` → `Op::Call` with return point
- `$ expr` → inline IR from Python expression
- `menu: "choice": ...` → `Op::SystemCall("RenPy.ShowMenu", choices)` + branch on result
- `show char at pos with trans` → `Op::SystemCall("RenPy.Show", ...)`
- `scene bg with trans` → `Op::SystemCall("RenPy.Scene", ...)`
- `play music "file"` → `Op::SystemCall("RenPy.PlayMusic", ...)`
- Rollback → IR coroutine model with `Yield` at each displayable statement

### Replacement Runtime (`runtime/renpy/ts/`)

- [ ] Display layer: character sprites (show/hide/at), scene backgrounds, z-ordering
- [ ] Dialogue box: name + text rendering with text tags (`{b}`, `{color=}`, `{i}`, etc.)
- [ ] Choice menu: list of options → player selects → callback
- [ ] Audio: background music (loop, fade), sound effects, voice
- [ ] Transitions: fade, dissolve, pixellate, slide, wipe, etc. (CSS animations or Canvas)
- [ ] ATL transforms: position, rotation, scale, alpha, animation timeline
- [ ] Rollback: history stack of yields, undo/redo navigation
- [ ] Save/load: serialize current yield point + all `store.*` variables
- [ ] Text tags: `{b}`, `{i}`, `{u}`, `{s}`, `{color=}`, `{size=}`, `{alpha=}`, `{font=}`, `{image=}`, `{nw}` (no-wait)
- [ ] NVL mode: multi-line message overlay
- [ ] `renpy.input()` — text input box
- [ ] `renpy.pause()` — wait for click/time
- [ ] `renpy.notify()` — notification popup

### Python Integration

**Primary approach (reincarnate): transpile**
Unpack `.rpa`, decompile `.rpyc` with `unrpyc` to recover `.rpy` source, then lower the Ren'Py AST to IR. Each `label` becomes a function; `scene`/`show`/`play`/`menu` become SystemCalls; rollback points become `Yield` ops. The existing transform pipeline and TypeScript backend produce clean emitted code — analysable, optimisable, and eventually targetable to Rust.

**Note: WASM/interpreter alternatives exist**
[renpyweb](https://github.com/renpy/renpyweb) (official, as of Ren'Py 7.4+) ships the full CPython engine as WASM. [Pyodide](https://pyodide.org/) is a heavier alternative. Both run the original interpreter loop — output is not emitted code, can't target Rust, and is opaque to analysis. These are fine for quick one-off deployment but are not the reincarnate approach.

## Known Challenges

- **ATL (Animation and Transformation Language)** — complex timeline/transform DSL; requires its own parser and animation system
- **Python interop** — `init python:` blocks and `$` statements may contain arbitrary Python; full fidelity requires Pyodide or a Python→TS transpiler
- **Persistent variables** — `persistent.*` variables survive across game sessions and may differ from `store.*` save variables
- **Custom displayables** — games can define `renpy.Displayable` subclasses in Python; these require Python execution
- **Screens language** — the `screen` statement (UI screens) is a separate DSL for constructing UI from widgets; complex to model

## References

- [Ren'Py Documentation](https://www.renpy.org/doc/html/)
- [Ren'Py Source (MIT)](https://github.com/renpy/renpy)
- [unrpyc decompiler](https://github.com/CensoredUsername/unrpyc)
