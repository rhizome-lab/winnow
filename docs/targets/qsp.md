# QSP (Quest Soft Player)

**Status: Planned** — No implementation started. Open-source reference engine available.

## Background

QSP is a Russian interactive fiction engine widely used for text adventures and eroge in Russian-speaking communities. Games are written in QSP-lang, a BASIC-like scripting language. The engine is open source under GPLv2.

## Format

- **`.qsp`** — compiled binary format. Essentially the source text XOR-scrambled with a key, stored in a simple binary envelope with per-location headers. Not true bytecode — the runtime parses text at load time.
- **`.qsps`** — plain text format (UTF-8). Functionally identical to `.qsp` but unobfuscated. Development files are typically `.qsps`; distributed games are usually `.qsp`.

Both formats contain **locations** — named blocks analogous to rooms/passages in other IF engines. Each location has optional `act` (action) blocks and an `onuse` (default action) block.

The `@qsp/converters` npm package provides documented read/write functions for both formats. The text format is fully documented in the QSP documentation.

Games may embed images, sounds, and HTML in the location text.

## QSP-lang

BASIC-like scripting language with:
- Variables (strings and numbers — no type declarations; `$varname` for string, `varname` for numeric)
- `IF ... THEN ... ELSEIF ... ELSE ... END` / `act 'text': ...`
- `goto 'location'` / `gosub 'location'` / `xgoto 'location'` (inter-location navigation)
- `msg 'text'` / `p 'text'` / `pl 'text'` — text output
- `*pl` / `*p` — clear output / append to output
- `addobj 'name'` / `delobj 'name'` / `killobj` — inventory management
- `setact 'text', 'code'` — add action button
- `menu 'title'` — display menu
- `play 'file'` — audio playback
- `showimage 'file'` — display image
- `copyarr` / `sortarr` — array operations
- `input 'prompt'` — text input

## Lifting Strategy

Full recompilation (Tier 2). The goal is emitted TypeScript (or Rust) per location — not bundling the libqsp interpreter.

1. Decode the `.qsp` binary (XOR key is known; `@qsp/converters` handles this) or read `.qsps` directly
2. Parse QSP-lang source text per location
3. Emit IR — each location is a function, `goto` becomes a call/jump, `act` becomes a choice registration
4. Run the standard transform pipeline
5. Emit TypeScript with a thin QSP runtime shim

**Note: WASM alternative exists**
`libqsp` (GPLv2 C engine) can be compiled to WASM via Emscripten. This works for quick deployment but ships an interpreter loop — not emitted code, not backend-agnostic, opaque to analysis.

## What Needs Building

### Format Parser (new crate: `reincarnate-frontend-qsp`)

- [ ] `.qsp` decoder — XOR descramble + binary envelope parser (reference: `@qsp/converters`)
- [ ] QSP-lang text parser:
  - Location declarations
  - Statement list: `IF`, `act`, `goto`/`gosub`/`xgoto`, output (`msg`, `p`, `pl`, `*pl`, `*p`), `addobj`/`delobj`, `setact`, `play`, `showimage`, `input`, `menu`
  - Expressions: arithmetic, string concatenation (`&`), comparison, `and`/`or`/`no`
  - Built-in functions: `len($str)`, `mid($str, pos, len)`, `ucase($str)`, `lcase($str)`, `str(num)`, `val($str)`, `rand(min, max)`, `max(a, b)`, `min(a, b)`, `arrsize(arr)`, `loc($name)`, etc.

### IR Mapping

- Location → function
- `goto 'loc'` → `Op::Br` or tail call to location function
- `gosub 'loc'` → `Op::Call`
- `act 'text': code end` → `SystemCall("QSP.RegisterAction", text, function)` inline
- `msg 'text'` → `SystemCall("QSP.Output", text)` + implicit yield
- `addobj / delobj` → `SystemCall("QSP.Inventory.*")`
- `showimage 'file'` → `SystemCall("QSP.ShowImage", file)`
- `input 'prompt'` → `SystemCall("QSP.Input", prompt)` + `Yield`

### Replacement Runtime (`runtime/qsp/ts/`)

- [ ] Text output buffer (HTML-formatted)
- [ ] Action list (buttons/links)
- [ ] Inventory panel
- [ ] Image display
- [ ] Menu dialog
- [ ] Text input dialog
- [ ] Audio playback
- [ ] Save/load (variable state + current location)
- [ ] `gosub` call stack

## References

- [QSPFoundation/qsp — reference C engine (GPLv2)](https://github.com/QSPFoundation/qsp)
- [QSP documentation](https://qsp.org/docs/)
- [@qsp/converters (npm) — format documentation](https://www.npmjs.com/package/@qsp/converters)
- [QSP — IFWiki](https://www.ifwiki.org/QSP)
