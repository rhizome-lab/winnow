# PuzzleScript

**Status: Planned (low priority)** — Already a web engine; the main use case is archival and offline hosting of existing games.

## Background

PuzzleScript is an open-source browser-based puzzle game engine by Stephen Lavelle (increpare). Games are written in a domain-specific language and compiled to JavaScript in the browser. The engine is hosted at [puzzlescript.net](https://www.puzzlescript.net/). PuzzleScript Plus ([Auroriax/PuzzleScriptPlus](https://github.com/Auroriax/PuzzleScriptPlus)) extends it with additional features.

## Format

PuzzleScript games are **plain text** — a single `.puz` file or inline text divided into labeled sections:

```
TITLE My Game

OBJECTS
Background
black

Player
black red
.0...
.1...

LEGEND
P = Player
. = Background

SOUNDS
Player MOVE 12345

COLLISION LAYERS
Background
Player

RULES
[ Player | ... ] -> [ | Player ... ]

WIN CONDITIONS
No Player

LEVELS
.....
..P..
.....
```

Key sections:
- `OBJECTS` — 5×5 sprite definitions with named color palette and pixel grid
- `LEGEND` — single-character aliases for objects and combinations
- `SOUNDS` — sound event associations (bfxr seed numbers)
- `COLLISION LAYERS` — layer ordering for collision detection
- `RULES` — pattern-replacement rules (the core game logic): `[pattern] -> [replacement]`
- `WIN CONDITIONS` — victory predicates (`No X`, `Some X`, `All X on Y`)
- `LEVELS` — ASCII grid maps

There is no binary format. Published games are typically hosted as:
- The official editor URL with source encoded in the URL hash
- A self-contained HTML file with the source embedded and the engine bundled

## Runtime

PuzzleScript games are compiled to JavaScript at load time. The compiler (`compiler.js`) processes source text in ~16 passes:
1. Parsing sections
2. Legend expansion (character aliases → object sets)
3. Rule parsing (pattern matching with direction specifiers: `UP`, `DOWN`, `LEFT`, `RIGHT`, `HORIZONTAL`, `VERTICAL`, `ACTION`, `STATIONARY`, `MOVING`)
4. Direction expansion (relative → absolute — `HORIZONTAL` → two rules for left/right)
5. Rule simplification (redundant rules removed)
6. Bitmask generation (each cell is a bitmask of which objects are present)

The compiled output feeds into `engine.js` — a turn-based engine that:
1. Receives a player input (move direction or action)
2. Applies all `late` rules first, then `realtime` rules, then regular rules
3. Iterates rule application until fixpoint (no further rules fire in a pass)
4. Checks win conditions
5. Renders the board state

Rule matching uses bitmask operations for O(1) per-cell checking.

## Lifting Strategy

Full recompilation (Tier 2). Parse the PuzzleScript source, compile rules to IR functions, and emit clean TypeScript — not bundling the original compiler + engine.

1. Parse the source text (section-based format)
2. Compile rules to IR — each rule is a function that pattern-matches cell bitmasks and applies replacements; the fixpoint loop is an IR loop
3. Emit TypeScript: compact, fast rule functions + a thin runtime (renderer, input handler, level loader)

The rule matching semantics are formally specified and deterministic, making exact translation straightforward. The compiled output will be faster than the original engine's interpreted rule loop.

**Note: engine-as-is option**
The original PuzzleScript engine already runs in any browser. For one-off archival of an existing game, bundling the source + engine is trivial and needs no reincarnate frontend. But it ships the full original compiler at runtime, and the output isn't analysable or retargetable.

## What Needs Building

- [ ] Source extractor from saved HTML / URL hash (to recover the plain-text source)
- [ ] PuzzleScript source parser (section-based text format)
- [ ] Rule compiler → IR (bitmask pattern-matching functions + fixpoint loop)
- [ ] Level data extractor → asset catalog
- [ ] Replacement runtime (`runtime/puzzlescript/ts/`):
  - [ ] Grid renderer (5×5 sprite tiles on canvas)
  - [ ] Input handler (arrow keys / WASD / action key)
  - [ ] Level progression and win condition checking
  - [ ] Sound (bfxr seed → generated audio)

## References

- [PuzzleScript source (MIT)](https://github.com/increpare/PuzzleScript)
- [PuzzleScript Plus](https://github.com/Auroriax/PuzzleScriptPlus)
- [PuzzleScript documentation](https://www.puzzlescript.net/Documentation/documentation.html)
