# CLAUDE.md

Behavioral rules for Claude Code in this repository.

## Overview

Reincarnate is a legacy software lifting framework. It extracts and transforms applications from obsolete runtimes (Flash, Director, VB6, HyperCard, RPG Maker, etc.) into modern web-based equivalents.

### Key Components

- **Explant**: Bytecode/script extraction and decompilation
- **Hypha**: Game/app translation with UI overlay replacement

### The "Lift" Strategy

**Tier 1: Native Patching** - For binaries you can't lift (hex editing, font replacement)
**Tier 2: Runtime Replacement** - For engines you can shim (hook draw calls, render via HTML/CSS overlay)

### Supported Targets

Reincarnate works on **bytecode and script**, not native code:
- Flash (ABC bytecode)
- Director/Shockwave (Lingo)
- VB6 (P-Code)
- Java Applets (JVM bytecode)
- Silverlight (.NET IL)
- HyperCard/ToolBook (stack formats)
- RPG Maker / Ren'Py / GameMaker

## Core Rule

**Note things down immediately:**
- Bugs/issues → fix or add to TODO.md
- Design decisions → docs/ or code comments
- Future work → TODO.md
- Key insights → this file

**Triggers:** User corrects you, 2+ failed attempts, "aha" moment, framework quirk discovered → document **before** proceeding. Never describe a bug in conversation and move on — write it to TODO.md first, then continue working.

**Do the work properly.** When asked to analyze X, actually read X - don't synthesize from conversation.

## Behavioral Patterns

From ecosystem-wide session analysis:

- **Question scope early:** Before implementing, ask whether it belongs in this crate/module
- **Check consistency:** Look at how similar things are done elsewhere in the codebase
- **Implement fully:** No silent arbitrary caps, incomplete pagination, or unexposed trait methods. The test projects (`~/reincarnate/flash/cc/` for Flash, `~/reincarnate/gamemaker/bounty/` for GML) are example inputs, not the universe — fixing only the cases they exercise is a half measure. If a class of bug exists, fix the entire class. If a pattern applies to all properties on a type, apply it to all of them, not just the ones that happen to blow up today.
- **Name for purpose:** Avoid names that describe one consumer
- **Verify before stating:** Don't assert API behavior or codebase facts without checking

## Design Principles

**Unify, don't multiply.** One interface for multiple engines > separate implementations per engine. Plugin systems > hardcoded switches.

**Lazy extraction.** Don't parse everything upfront. Extract on demand, cache aggressively.

**Preserve fidelity.** The goal is accurate reproduction, not "improvement". Make the old thing work, don't redesign it.

**Overlay > Patch.** When possible, render a modern UI layer over the original rather than patching internal rendering.

**Two-tier approach.** Accept that some targets need binary patching (Tier 1) while others can be fully lifted (Tier 2). Design APIs that work for both.

## Runtime Architecture

Each engine runtime uses a three-layer architecture:

```
Layer 3: API Shims (engine-specific)     e.g. sugarcube/output.ts, flash/display.ts
  ↓ imports hookable primitives from
Layer 2: Platform Interface              platform/index.ts (re-exports active impl)
  ↓ re-exports from
Layer 1: Platform Implementation         platform/browser.ts
```

**Platform interfaces abstract hookable concerns** — things a game deployer would want to swap or intercept without modifying game code:

| Concern | Examples | Why abstracted |
|---------|----------|----------------|
| Persistence | save/load/remove | Swap localStorage for cloud saves, IndexedDB |
| Audio | create/play/pause/stop/fade | Swap HTMLAudioElement for Web Audio API |
| Timing | setTimeout/setInterval wrappers | Hook for pause/resume, speed control |
| Rendering | canvas context, draw calls | Swap 2D canvas for WebGL (Flash/GameMaker) |
| Input | keyboard/mouse/touch/gamepad | On-screen joysticks, gamepad support, click-to-move (RPG Maker) |
| Save UI | save slot presentation, autosave | Custom save menus, cloud sync dialogs |

**Direct DOM ops stay in shim modules** — Twine is fundamentally DOM-based, so `createElement`, `querySelector`, etc. are called directly in the sugarcube/ modules, not abstracted through the platform layer. Only concerns that benefit from swappability go through platform.

**Platform concern modules never import from sibling concern modules.** Each module (persistence, audio, input, dialog, save-ui, settings-ui, layout, etc.) is independently swappable — swapping one must not require updating another. Cross-concern dependencies are injected via callbacks (e.g. `initCommands(registerCommand)`, `init(showDialog, closeDialog)`) or wired at the integration point (`index.ts`). If module A needs functionality from module B, it receives it as a parameter, never via `import`. Two modules sharing an implementation detail (e.g. save-ui and settings-ui both happen to use dialog today) is never a reason to merge them — a replacement save-ui might not use dialogs at all.

Swap platforms by changing the re-export in `platform/index.ts` — zero runtime cost since it's just module aliasing.

## Workflow

**Batch cargo commands** to minimize round-trips:
```bash
cargo clippy --all-targets --all-features -- -D warnings && cargo test
```
After editing multiple files, run the full check once — not after each edit. Formatting is handled automatically by the pre-commit hook (`cargo fmt`).

**When making the same change across multiple crates**, edit all files first, then build once.

**Minimize file churn.** When editing a file, read it once, plan all changes, and apply them in one pass. Avoid read-edit-build-fail-read-fix cycles by thinking through the complete change before starting.

**Commit after every phase.** When a plan has multiple steps, commit after each step that passes `cargo clippy && cargo test`. Do not accumulate changes across phases. Do not defer commits to the end. Do not rationalize skipping commits because changes are "intertwined" or "small". Each commit should represent one logical unit of progress. This is non-negotiable.

**Use `normalize view` for structural exploration:**
```bash
~/git/rhizone/normalize/target/debug/normalize view <file>    # outline with line numbers
~/git/rhizone/normalize/target/debug/normalize view <dir>     # directory structure
```

## Commit Convention

Use conventional commits: `type(scope): message`

Types:
- `feat` - New feature
- `fix` - Bug fix
- `refactor` - Code change that neither fixes a bug nor adds a feature
- `docs` - Documentation only
- `chore` - Maintenance (deps, CI, etc.)
- `test` - Adding or updating tests

Scope is optional but recommended for multi-crate repos.

## Negative Constraints

Do not:
- Announce actions ("I will now...") - just do them
- Leave work uncommitted — commit after every phase, no exceptions
- Create special cases - design to avoid them
- Add to the monolith - split by domain into sub-crates
- Cut corners with fallbacks - implement properly for each case
- Write stubs that return null/undefined — implement the function or don't add it. A `return null` stub is a landmine that crashes at runtime with a misleading error. If the full implementation is complex, implement the subset that covers the actual usage patterns.
- Mark as done prematurely - note what remains
- Dismiss known issues as "fine for now" — if you discover a gap, mismatch, or missing implementation during your work, add it to TODO.md immediately. Do not assume future sessions will rediscover the same issue. Every known problem must be written down before moving on.
- Use path dependencies in Cargo.toml - causes clippy to stash changes across repos
- Use `--no-verify` - fix the issue or fix the hook
- Assume tools are missing - check if `nix develop` is available for the right environment

## CLI Usage

Run via cargo from the repo root:

```bash
# Full pipeline: extract SWF → IR → transform → emit TypeScript
cargo run -p reincarnate-cli -- emit --manifest ~/reincarnate/flash/cc/reincarnate.json

# Print human-readable IR (for debugging)
cargo run -p reincarnate-cli -- print-ir <ir-json-file>

# Extract IR only (no transforms/emit)
cargo run -p reincarnate-cli -- extract --manifest ~/reincarnate/flash/cc/reincarnate.json

# Show project manifest info
cargo run -p reincarnate-cli -- info --manifest ~/reincarnate/flash/cc/reincarnate.json
```

The `--manifest` flag defaults to `reincarnate.json` in the current directory. Use `--skip-pass` to disable specific transform passes (e.g. `--skip-pass type-inference --skip-pass constant-folding`).

Debug flags (on `emit` only):
- `--dump-ir` — dump post-transform IR to stderr before structurization
- `--dump-ast` — dump raw AST to stderr before AST-to-AST passes
- `--dump-function <substring>` — filter dumps to functions whose name contains the substring

Test projects live under `~/reincarnate/<engine>/<game>/`:
- Flash: `~/reincarnate/flash/cc/`
- GML (GMS1): `~/reincarnate/gamemaker/bounty/`
- GML (GMS2): `~/reincarnate/gamemaker/deadestate/`
- Twine: `~/reincarnate/twine/` (subfolders `dol/`, `trc/`)

## Crate Structure

All crates use the `reincarnate-` prefix:
- `reincarnate-core` - Core types and traits
- `reincarnate-cli` - CLI binary (named `reincarnate`)
- `reincarnate-frontend-flash` - Flash/SWF frontend (in `crates/frontends/`)
- `reincarnate-frontend-director` - Director/Shockwave frontend (in `crates/frontends/`)
- etc.
