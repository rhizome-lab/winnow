# CLAUDE.md

Behavioral rules for Claude Code in this repository.

## Overview

Reincarnate is a legacy software lifting framework. It extracts and transforms applications from obsolete runtimes (Flash, Director, VB6, HyperCard, RPG Maker, etc.) into modern web-based equivalents.

### Key Components

- **Explant**: Bytecode/script extraction and decompilation
- **Hypha**: Game/app translation with UI overlay replacement

### Goals

- **High-performance, maintainable emitted code** — The output is compiled TypeScript (or Rust) functions, not a bundled interpreter. Shipped output should be readable, optimisable, and auditable.
- **Multiple backends** — The IR + transform pipeline is backend-agnostic. TypeScript is the current backend; Rust codegen is planned. Design choices that couple output to a single runtime target are wrong.

**Corollary: type inference belongs in the IR, not in a backend.** When inferring types (lambda param types, return types, collection element types), that work must happen in the frontend translator, the IR transform passes, or a dedicated inference pass — not inside the TypeScript printer or any other backend-specific code. A backend may *optimize display* (e.g. omit `: any` annotations when TypeScript can contextually infer), but it must never be the *source of truth* for what type a value has. The IR's `FunctionSig` / `Type` annotations are the record of what we know — backends read from that record, they don't create it.

**Corollary: never suggest bundling an existing interpreter as the lift strategy.** Tools like inkjs, Parchment, renpyweb, or libqsp-WASM produce running games but not emitted code. They are fine for one-off deployment but are not the reincarnate approach. When an engine already has a web interpreter, note it as a "quick deploy" alternative — not as the goal.

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

**Something unexpected is a signal, not noise.** When a result is surprising — output that's 12x larger than input, a file that shouldn't contain JS but does, a number that's off by an order of magnitude — stop immediately and ask why. Don't accept the anomaly and move on. Surprising results are almost always early evidence of a bug. Treating them as "close enough" or "probably fine" is how bugs stay hidden for sessions. Investigate before proceeding.

**Update CLAUDE.md the moment you find a systematic gap.** If a bug reveals that the existing rules didn't prevent a class of mistake, add the rule immediately — not after the fix, not at the end of the session. The rule is only useful if it's written before the next similar mistake can happen.

**A user correction means CLAUDE.md failed.** When the user has to correct your approach — not a factual mistake, but a wrong *way of thinking* about a problem — that is direct evidence that the rules didn't catch it. Don't just fix the immediate thing. Ask: what rule, if it had existed, would have prevented this? Write that rule before proceeding. A correction that doesn't produce a new rule is a correction that will happen again.

**Do the work properly.** When asked to analyze X, actually read X - don't synthesize from conversation.

## Behavioral Patterns

From ecosystem-wide session analysis:

- **CLAUDE.md updates are immediate priority.** When the user asks to update CLAUDE.md or documentation, drop everything and do it NOW. Context rots — defer even one task and the insight is lost. This overrides any in-progress work.
- **Question scope early:** Before implementing, ask whether it belongs in this crate/module
- **Check consistency:** Look at how similar things are done elsewhere in the codebase
- **Implement fully:** No silent arbitrary caps, incomplete pagination, or unexposed trait methods. The test projects (`~/reincarnate/flash/cc/` for Flash, `~/reincarnate/gamemaker/bounty/` for GML) are example inputs, not the universe — fixing only the cases they exercise is a half measure. If a class of bug exists, fix the entire class. If a pattern applies to all properties on a type, apply it to all of them, not just the ones that happen to blow up today. In a multi-stage pipeline, each stage independently encodes its own assumptions about the data it processes — fixing the assumption in one stage doesn't fix it in the others. When you correct a wrong assumption or add a new concept, grep for all the places that assumption lives (parser, translator, emitter, …) before closing the task.
- **Name for purpose:** Avoid names that describe one consumer
- **Verify before stating:** Don't assert API behavior or codebase facts without checking
- **Consider tests for bug fixes:** When fixing a bug that has a clear, self-contained reproduction — especially in compiler passes (transforms, structurizer, AST passes, emit) — write a regression test. Not every fix warrants a test (runtime stubs, config changes, one-off wiring), but if the bug can recur from future code changes, a test prevents that. Err on the side of writing the test if unsure.
- **Tests verify contract, not implementation.** A regression test must assert what the code *should* do — the externally observable behavior — not just document what it currently does. Writing a test that passes by construction (mirroring the implementation) defeats the purpose: it will still pass after the bug is reintroduced. If you discover that writing the test *correctly* causes it to fail, that means the code is wrong — mark the test `#[ignore = "known bug: <description>"]` and add it to TODO.md, but do not adjust the assertion to match the broken behavior.
- **Treat special-casing as a smell:** When a fix adds a narrow guard (`if this_specific_case { continue }`) to a pass, stop and ask whether the pass's core logic is wrong. A special case that prevents one crash often means the pass's assumptions are too broad — fix the assumption, not the symptom. Use `git blame` on the file to check whether a cluster of special-case guards have accumulated around the same function; that pattern indicates a deeper design gap.
- **Use git blame for audits:** When fixing a bug in a pass, `git blame` the surrounding function to see if other guards were patched in after the fact. A function with multiple post-hoc `continue` guards is likely fragile — the guards compensate for an incomplete model rather than fixing it. Audit the whole function, not just the line that broke.
- **Don't hand-roll what a library does.** When a well-maintained crate exists for a standard (HTML entities, Unicode normalization, MIME types, etc.), use it. Hand-rolling a partial reimplementation is not "simpler" — it's an incomplete, untested copy of someone else's work that will silently produce wrong results. The question isn't "is my list complete?" — it's "why am I writing this at all?"
- **Use the right abstraction level.** When a library provides both a high-level API and a low-level one, the default is the high-level API. Only drop to the low level when you have a concrete, specific reason (e.g. streaming, custom state machine, performance). Using an HTML tokenizer to query a document when a DOM API exists is the same mistake as hand-rolling string parsing when a parser exists — you're doing the library's job for it, badly. If you find yourself building state machines on top of a tokenizer, ask whether a higher-level API (e.g. `scraper`, `parse_document`) would make the state machine unnecessary.
- **Pre-existing bugs are bugs.** When investigating errors and you discover a bug that existed before your change, don't dismiss it as "pre-existing" or "not caused by my changes". Fix it if possible. If fixing it would derail the current task, add it to TODO.md as **critical priority** immediately — do not assume a future session will rediscover it.
- **Match the target's semantics.** When reimplementing a runtime's behavior (SugarCube expression parsing, GML bytecode decoding, etc.), check what the original runtime actually does. Don't guess at semantics or invent a "simplified" version. Read the source if available (SugarCube is open source, UndertaleModTool is open source).
- **Never widen runtime types to accommodate buggy game code.** TypeScript errors in emitted code that result from game author mistakes (wrong argument counts, wrong types) are correct behavior — they surface real bugs in the source material. Do not make function signatures more lenient (optional parameters, broader unions, `any`) to silence these errors. The emitted TypeScript should be as strictly typed as semantics allow. Use `git blame` to audit the runtime files periodically: any type that was widened to fix a "game error" is a red flag.

## Design Principles

**Correctness over copouts.** When a design problem surfaces, solve it correctly — don't paper over it with string substitution, special-case hacks, or "simplest thing that works". A correct solution may take more time, but a hacky one accumulates debt that costs more later. When two things are fundamentally different (e.g. SugarCube and Harlowe), model them as different — don't force-merge them because they share a container format.

**Unify, don't multiply.** One interface for multiple engines > separate implementations per engine. Plugin systems > hardcoded switches.

**Eliminate megamorphic dispatch at compile time.** When a runtime method dispatches on a string literal (e.g. `math("round", x)`, `str_op("trimmed", s)`), the backend rewrite pass must resolve the dispatch to a direct, monomorphic call — either a JS built-in (`Math.round(x)`) or a named function in a typed namespace object (`StringOps.trimmed(s)`). The namespace must be exported `as const` so TypeScript statically verifies that every dispatch name maps to an existing method. A missing method in the namespace is a compile-time error, not a silent runtime `undefined`. Pattern: (1) define pure functions, (2) export `as const` namespace, (3) add rewrite arm in `rewrites/twine/engine.rs`, (4) add test. Existing namespaces: `Math` (built-in), `Collections`, `Colors`, `StringOps`.

**Compose, don't enumerate.** Enums are almost always the wrong solution for configurable behavior. An enum is a closed, mutually exclusive set — it forces exactly one choice from a fixed menu, and adding a new option requires modifying the type definition. The right solution is usually a composable pipeline of wrappers (middleware/decorator pattern), where each concern is an independent transform that can be applied in any combination. Example: CSS isolation isn't `"none" | "scoped-css" | "shadow-dom" | "iframe"` — it's a list of composable wrappers `[scopedCSS(), shadowDOM(), iframe()]`, because you might want shadow DOM *and* an iframe simultaneously. Open composition > closed enumeration.

**Lazy extraction.** Don't parse everything upfront. Extract on demand, cache aggressively.

**Preserve fidelity.** The goal is accurate reproduction, not "improvement". Make the old thing work, don't redesign it.

**100% API compatibility is the target.** Every method, property, and behavior of the source runtime must be implemented — not just the ones exercised by current test projects. Test projects are examples, not the specification. If a method exists in the original API, it belongs in the runtime. "Our test game doesn't use it" is never a reason to skip or deprioritize an API method.

**Game author errors are not our bugs.** When emitted TypeScript produces a type error that faithfully reflects a bug in the original game's source code (e.g. using `|` instead of `||`, multiplying by `false`), that is correct behavior — reincarnate accurately reproduced what the author wrote. Do not "fix" the emission to suppress the diagnostic. Changing `|` to `||` alters semantics; annotating with `ts-ignore` hides a real bug; widening types to `any` throws away information. The right response is: document it as a game author error and move on. The only exception is if the error is caused by an imprecision in our type inference (e.g. we inferred `boolean` where the game intended `number`) — in that case, fix the inference, not the symptom.

**Overlay > Patch.** When possible, render a modern UI layer over the original rather than patching internal rendering.

**Two-tier approach.** Accept that some targets need binary patching (Tier 1) while others can be fully lifted (Tier 2). Design APIs that work for both.

**Games are instantiable.** Multiple independent game instances must be able to run on the same page without sharing state. All mutable runtime state lives on a root runtime instance (`GameRuntime`, `FlashRuntime`, etc.) created at the entry point and threaded through generated code. No singletons, no module-level mutable state. Pure functions (math, string ops, color constants) stay as static imports; stateful functions are destructured from `this._rt` in methods or received as a `_rt` parameter in free functions.

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
| Dialog | show/close modal, open state | Swap DOM modals for native `<dialog>`, toast notifications, mobile sheets |
| Save UI | save slot presentation | Swap modal slot list for cloud sync dashboard, thumbnail grid, Steam overlay |
| Settings UI | settings form presentation | Swap modal form for full-page panel, system preferences integration, accessibility UI |
| Layout | sidebar render/stow/toggle | Swap fixed sidebar for hamburger menu, bottom nav, collapsible drawer |

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

**Use `bun` for JavaScript/TypeScript scripting tasks** (e.g. inspecting HTML files, running quick JS snippets). `bun` is available in the dev environment — use it instead of `node` or `python3`.

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
- Write stubs that return null/undefined — implement the function or don't add it. A `return null` stub is a landmine that crashes at runtime with a misleading error. If the full implementation is complex, implement the subset that covers the actual usage patterns. Note: even when `null` is a valid return per the spec (e.g. "returns null if not found"), a stub that always returns null is still wrong — it silently reports "not found" when the real answer is "not implemented". Throw instead.
- Mark as done prematurely - note what remains
- Dismiss known issues as "fine for now" — if you discover a gap, mismatch, or missing implementation during your work, add it to TODO.md immediately. Do not assume future sessions will rediscover the same issue. Every known problem must be written down before moving on.
- Use path dependencies in Cargo.toml - causes clippy to stash changes across repos
- Use `--no-verify` - fix the issue or fix the hook
- Assume tools are missing - check if `nix develop` is available for the right environment
- Use module-level mutable state — state belongs on the runtime instance that owns its lifecycle. If data flows from A to B, pass it explicitly (return value, parameter, field on an instance). Module-level `let` variables that get mutated across calls are hidden coupling, make code unpredictable, and prevent multiple game instances from coexisting on the same page. There are no exceptions — even registries (`Map<string, PassageFn>`) belong on the runtime instance.
- Use DOM data attributes as a state-passing mechanism — if you need to communicate between code paths, pass values through function parameters or object fields. Storing data on elements and querying it back later is a jQuery-era anti-pattern. Data attributes are for CSS selectors and third-party integration, not for plumbing your own code.

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
