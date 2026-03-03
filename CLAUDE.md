# CLAUDE.md

Behavioral rules for Claude Code in this repository.

## Overview

Reincarnate is a legacy software lifting framework. It extracts and transforms applications from obsolete runtimes (Flash, Director, VB6, HyperCard, RPG Maker, etc.) into modern web-based equivalents.

### Goals

- **High-performance, maintainable emitted code** — The output is compiled TypeScript (or Rust) functions, not a bundled interpreter. Shipped output should be readable, optimisable, and auditable.
- **Multiple backends** — The IR + transform pipeline is backend-agnostic. TypeScript is the current backend; Rust codegen is planned. Design choices that couple output to a single runtime target are wrong.

**Corollary: type inference belongs in the IR, not in a backend.** Inference happens in the frontend, IR transform passes, or a dedicated pass — never inside the TypeScript printer. A backend may omit annotations when the target language can infer contextually, but `FunctionSig`/`Type` in the IR is the source of truth. Backends read from it; they don't create it.

**Corollary: never suggest bundling an existing interpreter.** inkjs, Parchment, renpyweb, libqsp-WASM produce running games but not emitted code. Note them as "quick deploy" alternatives — not the goal.

## Core Rule

**Note things down immediately:**
- Bugs/issues → fix or add to TODO.md
- Design decisions → docs/ or code comments
- Future work → TODO.md
- Key insights → this file

**Triggers:** User corrects you, 2+ failed attempts, "aha" moment, framework quirk discovered → document **before** proceeding. Never describe a bug in conversation and move on — write it to TODO.md first, then continue working.

**Conversation is not memory.** Anything said in conversation — a promise, a correction, a resolution — evaporates at the end of the session. If a statement implies future behavior change, it MUST be written to CLAUDE.md or a memory file immediately, or it will not happen. A statement like "I won't do X again" made only in conversation is a lie by omission: the next session has no access to it.

**Documentation before implementation.** Known limitations, new rules, and design decisions go to TODO.md/CLAUDE.md *before* writing code — not as cleanup after.

**Every observed problem goes to TODO.md. No exceptions.** Code comments, commit messages, and conversation are not tracked items. If you write a `// TODO` in source code, open TODO.md next. Pre-existing bugs you discover go to TODO.md as critical priority.

**Something unexpected is a signal, not noise.** Surprising results are almost always early bug evidence. Investigate before proceeding.

**Any correction or realization means update CLAUDE.md now.** When the user corrects an approach (not just a fact), or when you realize mid-session that a way of thinking was wrong — ask: what rule would have prevented this? Write it before proceeding. A correction without a new rule will repeat. If corrected twice on the same topic, the first rule was too narrow — write a broader principle covering the entire class.

**Do the work properly.** When asked to analyze X, actually read X - don't synthesize from conversation.

**Investigation findings go to TODO.md before continuing.** When you investigate errors, profile output, or analyze a class of bugs — the findings go to TODO.md immediately, not after the next step. Investigation without written findings is wasted work: the next session can't see what you learned. Write the detailed breakdown (root causes, affected counts, code locations, fix strategies) to TODO.md, THEN proceed to fix or dig deeper. Presenting findings in conversation without recording them is the same as not finding them.

## Behavioral Patterns

From ecosystem-wide session analysis:

- **CLAUDE.md updates are immediate priority.** When the user asks to update CLAUDE.md or documentation, drop everything and do it NOW. Context rots — defer even one task and the insight is lost. This overrides any in-progress work.
- **Question scope early:** Before implementing, ask whether it belongs in this crate/module
- **Check consistency:** Look at how similar things are done elsewhere in the codebase
- **Implement fully.** Test projects are examples, not the spec — fix the entire class, not just the case that blew up. In a multi-stage pipeline, grep all stages (parser, translator, emitter) before closing a task; each encodes its own assumptions. Every API method, even ones no test game uses, belongs in the runtime.
- **Name for purpose:** Avoid names that describe one consumer
- **Verify before stating:** Don't assert API behavior or codebase facts without checking
- **Write regression tests for reproducible compiler bugs.** If a bug in a compiler pass (transforms, structurizer, AST, emit) can recur from future changes, write a test. Tests must assert the correct externally-observable behavior — not mirror the implementation. If writing the correct assertion causes it to fail, mark it `#[ignore = "known bug: ..."]` and add to TODO.md; never adjust the assertion to match broken behavior.
- **Treat special-casing as a smell:** When a fix adds a narrow guard (`if this_specific_case { continue }`) to a pass, stop and ask whether the pass's core logic is wrong. A special case that prevents one crash often means the pass's assumptions are too broad — fix the assumption, not the symptom. Use `git blame` on the file to check whether a cluster of special-case guards have accumulated around the same function; that pattern indicates a deeper design gap.
- **Don't hand-roll what a library does; use the right abstraction level.** Use the high-level API by default — only drop to low-level with a concrete reason (streaming, performance). Building state machines on a tokenizer when a DOM parser exists is the same mistake as hand-rolling string parsing. Specific instances: JS identifier validity → `unicode_ident::is_xid_start`/`is_xid_continue` (plus `$`), NOT `[a-zA-Z_$][a-zA-Z0-9_$]*`; JS string escaping → `serde_json::to_string`, not `format!("\"{}\"", s)`.
- **Correctness is non-negotiable — 100%, always.** If a standard says X, implement X. Never defend a shortcut with "our inputs are ASCII-only" or "this won't come up in practice."
- **Fewer errors does not mean more correct.** A change that reduces error counts can silently substitute *wrong* values for *missing* ones — trading loud failures for quiet ones. Always verify that *output values* are right, not just that the error count went down. A wrong answer that type-checks is worse than a placeholder that doesn't.
- **Verify semantics against the authoritative source.** When reimplementing runtime behavior, check what the original actually does — read the source if available (SugarCube, UndertaleModTool are open source). If no authoritative source is found, record the assumption in TODO.md. An unverified guess is indistinguishable from verified fact to the next session.
- **When something exists, it exists for a reason.** Before removing or bypassing a mechanism, read why it was added. Fix how it works; don't delete it because it causes a symptom.
- **Never weaken the type system to silence diagnostics.** TS errors from game author mistakes are correct behavior — don't make signatures more lenient (`optional`, unions, `any`). `any` in emitted output means inference gave up — ask what information would eliminate it, then get it. Defaults must match the source language's missing-arg semantics and inferred type, not make the type checker shut up. A `string` param defaulting to `0.0` is always wrong.

## Design Principles

**Multi-turn confusion means missing tooling.** Repeated grep/hack combos and temp-file workarounds signal that the right tool doesn't exist. Add it to TODO.md — don't build it on the spot if non-trivial, but don't keep working around its absence either.

**Unify, don't multiply.** One interface for multiple engines > separate implementations per engine. Plugin systems > hardcoded switches.

**Eliminate megamorphic dispatch at compile time.** When a runtime method dispatches on a string literal (e.g. `math("round", x)`), the backend rewrite pass must resolve it to a direct monomorphic call — a JS built-in or a named function in a typed `as const` namespace (`StringOps.trimmed(s)`). A missing method is a compile-time error, not silent `undefined`. Existing namespaces: `Math` (built-in), `Collections`, `Colors`, `StringOps`.

**Compose, don't enumerate.** Enums force exactly one choice from a closed menu; adding an option requires modifying the type. Prefer a composable pipeline of independent wrappers that can be combined freely. Open composition > closed enumeration.

**Lazy extraction.** Don't parse everything upfront. Extract on demand, cache aggressively.

**Preserve fidelity.** The goal is accurate reproduction, not "improvement". Make the old thing work, don't redesign it.

**Reproduce source faithfully, including bugs.** When emitted TypeScript reflects a bug in the source (e.g. `|` instead of `||`, `array_length(noone)`), that is correct behavior — don't suppress it. Don't add special-case guards in rewrite passes to "fix" source bugs; the emitted code should be wrong in exactly the same way the source was. The only exception: if the error stems from imprecise type inference on our end, fix the inference.

**Overlay > Patch.** When possible, render a modern UI layer over the original rather than patching internal rendering.

**Two-tier approach.** Accept that some targets need binary patching (Tier 1) while others can be fully lifted (Tier 2). Design APIs that work for both.

**Games are instantiable — no singletons.** Multiple game instances must coexist on one page. All mutable runtime state lives on a root runtime instance (`GameRuntime`, `FlashRuntime`, etc.) threaded through generated code — never in module-level `let` variables. Do not argue "one instance is realistic"; it's a load-bearing rule (breaks hot reload, parallel testing, local multiplayer). Ask *where does this state belong?*, not *does the rule apply?*

## Runtime Architecture

Three-layer architecture: API Shims → Platform Interface (`platform/index.ts`) → Platform Implementation (`platform/browser.ts`). Swap platforms by changing the re-export in `platform/index.ts`. See `docs/architecture.md` for the full concern table and platform function list.

**Platform concern modules never import from siblings.** Each is independently swappable. Cross-concern deps are injected via callbacks (`init(showDialog, closeDialog)`), never via `import`. Direct DOM ops stay in shim modules, not the platform layer.

**The platform interface is a cross-language contract** (TS, Rust, C#/Unity, SDL). Three hard rules:
- **Generic names only**: `play`/`stop`/`save` — never `audioPlay`/`audio_play_sound`. Engine-specific args are absorbed in the shim layer.
- **Primitives only in the API surface**: no `AudioBuffer`/`HTMLImageElement` in exported signatures — opaque handles instead.
- **Canonical names are snake_case** (TS implements as camelCase).

## Workflow

**Batch cargo commands** to minimize round-trips:
```bash
cargo clippy --all-targets --all-features -- -D warnings && cargo test -- --include-ignored
```
Always pass `--include-ignored` when running tests locally — some tests (e.g. real-game datawin tests) are gated with `#[ignore]` for CI but must pass locally. After editing multiple files, run the full check once — not after each edit. Formatting is handled automatically by the pre-commit hook (`cargo fmt`).

**When making the same change across multiple crates**, edit all files first, then build once.

**Minimize file churn.** When editing a file, read it once, plan all changes, and apply them in one pass. Avoid read-edit-build-fail-read-fix cycles by thinking through the complete change before starting.

**Commit after every phase.** When a plan has multiple steps, commit after each step that passes `cargo clippy && cargo test`. Do not accumulate changes across phases. Do not defer commits to the end. Do not rationalize skipping commits because changes are "intertwined" or "small". Each commit should represent one logical unit of progress. This is non-negotiable.

**Use `bun` for JavaScript/TypeScript scripting tasks** (e.g. inspecting HTML files, running quick JS snippets). `bun` is available in the dev environment — use it instead of `node` or `python3`.

**Use `normalize view` for structural exploration:**
```bash
~/git/rhizone/normalize/target/debug/normalize view <file>    # outline with line numbers
~/git/rhizone/normalize/target/debug/normalize view <dir>     # directory structure
```

## Context Management

**Use subagents to protect the main context window.** For broad exploration or mechanical multi-file work, delegate to an Explore or general-purpose subagent rather than running searches inline. The subagent returns a distilled summary; raw tool output stays out of the main context.

Rules of thumb:
- Research tasks (investigating a question, surveying patterns) → subagent; don't pollute main context with exploratory noise
- Searching >5 files or running >3 rounds of grep/read → use a subagent
- Codebase-wide analysis (architecture, patterns, cross-file survey) → always subagent
- Mechanical work across many files (applying the same change everywhere) → parallel subagents
- Single targeted lookup (one file, one symbol) → inline is fine

## Session Handoff

Use plan mode as a handoff mechanism when:
- A task is fully complete (committed, pushed, docs updated)
- The session has drifted from its original purpose
- Context has accumulated enough that a fresh start would help

**For handoffs:** enter plan mode, write a short plan pointing at TODO.md, and ExitPlanMode. **Do NOT investigate first** — the session is context-heavy and about to be discarded. The fresh session investigates after approval.

**For mid-session planning** on a different topic: investigating inside plan mode is fine — context isn't being thrown away.

Before the handoff plan, update TODO.md and memory files with anything worth preserving.

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
- **Stubs must throw, not silently fail.** Implement the function or `throw Error("name: not yet implemented")` + add a TODO.md entry immediately. Silent returns (`0`, `""`, `false`, `null`, `{}`) are always wrong. For platform APIs with no browser equivalent: explicit no-op with a comment (`/* no-op — PSN commerce not available in browser */`), not a silent return.
- Mark as done prematurely; dismiss known issues as "fine for now" — every gap goes to TODO.md immediately
- Use path dependencies in Cargo.toml - causes clippy to stash changes across repos
- Use `--no-verify` - fix the issue or fix the hook
- Use `git add -p` or any other interactive command (`git rebase -i`, `git add -i`, etc.) — these block waiting for stdin and will hang forever in a non-interactive shell. Always stage files by name: `git add <file1> <file2>`.
- Assume tools are missing - check if `nix develop` is available for the right environment
- Use module-level mutable state — state belongs on the runtime instance. Pass data explicitly (return value, parameter, field). No exceptions — even registries belong on the instance. See "Games are instantiable."
- Use DOM data attributes as a state-passing mechanism — pass values through function parameters or object fields instead
- **Promote `|`/`&` to `||`/`&&` based on inferred types.** They're semantically different: `|` evaluates both operands, `||` short-circuits. TS2447/TS2363 from `boolean | boolean` are game-author errors — don't suppress them by changing the emitted operator.

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
- `--dump-function <pattern>` — filter dumps to matching functions; supports bare substring, case-insensitive, and qualified names (`Gun.step`, `Gun::step`) via split-part matching
- `--dump-ir-after <pass>` — run pipeline up through the named pass, dump IR, then exit; use `frontend` to dump before any transforms

Additional subcommands:
- `list-functions [--filter <pattern>]` — list all IR function names (exact names used internally, same matching as `--dump-function`; run this first when `--dump-function` produces no output)
- `disasm [--function <filter>]` — disassemble GML bytecode directly from the DataWin (no IR pipeline); resolves variable names, strings, function names, instance types, and break signal names; without `--function`, lists all CODE entry names
- `stress [--runs N] [--skip-pass P] [--preset P]` — run the transform pipeline N times (default 5), detect fixpoint convergence or oscillation; use when adding a new pass to verify it doesn't conflict with existing passes

Test projects: see MEMORY.md for full list. Key manifests: `~/reincarnate/<engine>/<game>/reincarnate.json`.

**Checking TypeScript output:** Use `reincarnate check` for counts and summaries:
```bash
cargo run -p reincarnate-cli -- check --manifest ~/reincarnate/gamemaker/deadestate/reincarnate.json
# With baseline comparison:
cargo run -p reincarnate-cli -- check --manifest ... --baseline baseline.json
# Save a new baseline:
cargo run -p reincarnate-cli -- check --manifest ... --save-baseline baseline.json
```
Output: per-code counts (`TS2345: 222`, `TS2322: 180`, ...), per-file top 20, and total. Use `--json` for machine-readable output (includes full `diagnostics` array with file/line/message).

Filter flags: `--examples N`, `--filter-code TS2345`, `--filter-file foo.ts`, `--filter-message <text>` — compose (AND). When reporting TS error counts in TODO.md, always include total AND per-code breakdown.

## Crate Structure

All crates use the `reincarnate-` prefix:
- `reincarnate-core` - Core types and traits
- `reincarnate-cli` - CLI binary (named `reincarnate`)
- `reincarnate-frontend-flash` - Flash/SWF frontend (in `crates/frontends/`)
- `reincarnate-frontend-director` - Director/Shockwave frontend (in `crates/frontends/`)
- etc.
