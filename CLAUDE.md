# CLAUDE.md

Behavioral rules for Claude Code in this repository.

## Overview

Reincarnate is a legacy software lifting framework. It extracts and transforms applications from obsolete runtimes (Flash, Director, VB6, HyperCard, RPG Maker, etc.) into modern web-based equivalents.

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

**Conversation is not memory.** Anything said in conversation — a promise, a correction, a resolution — evaporates at the end of the session. If a statement implies future behavior change, it MUST be written to CLAUDE.md or a memory file immediately, or it will not happen. A statement like "I won't do X again" made only in conversation is a lie by omission: the next session has no access to it.

**Warning patterns — these phrases mean something needs to be written down RIGHT NOW:**
- "I won't do X again" / "I'll remember to..." / "I've learned that..."
- "Next time I'll..." / "From now on I'll..."
- "That was a mistake, I should..." (without immediately editing a file)
- Any acknowledgement of a recurring error without a corresponding CLAUDE.md or memory edit

If you catch yourself saying any of these, stop and write the rule before finishing the sentence.

**TODO.md and CLAUDE.md updates come before implementation, not after.** When a plan includes deferred scope, known limitations, new rules, or design decisions — write them to TODO.md/CLAUDE.md *before* writing any code. Documentation tasks are not cleanup to do at the end; they are the first step. The plan is the moment you know these items exist — waiting until after the code is written risks forgetting them entirely. This applies to deferred scope cuts, new behavioral rules, architecture notes, and any TODO.md/CLAUDE.md change identified in the plan.

**Every observed problem goes to TODO.md. No exceptions. No deferral.** When you notice a problem — a TODO comment in code, a violation of an architecture rule, a missing implementation, a known bug you can't fix right now — write it to TODO.md *before* continuing. Not "at the end of the session". Not "if I remember". Not "it's already in a code comment". A code comment is not a tracked item. A commit message is not a tracked item. A mention in conversation is not a tracked item. The only tracked item is an entry in TODO.md. If you write a TODO comment in source code, the NEXT action is to open TODO.md and write the entry. This rule has no exceptions based on priority, size, or confidence that it will be fixed soon.

**Something unexpected is a signal, not noise.** When a result is surprising — output that's 12x larger than input, a file that shouldn't contain JS but does, a number that's off by an order of magnitude — stop immediately and ask why. Don't accept the anomaly and move on. Surprising results are almost always early evidence of a bug. Treating them as "close enough" or "probably fine" is how bugs stay hidden for sessions. Investigate before proceeding.

**Update CLAUDE.md the moment you find a systematic gap.** If a bug reveals that the existing rules didn't prevent a class of mistake, add the rule immediately — not after the fix, not at the end of the session. The rule is only useful if it's written before the next similar mistake can happen.

**A user correction means CLAUDE.md failed.** When the user has to correct your approach — not a factual mistake, but a wrong *way of thinking* about a problem — that is direct evidence that the rules didn't catch it. Don't just fix the immediate thing. Ask: what rule, if it had existed, would have prevented this? Write that rule before proceeding. A correction that doesn't produce a new rule is a correction that will happen again.

**Multiple corrections on the same topic means the rule isn't strong enough.** If the user corrects you twice on related aspects of the same issue within a session (e.g. "that default is wrong" → "that type is wrong too" → "also think about other backends"), the first CLAUDE.md edit was too narrow. Go back and write a broader principle that covers the entire class of mistake, not just the specific instance. A rule that requires three corrections to converge on the right behavior is three rules too late.

**If you acknowledge you could have done better, CLAUDE.md failed.** When you realize mid-session that an approach was wrong — not a factual mistake, but a *wrong way of thinking* about a problem — that is evidence the rules didn't prevent it. Do not just fix the immediate thing. Ask: what rule, if written beforehand, would have prevented this mistake? Write that rule immediately. A realization without a new rule is a realization that will recur.

**Do the work properly.** When asked to analyze X, actually read X - don't synthesize from conversation.

**Investigation findings go to TODO.md before continuing.** When you investigate errors, profile output, or analyze a class of bugs — the findings go to TODO.md immediately, not after the next step. Investigation without written findings is wasted work: the next session can't see what you learned. Write the detailed breakdown (root causes, affected counts, code locations, fix strategies) to TODO.md, THEN proceed to fix or dig deeper. Presenting findings in conversation without recording them is the same as not finding them.

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
- **Don't hand-roll what a library does.** When a well-maintained crate exists for a standard (HTML entities, Unicode normalization, MIME types, etc.), use it. Hand-rolling a partial reimplementation is not "simpler" — it's an incomplete, untested copy of someone else's work that will silently produce wrong results. The question isn't "is my list complete?" — it's "why am I writing this at all?" Specific instance: **JavaScript identifier validity** — the rule is `ID_Start (ID_Continue)*` using Unicode character categories, NOT `[a-zA-Z_$][a-zA-Z0-9_$]*` (that ASCII heuristic silently rejects valid Unicode identifiers and `$` needs to be explicitly added). Use `unicode_ident::is_xid_start` / `is_xid_continue` (plus `$` as a JS extension) from the `unicode-ident` crate. For string output, use `serde_json::to_string` for correct JSON/JS string escaping — don't quote with `format!("\"{}\"", s)` which doesn't escape backslashes or embedded quotes.
- **Correctness is non-negotiable — 100%, always.** There is no "close enough", no "good enough for our use case", no "the game probably won't trigger this". If a standard says X, implement X, not a subset. "Overkill" only applies when you have positive proof (e.g. a soundness argument, a test showing the edge case cannot occur) that the full implementation is unnecessary for this specific, controlled input. Without that proof, implement it correctly. A partial implementation is a bug waiting to happen — it will silently produce wrong results for inputs you didn't test. Never defend a shortcut by saying "our inputs are ASCII-only" or "this won't come up in practice".
- **Use the right abstraction level.** When a library provides both a high-level API and a low-level one, the default is the high-level API. Only drop to the low level when you have a concrete, specific reason (e.g. streaming, custom state machine, performance). Using an HTML tokenizer to query a document when a DOM API exists is the same mistake as hand-rolling string parsing when a parser exists — you're doing the library's job for it, badly. If you find yourself building state machines on top of a tokenizer, ask whether a higher-level API (e.g. `scraper`, `parse_document`) would make the state machine unnecessary.
- **Pre-existing bugs are bugs.** When investigating errors and you discover a bug that existed before your change, don't dismiss it as "pre-existing" or "not caused by my changes". Fix it if possible. If fixing it would derail the current task, add it to TODO.md as **critical priority** immediately — do not assume a future session will rediscover it.
- **Match the target's semantics.** When reimplementing a runtime's behavior (SugarCube expression parsing, GML bytecode decoding, etc.), check what the original runtime actually does. Don't guess at semantics or invent a "simplified" version. Read the source if available (SugarCube is open source, UndertaleModTool is open source).
- **Fewer errors does not mean more correct.** A change that reduces error counts is not evidence of correctness — it can silently substitute *wrong* values where there were previously *missing* values, trading loud failures for quiet ones. Always verify that the *output values* are right, not just that the error count went down. Spot-check resolved names, emitted code, and runtime behavior against the source material. A wrong answer that type-checks is worse than a placeholder that doesn't, because it hides behind passing diagnostics indefinitely. Example: pushref type_tag=0→FUNC reduced TS2304 errors by producing *a* name for every reference, but the names were all wrong (random functions instead of object names) — it hid for weeks behind TS2345.
- **Never silently guess at semantics.** When implementing behavior that depends on external definitions (format specs, bytecode encodings, type mappings, runtime semantics), first try to find an authoritative source — the spec, the reference implementation's source code, official documentation. If you cannot find one, you *must* record the assumption in TODO.md as something that needs verification. An unverified guess that isn't written down is indistinguishable from a verified fact to the next session. The failure mode is: guess → it happens to reduce errors → gets committed as if correct → stays wrong for weeks. Writing "assumption: X means Y, needs verification against Z" in TODO.md prevents silent entrenchment.
- **When something exists, it exists for a reason.** Before removing or bypassing an existing mechanism (override check, special case, flag, registry entry), read the code that uses it and ask why it was added. If a game overrides a built-in macro, it overrides it for a reason — different behavior, not identical behavior. If a check seems wrong, the fix is to fix how it works, not to delete it. Deleting a mechanism because it causes a symptom is treating the symptom, not the cause.
- **Never widen runtime types to accommodate buggy game code.** TypeScript errors in emitted code that result from game author mistakes (wrong argument counts, wrong types) are correct behavior — they surface real bugs in the source material. Do not make function signatures more lenient (optional parameters, broader unions, `any`) to silence these errors. The emitted TypeScript should be as strictly typed as semantics allow. Use `git blame` to audit the runtime files periodically: any type that was widened to fix a "game error" is a red flag.
- **`any` is a failure of inference, not a feature.** Every `any` in emitted output means the type system gave up. Never accept `any` as "good enough" or argue that a param "could receive any value" as justification. If type inference narrowed a param to `string`, that's real information — use it. If it didn't narrow, that's a gap in inference to fix, not a property of the code. The goal is zero `any` annotations in emitted output. When you encounter `any` in emitted code, ask: what information would eliminate it? Then go get that information (from usage patterns, from runtime signatures, from the source language's type system).
- **Defaults must encode author intent, not suppress diagnostics.** When making a parameter optional, the default value must reflect what the author intended — not what makes the type checker shut up. If the author didn't intend a default (e.g. variadic scripts guarded by `argument_count`), the default must match the source language's missing-arg semantics AND the inferred type. A `string` param defaulting to `0.0` is always wrong. A `number` param defaulting to `null` is wrong unless the param is genuinely nullable. When in doubt, read what actually happens when the arg is missing in the original runtime, and reproduce that faithfully with the correct type.

## Design Principles

**Multi-turn confusion means missing tooling.** When a debugging or investigation task requires multiple rounds of grep/hack combos, redirecting output through temp files, or improvised extraction scripts that don't belong in the codebase — that is a signal that the *correct tool for the job doesn't exist yet*. Stop, note what tool is missing (e.g. "per-function IR dump command", "bytecode disassembler subcommand", "IR diff between two pipeline stages"), and add it to TODO.md before continuing. Don't build the missing tool on the spot if it's non-trivial, but don't keep hacking around its absence either — the workaround accumulates over sessions and the tool never gets built.

**Correctness over copouts.** When a design problem surfaces, solve it correctly — don't paper over it with string substitution, special-case hacks, or "simplest thing that works". A correct solution may take more time, but a hacky one accumulates debt that costs more later. When two things are fundamentally different (e.g. SugarCube and Harlowe), model them as different — don't force-merge them because they share a container format.

**Unify, don't multiply.** One interface for multiple engines > separate implementations per engine. Plugin systems > hardcoded switches.

**Eliminate megamorphic dispatch at compile time.** When a runtime method dispatches on a string literal (e.g. `math("round", x)`, `str_op("trimmed", s)`), the backend rewrite pass must resolve the dispatch to a direct, monomorphic call — either a JS built-in (`Math.round(x)`) or a named function in a typed namespace object (`StringOps.trimmed(s)`). The namespace must be exported `as const` so TypeScript statically verifies that every dispatch name maps to an existing method. A missing method in the namespace is a compile-time error, not a silent runtime `undefined`. Pattern: (1) define pure functions, (2) export `as const` namespace, (3) add rewrite arm in `rewrites/twine/engine.rs`, (4) add test. Existing namespaces: `Math` (built-in), `Collections`, `Colors`, `StringOps`.

**Compose, don't enumerate.** Enums are almost always the wrong solution for configurable behavior. An enum is a closed, mutually exclusive set — it forces exactly one choice from a fixed menu, and adding a new option requires modifying the type definition. The right solution is usually a composable pipeline of wrappers (middleware/decorator pattern), where each concern is an independent transform that can be applied in any combination. Example: CSS isolation isn't `"none" | "scoped-css" | "shadow-dom" | "iframe"` — it's a list of composable wrappers `[scopedCSS(), shadowDOM(), iframe()]`, because you might want shadow DOM *and* an iframe simultaneously. Open composition > closed enumeration.

**Lazy extraction.** Don't parse everything upfront. Extract on demand, cache aggressively.

**Preserve fidelity.** The goal is accurate reproduction, not "improvement". Make the old thing work, don't redesign it.

**100% API compatibility is the target.** Every method, property, and behavior of the source runtime must be implemented — not just the ones exercised by current test projects. Test projects are examples, not the specification. If a method exists in the original API, it belongs in the runtime. "Our test game doesn't use it" is never a reason to skip or deprioritize an API method.

**Game author errors are not our bugs.** When emitted TypeScript produces a type error that faithfully reflects a bug in the original game's source code (e.g. using `|` instead of `||`, multiplying by `false`), that is correct behavior — reincarnate accurately reproduced what the author wrote. Do not "fix" the emission to suppress the diagnostic. Changing `|` to `||` alters semantics; annotating with `ts-ignore` hides a real bug; widening types to `any` throws away information. The right response is: document it as a game author error and move on. The only exception is if the error is caused by an imprecision in our type inference (e.g. we inferred `boolean` where the game intended `number`) — in that case, fix the inference, not the symptom.

**Source-level bugs must be translated faithfully, not corrected.** When the original source contains a semantic bug (e.g. `array_length(noone)`, dead code that references undefined, arithmetic on an instance ID), the emitted code must reproduce that behavior faithfully — not silently fix it or change semantics. A rewrite pass that transforms `array_length(x)` → `x.length` is correct even when `x` happens to be `-4` (noone), because the source code was buggy in the same way. Do not add special-case guards in rewrite passes to "fix" source bugs — the emitted code should be wrong in exactly the same way the source was wrong. The goal is fidelity, not correctness relative to some hypothetical "what the author intended."

**Overlay > Patch.** When possible, render a modern UI layer over the original rather than patching internal rendering.

**Two-tier approach.** Accept that some targets need binary patching (Tier 1) while others can be fully lifted (Tier 2). Design APIs that work for both.

**Games are instantiable.** Multiple independent game instances must be able to run on the same page without sharing state. All mutable runtime state lives on a root runtime instance (`GameRuntime`, `FlashRuntime`, etc.) created at the entry point and threaded through generated code. No singletons, no module-level mutable state. Pure functions (math, string ops, color constants) stay as static imports; stateful functions are destructured from `this._rt` in methods or received as a `_rt` parameter in free functions.

**The singleton rule is an architectural invariant, not defensive programming.** Do not argue that a module-level singleton or `let` variable is acceptable because "multiple instances per page won't happen in practice" or "avoiding singletons is just a precaution." It is not a precaution — it is a load-bearing rule. Module-level mutable state couples code to a single runtime lifetime, breaks hot reload, prevents parallel testing, prevents local multiplayer (two players on the same page), and makes lifecycle ownership invisible. These costs exist even with one instance. The correct challenge to raise when applying this rule is: *where does this state actually belong?* — not whether the rule applies.

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

**The platform interface is a cross-language contract.** TypeScript, Rust, C#/Unity, SDL — all language runtimes implement the same conceptual interface. TypeScript is the current backend, not the only one. This has three hard consequences:
- **Generic names only.** Platform functions are named for what they do, not for the engine API they serve: `play`, `stop`, `save`, `load` — never `audioPlay`, `audio_play_sound`, `steam_file_write`. Engine-specific concepts (GML `priority`, `audio_group`, etc.) are absorbed by the engine shim layer and must never appear in the platform.
- **Primitive types only in the API surface.** No language-specific types (`AudioBuffer`, `HTMLImageElement`) in exported function signatures — those are implementation details hidden inside the platform module. Across all languages, parameters and return values are ints, floats, bools, strings, or opaque integer handles.
- **Canonical names are snake_case.** TypeScript implementations use camelCase (`playSound`), Rust uses snake_case (`play_sound`) — but the conceptual name is snake_case. Use that when documenting or discussing the interface. See `docs/architecture.md` Platform Interface section for the full function list.

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
- **When you write a throw-stub for a not-yet-implemented feature, document it in TODO.md unconditionally and immediately.** Do not defer. Do not assume "we'll add it later." Every `throw new Error("... not yet implemented")` that you write must have a corresponding TODO.md entry at the time you write it — not at the end of the session, not if you remember. The throw-stub is a cop-out; the TODO.md entry is the accountability. No exceptions.
- **GML runtime stubs must throw, not silently fail.** When adding a new GML built-in to the runtime: if it requires a real implementation and you haven't implemented it, throw `Error("name: not yet implemented")`. This applies to every unimplemented function — there are no exceptions based on return type or perceived importance. Silent returns of 0/""/false and empty `void {}` stubs are both wrong: they let game code run with missing functionality and fail silently far from the source. A throw that clearly signals an unimplemented feature is always better than silent misbehavior. When auditing existing runtime stubs, grep for `{ return 0; }`, `{ return ""; }`, `{ return false; }`, `{ return -1; }`, and `{}` on all functions that haven't been deliberately implemented.
- **Platform APIs (Steam, PSN, etc.) follow the PSN pattern — implement, no-op, or throw; never silent returns.** Three cases: (1) *Browser equivalent exists* → implement it properly (PSN/Steam achievements → localStorage, Steam cloud saves → localStorage). (2) *No browser equivalent* → explicit no-op with a comment explaining why: `/* no-op — PSN commerce not available in browser */`. (3) *Genuinely undecided* → throw. A silent `return false` or `return 0` with no comment is wrong in all cases — it either hides a real implementation opportunity (case 1) or hides the fact that a feature was silently dropped (case 2). The comment in case 2 is mandatory: it distinguishes "deliberately not implemented because there's no equivalent" from "forgot about this."
- Mark as done prematurely - note what remains
- Dismiss known issues as "fine for now" — if you discover a gap, mismatch, or missing implementation during your work, add it to TODO.md immediately. Do not assume future sessions will rediscover the same issue. Every known problem must be written down before moving on.
- Use path dependencies in Cargo.toml - causes clippy to stash changes across repos
- Use `--no-verify` - fix the issue or fix the hook
- Use `git add -p` or any other interactive command (`git rebase -i`, `git add -i`, etc.) — these block waiting for stdin and will hang forever in a non-interactive shell. Always stage files by name: `git add <file1> <file2>`.
- Assume tools are missing - check if `nix develop` is available for the right environment
- Use module-level mutable state — state belongs on the runtime instance that owns its lifecycle. If data flows from A to B, pass it explicitly (return value, parameter, field on an instance). Module-level `let` variables that get mutated across calls are hidden coupling, make code unpredictable, and prevent multiple game instances from coexisting on the same page. There are no exceptions — even registries (`Map<string, PassageFn>`) belong on the runtime instance. **Do not argue that a singleton is acceptable because "one instance is the realistic case" — see the "Games are instantiable" design principle for why this reasoning is wrong.**
- Use DOM data attributes as a state-passing mechanism — if you need to communicate between code paths, pass values through function parameters or object fields. Storing data on elements and querying it back later is a jQuery-era anti-pattern. Data attributes are for CSS selectors and third-party integration, not for plumbing your own code.
- **Promote `|`/`&` to `||`/`&&` based on inferred types.** `|` and `||` are semantically different: `|` always evaluates both operands; `||` short-circuits. Replacing `a | b` with `a || b` silently changes runtime behavior when `b` has side effects. TypeScript errors from `boolean | boolean` (TS2447/TS2363) are game-author errors — the author used bitwise operators where they meant logical ones. Do not suppress these by silently changing the emitted operator. This has been decided multiple times.

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
- `--dump-ir-after <pass>` — run pipeline up through the named pass, dump IR, then exit; use `frontend` to dump before any transforms; valid pass names: `type-inference`, `call-site-type-flow`, `constraint-solve`, `call-site-type-widen`, `constant-folding`, `cfg-simplify`, `coroutine-lowering`, `redundant-cast-elimination`, `mem2reg`, `dead-code-elimination`

Additional subcommands:
- `list-functions [--filter <pattern>]` — list all IR function names (exact names used internally, same matching as `--dump-function`; run this first when `--dump-function` produces no output)
- `disasm [--function <filter>]` — disassemble GML bytecode directly from the DataWin (no IR pipeline); resolves variable names, strings, function names, instance types, and break signal names; without `--function`, lists all CODE entry names
- `stress [--runs N] [--skip-pass P] [--preset P]` — run the transform pipeline N times (default 5), detect fixpoint convergence or oscillation; use when adding a new pass to verify it doesn't conflict with existing passes

Test projects live under `~/reincarnate/<engine>/<game>/`:
- Flash: `~/reincarnate/flash/cc/`
- GML (GMS1): `~/reincarnate/gamemaker/bounty/`
- GML (GMS2): `~/reincarnate/gamemaker/deadestate/`
- Twine: `~/reincarnate/twine/` (subfolders `dol/`, `trc/`, and 19+ Harlowe games)

**Checking TypeScript output:** Use `reincarnate check` for counts and summaries:
```bash
cargo run -p reincarnate-cli -- check --manifest ~/reincarnate/gamemaker/deadestate/reincarnate.json
# With baseline comparison:
cargo run -p reincarnate-cli -- check --manifest ... --baseline baseline.json
# Save a new baseline:
cargo run -p reincarnate-cli -- check --manifest ... --save-baseline baseline.json
```
Output: per-code counts (`TS2345: 222`, `TS2322: 180`, ...), per-file top 20, and total. Use `--json` for machine-readable output (includes full `diagnostics` array with file/line/message).

Default output: per-code counts + up to 3 deduplicated example messages per code. Control with:
- `--examples N` — number of examples per code (0 = counts only, -1 = all)
- `--filter-code TS2345` — show only that error code (all matches, not just examples)
- `--filter-file foo.ts` — show only diagnostics in files matching the substring
- `--filter-message <text>` — show only diagnostics whose message contains the text
- Filters compose (AND); filtered output shows "N of M total" so you never lose the true count

When reporting TS error counts (e.g. in TODO.md), always include the total count AND the per-code breakdown.

## Crate Structure

All crates use the `reincarnate-` prefix:
- `reincarnate-core` - Core types and traits
- `reincarnate-cli` - CLI binary (named `reincarnate`)
- `reincarnate-frontend-flash` - Flash/SWF frontend (in `crates/frontends/`)
- `reincarnate-frontend-director` - Director/Shockwave frontend (in `crates/frontends/`)
- etc.
