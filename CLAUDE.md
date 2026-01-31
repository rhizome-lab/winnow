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

**Triggers:** User corrects you, 2+ failed attempts, "aha" moment, framework quirk discovered → document before proceeding.

**Do the work properly.** When asked to analyze X, actually read X - don't synthesize from conversation.

## Behavioral Patterns

From ecosystem-wide session analysis:

- **Question scope early:** Before implementing, ask whether it belongs in this crate/module
- **Check consistency:** Look at how similar things are done elsewhere in the codebase
- **Implement fully:** No silent arbitrary caps, incomplete pagination, or unexposed trait methods
- **Name for purpose:** Avoid names that describe one consumer
- **Verify before stating:** Don't assert API behavior or codebase facts without checking

## Design Principles

**Unify, don't multiply.** One interface for multiple engines > separate implementations per engine. Plugin systems > hardcoded switches.

**Lazy extraction.** Don't parse everything upfront. Extract on demand, cache aggressively.

**Preserve fidelity.** The goal is accurate reproduction, not "improvement". Make the old thing work, don't redesign it.

**Overlay > Patch.** When possible, render a modern UI layer over the original rather than patching internal rendering.

**Two-tier approach.** Accept that some targets need binary patching (Tier 1) while others can be fully lifted (Tier 2). Design APIs that work for both.

## Workflow

**Batch cargo commands** to minimize round-trips:
```bash
cargo clippy --all-targets --all-features -- -D warnings && cargo test
```
After editing multiple files, run the full check once — not after each edit. Formatting is handled automatically by the pre-commit hook (`cargo fmt`).

**When making the same change across multiple crates**, edit all files first, then build once.

**Minimize file churn.** When editing a file, read it once, plan all changes, and apply them in one pass. Avoid read-edit-build-fail-read-fix cycles by thinking through the complete change before starting.

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
- Leave work uncommitted
- Create special cases - design to avoid them
- Add to the monolith - split by domain into sub-crates
- Cut corners with fallbacks - implement properly for each case
- Mark as done prematurely - note what remains
- Use path dependencies in Cargo.toml - causes clippy to stash changes across repos
- Use `--no-verify` - fix the issue or fix the hook
- Assume tools are missing - check if `nix develop` is available for the right environment

## Crate Structure

All crates use the `reincarnate-` prefix:
- `reincarnate-core` - Core types and traits
- `reincarnate-cli` - CLI binary (named `reincarnate`)
- `reincarnate-flash` - Flash/SWF support
- `reincarnate-director` - Director/Shockwave support
- etc.
