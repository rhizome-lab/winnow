# ADR 002: TypeScript Strict Configuration for Generated Projects

**Status:** Accepted

**Date:** 2026-02-21

## Context

Generated TypeScript projects ship with a `tsconfig.json` produced by
`scaffold.rs`. The original config used `strict: true` but immediately
overrode one of its key sub-flags with `noImplicitAny: false`, leaving the
generated codebases under-checked. We audited all TypeScript strict options
against the full DoL (SugarCube) and GML output to determine what was safe to
enable.

## Decisions

### Options added

| Option | Reason |
|---|---|
| `verbatimModuleSyntax` | Enforces `import type` for type-only imports; zero new errors across all test projects |
| `noImplicitReturns` | All code paths must return; zero new errors |
| `noImplicitOverride` | Overriding methods must carry `override` keyword; zero new errors |
| `noUncheckedIndexedAccess` | Array/map indexing returns `T \| undefined`; surfaces real null-safety gaps in runtime code |
| `exactOptionalPropertyTypes` | `prop?: T` cannot be set to `undefined` explicitly; surfaces sloppy optional-field handling |
| `allowUnusedLabels: false` | Catches dead labels |
| `allowUnreachableCode: false` | Catches dead code |

### Options deferred

| Option | Reason |
|---|---|
| `noImplicitAny: false` **kept** | The emitter occasionally produces implicit `any` (e.g. unresolved variable types). Enabling this across the board would surface emitter gaps as user-facing errors rather than emitter bugs. Tracked as a separate task: fix the emitter, then enable the flag. |
| `noFallthroughCasesInSwitch` **not added** | The `JsStmt::Dispatch` node (structurizer output for irreducible control flow) uses an intentional `switch ($block) { case N: { ... } /* falls through */ }` loop pattern. Every non-terminal block in the dispatch switch would be flagged. Enabling this flag would require rearchitecting the dispatch emitter — a separate, non-trivial task. |

### Why not `@tsconfig/strictest`?

The `@tsconfig/strictest` package would add a devDependency to every generated
project and require an `npm install` before the config can be resolved.
Generated projects should be self-contained — the tsconfig must work without
extra installs. Inlining the options explicitly is cleaner and more auditable.

## Consequences

- Generated `tsconfig.json` now catches a wider class of type errors at
  compile time.
- Runtime source files (`runtime/*/ts/`) need ~293 fixes (primarily
  `noUncheckedIndexedAccess` null guards and `exactOptionalPropertyTypes`
  object literal adjustments). These are tracked as follow-on work.
- The `Dispatch` emitter is a known source of `noFallthroughCasesInSwitch`
  violations; any future restructuring of that node should reconsider enabling
  that flag.
