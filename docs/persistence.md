# Persistence & Save System

## Overview

Reincarnate upgrades the save system of every lifted game. Original engines have
wildly different (and often limited) persistence: Harlowe uses sessionStorage
with no save slots, SugarCube has localStorage-backed slots but no autosave,
GameMaker games may have custom save/load code. The lifted version provides a
unified, composable persistence architecture that deployers configure to their
needs.

## Design Principles

**Save state and history are separate concerns.** A save is the current moment
(current passage/state + current variables). History (the undo stack) is a UI
feature with its own retention policy. They compose independently.

**The engine produces state; the platform persists it.** The engine calls
`commit(state)` after each transition. It does not know or care about storage
backends, retention policies, or slot counts. Persistence strategy is injected
by the deployer at initialization.

**Compose, don't enumerate.** The save system is not a menu of mutually
exclusive options. It's a set of independent, composable pieces that deployers
wire together. A deployer might want rolling autosave *and* manual slots *and*
writes to multiple backends simultaneously.

## Architecture

Three independent axes of composition:

### 1. State Transforms (what to save)

Transforms run on the state before it reaches any backend.

| Transform | Effect |
|-----------|--------|
| **identity** | Pass through unchanged (default) |
| **truncateHistory(n)** | Keep only the last *n* moments in the undo stack |
| **stripHistory** | Remove history entirely, save only current state |

### 2. Timing (when to save)

Controls when `commit()` calls propagate to backends.

| Strategy | Behavior |
|----------|----------|
| **immediate** | Every commit writes immediately |
| **debounced(ms)** | Coalesces rapid transitions (e.g. `<<goto>>` chains) |
| **manual** | Only writes on explicit user action (save button, keybind) |

### 3. Backends (where to save)

Storage implementations that the platform provides.

| Backend | Scope | Use case |
|---------|-------|----------|
| **localStorage** | Persistent, per-origin | Web default |
| **sessionStorage** | Session-scoped, per-tab | Temporary/undo only |
| **filesystem** | Persistent, user-visible | Desktop, FS Access API |
| **IndexedDB** | Persistent, large storage | WASM, large saves |
| **cloud** | Persistent, cross-device | Multiplayer / sync |

### Composition

Deployers compose these pieces at initialization:

```
commit(state)
  -> debounced(200ms)
    -> tee(
        rollingSnapshots(sessionBackend, 50),   // fast, session-scoped
        rollingSnapshots(localBackend, 10),      // durable, cross-session
        singleSlot(fsBackend),                   // user-visible file
      )

// independent, user-triggered:
manualSlots(localBackend, 8)
```

The `tee()` combinator fans out writes to multiple backends. Each backend
operates independently -- writing to sessionStorage does not block the
filesystem write.

### Engine Integration

The engine's only responsibility is calling `commit()` after each state
transition. For Twine, this happens in `goto()` after rendering. For GameMaker,
after each step event. The engine does not import or reference any persistence
strategy -- it receives a `commit` function at initialization.

```typescript
// Engine side (e.g. navigation.ts)
function goto(target: string) {
  state.pushMoment(target);
  render(target);
  commit(state.currentMoment());  // platform handles the rest
}
```

## History Strategies

History (the undo stack) has its own persistence and retention policies,
independent of save state.

### What is a moment?

A moment is a snapshot of game state at a point in time. The naive approach
stores a full deep clone of all variables at each passage transition. This
works for small games but scales poorly -- a game like Degrees of Lewdity
with thousands of variables would clone the entire state on every click.

### Diff-based history

Instead of storing full snapshots, store **diffs** -- only the variables that
changed in each transition:

```typescript
interface Moment {
  title: string;
  diff: Record<string, { old: any; new: any }>;
}
```

- **Push**: diff current vars against previous, store only changes
- **Pop (undo)**: apply reverse diff (restore `old` values)
- **Current state**: always kept in full as the live working copy

A passage that changes 3 variables out of 2000 stores 3 key-value pairs, not
2000. Undo applies 3 reversals, not a full state replacement.

### Snapshot-based history

The simple approach: deep clone all variables at each transition.

```typescript
interface Moment {
  title: string;
  variables: Record<string, any>;
}
```

Appropriate for small games where the variable count is low and cloning is
cheap. No diff computation overhead.

### Choosing a strategy

| Strategy | Memory | Undo cost | Complexity | Best for |
|----------|--------|-----------|------------|----------|
| **Diff** | O(changes per step) | Apply reverse diff | Medium | Large state (DoL, Dead Estate) |
| **Snapshot** | O(total state per step) | Replace state | Simple | Small state (short Twine, Harlowe games) |

Both strategies support the same retention policies (keep all, keep last N,
etc.) since they implement the same `HistoryStrategy` interface.

### Retention

How many moments to keep is a deployer decision:

- **Unbounded**: keep all moments (default for small games)
- **Windowed(n)**: keep last *n* moments, prune oldest (default for large games)

Pruning means the back button only goes *n* steps. This is a conscious deployer
tradeoff, not a hidden side effect.

### Persistence

History persistence is orthogonal to retention:

- **In-memory**: history dies when the app closes (simplest)
- **Session-scoped**: history survives refresh, dies on app close
- **Persistent**: history survives across launches

These use the same backend infrastructure as save state -- a
`persistedHistory(backend)` wrapper writes the moment stack to a backend on
each push.

## Default Configurations

We ship sensible defaults that deployers can use without configuration:

### Web (small game)

```
autosave: immediate -> singleSlot(localStorage)
history:  snapshot, unbounded, in-memory
manual:   8 slots, localStorage
```

### Web (large game)

```
autosave: debounced(200ms) -> singleSlot(localStorage)
history:  diff, windowed(100), in-memory
manual:   8 slots, localStorage
```

### Desktop

```
autosave: debounced(500ms) -> singleSlot(filesystem)
history:  diff, windowed(200), in-memory
manual:   8 slots, filesystem
```

## Migration from Original Engines

| Engine | Original behavior | Lifted behavior |
|--------|------------------|-----------------|
| **Harlowe** | sessionStorage only, no slots, no autosave | Continuous autosave + 8 manual slots |
| **SugarCube** | localStorage slots, `Config.saves.autosave` flag (often unused) | Continuous autosave + manual slots + export |
| **GameMaker** | Custom `game_save`/`game_load`, author-defined | Continuous autosave + manual slots |
| **Flash** | SharedObject (localStorage equivalent), author-defined | Continuous autosave + manual slots |

The lifted version is always an upgrade. Authors who relied on "closing the tab
= no save" can configure `manual`-only persistence if needed, but the default
is continuous autosave.
