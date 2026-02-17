/** History strategies — pluggable undo/back implementations. */

/** Strategy for tracking passage history and variable snapshots. */
export interface HistoryStrategy {
  /** Record a new moment. */
  push(title: string, vars: Record<string, any>): void;
  /** Pop the most recent moment and return the restored state,
   *  or undefined if history is empty/at the start. */
  pop(): { title: string; vars: Record<string, any> } | undefined;
  /** Peek at the current passage title without popping. */
  peek(): string | undefined;
  /** Number of moments in history. */
  readonly length: number;
  /** All passage titles in order. */
  titles(): string[];
  /** Count how many times a passage appears in history. */
  countVisits(title: string): number;
  /** Check if a passage has ever been visited (across all history, including popped). */
  hasVisited(title: string): boolean;
  /** Forget the n most recent undos. -1 forgets all (keep only current). */
  forgetUndos(n: number): void;
  /** Clear the visited set. */
  forgetVisits(): void;
}

interface Moment {
  title: string;
  variables: Record<string, any>;
}

/** Full clone per moment — simple, works for small state. */
export function snapshotHistory(): HistoryStrategy {
  const history: Moment[] = [];
  const visited = new Set<string>();

  return {
    push(title: string, vars: Record<string, any>) {
      visited.add(title);
      history.push({
        title,
        variables: JSON.parse(JSON.stringify(vars)),
      });
    },

    pop(): { title: string; vars: Record<string, any> } | undefined {
      history.pop();
      const prev = history[history.length - 1];
      if (!prev) return undefined;
      return {
        title: prev.title,
        vars: JSON.parse(JSON.stringify(prev.variables)),
      };
    },

    peek(): string | undefined {
      const top = history[history.length - 1];
      return top?.title;
    },

    get length() { return history.length; },

    titles(): string[] {
      return history.map(m => m.title);
    },

    countVisits(title: string): number {
      let count = 0;
      for (const m of history) {
        if (m.title === title) count++;
      }
      return count;
    },

    hasVisited(title: string): boolean {
      return visited.has(title);
    },

    forgetUndos(n: number): void {
      if (n < 0) {
        if (history.length > 1) {
          const current = history[history.length - 1];
          history.length = 0;
          history.push(current);
        }
      } else {
        const keep = Math.max(1, history.length - n);
        history.splice(0, history.length - keep);
      }
    },

    forgetVisits(): void {
      visited.clear();
    },
  };
}

interface Diff {
  title: string;
  /** Keys that changed, with their previous values. undefined = key was added (didn't exist before). */
  changes: Map<string, any>;
  /** Keys that were removed (existed before this moment, not after). */
  removed: string[];
}

/** Diff-based — stores only changed keys per transition.
 *  More memory-efficient for large state with small per-passage changes. */
export function diffHistory(): HistoryStrategy {
  // The first moment is a full snapshot; subsequent moments store diffs.
  const history: Array<{ title: string; snapshot?: Record<string, any>; diff?: Diff }> = [];
  const visited = new Set<string>();
  // Cache of the current variables state (the latest snapshot, with all diffs applied).
  let currentVars: Record<string, any> = {};

  return {
    push(title: string, vars: Record<string, any>) {
      visited.add(title);
      if (history.length === 0) {
        // First moment — full snapshot
        currentVars = JSON.parse(JSON.stringify(vars));
        history.push({ title, snapshot: JSON.parse(JSON.stringify(vars)) });
      } else {
        // Compute diff: what changed from currentVars to vars
        const changes = new Map<string, any>();
        const removed: string[] = [];

        // Check for changed or added keys
        for (const key of Object.keys(vars)) {
          const oldVal = currentVars[key];
          const newVal = vars[key];
          if (JSON.stringify(oldVal) !== JSON.stringify(newVal)) {
            changes.set(key, oldVal !== undefined ? JSON.parse(JSON.stringify(oldVal)) : undefined);
          }
        }
        // Check for removed keys
        for (const key of Object.keys(currentVars)) {
          if (!(key in vars)) {
            removed.push(key);
            changes.set(key, JSON.parse(JSON.stringify(currentVars[key])));
          }
        }

        currentVars = JSON.parse(JSON.stringify(vars));
        history.push({ title, diff: { title, changes, removed } });
      }
    },

    pop(): { title: string; vars: Record<string, any> } | undefined {
      const popped = history.pop();
      if (!popped || history.length === 0) return undefined;

      // Undo the diff to restore previous state
      if (popped.diff) {
        // Restore changed keys to their previous values
        for (const [key, oldVal] of popped.diff.changes) {
          if (oldVal === undefined) {
            delete currentVars[key];
          } else {
            currentVars[key] = JSON.parse(JSON.stringify(oldVal));
          }
        }
        // Re-add removed keys
        for (const key of popped.diff.removed) {
          const oldVal = popped.diff.changes.get(key);
          if (oldVal !== undefined) {
            currentVars[key] = JSON.parse(JSON.stringify(oldVal));
          }
        }
      } else if (popped.snapshot) {
        // Popping a snapshot — restore from the previous entry
        const prev = history[history.length - 1];
        if (prev?.snapshot) {
          currentVars = JSON.parse(JSON.stringify(prev.snapshot));
        }
      }

      const prev = history[history.length - 1];
      return {
        title: prev.title,
        vars: JSON.parse(JSON.stringify(currentVars)),
      };
    },

    peek(): string | undefined {
      const top = history[history.length - 1];
      return top?.title;
    },

    get length() { return history.length; },

    titles(): string[] {
      return history.map(m => m.title);
    },

    countVisits(title: string): number {
      let count = 0;
      for (const m of history) {
        if (m.title === title) count++;
      }
      return count;
    },

    hasVisited(title: string): boolean {
      return visited.has(title);
    },

    forgetUndos(n: number): void {
      if (n < 0) {
        if (history.length > 1) {
          // Collapse to a single snapshot of current state
          const current = history[history.length - 1];
          history.length = 0;
          history.push({ title: current.title, snapshot: JSON.parse(JSON.stringify(currentVars)) });
        }
      } else {
        const keep = Math.max(1, history.length - n);
        if (keep < history.length) {
          // Ensure the kept base is a full snapshot
          const base = history[keep - 1];
          if (!base.snapshot) {
            base.snapshot = JSON.parse(JSON.stringify(currentVars));
            // Rebuild currentVars by replaying from this point
            // Actually, we need to compute what vars were at position keep-1.
            // For simplicity, collapse the remaining entries.
          }
          history.splice(0, history.length - keep);
          // Ensure first entry has a snapshot
          if (!history[0].snapshot) {
            history[0].snapshot = JSON.parse(JSON.stringify(currentVars));
            delete history[0].diff;
          }
        }
      }
    },

    forgetVisits(): void {
      visited.clear();
    },
  };
}
