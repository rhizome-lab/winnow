/** Platform save service — shared persistence logic for all engines.
 *
 * Engines provide a SaveableState implementation that serializes/deserializes
 * their own variable format. The save service handles slots, keybinds, and UI.
 * Deployers can swap the SaveBackend to change where saves are stored.
 */

import { type SaveSlotInfo } from "./save-ui";

/** Deployer-configurable persistence options (from reincarnate.json). */
export interface PersistenceOpts {
  autosave?: boolean;
  resume?: "auto" | "prompt" | "ignore";
  history?: "snapshot" | "diff";
  slot_count?: number;
  debounce_ms?: number;
}

// --- Backend abstraction ---

/** Storage backend — deployers swap this to change where saves go. */
export interface SaveBackend {
  load(key: string): string | null;
  save(key: string, value: string): void;
  remove(key: string): void;
}

/** What the engine provides to the persistence layer. */
export interface SaveableState {
  /** Serialize current state for saving. */
  serialize(): string;
  /** Restore state from serialized data. Returns passage title to navigate to. */
  deserialize(data: string): string | undefined;
}

// --- Save service ---

export class SaveManager {
  private state: SaveableState;
  private backend: SaveBackend;
  private gotoFn: (passage: string) => void;
  private _showSaveUI: (
    slots: SaveSlotInfo[],
    onSave: (i: number) => void,
    onLoad: (i: number) => void,
    onDelete: (i: number) => void,
  ) => void;
  private slotPrefix = "reincarnate-save-";
  private autosaveEnabled = true;
  private static readonly SLOT_COUNT = 8;

  constructor(
    state: SaveableState,
    backend: SaveBackend,
    gotoFn: (passage: string) => void,
    registerCommand: (id: string, binding: string, handler: () => void) => void,
    showSaveUI: (
      slots: SaveSlotInfo[],
      onSave: (i: number) => void,
      onLoad: (i: number) => void,
      onDelete: (i: number) => void,
    ) => void,
    prefix?: string,
    autosave?: boolean,
  ) {
    this.state = state;
    this.backend = backend;
    this.gotoFn = gotoFn;
    this._showSaveUI = showSaveUI;
    if (prefix !== undefined) this.slotPrefix = prefix;
    this.autosaveEnabled = autosave !== false;

    registerCommand("quicksave", "$mod+s", () => this.saveSlot("auto"));
    registerCommand("quickload", "", () => {
      const title = this.loadSlot("auto");
      if (title) this.gotoFn(title);
    });
    for (let i = 0; i < SaveManager.SLOT_COUNT; i++) {
      const slot = i;
      registerCommand(`save-to-slot-${slot + 1}`, "", () => this.saveSlot(String(slot)));
      registerCommand(`load-from-slot-${slot + 1}`, "", () => {
        const title = this.loadSlot(String(slot));
        if (title) this.gotoFn(title);
      });
    }
    registerCommand("open-saves", "", () => {
      const slots: SaveSlotInfo[] = [];
      for (let i = 0; i < SaveManager.SLOT_COUNT; i++) {
        const has = this.hasSlot(String(i));
        slots.push({ index: i, title: has ? `Save ${i + 1}` : null, date: null, isEmpty: !has });
      }
      this._showSaveUI(
        slots,
        (i) => this.saveSlot(String(i)),
        (i) => { const t = this.loadSlot(String(i)); if (t) this.gotoFn(t); },
        (i) => this.deleteSlot(String(i)),
      );
    });
    registerCommand("export-save", "", () => {
      navigator.clipboard.writeText(btoa(this.state.serialize()));
    });
  }

  /** Continuous autosave — call after each passage transition. */
  commit(): void {
    if (!this.autosaveEnabled) return;
    try {
      this.backend.save(this.slotPrefix + "_autosave", this.state.serialize());
    } catch {
      // Silent fail — autosave is best-effort
    }
  }

  /** Check for an existing autosave and resume. Returns passage title or undefined. */
  tryResume(): string | undefined {
    const raw = this.backend.load(this.slotPrefix + "_autosave");
    if (raw === null) return undefined;
    return this.state.deserialize(raw);
  }

  /** Clear the autosave. */
  clearAutosave(): void {
    this.backend.remove(this.slotPrefix + "_autosave");
  }

  /** Save current state to a named slot. */
  saveSlot(name: string): boolean {
    try {
      this.backend.save(this.slotPrefix + name, this.state.serialize());
      return true;
    } catch {
      return false;
    }
  }

  /** Load state from a named slot. Returns passage title or undefined. */
  loadSlot(name: string): string | undefined {
    const raw = this.backend.load(this.slotPrefix + name);
    if (raw === null) return undefined;
    return this.state.deserialize(raw);
  }

  /** Delete a save slot. */
  deleteSlot(name: string): void {
    this.backend.remove(this.slotPrefix + name);
  }

  /** Check if a save slot exists. */
  hasSlot(name: string): boolean {
    return this.backend.load(this.slotPrefix + name) !== null;
  }

  /** Get the number of used slots (excluding auto). */
  slotCount(): number {
    let n = 0;
    for (let i = 0; i < SaveManager.SLOT_COUNT; i++) {
      if (this.hasSlot(String(i))) n++;
    }
    return n;
  }

  /** Get the total number of available slots. */
  totalSlots(): number {
    return SaveManager.SLOT_COUNT;
  }
}

// --- Composable backend wrappers (pure, no state) ---

/** Fan-out writes to multiple backends. Reads from the first. */
export function tee(...backends: SaveBackend[]): SaveBackend {
  return {
    load(key: string) { return backends[0]?.load(key) ?? null; },
    save(key: string, value: string) { for (const b of backends) b.save(key, value); },
    remove(key: string) { for (const b of backends) b.remove(key); },
  };
}

/** Debounce writes by key. Reads always go through immediately. */
export function debounced(inner: SaveBackend, ms: number): SaveBackend {
  const timers = new Map<string, ReturnType<typeof setTimeout>>();
  return {
    load(key: string) { return inner.load(key); },
    save(key: string, value: string) {
      const existing = timers.get(key);
      if (existing !== undefined) clearTimeout(existing);
      timers.set(key, setTimeout(() => {
        timers.delete(key);
        inner.save(key, value);
      }, ms));
    },
    remove(key: string) {
      const existing = timers.get(key);
      if (existing !== undefined) { clearTimeout(existing); timers.delete(key); }
      inner.remove(key);
    },
  };
}

/** Keep only the last N saves per key prefix, auto-pruning older ones. */
export function rolling(inner: SaveBackend, n: number, prefix: string): SaveBackend {
  const indexKey = prefix + "__rolling_index";
  function getIndex(): string[] {
    const raw = inner.load(indexKey);
    return raw ? JSON.parse(raw) : [];
  }
  function setIndex(keys: string[]) {
    inner.save(indexKey, JSON.stringify(keys));
  }
  return {
    load(key: string) { return inner.load(key); },
    save(key: string, value: string) {
      inner.save(key, value);
      const idx = getIndex();
      const pos = idx.indexOf(key);
      if (pos !== -1) idx.splice(pos, 1);
      idx.push(key);
      while (idx.length > n) {
        const old = idx.shift()!;
        inner.remove(old);
      }
      setIndex(idx);
    },
    remove(key: string) {
      inner.remove(key);
      const idx = getIndex();
      const pos = idx.indexOf(key);
      if (pos !== -1) { idx.splice(pos, 1); setIndex(idx); }
    },
  };
}
