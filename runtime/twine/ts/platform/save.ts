/** Platform save service — shared persistence logic for all engines.
 *
 * Engines provide a SaveableState implementation that serializes/deserializes
 * their own variable format. The save service handles slots, keybinds, and UI.
 * Deployers can swap the SaveBackend to change where saves are stored.
 */

import { type SaveSlotInfo, showSaveUI } from "./save-ui";

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

// --- Module state (injected via init) ---

let state: SaveableState;
let backend: SaveBackend;
let gotoFn: (passage: string) => void;
let slotPrefix = "reincarnate-save-";
let autosaveEnabled = true;
const SLOT_COUNT = 8;

// --- Public API ---

/** Initialize the save service. Must be called before any other save function. */
export function init(
  s: SaveableState,
  b: SaveBackend,
  goto: (passage: string) => void,
  registerCommand: (id: string, binding: string, handler: () => void) => void,
  prefix?: string,
  autosave?: boolean,
): void {
  state = s;
  backend = b;
  gotoFn = goto;
  if (prefix !== undefined) slotPrefix = prefix;
  autosaveEnabled = autosave !== false;

  registerCommand("quicksave", "$mod+s", () => saveSlot("auto"));
  registerCommand("quickload", "", () => {
    const title = loadSlot("auto");
    if (title) gotoFn(title);
  });
  for (let i = 0; i < SLOT_COUNT; i++) {
    const slot = i;
    registerCommand(`save-to-slot-${slot + 1}`, "", () => saveSlot(String(slot)));
    registerCommand(`load-from-slot-${slot + 1}`, "", () => {
      const title = loadSlot(String(slot));
      if (title) gotoFn(title);
    });
  }
  registerCommand("open-saves", "", () => {
    const slots: SaveSlotInfo[] = [];
    for (let i = 0; i < SLOT_COUNT; i++) {
      const has = hasSlot(String(i));
      slots.push({ index: i, title: has ? `Save ${i + 1}` : null, date: null, isEmpty: !has });
    }
    showSaveUI(
      slots,
      (i) => saveSlot(String(i)),
      (i) => { const t = loadSlot(String(i)); if (t) gotoFn(t); },
      (i) => deleteSlot(String(i)),
    );
  });
  registerCommand("export-save", "", () => {
    navigator.clipboard.writeText(btoa(state.serialize()));
  });
}

/** Continuous autosave — call after each passage transition. */
export function commit(): void {
  if (!autosaveEnabled) return;
  try {
    backend.save(slotPrefix + "_autosave", state.serialize());
  } catch {
    // Silent fail — autosave is best-effort
  }
}

/** Check for an existing autosave and resume. Returns passage title or undefined. */
export function tryResume(): string | undefined {
  const raw = backend.load(slotPrefix + "_autosave");
  if (raw === null) return undefined;
  return state.deserialize(raw);
}

/** Clear the autosave. */
export function clearAutosave(): void {
  backend.remove(slotPrefix + "_autosave");
}

/** Save current state to a named slot. */
export function saveSlot(name: string): boolean {
  try {
    backend.save(slotPrefix + name, state.serialize());
    return true;
  } catch {
    return false;
  }
}

/** Load state from a named slot. Returns passage title or undefined. */
export function loadSlot(name: string): string | undefined {
  const raw = backend.load(slotPrefix + name);
  if (raw === null) return undefined;
  return state.deserialize(raw);
}

/** Delete a save slot. */
export function deleteSlot(name: string): void {
  backend.remove(slotPrefix + name);
}

/** Check if a save slot exists. */
export function hasSlot(name: string): boolean {
  return backend.load(slotPrefix + name) !== null;
}

/** Get the number of used slots (excluding auto). */
export function slotCount(): number {
  let n = 0;
  for (let i = 0; i < SLOT_COUNT; i++) {
    if (hasSlot(String(i))) n++;
  }
  return n;
}

/** Get the total number of available slots. */
export function totalSlots(): number {
  return SLOT_COUNT;
}

// --- Composable backend wrappers ---

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
