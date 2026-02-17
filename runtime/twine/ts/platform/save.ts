/** Platform save service — shared persistence logic for all engines.
 *
 * Engines provide a SaveableState implementation that serializes/deserializes
 * their own variable format. The save service handles slots, keybinds, and UI.
 * Deployers can swap the SaveBackend to change where saves are stored.
 */

import { type SaveSlotInfo, showSaveUI } from "./save-ui";

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
const SLOT_COUNT = 8;

// --- Public API ---

/** Initialize the save service. Must be called before any other save function. */
export function init(
  s: SaveableState,
  b: SaveBackend,
  goto: (passage: string) => void,
  registerCommand: (id: string, binding: string, handler: () => void) => void,
  prefix?: string,
): void {
  state = s;
  backend = b;
  gotoFn = goto;
  if (prefix !== undefined) slotPrefix = prefix;

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
