/** Harlowe story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * Harlowe also tracks `it` — the result of the most recent expression.
 */

import { loadLocal, saveLocal, removeLocal, type SaveSlotInfo, showSaveUI } from "../platform";

// --- Variable stores ---

const storyVars: Record<string, any> = {};
const tempVars: Record<string, any> = {};
let itValue: any = undefined;

/** Get a story variable by name (without the $ prefix). */
export function get(name: string): any {
  return storyVars[name];
}

/** Set a story variable by name (without the $ prefix). */
export function set(name: string, value: any): void {
  storyVars[name] = value;
  itValue = value;
}

/** Get the `it` keyword value (result of most recent expression). */
export function get_it(): any {
  return itValue;
}

/** Clear all temp variables (called at start of each passage). */
export function clearTemps(): void {
  for (const key of Object.keys(tempVars)) {
    delete tempVars[key];
  }
}

// --- History ---

interface Moment {
  title: string;
  variables: Record<string, any>;
}

const history: Moment[] = [];
const visitedSet: Set<string> = new Set();

/** Deep-clone story variables and push onto history. */
export function pushMoment(title: string): void {
  visitedSet.add(title);
  history.push({
    title,
    variables: JSON.parse(JSON.stringify(storyVars)),
  });
}

/** Pop the most recent moment and restore story variables. */
export function popMoment(): string | undefined {
  history.pop();
  const prev = history[history.length - 1];
  if (!prev) return undefined;
  for (const key of Object.keys(storyVars)) {
    delete storyVars[key];
  }
  Object.assign(storyVars, JSON.parse(JSON.stringify(prev.variables)));
  return prev.title;
}

/** Get the number of moments in history. */
export function historyLength(): number {
  return history.length;
}

/** Check if a passage has ever been visited. */
export function hasVisited(title: string): boolean {
  return visitedSet.has(title);
}

/** Count how many times a passage appears in the history. */
export function visits(title: string): number {
  let count = 0;
  for (const moment of history) {
    if (moment.title === title) count++;
  }
  return count;
}

/** Get the current passage title. */
export function currentPassage(): string | undefined {
  const top = history[history.length - 1];
  return top?.title;
}

/** Get all passage titles from history. */
export function historyTitles(): string[] {
  return history.map(m => m.title);
}

// --- Persistence ---

const SLOT_PREFIX = "reincarnate-harlowe-save-";

/** Save current state to a named slot. */
export function saveSlot(name: string): boolean {
  try {
    const data = JSON.stringify({
      history,
      variables: storyVars,
    });
    saveLocal(SLOT_PREFIX + name, data);
    return true;
  } catch {
    return false;
  }
}

/** Load state from a named slot. Returns passage title or undefined. */
export function loadSlot(name: string): string | undefined {
  const raw = loadLocal(SLOT_PREFIX + name);
  if (raw === null) return undefined;
  const data = JSON.parse(raw);
  history.length = 0;
  for (const moment of data.history) {
    history.push(moment);
  }
  for (const key of Object.keys(storyVars)) {
    delete storyVars[key];
  }
  Object.assign(storyVars, data.variables);
  const top = history[history.length - 1];
  return top?.title;
}

/** Delete a save slot. */
export function deleteSlot(name: string): void {
  removeLocal(SLOT_PREFIX + name);
}

/** Check if a save slot exists. */
export function hasSlot(name: string): boolean {
  return loadLocal(SLOT_PREFIX + name) !== null;
}

const SLOT_COUNT = 8;

/** Register save/load commands. */
export function initCommands(
  registerCommand: (id: string, binding: string, handler: () => void) => void,
  goto: (passage: string) => void,
): void {
  registerCommand("quicksave", "$mod+s", () => saveSlot("auto"));
  registerCommand("quickload", "", () => {
    const title = loadSlot("auto");
    if (title) goto(title);
  });
  for (let i = 0; i < SLOT_COUNT; i++) {
    const slot = i;
    registerCommand(`save-to-slot-${slot + 1}`, "", () => saveSlot(String(slot)));
    registerCommand(`load-from-slot-${slot + 1}`, "", () => {
      const title = loadSlot(String(slot));
      if (title) goto(title);
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
      (i) => { const t = loadSlot(String(i)); if (t) goto(t); },
      (i) => deleteSlot(String(i)),
    );
  });
}
