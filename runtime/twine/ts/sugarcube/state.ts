/** SugarCube story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * The moment system tracks history for back/return navigation.
 */

import { loadLocal, saveLocal, removeLocal, type SaveSlotInfo, showSaveUI } from "../platform";

// --- Variable stores ---

const storyVars: Record<string, any> = {};
const tempVars: Record<string, any> = {};

/** Get a story or temp variable. */
export function get(name: string): any {
  if (name.startsWith("_")) {
    return tempVars[name];
  }
  return storyVars[name];
}

/** Set a story or temp variable. */
export function set(name: string, value: any): void {
  if (name.startsWith("_")) {
    tempVars[name] = value;
  } else {
    storyVars[name] = value;
  }
}

/** Delete a story or temp variable. */
export function unset(name: string): void {
  if (name.startsWith("_")) {
    delete tempVars[name];
  } else {
    delete storyVars[name];
  }
}

/** Clear all temp variables (called at start of each passage). */
export function clearTemps(): void {
  for (const key of Object.keys(tempVars)) {
    delete tempVars[key];
  }
}

// --- Moment system ---

interface Moment {
  title: string;
  variables: Record<string, any>;
}

const history: Moment[] = [];

/** Set of all passages that have been visited during this session. */
const visitedSet: Set<string> = new Set();

/** Deep-clone story variables and push onto history. */
export function pushMoment(title: string): void {
  visitedSet.add(title);
  history.push({
    title,
    variables: JSON.parse(JSON.stringify(storyVars)),
  });
}

/** Pop the most recent moment and restore story variables.
 *  Returns the title of the moment that was restored (the passage
 *  we're going *back to*), or undefined if history is empty.
 */
export function popMoment(): string | undefined {
  // Pop current moment (the one we're leaving)
  history.pop();
  // Peek at the previous moment (the one we're returning to)
  const prev = history[history.length - 1];
  if (!prev) {
    return undefined;
  }
  // Restore story variables from the previous moment
  for (const key of Object.keys(storyVars)) {
    delete storyVars[key];
  }
  Object.assign(storyVars, JSON.parse(JSON.stringify(prev.variables)));
  return prev.title;
}

/** Peek at the current moment title without popping. */
export function peekMoment(): string | undefined {
  const top = history[history.length - 1];
  return top?.title;
}

/** Get the number of moments in history. */
export function historyLength(): number {
  return history.length;
}

// --- Visited tracking ---

/** Check if a passage has ever been visited. */
export function hasPlayed(title: string): boolean {
  return visitedSet.has(title);
}

/** Count how many times a passage appears in the history. */
export function visited(title: string): number {
  let count = 0;
  for (const moment of history) {
    if (moment.title === title) count++;
  }
  return count;
}

/** Get all passage titles from the history in order. */
export function passages(): string[] {
  return history.map(m => m.title);
}

// --- Persistence (save/load slots via platform) ---

const SLOT_PREFIX = "reincarnate-save-";

/** Save current state to a named slot. */
export function saveSlot(name: string): void {
  const data = JSON.stringify({
    history,
    variables: storyVars,
  });
  saveLocal(SLOT_PREFIX + name, data);
}

/** Load state from a named slot. Returns the passage title to navigate to,
 *  or undefined if the slot doesn't exist.
 */
export function loadSlot(name: string): string | undefined {
  const raw = loadLocal(SLOT_PREFIX + name);
  if (raw === null) return undefined;

  const data = JSON.parse(raw);

  // Restore history
  history.length = 0;
  for (const moment of data.history) {
    history.push(moment);
  }

  // Restore story variables
  for (const key of Object.keys(storyVars)) {
    delete storyVars[key];
  }
  Object.assign(storyVars, data.variables);

  // Return the title of the most recent moment
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

// --- Export/Import for Save.export/import ---

/** Export the full history array for serialization. */
export function exportHistory(): Moment[] {
  return JSON.parse(JSON.stringify(history));
}

/** Export the current story variables for serialization. */
export function exportVariables(): Record<string, any> {
  return JSON.parse(JSON.stringify(storyVars));
}

/** Import full state (history + variables) from deserialized data. */
export function importState(importedHistory: Moment[], importedVars: Record<string, any>): void {
  history.length = 0;
  for (const moment of importedHistory) {
    history.push(moment);
  }
  for (const key of Object.keys(storyVars)) {
    delete storyVars[key];
  }
  Object.assign(storyVars, importedVars);
}

const SLOT_COUNT = 8;

/** Register commands for save/load operations. */
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
  registerCommand("export-save", "", () => {
    const data = { history: exportHistory(), variables: exportVariables() };
    navigator.clipboard.writeText(btoa(JSON.stringify(data)));
  });
}
