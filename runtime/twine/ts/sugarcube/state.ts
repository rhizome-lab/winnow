/** SugarCube story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * The moment system tracks history for back/return navigation.
 */

import { loadLocal, saveLocal, removeLocal } from "../platform";

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

/** Deep-clone story variables and push onto history. */
export function pushMoment(title: string): void {
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
