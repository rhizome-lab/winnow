/** SugarCube navigation — passage registry, goto/back/return/include. */

import * as State from "./state";
import * as Output from "./output";

/** Registry of passage name → passage function. */
const passages: Map<string, () => void> = new Map();

/** Current passage name. */
let currentPassage = "";

/** Register all passage functions and navigate to the first one. */
export function startStory(passageMap: Record<string, () => void>): void {
  for (const [name, fn] of Object.entries(passageMap)) {
    passages.set(name, fn);
  }
  // Navigate to the first passage (insertion order = declaration order).
  const firstName = Object.keys(passageMap)[0];
  if (firstName) {
    goto(firstName);
  }
}

/** Navigate to a passage by name. */
export function goto(target: string): void {
  const fn = passages.get(target);
  if (!fn) {
    console.error(`[navigation] passage not found: "${target}"`);
    return;
  }
  State.pushMoment(target);
  State.clearTemps();
  currentPassage = target;
  Output.clear();
  fn();
  Output.flush();
}

/** Go back to the previous passage. */
export function back(): void {
  const title = State.popMoment();
  if (title === undefined) {
    console.warn("[navigation] no history to go back to");
    return;
  }
  currentPassage = title;
  const fn = passages.get(title);
  if (!fn) {
    console.error(`[navigation] passage not found on back: "${title}"`);
    return;
  }
  State.clearTemps();
  Output.clear();
  fn();
  Output.flush();
}

/** Return to the previous passage (alias for back). */
// Using a wrapper to avoid JS reserved word in export.
export { returnNav as return };
function returnNav(): void {
  back();
}

/** Include (embed) another passage inline without navigation. */
export function include(passage: string): void {
  const fn = passages.get(passage);
  if (!fn) {
    console.error(`[navigation] passage not found for include: "${passage}"`);
    return;
  }
  fn();
}

/** Get the current passage name. */
export function current(): string {
  return currentPassage;
}

/** Check if a passage exists in the registry. */
export function has(name: string): boolean {
  return passages.has(name);
}

/** Get a passage function by name (for widget/engine lookup). */
export function getPassage(name: string): (() => void) | undefined {
  return passages.get(name);
}
