/** Harlowe navigation — passage registry, goto, display (include). */

import * as State from "./state";
import * as Output from "./output";

/** Registry of passage name → passage function. */
const passages: Map<string, () => void> = new Map();

/** Registry of passage name → tags. */
const passageTags: Map<string, string[]> = new Map();

/** Current passage name. */
let currentPassage = "";

/** Register all passage functions and start the story.
 *
 * Follows the same init pattern as Harlowe:
 * 1. Register all passages
 * 2. Navigate to the start passage
 */
export function startStory(
  passageMap: Record<string, () => void>,
  startPassage?: string,
  tagMap?: Record<string, string[]>,
): void {
  for (const [name, fn] of Object.entries(passageMap)) {
    passages.set(name, fn);
  }
  if (tagMap) {
    for (const [name, tags] of Object.entries(tagMap)) {
      passageTags.set(name, tags);
    }
  }

  // Navigate to the explicit start passage, or fall back to first registered
  const target = startPassage || Object.keys(passageMap)[0];
  if (target) {
    goto(target);
  }
}

/** Render a passage with full lifecycle. */
function renderPassage(target: string, fn: () => void): void {
  State.clearTemps();
  currentPassage = target;
  Output.clear();

  try {
    fn();
  } catch (e) {
    console.error(`[harlowe] error in passage "${target}":`, e);
    Output.text(`Error in passage "${target}": ${e}`);
  }

  Output.flush();
}

/** Navigate to a passage by name. */
export function goto(target: string): void {
  const fn = passages.get(target);
  if (!fn) {
    console.error(`[harlowe] passage not found: "${target}"`);
    return;
  }
  State.pushMoment(target);
  renderPassage(target, fn);
}

/** Include (embed) another passage inline without navigation. */
export function display(passage: string): void {
  const fn = passages.get(passage);
  if (!fn) {
    console.error(`[harlowe] passage not found for display: "${passage}"`);
    return;
  }
  try {
    fn();
  } catch (e) {
    console.error(`[harlowe] error in displayed passage "${passage}":`, e);
    Output.text(`Error in passage "${passage}": ${e}`);
  }
}

/** Get the current passage name. */
export function current(): string {
  return currentPassage;
}

/** Check if a passage exists in the registry. */
export function has(name: string): boolean {
  return passages.has(name);
}

/** Get a passage function by name. */
export function getPassage(name: string): (() => void) | undefined {
  return passages.get(name);
}

/** Get the tags for a passage. */
export function getTags(name: string): string[] {
  return passageTags.get(name) || [];
}

/** Get all passage names in the registry. */
export function allPassages(): string[] {
  return Array.from(passages.keys());
}

/** Register commands for navigation. */
export function initCommands(
  registerCommand: (id: string, binding: string, handler: () => void) => void,
): void {
  registerCommand("go-back", "", () => {
    const title = State.popMoment();
    if (title) {
      const fn = passages.get(title);
      if (fn) renderPassage(title, fn);
    }
  });
  registerCommand("restart", "", () => location.reload());
}
