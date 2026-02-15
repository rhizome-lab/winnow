/** SugarCube navigation — passage registry, goto/back/return/include. */

import * as State from "./state";
import * as Output from "./output";
import * as Events from "./events";

/** Registry of passage name → passage function. */
const passages: Map<string, () => void> = new Map();

/** Registry of passage name → tags. */
const passageTags: Map<string, string[]> = new Map();

/** Current passage name. */
let currentPassage = "";

/** Register all passage functions and start the story.
 *
 * Follows SugarCube's init order:
 * 1. Register all passages/widgets
 * 2. Run StoryInit passage (if it exists)
 * 3. Navigate to the explicit start passage (or first registered)
 */
export function startStory(passageMap: Record<string, () => void>, startPassage?: string, tagMap?: Record<string, string[]>): void {
  for (const [name, fn] of Object.entries(passageMap)) {
    passages.set(name, fn);
  }
  if (tagMap) {
    for (const [name, tags] of Object.entries(tagMap)) {
      passageTags.set(name, tags);
    }
  }

  // Run StoryInit if it exists (initializes story variables)
  const storyInit = passages.get("StoryInit");
  if (storyInit) {
    try {
      storyInit();
    } catch (e) {
      console.error("[navigation] error in StoryInit:", e);
    }
  }

  // Navigate to the explicit start passage, or fall back to first registered
  const target = startPassage || Object.keys(passageMap)[0];
  if (target) {
    goto(target);
  }

  Events.trigger(":storyready");
}

/** Run a special passage by name, if it exists. Errors are logged. */
function runSpecial(name: string): void {
  const fn = passages.get(name);
  if (fn) {
    try {
      fn();
    } catch (e) {
      console.error(`[navigation] error in ${name}:`, e);
    }
  }
}

/** Render a passage with full event lifecycle and special passage support. */
function renderPassage(target: string, fn: () => void): void {
  State.clearTemps();
  currentPassage = target;
  Output.clear();

  // Check for nobr tag
  const tags = passageTags.get(target);
  if (tags && tags.includes("nobr")) {
    Output.setNobr(true);
  }

  Events.trigger(":passageinit", { passage: target });

  // PassageReady runs after :passageinit, before the main passage
  runSpecial("PassageReady");

  Events.trigger(":passagestart", { passage: target });

  // PassageHeader content is prepended before the main passage
  runSpecial("PassageHeader");

  try {
    fn();
  } catch (e) {
    console.error(`[navigation] error in passage "${target}":`, e);
    Output.text(`Error in passage "${target}": ${e}`);
  }

  // PassageFooter content is appended after the main passage
  runSpecial("PassageFooter");

  Events.trigger(":passagerender", { passage: target });

  // PassageDone runs after :passagerender, before flush
  runSpecial("PassageDone");

  Output.setNobr(false);
  Output.flush();

  Events.trigger(":passageend", { passage: target });
  Events.trigger(":passagedisplay", { passage: target });
}

/** Navigate to a passage by name. */
export function goto(target: string): void {
  const fn = passages.get(target);
  if (!fn) {
    console.error(`[navigation] passage not found: "${target}"`);
    return;
  }
  State.pushMoment(target);
  renderPassage(target, fn);
}

/** Go back to the previous passage. */
export function back(): void {
  const title = State.popMoment();
  if (title === undefined) {
    console.warn("[navigation] no history to go back to");
    return;
  }
  const fn = passages.get(title);
  if (!fn) {
    console.error(`[navigation] passage not found on back: "${title}"`);
    return;
  }
  renderPassage(title, fn);
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
  try {
    fn();
  } catch (e) {
    console.error(`[navigation] error in included passage "${passage}":`, e);
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

/** Get a passage function by name (for widget/engine lookup). */
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
export function initCommands(registerCommand: (id: string, binding: string, handler: () => void) => void): void {
  registerCommand("go-back", "", () => back());
  registerCommand("restart", "", () => location.reload());
}
