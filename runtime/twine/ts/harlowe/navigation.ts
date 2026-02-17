/** Harlowe navigation — passage registry, goto, display (include). */

import * as State from "./state";
import { HarloweContext, clear, wrapInTransitionContainer } from "./context";

/** Passage function type — receives h context, returns void. */
export type PassageFn = (h: HarloweContext) => void;

/** Registry of passage name -> passage function. */
const passages: Map<string, PassageFn> = new Map();

/** Registry of passage name -> tags. */
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
  passageMap: Record<string, PassageFn>,
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
function renderPassage(target: string, fn: PassageFn): void {
  State.clearTemps();
  currentPassage = target;
  clear();

  const story = document.querySelector("tw-story");
  if (!story) return;

  // Create <tw-passage> with tags attribute
  const passage = document.createElement("tw-passage");
  const tags = passageTags.get(target);
  if (tags && tags.length > 0) {
    passage.setAttribute("tags", tags.join(" "));
  }

  // Create <tw-sidebar> with undo/redo icons
  const sidebar = document.createElement("tw-sidebar");
  const undoIcon = document.createElement("tw-icon");
  undoIcon.setAttribute("tabindex", "0");
  undoIcon.setAttribute("title", "Undo");
  undoIcon.textContent = "\u21A9";
  undoIcon.addEventListener("click", () => {
    const title = State.popMoment();
    if (title) {
      const pfn = passages.get(title);
      if (pfn) renderPassage(title, pfn);
    }
  });
  const redoIcon = document.createElement("tw-icon");
  redoIcon.setAttribute("tabindex", "0");
  redoIcon.setAttribute("title", "Redo");
  redoIcon.textContent = "\u21AA";
  sidebar.appendChild(undoIcon);
  sidebar.appendChild(redoIcon);
  passage.appendChild(sidebar);

  story.appendChild(passage);

  // Render content into a transition container (sidebar stays outside)
  const transitionContainer = document.createElement("tw-transition-container") as HTMLElement;
  transitionContainer.style.animation = "tw-dissolve 0.8s ease-in-out";
  transitionContainer.style.display = "block";
  transitionContainer.setAttribute("data-t8n", "dissolve");
  passage.appendChild(transitionContainer);

  const h = new HarloweContext(transitionContainer);
  try {
    fn(h);
  } catch (e) {
    console.error(`[harlowe] error in passage "${target}":`, e);
    transitionContainer.appendChild(document.createTextNode(`Error in passage "${target}": ${e}`));
  } finally {
    h.closeAll();
  }
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

/** Include (embed) another passage inline using the provided context. */
export function display(passage: string, h: HarloweContext): void {
  const fn = passages.get(passage);
  if (!fn) {
    console.error(`[harlowe] passage not found for display: "${passage}"`);
    h.text(`[passage not found: "${passage}"]`);
    return;
  }
  try {
    fn(h);
  } catch (e) {
    console.error(`[harlowe] error in displayed passage "${passage}":`, e);
    h.text(`Error in passage "${passage}": ${e}`);
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
export function getPassage(name: string): PassageFn | undefined {
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
