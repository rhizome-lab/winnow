/** Harlowe navigation — passage registry, goto, display (include). */

import { HarloweContext, cancelTimers, departOldPassage } from "./context";
import type { HarloweRuntime } from "./runtime";
import type { DocumentFactory } from "../../../shared/ts/render-root";
import { commitSave } from "../platform";

/** Passage function type — receives runtime and h context, returns void. */
export type PassageFn = (rt: HarloweRuntime, h: HarloweContext) => void;

export class HarloweNavigation {
  passages: Map<string, PassageFn> = new Map();
  passageTags: Map<string, string[]> = new Map();
  currentPassage = "";
  lastDepart: { name: string; duration?: string } | undefined;
  /** Document factory — defaults to global document. */
  doc: DocumentFactory = document;
  /** Container element for rendering passages (tw-story equivalent). */
  container: Element | ShadowRoot | null = null;
  /** Active (live:) timer IDs — tracked for cleanup on passage transition. */
  activeTimers: number[] = [];

  private rt: HarloweRuntime;

  constructor(rt: HarloweRuntime) {
    this.rt = rt;
  }

  /** Render a passage with full lifecycle. */
  renderPassage(target: string, fn: PassageFn): void {
    this.rt.State.clearTemps();
    this.currentPassage = target;
    cancelTimers(this.activeTimers);

    const doc = this.doc;
    const story = this.container ?? document.querySelector("tw-story");
    if (!story) return;

    // Animate out old passage (or remove immediately if no depart transition).
    departOldPassage(story as Element, this.lastDepart, doc);
    this.lastDepart = undefined;

    // Create <tw-passage> with tags attribute
    const passage = doc.createElement("tw-passage");
    const tags = this.passageTags.get(target);
    if (tags && tags.length > 0) {
      passage.setAttribute("tags", tags.join(" "));
    }

    // Create <tw-sidebar> with undo/redo icons
    const sidebar = doc.createElement("tw-sidebar");
    const undoIcon = doc.createElement("tw-icon");
    undoIcon.setAttribute("tabindex", "0");
    undoIcon.setAttribute("title", "Undo");
    undoIcon.textContent = "\u21A9";
    undoIcon.addEventListener("click", () => {
      const title = this.rt.State.popMoment();
      if (title) {
        const pfn = this.passages.get(title);
        if (pfn) this.renderPassage(title, pfn);
      }
    });
    const redoIcon = doc.createElement("tw-icon");
    redoIcon.setAttribute("tabindex", "0");
    redoIcon.setAttribute("title", "Redo");
    redoIcon.textContent = "\u21AA";
    sidebar.appendChild(undoIcon);
    sidebar.appendChild(redoIcon);
    passage.appendChild(sidebar);

    story.appendChild(passage);

    const h = new HarloweContext(passage, this.rt, doc);
    try {
      fn(this.rt, h);
    } catch (e) {
      console.error(`[harlowe] error in passage "${target}":`, e);
      passage.appendChild(doc.createTextNode(`Error in passage "${target}": ${e}`));
    } finally {
      h.closeAll();
    }
    // Capture depart transition for use when navigating away from this passage.
    this.lastDepart = h.departTransition;
  }

  /** Navigate to a passage by name. */
  goto(target: string): void {
    const fn = this.passages.get(target);
    if (!fn) {
      console.error(`[harlowe] passage not found: "${target}"`);
      return;
    }
    this.rt.State.pushMoment(target);
    this.renderPassage(target, fn);
    commitSave();
  }

  /** Include (embed) another passage inline using the provided context. */
  display(passage: string, h: HarloweContext): void {
    const fn = this.passages.get(passage);
    if (!fn) {
      console.error(`[harlowe] passage not found for display: "${passage}"`);
      h.text(`[passage not found: "${passage}"]`);
      return;
    }
    try {
      fn(this.rt, h);
    } catch (e) {
      console.error(`[harlowe] error in displayed passage "${passage}":`, e);
      h.text(`Error in passage "${passage}": ${e}`);
    }
  }

  /** Get the current passage name. */
  current(): string {
    return this.currentPassage;
  }

  /** Check if a passage exists in the registry. */
  has(name: string): boolean {
    return this.passages.has(name);
  }

  /** Get a passage function by name. */
  getPassage(name: string): PassageFn | undefined {
    return this.passages.get(name);
  }

  /** Get the tags for a passage. */
  getTags(name: string): string[] {
    return this.passageTags.get(name) || [];
  }

  /** Get all passage names in the registry. */
  allPassages(): string[] {
    return Array.from(this.passages.keys());
  }

  /** Register commands for navigation. */
  initCommands(
    registerCommand: (id: string, binding: string, handler: () => void) => void,
  ): void {
    registerCommand("go-back", "", () => {
      const title = this.rt.State.popMoment();
      if (title) {
        const fn = this.passages.get(title);
        if (fn) this.renderPassage(title, fn);
      }
    });
    registerCommand("restart", "", () => location.reload());
  }

  restart(): void {
    location.reload();
  }
}
