/** Harlowe story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * Harlowe also tracks `it` — the result of the most recent expression.
 */

import { loadLocal, saveLocal, removeLocal, type SaveSlotInfo, showSaveUI } from "../platform";

export interface Moment {
  title: string;
  variables: Record<string, any>;
}

const SLOT_PREFIX = "reincarnate-harlowe-save-";
const SLOT_COUNT = 8;

export class HarloweState {
  storyVars: Record<string, any> = {};
  tempVars: Record<string, any> = {};
  itValue: any = undefined;
  history: Moment[] = [];
  visitedSet: Set<string> = new Set();

  // --- Variable accessors ---

  /** Get a story variable by name (without the $ prefix).
   *  Uninitialized variables default to 0, matching Harlowe 2.x behavior. */
  get(name: string): any {
    return name in this.storyVars ? this.storyVars[name] : 0;
  }

  /** Set a story variable by name (without the $ prefix). */
  set(name: string, value: any): void {
    this.storyVars[name] = value;
    this.itValue = value;
  }

  /** Get the `it` keyword value (result of most recent expression). */
  get_it(): any {
    return this.itValue;
  }

  /** Clear all temp variables (called at start of each passage). */
  clearTemps(): void {
    for (const key of Object.keys(this.tempVars)) {
      delete this.tempVars[key];
    }
  }

  // --- History ---

  /** Deep-clone story variables and push onto history. */
  pushMoment(title: string): void {
    this.visitedSet.add(title);
    this.history.push({
      title,
      variables: JSON.parse(JSON.stringify(this.storyVars)),
    });
  }

  /** Pop the most recent moment and restore story variables. */
  popMoment(): string | undefined {
    this.history.pop();
    const prev = this.history[this.history.length - 1];
    if (!prev) return undefined;
    for (const key of Object.keys(this.storyVars)) {
      delete this.storyVars[key];
    }
    Object.assign(this.storyVars, JSON.parse(JSON.stringify(prev.variables)));
    return prev.title;
  }

  /** Get the number of moments in history. */
  historyLength(): number {
    return this.history.length;
  }

  /** Check if a passage has ever been visited. */
  hasVisited(title: string): boolean {
    return this.visitedSet.has(title);
  }

  /** Count how many times a passage appears in the history. */
  visits(title: string): number {
    let count = 0;
    for (const moment of this.history) {
      if (moment.title === title) count++;
    }
    return count;
  }

  /** Get the current passage title. */
  currentPassage(): string | undefined {
    const top = this.history[this.history.length - 1];
    return top?.title;
  }

  /** Get all passage titles from history. */
  historyTitles(): string[] {
    return this.history.map(m => m.title);
  }

  /** Forget the n most recent undos. -1 forgets all. */
  forgetUndos(n: number): void {
    if (n < 0) {
      // Keep only the current moment
      if (this.history.length > 1) {
        const current = this.history[this.history.length - 1];
        this.history.length = 0;
        this.history.push(current);
      }
    } else {
      // Remove n most recent moments (keeping at least the current one)
      const keep = Math.max(1, this.history.length - n);
      this.history.splice(0, this.history.length - keep);
    }
  }

  /** Clear visit history. */
  forgetVisits(): void {
    this.visitedSet.clear();
  }

  // --- Persistence ---

  /** Save current state to a named slot. */
  saveSlot(name: string): boolean {
    try {
      const data = JSON.stringify({
        history: this.history,
        variables: this.storyVars,
      });
      saveLocal(SLOT_PREFIX + name, data);
      return true;
    } catch {
      return false;
    }
  }

  /** Load state from a named slot. Returns passage title or undefined. */
  loadSlot(name: string): string | undefined {
    const raw = loadLocal(SLOT_PREFIX + name);
    if (raw === null) return undefined;
    const data = JSON.parse(raw);
    this.history.length = 0;
    for (const moment of data.history) {
      this.history.push(moment);
    }
    for (const key of Object.keys(this.storyVars)) {
      delete this.storyVars[key];
    }
    Object.assign(this.storyVars, data.variables);
    const top = this.history[this.history.length - 1];
    return top?.title;
  }

  /** Delete a save slot. */
  deleteSlot(name: string): void {
    removeLocal(SLOT_PREFIX + name);
  }

  /** Check if a save slot exists. */
  hasSlot(name: string): boolean {
    return loadLocal(SLOT_PREFIX + name) !== null;
  }

  /** Register save/load commands. */
  initCommands(
    registerCommand: (id: string, binding: string, handler: () => void) => void,
    goto: (passage: string) => void,
  ): void {
    registerCommand("quicksave", "$mod+s", () => this.saveSlot("auto"));
    registerCommand("quickload", "", () => {
      const title = this.loadSlot("auto");
      if (title) goto(title);
    });
    for (let i = 0; i < SLOT_COUNT; i++) {
      const slot = i;
      registerCommand(`save-to-slot-${slot + 1}`, "", () => this.saveSlot(String(slot)));
      registerCommand(`load-from-slot-${slot + 1}`, "", () => {
        const title = this.loadSlot(String(slot));
        if (title) goto(title);
      });
    }
    registerCommand("open-saves", "", () => {
      const slots: SaveSlotInfo[] = [];
      for (let i = 0; i < SLOT_COUNT; i++) {
        const has = this.hasSlot(String(i));
        slots.push({ index: i, title: has ? `Save ${i + 1}` : null, date: null, isEmpty: !has });
      }
      showSaveUI(
        slots,
        (i) => this.saveSlot(String(i)),
        (i) => { const t = this.loadSlot(String(i)); if (t) goto(t); },
        (i) => this.deleteSlot(String(i)),
      );
    });
  }
}
