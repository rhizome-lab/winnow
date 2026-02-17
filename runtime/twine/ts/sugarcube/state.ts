/** SugarCube story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * The moment system tracks history for back/return navigation.
 */

import { loadLocal, saveLocal, removeLocal, type SaveSlotInfo, showSaveUI } from "../platform";

// --- Moment type ---

interface Moment {
  title: string;
  variables: Record<string, any>;
}

const SLOT_PREFIX = "reincarnate-save-";
const SLOT_COUNT = 8;

export class SCState {
  private storyVars: Record<string, any> = {};
  private tempVars: Record<string, any> = {};
  private history: Moment[] = [];
  private visitedSet: Set<string> = new Set();

  /** Get a story or temp variable. */
  get(name: string): any {
    if (name.startsWith("_")) {
      return this.tempVars[name];
    }
    return this.storyVars[name];
  }

  /** Set a story or temp variable. */
  set(name: string, value: any): void {
    if (name.startsWith("_")) {
      this.tempVars[name] = value;
    } else {
      this.storyVars[name] = value;
    }
  }

  /** Delete a story or temp variable. */
  unset(name: string): void {
    if (name.startsWith("_")) {
      delete this.tempVars[name];
    } else {
      delete this.storyVars[name];
    }
  }

  /** Clear all temp variables (called at start of each passage). */
  clearTemps(): void {
    for (const key of Object.keys(this.tempVars)) {
      delete this.tempVars[key];
    }
  }

  /** Deep-clone story variables and push onto history. */
  pushMoment(title: string): void {
    this.visitedSet.add(title);
    this.history.push({
      title,
      variables: JSON.parse(JSON.stringify(this.storyVars)),
    });
  }

  /** Pop the most recent moment and restore story variables.
   *  Returns the title of the moment that was restored (the passage
   *  we're going *back to*), or undefined if history is empty.
   */
  popMoment(): string | undefined {
    this.history.pop();
    const prev = this.history[this.history.length - 1];
    if (!prev) {
      return undefined;
    }
    for (const key of Object.keys(this.storyVars)) {
      delete this.storyVars[key];
    }
    Object.assign(this.storyVars, JSON.parse(JSON.stringify(prev.variables)));
    return prev.title;
  }

  /** Peek at the current moment title without popping. */
  peekMoment(): string | undefined {
    const top = this.history[this.history.length - 1];
    return top?.title;
  }

  /** Get the number of moments in history. */
  historyLength(): number {
    return this.history.length;
  }

  /** Check if a passage has ever been visited. */
  hasPlayed(title: string): boolean {
    return this.visitedSet.has(title);
  }

  /** Count how many times a passage appears in the history. */
  visited(title: string): number {
    let count = 0;
    for (const moment of this.history) {
      if (moment.title === title) count++;
    }
    return count;
  }

  /** Get all passage titles from the history in order. */
  passages(): string[] {
    return this.history.map(m => m.title);
  }

  /** Save current state to a named slot. */
  saveSlot(name: string): void {
    const data = JSON.stringify({
      history: this.history,
      variables: this.storyVars,
    });
    saveLocal(SLOT_PREFIX + name, data);
  }

  /** Load state from a named slot. Returns the passage title to navigate to,
   *  or undefined if the slot doesn't exist.
   */
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

  /** Export the full history array for serialization. */
  exportHistory(): Moment[] {
    return JSON.parse(JSON.stringify(this.history));
  }

  /** Export the current story variables for serialization. */
  exportVariables(): Record<string, any> {
    return JSON.parse(JSON.stringify(this.storyVars));
  }

  /** Import full state (history + variables) from deserialized data. */
  importState(importedHistory: Moment[], importedVars: Record<string, any>): void {
    this.history.length = 0;
    for (const moment of importedHistory) {
      this.history.push(moment);
    }
    for (const key of Object.keys(this.storyVars)) {
      delete this.storyVars[key];
    }
    Object.assign(this.storyVars, importedVars);
  }

  /** Register commands for save/load operations. */
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
    registerCommand("export-save", "", () => {
      const data = { history: this.exportHistory(), variables: this.exportVariables() };
      navigator.clipboard.writeText(btoa(JSON.stringify(data)));
    });
  }
}
