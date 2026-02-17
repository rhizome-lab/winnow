/** Harlowe story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * Harlowe also tracks `it` — the result of the most recent expression.
 */

import type { SaveableState } from "../platform";

export interface Moment {
  title: string;
  variables: Record<string, any>;
}

export class HarloweState implements SaveableState {
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

  // --- SaveableState implementation ---

  serialize(): string {
    const top = this.history[this.history.length - 1];
    return JSON.stringify({
      title: top?.title,
      variables: this.storyVars,
    });
  }

  deserialize(data: string): string | undefined {
    const parsed = JSON.parse(data);
    for (const key of Object.keys(this.storyVars)) {
      delete this.storyVars[key];
    }
    Object.assign(this.storyVars, parsed.variables);
    // Reset history — a loaded save starts a fresh history from the restored passage
    this.history.length = 0;
    if (parsed.title) {
      this.pushMoment(parsed.title);
    }
    return parsed.title;
  }
}
