/** SugarCube story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * The moment system tracks history for back/return navigation.
 */

import type { SaveableState } from "../platform";

// --- Moment type ---

interface Moment {
  title: string;
  variables: Record<string, any>;
}

export class SCState implements SaveableState {
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

  // --- SaveableState implementation ---

  serialize(): string {
    return JSON.stringify({
      history: this.history,
      variables: this.storyVars,
    });
  }

  deserialize(data: string): string | undefined {
    const parsed = JSON.parse(data);
    this.history.length = 0;
    for (const moment of parsed.history) {
      this.history.push(moment);
    }
    for (const key of Object.keys(this.storyVars)) {
      delete this.storyVars[key];
    }
    Object.assign(this.storyVars, parsed.variables);
    const top = this.history[this.history.length - 1];
    return top?.title;
  }
}
