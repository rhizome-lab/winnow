/** Harlowe story variable state management.
 *
 * Two stores: storyVars ($-prefixed) persist across passages,
 * tempVars (_-prefixed) are cleared each passage transition.
 * Harlowe also tracks `it` — the result of the most recent expression.
 */

import { type SaveableState, type HistoryStrategy, snapshotHistory } from "../platform";

export class HarloweState implements SaveableState {
  storyVars: Record<string, any> = {};
  tempVars: Record<string, any> = {};
  itValue: any = undefined;
  private history: HistoryStrategy;

  constructor(history?: HistoryStrategy) {
    this.history = history ?? snapshotHistory();
  }

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

  // --- History (delegated to strategy) ---

  pushMoment(title: string): void {
    this.history.push(title, this.storyVars);
  }

  popMoment(): string | undefined {
    const restored = this.history.pop();
    if (!restored) return undefined;
    for (const key of Object.keys(this.storyVars)) {
      delete this.storyVars[key];
    }
    Object.assign(this.storyVars, restored.vars);
    return restored.title;
  }

  historyLength(): number {
    return this.history.length;
  }

  hasVisited(title: string): boolean {
    return this.history.hasVisited(title);
  }

  visits(title: string): number {
    return this.history.countVisits(title);
  }

  currentPassage(): string | undefined {
    return this.history.peek();
  }

  historyTitles(): string[] {
    return this.history.titles();
  }

  forgetUndos(n: number): void {
    this.history.forgetUndos(n);
  }

  forgetVisits(): void {
    this.history.forgetVisits();
  }

  // --- SaveableState implementation ---

  serialize(): string {
    return JSON.stringify({
      title: this.history.peek(),
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
    this.history.forgetUndos(-1);
    if (parsed.title) {
      this.pushMoment(parsed.title);
    }
    return parsed.title;
  }
}
