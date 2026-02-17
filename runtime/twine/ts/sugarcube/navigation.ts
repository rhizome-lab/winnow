/** SugarCube navigation — passage registry, goto/back/return/include. */

import type { SugarCubeRuntime } from "./runtime";

/** Passage function signature: receives the runtime instance. */
export type PassageFn = (rt: SugarCubeRuntime) => void;

export class SCNavigation {
  passages: Map<string, PassageFn> = new Map();
  passageTags: Map<string, string[]> = new Map();
  currentPassage = "";

  private rt: SugarCubeRuntime;

  constructor(rt: SugarCubeRuntime) {
    this.rt = rt;
  }

  /** Run a special passage by name, if it exists. Errors are logged. */
  private runSpecial(name: string): void {
    const fn = this.passages.get(name);
    if (fn) {
      try {
        fn(this.rt);
      } catch (e) {
        console.error(`[navigation] error in ${name}:`, e);
      }
    }
  }

  /** Render a passage with full event lifecycle and special passage support. */
  private renderPassage(target: string, fn: PassageFn): void {
    this.rt.State.clearTemps();
    this.currentPassage = target;
    this.rt.Output.clear();

    const tags = this.passageTags.get(target) || [];
    if (tags.includes("nobr")) {
      this.rt.Output.setNobr(true);
    }

    const passageObj = { title: target, tags };

    this.rt.Events.trigger(":passageinit", { passage: passageObj });
    this.runSpecial("PassageReady");
    this.rt.Events.trigger(":passagestart", { passage: passageObj });
    this.runSpecial("PassageHeader");

    try {
      fn(this.rt);
    } catch (e) {
      console.error(`[navigation] error in passage "${target}":`, e);
      this.rt.Output.text(`Error in passage "${target}": ${e}`);
    }

    this.runSpecial("PassageFooter");
    this.rt.Events.trigger(":passagerender", { passage: passageObj });
    this.runSpecial("PassageDone");

    this.rt.Output.setNobr(false);
    this.rt.Output.flush();

    this.rt.Events.trigger(":passageend", { passage: passageObj });
    this.rt.Events.trigger(":passagedisplay", { passage: passageObj });
  }

  /** Navigate to a passage by name. */
  goto(target: string): void {
    const fn = this.passages.get(target);
    if (!fn) {
      console.error(`[navigation] passage not found: "${target}"`);
      return;
    }
    this.rt.State.pushMoment(target);
    this.renderPassage(target, fn);
  }

  /** Go back to the previous passage. */
  back(): void {
    const title = this.rt.State.popMoment();
    if (title === undefined) {
      console.warn("[navigation] no history to go back to");
      return;
    }
    const fn = this.passages.get(title);
    if (!fn) {
      console.error(`[navigation] passage not found on back: "${title}"`);
      return;
    }
    this.renderPassage(title, fn);
  }

  /** Return to the previous passage (alias for back). */
  return(): void {
    this.back();
  }

  /** Include (embed) another passage inline without navigation. */
  include(passage: string): void {
    const fn = this.passages.get(passage);
    if (!fn) {
      console.error(`[navigation] passage not found for include: "${passage}"`);
      return;
    }
    try {
      fn(this.rt);
    } catch (e) {
      console.error(`[navigation] error in included passage "${passage}":`, e);
      this.rt.Output.text(`Error in passage "${passage}": ${e}`);
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

  /** Get a passage function by name (for widget/engine lookup). */
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
  initCommands(registerCommand: (id: string, binding: string, handler: () => void) => void): void {
    registerCommand("go-back", "", () => this.back());
    registerCommand("restart", "", () => location.reload());
  }
}
