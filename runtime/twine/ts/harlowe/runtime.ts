/** Harlowe runtime — owns all mutable sub-objects.
 *
 * Multiple independent game instances can coexist on the same page,
 * each with its own HarloweRuntime. No module-level mutable state.
 */

import type { RenderRoot } from "../../../shared/ts/render-root";
import type { PassageFn } from "./navigation";
import { HarloweState } from "./state";
import { HarloweNavigation } from "./navigation";
import { HarloweEngine } from "./engine";
import * as Platform from "../platform";

export class HarloweRuntime {
  readonly State: HarloweState;
  readonly Navigation: HarloweNavigation;
  readonly Engine: HarloweEngine;

  constructor() {
    this.State = new HarloweState();
    this.Navigation = new HarloweNavigation(this);
    this.Engine = new HarloweEngine(this);
  }

  /**
   * Register all passage functions and start the story.
   *
   * Follows the same init pattern as Harlowe:
   * 1. Register all passages
   * 2. Navigate to the start passage
   */
  start(
    passageMap: Record<string, PassageFn>,
    startPassage?: string,
    tagMap?: Record<string, string[]>,
    opts?: { root?: RenderRoot },
  ): void {
    if (opts?.root) {
      this.Navigation.doc = opts.root.doc;
      this.Navigation.container = opts.root.container;
    }

    for (const [name, fn] of Object.entries(passageMap)) {
      this.Navigation.passages.set(name, fn);
    }
    if (tagMap) {
      for (const [name, tags] of Object.entries(tagMap)) {
        this.Navigation.passageTags.set(name, tags);
      }
    }

    // Register commands
    this.State.initCommands(Platform.registerCommand, this.Navigation.goto.bind(this.Navigation));
    this.Navigation.initCommands(Platform.registerCommand);

    // Navigate to the explicit start passage, or fall back to first registered
    const target = startPassage || Object.keys(passageMap)[0];
    if (target) {
      this.Navigation.goto(target);
    }
  }
}

export function createHarloweRuntime(): HarloweRuntime {
  return new HarloweRuntime();
}
