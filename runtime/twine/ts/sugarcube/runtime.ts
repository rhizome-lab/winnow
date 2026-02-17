/** SugarCube runtime — owns all mutable sub-objects.
 *
 * Multiple independent game instances can coexist on the same page,
 * each with its own SugarCubeRuntime. No module-level mutable state.
 */

import type { RenderRoot } from "../../../shared/ts/render-root";
import type { PassageFn } from "./navigation";
import { SCState } from "./state";
import { SCEvents } from "./events";
import { SCOutput } from "./output";
import { SCMacro } from "./macro";
import { SCSettings } from "./settings";
import { SCAudio } from "./audio";
import { SCDOM } from "./dom";
import { SCInput } from "./input";
import { SCWidget } from "./widget";
import { SCNavigation } from "./navigation";
import { SCEngine } from "./engine";
import { Wikifier } from "./wikifier";

export class SugarCubeRuntime {
  readonly State: SCState;
  readonly Events: SCEvents;
  readonly Output: SCOutput;
  readonly Macro: SCMacro;
  readonly Settings: SCSettings;
  readonly Audio: SCAudio;
  readonly DOM: SCDOM;
  readonly Input: SCInput;
  readonly Widget: SCWidget;
  readonly Navigation: SCNavigation;
  readonly Engine: SCEngine;

  constructor() {
    Wikifier.rt = this;
    this.State = new SCState();
    this.Events = new SCEvents();
    this.Output = new SCOutput(this);
    this.Macro = new SCMacro(this);
    this.Settings = new SCSettings();
    this.Audio = new SCAudio();
    this.DOM = new SCDOM(this);
    this.Input = new SCInput(this);
    this.Widget = new SCWidget(this);
    this.Navigation = new SCNavigation(this);
    this.Engine = new SCEngine(this);
  }

  /**
   * Register all passage functions and start the story.
   *
   * Follows SugarCube's init order:
   * 1. Register all passages/widgets
   * 2. Run StoryInit passage (if it exists)
   * 3. Navigate to the explicit start passage (or first registered)
   */
  start(
    passageMap: Record<string, PassageFn>,
    startPassage?: string,
    tagMap?: Record<string, string[]>,
    opts?: { root?: RenderRoot },
  ): void {
    if (opts?.root) {
      this.Output.doc = opts.root.doc;
      this.Output.container = opts.root.container as Element;
    }

    for (const [name, fn] of Object.entries(passageMap)) {
      this.Navigation.passages.set(name, fn);
    }
    if (tagMap) {
      for (const [name, tags] of Object.entries(tagMap)) {
        this.Navigation.passageTags.set(name, tags);
      }
    }

    // Run StoryInit if it exists
    const storyInit = this.Navigation.passages.get("StoryInit");
    if (storyInit) {
      try {
        storyInit(this);
      } catch (e) {
        console.error("[navigation] error in StoryInit:", e);
      }
    }

    const target = startPassage || Object.keys(passageMap)[0];
    if (target) {
      this.Navigation.goto(target);
    }

    this.Events.trigger(":storyready");
  }
}

export function createSugarCubeRuntime(): SugarCubeRuntime {
  return new SugarCubeRuntime();
}
