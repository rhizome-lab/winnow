/** Harlowe runtime — owns all mutable sub-objects.
 *
 * Multiple independent game instances can coexist on the same page,
 * each with its own HarloweRuntime. No module-level mutable state.
 */

import type { RenderRoot } from "../shared/render-root";
import type { PassageFn } from "./navigation";
import { HarloweState } from "./state";
import { HarloweNavigation } from "./navigation";
import { HarloweEngine } from "./engine";
import { HarloweAudio, type HarloweAudioOpts } from "./audio";
import * as Platform from "../platform";
import type { PersistenceOpts } from "../platform";

export interface HarloweRuntimeOpts {
  /** Audio subsystem options. Set `enabled: false` to disable HAL audio. */
  audio?: HarloweAudioOpts;
}

export class HarloweRuntime {
  readonly State: HarloweState;
  readonly Navigation: HarloweNavigation;
  readonly Engine: HarloweEngine;
  readonly Audio: HarloweAudio;

  constructor(persistence?: PersistenceOpts, opts?: HarloweRuntimeOpts) {
    const history = persistence?.history === "diff"
      ? Platform.diffHistory()
      : Platform.snapshotHistory();
    this.State = new HarloweState(history);
    this.Navigation = new HarloweNavigation(this);
    this.Engine = new HarloweEngine(this);
    this.Audio = new HarloweAudio(opts?.audio);
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
    opts?: { root?: RenderRoot; persistence?: PersistenceOpts; sourceMap?: Record<string, string> },
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
    if (opts?.sourceMap) {
      for (const [name, src] of Object.entries(opts.sourceMap)) {
        this.Engine.passageSources.set(name, src);
      }
    }

    const p = opts?.persistence;

    // Wire persistence
    let backend = Platform.localStorageBackend();
    if (p?.debounce_ms) {
      backend = Platform.debounced(backend, p.debounce_ms);
    }
    Platform.initSave(
      this.State,
      backend,
      this.Navigation.goto.bind(this.Navigation),
      Platform.registerCommand,
      "reincarnate-harlowe-save-",
      p?.autosave,
    );
    this.Navigation.initCommands(Platform.registerCommand);

    // Try to resume from autosave (unless autosave is explicitly disabled or resume is "ignore")
    const autosave = p?.autosave !== false;
    const resume = p?.resume ?? "auto";
    if (autosave && resume !== "ignore") {
      const resumed = Platform.tryResume();
      if (resumed) {
        const fn = this.Navigation.passages.get(resumed);
        if (fn) {
          this.Navigation.renderPassage(resumed, fn);
          return;
        }
      }
    }

    const target = startPassage || Object.keys(passageMap)[0];
    if (target) {
      this.Navigation.goto(target);
    }
  }
}

export function createHarloweRuntime(): HarloweRuntime {
  return new HarloweRuntime();
}
