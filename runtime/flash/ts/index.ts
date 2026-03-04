import "./flash/vector"; // AS3 Vector compat (Array prototype patches)

export { TimingShim } from "./timing";
export { InputShim } from "./input";
export { RendererShim } from "./renderer";
export { AudioShim } from "./audio";
export { SaveShim } from "./save";
export { UiShim } from "./ui";

import { TimingShim } from "./timing";
import { InputShim } from "./input";
import { RendererShim } from "./renderer";
import { AudioShim } from "./audio";
import { SaveShim } from "./save";
import { UiShim } from "./ui";

/** Holds all Flash shim state for one game instance. */
export class FlashShims {
  constructor(
    public readonly timing: TimingShim,
    public readonly input: InputShim,
    public readonly renderer: RendererShim,
    public readonly audio: AudioShim,
    public readonly save: SaveShim,
    public readonly ui: UiShim,
  ) {}

  static create(canvas: HTMLCanvasElement, savePrefix = "reincarnate:"): FlashShims {
    return new FlashShims(
      new TimingShim(),
      new InputShim(canvas),
      new RendererShim(canvas),
      new AudioShim(),
      new SaveShim(savePrefix),
      new UiShim(),
    );
  }
}

// ---------------------------------------------------------------------------
// Backward-compatible singleton bindings for emitted code.
//
// Emitted Flash code imports these by name: `import { renderer } from "..."`.
// These are live ES module bindings — initFlash() updates them in place so
// imports resolve to the correct instance after initialization.
//
// TODO: update the Flash emitter to thread a FlashShims parameter through
// emitted classes instead of importing module-level names, then remove these.
// ---------------------------------------------------------------------------

export let renderer: RendererShim = undefined as unknown as RendererShim;
export let audio: AudioShim = undefined as unknown as AudioShim;
export let input: InputShim = undefined as unknown as InputShim;
export let timing: TimingShim = undefined as unknown as TimingShim;
export let save: SaveShim = undefined as unknown as SaveShim;
export let ui: UiShim = undefined as unknown as UiShim;

/**
 * Initialize Flash runtime shims against a canvas element.
 * Must be called once before using renderer/input/timing/audio/save/ui.
 * Returns the FlashShims instance (for multi-instance use, create FlashShims.create() directly).
 */
export function initFlash(canvas: HTMLCanvasElement, savePrefix = "reincarnate:"): FlashShims {
  const shims = FlashShims.create(canvas, savePrefix);
  renderer = shims.renderer;
  audio = shims.audio;
  input = shims.input;
  timing = shims.timing;
  save = shims.save;
  ui = shims.ui;
  return shims;
}
