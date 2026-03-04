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

