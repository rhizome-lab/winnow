/** SugarCube widget invocation.
 *
 * Widget functions are emitted as `widget_<name>` and registered in the
 * passage registry alongside regular passages. The <<widget>> macro
 * invocation calls Widget.call(name, ...args), which looks up the
 * function and invokes it.
 */

import type { SugarCubeRuntime } from "./runtime";

export class SCWidget {
  private rt: SugarCubeRuntime;

  constructor(rt: SugarCubeRuntime) {
    this.rt = rt;
  }

  /** Invoke a widget by name. */
  call(name: string, ...args: any[]): void {
    this.rt.State.set("_args", args);

    const widgetFn = this.rt.Navigation.getPassage(name);
    if (widgetFn) {
      widgetFn(this.rt);
      return;
    }

    const macroDef = this.rt.Macro.get(name);
    if (macroDef) {
      this.rt.Macro.invokeMacro(macroDef, name, args);
      return;
    }

    console.warn(`[widget] widget not found: "${name}"`);
  }

  /** Start a widget content block (<<widget>> body content). */
  content_start(): void {
    this.rt.Output.pushBuffer();
  }

  /** End a widget content block. */
  content_end(): void {
    const body = this.rt.Output.popBuffer();
    this.rt.State.set("_contents", body);
  }
}
