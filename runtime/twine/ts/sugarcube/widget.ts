/** SugarCube widget invocation.
 *
 * Widget functions are emitted as `widget_<name>` and registered in the
 * passage registry alongside regular passages. The <<widget>> macro
 * invocation calls Widget.call(name, ...args), which looks up the
 * function and invokes it.
 */

import * as State from "./state";
import { pushBuffer, popBuffer } from "./output";

/** Invoke a widget by name. */
export function call(name: string, ...args: any[]): void {
  // Set _args temp variable for the widget to access
  State.set("_args", args);

  // Look up widget function in the passage registry
  import("./navigation").then((nav) => {
    const fn = nav.getPassage(name);
    if (fn) {
      fn();
    } else {
      console.warn(`[widget] widget not found: "${name}"`);
    }
  });
}

/** Start a widget content block (<<widget>> body content). */
export function content_start(): void {
  pushBuffer();
}

/** End a widget content block. */
export function content_end(): void {
  const body = popBuffer();
  // Widget body content is captured but consumed by the widget function.
  // The widget accesses it via the _contents temp variable.
  State.set("_contents", body);
}
