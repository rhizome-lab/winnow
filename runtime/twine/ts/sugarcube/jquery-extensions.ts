/** SugarCube-specific jQuery prototype extensions.
 *
 * Patches $.fn with methods that SugarCube adds to jQuery:
 * - .wiki(markup) — parse SugarCube markup and append to each element
 * - .wikiWithOptions(opts, markup) — same with options
 * - .ariaClick(options, handler) — accessible click + keypress handler
 *
 * Call installExtensions() after jQuery is on globalThis.
 */

import { Wikifier } from "./wikifier";

export function installExtensions(): void {
  const $ = (globalThis as any).jQuery;
  if (!$ || !$.fn) return;

  $.fn.wiki = function (markup: string) {
    return this.each(function () {
      const frag = Wikifier.wikifyEval(markup);
      (this as Element).appendChild(frag);
    });
  };

  $.fn.wikiWithOptions = function (_opts: any, markup: string) {
    return this.wiki(markup);
  };

  /** Add click + keypress(Enter/Space) handler with ARIA attributes. */
  $.fn.ariaClick = function (optionsOrHandler: any, handler?: Function) {
    let opts: any = {};
    let fn: Function;

    if (typeof optionsOrHandler === "function") {
      fn = optionsOrHandler;
    } else {
      opts = optionsOrHandler || {};
      fn = handler!;
    }

    return this.each(function () {
      const el = this as HTMLElement;

      // Set ARIA attributes
      if (!el.hasAttribute("tabindex")) {
        el.setAttribute("tabindex", "0");
      }
      if (!el.hasAttribute("role")) {
        el.setAttribute("role", opts.role || "button");
      }

      el.addEventListener("click", (e: Event) => {
        e.preventDefault();
        fn.call(el, e);
      });

      el.addEventListener("keypress", (e: KeyboardEvent) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          fn.call(el, e);
        }
      });
    });
  };
}
