/** SugarCube-specific jQuery prototype extensions.
 *
 * Patches $.fn with methods that SugarCube adds to jQuery:
 * - .wiki(markup) — parse SugarCube markup and append to each element
 * - .wikiWithOptions(opts, markup) — same with options
 * - .ariaClick(options, handler) — accessible click + keypress handler
 *
 * Call installExtensions() after jQuery is on globalThis.
 */

import { Wikifier, type WikifierOptions } from "./wikifier";

export function installExtensions(): void {
  const $ = (globalThis as any).jQuery;
  if (!$ || !$.fn) return;

  $.fn.wiki = function (this: any, markup: string) {
    return this.each(function (this: HTMLElement) {
      const frag = Wikifier.wikifyEval(markup);
      this.appendChild(frag);
    });
  };

  $.fn.wikiWithOptions = function (this: any, opts: WikifierOptions, markup: string) {
    return this.each(function (this: HTMLElement) {
      const frag = Wikifier.wikifyEval(markup, opts);
      this.appendChild(frag);
    });
  };

  /** Add click + keypress(Enter/Space) handler with ARIA attributes. */
  $.fn.ariaClick = function (this: any, optionsOrHandler: any, handler?: Function) {
    let opts: any = {};
    let fn: Function;

    if (typeof optionsOrHandler === "function") {
      fn = optionsOrHandler;
    } else {
      opts = optionsOrHandler || {};
      fn = handler!;
    }

    return this.each(function (this: HTMLElement) {
      const el = this;

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
