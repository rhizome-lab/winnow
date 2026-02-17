/** SugarCube DOM manipulation macros.
 *
 * Block macros like <<replace "#id">>...<<endreplace>> that target
 * existing DOM elements by CSS selector. Each macro uses the output
 * buffer stack to accumulate content, then applies it to matched elements.
 */

import type { SugarCubeRuntime } from "./runtime";

// --- DOM macro context stack ---

interface DomMacroContext {
  selector: string;
  operation: string;
  classes?: string[];
}

export class SCDOM {
  private domStack: DomMacroContext[] = [];
  private rt: SugarCubeRuntime;

  constructor(rt: SugarCubeRuntime) {
    this.rt = rt;
  }

  private queryRoot(): ParentNode {
    return this.rt.Output.container ?? document;
  }

  private passagesContainer(): Element | null {
    return this.rt.Output.container ?? document.getElementById("passages");
  }

  // --- Replace ---

  replace_start(selector: string): void {
    this.domStack.push({ selector, operation: "replace" });
    this.rt.Output.pushBuffer();
  }

  replace_end(): void {
    const body = this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx) return;

    const targets = this.queryRoot().querySelectorAll(ctx.selector);
    for (const target of targets) {
      while (target.firstChild) target.removeChild(target.firstChild);
      target.appendChild(body.cloneNode(true));
    }
  }

  // --- Append ---

  append_start(selector: string): void {
    this.domStack.push({ selector, operation: "append" });
    this.rt.Output.pushBuffer();
  }

  append_end(): void {
    const body = this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx) return;

    const targets = this.queryRoot().querySelectorAll(ctx.selector);
    for (const target of targets) {
      target.appendChild(body.cloneNode(true));
    }
  }

  // --- Prepend ---

  prepend_start(selector: string): void {
    this.domStack.push({ selector, operation: "prepend" });
    this.rt.Output.pushBuffer();
  }

  prepend_end(): void {
    const body = this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx) return;

    const targets = this.queryRoot().querySelectorAll(ctx.selector);
    for (const target of targets) {
      target.insertBefore(body.cloneNode(true), target.firstChild);
    }
  }

  // --- Copy ---

  copy_start(selector: string): void {
    this.domStack.push({ selector, operation: "copy" });
    this.rt.Output.pushBuffer();
  }

  copy_end(): void {
    this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx) return;

    const source = this.queryRoot().querySelector(ctx.selector);
    if (!source) return;

    const container = this.passagesContainer();
    if (container) {
      for (const child of Array.from(source.childNodes)) {
        container.appendChild(child.cloneNode(true));
      }
    }
  }

  // --- Remove ---

  remove_start(selector: string): void {
    this.domStack.push({ selector, operation: "remove" });
    this.rt.Output.pushBuffer();
  }

  remove_end(): void {
    this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx) return;

    const targets = this.queryRoot().querySelectorAll(ctx.selector);
    for (const target of targets) {
      target.remove();
    }
  }

  // --- Toggle/Add/Remove class ---

  toggleclass_start(selector: string, ...classes: string[]): void {
    this.domStack.push({ selector, operation: "toggleclass", classes });
    this.rt.Output.pushBuffer();
  }

  toggleclass_end(): void {
    this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx || !ctx.classes) return;

    const targets = this.queryRoot().querySelectorAll(ctx.selector);
    for (const target of targets) {
      for (const cls of ctx.classes) {
        target.classList.toggle(cls);
      }
    }
  }

  addclass_start(selector: string, ...classes: string[]): void {
    this.domStack.push({ selector, operation: "addclass", classes });
    this.rt.Output.pushBuffer();
  }

  addclass_end(): void {
    this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx || !ctx.classes) return;

    const targets = this.queryRoot().querySelectorAll(ctx.selector);
    for (const target of targets) {
      for (const cls of ctx.classes) {
        target.classList.add(cls);
      }
    }
  }

  removeclass_start(selector: string, ...classes: string[]): void {
    this.domStack.push({ selector, operation: "removeclass", classes });
    this.rt.Output.pushBuffer();
  }

  removeclass_end(): void {
    this.rt.Output.popBuffer();
    const ctx = this.domStack.pop();
    if (!ctx || !ctx.classes) return;

    const targets = this.queryRoot().querySelectorAll(ctx.selector);
    for (const target of targets) {
      for (const cls of ctx.classes) {
        target.classList.remove(cls);
      }
    }
  }
}
