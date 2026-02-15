/** SugarCube DOM manipulation macros.
 *
 * Block macros like <<replace "#id">>...<<endreplace>> that target
 * existing DOM elements by CSS selector. Each macro uses the output
 * buffer stack to accumulate content, then applies it to matched elements.
 */

import { pushBuffer, popBuffer } from "./output";

// --- DOM macro context stack ---

interface DomMacroContext {
  selector: string;
  operation: string;
  classes?: string[];
}

const domStack: DomMacroContext[] = [];

// --- Replace ---

export function replace_start(selector: string): void {
  domStack.push({ selector, operation: "replace" });
  pushBuffer();
}

export function replace_end(): void {
  const body = popBuffer();
  const ctx = domStack.pop();
  if (!ctx) return;

  const targets = document.querySelectorAll(ctx.selector);
  for (const target of targets) {
    while (target.firstChild) target.removeChild(target.firstChild);
    target.appendChild(body.cloneNode(true));
  }
}

// --- Append ---

export function append_start(selector: string): void {
  domStack.push({ selector, operation: "append" });
  pushBuffer();
}

export function append_end(): void {
  const body = popBuffer();
  const ctx = domStack.pop();
  if (!ctx) return;

  const targets = document.querySelectorAll(ctx.selector);
  for (const target of targets) {
    target.appendChild(body.cloneNode(true));
  }
}

// --- Prepend ---

export function prepend_start(selector: string): void {
  domStack.push({ selector, operation: "prepend" });
  pushBuffer();
}

export function prepend_end(): void {
  const body = popBuffer();
  const ctx = domStack.pop();
  if (!ctx) return;

  const targets = document.querySelectorAll(ctx.selector);
  for (const target of targets) {
    target.insertBefore(body.cloneNode(true), target.firstChild);
  }
}

// --- Copy ---

export function copy_start(selector: string): void {
  domStack.push({ selector, operation: "copy" });
  pushBuffer();
}

export function copy_end(): void {
  popBuffer(); // discard body (copy doesn't use it)
  const ctx = domStack.pop();
  if (!ctx) return;

  const source = document.querySelector(ctx.selector);
  if (!source) return;

  // Clone matched element's content into the #passages container
  const container = document.getElementById("passages");
  if (container) {
    for (const child of Array.from(source.childNodes)) {
      container.appendChild(child.cloneNode(true));
    }
  }
}

// --- Remove ---

export function remove_start(selector: string): void {
  domStack.push({ selector, operation: "remove" });
  pushBuffer();
}

export function remove_end(): void {
  popBuffer(); // discard body
  const ctx = domStack.pop();
  if (!ctx) return;

  const targets = document.querySelectorAll(ctx.selector);
  for (const target of targets) {
    target.remove();
  }
}

// --- Toggle/Add/Remove class ---

export function toggleclass_start(selector: string, ...classes: string[]): void {
  domStack.push({ selector, operation: "toggleclass", classes });
  pushBuffer();
}

export function toggleclass_end(): void {
  popBuffer(); // discard body
  const ctx = domStack.pop();
  if (!ctx || !ctx.classes) return;

  const targets = document.querySelectorAll(ctx.selector);
  for (const target of targets) {
    for (const cls of ctx.classes) {
      target.classList.toggle(cls);
    }
  }
}

export function addclass_start(selector: string, ...classes: string[]): void {
  domStack.push({ selector, operation: "addclass", classes });
  pushBuffer();
}

export function addclass_end(): void {
  popBuffer(); // discard body
  const ctx = domStack.pop();
  if (!ctx || !ctx.classes) return;

  const targets = document.querySelectorAll(ctx.selector);
  for (const target of targets) {
    for (const cls of ctx.classes) {
      target.classList.add(cls);
    }
  }
}

export function removeclass_start(selector: string, ...classes: string[]): void {
  domStack.push({ selector, operation: "removeclass", classes });
  pushBuffer();
}

export function removeclass_end(): void {
  popBuffer(); // discard body
  const ctx = domStack.pop();
  if (!ctx || !ctx.classes) return;

  const targets = document.querySelectorAll(ctx.selector);
  for (const target of targets) {
    for (const cls of ctx.classes) {
      target.classList.remove(cls);
    }
  }
}
