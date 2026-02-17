/** SugarCube output rendering.
 *
 * Passage code calls these methods to produce visible output. Content
 * is accumulated in a DocumentFragment buffer, then flushed to the
 * #passages container. A buffer stack supports nested content blocks
 * (link bodies, widget bodies, DOM macro targets). An element stack
 * tracks open HTML elements for proper nesting of structured HTML nodes.
 */

import { scheduleTimeout, cancelTimeout, scheduleInterval, cancelInterval } from "../platform";
import type { DocumentFactory } from "../../../shared/ts/render-root";
import type { SugarCubeRuntime } from "./runtime";

// --- Interfaces ---

interface LinkBlockContext {
  variant: string;
  text: string;
  passage?: string;
}

interface TimedContext {
  delay: number;
  transition?: string;
}

interface RepeatContext {
  interval: number;
  transition?: string;
}

interface TypeContext {
  speed: number;
}

export class SCOutput {
  nobrActive = false;
  bufferStack: DocumentFragment[] = [];
  elementStack: HTMLElement[] = [];
  private linkBlockStack: LinkBlockContext[] = [];
  private timedStack: TimedContext[] = [];
  private activeTimers: number[] = [];
  private repeatStack: RepeatContext[] = [];
  private typeStack: TypeContext[] = [];
  /** Document factory — defaults to global document. */
  doc: DocumentFactory = document;
  /** Passages container element — defaults to document.getElementById("passages"). */
  container: Element | null = null;

  private rt: SugarCubeRuntime;

  constructor(rt: SugarCubeRuntime) {
    this.rt = rt;
  }

  // --- nobr ---

  /** Set the nobr (no line break) flag. When active, break() is suppressed. */
  setNobr(active: boolean): void {
    this.nobrActive = active;
  }

  // --- Buffer stack ---

  private currentBuffer(): DocumentFragment {
    if (this.bufferStack.length === 0) {
      this.bufferStack.push(this.doc.createDocumentFragment());
    }
    return this.bufferStack[this.bufferStack.length - 1];
  }

  /** Push a new output buffer for nested content. */
  pushBuffer(): DocumentFragment {
    const frag = this.doc.createDocumentFragment();
    this.bufferStack.push(frag);
    return frag;
  }

  /** Pop and return the top buffer. */
  popBuffer(): DocumentFragment {
    return this.bufferStack.pop() || this.doc.createDocumentFragment();
  }

  // --- Element stack ---

  private appendNode(node: Node): void {
    if (this.elementStack.length > 0) {
      this.elementStack[this.elementStack.length - 1].appendChild(node);
    } else {
      this.currentBuffer().appendChild(node);
    }
  }

  // --- Core output functions ---

  /** Emit plain text. */
  text(s: string): void {
    this.appendNode(this.doc.createTextNode(s));
  }

  /** Print a value (<<print expr>>). */
  print(v: any): void {
    this.appendNode(this.doc.createTextNode(String(v)));
  }

  // --- Structured HTML element functions ---

  /** Open an HTML element, push onto element stack. */
  open_element(tag: string, ...attrs: string[]): void {
    const el = this.doc.createElement(tag);
    for (let i = 0; i < attrs.length; i += 2) {
      el.setAttribute(attrs[i], attrs[i + 1]);
    }
    this.appendNode(el);
    this.elementStack.push(el);
  }

  /** Close the current open element (pop from element stack). */
  close_element(): void {
    this.elementStack.pop();
  }

  /** Emit a void/self-closing HTML element (no push). */
  void_element(tag: string, ...attrs: string[]): void {
    const el = this.doc.createElement(tag);
    for (let i = 0; i < attrs.length; i += 2) {
      el.setAttribute(attrs[i], attrs[i + 1]);
    }
    this.appendNode(el);
  }

  /** Set a dynamic attribute on the most recently opened/emitted element. */
  set_attribute(name: string, value: any): void {
    let el: Element | null = null;
    if (this.elementStack.length > 0) {
      el = this.elementStack[this.elementStack.length - 1];
    } else {
      const buf = this.currentBuffer();
      el = buf.lastElementChild;
    }
    if (el) {
      el.setAttribute(name, String(value));
    }
  }

  /** Emit a line break. Suppressed when nobr tag is active. */
  break(): void {
    if (this.nobrActive) return;
    this.appendNode(this.doc.createElement("br"));
  }

  // --- Links ---

  /** Emit a simple link (no body content). */
  link(text: string, passage?: string, setter?: () => void): void {
    const a = this.doc.createElement("a");
    a.textContent = text;
    if (passage || setter) {
      a.addEventListener("click", (e) => {
        e.preventDefault();
        if (setter) setter();
        if (passage) {
          this.rt.Navigation.goto(passage);
        }
      });
    }
    this.appendNode(a);
  }

  // --- Images ---

  /** Emit an inline image, optionally wrapped in a link. */
  image(src: string, link?: string): void {
    const img = this.doc.createElement("img");
    img.src = src;
    if (link) {
      const a = this.doc.createElement("a");
      a.href = link;
      a.appendChild(img);
      this.appendNode(a);
    } else {
      this.appendNode(img);
    }
  }

  // --- Link blocks (<<link>> with body) ---

  /** Start a link block — push buffer for body content. */
  link_block_start(variant: string, text: string, passage?: string): void {
    this.linkBlockStack.push({ variant, text, passage });
    this.pushBuffer();
  }

  /** End a link block — pop buffer, wrap in link element. */
  link_block_end(): void {
    const body = this.popBuffer();
    const ctx = this.linkBlockStack.pop();
    if (!ctx) return;

    const wrapper = this.doc.createElement("span");
    wrapper.className = "link-block";

    const a = this.doc.createElement("a");
    a.textContent = ctx.text;
    a.addEventListener("click", (e) => {
      e.preventDefault();
      const parent = a.parentElement;
      if (parent) {
        if (ctx.variant === "linkreplace") {
          while (parent.firstChild) parent.removeChild(parent.firstChild);
        } else if (ctx.variant === "linkprepend") {
          parent.insertBefore(body.cloneNode(true), a);
        } else {
          parent.appendChild(body.cloneNode(true));
        }
        if (ctx.variant !== "link") {
          a.remove();
        }
      }
      if (ctx.passage) {
        this.rt.Navigation.goto(ctx.passage);
      }
    });

    wrapper.appendChild(a);
    this.appendNode(wrapper);
  }

  // --- Timed/Repeat/Type blocks ---

  /** Start a timed block. */
  timed_start(delay: string | number, transition?: string): void {
    const ms = parseDelay(delay);
    this.timedStack.push({ delay: ms, transition });
    this.pushBuffer();
  }

  /** End a timed block — schedule content to appear after delay. */
  timed_end(): void {
    const body = this.popBuffer();
    const ctx = this.timedStack.pop();
    if (!ctx) return;

    const container = this.doc.createElement("span");
    container.className = "timed-content";
    container.style.display = "none";
    container.appendChild(body);
    this.appendNode(container);

    const id = scheduleTimeout(() => {
      container.style.display = "";
    }, ctx.delay);
    this.activeTimers.push(id);
  }

  /** Start a repeat block. */
  repeat_start(interval: string | number, transition?: string): void {
    const ms = parseDelay(interval);
    this.repeatStack.push({ interval: ms, transition });
    this.pushBuffer();
  }

  /** End a repeat block — append content at interval. */
  repeat_end(): void {
    const body = this.popBuffer();
    const ctx = this.repeatStack.pop();
    if (!ctx) return;

    const container = this.doc.createElement("span");
    container.className = "repeat-content";
    this.appendNode(container);

    const id = scheduleInterval(() => {
      container.appendChild(body.cloneNode(true));
    }, ctx.interval);
    this.activeTimers.push(id);
  }

  /** Start a type (typewriter) block. */
  type_start(speed: string | number): void {
    const ms = parseDelay(speed);
    this.typeStack.push({ speed: ms });
    this.pushBuffer();
  }

  /** End a type block — reveal characters one at a time. */
  type_end(): void {
    const body = this.popBuffer();
    const ctx = this.typeStack.pop();
    if (!ctx) return;

    const container = this.doc.createElement("span");
    container.className = "type-content";
    this.appendNode(container);

    const fullText = body.textContent || "";
    let charIndex = 0;

    const id = scheduleInterval(() => {
      if (charIndex < fullText.length) {
        container.textContent = fullText.substring(0, charIndex + 1);
        charIndex++;
      } else {
        while (container.firstChild) container.removeChild(container.firstChild);
        container.appendChild(body);
        cancelInterval(id);
      }
    }, ctx.speed);
    this.activeTimers.push(id);
  }

  // --- Flush/Clear ---

  /** Flush the output buffer to #passages. */
  flush(): void {
    const container = this.container ?? document.getElementById("passages");
    if (!container) return;
    while (this.bufferStack.length > 0) {
      const buf = this.bufferStack.shift()!;
      container.appendChild(buf);
    }
  }

  /** Clear the #passages container and cancel any active timers. */
  clear(): void {
    const container = this.container ?? document.getElementById("passages");
    if (container) {
      while (container.firstChild) {
        container.removeChild(container.firstChild);
      }
    }
    for (const id of this.activeTimers) {
      cancelTimeout(id);
      cancelInterval(id);
    }
    this.activeTimers.length = 0;
    this.bufferStack.length = 0;
    this.elementStack.length = 0;
  }
}

// --- Helpers ---

/** Parse a delay value (number or string like "2s", "500ms"). */
function parseDelay(value: string | number): number {
  if (typeof value === "number") return value;
  const s = value.trim().toLowerCase();
  if (s.endsWith("ms")) return parseFloat(s);
  if (s.endsWith("s")) return parseFloat(s) * 1000;
  return parseFloat(s) || 0;
}
