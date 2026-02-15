/** Harlowe output rendering.
 *
 * Passage code calls these functions to produce visible output. Content
 * is accumulated in a DocumentFragment buffer, then flushed to the
 * #passages container. A changer stack applies styling to content as
 * it is emitted.
 */

import { scheduleInterval, cancelInterval } from "../platform";

// --- Changer stack ---

interface Changer {
  name: string;
  args: any[];
}

const changerStack: Changer[] = [];

/** Push a changer onto the stack (affects all output until popped). */
export function push_changer(changer: Changer): void {
  changerStack.push(changer);
}

/** Pop the top changer from the stack. */
export function pop_changer(): void {
  changerStack.pop();
}

// --- Buffer stack ---

const bufferStack: DocumentFragment[] = [];

function currentBuffer(): DocumentFragment {
  if (bufferStack.length === 0) {
    bufferStack.push(document.createDocumentFragment());
  }
  return bufferStack[bufferStack.length - 1];
}

/** Push a new output buffer for nested content. */
export function pushBuffer(): DocumentFragment {
  const frag = document.createDocumentFragment();
  bufferStack.push(frag);
  return frag;
}

/** Pop and return the top buffer. */
export function popBuffer(): DocumentFragment {
  return bufferStack.pop() || document.createDocumentFragment();
}

// --- Changer application ---

/** Wrap an element with current changer stack styling. */
function applyChangers(el: HTMLElement): void {
  for (const changer of changerStack) {
    switch (changer.name) {
      case "color":
      case "colour":
        el.style.color = String(changer.args[0]);
        break;
      case "text-colour":
      case "text-color":
        el.style.color = String(changer.args[0]);
        break;
      case "background":
        el.style.backgroundColor = String(changer.args[0]);
        break;
      case "text-style": {
        const style = String(changer.args[0]);
        switch (style) {
          case "bold": el.style.fontWeight = "bold"; break;
          case "italic": el.style.fontStyle = "italic"; break;
          case "underline": el.style.textDecoration = "underline"; break;
          case "strike": el.style.textDecoration = "line-through"; break;
          case "superscript": el.style.verticalAlign = "super"; el.style.fontSize = "0.8em"; break;
          case "subscript": el.style.verticalAlign = "sub"; el.style.fontSize = "0.8em"; break;
          case "blink": el.style.animation = "blink 1s step-end infinite"; break;
          case "shudder": el.style.animation = "shudder 0.1s infinite"; break;
          case "mark": el.style.backgroundColor = "hsla(60, 100%, 50%, 0.6)"; break;
          case "condense": el.style.letterSpacing = "-0.08em"; break;
          case "expand": el.style.letterSpacing = "0.1em"; break;
          case "outline": el.style.webkitTextStroke = "1px"; el.style.color = "transparent"; break;
          case "shadow": el.style.textShadow = "0.08em 0.08em 0.08em black"; break;
          case "emboss": el.style.textShadow = "0.04em 0.04em 0em rgba(0,0,0,0.5)"; break;
          case "blur": el.style.filter = "blur(2px)"; el.style.transition = "filter 0.3s"; break;
          case "smear": el.style.filter = "blur(1px)"; el.style.textShadow = "0em 0em 0.3em currentColor"; break;
          case "mirror": el.style.transform = "scaleX(-1)"; el.style.display = "inline-block"; break;
          case "upside-down": el.style.transform = "scaleY(-1)"; el.style.display = "inline-block"; break;
          case "fade-in-out": el.style.animation = "fade-in-out 2s ease-in-out infinite"; break;
          case "rumble": el.style.animation = "rumble 0.1s infinite"; break;
        }
        break;
      }
      case "font":
        el.style.fontFamily = String(changer.args[0]);
        break;
      case "text-size":
        el.style.fontSize = String(changer.args[0]);
        break;
      case "align": {
        const align = String(changer.args[0]);
        el.style.textAlign = align;
        break;
      }
      case "opacity":
        el.style.opacity = String(changer.args[0]);
        break;
      case "text-rotate-z":
        el.style.transform = `rotate(${changer.args[0]}deg)`;
        el.style.display = "inline-block";
        break;
      case "css":
        el.setAttribute("style", el.getAttribute("style") + ";" + String(changer.args[0]));
        break;
      case "transition":
      case "transition-time":
      case "transition-arrive":
      case "transition-depart":
        // Store as data attribute for transition system
        el.dataset[`tw_${changer.name.replace(/-/g, "_")}`] = String(changer.args[0]);
        break;
      case "collapse":
        el.classList.add("tw-collapse");
        break;
      case "nobr":
        el.classList.add("tw-nobr");
        break;
      case "hidden":
        el.style.display = "none";
        break;
      case "hover-style":
        // Store hover changer for later application
        el.dataset.tw_hover = JSON.stringify(changer.args[0]);
        break;
    }
  }
}

/** Wrap text/content in a span if changers are active. */
function wrapWithChangers(node: Node): Node {
  if (changerStack.length === 0) return node;
  const span = document.createElement("span");
  span.appendChild(node);
  applyChangers(span);
  return span;
}

// --- Core output functions ---

/** Emit plain text. */
export function text(s: string): void {
  const node = document.createTextNode(s);
  currentBuffer().appendChild(wrapWithChangers(node));
}

/** Print a value (convert to string and emit). */
export function print(v: any): void {
  const node = document.createTextNode(String(v));
  currentBuffer().appendChild(wrapWithChangers(node));
}

/** Emit raw HTML. */
export function html(s: string): void {
  const buf = currentBuffer();
  const temp = document.createElement("template");
  temp.innerHTML = s;
  if (changerStack.length > 0) {
    const span = document.createElement("span");
    span.appendChild(temp.content.cloneNode(true));
    applyChangers(span);
    buf.appendChild(span);
  } else {
    buf.appendChild(temp.content.cloneNode(true));
  }
}

// --- Links ---

/** Emit a link that navigates to a passage. */
export function link(text: string, passage: string): void {
  const buf = currentBuffer();
  const a = document.createElement("a");
  a.textContent = text;
  a.className = "tw-link";
  a.addEventListener("click", (e) => {
    e.preventDefault();
    import("./navigation").then((nav) => nav.goto(passage));
  });
  buf.appendChild(wrapWithChangers(a));
}

/** Emit a link that runs a callback when clicked. */
export function link_callback(text: string, callback: () => void): void {
  const buf = currentBuffer();
  const a = document.createElement("a");
  a.textContent = text;
  a.className = "tw-link";
  a.addEventListener("click", (e) => {
    e.preventDefault();
    callback();
  });
  buf.appendChild(wrapWithChangers(a));
}

// --- Flush/Clear ---

const activeTimers: number[] = [];

/** Flush the output buffer to #passages. */
export function flush(): void {
  const container = document.getElementById("passages");
  if (!container) return;
  while (bufferStack.length > 0) {
    const buf = bufferStack.shift()!;
    container.appendChild(buf);
  }
}

/** Clear the #passages container and cancel active timers. */
export function clear(): void {
  const container = document.getElementById("passages");
  if (container) {
    while (container.firstChild) {
      container.removeChild(container.firstChild);
    }
  }
  for (const id of activeTimers) {
    cancelInterval(id);
  }
  activeTimers.length = 0;
  bufferStack.length = 0;
  changerStack.length = 0;
}

/** Register a timer ID for cleanup on passage transition. */
export function trackTimer(id: number): void {
  activeTimers.push(id);
}
