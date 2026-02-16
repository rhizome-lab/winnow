/** Harlowe output rendering.
 *
 * Passage code calls these functions to produce visible output. Content
 * is accumulated in a DocumentFragment buffer, then flushed to the
 * #passages container. A changer stack applies styling to content as
 * it is emitted. An element stack tracks open HTML elements for proper
 * nesting of structured HTML nodes.
 */

import { scheduleInterval, cancelInterval } from "../platform";

// --- Changer stack ---

interface Changer {
  name: string;
  args: any[];
}

const changerStack: Changer[] = [];
const changerFrames: number[] = [];

/** Push a changer onto the stack (affects all output until popped).
 *  Composed changers (arrays from `+` operator) push multiple items
 *  but are tracked as a single frame so pop_changer removes them all. */
export function push_changer(changer: Changer | Changer[]): void {
  if (Array.isArray(changer)) {
    for (const c of changer) changerStack.push(c);
    changerFrames.push(changer.length);
  } else {
    changerStack.push(changer);
    changerFrames.push(1);
  }
}

/** Pop the most recently pushed changer (or composed changer group). */
export function pop_changer(): void {
  const count = changerFrames.pop() || 1;
  for (let i = 0; i < count; i++) changerStack.pop();
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

// --- Element stack (for structured HTML nesting) ---

const elementStack: HTMLElement[] = [];

/** Append a node to the current parent (top of element stack, or buffer). */
function appendNode(node: Node): void {
  if (elementStack.length > 0) {
    elementStack[elementStack.length - 1].appendChild(node);
  } else {
    currentBuffer().appendChild(node);
  }
}

/** Create a container element at the current output position and return it.
 *  Used by live() to scope re-renders to just the live block's area. */
export function createContainer(className?: string): HTMLElement {
  const el = document.createElement("span");
  if (className) el.className = className;
  appendNode(el);
  return el;
}

// --- Color resolution ---

/** Harlowe named color palette — hex values from Harlowe source (colour.js). */
const HARLOWE_COLORS: Record<string, [number, number, number]> = {
  red: [0xe6, 0x19, 0x19],
  orange: [0xe6, 0x80, 0x19],
  yellow: [0xe5, 0xe6, 0x19],
  lime: [0x80, 0xe6, 0x19],
  green: [0x19, 0xe6, 0x19],
  aqua: [0x19, 0xe5, 0xe6],
  cyan: [0x19, 0xe5, 0xe6],
  blue: [0x19, 0x7f, 0xe6],
  navy: [0x19, 0x19, 0xe6],
  purple: [0x7f, 0x19, 0xe6],
  magenta: [0xe6, 0x19, 0xe5],
  fuchsia: [0xe6, 0x19, 0xe5],
  white: [0xff, 0xff, 0xff],
  black: [0x00, 0x00, 0x00],
  grey: [0x88, 0x88, 0x88],
  gray: [0x88, 0x88, 0x88],
};

/** Blend two RGB triples using Harlowe's formula: (a + b) * 0.6, clamped to 255. */
function blendColors(a: [number, number, number], b: [number, number, number]): [number, number, number] {
  return [
    Math.min(Math.round((a[0] + b[0]) * 0.6), 255),
    Math.min(Math.round((a[1] + b[1]) * 0.6), 255),
    Math.min(Math.round((a[2] + b[2]) * 0.6), 255),
  ];
}

/** Resolve a Harlowe color value to a CSS color string.
 *  Handles named colors, color+color composition (chained pairwise
 *  left-to-right), and pass-through for hex/rgb()/hsl() values. */
function resolveColor(value: string): string {
  const s = value.trim().toLowerCase();
  if (s === "transparent") return "transparent";
  if (!s.includes("+")) {
    const rgb = HARLOWE_COLORS[s];
    if (rgb) return `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
    return value;
  }
  const parts = s.split("+");
  const first = HARLOWE_COLORS[parts[0].trim()];
  if (!first) return value;
  let acc: [number, number, number] = first;
  for (let i = 1; i < parts.length; i++) {
    const rgb = HARLOWE_COLORS[parts[i].trim()];
    if (!rgb) return value;
    acc = blendColors(acc, rgb);
  }
  return `rgb(${acc[0]}, ${acc[1]}, ${acc[2]})`;
}

// --- Transition animation support ---

let transitionStylesInjected = false;

/** Inject CSS @keyframes for Harlowe transitions (once). */
function injectTransitionStyles(): void {
  if (transitionStylesInjected) return;
  transitionStylesInjected = true;
  const style = document.createElement("style");
  style.textContent = `
@keyframes tw-dissolve { from { opacity: 0; } to { opacity: 1; } }
@keyframes tw-slide-left { from { transform: translateX(-100%); } to { transform: translateX(0); } }
@keyframes tw-slide-right { from { transform: translateX(100%); } to { transform: translateX(0); } }
@keyframes tw-slide-up { from { transform: translateY(-100%); } to { transform: translateY(0); } }
@keyframes tw-slide-down { from { transform: translateY(100%); } to { transform: translateY(0); } }
@keyframes tw-fade-left { from { opacity: 0; transform: translateX(-50%); } to { opacity: 1; transform: translateX(0); } }
@keyframes tw-fade-right { from { opacity: 0; transform: translateX(50%); } to { opacity: 1; transform: translateX(0); } }
@keyframes tw-fade-up { from { opacity: 0; transform: translateY(-50%); } to { opacity: 1; transform: translateY(0); } }
@keyframes tw-fade-down { from { opacity: 0; transform: translateY(50%); } to { opacity: 1; transform: translateY(0); } }
@keyframes tw-zoom { from { transform: scale(0); } to { transform: scale(1); } }
@keyframes tw-blur { from { filter: blur(10px); opacity: 0; } to { filter: blur(0); opacity: 1; } }
@keyframes tw-flicker { 0% { opacity: 0; } 5% { opacity: 1; } 10% { opacity: 0; } 15% { opacity: 1; } 20% { opacity: 0; } 30% { opacity: 1; } 100% { opacity: 1; } }
@keyframes tw-shudder { 0% { transform: translateX(-3px); } 25% { transform: translateX(3px); } 50% { transform: translateX(-2px); } 75% { transform: translateX(2px); } 100% { transform: translateX(0); } }
@keyframes tw-pulse { 0% { transform: scale(1); } 50% { transform: scale(1.1); } 100% { transform: scale(1); } }
@keyframes tw-rumble { 0% { transform: translate(-2px, 2px); } 25% { transform: translate(2px, -2px); } 50% { transform: translate(-2px, -2px); } 75% { transform: translate(2px, 2px); } 100% { transform: translate(0, 0); } }
`;
  document.head.appendChild(style);
}

/** Apply CSS animation from data-tw_transition attributes. */
function applyTransition(el: HTMLElement): void {
  const name = el.dataset.tw_transition || el.dataset.tw_transition_arrive;
  if (!name) return;
  injectTransitionStyles();
  const duration = el.dataset.tw_transition_time || "0.8s";
  const animName = `tw-${name}`;
  el.style.animation = `${animName} ${duration} ease-in-out`;
}

// --- Alignment resolution ---

/** Translate Harlowe alignment arrow notation to CSS text-align.
 *  "=><=" → center, "=>" → right, "<=" → left, "<=>" → justify.
 *  Proportional forms like "==>" or "===>" map to right (the exact
 *  percentage offset isn't expressible with text-align alone). */
function resolveAlign(value: string): string {
  const s = value.trim();
  if (s === "=><=" || s === "=><=") return "center";
  if (s === "<=>") return "justify";
  if (s.endsWith("<=") && !s.startsWith("<=")) return "center";
  if (/^=+>$/.test(s)) return "right";
  if (/^<=+$/.test(s)) return "left";
  // Already a CSS value (left, center, right, justify)
  return s;
}

// --- Changer application ---

/** Wrap an element with current changer stack styling. */
function applyChangers(el: HTMLElement): void {
  for (const changer of changerStack) {
    switch (changer.name) {
      case "color":
      case "colour":
        el.style.color = resolveColor(String(changer.args[0]));
        break;
      case "text-colour":
      case "text-color":
        el.style.color = resolveColor(String(changer.args[0]));
        break;
      case "background":
        el.style.backgroundColor = resolveColor(String(changer.args[0]));
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
        const align = resolveAlign(String(changer.args[0]));
        el.style.textAlign = align;
        if (align === "center" || align === "right") {
          el.style.display = "block";
        }
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
  // After all changers applied, activate any transition animation
  applyTransition(el);
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
  appendNode(wrapWithChangers(node));
}

/** Print a value (convert to string and emit). */
export function print(v: any): void {
  const node = document.createTextNode(String(v));
  appendNode(wrapWithChangers(node));
}

// --- Structured HTML element functions ---

/** Open an HTML element, apply changers, push onto element stack. */
export function open_element(tag: string, ...attrs: string[]): void {
  const el = document.createElement(tag);
  for (let i = 0; i < attrs.length; i += 2) {
    el.setAttribute(attrs[i], attrs[i + 1]);
  }
  applyChangers(el);
  appendNode(el);
  elementStack.push(el);
}

/** Close the current open element (pop from element stack). */
export function close_element(): void {
  elementStack.pop();
}

/** Emit a void/self-closing HTML element (no push). */
export function void_element(tag: string, ...attrs: string[]): void {
  const el = document.createElement(tag);
  for (let i = 0; i < attrs.length; i += 2) {
    el.setAttribute(attrs[i], attrs[i + 1]);
  }
  applyChangers(el);
  appendNode(el);
}

// --- Links ---

/** Emit a link that navigates to a passage. */
export function link(text: string, passage: string): void {
  const a = document.createElement("a");
  a.textContent = text;
  a.className = "tw-link";
  a.addEventListener("click", (e) => {
    e.preventDefault();
    import("./navigation").then((nav) => nav.goto(passage));
  });
  appendNode(wrapWithChangers(a));
}

/** Emit a link that runs a callback when clicked. */
export function link_callback(text: string, callback: () => void): void {
  const a = document.createElement("a");
  a.textContent = text;
  a.className = "tw-link";
  a.addEventListener("click", (e) => {
    e.preventDefault();
    callback();
  });
  appendNode(wrapWithChangers(a));
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
  changerFrames.length = 0;
  elementStack.length = 0;
}

/** Register a timer ID for cleanup on passage transition. */
export function trackTimer(id: number): void {
  activeTimers.push(id);
}
