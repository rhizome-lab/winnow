/** HarloweContext — hyperscript-style content emission for Harlowe passages.
 *
 * Passage functions receive an `h: HarloweContext` parameter and produce
 * output via method calls: `h.text("Hello")`, `h.em(h.strong("world"))`,
 * `h.link("Go", "Next")`. No intermediate ContentNode tree — methods
 * create and append DOM elements directly.
 *
 * Nesting convention:
 * - Return-value style: `h.em(h.strong("world"))` — inner appends first,
 *   outer moves it via appendChild (DOM move semantics).
 * - Callback style: `h.em(() => { h.text("a"); h.strong("b"); })` — for
 *   complex nesting with multiple sequential children.
 * - String children: `h.em("hello")` — shorthand for text node.
 */

import * as State from "./state";
import * as Navigation from "./navigation";
import { scheduleInterval, cancelInterval } from "../platform";

// Re-export Changer type so engine.ts and other modules can import from here.
export interface Changer {
  name: string;
  args: any[];
}

/** A child argument to an element method. */
type Child = Node | string | (() => void);

// --- Color resolution (moved from output.ts) ---

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

function blendColors(a: [number, number, number], b: [number, number, number]): [number, number, number] {
  return [
    Math.min(Math.round((a[0] + b[0]) * 0.6), 255),
    Math.min(Math.round((a[1] + b[1]) * 0.6), 255),
    Math.min(Math.round((a[2] + b[2]) * 0.6), 255),
  ];
}

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

function applyTransition(el: HTMLElement): void {
  const name = el.dataset.tw_transition || el.dataset.tw_transition_arrive;
  if (!name) return;
  injectTransitionStyles();
  const duration = el.dataset.tw_transition_time || "0.8s";
  const animName = `tw-${name}`;
  el.style.animation = `${animName} ${duration} ease-in-out`;
}

// --- Alignment resolution ---

function resolveAlign(value: string): string {
  const s = value.trim();
  if (s === "=><=" || s === "=><=") return "center";
  if (s === "<=>") return "justify";
  if (s.endsWith("<=") && !s.startsWith("<=")) return "center";
  if (/^=+>$/.test(s)) return "right";
  if (/^<=+$/.test(s)) return "left";
  return s;
}

// --- Changer application ---

function applyChanger(el: HTMLElement, changer: Changer): void {
  switch (changer.name) {
    case "color":
    case "colour":
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
      const a = resolveAlign(String(changer.args[0]));
      el.style.textAlign = a;
      if (a === "center" || a === "right") el.style.display = "block";
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
      el.setAttribute("style", (el.getAttribute("style") || "") + ";" + String(changer.args[0]));
      break;
    case "transition":
    case "transition-time":
    case "transition-arrive":
    case "transition-depart":
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
      el.dataset.tw_hover = JSON.stringify(changer.args[0]);
      break;
  }
}

function applyChangers(el: HTMLElement, changers: Changer | Changer[]): void {
  if (Array.isArray(changers)) {
    for (const c of changers) applyChanger(el, c);
  } else {
    applyChanger(el, changers);
  }
  applyTransition(el);
}

// --- Timer tracking ---

const activeTimers: number[] = [];

// --- Stop signal for (live:) ---

let stopRequested = false;

/** Request that the current (live:) interval stops. */
export function requestStop(): void {
  stopRequested = true;
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
}

// --- HarloweContext ---

export class HarloweContext {
  private containerStack: (Element | DocumentFragment)[];

  constructor(container: Element | DocumentFragment) {
    this.containerStack = [container];
  }

  /** Current container (top of stack). */
  private current(): Element | DocumentFragment {
    return this.containerStack[this.containerStack.length - 1];
  }

  /** Push a new container onto the stack. */
  private push(el: Element | DocumentFragment): void {
    this.containerStack.push(el);
  }

  /** Pop the current container from the stack. */
  private pop(): void {
    if (this.containerStack.length > 1) {
      this.containerStack.pop();
    }
  }

  /** Close all open containers (safety net for early exit). */
  closeAll(): void {
    while (this.containerStack.length > 1) {
      this.containerStack.pop();
    }
  }

  // --- Child processing ---

  /** Append children to an element. */
  private appendChildren(parent: Element, children: Child[]): void {
    for (const child of children) {
      if (typeof child === "string") {
        parent.appendChild(document.createTextNode(child));
      } else if (typeof child === "function") {
        this.push(parent);
        child();
        this.pop();
      } else if (child instanceof Node) {
        parent.appendChild(child);
      }
    }
  }

  // --- Content methods ---

  /** Create and append a text node. */
  text(s: string): Node {
    const node = document.createTextNode(s);
    this.current().appendChild(node);
    return node;
  }

  /** Create and append a <br>. */
  br(): Node {
    const el = document.createElement("br");
    this.current().appendChild(el);
    return el;
  }

  /** Create and append an <hr>. */
  hr(): Node {
    const el = document.createElement("hr");
    this.current().appendChild(el);
    return el;
  }

  /** Create and append an <img>. */
  img(src: string): Node {
    const el = document.createElement("img");
    el.src = src;
    this.current().appendChild(el);
    return el;
  }

  /** Generic void element (no children). */
  voidEl(tag: string, ...attrs: string[]): Node {
    const el = document.createElement(tag);
    for (let i = 0; i + 1 < attrs.length; i += 2) {
      el.setAttribute(attrs[i], attrs[i + 1]);
    }
    this.current().appendChild(el);
    return el;
  }

  /** Generic element with children. */
  el(tag: string, ...children: Child[]): Node {
    const el = document.createElement(tag);
    this.appendChildren(el, children);
    this.current().appendChild(el);
    return el;
  }

  /** <strong> */
  strong(...children: Child[]): Node {
    return this.el("strong", ...children);
  }

  /** <em> */
  em(...children: Child[]): Node {
    return this.el("em", ...children);
  }

  /** <del> */
  del(...children: Child[]): Node {
    return this.el("del", ...children);
  }

  /** <sup> */
  sup(...children: Child[]): Node {
    return this.el("sup", ...children);
  }

  /** <sub> */
  sub(...children: Child[]): Node {
    return this.el("sub", ...children);
  }

  // --- Interactive elements ---

  /** Passage navigation link. */
  link(text: string, passage: string): Node {
    const a = document.createElement("a");
    a.textContent = text;
    a.className = "tw-link";
    a.addEventListener("click", (e) => {
      e.preventDefault();
      Navigation.goto(passage);
    });
    this.current().appendChild(a);
    return a;
  }

  /** Link with callback (replaces itself with callback output). */
  linkCb(text: string, cb: (h: HarloweContext) => void): Node {
    const a = document.createElement("a");
    a.textContent = text;
    a.className = "tw-link";
    a.addEventListener("click", (e) => {
      e.preventDefault();
      const frag = document.createDocumentFragment();
      const subH = new HarloweContext(frag);
      try {
        cb(subH);
      } finally {
        subH.closeAll();
      }
      a.replaceWith(frag);
    });
    this.current().appendChild(a);
    return a;
  }

  /** Timed content — runs callback at interval, replacing content each time. */
  live(interval: number, cb: (h: HarloweContext) => void): Node {
    const container = document.createElement("span");
    container.className = "tw-live";
    this.current().appendChild(container);
    const ms = interval * 1000;
    const id = scheduleInterval(() => {
      container.innerHTML = "";
      const subH = new HarloweContext(container);
      try {
        cb(subH);
      } finally {
        subH.closeAll();
      }
      if (stopRequested) {
        cancelInterval(id);
        stopRequested = false;
      }
    }, ms);
    activeTimers.push(id);
    return container;
  }

  /** Print a value as text. */
  printVal(v: any): Node {
    const node = document.createTextNode(String(v));
    this.current().appendChild(node);
    return node;
  }

  /** Display (embed) another passage inline. */
  displayPassage(name: string): void {
    Navigation.display(name, this);
  }

  // --- Styled content ---

  /** Apply a changer (or changer array) to a span wrapping children. */
  styled(changer: Changer | Changer[], ...children: Child[]): Node {
    const span = document.createElement("span");
    applyChangers(span, changer);
    this.appendChildren(span, children);
    this.current().appendChild(span);
    return span;
  }

  /** Shorthand changer+content methods. */
  color(v: string, ...children: Child[]): Node {
    return this.styled({ name: "color", args: [v] }, ...children);
  }

  background(v: string, ...children: Child[]): Node {
    return this.styled({ name: "background", args: [v] }, ...children);
  }

  textStyle(v: string, ...children: Child[]): Node {
    return this.styled({ name: "text-style", args: [v] }, ...children);
  }

  font(v: string, ...children: Child[]): Node {
    return this.styled({ name: "font", args: [v] }, ...children);
  }

  align(v: string, ...children: Child[]): Node {
    return this.styled({ name: "align", args: [v] }, ...children);
  }

  opacity(v: number, ...children: Child[]): Node {
    return this.styled({ name: "opacity", args: [v] }, ...children);
  }

  css(v: string, ...children: Child[]): Node {
    return this.styled({ name: "css", args: [v] }, ...children);
  }

  transition(v: string, ...children: Child[]): Node {
    return this.styled({ name: "transition", args: [v] }, ...children);
  }

  transitionTime(v: number, ...children: Child[]): Node {
    return this.styled({ name: "transition-time", args: [v] }, ...children);
  }

  hidden(...children: Child[]): Node {
    return this.styled({ name: "hidden", args: [true] }, ...children);
  }

  textSize(v: string, ...children: Child[]): Node {
    return this.styled({ name: "text-size", args: [v] }, ...children);
  }

  textRotateZ(v: number, ...children: Child[]): Node {
    return this.styled({ name: "text-rotate-z", args: [v] }, ...children);
  }

  collapse(...children: Child[]): Node {
    return this.styled({ name: "collapse", args: [true] }, ...children);
  }

  nobr(...children: Child[]): Node {
    return this.styled({ name: "nobr", args: [true] }, ...children);
  }

  hoverStyle(v: any, ...children: Child[]): Node {
    return this.styled({ name: "hover-style", args: [v] }, ...children);
  }

  // --- State ---

  /** Get a story variable. */
  get(name: string): any {
    return State.get(name);
  }

  /** Set a story variable. */
  set(name: string, value: any): void {
    State.set(name, value);
  }

  /** Get the `it` keyword value. */
  get_it(): any {
    return State.get_it();
  }

  // --- Navigation ---

  /** Navigate to a passage. */
  goto(target: string): void {
    Navigation.goto(target);
  }
}
