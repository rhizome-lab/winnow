/** Harlowe engine — changers, data operations, runtime helpers.
 *
 * Implements all Harlowe.Engine system call methods dispatched from
 * the compiled passage code.
 */

import { type Changer, HarloweContext, requestStop } from "./context";
import * as State from "./state";
import * as Navigation from "./navigation";

// --- Changers ---

/** Create a changer value from a macro name and arguments. */
export function create_changer(name: string, ...args: any[]): Changer {
  return { name, args };
}

// --- Changer composition ---

/** Check if a value is a Changer object. */
function isChanger(v: any): v is Changer {
  return v != null && typeof v === "object" && "name" in v && "args" in v;
}

/** Compose two changers (or changer arrays) into a changer array. */
function composeChangers(a: Changer | Changer[], b: Changer): Changer[] {
  return Array.isArray(a) ? [...a, b] : [a, b];
}

/** Harlowe `+` operator — composes changers, concatenates arrays/maps/sets,
 *  or falls back to JS `+` for other types. */
export function plus(a: any, b: any): any {
  if (isChanger(a) && isChanger(b)) return composeChangers(a, b);
  if (Array.isArray(a) && isChanger(b)) return composeChangers(a as any, b);
  if (Array.isArray(a) && Array.isArray(b)) return [...a, ...b];
  if (a instanceof Map && b instanceof Map) return new Map([...a, ...b]);
  if (a instanceof Set && b instanceof Set) return new Set([...a, ...b]);
  return a + b;
}

// --- Boolean/logic ---

/** Harlowe `not` operator. */
export function not(val: any): boolean {
  return !val;
}

// --- Control flow ---

/** `(stop:)` — signals the current (live:) interval to stop. */
export function stop(): void {
  requestStop();
}

// --- Save/load ---

/** `(save-game: slot)` — save current game state. Returns true on success. */
export function save_game(slot: any): boolean {
  return State.saveSlot(String(slot));
}

/** `(load-game: slot)` — load game state and navigate to saved passage. */
export function load_game(slot: any): void {
  const title = State.loadSlot(String(slot));
  if (title) {
    Navigation.goto(title);
  }
}

// --- Dialogs ---

/** `(alert: message)` */
export function alert(message: any): void {
  window.alert(String(message));
}

/** `(prompt: message, default?)` */
export function prompt(message: any, defaultValue?: any): string | null {
  return window.prompt(String(message), defaultValue != null ? String(defaultValue) : undefined);
}

/** `(confirm: message)` */
export function confirm(message: any): boolean {
  return window.confirm(String(message));
}

// --- Collection operations ---

/** `contains` operator: checks if a collection contains a value. */
export function contains(collection: any, value: any): boolean {
  if (typeof collection === "string") {
    return collection.includes(String(value));
  }
  if (Array.isArray(collection)) {
    return collection.includes(value);
  }
  if (collection instanceof Set) {
    return collection.has(value);
  }
  if (collection instanceof Map) {
    return collection.has(value);
  }
  if (typeof collection === "object" && collection !== null) {
    return value in collection;
  }
  return false;
}

/** `is in` operator: checks if a value is in a collection. */
export function is_in(value: any, collection: any): boolean {
  return contains(collection, value);
}

// --- Value macros ---

/** `(random: min, max)` */
export function random(...args: any[]): number {
  if (args.length >= 2) {
    const min = Math.floor(Number(args[0]));
    const max = Math.floor(Number(args[1]));
    return Math.floor(Math.random() * (max - min + 1)) + min;
  }
  if (args.length === 1) {
    return Math.floor(Math.random() * Math.floor(Number(args[0])));
  }
  return Math.random();
}

/** `(either: ...values)` — pick one at random. */
export function either(...args: any[]): any {
  return args[Math.floor(Math.random() * args.length)];
}

/** `(str: value)` / `(string: value)` */
export function str(...args: any[]): string {
  return args.map(String).join("");
}

/** `(num: value)` / `(number: value)` */
export function num(value: any): number {
  return Number(value);
}

/** `(a: ...values)` / `(array: ...values)` */
export function array(...args: any[]): any[] {
  return [...args];
}

/** `(dm: key, value, ...)` / `(datamap: key, value, ...)` */
export function datamap(...args: any[]): Map<any, any> {
  const m = new Map();
  for (let i = 0; i + 1 < args.length; i += 2) {
    m.set(args[i], args[i + 1]);
  }
  return m;
}

/** `(ds: ...values)` / `(dataset: ...values)` */
export function dataset(...args: any[]): Set<any> {
  return new Set(args);
}

// --- Property access ---

/** `$var's property` or `(nth: n) of $arr` */
export function get_property(obj: any, prop: any): any {
  if (obj instanceof Map) {
    return obj.get(prop);
  }
  if (obj instanceof Set) {
    const arr = Array.from(obj);
    if (typeof prop === "number") return arr[prop - 1]; // 1-indexed
    if (prop === "length") return arr.length;
    if (prop === "last") return arr[arr.length - 1];
    return undefined;
  }
  if (Array.isArray(obj)) {
    if (typeof prop === "number") return obj[prop - 1]; // 1-indexed
    if (prop === "length") return obj.length;
    if (prop === "last") return obj[obj.length - 1];
    return obj[prop];
  }
  if (typeof obj === "string") {
    if (typeof prop === "number") return obj[prop - 1]; // 1-indexed
    if (prop === "length") return obj.length;
    if (prop === "last") return obj[obj.length - 1];
    return undefined;
  }
  if (typeof obj === "object" && obj !== null) {
    return obj[prop];
  }
  return undefined;
}

/** `$var's property to value` */
export function set_property(obj: any, prop: any, value: any): void {
  if (obj instanceof Map) {
    obj.set(prop, value);
  } else if (Array.isArray(obj)) {
    if (typeof prop === "number") {
      obj[prop - 1] = value; // 1-indexed
    } else {
      obj[prop] = value;
    }
  } else if (typeof obj === "object" && obj !== null) {
    obj[prop] = value;
  }
}

// --- Math ---

/** `(lerp: a, b, t)` — standalone so the call site is monomorphic. */
export function lerp(a: any, b: any, t: any): number {
  const na = Number(a), nb = Number(b), nt = Number(t);
  return na + (nb - na) * nt;
}

/** Math functions dispatched by name (fallback for unknown ops). */
export function math(name: string, ...args: any[]): number {
  const nums = args.map(Number);
  switch (name) {
    case "round": return Math.round(nums[0]);
    case "floor": return Math.floor(nums[0]);
    case "ceil": return Math.ceil(nums[0]);
    case "abs": return Math.abs(nums[0]);
    case "min": return Math.min(...nums);
    case "max": return Math.max(...nums);
    case "sqrt": return Math.sqrt(nums[0]);
    case "sin": return Math.sin(nums[0]);
    case "cos": return Math.cos(nums[0]);
    case "tan": return Math.tan(nums[0]);
    case "log": return Math.log(nums[0]);
    case "pow": return Math.pow(nums[0], nums[1]);
    case "sign": return Math.sign(nums[0]);
    case "clamp": return Math.min(Math.max(nums[0], nums[1]), nums[2]);
    case "lerp": return lerp(args[0], args[1], args[2]);
    default:
      console.warn(`[harlowe] unknown math function: ${name}`);
      return NaN;
  }
}

// --- Collection operations (higher-order) ---

function sorted(...args: any[]): any[] {
  return [...args].flat().sort((a, b) => {
    if (typeof a === "string") return a.localeCompare(b);
    return Number(a) - Number(b);
  });
}

function reversed(...args: any[]): any[] {
  return [...args].flat().reverse();
}

function rotated(...args: any[]): any[] {
  const n = Number(args[0]);
  const arr = args.slice(1);
  const len = arr.length;
  if (len === 0) return [];
  const shift = ((n % len) + len) % len;
  return [...arr.slice(shift), ...arr.slice(0, shift)];
}

function shuffled(...args: any[]): any[] {
  const arr = [...args];
  for (let i = arr.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
  return arr;
}

function count(...args: any[]): number {
  const arr = Array.isArray(args[0]) ? args[0] : [args[0]];
  const val = args[1];
  return arr.filter(x => x === val).length;
}

function range(...args: any[]): number[] {
  const start = Number(args[0]);
  const end = Number(args[1]);
  const result = [];
  for (let i = start; i <= end; i++) result.push(i);
  return result;
}

function find(...args: any[]): any {
  const arr = Array.isArray(args[0]) ? args[0] : [];
  const pred = args[1];
  return typeof pred === "function" ? arr.find(pred) : arr.find(x => x === pred);
}

function joined(...args: any[]): string {
  const sep = args.length > 1 ? String(args[args.length - 1]) : "";
  const items = args.length > 1 ? args.slice(0, -1) : args;
  return items.flat().join(sep);
}

function subarray(...args: any[]): any[] {
  return Array.isArray(args[0]) ? args[0].slice(Number(args[1]) - 1, Number(args[2])) : [];
}

function substring(...args: any[]): string {
  return typeof args[0] === "string" ? args[0].slice(Number(args[1]) - 1, Number(args[2])) : "";
}

function lowercase(...args: any[]): string {
  return String(args[0]).toLowerCase();
}

function uppercase(...args: any[]): string {
  return String(args[0]).toUpperCase();
}

function datanames(...args: any[]): any[] {
  return args[0] instanceof Map ? Array.from(args[0].keys()) : Object.keys(args[0] || {});
}

function datavalues(...args: any[]): any[] {
  return args[0] instanceof Map ? Array.from(args[0].values()) : Object.values(args[0] || {});
}

function dataentries(...args: any[]): any[] {
  return args[0] instanceof Map ? Array.from(args[0].entries()).map(([k, v]) => ({ name: k, value: v })) : [];
}

function somePass(...args: any[]): boolean {
  const pred = args[0];
  const items = args.slice(1);
  if (typeof pred !== "function") return false;
  return items.some(pred);
}

function allPass(...args: any[]): boolean {
  const pred = args[0];
  const items = args.slice(1);
  if (typeof pred !== "function") return false;
  return items.every(pred);
}

function nonePass(...args: any[]): boolean {
  const pred = args[0];
  const items = args.slice(1);
  if (typeof pred !== "function") return false;
  return !items.some(pred);
}

export const Collections = {
  sorted, reversed, rotated, shuffled, count, range,
  find, joined, subarray, substring, lowercase, uppercase,
  datanames, datavalues, dataentries,
  somePass, allPass, nonePass,
} as const;

/** Collection operations (fallback for unknown ops). */
export function collection_op(name: string, ...args: any[]): any {
  const kebabMap: Record<string, string> = {
    "some-pass": "somePass", "all-pass": "allPass", "none-pass": "nonePass",
  };
  const key = kebabMap[name] ?? name;
  const fn = (Collections as any)[key];
  if (fn) return fn(...args);
  console.warn(`[harlowe] unknown collection op: ${name}`);
  return undefined;
}

// --- Value macro (standalone) ---

/** Generic value macro dispatcher. */
export function value_macro(name: string, ...args: any[]): any {
  switch (name) {
    case "str": case "string": return str(...args);
    case "num": case "number": return num(args[0]);
    case "random": return random(...args);
    case "either": return either(...args);
    case "a": case "array": return array(...args);
    case "dm": case "datamap": return datamap(...args);
    case "ds": case "dataset": return dataset(...args);
    default:
      console.warn(`[harlowe] unknown value macro: ${name}`);
      return undefined;
  }
}

// --- Save game queries ---

/** `(saved-games:)` — returns a datamap of slot -> boolean. */
export function saved_games(): Map<string, boolean> {
  const m = new Map<string, boolean>();
  for (let i = 0; i < 8; i++) {
    m.set(String(i), State.hasSlot(String(i)));
  }
  return m;
}

/** `(passage:)` — returns the current passage name. */
export function current_passage(): string {
  return Navigation.current();
}

// --- Meta queries ---

/** `(visits:)`, `(turns:)`, `(time:)`, `(history:)` */
export function meta(name: string): any {
  switch (name) {
    case "visits": return State.historyLength();
    case "turns": return State.historyLength();
    case "time": return 0; // TODO: track elapsed time
    case "history": return State.historyTitles();
    default:
      console.warn(`[harlowe] unknown meta query: ${name}`);
      return undefined;
  }
}

// --- Color operations ---

function rgb(r: any, g: any, b: any): string { return `rgb(${r}, ${g}, ${b})`; }
function rgba(r: any, g: any, b: any, a: any): string { return `rgba(${r}, ${g}, ${b}, ${a})`; }
function hsl(h: any, s: any, l: any): string { return `hsl(${h}, ${s}%, ${l}%)`; }
function hsla(h: any, s: any, l: any, a: any): string { return `hsla(${h}, ${s}%, ${l}%, ${a})`; }

export const Colors = { rgb, rgba, hsl, hsla } as const;

/** Color manipulation functions (fallback for unknown ops). */
export function color_op(name: string, ...args: any[]): string {
  const fn = (Colors as any)[name];
  if (fn) return fn(...args);
  console.warn(`[harlowe] unknown color op: ${name}`);
  return String(args[0] ?? "");
}

// --- DOM macros ---

/** `(replace:)`, `(append:)`, `(prepend:)`, `(show:)`, `(hide:)`, `(rerun:)` */
export function dom_macro(method: string, ...args: any[]): void {
  const selector = args.length > 0 ? String(args[0]) : "";
  const callback = args.length > 1 && typeof args[args.length - 1] === "function"
    ? args[args.length - 1] as (h: HarloweContext) => void
    : undefined;

  const container = document.getElementById("passages");
  if (!container) return;

  switch (method) {
    case "replace": {
      const targets = container.querySelectorAll(selector);
      targets.forEach(el => {
        el.innerHTML = "";
        if (callback) {
          const h = new HarloweContext(el);
          try { callback(h); } finally { h.closeAll(); }
        }
      });
      break;
    }
    case "append": {
      const targets = container.querySelectorAll(selector);
      targets.forEach(el => {
        if (callback) {
          const h = new HarloweContext(el);
          try { callback(h); } finally { h.closeAll(); }
        }
      });
      break;
    }
    case "prepend": {
      const targets = container.querySelectorAll(selector);
      targets.forEach(el => {
        if (callback) {
          const frag = document.createDocumentFragment();
          const h = new HarloweContext(frag);
          try { callback(h); } finally { h.closeAll(); }
          el.insertBefore(frag, el.firstChild);
        }
      });
      break;
    }
    case "show": {
      const targets = container.querySelectorAll(selector);
      targets.forEach(el => (el as HTMLElement).style.display = "");
      break;
    }
    case "hide": {
      const targets = container.querySelectorAll(selector);
      targets.forEach(el => (el as HTMLElement).style.display = "none");
      break;
    }
    case "rerun":
      if (callback) {
        const h = new HarloweContext(container);
        try { callback(h); } finally { h.closeAll(); }
      }
      break;
    default:
      console.warn(`[harlowe] unknown DOM macro: ${method}`);
  }
}

// --- Click macros ---

/** `(click:)`, `(click-replace:)`, `(click-append:)`, `(click-prepend:)` */
export function click_macro(method: string, ...args: any[]): void {
  const selector = args.length > 0 ? String(args[0]) : "";
  const callback = args.length > 1 && typeof args[args.length - 1] === "function"
    ? args[args.length - 1] as (h: HarloweContext) => void
    : undefined;

  const container = document.getElementById("passages");
  if (!container) return;

  const targets = container.querySelectorAll(selector);
  targets.forEach(el => {
    (el as HTMLElement).style.cursor = "pointer";
    el.addEventListener("click", () => {
      if (!callback) return;
      if (method === "click-replace") {
        el.innerHTML = "";
        const h = new HarloweContext(el);
        try { callback(h); } finally { h.closeAll(); }
      } else if (method === "click-append") {
        const h = new HarloweContext(el);
        try { callback(h); } finally { h.closeAll(); }
      } else if (method === "click-prepend") {
        const frag = document.createDocumentFragment();
        const h = new HarloweContext(frag);
        try { callback(h); } finally { h.closeAll(); }
        el.insertBefore(frag, el.firstChild);
      } else {
        // plain click — render inline
        const h = new HarloweContext(el);
        try { callback(h); } finally { h.closeAll(); }
      }
    }, { once: true });
  });
}

// --- Unknown macro fallback ---

/** Handle unknown macros gracefully. */
export function unknown_macro(name: string, ...args: any[]): any {
  console.warn(`[harlowe] unknown macro: (${name}:)`, args);
  return undefined;
}

// --- Generic call fallback ---

/** Handle unknown function calls in expressions. */
export function call(name: string, ...args: any[]): any {
  console.warn(`[harlowe] unknown function call: ${name}`, args);
  return undefined;
}
