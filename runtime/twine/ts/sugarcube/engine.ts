/** SugarCube engine operations.
 *
 * Contains runtime helpers for JS constructs that can't be expressed as
 * direct rewrites (ushr, instanceof, clone, eval, etc.) plus iterator
 * protocol and control flow helpers.
 *
 * Methods like `new`, `typeof`, `delete`, `in`, `pow`, `def`, `ndef`,
 * `is_nullish`, and `to_string` are rewritten to native JS constructs by
 * the backend rewrite pass — they never reach this module at runtime.
 */

import * as State from "./state";
import * as Navigation from "./navigation";

// --- Global setup for eval'd scripts ---

let globalsInitialized = false;

/** Set up SugarCube globals on globalThis so eval'd scripts can reference them.
 *
 * DoL's user scripts reference setup, V, Config, State, Save, etc. as
 * bare globals. This function exposes them once, before any eval runs.
 */
function ensureGlobals(): void {
  if (globalsInitialized) return;
  globalsInitialized = true;

  const g = globalThis as any;

  // setup: empty object that user scripts populate with game data
  if (!g.setup) g.setup = {};

  // V: proxy for story variables (State.get/set)
  if (!g.V) {
    g.V = new Proxy({} as Record<string, any>, {
      get(_t: any, prop: string) { return State.get(prop); },
      set(_t: any, prop: string, val: any) { State.set(prop, val); return true; },
    });
  }

  // SugarCube API surface stubs
  if (!g.Config) g.Config = { passages: {}, saves: {}, history: {} };
  if (!g.Save) g.Save = { slots: { length: 8 }, autosave: {} };
  if (!g.Template) g.Template = { size: 0 };
  if (!g.Wikifier) g.Wikifier = {};
  if (!g.Story) {
    g.Story = {
      get: (name: string) => ({ title: name, text: "" }),
      has: (name: string) => Navigation.has(name),
    };
  }
  if (!g.passage) g.passage = () => Navigation.current();

  // Expose State on global for scripts that reference it directly
  if (!g.State) {
    g.State = {
      variables: g.V,
      temporary: {},
      get active() { return { title: Navigation.current(), variables: g.V }; },
    };
  }
}

/** Resolve a bare name (used for function lookups in expression context). */
export function resolve(name: string): any {
  // Check passage/widget registry first, then globalThis
  return (globalThis as any)[name];
}

/** Deep clone a value (SugarCube's clone() function). */
export function clone(value: any): any {
  if (value === null || value === undefined) return value;
  if (typeof value !== "object") return value;
  return JSON.parse(JSON.stringify(value));
}

/** Create an iterator over a collection (for <<for _v range collection>>). */
export function iterate(collection: any): { entries: [any, any][]; index: number } {
  const entries: [any, any][] = [];
  if (Array.isArray(collection)) {
    for (let i = 0; i < collection.length; i++) {
      entries.push([i, collection[i]]);
    }
  } else if (collection && typeof collection === "object") {
    for (const key of Object.keys(collection)) {
      entries.push([key, (collection as any)[key]]);
    }
  }
  return { entries, index: 0 };
}

/** Check if an iterator has more elements. */
export function iterator_has_next(iter: { entries: [any, any][]; index: number }): boolean {
  return iter.index < iter.entries.length;
}

/** Get the next value from an iterator. */
export function iterator_next_value(iter: { entries: [any, any][]; index: number }): any {
  const entry = iter.entries[iter.index];
  iter.index++;
  return entry ? entry[1] : undefined;
}

/** Get the next key from an iterator. */
export function iterator_next_key(iter: { entries: [any, any][]; index: number }): any {
  // index was already advanced by iterator_next_value
  const entry = iter.entries[iter.index - 1];
  return entry ? entry[0] : undefined;
}

/** Unsigned right shift (>>>). */
export function ushr(a: any, b: any): number {
  return (a as number) >>> (b as number);
}

/** instanceof check. */
export function instanceof_(value: any, type_: any): boolean {
  return value instanceof type_;
}
export { instanceof_ as instanceof };

/** Create an arrow function from parameter names and a body expression string. */
export function arrow(params: string, body: string): (...args: any[]) => any {
  ensureGlobals();
  const g = globalThis as any;
  return new Function(
    "State", "setup", "V", "Config",
    `return function(${params}) { return ${body}; }`
  )(g.State, g.setup, g.V, g.Config);
}

/** Evaluate raw JavaScript code (<<script>> blocks). */
// Using a wrapper to avoid shadowing the global eval.
export { evalCode as eval };
function evalCode(code: string): void {
  ensureGlobals();
  const g = globalThis as any;
  const fn = new Function("State", "setup", "V", "Config", code);
  fn(g.State, g.setup, g.V, g.Config);
}

/** Throw an error. */
export function error(message: string): never {
  throw new Error(message);
}

// --- Deferred execution (<<done>>) ---

const doneQueue: (() => void)[] = [];
let doneBuffering = false;

/** Start a <<done>> block (deferred execution until end of passage). */
export function done_start(): void {
  doneBuffering = true;
}

/** End a <<done>> block. */
export function done_end(): void {
  doneBuffering = false;
}

/** Execute all queued done blocks. Called after passage rendering. */
export function flushDone(): void {
  const queued = doneQueue.splice(0);
  for (const fn of queued) {
    fn();
  }
}

// --- Loop control flow ---

/** Sentinel for <<break>>. */
const BREAK_SENTINEL = Symbol("break");

/** Sentinel for <<continue>>. */
const CONTINUE_SENTINEL = Symbol("continue");

/** Break out of a loop (<<break>>). */
// Using a wrapper to avoid JS reserved word.
export { breakLoop as break };
function breakLoop(): never {
  throw BREAK_SENTINEL;
}

/** Continue to next iteration (<<continue>>). */
// Using a wrapper to avoid JS reserved word.
export { continueLoop as continue };
function continueLoop(): never {
  throw CONTINUE_SENTINEL;
}

/** Check if an error is a break sentinel. */
export function isBreak(e: unknown): boolean {
  return e === BREAK_SENTINEL;
}

/** Check if an error is a continue sentinel. */
export function isContinue(e: unknown): boolean {
  return e === CONTINUE_SENTINEL;
}
