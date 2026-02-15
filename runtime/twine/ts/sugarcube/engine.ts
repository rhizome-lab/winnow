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
import * as Macro from "./macro";
import * as Settings from "./settings";
import * as Platform from "../platform";
import jQuery from "jquery";
import { installExtensions } from "./jquery-extensions";
import { Wikifier } from "./wikifier";
import { audioCache, audioGroups, playlists } from "./audio";
import { pushBuffer, popBuffer } from "./output";

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

  // --- jQuery ---
  g.jQuery = jQuery;
  g.$ = jQuery;
  installExtensions();

  // --- version: SugarCube version info (checked by macro addons) ---
  g.version = Object.freeze({
    title: "SugarCube",
    major: 2,
    minor: 36,
    patch: 1,
    prerelease: null,
    build: null,
    date: new Date("2023-01-01"),
    extensions: {},
    toString() { return "2.36.1"; },
    long() { return `SugarCube v2.36.1`; },
  });

  // --- setup: empty object that user scripts populate with game data ---
  if (!g.setup) g.setup = {};

  // --- V: proxy for story variables (State.get/set) ---
  if (!g.V) {
    g.V = new Proxy({} as Record<string, any>, {
      get(_t: any, prop: string) { return State.get(prop); },
      set(_t: any, prop: string, val: any) { State.set(prop, val); return true; },
    });
  }

  // --- Config ---
  g.Config = {
    passages: { nobr: false, descriptions: true, start: "Start", transitionOut: null },
    saves: { autoload: false, autosave: true, id: "reincarnate", isAllowed() { return true; }, slots: 8 },
    history: { controls: true, maxStates: 100 },
    navigation: { override: undefined },
    debug: false,
    addVisitedLinkClass: true,
    cleanupWikifierOutput: false,
  };

  // --- PRNG (seedable pseudo-random number generator) ---
  // SugarCube v2 exposes State.prng for seeded random. Uses a simple
  // mulberry32-based PRNG that can be initialized, serialized, and restored.
  let prngEnabled = false;
  let prngSeed = "";
  let prngState = 0;
  const prngOriginalRandom = Math.random;

  function mulberry32(seed: number): () => number {
    let s = seed | 0;
    return () => {
      s = (s + 0x6D2B79F5) | 0;
      let t = Math.imul(s ^ (s >>> 15), 1 | s);
      t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }

  function hashSeed(seed: string): number {
    let h = 0;
    for (let i = 0; i < seed.length; i++) {
      h = Math.imul(31, h) + seed.charCodeAt(i) | 0;
    }
    return h;
  }

  const prng = {
    init(seed?: string, _useEntropy?: boolean) {
      prngSeed = seed ?? String(Date.now());
      prngState = hashSeed(prngSeed);
      prngEnabled = true;
      const gen = mulberry32(prngState);
      Math.random = gen;
    },
    isEnabled() { return prngEnabled; },
    get pull() { return Math.random(); },
    get seed() { return prngSeed; },
    get state() { return prngState; },
    set state(s: number) {
      prngState = s;
      Math.random = mulberry32(s);
    },
  };

  // --- State ---
  g.State = {
    variables: g.V,
    temporary: new Proxy({} as Record<string, any>, {
      get(_t: any, prop: string) { return State.get("_" + (prop as string)); },
      set(_t: any, prop: string, val: any) { State.set("_" + (prop as string), val); return true; },
    }),
    get active() { return { title: Navigation.current(), variables: g.V }; },
    hasPlayed(passage: string) { return State.hasPlayed(passage); },
    length: 0,
    size: 0,
    isEmpty() { return true; },
    get turns() { return State.historyLength(); },
    get passage() { return Navigation.current(); },
    // Expose underlying get/set for direct access
    getVar(name: string) { return State.get(name); },
    setVar(name: string, value: any) { State.set(name, value); },
    prng,
  };

  // --- Save ---
  const saveSlotCount = 8;
  const onLoadCallbacks = new Set<Function>();
  const onSaveCallbacks = new Set<Function>();

  g.Save = {
    slots: {
      length: saveSlotCount,
      count() {
        let n = 0;
        for (let i = 0; i < saveSlotCount; i++) { if (State.hasSlot(String(i))) n++; }
        return n;
      },
      isEmpty() { return g.Save.slots.count() === 0; },
      has(i: number) { return State.hasSlot(String(i)); },
      get(i: number) {
        const raw = State.loadSlot(String(i));
        return raw !== undefined ? { title: raw } : null;
      },
      load(i: number) {
        const title = State.loadSlot(String(i));
        if (title) {
          for (const cb of onLoadCallbacks) cb();
          Navigation.goto(title);
        }
      },
      save(i: number, title?: string, metadata?: any) {
        for (const cb of onSaveCallbacks) cb();
        State.saveSlot(String(i));
      },
      delete(i: number) { State.deleteSlot(String(i)); },
      ok() { return true; },
    },
    autosave: {
      has() { return State.hasSlot("auto"); },
      get() {
        const raw = State.loadSlot("auto");
        return raw !== undefined ? { title: raw } : null;
      },
      load() {
        const title = State.loadSlot("auto");
        if (title) {
          for (const cb of onLoadCallbacks) cb();
          Navigation.goto(title);
        }
      },
      save(_title?: string, _metadata?: any) { State.saveSlot("auto"); },
      delete() { State.deleteSlot("auto"); },
      ok() { return true; },
    },
    export() {
      // Export full state as base64 JSON
      const data = { history: State.exportHistory(), variables: State.exportVariables() };
      return btoa(JSON.stringify(data));
    },
    import(data: string) {
      try {
        const parsed = JSON.parse(atob(data));
        State.importState(parsed.history, parsed.variables);
        const title = State.peekMoment();
        if (title) Navigation.goto(title);
      } catch (e) {
        console.error("[Save] import failed:", e);
      }
    },
    serialize() { return g.Save.export(); },
    deserialize(data: string) { g.Save.import(data); },
    onLoad: {
      add(fn: Function) { onLoadCallbacks.add(fn); },
      delete(fn: Function) { onLoadCallbacks.delete(fn); },
      clear() { onLoadCallbacks.clear(); },
      get size() { return onLoadCallbacks.size; },
    },
    onSave: {
      add(fn: Function) { onSaveCallbacks.add(fn); },
      delete(fn: Function) { onSaveCallbacks.delete(fn); },
      clear() { onSaveCallbacks.clear(); },
      get size() { return onSaveCallbacks.size; },
    },
  };

  // --- Macro ---
  g.Macro = Macro;

  // --- Template ---
  const templates = new Map<string, Function>();
  g.Template = {
    add(nameOrNames: string | string[], fn: Function) {
      const names = Array.isArray(nameOrNames) ? nameOrNames : [nameOrNames];
      for (const n of names) templates.set(n, fn);
    },
    has(name: string) { return templates.has(name); },
    get(name: string) { return templates.get(name); },
    delete(nameOrNames: string | string[]) {
      const names = Array.isArray(nameOrNames) ? nameOrNames : [nameOrNames];
      for (const n of names) templates.delete(n);
    },
    get size() { return templates.size; },
  };

  // --- Wikifier ---
  g.Wikifier = Wikifier;

  // --- Passage ---
  class PassageShim {
    title: string;
    text: string;
    tags: string[];
    domId: string;
    constructor(title?: string) {
      this.title = title || "";
      this.text = "";
      this.tags = Navigation.getTags(this.title);
      this.domId = "passage-" + this.title.replace(/\s+/g, "-").toLowerCase();
    }
    description(): string { return this.title; }
    processText(): string { return this.text; }
    /** Render this passage's content into a DocumentFragment.
     *  In compiled mode, calls the passage function with a captured output buffer.
     */
    render(): DocumentFragment {
      const fn = Navigation.getPassage(this.title);
      if (!fn) return document.createDocumentFragment();
      pushBuffer();
      try {
        fn();
      } catch (e) {
        console.error(`[Passage.render] error in "${this.title}":`, e);
      }
      return popBuffer();
    }
    static get(name: string): PassageShim { return new PassageShim(name); }
    static has(name: string): boolean { return Navigation.has(name); }
  }
  g.Passage = PassageShim;

  // --- Scripting ---
  g.Scripting = {
    evalJavaScript(expr: string): any {
      return new Function("State", "setup", "V", "Config", `return (${expr})`)(g.State, g.setup, g.V, g.Config);
    },
    evalTwineScript(code: string, _output?: DocumentFragment): void {
      new Function("State", "setup", "V", "Config", code)(g.State, g.setup, g.V, g.Config);
    },
    parse(code: string): string { return code; },
  };

  // --- Engine ---
  g.Engine = {
    play(passage: string) { Navigation.goto(passage); },
    show() {},
    restart() { location.reload(); },
    goto(passage: string) { Navigation.goto(passage); },
    backward() { Navigation.back(); },
    forward() {},
    isPlaying() { return true; },
    state: "playing",
    minDomActionDelay: 40,
  };

  // --- Story ---
  g.Story = {
    get(name: string) { return new PassageShim(name); },
    has(name: string) { return Navigation.has(name); },
    title: "Reincarnate Story",
    domId: "story",
  };

  // --- UIBar ---
  let uiBarStowed = false;
  g.UIBar = {
    stow() { uiBarStowed = true; Platform.stowSidebar(); },
    unstow() { uiBarStowed = false; Platform.unstowSidebar(); },
    destroy() { Platform.destroySidebar(); },
    hide() { Platform.stowSidebar(); },
    show() { Platform.unstowSidebar(); },
    isStowed() { return uiBarStowed; },
  };

  // --- L10n ---
  g.l10nStrings = {};
  g.L10n = {
    get(key: string) { return key; },
  };

  // --- Setting ---
  g.Setting = {
    addToggle: Settings.addToggle,
    addList: Settings.addList,
    addRange: Settings.addRange,
    load: Settings.load,
    save: Settings.save,
    reset: Settings.reset,
  };

  // --- Has (browser feature detection) ---
  g.Has = {
    audio: typeof HTMLAudioElement !== "undefined",
    fileAPI: typeof File !== "undefined" && typeof FileReader !== "undefined" && typeof FileList !== "undefined",
    geolocation: "geolocation" in navigator,
    mutationObserver: typeof MutationObserver !== "undefined",
    performance: typeof performance !== "undefined" && typeof performance.now === "function",
    storage: (() => {
      try { const k = "~sc-test"; localStorage.setItem(k, k); localStorage.removeItem(k); return true; }
      catch { return false; }
    })(),
  };

  // --- LoadScreen ---
  let loadScreenLockId = 0;
  const loadScreenLocks = new Set<number>();
  g.LoadScreen = {
    lock(): number {
      const id = ++loadScreenLockId;
      loadScreenLocks.add(id);
      return id;
    },
    unlock(id: number): void {
      loadScreenLocks.delete(id);
    },
    get size() { return loadScreenLocks.size; },
  };

  // --- Fullscreen ---
  g.Fullscreen = {
    isEnabled(): boolean {
      return document.fullscreenEnabled ?? false;
    },
    isFullscreen(): boolean {
      return document.fullscreenElement != null;
    },
    get element(): Element | null {
      return document.fullscreenElement;
    },
    request(el?: Element): Promise<void> {
      return (el || document.documentElement).requestFullscreen();
    },
    exit(): Promise<void> {
      return document.exitFullscreen ? document.exitFullscreen() : Promise.resolve();
    },
    toggle(el?: Element): Promise<void> {
      return document.fullscreenElement
        ? g.Fullscreen.exit()
        : g.Fullscreen.request(el);
    },
    onChange(fn: EventListener): void {
      document.addEventListener("fullscreenchange", fn);
    },
    offChange(fn: EventListener): void {
      document.removeEventListener("fullscreenchange", fn);
    },
    onError(fn: EventListener): void {
      document.addEventListener("fullscreenerror", fn);
    },
    offError(fn: EventListener): void {
      document.removeEventListener("fullscreenerror", fn);
    },
  };

  // --- SimpleAudio ---
  let saMasterMuted = false;
  let saMasterVolume = 1;
  let saMuteOnHidden = false;

  g.SimpleAudio = {
    load(): void {
      for (const handle of audioCache.values()) {
        Platform.pauseAudio(handle);
        Platform.seekAudio(handle, 0);
      }
    },
    loadWithScreen(): void {
      const lockId = g.LoadScreen.lock();
      g.SimpleAudio.load();
      g.LoadScreen.unlock(lockId);
    },
    mute(state?: boolean): boolean {
      if (state !== undefined) {
        saMasterMuted = state;
        for (const handle of audioCache.values()) {
          Platform.setMuted(handle, state);
        }
      }
      return saMasterMuted;
    },
    muteOnHidden(state?: boolean): boolean {
      if (state !== undefined) saMuteOnHidden = state;
      return saMuteOnHidden;
    },
    volume(level?: number): number {
      if (level !== undefined) {
        saMasterVolume = Math.max(0, Math.min(1, level));
        for (const handle of audioCache.values()) {
          Platform.setVolume(handle, saMasterVolume);
        }
      }
      return saMasterVolume;
    },
    stop(): void {
      for (const handle of audioCache.values()) {
        Platform.stopAudio(handle);
      }
    },
    select(_selector: string) {
      // AudioRunner stub — selector-based batch operations
      return {
        play() {}, pause() {}, stop() {},
        mute(_s?: boolean) { return this; },
        volume(_v?: number) { return this; },
        loop(_s?: boolean) { return this; },
        fade(_duration: number, _toVol: number) { return this; },
        fadeIn(_duration?: number) { return this; },
        fadeOut(_duration?: number) { return this; },
      };
    },
    tracks: {
      add(trackId: string, ...sources: string[]) {
        audioCache.set(trackId, Platform.createAudio(sources));
      },
      get(trackId: string) { return audioCache.get(trackId) ?? null; },
      has(trackId: string) { return audioCache.has(trackId); },
      delete(trackId: string) {
        const handle = audioCache.get(trackId);
        if (handle) {
          Platform.stopAudio(handle);
          audioCache.delete(trackId);
        }
      },
      get length() { return audioCache.size; },
    },
    groups: {
      add(groupId: string, ...trackIds: string[]) { audioGroups.set(groupId, trackIds); },
      get(groupId: string) { return audioGroups.get(groupId) ?? null; },
      has(groupId: string) { return audioGroups.has(groupId); },
      delete(groupId: string) { audioGroups.delete(groupId); },
      get length() { return audioGroups.size; },
    },
    lists: {
      add(listId: string, ...trackIds: string[]) { playlists.set(listId, { tracks: trackIds, currentIndex: 0, loop: false }); },
      get(listId: string) { return playlists.get(listId) ?? null; },
      has(listId: string) { return playlists.has(listId); },
      delete(listId: string) { playlists.delete(listId); },
      get length() { return playlists.size; },
    },
  };

  // --- Commands ---
  // Each module registers its own commands; engine.ts just calls initCommands.
  Settings.initCommands(Platform.registerCommand);
  State.initCommands(Platform.registerCommand, Navigation.goto);
  Navigation.initCommands(Platform.registerCommand);

  // Load persisted settings (after initCommands so toggle/list commands are registered)
  Settings.load();

  // --- Dialog ---
  let dialogTitle = "";
  let dialogBody: HTMLDivElement = document.createElement("div");

  g.Dialog = {
    setup(title?: string, _classNames?: string) {
      dialogTitle = title || "";
      dialogBody = document.createElement("div");
      return dialogBody;
    },
    isOpen() { return Platform.isDialogOpen(); },
    open(_options?: any, _closeFn?: Function) {
      Platform.showDialog(dialogTitle, dialogBody);
    },
    close() { Platform.closeDialog(); },
    body() { return dialogBody; },
    append(...content: any[]) {
      for (const c of content) {
        if (c instanceof Node) dialogBody.appendChild(c);
        else dialogBody.appendChild(document.createTextNode(String(c)));
      }
    },
    wiki(content: string) {
      dialogBody.appendChild(Wikifier.wikifyEval(content));
    },
  };

  // --- passage function (returns current passage name) ---
  g.passage = () => Navigation.current();

  // --- visited / visitedTags ---
  g.visited = (...passageNames: string[]) => {
    if (passageNames.length === 0) {
      return State.visited(Navigation.current());
    }
    return Math.min(...passageNames.map(p => State.visited(p)));
  };
  g.visitedTags = (...tags: string[]) => {
    let count = 0;
    for (const title of State.passages()) {
      const passageTags = Navigation.getTags(title);
      if (tags.every(t => passageTags.includes(t))) count++;
    }
    return count;
  };
  g.turns = () => State.historyLength();
  g.previous = () => {
    const all = State.passages();
    return all.length >= 2 ? all[all.length - 2] : "";
  };

  // --- tags() ---
  g.tags = (...passageNames: string[]) => {
    if (passageNames.length === 0) {
      return Navigation.getTags(Navigation.current());
    }
    const result: string[] = [];
    for (const name of passageNames) {
      result.push(...Navigation.getTags(name));
    }
    return result;
  };

  // --- importStyles(url) ---
  g.importStyles = (url: string): Promise<void> => {
    return new Promise((resolve, reject) => {
      const link = document.createElement("link");
      link.rel = "stylesheet";
      link.href = url;
      link.onload = () => resolve();
      link.onerror = () => reject(new Error(`failed to load stylesheet: ${url}`));
      document.head.appendChild(link);
    });
  };
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
