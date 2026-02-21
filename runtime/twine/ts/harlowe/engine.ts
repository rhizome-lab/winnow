/** Harlowe engine — changers, data operations, runtime helpers.
 *
 * Implements all Harlowe.Engine system call methods dispatched from
 * the compiled passage code.
 */

import { type Changer, HarloweContext } from "./context";
import type { HarloweRuntime } from "./runtime";
import type { DocumentFactory } from "../shared/render-root";

/** Passage info object returned by `(open-storylets:)`. */
export interface PassageInfo {
  name: string;
  source: string;
  tags: string[];
}

export class HarloweEngine {
  private rt: HarloweRuntime;
  /** Map from passage name to its storylet condition function. */
  private storyletConditions: Record<string, (rt: HarloweRuntime) => boolean> = {};
  /** Map from passage name to its raw source text, for `(source:)`. */
  passageSources: Map<string, string> = new Map();

  constructor(rt: HarloweRuntime) {
    this.rt = rt;
  }

  /** Register the storylet condition map for `(open-storylets:)`. */
  registerStoryletsConditions(conditions: Record<string, (rt: HarloweRuntime) => boolean>): void {
    this.storyletConditions = conditions;
  }

  /** `(open-storylets:)` / `(open-storylets: where lambda)` —
   *  Returns passage info objects for all storylets whose condition is true.
   *  Optional filter lambda `(info) => boolean` further restricts results. */
  open_storylets(filter?: (item: PassageInfo) => boolean): PassageInfo[] {
    const results: PassageInfo[] = [];
    for (const [name, cond] of Object.entries(this.storyletConditions)) {
      if (!cond(this.rt)) continue;
      const tags = this.rt.Navigation.getTags(name);
      const info: PassageInfo = { name, source: this.passageSources.get(name) ?? "", tags };
      if (!filter || filter(info)) {
        results.push(info);
      }
    }
    return results;
  }

  // --- DOM helpers ---

  /** Get the document factory. */
  private doc() { return this.rt.Navigation.doc; }

  /** Get the tw-story container. */
  private story() { return this.rt.Navigation.container ?? document.querySelector("tw-story"); }

  /** Get the current tw-passage or tw-story. */
  private passage() {
    const s = this.story();
    if (s && (s as Element).querySelector) {
      return (s as Element).querySelector("tw-passage") ?? s;
    }
    return s;
  }

  // --- Changers ---

  /** Create a changer value from a macro name and arguments. */
  create_changer(name: string, ...args: any[]): Changer {
    return { name, args };
  }

  // --- Changer composition ---

  /** Harlowe `+` operator — composes changers, concatenates arrays/maps/sets,
   *  or falls back to JS `+` for other types. */
  plus(a: any, b: any): any {
    if (isChanger(a) && isChanger(b)) return composeChangers(a, b);
    if (Array.isArray(a) && isChanger(b)) return composeChangers(a as any, b);
    if (Array.isArray(a) && Array.isArray(b)) return [...a, ...b];
    if (a instanceof Map && b instanceof Map) return new Map([...a, ...b]);
    if (a instanceof Set && b instanceof Set) return new Set([...a, ...b]);
    return a + b;
  }

  /** Harlowe `-` operator — removes elements from arrays/sets, or falls back
   *  to JS `-` for numbers. */
  minus(a: any, b: any): any {
    if (Array.isArray(a) && Array.isArray(b)) {
      const remove = new Set(b);
      return (a as any[]).filter((x) => !remove.has(x));
    }
    if (a instanceof Set && Array.isArray(b)) return new Set([...a].filter((x) => !b.includes(x)));
    return a - b;
  }

  // --- Boolean/logic ---

  /** Harlowe `not` operator. */
  not(val: any): boolean {
    return !val;
  }

  // --- Save/load ---

  /** `(save-game: slot)` — save current game state. Returns true on success. */
  save_game(slot: string | number): boolean {
    return this.rt.State.saveSlot(String(slot));
  }

  /** `(load-game: slot)` — load game state and navigate to saved passage. */
  load_game(slot: string | number): void {
    const title = this.rt.State.loadSlot(String(slot));
    if (title) {
      this.rt.Navigation.goto(title);
    }
  }

  // --- Dialogs ---

  /** `(alert: message)` */
  alert(message: any): void {
    window.alert(String(message));
  }

  /** `(prompt: message, default?)` */
  prompt(message: any, defaultValue?: any): string | null {
    return window.prompt(String(message), defaultValue != null ? String(defaultValue) : undefined);
  }

  /** `(confirm: message)` */
  confirm(message: any): boolean {
    return window.confirm(String(message));
  }

  // --- Collection operations ---

  /** `contains` operator: checks if a collection contains a value. */
  contains(collection: any, value: any): boolean {
    return contains(collection, value);
  }

  /** `is in` operator: checks if a value is in a collection. */
  is_in(value: any, collection: any): boolean {
    return contains(collection, value);
  }

  /** `is a TYPE` / `is an TYPE` type-check operator. */
  is_a(value: any, type: string): boolean {
    switch (type) {
      case "boolean": return typeof value === "boolean";
      case "number": return typeof value === "number";
      case "string": return typeof value === "string";
      case "integer": return typeof value === "number" && Number.isInteger(value);
      case "array": return Array.isArray(value);
      case "datamap": return value instanceof Map;
      case "dataset": return value instanceof Set;
      case "changer": return typeof value === "object" && value !== null && "changer" in value;
      case "odd": return typeof value === "number" && Number.isInteger(value) && value % 2 !== 0;
      case "even": return typeof value === "number" && Number.isInteger(value) && value % 2 === 0;
      default: return false;
    }
  }

  // --- Iteration ---

  /** `(for: each _item, ...iterable)[hook]` — iterate over items, calling `cb(h, item)` for each.
   *  The context `h` is passed through so the callback can emit content. */
  for_each(iterable: any, cb: (h: HarloweContext, item: any) => void, h: HarloweContext): void {
    const arr: any[] = Array.isArray(iterable) ? iterable
      : iterable instanceof Set ? Array.from(iterable)
      : iterable instanceof Map ? Array.from(iterable.values())
      : [];
    for (const item of arr) {
      cb(h, item);
    }
  }

  // --- Value macros ---

  /** `(random: min, max)` */
  random(...args: any[]): number {
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
  either(...args: any[]): any {
    return args[Math.floor(Math.random() * args.length)];
  }

  /** `(str: value)` / `(string: value)` */
  str(...args: any[]): string {
    return args.map(String).join("");
  }

  /** `(num: value)` / `(number: value)` */
  num(value: any): number {
    return Number(value);
  }

  /** `(a: ...values)` / `(array: ...values)` */
  array(...args: any[]): any[] {
    return [...args];
  }

  /** `(dm: key, value, ...)` / `(datamap: key, value, ...)` */
  datamap(...args: any[]): Map<any, any> {
    const m = new Map();
    for (let i = 0; i + 1 < args.length; i += 2) {
      m.set(args[i], args[i + 1]);
    }
    return m;
  }

  /** `(ds: ...values)` / `(dataset: ...values)` */
  dataset(...args: any[]): Set<any> {
    return new Set(args);
  }

  // --- Property access ---

  /** `$var's property` or `(nth: n) of $arr` */
  get_property(obj: any, prop: any): any {
    return get_property(obj, prop);
  }

  /** `$var's property to value` */
  set_property(obj: any, prop: any, value: any): void {
    set_property(obj, prop, value);
  }

  /** Build a range descriptor for `1stto4th`, `2ndto2ndlast`, etc. */
  make_range(first: number, last: number): HarloweRange {
    return make_range(first, last);
  }

  // --- Math ---

  /** `(lerp: a, b, t)` */
  lerp(a: any, b: any, t: any): number {
    const na = Number(a), nb = Number(b), nt = Number(t);
    return na + (nb - na) * nt;
  }

  /** Math functions dispatched by name (fallback for unknown ops). */
  math(name: string, ...args: any[]): number {
    const nums = args.map(Number);
    switch (name) {
      case "round": return Math.round(nums[0]!);
      case "floor": return Math.floor(nums[0]!);
      case "ceil": return Math.ceil(nums[0]!);
      case "abs": return Math.abs(nums[0]!);
      case "min": return Math.min(...nums);
      case "max": return Math.max(...nums);
      case "sqrt": return Math.sqrt(nums[0]!);
      case "sin": return Math.sin(nums[0]!);
      case "cos": return Math.cos(nums[0]!);
      case "tan": return Math.tan(nums[0]!);
      case "log": return Math.log(nums[0]!);
      case "log10": return Math.log10(nums[0]!);
      case "log2": return Math.log2(nums[0]!);
      case "pow": return Math.pow(nums[0]!, nums[1]!);
      case "sign": return Math.sign(nums[0]!);
      case "trunc": return Math.trunc(nums[0]!);
      case "exp": return Math.exp(nums[0]!);
      case "clamp": return Math.min(Math.max(nums[0]!, nums[1]!), nums[2]!);
      case "lerp": return this.lerp(args[0], args[1], args[2]);
      default:
        console.warn(`[harlowe] unknown math function: ${name}`);
        return NaN;
    }
  }

  /** Collection operations (fallback for unknown ops). */
  collection_op(name: string, ...args: any[]): any {
    const kebabMap: Record<string, string> = {
      "some-pass": "somePass", "all-pass": "allPass", "none-pass": "nonePass",
      "sorted-by": "sortedBy", "rotated-to": "rotatedTo",
    };
    const key = kebabMap[name] ?? name;
    const fn = (Collections as any)[key];
    if (fn) return fn(...args);
    console.warn(`[harlowe] unknown collection op: ${name}`);
    return undefined;
  }

  /** String operations dispatched by name. Delegates to `StringOps` namespace.
   *  Compile-time call sites are rewritten by the backend to call StringOps directly. */
  str_op(name: string, ...args: any[]): any {
    // Alias map: Harlowe kebab-case names → StringOps camelCase method names
    const aliases: Record<string, keyof typeof StringOps> = {
      "string-reversed": "strReversed",
      "string-nth": "strNth",
      "string-repeated": "strRepeated",
      "string-find": "strFind",
      "string-replaced": "strReplaced",
      "replaced": "strReplaced",
      "digit-format": "digitFormat",
      "str-reversed": "strReversed",
      "str-nth": "strNth",
      "str-repeated": "strRepeated",
      "str-find": "strFind",
      "str-replaced": "strReplaced",
    };
    const key = (aliases[name] ?? name) as keyof typeof StringOps;
    const fn = StringOps[key] as ((...args: any[]) => any) | undefined;
    if (fn) return fn(...args);
    console.warn(`[harlowe] unknown str_op: ${name}`);
    return String(args[0] ?? "");
  }

  /** Generic value macro dispatcher — covers all Harlowe value macros. */
  value_macro(name: string, ...args: any[]): any {
    switch (name) {
      // Primitive constructors
      case "str": case "string": return this.str(...args);
      case "num": case "number": return this.num(args[0]);
      case "random": return this.random(...args);
      case "either": return this.either(...args);
      case "a": case "array": return this.array(...args);
      case "dm": case "datamap": return this.datamap(...args);
      case "ds": case "dataset": return this.dataset(...args);
      // Math — delegate to math() dispatcher
      case "round": case "floor": case "ceil": case "abs":
      case "min": case "max": case "sqrt": case "sin": case "cos":
      case "tan": case "log": case "log10": case "log2": case "pow":
      case "sign": case "clamp": case "lerp":
      case "trunc": case "exp":
        return this.math(name, ...args);
      // String ops — delegate to str_op()
      case "upperfirst": case "lowerfirst":
      case "str-reversed": case "string-reversed": case "trimmed": case "words":
      case "str-nth": case "string-nth": case "str-repeated": case "string-repeated":
      case "str-find": case "string-find":
      case "str-replaced": case "string-replaced": case "replaced":
      case "digit-format": case "plural":
        return this.str_op(name, ...args);
      // Collection ops — delegate to collection_op()
      case "sorted": case "sorted-by": case "reversed": case "rotated": case "rotated-to":
      case "shuffled": case "range": case "folded": case "interlaced":
      case "repeated": case "joined": case "subarray": case "substring":
      case "lowercase": case "uppercase": case "count": case "unique":
      case "some-pass": case "all-pass": case "none-pass": case "find":
      case "altered": case "datanames": case "datavalues": case "dataentries":
      case "permutations": case "pass":
        return this.collection_op(name, ...args);
      // Passage queries
      case "passage": return this.current_passage(args[0]);
      case "passages": {
        // `(passages:)` — returns array of all passage info, optionally filtered by lambda.
        const filter = args[0] && typeof args[0] === "function" ? args[0] : null;
        const result = Array.from(this.rt.Navigation.passages.keys()).map(name => ({
          name, source: this.passageSources.get(name) ?? "", tags: this.rt.Navigation.passageTags.get(name) ?? [],
        }));
        return filter ? result.filter((p: any) => filter(p)) : result;
      }
      // Saved games map
      case "saved-games": return this.saved_games();
      // Source — return the raw source of the current passage if available.
      case "source": return this.passageSources.get(this.rt.Navigation.current()) ?? "";
      // Metadata — runtime metadata not meaningful; return undefined.
      case "metadata": return undefined;
      // Meta queries
      case "visited": return this.rt.State.hasVisited(args[0]);
      case "visits": return args.length ? this.rt.State.visits(args[0]) : this.rt.State.current_visits();
      case "turns": return this.rt.State.turns();
      case "history": return this.rt.State.historyTitles();
      // Date and time
      case "current-date": {
        const d = new Date();
        return d.toLocaleDateString();
      }
      case "current-time": {
        const d = new Date();
        return d.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
      }
      case "weekday": {
        // Harlowe: 1=Sunday, 7=Saturday
        return new Date().getDay() + 1;
      }
      case "monthday": return new Date().getDate();
      case "monthname": return new Date().toLocaleString("default", { month: "long" });
      case "yearday": {
        const now = new Date();
        const start = new Date(now.getFullYear(), 0, 0);
        const diff = now.getTime() - start.getTime();
        return Math.floor(diff / 86400000);
      }
      // Datamap aliases
      case "dm-names": case "data-names": return Collections.datanames(args[0]);
      case "dm-values": case "data-values": return Collections.datavalues(args[0]);
      case "dm-entries": case "data-entries": return Collections.dataentries(args[0]);
      case "dm-altered": case "datamap-altered": return Collections.dmAltered(args[0], args[1]);
      // String split
      case "split": case "splitted": return Collections.splitStr(String(args[0]), String(args[1]));
      // Misc
      case "cond": {
        // `(cond: bool1, val1, bool2, val2, ..., defaultVal)` — pairs + optional fallback
        for (let i = 0; i + 1 < args.length; i += 2) {
          if (args[i]) return args[i + 1];
        }
        return args.length % 2 === 1 ? args[args.length - 1] : undefined;
      }
      case "nth": {
        // `(nth: n, val1, val2, ...)` — 1-indexed, wraps cyclically
        const n = Math.floor(args[0] as number);
        const vals = args.slice(1);
        if (vals.length === 0) return undefined;
        return vals[(n - 1) % vals.length];
      }
      default:
        console.warn(`[harlowe] unknown value macro: ${name}`);
        return undefined;
    }
  }

  // --- Save game queries ---

  /** `(saved-games:)` — returns a datamap of slot -> boolean. */
  saved_games(): Map<string, boolean> {
    const m = new Map<string, boolean>();
    for (let i = 0; i < 8; i++) {
      m.set(String(i), this.rt.State.hasSlot(String(i)));
    }
    return m;
  }

  /** `(passage:)` — returns the current (or named) passage info. */
  current_passage(name?: string): any {
    const title = name ?? this.rt.Navigation.current();
    const tags = this.rt.Navigation.passageTags.get(title) ?? [];
    return { name: title, source: this.passageSources.get(title) ?? "", tags };
  }

  // --- Meta queries ---

  /** `(visits:)`, `(turns:)`, `(time:)`, `(history:)` */
  meta(name: string): any {
    switch (name) {
      case "visits": return this.rt.State.current_visits();
      case "turns": return this.rt.State.turns();
      case "time": return Date.now() - this.rt.Navigation.passageStartTime;
      case "history": return this.rt.State.historyTitles();
      default:
        console.warn(`[harlowe] unknown meta query: ${name}`);
        return undefined;
    }
  }

  /** Color manipulation functions (fallback for unknown ops). */
  color_op(name: string, ...args: any[]): string {
    const fn = (Colors as any)[name];
    if (fn) return fn(...args);
    console.warn(`[harlowe] unknown color op: ${name}`);
    return String(args[0] ?? "");
  }

  // --- DOM macros ---

  /** `(replace:)`, `(append:)`, `(prepend:)`, `(show:)`, `(hide:)`, `(rerun:)` */
  dom_macro(method: string, ...args: any[]): void {
    const selector = args.length > 0 ? String(args[0]) : "";
    const callback = args.length > 1 && typeof args[args.length - 1] === "function"
      ? args[args.length - 1] as (h: HarloweContext) => void
      : undefined;

    const container = this.story();
    if (!container) return;
    const doc = this.doc();

    switch (method) {
      case "replace": {
        const targets = container.querySelectorAll(selector);
        targets.forEach(el => {
          el.innerHTML = "";
          if (callback) {
            const h = new HarloweContext(el, this.rt, doc);
            try { callback(h); } finally { h.closeAll(); }
          }
        });
        break;
      }
      case "append": {
        const targets = container.querySelectorAll(selector);
        targets.forEach(el => {
          if (callback) {
            const h = new HarloweContext(el, this.rt, doc);
            try { callback(h); } finally { h.closeAll(); }
          }
        });
        break;
      }
      case "prepend": {
        const targets = container.querySelectorAll(selector);
        targets.forEach(el => {
          if (callback) {
            const frag = doc.createDocumentFragment();
            const h = new HarloweContext(frag, this.rt, doc);
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
          const h = new HarloweContext(container as Element, this.rt, doc);
          try { callback(h); } finally { h.closeAll(); }
        }
        break;
      default:
        console.warn(`[harlowe] unknown DOM macro: ${method}`);
    }
  }

  // --- Click macros ---

  /** `(click:)`, `(click-replace:)`, `(click-append:)`, `(click-prepend:)` */
  click_macro(method: string, ...args: any[]): void {
    const selector = args.length > 0 ? String(args[0]) : "";
    const callback = args.length > 1 && typeof args[args.length - 1] === "function"
      ? args[args.length - 1] as (h: HarloweContext) => void
      : undefined;

    const container = this.story();
    if (!container) return;
    const doc = this.doc();

    const targets = container.querySelectorAll(selector);
    targets.forEach(el => {
      (el as HTMLElement).style.cursor = "pointer";
      el.addEventListener("click", () => {
        if (!callback) return;
        if (method === "click-replace") {
          el.innerHTML = "";
          const h = new HarloweContext(el, this.rt, doc);
          try { callback(h); } finally { h.closeAll(); }
        } else if (method === "click-append") {
          const h = new HarloweContext(el, this.rt, doc);
          try { callback(h); } finally { h.closeAll(); }
        } else if (method === "click-prepend") {
          const frag = doc.createDocumentFragment();
          const h = new HarloweContext(frag, this.rt, doc);
          try { callback(h); } finally { h.closeAll(); }
          el.insertBefore(frag, el.firstChild);
        } else if (method === "click-rerun") {
          el.innerHTML = "";
          const h = new HarloweContext(el, this.rt, doc);
          try { callback(h); } finally { h.closeAll(); }
        } else {
          // plain click — render inline
          const h = new HarloweContext(el, this.rt, doc);
          try { callback(h); } finally { h.closeAll(); }
        }
        // click-rerun keeps the listener active for repeated clicks
      }, method === "click-rerun" ? undefined : { once: true });
    });
  }

  // --- State management ---

  /** `(forget-undos: n)` — forget the n most recent undos. -1 forgets all. */
  forget_undos(n: any): void {
    this.rt.State.forgetUndos(Number(n));
  }

  /** `(forget-visits:)` — clear visit history. */
  forget_visits(): void {
    this.rt.State.forgetVisits();
  }

  // --- Meter ---

  /** `(meter: $var, max, label, color)` — progress meter element. */
  meter_macro(value: any, max: any, ...rest: any[]): void {
    const label: any = rest[rest.length - 2] ?? rest[0];
    const color: any = rest[rest.length - 1] ?? rest[0];
    const container = this.passage();
    if (!container) return;
    const doc = this.doc();

    const numValue = Number(value);
    const numMax = Number(max);
    const pct = numMax > 0 ? Math.min(100, Math.max(0, (numValue / numMax) * 100)) : 0;

    const meter = doc.createElement("tw-meter") as HTMLElement;
    meter.style.display = "block";
    meter.style.position = "relative";
    meter.style.height = "1.5em";
    meter.style.border = "1px solid #fff";
    meter.style.marginBottom = "0.5em";

    const bar = doc.createElement("div");
    bar.style.height = "100%";
    bar.style.width = `${pct}%`;
    bar.style.backgroundColor = color ? String(color) : "green";
    bar.style.transition = "width 0.3s";
    meter.appendChild(bar);

    if (label != null) {
      const labelEl = doc.createElement("span");
      labelEl.textContent = String(label);
      labelEl.style.position = "absolute";
      labelEl.style.left = "50%";
      labelEl.style.top = "50%";
      labelEl.style.transform = "translate(-50%, -50%)";
      meter.appendChild(labelEl);
    }

    container.appendChild(meter);
  }

  // --- Enchant ---

  /** `(enchant: selector, changer)` — wraps matching elements in tw-enchantment. */
  enchant(selector: any, changer: any): void {
    const storyEl = this.story();
    if (!storyEl) return;
    const css = resolveHookSelector(selector);
    enchantElements(storyEl as Element, css, changer as Changer | Changer[] | ((item: Element, pos: number) => Changer | Changer[]), this.doc());
  }

  /** `(enchant-in: selector, changer)` — like enchant but scoped to current passage. */
  enchant_in(selector: any, changer: any): void {
    const passageEl = this.passage();
    if (!passageEl) return;
    const css = resolveHookSelector(selector);
    enchantElements(passageEl as Element, css, changer as Changer | Changer[] | ((item: Element, pos: number) => Changer | Changer[]), this.doc());
  }

  // --- Dialog ---

  /** `(dialog: title, closeLabel)[hook]` — modal dialog. */
  dialog_macro(...args: any[]): void {
    const callback = args.length > 0 && typeof args[args.length - 1] === "function"
      ? args.pop() as (h: HarloweContext) => void
      : undefined;
    const title = args.length > 0 ? String(args[0]) : "";
    const closeLabel = args.length > 1 ? String(args[1]) : "Close";

    const storyEl = this.story();
    if (!storyEl || !callback) return;
    const doc = this.doc();

    const backdrop = doc.createElement("tw-backdrop") as HTMLElement;
    backdrop.style.position = "fixed";
    backdrop.style.inset = "0";
    backdrop.style.zIndex = "999996";
    backdrop.style.backgroundColor = "rgba(0, 0, 0, 0.8)";
    backdrop.style.display = "flex";
    backdrop.style.alignItems = "center";
    backdrop.style.justifyContent = "center";

    const dialog = doc.createElement("tw-dialog") as HTMLElement;
    dialog.style.zIndex = "999997";
    dialog.style.border = "#fff solid 2px";
    dialog.style.padding = "2em";
    dialog.style.backgroundColor = "#000";
    dialog.style.color = "#fff";
    dialog.style.maxWidth = "80vw";
    dialog.style.maxHeight = "80vh";
    dialog.style.overflow = "auto";
    backdrop.appendChild(dialog);

    if (title) {
      const heading = doc.createElement("h2");
      heading.textContent = title;
      dialog.appendChild(heading);
    }

    const h = new HarloweContext(dialog, this.rt, doc);
    try {
      callback(h);
    } finally {
      h.closeAll();
    }

    const links = doc.createElement("tw-dialog-links") as HTMLElement;
    links.style.display = "block";
    links.style.marginTop = "1em";
    links.style.textAlign = "right";
    const closeLink = doc.createElement("tw-link") as HTMLElement;
    closeLink.setAttribute("tabindex", "0");
    closeLink.textContent = closeLabel;
    closeLink.addEventListener("click", () => backdrop.remove());
    closeLink.addEventListener("keydown", (e) => {
      if (e.key === "Enter") backdrop.remove();
    });
    links.appendChild(closeLink);
    dialog.appendChild(links);

    storyEl.appendChild(backdrop);
  }

  // --- Columns ---

  /** `(columns: "1fr", "2fr", ...)[hook]` — flex row with column widths. */
  columns_macro(...args: any[]): void {
    const callback = args.length > 0 && typeof args[args.length - 1] === "function"
      ? args.pop() as (h: HarloweContext) => void
      : undefined;
    const widths = args.map(String);

    const container = this.passage();
    if (!container || !callback) return;
    const doc = this.doc();

    const columns = doc.createElement("tw-columns") as HTMLElement;
    columns.style.display = "flex";
    columns.style.flexDirection = "row";
    columns.style.justifyContent = "space-between";
    container.appendChild(columns);

    // Create the first column
    const firstCol = doc.createElement("tw-column") as HTMLElement;
    firstCol.style.flex = widths.length > 0 ? widths[0]! : "1";
    columns.appendChild(firstCol);

    const h = new HarloweContext(columns, this.rt, doc);
    // Push the first column as the current container
    (h as any).containerStack = [columns, firstCol];
    try {
      callback(h);
    } finally {
      h.closeAll();
    }

    // Apply widths to columns after callback has run (column breaks create new columns)
    const cols = columns.querySelectorAll(":scope > tw-column");
    cols.forEach((col, i) => {
      (col as HTMLElement).style.flex = i < widths.length ? widths[i]! : "1";
    });
  }

  // --- Navigation macros ---

  /** `(goto-url:)` — open a URL in a new tab. */
  goto_url(url: string): void {
    window.open(String(url), "_blank");
  }

  /** `(scroll:)` — scroll to a named hook or the top of the page. */
  scroll_macro(selector?: string, _durationOrHideTarget?: any): void {
    const container = this.story();
    if (!container) return;
    if (selector) {
      const el = container.querySelector(String(selector));
      if (el) el.scrollIntoView({ behavior: "smooth" });
    } else if (container instanceof Element) {
      container.scrollTop = 0;
    }
  }

  // --- Visual macros ---

  /** `(animate:)` — apply a CSS animation to a named hook. */
  animate_macro(selector: string, animation: string, duration?: number): void {
    const container = this.story();
    if (!container) return;
    const targets = container.querySelectorAll(String(selector));
    const anim = duration ? `${animation} ${duration}s` : String(animation);
    targets.forEach(el => {
      (el as HTMLElement).style.animation = anim;
    });
  }

  // --- Interactive link macros ---

  /** `(link-rerun: text)[hook]` — link that reruns the hook on each click. */
  link_rerun(text: string, cb: (h: HarloweContext) => void): void {
    const doc = this.doc();
    const link = doc.createElement("tw-link");
    link.textContent = String(text);
    const output = doc.createElement("span");
    const rerun = (): void => {
      output.innerHTML = "";
      const h = new HarloweContext(output, this.rt, doc);
      try { cb(h); } finally { h.closeAll(); }
    };
    link.addEventListener("click", (e: Event) => {
      e.preventDefault();
      rerun();
    });
    const wrapper = doc.createElement("span");
    wrapper.appendChild(link);
    wrapper.appendChild(output);
    this.passage()?.appendChild(wrapper);
  }

  /** `(link-reveal: text)[hook]` — link that vanishes and renders hook in its place on click. */
  link_reveal(text: string, cb: (h: HarloweContext) => void): void {
    const doc = this.doc();
    const wrapper = doc.createElement("span");
    const link = doc.createElement("tw-link");
    link.textContent = String(text);
    link.addEventListener("click", (e: Event) => {
      e.preventDefault();
      wrapper.removeChild(link);
      const h = new HarloweContext(wrapper, this.rt, doc);
      try { cb(h); } finally { h.closeAll(); }
    });
    wrapper.appendChild(link);
    this.passage()?.appendChild(wrapper);
  }

  /** `(link-reveal-goto: text, passage)[hook]` — link that reveals hook content then navigates. */
  link_reveal_goto(text: string, passage: string, cb?: (h: HarloweContext) => void): void {
    const doc = this.doc();
    const wrapper = doc.createElement("span");
    const link = doc.createElement("tw-link");
    link.textContent = String(text);
    link.addEventListener("click", (e: Event) => {
      e.preventDefault();
      wrapper.removeChild(link);
      if (cb) {
        const h = new HarloweContext(wrapper, this.rt, doc);
        try { cb(h); } finally { h.closeAll(); }
      }
      this.rt.Navigation.goto(String(passage));
    });
    wrapper.appendChild(link);
    this.passage()?.appendChild(wrapper);
  }

  /** `(link-undo: text)` — link that undoes the last turn. */
  link_undo(text: string): void {
    const doc = this.doc();
    const link = doc.createElement("tw-link");
    link.textContent = String(text);
    link.addEventListener("click", (e: Event) => {
      e.preventDefault();
      this.rt.Navigation.undo();
    });
    this.passage()?.appendChild(link);
  }

  /** `(link-fullscreen:)` — link that toggles browser fullscreen. */
  link_fullscreen(enterText: string, exitText?: string, _blockedText?: string): void {
    const doc = this.doc();
    const link = doc.createElement("tw-link");
    link.textContent = document.fullscreenElement ? (exitText ?? String(enterText)) : String(enterText);
    link.addEventListener("click", (e: Event) => {
      e.preventDefault();
      if (!document.fullscreenElement) {
        document.documentElement.requestFullscreen().catch(() => {});
        if (exitText) link.textContent = exitText;
      } else {
        document.exitFullscreen().catch(() => {});
        link.textContent = String(enterText);
      }
    });
    this.passage()?.appendChild(link);
  }

  // --- Timed macro ---

  /** `(after: Ns)[hook]` — render the hook after a delay. */
  after_macro(delay: number, cb: (h: HarloweContext) => void): void {
    const container = this.passage();
    if (!container) return;
    const doc = this.doc();
    const ms = Number(delay) * 1000;
    setTimeout(() => {
      const h = new HarloweContext(container, this.rt, doc);
      try { cb(h); } finally { h.closeAll(); }
    }, ms);
  }

  // --- Input macros ---

  /** `(checkbox:)`, `(dropdown:)`, `(input-box:)` — interactive form elements. */
  input_macro(name: string, ...args: any[]): void {
    const container = this.passage();
    if (!container) return;
    const doc = this.doc();
    const bindRef = args.length > 0 ? args[0] : undefined;
    const hasRef = bindRef != null && typeof bindRef === "object" && typeof bindRef.set === "function";
    const options = args.slice(1);
    if (name === "checkbox") {
      const falseVal = options[0] ?? "false";
      const trueVal = options[1] ?? "true";
      const input = doc.createElement("input") as HTMLInputElement;
      input.type = "checkbox";
      if (hasRef) {
        if (bindRef.twoWay) {
          input.checked = bindRef.get() === trueVal;
        }
        input.addEventListener("change", () => {
          bindRef.set(input.checked ? trueVal : falseVal);
        });
      }
      container.appendChild(input);
    } else if (name === "dropdown") {
      const select = doc.createElement("select") as HTMLSelectElement;
      for (const opt of options) {
        const option = doc.createElement("option") as HTMLOptionElement;
        option.textContent = String(opt);
        option.value = String(opt);
        select.appendChild(option);
      }
      if (hasRef) {
        if (bindRef.twoWay) {
          select.value = String(bindRef.get());
        }
        select.addEventListener("change", () => bindRef.set(select.value));
      }
      container.appendChild(select);
    } else if (name === "input-box") {
      const lines = options[0] ?? "oneline";
      const defaultVal = String(options[1] ?? "");
      const input = (String(lines) === "oneline"
        ? doc.createElement("input")
        : doc.createElement("textarea")) as HTMLInputElement | HTMLTextAreaElement;
      input.value = hasRef && bindRef.twoWay ? String(bindRef.get()) : defaultVal;
      if (hasRef) {
        input.addEventListener("input", () => bindRef.set(input.value));
      }
      container.appendChild(input);
    }
  }

  /**
   * `(cycling-link:)` / `(seq-link:)` — interactive link cycling through options.
   * Args: `cycling: boolean, [bindRef?,] opt1, opt2, ...`
   * If the first content arg is a bind-ref, clicking also sets the variable.
   */
  cycling_link(cycling: boolean, ...args: any[]): void {
    const container = this.passage();
    if (!container) return;
    const doc = this.doc();

    // Detect optional bind-ref as first arg (has .get and .set methods).
    let bindRef: { get: () => any; set: (v: any) => void } | undefined;
    let opts: string[];
    if (args.length > 0 && args[0] != null && typeof args[0] === "object" && typeof args[0].set === "function") {
      bindRef = args[0];
      opts = args.slice(1).map(String);
    } else {
      opts = args.map(String);
    }
    if (opts.length === 0) return;

    // Start from current variable value if bind ref is provided, otherwise first option.
    let idx = 0;
    if (bindRef) {
      const cur = String(bindRef.get());
      const found = opts.indexOf(cur);
      if (found >= 0) idx = found;
    }

    const link = doc.createElement("a") as HTMLAnchorElement;
    link.className = "cycling-link";
    link.href = "javascript:void(0)";
    link.textContent = opts[idx]!;

    link.addEventListener("click", (e) => {
      e.preventDefault();
      if (cycling) {
        idx = (idx + 1) % opts.length;
      } else {
        idx = Math.min(idx + 1, opts.length - 1);
      }
      link.textContent = opts[idx]!;
      if (bindRef) bindRef.set(opts[idx]!);
    });

    container.appendChild(link);
  }

  // --- Unknown macro fallback ---

  /** Handle unknown macros gracefully. */
  unknown_macro(name: string, ...args: any[]): any {
    console.warn(`[harlowe] unknown macro: (${name}:)`, args);
    return undefined;
  }

  // --- Generic call fallback ---

  /** Handle unknown function calls in expressions. */
  call(name: string, ...args: any[]): any {
    console.warn(`[harlowe] unknown function call: ${name}`, args);
    return undefined;
  }
}

// --- Pure utility functions (no state) ---

/** Check if a value is a Changer object. */
function isChanger(v: any): v is Changer {
  return v != null && typeof v === "object" && "name" in v && "args" in v;
}

/** Compose two changers (or changer arrays) into a changer array. */
function composeChangers(a: Changer | Changer[], b: Changer): Changer[] {
  return Array.isArray(a) ? [...a, b] : [a, b];
}

/** `contains` operator: checks if a collection contains a value. */
function contains(collection: any, value: any): boolean {
  if (typeof collection === "string") return collection.includes(String(value));
  if (Array.isArray(collection)) return collection.includes(value);
  if (collection instanceof Set) return collection.has(value);
  if (collection instanceof Map) return collection.has(value);
  if (typeof collection === "object" && collection !== null) return value in collection;
  return false;
}

type HarloweRange = { first: number; last: number };

/** Resolve a numeric ordinal (positive = 1-based, negative = from-end) to a 0-based index. */
function resolve_index(n: number, len: number): number {
  return n >= 0 ? n - 1 : len + n;
}

/** Create a range descriptor for slice access. */
function make_range(first: number, last: number): HarloweRange {
  return { first, last };
}

/** Apply a range to a sequential collection. */
function apply_range(obj: any[], range: HarloweRange): any[] {
  const len = obj.length;
  const from = resolve_index(range.first, len);
  const to = resolve_index(range.last, len);
  return obj.slice(from, to + 1);
}

/** `$var's property` or `(nth: n) of $arr` */
function get_property(obj: any, prop: any): any {
  if (obj instanceof Map) return obj.get(prop);
  const arr_like = obj instanceof Set ? Array.from(obj)
    : Array.isArray(obj) ? obj
    : typeof obj === "string" ? obj
    : null;
  if (arr_like !== null) {
    const len = arr_like.length;
    if (typeof prop === "number") {
      // positive = 1-based forward, negative = from end (-1 = last, -2 = 2ndlast)
      return arr_like[resolve_index(prop, len)];
    }
    if (prop === "length") return len;
    if (typeof prop === "object" && prop !== null && "first" in prop) {
      // Range slice: return sub-array/string
      if (typeof arr_like === "string") {
        const from = resolve_index(prop.first, len);
        const to = resolve_index(prop.last, len);
        return arr_like.slice(from, to + 1);
      }
      return apply_range(arr_like as any[], prop);
    }
    if (Array.isArray(arr_like)) return arr_like[prop];
    return undefined;
  }
  if (typeof obj === "object" && obj !== null) return obj[prop];
  return undefined;
}

/** `$var's property to value` */
function set_property(obj: any, prop: any, value: any): void {
  if (obj instanceof Map) {
    obj.set(prop, value);
  } else if (Array.isArray(obj)) {
    if (typeof prop === "number") {
      obj[resolve_index(prop, obj.length)] = value;
    } else {
      obj[prop] = value;
    }
  } else if (typeof obj === "object" && obj !== null) {
    obj[prop] = value;
  }
}

// --- Collection operations (pure) ---

// Harlowe: (sorted: value, ...) — individual values to sort (NOT arrays).
// Passing a single array is an error in Harlowe; use spread ...$arr instead.
// When a `via` lambda is present, the rewrite pass routes to `Collections.sortedBy`.
function sorted<T>(...items: T[]): T[] {
  return [...items].sort((a, b) => {
    if (typeof a === "string") return (a as string).localeCompare(b as string);
    return Number(a) - Number(b);
  });
}
function reversed<T>(...items: T[]): T[] {
  return [...items].reverse();
}
function rotated<T>(n: number, ...items: T[]): T[] {
  const len = items.length;
  if (len === 0) return [];
  const shift = ((n % len) + len) % len;
  return [...items.slice(shift), ...items.slice(0, shift)];
}
function shuffled<T>(...items: T[]): T[] {
  const arr = [...items];
  for (let i = arr.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [arr[i], arr[j]] = [arr[j]!, arr[i]!];
  }
  return arr;
}
function count(arr: unknown, value: unknown): number {
  const a = Array.isArray(arr) ? arr : [arr];
  return a.filter(x => x === value).length;
}
function range(start: number, end: number): number[] {
  const result: number[] = [];
  for (let i = start; i <= end; i++) result.push(i);
  return result;
}
// Harlowe: (find: where condition, ...items) — returns ALL matching items as an array.
// Items are individual values (not arrays); Harlowe errors if you pass a single array.
function find<T>(fn: (item: T) => boolean, ...items: T[]): T[] {
  if (typeof fn !== "function") return [];
  return items.filter(fn);
}
// Harlowe: (joined: sep, item1, item2, ...) — separator is first, items follow.
// Items are individual values; use spread ...$arr to join an existing array.
function joined(sep: unknown, ...items: unknown[]): string {
  return items.join(String(sep ?? ""));
}
function subarray(arr: unknown[], start: number, end?: number): unknown[] {
  return Array.isArray(arr) ? arr.slice(start - 1, end) : [];
}
function substring(str: string, start: number, end?: number): string {
  return typeof str === "string" ? str.slice(start - 1, end) : "";
}
function lowercase(s: unknown): string { return String(s).toLowerCase(); }
function uppercase(s: unknown): string { return String(s).toUpperCase(); }
function datanames(m: unknown): (string | number)[] {
  return m instanceof Map ? Array.from(m.keys()) : Object.keys((m as object) || {});
}
function datavalues(m: unknown): unknown[] {
  return m instanceof Map ? Array.from(m.values()) : Object.values((m as object) || {});
}
function dataentries(m: unknown): { name: unknown; value: unknown }[] {
  return m instanceof Map ? Array.from(m.entries()).map(([k, v]) => ({ name: k, value: v })) : [];
}
function somePass<T>(fn: (item: T) => boolean, ...items: T[]): boolean {
  if (typeof fn !== "function") return false;
  return items.some(fn);
}
function allPass<T>(fn: (item: T) => boolean, ...items: T[]): boolean {
  if (typeof fn !== "function") return false;
  return items.every(fn);
}
function nonePass<T>(fn: (item: T) => boolean, ...items: T[]): boolean {
  if (typeof fn !== "function") return false;
  return !items.some(fn);
}
function altered<T, U>(fn: (item: T, index: number) => U, ...items: T[]): U[] {
  if (typeof fn !== "function") return items as unknown as U[];
  return items.map(fn);
}
function sortedBy<T>(fn: (item: T) => unknown, ...items: T[]): T[] {
  const arr = [...items];
  if (typeof fn !== "function") return arr;
  return arr.sort((a, b) => {
    const ka = fn(a), kb = fn(b);
    if (typeof ka === "string") return ka.localeCompare(String(kb));
    return Number(ka) - Number(kb);
  });
}
// Harlowe: (interlaced: array1, array2, ...) — each argument IS an array.
// Returns elements interleaved, stopping at the shortest array.
function interlaced<T>(...arrays: T[][]): T[] {
  if (arrays.length === 0) return [];
  // Harlowe stops at the shortest array (Math.min, not Math.max).
  const minLen = Math.min(...arrays.map((a) => a.length));
  const result: T[] = [];
  for (let i = 0; i < minLen; i++) {
    for (const arr of arrays) {
      result.push(arr[i]!);
    }
  }
  return result;
}
function repeated(n: number, ...values: unknown[]): unknown[] {
  const result: unknown[] = [];
  for (let i = 0; i < n; i++) result.push(...values);
  return result;
}
function folded<T, A>(fn: (item: T, acc: A) => A, initial: A, ...items: T[]): A {
  if (typeof fn !== "function") return initial;
  return items.reduce((acc: A, item: T) => fn(item, acc), initial);
}

function pass<T>(value: T): T { return value; }

function unique<T>(...items: T[]): T[] {
  return [...new Set(items)];
}

function rotatedTo<T>(target: T, ...items: T[]): T[] {
  const idx = items.indexOf(target);
  if (idx < 0) return [...items];
  return [...items.slice(idx), ...items.slice(0, idx)];
}

function splitStr(sep: string, str: string): string[] {
  return sep === "" ? [...str] : str.split(sep);
}

function dmAltered(fn: any, map: any): Map<any, any> {
  if (!(map instanceof Map) || typeof fn !== "function") return new Map();
  const result = new Map();
  for (const [k, v] of map) {
    result.set(k, fn(v));
  }
  return result;
}

function permutations<T>(...items: T[]): T[][] {
  if (items.length === 0) return [[]];
  const result: T[][] = [];
  function perm(arr: T[], current: T[]): void {
    if (arr.length === 0) { result.push(current); return; }
    for (let i = 0; i < arr.length; i++) {
      perm([...arr.slice(0, i), ...arr.slice(i + 1)], [...current, arr[i]!]);
    }
  }
  perm([...items], []);
  return result;
}

export const Collections = {
  sorted, reversed, rotated, rotatedTo, shuffled, count, range,
  find, joined, subarray, substring, lowercase, uppercase,
  datanames, datavalues, dataentries, dmAltered,
  somePass, allPass, nonePass, altered,
  sortedBy, interlaced, repeated, folded,
  pass, permutations, unique, splitStr,
} as const;

// --- Color operations (pure) ---

function rgb(r: number, g: number, b: number): string { return `rgb(${r}, ${g}, ${b})`; }
function rgba(r: number, g: number, b: number, a: number): string { return `rgba(${r}, ${g}, ${b}, ${a})`; }
function hsl(h: number, s: number, l: number): string { return `hsl(${h}, ${s}%, ${l}%)`; }
function hsla(h: number, s: number, l: number, a: number): string { return `hsla(${h}, ${s}%, ${l}%, ${a})`; }

export const Colors = { rgb, rgba, hsl, hsla } as const;

// --- String operations (pure) ---

function upperfirst(s: any): string {
  const str = String(s);
  return str ? str.charAt(0).toUpperCase() + str.slice(1) : str;
}
function lowerfirst(s: any): string {
  const str = String(s);
  return str ? str.charAt(0).toLowerCase() + str.slice(1) : str;
}
function strReversed(s: any): string { return [...String(s)].reverse().join(""); }
function trimmed(s: unknown): string;
function trimmed(pattern: unknown, s: unknown): string;
function trimmed(sOrPattern: unknown, s?: unknown): string {
  // (trimmed: str) or (trimmed: datatype_pattern, str) — pattern trimming not yet implemented
  return String(s !== undefined ? s : sOrPattern).trim();
}
function words(s: any): string[] {
  const str = String(s).trim();
  return str.length === 0 ? [] : str.split(/\s+/);
}
function strNth(n: any, s: any): string {
  const str = String(s);
  const i = Math.floor(Number(n)) - 1; // Harlowe is 1-indexed
  return i >= 0 && i < str.length ? str[i]! : "";
}
function strRepeated(n: any, s: any): string {
  return String(s).repeat(Math.max(0, Math.floor(Number(n))));
}
function strFind(pattern: any, s: any): number[] {
  const str = String(s);
  if (typeof pattern !== "string") return [];
  const positions: number[] = [];
  let idx = 0;
  while (idx < str.length) {
    const found = str.indexOf(pattern, idx);
    if (found === -1) break;
    positions.push(found + 1); // Harlowe is 1-indexed
    idx = found + 1;
  }
  return positions;
}
function strReplaced(searchFor: string, replacement: string, str: string): string;
function strReplaced(count: number, searchFor: string, replacement: string, str: string): string;
function strReplaced(...args: unknown[]): string {
  // (str-replaced: searchFor, replacement, str)
  // (str-replaced: count, searchFor, replacement, str) — count limits replacements
  let count: number | undefined;
  let searchFor: unknown;
  let replacement: unknown;
  let str: unknown;
  if (args.length === 4) {
    [count, searchFor, replacement, str] = args as [number, string, string, string];
  } else {
    [searchFor, replacement, str] = args as [string, string, string];
  }
  const s = String(str);
  if (typeof searchFor !== "string" || typeof replacement !== "string") return s;
  if (count !== undefined) {
    let result = s;
    let replaced = 0;
    let idx = 0;
    while (replaced < count) {
      const found = result.indexOf(searchFor, idx);
      if (found === -1) break;
      result = result.slice(0, found) + replacement + result.slice(found + searchFor.length);
      idx = found + replacement.length;
      replaced++;
    }
    return result;
  }
  return s.split(searchFor).join(replacement);
}
function digitFormat(fmt: any, num: any): string {
  const n = Number(num);
  const format = String(fmt);
  const dotIdx = format.indexOf(".");
  const decimals = dotIdx === -1 ? 0 : format.length - dotIdx - 1;
  return n.toLocaleString(undefined, {
    minimumFractionDigits: decimals,
    maximumFractionDigits: decimals,
  });
}
function plural(n: any, singular: any, pluralForm?: any): string {
  return Number(n) === 1 ? String(singular)
    : pluralForm !== undefined ? String(pluralForm) : String(singular) + "s";
}

export const StringOps = {
  upperfirst, lowerfirst, strReversed, trimmed, words,
  strNth, strRepeated, strFind, strReplaced, digitFormat, plural,
} as const;

// --- Enchantment helpers (pure) ---

/** Resolve a Harlowe hook selector to a CSS selector string. */
function resolveHookSelector(selector: any): string {
  const s = String(selector);
  if (s.startsWith("?")) return `tw-hook[name="${s.slice(1)}"]`;
  return s;
}

/** Apply a changer to matching elements by wrapping them in tw-enchantment. */
function enchantElements(scope: Element, selector: string, changer: Changer | Changer[] | ((item: Element, pos: number) => Changer | Changer[]), doc: DocumentFactory): void {
  const targets = scope.querySelectorAll(selector);
  targets.forEach((el, i) => {
    if (el.parentElement?.tagName.toLowerCase() === "tw-enchantment") return;
    const wrapper = doc.createElement("tw-enchantment") as HTMLElement;
    const resolved = typeof changer === "function" ? changer(el as Element, i + 1) : changer;
    if (Array.isArray(resolved)) {
      for (const c of resolved) applyChangerToElement(wrapper, c);
    } else {
      applyChangerToElement(wrapper, resolved);
    }
    el.parentNode?.insertBefore(wrapper, el);
    wrapper.appendChild(el);
  });
}

function applyChangerToElement(el: HTMLElement, changer: Changer): void {
  switch (changer.name) {
    case "color": case "colour": case "text-colour": case "text-color":
      el.style.color = String(changer.args[0]);
      break;
    case "background":
      el.style.backgroundColor = String(changer.args[0]);
      break;
    case "text-style": {
      const style = String(changer.args[0]);
      if (style === "bold") el.style.fontWeight = "bold";
      else if (style === "italic") el.style.fontStyle = "italic";
      else if (style === "underline") el.style.textDecoration = "underline";
      break;
    }
    case "font":
      el.style.fontFamily = String(changer.args[0]);
      break;
    case "css":
      el.setAttribute("style", (el.getAttribute("style") || "") + ";" + String(changer.args[0]));
      break;
    default:
      break;
  }
}

