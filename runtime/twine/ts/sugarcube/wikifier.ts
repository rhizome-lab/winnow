/** SugarCube 2 runtime Wikifier.
 *
 * A pluggable regex-handler registry that parses SugarCube markup into DOM.
 * Games can add/remove parsers at runtime via Wikifier.Parser.add/delete.
 *
 * The core loop finds the earliest regex match among all registered parsers
 * and delegates to the matching handler. Handlers receive a Wikifier instance
 * with position tracking (matchStart, matchLength, nextMatch) and output
 * helpers (outputText, subWikify).
 *
 * Two-phase model:
 * 1. Parse + render in a single pass (SugarCube's original design)
 *    — handlers directly produce DOM nodes in the output
 * 2. Cache by markup string with parser dependency tracking
 *    — on Parser.add/delete("X"), evict entries that used parser "X"
 */

import type { SugarCubeRuntime } from "./runtime";

// ---------------------------------------------------------------------------
// Span helpers
// ---------------------------------------------------------------------------

/** Convert a source offset to line:col (1-based). */
export function offsetToLineCol(source: string, offset: number): { line: number; col: number } {
  let line = 1;
  let col = 1;
  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      col = 1;
    } else {
      col++;
    }
  }
  return { line, col };
}

// ---------------------------------------------------------------------------
// Parser definition
// ---------------------------------------------------------------------------

export interface ParserDef {
  name: string;
  match: string;
  handler: (w: Wikifier) => void;
  profiles?: string[];
}

// ---------------------------------------------------------------------------
// Wikifier options
// ---------------------------------------------------------------------------

export interface WikifierOptions {
  /** Parser profile: 'all' uses every parser, 'core' skips block-level parsers. */
  profile?: string;
  /** Post-process <br> sequences into <p> elements. Default: false. */
  cleanup?: boolean;
  /** Suppress <br> for newlines. */
  nobr?: boolean;
  /** Case-insensitive terminator matching (used internally by HTML parser). */
  ignoreTerminatorCase?: boolean;
}

const DEFAULT_OPTIONS: WikifierOptions = { profile: "all" };

// ---------------------------------------------------------------------------
// Parser registry
// ---------------------------------------------------------------------------

export class ParserRegistry {
  private parsers: ParserDef[] = [];
  private profileCache: Map<string, { re: RegExp; parsers: ParserDef[] }> = new Map();
  modified = false;

  add(def: ParserDef): void {
    // Replace existing parser with the same name
    const idx = this.parsers.findIndex(p => p.name === def.name);
    if (idx >= 0) {
      this.parsers[idx] = def;
    } else {
      this.parsers.push(def);
    }
    this.profileCache.clear();
    this.modified = true;
    Wikifier.invalidateCache(def.name);
  }

  delete(name: string): void {
    const idx = this.parsers.findIndex(p => p.name === name);
    if (idx >= 0) {
      this.parsers.splice(idx, 1);
      this.profileCache.clear();
      this.modified = true;
      Wikifier.invalidateCache(name);
    }
  }

  get(name: string): ParserDef | undefined {
    return this.parsers.find(p => p.name === name);
  }

  has(name: string): boolean {
    return this.parsers.some(p => p.name === name);
  }

  /** Get all registered parsers (read-only snapshot). */
  getParsers(): readonly ParserDef[] {
    return this.parsers;
  }

  /** Get parsers filtered by profile, with compiled regex. */
  getProfile(profile: string): { re: RegExp; parsers: ParserDef[] } {
    const cached = this.profileCache.get(profile);
    if (cached) return cached;

    let filtered: ParserDef[];
    if (profile === "all") {
      filtered = this.parsers;
    } else {
      filtered = this.parsers.filter(
        p => !p.profiles || p.profiles.includes(profile),
      );
    }

    const combined = filtered.map(p => `(${p.match})`).join("|");
    const re = new RegExp(combined, "gm");
    const entry = { re, parsers: filtered };
    this.profileCache.set(profile, entry);
    return entry;
  }

  /** Compile a single combined regex from all parser match patterns.
   *  Shorthand for getProfile("all").re.
   */
  getCompiledRe(): RegExp {
    return this.getProfile("all").re;
  }

  /** Number of registered parsers. */
  get length(): number {
    return this.parsers.length;
  }
}

// ---------------------------------------------------------------------------
// Cache entry
// ---------------------------------------------------------------------------

interface CacheEntry {
  fragment: DocumentFragment;
  parserDeps: Set<string>;
}

// ---------------------------------------------------------------------------
// Wikifier class
// ---------------------------------------------------------------------------

export class Wikifier {
  // --- Static API ---

  /** Set by SugarCubeRuntime constructor — provides access to all sub-objects. */
  static rt: SugarCubeRuntime;

  static Parser: ParserRegistry = new ParserRegistry();

  private static cache: Map<string, CacheEntry> = new Map();

  /** Invalidate cache entries that depend on a specific parser. */
  static invalidateCache(parserName: string): void {
    for (const [key, entry] of Wikifier.cache) {
      if (entry.parserDeps.has(parserName)) {
        Wikifier.cache.delete(key);
      }
    }
  }

  /** Parse and render SugarCube markup, returning a DocumentFragment. */
  static wikifyEval(markup: string, options?: WikifierOptions): DocumentFragment {
    // Build cache key that incorporates options
    const cacheKey = options ? markup + "\0" + JSON.stringify(options) : markup;

    const cached = Wikifier.cache.get(cacheKey);
    if (cached) {
      return cached.fragment.cloneNode(true) as DocumentFragment;
    }

    const frag = Wikifier.rt.Output.doc.createDocumentFragment();
    const w = new Wikifier(frag, markup, options);
    Wikifier.cache.set(cacheKey, {
      fragment: frag.cloneNode(true) as DocumentFragment,
      parserDeps: w.parserDeps,
    });
    return frag;
  }

  /** Get a story variable value. */
  static getValue(name: string): any {
    return Wikifier.rt.State.get(name);
  }

  /** Set a story variable value. */
  static setValue(name: string, value: any): void {
    Wikifier.rt.State.set(name, value);
  }

  /** Check if a URL is external. */
  static isExternalLink(url: string): boolean {
    return /^(?:https?:\/\/|mailto:|tel:)/.test(url);
  }

  /** Create an external link element. */
  static createExternalLink(node: Node, url: string, text?: string): HTMLAnchorElement {
    const a = Wikifier.rt.Output.doc.createElement("a");
    a.href = url;
    a.target = "_blank";
    a.rel = "noopener noreferrer";
    if (text) {
      a.textContent = text;
    }
    node.appendChild(a);
    return a;
  }

  /** Create an internal passage link element. */
  static createInternalLink(node: Node, passage: string, text?: string, setter?: () => void): HTMLAnchorElement {
    const a = Wikifier.rt.Output.doc.createElement("a");
    a.classList.add("link-internal");
    if (text) {
      a.textContent = text;
    }
    if (Wikifier.rt.Navigation.has(passage)) {
      if (Wikifier.rt.State.hasPlayed(passage)) {
        a.classList.add("link-visited");
      }
      a.addEventListener("click", (e) => {
        e.preventDefault();
        if (setter) setter();
        Wikifier.rt.Navigation.goto(passage);
      });
      // Keyboard accessibility
      a.addEventListener("keypress", (e: KeyboardEvent) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (setter) setter();
          Wikifier.rt.Navigation.goto(passage);
        }
      });
      a.tabIndex = 0;
      a.setAttribute("role", "link");
    } else {
      a.classList.add("link-broken");
      a.title = `passage "${passage}" does not exist`;
    }
    node.appendChild(a);
    return a;
  }

  // --- Helpers namespace (for DoL link parser compat) ---

  static helpers = {
    /** Parse a square-bracketed markup at the wikifier's current position.
     *  Returns parsed link data or error.
     */
    parseSquareBracketedMarkup(w: Wikifier): {
      error?: string;
      pos: number;
      isLink: boolean;
      isImage: boolean;
      link?: string;
      text?: string;
      setter?: string;
      forceInternal?: boolean;
      source?: string;
      align?: string;
      title?: string;
    } {
      const source = w.source;
      let pos = w.matchStart;
      const result: ReturnType<typeof Wikifier.helpers.parseSquareBracketedMarkup> = {
        pos,
        isLink: false,
        isImage: false,
      };

      // Determine if this is an image or link
      if (source.startsWith("[img[", pos)) {
        result.isImage = true;
        pos += 5; // skip [img[
      } else if (source.startsWith("[[", pos)) {
        result.isLink = true;
        pos += 2; // skip [[
      } else {
        result.error = "expected [[ or [img[";
        return result;
      }

      // Find the closing brackets
      let depth = 0;
      let content = "";
      const start = pos;

      for (; pos < source.length; pos++) {
        const ch = source[pos];
        if (ch === "[") {
          depth++;
        } else if (ch === "]") {
          if (depth > 0) {
            depth--;
          } else {
            break;
          }
        }
      }

      content = source.slice(start, pos);

      if (result.isImage) {
        // [img[src]] or [img[src][link]]
        // pos is at first ], check for ][link]
        pos++; // skip ]
        if (pos < source.length && source[pos] === "[") {
          // Has link target
          pos++; // skip [
          const linkStart = pos;
          while (pos < source.length && source[pos] !== "]") pos++;
          result.link = source.slice(linkStart, pos);
          pos++; // skip ]
        }
        pos++; // skip final ]
        result.source = content;
        result.pos = pos;
        return result;
      }

      // Link: parse content for separators
      // [[text|passage]], [[text->passage]], [[passage<-text]], [[passage]]
      let text: string | undefined;
      let link: string;

      const pipeIdx = content.indexOf("|");
      const arrowRightIdx = content.indexOf("->");
      const arrowLeftIdx = content.indexOf("<-");

      if (pipeIdx >= 0) {
        text = content.slice(0, pipeIdx);
        link = content.slice(pipeIdx + 1);
      } else if (arrowRightIdx >= 0) {
        text = content.slice(0, arrowRightIdx);
        link = content.slice(arrowRightIdx + 2);
      } else if (arrowLeftIdx >= 0) {
        link = content.slice(0, arrowLeftIdx);
        text = content.slice(arrowLeftIdx + 2);
      } else {
        link = content;
        text = content;
      }

      result.text = text;
      result.link = link;

      pos++; // skip first ]

      // Check for setter [$...]
      if (pos < source.length && source[pos] === "[") {
        if (source[pos + 1] === "$" || source[pos + 1] === "_") {
          pos++; // skip [
          const setterStart = pos;
          let setterDepth = 0;
          for (; pos < source.length; pos++) {
            if (source[pos] === "[") setterDepth++;
            else if (source[pos] === "]") {
              if (setterDepth > 0) setterDepth--;
              else break;
            }
          }
          result.setter = source.slice(setterStart, pos);
          pos++; // skip ]
        }
      }

      pos++; // skip final ]
      result.pos = pos;
      return result;
    },

    /** Evaluate a passage ID expression (may contain $var). */
    evalPassageId(expr: string): string {
      if (expr.includes("$") || expr.includes("_")) {
        try {
          return Wikifier.evalExpression(expr);
        } catch {
          return expr;
        }
      }
      return expr;
    },

    /** Evaluate display text (may contain $var). */
    evalText(text: string): string {
      return Wikifier.interpolateVars(text);
    },

    /** Create a shadow setter callback from setter code. */
    createShadowSetterCallback(code: string): () => void {
      return () => {
        try {
          Wikifier.evalExpression(code);
        } catch (e) {
          console.error("[wikifier] setter error:", e);
        }
      };
    },
  };

  // --- Expression evaluation helpers ---

  /** Evaluate a JavaScript expression in the SugarCube context. */
  static evalExpression(expr: string): any {
    const g = globalThis as any;
    return new Function("State", "setup", "Config", "settings", `return (${expr})`)(
      g.State, g.setup, g.Config, g.settings,
    );
  }

  /** Interpolate $var and _var references in a string. */
  static interpolateVars(text: string): string {
    return text.replace(/(?:\$|_)[\w.[\]]+/g, (match) => {
      try {
        return String(Wikifier.evalExpression(match));
      } catch {
        return match;
      }
    });
  }

  // --- Instance ---

  output: DocumentFragment | Node;
  source: string;
  options: WikifierOptions;
  matchStart = 0;
  matchLength = 0;
  matchText = "";
  nextMatch = 0;
  parserDeps: Set<string> = new Set();

  private _match: RegExpExecArray | null = null;

  constructor(output: DocumentFragment | Node | null, source: string, options?: WikifierOptions) {
    this.output = output || Wikifier.rt.Output.doc.createDocumentFragment();
    this.source = source;
    this.options = Object.assign({}, DEFAULT_OPTIONS, options);
    this.subWikify(this.output);
  }

  /** Output plain text between two source positions. */
  outputText(destination: Node, startPos: number, endPos: number): void {
    if (endPos > startPos) {
      destination.appendChild(Wikifier.rt.Output.doc.createTextNode(this.source.slice(startPos, endPos)));
    }
  }

  /** Parse and render from current position, optionally stopping at a terminator regex.
   *  @param output — destination node
   *  @param termRegex — stop when this regex matches (consumed but not rendered)
   *  @param localOptions — options merged on top of this.options for this call
   */
  subWikify(output: Node, termRegex?: RegExp | null, localOptions?: WikifierOptions): void {
    const oldOutput = this.output;
    const oldOptions = this.options;
    this.output = output;

    if (localOptions) {
      this.options = Object.assign({}, this.options, localOptions);
    }

    const profile = this.options.profile || "all";
    const { re, parsers } = Wikifier.Parser.getProfile(profile);
    re.lastIndex = this.nextMatch;

    // Handle ignoreTerminatorCase
    let effectiveTermRe = termRegex ?? null;
    if (effectiveTermRe && this.options.ignoreTerminatorCase) {
      effectiveTermRe = new RegExp(effectiveTermRe.source, "gim");
    }

    let terminatorMatch: RegExpExecArray | null = null;

    while (true) {
      // Check terminator first
      if (effectiveTermRe) {
        effectiveTermRe.lastIndex = this.nextMatch;
        terminatorMatch = effectiveTermRe.exec(this.source);
      }

      // Find earliest parser match
      re.lastIndex = this.nextMatch;
      const match = re.exec(this.source);

      // If terminator comes first (or no parser match), stop
      if (terminatorMatch && (!match || terminatorMatch.index <= match.index)) {
        // Output text before terminator
        if (terminatorMatch.index > this.nextMatch) {
          this.outputText(this.output, this.nextMatch, terminatorMatch.index);
        }
        this.nextMatch = terminatorMatch.index + terminatorMatch[0].length;
        break;
      }

      if (!match) {
        // No more matches — output remaining text
        if (this.nextMatch < this.source.length) {
          this.outputText(this.output, this.nextMatch, this.source.length);
          this.nextMatch = this.source.length;
        }
        break;
      }

      // Output text before match
      if (match.index > this.nextMatch) {
        this.outputText(this.output, this.nextMatch, match.index);
      }

      // Find which parser group matched
      let parserDef: ParserDef | undefined;
      for (let i = 0; i < parsers.length; i++) {
        if (match[i + 1] !== undefined) {
          parserDef = parsers[i];
          break;
        }
      }

      if (parserDef) {
        this.matchStart = match.index;
        this.matchLength = match[0].length;
        this.matchText = match[0];
        this.nextMatch = match.index + match[0].length;
        this._match = match;
        this.parserDeps.add(parserDef.name);

        try {
          parserDef.handler(this);
        } catch (e) {
          const pos = offsetToLineCol(this.source, match.index);
          console.error(
            `[wikifier] error in parser "${parserDef.name}" at ${pos.line}:${pos.col}:`,
            e,
          );
          this.nextMatch = match.index + match[0].length;
        }
      } else {
        this.nextMatch = match.index + match[0].length;
      }
    }

    this.output = oldOutput;
    this.options = oldOptions;
  }

  /** Get the current match object. */
  get match(): RegExpExecArray | null {
    return this._match;
  }

  /** Format an error message with source location. */
  error(msg: string, source?: string, offset?: number): string {
    const src = source || this.source;
    const off = offset ?? this.matchStart;
    const pos = offsetToLineCol(src, off);
    return `${msg} (at line ${pos.line}:${pos.col})`;
  }
}

// ---------------------------------------------------------------------------
// Built-in parsers — Phase 1: simple formatting
// ---------------------------------------------------------------------------

/** Block comment: /% ... %/ (SugarCube-style) and /* ... *​/ (CSS-style). */
Wikifier.Parser.add({
  name: "comment",
  match: "(?:/(?:%|\\*))",
  handler(w: Wikifier) {
    const src = w.source;
    const start = w.matchStart;
    if (src[start + 1] === "%") {
      // /% ... %/
      const end = src.indexOf("%/", w.nextMatch);
      if (end >= 0) {
        w.nextMatch = end + 2;
      } else {
        w.nextMatch = src.length;
      }
    } else {
      // /* ... */
      const end = src.indexOf("*/", w.nextMatch);
      if (end >= 0) {
        w.nextMatch = end + 2;
      } else {
        w.nextMatch = src.length;
      }
    }
    // Comments produce no output
  },
});

/** Inline HTML comment: <!-- ... --> */
Wikifier.Parser.add({
  name: "htmlComment",
  match: "<!--",
  handler(w: Wikifier) {
    const end = w.source.indexOf("-->", w.nextMatch);
    if (end >= 0) {
      w.nextMatch = end + 3;
    } else {
      w.nextMatch = w.source.length;
    }
  },
});

/** Line break: literal \n. Suppressed when options.nobr is true. */
Wikifier.Parser.add({
  name: "lineBreak",
  match: "\\n",
  handler(w: Wikifier) {
    if (!w.options.nobr) {
      w.output.appendChild(Wikifier.rt.Output.doc.createElement("br"));
    }
  },
});

/** Emdash: -- → — */
Wikifier.Parser.add({
  name: "emdash",
  match: "--",
  handler(w: Wikifier) {
    w.output.appendChild(Wikifier.rt.Output.doc.createTextNode("\u2014"));
  },
});

/** Escape: backslash followed by any char. */
Wikifier.Parser.add({
  name: "escape",
  match: "\\\\.",
  handler(w: Wikifier) {
    // Output the character after the backslash
    w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText.slice(1)));
  },
});

/** Heading: ! at start of line. Block-level, excluded from 'core' profile. */
Wikifier.Parser.add({
  name: "heading",
  profiles: ["all"],
  match: "^!{1,6}",
  handler(w: Wikifier) {
    const level = Math.min(w.matchText.length, 6);
    const h = Wikifier.rt.Output.doc.createElement(`h${level}`);
    // Parse until end of line
    const termRe = /\n/gm;
    w.subWikify(h, termRe);
    w.output.appendChild(h);
  },
});

/** Horizontal rule: ---- (4+ hyphens) at start of line.
 *  Block-level, excluded from 'core' profile.
 */
Wikifier.Parser.add({
  name: "horizontalRule",
  profiles: ["all"],
  match: "^-{4,}$",
  handler(w: Wikifier) {
    w.output.appendChild(Wikifier.rt.Output.doc.createElement("hr"));
  },
});

/** Bold/italic toggle pairs. */
Wikifier.Parser.add({
  name: "boldItalic",
  match: "'{2,3}|/{2}",
  handler(w: Wikifier) {
    const markup = w.matchText;
    let tag: string;
    let termPattern: string;
    if (markup === "'''") {
      tag = "b";
      termPattern = "'''";
    } else if (markup === "''") {
      tag = "i";
      termPattern = "''";
    } else {
      // //
      tag = "i";
      termPattern = "//";
    }
    const el = Wikifier.rt.Output.doc.createElement(tag);
    const termRe = new RegExp(termPattern.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), "gm");
    w.subWikify(el, termRe);
    w.output.appendChild(el);
  },
});

/** Underline: __text__ */
Wikifier.Parser.add({
  name: "underline",
  match: "__",
  handler(w: Wikifier) {
    const el = Wikifier.rt.Output.doc.createElement("u");
    w.subWikify(el, /__/gm);
    w.output.appendChild(el);
  },
});

/** Strikethrough: ==text== */
Wikifier.Parser.add({
  name: "strikethrough",
  match: "==",
  handler(w: Wikifier) {
    const el = Wikifier.rt.Output.doc.createElement("s");
    w.subWikify(el, /==/gm);
    w.output.appendChild(el);
  },
});

/** Superscript: ^^text^^ */
Wikifier.Parser.add({
  name: "superscript",
  match: "\\^\\^",
  handler(w: Wikifier) {
    const el = Wikifier.rt.Output.doc.createElement("sup");
    w.subWikify(el, /\^\^/gm);
    w.output.appendChild(el);
  },
});

/** Subscript: ~~text~~ */
Wikifier.Parser.add({
  name: "subscript",
  match: "~~",
  handler(w: Wikifier) {
    const el = Wikifier.rt.Output.doc.createElement("sub");
    w.subWikify(el, /~~/gm);
    w.output.appendChild(el);
  },
});

/** Monospaced inline: {{{text}}} */
Wikifier.Parser.add({
  name: "monospacedByLine",
  match: "\\{\\{\\{",
  handler(w: Wikifier) {
    const end = w.source.indexOf("}}}", w.nextMatch);
    if (end >= 0) {
      const code = Wikifier.rt.Output.doc.createElement("code");
      code.textContent = w.source.slice(w.nextMatch, end);
      w.output.appendChild(code);
      w.nextMatch = end + 3;
    } else {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
    }
  },
});

/** Nowiki: """text""" — prevents parsing. */
Wikifier.Parser.add({
  name: "nowiki",
  match: '"{3}',
  handler(w: Wikifier) {
    const end = w.source.indexOf('"""', w.nextMatch);
    if (end >= 0) {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.source.slice(w.nextMatch, end)));
      w.nextMatch = end + 3;
    } else {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
    }
  },
});

// ---------------------------------------------------------------------------
// Built-in parsers — Phase 2: links + images
// ---------------------------------------------------------------------------

/** Link: [[passage]], [[text|passage]], [[text->passage]], [[passage<-text]]
 *  with optional setter [$var = val].
 */
Wikifier.Parser.add({
  name: "link",
  match: "\\[\\[",
  handler(w: Wikifier) {
    const parsed = Wikifier.helpers.parseSquareBracketedMarkup(w);

    if (parsed.error) {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
      return;
    }

    w.nextMatch = parsed.pos;

    if (!parsed.link) {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
      return;
    }

    const passage = Wikifier.helpers.evalPassageId(parsed.link.trim());
    const displayText = parsed.text != null ? Wikifier.helpers.evalText(parsed.text.trim()) : passage;

    let setter: (() => void) | undefined;
    if (parsed.setter) {
      setter = Wikifier.helpers.createShadowSetterCallback(parsed.setter);
    }

    if (Wikifier.isExternalLink(passage)) {
      const a = Wikifier.createExternalLink(w.output, passage, displayText);
      a.textContent = displayText;
    } else {
      Wikifier.createInternalLink(w.output, passage, displayText, setter);
    }
  },
});

/** Image: [img[src]], [img[src][link]] */
Wikifier.Parser.add({
  name: "image",
  match: "\\[img\\[",
  handler(w: Wikifier) {
    const parsed = Wikifier.helpers.parseSquareBracketedMarkup(w);

    if (parsed.error || !parsed.source) {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
      return;
    }

    w.nextMatch = parsed.pos;

    const src = Wikifier.helpers.evalText(parsed.source.trim());
    const img = Wikifier.rt.Output.doc.createElement("img");
    img.src = src;

    if (parsed.link) {
      const link = Wikifier.helpers.evalPassageId(parsed.link.trim());
      if (Wikifier.isExternalLink(link)) {
        const a = Wikifier.createExternalLink(w.output, link);
        a.appendChild(img);
      } else {
        const a = Wikifier.createInternalLink(w.output, link);
        a.appendChild(img);
      }
    } else {
      w.output.appendChild(img);
    }
  },
});

// ---------------------------------------------------------------------------
// Built-in parsers — Phase 4: macros
// ---------------------------------------------------------------------------

/** Macro: <<name args>>, block macros <<name>>...body...<</name>>.
 *  Handles self-closing, block, and widget-fallback macros.
 */
Wikifier.Parser.add({
  name: "macro",
  match: "<<(?:\\/[A-Za-z][\\w-]*|[A-Za-z][\\w-]*)",
  handler(w: Wikifier) {
    const src = w.source;
    const start = w.matchStart;

    // Check if this is a closing tag (handled by subWikify terminator)
    if (src[start + 2] === "/") {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
      return;
    }

    // Parse macro name from matchText: <<name...
    const nameMatch = w.matchText.match(/^<<([A-Za-z][\w-]*)/);
    if (!nameMatch) {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
      return;
    }

    const macroName = nameMatch[1];

    // Find closing >> to get the full macro tag
    let pos = w.nextMatch;
    let depth = 0;
    let foundClose = false;

    for (; pos < src.length - 1; pos++) {
      if (src[pos] === ">" && src[pos + 1] === ">") {
        if (depth === 0) {
          foundClose = true;
          break;
        }
        depth--;
        pos++; // skip second >
      } else if (src[pos] === "<" && src[pos + 1] === "<") {
        depth++;
        pos++; // skip second <
      }
    }

    if (!foundClose) {
      const loc = offsetToLineCol(src, start);
      console.warn(`[wikifier] unclosed macro <<${macroName}>> at ${loc.line}:${loc.col}`);
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(w.matchText));
      return;
    }

    // Raw argument string (between name and >>)
    const rawArgs = src.slice(start + 2 + macroName.length, pos).trim();
    w.nextMatch = pos + 2; // past >>

    // Look up macro
    const macroDef = Wikifier.rt.Macro.get(macroName);

    if (macroDef) {
      // Parse arguments
      const parsedArgs = macroDef.skipArgs ? [] : parseMacroArgs(rawArgs);
      (parsedArgs as any).raw = rawArgs;
      (parsedArgs as any).full = rawArgs;

      // Check if this is a block macro (has tags)
      const isBlock = macroDef.tags != null && Array.isArray(macroDef.tags);
      const payload: MacroPayloadEntry[] = [];

      if (isBlock) {
        collectMacroPayload(w, macroName, macroDef.tags!, payload);
      }

      // Build macro context
      const macroOutput = Wikifier.rt.Output.doc.createDocumentFragment();
      const context = {
        name: macroName,
        args: parsedArgs,
        output: macroOutput,
        payload,
        error(msg: string): string {
          const loc = offsetToLineCol(src, start);
          return `Error in macro <<${macroName}>> at ${loc.line}:${loc.col}: ${msg}`;
        },
        addShadow(_varName: string): void {},
        createShadowWrapper(fn: Function): Function {
          return fn;
        },
        createDebugView(): void {},
        self: macroDef,
        parser: w,
      };

      try {
        macroDef.handler.call(context);
      } catch (e) {
        const loc = offsetToLineCol(src, start);
        console.error(`[wikifier] error in macro <<${macroName}>> at ${loc.line}:${loc.col}:`, e);
      }

      w.output.appendChild(macroOutput);
    } else {
      // Try widget fallback
      if (Wikifier.rt.Navigation.has(macroName) || Wikifier.rt.Navigation.has("widget_" + macroName)) {
        const parsedArgs = parseMacroArgs(rawArgs);
        Wikifier.rt.Widget.call(macroName, ...parsedArgs);
      } else {
        // Unknown macro — output as text with warning
        const loc = offsetToLineCol(src, start);
        console.warn(`[wikifier] unknown macro <<${macroName}>> at ${loc.line}:${loc.col}`);
        w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(`<<${macroName}${rawArgs ? " " + rawArgs : ""}>>`));
      }
    }
  },
});

interface MacroPayloadEntry {
  name: string;
  args: any[];
  contents: string;
  output: DocumentFragment;
}

/** Collect macro payload entries (body segments between clause tags). */
function collectMacroPayload(
  w: Wikifier,
  macroName: string,
  clauseTags: string[],
  payload: MacroPayloadEntry[],
): void {
  const src = w.source;
  const allClauseTags = new Set(clauseTags);

  // First payload entry is for the main macro body
  let currentName = macroName;
  let currentArgs: any[] = [];
  let bodyStart = w.nextMatch;

  // Scan for clause tags and closing tag
  const tagRe = new RegExp(
    `<<(?:(${escapeRegex(macroName)})(\\s(?:(?:.|\\n)*?))?>>"?|` +
    `(${[...clauseTags.map(escapeRegex), escapeRegex(macroName)].join("|")})(\\s(?:(?:.|\\n)*?))?>>"?|` +
    `\\/(${escapeRegex(macroName)})>>)`,
    "gm",
  );

  let depth = 0;

  while (w.nextMatch < src.length) {
    tagRe.lastIndex = w.nextMatch;
    const match = tagRe.exec(src);
    if (!match) {
      // No closing tag found — unclosed macro
      const loc = offsetToLineCol(src, bodyStart);
      console.warn(
        `[wikifier] unclosed block macro <<${macroName}>> opened at ${loc.line}:${loc.col}`,
      );
      break;
    }

    // Check what we matched
    if (match[5]) {
      // Closing tag: <</name>>
      if (depth === 0) {
        // Capture final body segment
        const bodyEnd = match.index;
        const bodyContent = src.slice(bodyStart, bodyEnd);
        const bodyFrag = Wikifier.rt.Output.doc.createDocumentFragment();
        new BodyWikifier(w, bodyFrag, bodyStart, bodyEnd);
        payload.push({
          name: currentName,
          args: currentArgs,
          contents: bodyContent,
          output: bodyFrag,
        });
        w.nextMatch = match.index + match[0].length;
        return;
      }
      depth--;
      w.nextMatch = match.index + match[0].length;
      continue;
    }

    if (match[1]) {
      // Opening of a nested same-name macro — increase depth
      depth++;
      w.nextMatch = match.index + match[0].length;
      continue;
    }

    if (match[3] && depth === 0 && allClauseTags.has(match[3])) {
      // Clause tag at our depth
      const bodyEnd = match.index;
      const bodyContent = src.slice(bodyStart, bodyEnd);
      const bodyFrag = Wikifier.rt.Output.doc.createDocumentFragment();
      new BodyWikifier(w, bodyFrag, bodyStart, bodyEnd);
      payload.push({
        name: currentName,
        args: currentArgs,
        contents: bodyContent,
        output: bodyFrag,
      });

      // Start new segment
      currentName = match[3];
      currentArgs = match[4] ? parseMacroArgs(match[4].trim()) : [];
      w.nextMatch = match.index + match[0].length;
      bodyStart = w.nextMatch;
      continue;
    }

    // Something else or nested — skip past
    w.nextMatch = match.index + match[0].length;
  }
}

/** Helper to wikify a body segment. Creates a Wikifier that parses a substring. */
class BodyWikifier {
  constructor(parentW: Wikifier, output: DocumentFragment, bodyStart: number, bodyEnd: number) {
    const src = parentW.source;
    const bodySource = src.slice(bodyStart, bodyEnd);
    if (bodySource.length > 0) {
      // Parse the body segment through a new wikifier
      new Wikifier(output, bodySource);
    }
  }
}

/** Parse macro argument string into an array of values.
 *  Handles quoted strings, numbers, booleans, null, undefined,
 *  and bare expressions.
 */
function parseMacroArgs(raw: string): any[] {
  if (!raw) return [];

  const args: any[] = [];
  const re = /(?:"((?:\\.|[^"\\])*)"|'((?:\\.|[^'\\])*)'|`((?:\\.|[^`\\])*)`|\[(?:img)??\[|(?:[^\s"'`\[\]]+))/g;
  let match;

  while ((match = re.exec(raw)) !== null) {
    const token = match[0];

    if (match[1] !== undefined) {
      // Double-quoted string
      args.push(match[1].replace(/\\(.)/g, "$1"));
    } else if (match[2] !== undefined) {
      // Single-quoted string
      args.push(match[2].replace(/\\(.)/g, "$1"));
    } else if (match[3] !== undefined) {
      // Template string — evaluate
      try {
        args.push(Wikifier.evalExpression("`" + match[3] + "`"));
      } catch {
        args.push(match[3]);
      }
    } else if (token === "true") {
      args.push(true);
    } else if (token === "false") {
      args.push(false);
    } else if (token === "null") {
      args.push(null);
    } else if (token === "undefined") {
      args.push(undefined);
    } else if (/^-?\d+(?:\.\d+)?(?:e[+-]?\d+)?$/i.test(token)) {
      args.push(Number(token));
    } else if (token.startsWith("$") || token.startsWith("_")) {
      // Variable reference — evaluate
      try {
        args.push(Wikifier.evalExpression(token));
      } catch {
        args.push(token);
      }
    } else {
      // Bare token — try to evaluate as expression, fall back to string
      try {
        args.push(Wikifier.evalExpression(token));
      } catch {
        args.push(token);
      }
    }
  }

  return args;
}

function escapeRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

// ---------------------------------------------------------------------------
// Built-in parsers — Phase 3: variable interpolation + HTML
// ---------------------------------------------------------------------------

/** Variable interpolation: $var, _var, $obj.prop, $arr[0], etc.
 *  Evaluates via Wikifier.rt.State.get() and outputs the result as text.
 */
Wikifier.Parser.add({
  name: "variable",
  match: "(?:\\$|_)\\w+(?:(?:\\.\\w+)|(?:\\[[^\\]]+\\]))*",
  handler(w: Wikifier) {
    const expr = w.matchText;
    try {
      const value = Wikifier.evalExpression(expr);
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(value == null ? "" : String(value)));
    } catch {
      // If evaluation fails, output the raw text
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(expr));
    }
  },
});

/** HTML tag: <tag attr="val">, </tag>, <tag />.
 *  Supports @attr="expr" for dynamic attribute evaluation.
 *  Void elements are self-closing. Block elements use subWikify.
 */
Wikifier.Parser.add({
  name: "html",
  match: "<\\/?[A-Za-z][\\w-]*(?:\\s[^>]*)?\\/?>",
  handler(w: Wikifier) {
    const src = w.source;
    const tagText = w.matchText;

    // Check if it's a closing tag
    if (tagText.startsWith("</")) {
      // Closing tags are handled by subWikify terminators, not here.
      // If we hit one outside subWikify, output it as text.
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(tagText));
      return;
    }

    // Parse tag name
    const tagMatch = tagText.match(/^<([A-Za-z][\w-]*)/);
    if (!tagMatch) {
      w.output.appendChild(Wikifier.rt.Output.doc.createTextNode(tagText));
      return;
    }

    const tagName = tagMatch[1].toLowerCase();
    const isSelfClosing = tagText.endsWith("/>") || isVoidElement(tagName);

    const el = Wikifier.rt.Output.doc.createElement(tagName);

    // Parse attributes
    const attrRegex = /\s+([@\w-]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|(\S+)))?/g;
    let attrMatch;
    while ((attrMatch = attrRegex.exec(tagText)) !== null) {
      let attrName = attrMatch[1];
      const attrValue = attrMatch[2] ?? attrMatch[3] ?? attrMatch[4] ?? "";

      if (attrName.startsWith("@")) {
        // Dynamic attribute: evaluate expression
        attrName = attrName.slice(1);
        try {
          const evalValue = Wikifier.evalExpression(attrValue);
          el.setAttribute(attrName, evalValue == null ? "" : String(evalValue));
        } catch {
          el.setAttribute(attrName, attrValue);
        }
      } else {
        el.setAttribute(attrName, attrValue);
      }
    }

    if (!isSelfClosing) {
      // Block element — parse content until closing tag
      const closeRe = new RegExp(`<\\/${tagName}\\s*>`, "gim");
      w.subWikify(el, closeRe);
    }

    w.output.appendChild(el);
  },
});

const VOID_ELEMENTS = new Set([
  "area", "base", "br", "col", "embed", "hr", "img", "input",
  "link", "meta", "param", "source", "track", "wbr",
]);

function isVoidElement(tagName: string): boolean {
  return VOID_ELEMENTS.has(tagName);
}

// Reset modified flag after initial parser registration
Wikifier.Parser.modified = false;
