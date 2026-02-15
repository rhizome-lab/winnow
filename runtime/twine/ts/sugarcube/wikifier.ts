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

import * as State from "./state";
import * as Macro from "./macro";
import * as Widget from "./widget";
import * as Navigation from "./navigation";

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
// Parser registry
// ---------------------------------------------------------------------------

export class ParserRegistry {
  private parsers: ParserDef[] = [];
  private compiledRe: RegExp | null = null;
  private parserNames: string[] = [];
  modified = false;

  add(def: ParserDef): void {
    // Replace existing parser with the same name
    const idx = this.parsers.findIndex(p => p.name === def.name);
    if (idx >= 0) {
      this.parsers[idx] = def;
    } else {
      this.parsers.push(def);
    }
    this.compiledRe = null;
    this.modified = true;
    Wikifier.invalidateCache(def.name);
  }

  delete(name: string): void {
    const idx = this.parsers.findIndex(p => p.name === name);
    if (idx >= 0) {
      this.parsers.splice(idx, 1);
      this.compiledRe = null;
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

  /** Compile a single combined regex from all parser match patterns. */
  getCompiledRe(): RegExp {
    if (this.compiledRe === null) {
      this.parserNames = this.parsers.map(p => p.name);
      const combined = this.parsers.map(p => `(${p.match})`).join("|");
      this.compiledRe = new RegExp(combined, "gm");
    }
    return this.compiledRe;
  }

  /** Get the parser that corresponds to a regex match group index. */
  getParserForGroup(groupIndex: number): ParserDef | undefined {
    return this.parsers[groupIndex];
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
  static wikifyEval(markup: string): DocumentFragment {
    // Check cache
    const cached = Wikifier.cache.get(markup);
    if (cached) {
      return cached.fragment.cloneNode(true) as DocumentFragment;
    }

    const frag = document.createDocumentFragment();
    const w = new Wikifier(frag, markup);
    Wikifier.cache.set(markup, {
      fragment: frag.cloneNode(true) as DocumentFragment,
      parserDeps: w.parserDeps,
    });
    return frag;
  }

  /** Get a story variable value. */
  static getValue(name: string): any {
    return State.get(name);
  }

  /** Set a story variable value. */
  static setValue(name: string, value: any): void {
    State.set(name, value);
  }

  /** Check if a URL is external. */
  static isExternalLink(url: string): boolean {
    return /^(?:https?:\/\/|mailto:|tel:)/.test(url);
  }

  /** Create an external link element. */
  static createExternalLink(node: Node, url: string, text?: string): HTMLAnchorElement {
    const a = document.createElement("a");
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
    const a = document.createElement("a");
    a.classList.add("link-internal");
    if (text) {
      a.textContent = text;
    }
    if (Navigation.has(passage)) {
      if (State.hasPlayed(passage)) {
        a.classList.add("link-visited");
      }
      a.addEventListener("click", (e) => {
        e.preventDefault();
        if (setter) setter();
        Navigation.goto(passage);
      });
      // Keyboard accessibility
      a.addEventListener("keypress", (e: KeyboardEvent) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (setter) setter();
          Navigation.goto(passage);
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
    return new Function("State", "setup", "V", "Config", "settings", `return (${expr})`)(
      g.State, g.setup, g.V, g.Config, g.settings,
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
  matchStart = 0;
  matchLength = 0;
  matchText = "";
  nextMatch = 0;
  parserDeps: Set<string> = new Set();

  private _match: RegExpExecArray | null = null;

  constructor(output: DocumentFragment | Node | null, source: string) {
    this.output = output || document.createDocumentFragment();
    this.source = source;
    this.subWikify(this.output);
  }

  /** Output plain text between two source positions. */
  outputText(destination: Node, startPos: number, endPos: number): void {
    if (endPos > startPos) {
      destination.appendChild(document.createTextNode(this.source.slice(startPos, endPos)));
    }
  }

  /** Parse and render from current position, optionally stopping at a terminator regex. */
  subWikify(output: Node, termRegex?: RegExp | null, context?: any): void {
    const oldOutput = this.output;
    this.output = output;

    const registry = Wikifier.Parser;
    const re = registry.getCompiledRe();
    re.lastIndex = this.nextMatch;

    const parsers = registry.getParsers();
    let terminatorMatch: RegExpExecArray | null = null;

    while (true) {
      // Check terminator first
      if (termRegex) {
        termRegex.lastIndex = this.nextMatch;
        terminatorMatch = termRegex.exec(this.source);
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

/** Line break: literal \n. */
Wikifier.Parser.add({
  name: "lineBreak",
  match: "\\n",
  handler(w: Wikifier) {
    w.output.appendChild(document.createElement("br"));
  },
});

/** Emdash: -- → — */
Wikifier.Parser.add({
  name: "emdash",
  match: "--",
  handler(w: Wikifier) {
    w.output.appendChild(document.createTextNode("\u2014"));
  },
});

/** Escape: backslash followed by any char. */
Wikifier.Parser.add({
  name: "escape",
  match: "\\\\.",
  handler(w: Wikifier) {
    // Output the character after the backslash
    w.output.appendChild(document.createTextNode(w.matchText.slice(1)));
  },
});

/** Heading: ! at start of line. */
Wikifier.Parser.add({
  name: "heading",
  match: "^!{1,6}",
  handler(w: Wikifier) {
    const level = Math.min(w.matchText.length, 6);
    const h = document.createElement(`h${level}`);
    // Parse until end of line
    const termRe = /\n/gm;
    w.subWikify(h, termRe);
    w.output.appendChild(h);
  },
});

/** Horizontal rule: ---- (4+ hyphens) at start of line. */
Wikifier.Parser.add({
  name: "horizontalRule",
  match: "^-{4,}$",
  handler(w: Wikifier) {
    w.output.appendChild(document.createElement("hr"));
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
    const el = document.createElement(tag);
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
    const el = document.createElement("u");
    w.subWikify(el, /__/gm);
    w.output.appendChild(el);
  },
});

/** Strikethrough: ==text== */
Wikifier.Parser.add({
  name: "strikethrough",
  match: "==",
  handler(w: Wikifier) {
    const el = document.createElement("s");
    w.subWikify(el, /==/gm);
    w.output.appendChild(el);
  },
});

/** Superscript: ^^text^^ */
Wikifier.Parser.add({
  name: "superscript",
  match: "\\^\\^",
  handler(w: Wikifier) {
    const el = document.createElement("sup");
    w.subWikify(el, /\^\^/gm);
    w.output.appendChild(el);
  },
});

/** Subscript: ~~text~~ */
Wikifier.Parser.add({
  name: "subscript",
  match: "~~",
  handler(w: Wikifier) {
    const el = document.createElement("sub");
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
      const code = document.createElement("code");
      code.textContent = w.source.slice(w.nextMatch, end);
      w.output.appendChild(code);
      w.nextMatch = end + 3;
    } else {
      w.output.appendChild(document.createTextNode(w.matchText));
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
      w.output.appendChild(document.createTextNode(w.source.slice(w.nextMatch, end)));
      w.nextMatch = end + 3;
    } else {
      w.output.appendChild(document.createTextNode(w.matchText));
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
      w.output.appendChild(document.createTextNode(w.matchText));
      return;
    }

    w.nextMatch = parsed.pos;

    if (!parsed.link) {
      w.output.appendChild(document.createTextNode(w.matchText));
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
      w.output.appendChild(document.createTextNode(w.matchText));
      return;
    }

    w.nextMatch = parsed.pos;

    const src = Wikifier.helpers.evalText(parsed.source.trim());
    const img = document.createElement("img");
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

// Reset modified flag after initial parser registration
Wikifier.Parser.modified = false;
