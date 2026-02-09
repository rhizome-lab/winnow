/**
 * flash.text package — TextField, TextFormat, TextFieldType,
 * TextFieldAutoSize, Font.
 */

import { Rectangle } from "./flash_geom";
import { InteractiveObject } from "./flash_display";

// ---------------------------------------------------------------------------
// TextFieldType
// ---------------------------------------------------------------------------

export class TextFieldType {
  static readonly DYNAMIC = "dynamic";
  static readonly INPUT = "input";
}

// ---------------------------------------------------------------------------
// TextFieldAutoSize
// ---------------------------------------------------------------------------

export class TextFieldAutoSize {
  static readonly CENTER = "center";
  static readonly LEFT = "left";
  static readonly NONE = "none";
  static readonly RIGHT = "right";
}

// ---------------------------------------------------------------------------
// TextFormat
// ---------------------------------------------------------------------------

export class TextFormat {
  align: string | null;
  blockIndent: number | null;
  bold: boolean | null;
  bullet: boolean | null;
  color: number | null;
  display: string | null;
  font: string | null;
  indent: number | null;
  italic: boolean | null;
  kerning: boolean | null;
  leading: number | null;
  leftMargin: number | null;
  letterSpacing: number | null;
  rightMargin: number | null;
  size: number | null;
  tabStops: number[] | null;
  target: string | null;
  underline: boolean | null;
  url: string | null;

  constructor(
    font: string | null = null,
    size: number | null = null,
    color: number | null = null,
    bold: boolean | null = null,
    italic: boolean | null = null,
    underline: boolean | null = null,
    url: string | null = null,
    target: string | null = null,
    align: string | null = null,
    leftMargin: number | null = null,
    rightMargin: number | null = null,
    indent: number | null = null,
    leading: number | null = null,
  ) {
    this.font = font;
    this.size = size;
    this.color = color;
    this.bold = bold;
    this.italic = italic;
    this.underline = underline;
    this.url = url;
    this.target = target;
    this.align = align;
    this.leftMargin = leftMargin;
    this.rightMargin = rightMargin;
    this.indent = indent;
    this.leading = leading;
    this.blockIndent = null;
    this.bullet = null;
    this.display = null;
    this.kerning = null;
    this.letterSpacing = null;
    this.tabStops = null;
  }
}

// ---------------------------------------------------------------------------
// TextField
// ---------------------------------------------------------------------------

export class TextField extends InteractiveObject {
  alwaysShowSelection = false;
  antiAliasType = "normal";
  autoSize = TextFieldAutoSize.NONE;
  background = false;
  backgroundColor = 0xffffff;
  border = false;
  borderColor = 0x000000;
  bottomScrollV = 1;
  caretIndex = 0;
  condenseWhite = false;
  defaultTextFormat: TextFormat = new TextFormat();
  displayAsPassword = false;
  embedFonts = false;
  gridFitType = "pixel";
  maxChars = 0;
  maxScrollH = 0;
  maxScrollV = 1;
  mouseWheelEnabled = true;
  multiline = false;
  numLines = 1;
  restrict: string | null = null;
  scrollH = 0;
  scrollV = 1;
  selectable = true;
  selectionBeginIndex = 0;
  selectionEndIndex = 0;
  sharpness = 0;
  styleSheet: any = null;
  textColor = 0x000000;
  textHeight = 0;
  textWidth = 0;
  thickness = 0;
  type = TextFieldType.DYNAMIC;
  useRichTextClipboard = false;
  wordWrap = false;

  private _text = "";
  private _htmlText = "";
  private _formats: { begin: number; end: number; format: TextFormat }[] = [];

  get text(): string {
    return this._text;
  }
  set text(value: string) {
    this._text = value;
    this._htmlText = value;
    this.numLines = (value.match(/\n/g) || []).length + 1;
  }

  get htmlText(): string {
    return this._htmlText;
  }
  set htmlText(value: string) {
    this._htmlText = value;
    // Strip tags for the plain text representation.
    this._text = value.replace(/<[^>]*>/g, "");
    this.numLines = (this._text.match(/\n/g) || []).length + 1;
  }

  get length(): number {
    return this._text.length;
  }

  appendText(newText: string): void {
    this.text = this._text + newText;
  }

  getTextFormat(beginIndex = -1, endIndex = -1): TextFormat {
    void beginIndex;
    void endIndex;
    if (this._formats.length > 0) return this._formats[0].format;
    return this.defaultTextFormat;
  }

  setTextFormat(format: TextFormat, beginIndex = -1, endIndex = -1): void {
    const begin = beginIndex < 0 ? 0 : beginIndex;
    const end = endIndex < 0 ? this._text.length : endIndex;
    this._formats.push({ begin, end, format });
  }

  replaceSelectedText(value: string): void {
    this.replaceText(this.selectionBeginIndex, this.selectionEndIndex, value);
  }

  replaceText(beginIndex: number, endIndex: number, newText: string): void {
    this._text =
      this._text.substring(0, beginIndex) + newText + this._text.substring(endIndex);
    this._htmlText = this._text;
  }

  setSelection(beginIndex: number, endIndex: number): void {
    this.selectionBeginIndex = beginIndex;
    this.selectionEndIndex = endIndex;
    this.caretIndex = endIndex;
  }

  getCharBoundaries(charIndex: number): Rectangle | null {
    void charIndex;
    return new Rectangle(0, 0, 8, 16);
  }

  getCharIndexAtPoint(x: number, y: number): number {
    void x;
    void y;
    return 0;
  }

  getLineIndexAtPoint(x: number, y: number): number {
    void x;
    void y;
    return 0;
  }

  getLineIndexOfChar(charIndex: number): number {
    void charIndex;
    return 0;
  }

  getLineLength(lineIndex: number): number {
    const lines = this._text.split("\n");
    return lines[lineIndex]?.length ?? 0;
  }

  getLineMetrics(lineIndex: number): any {
    void lineIndex;
    return { ascent: 12, descent: 4, height: 16, leading: 0, width: 0, x: 0 };
  }

  getLineOffset(lineIndex: number): number {
    const lines = this._text.split("\n");
    let offset = 0;
    for (let i = 0; i < lineIndex && i < lines.length; i++) {
      offset += lines[i].length + 1;
    }
    return offset;
  }

  getLineText(lineIndex: number): string {
    return this._text.split("\n")[lineIndex] ?? "";
  }

  getFirstCharInParagraph(charIndex: number): number {
    const before = this._text.substring(0, charIndex);
    const nlPos = before.lastIndexOf("\n");
    return nlPos === -1 ? 0 : nlPos + 1;
  }

  getParagraphLength(charIndex: number): number {
    const start = this.getFirstCharInParagraph(charIndex);
    const nlPos = this._text.indexOf("\n", start);
    return (nlPos === -1 ? this._text.length : nlPos) - start;
  }

  getImageReference(_id: string): any {
    return null;
  }

  static isFontCompatible(_fontName: string, _fontStyle: string): boolean {
    return true;
  }
}

// ---------------------------------------------------------------------------
// Font
// ---------------------------------------------------------------------------

export class Font {
  fontName = "";
  fontStyle = "regular";
  fontType = "embedded";

  hasGlyphs(str: string): boolean {
    void str;
    return true;
  }

  static enumerateFonts(_enumerateDeviceFonts = false): Font[] {
    return [];
  }

  static registerFont(_fontClass: any): void {}
}
