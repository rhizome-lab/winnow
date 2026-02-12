/**
 * flash.text package — TextField, TextFormat, TextFieldType,
 * TextFieldAutoSize, Font.
 */

import { Rectangle } from "./geom";
import { InteractiveObject, registerTimelineFactory } from "./display";
import { createMeasureContext } from "./platform";

// ---------------------------------------------------------------------------
// Shared off-screen canvas for text measurement
// ---------------------------------------------------------------------------

let _measureCtx: CanvasRenderingContext2D | null = null;

function getMeasureCtx(): CanvasRenderingContext2D {
  if (!_measureCtx) {
    _measureCtx = createMeasureContext();
  }
  return _measureCtx;
}

function _fontString(fmt: TextFormat, defaultFmt: TextFormat): string {
  const italic = (fmt.italic ?? defaultFmt.italic) ? "italic " : "";
  const bold = (fmt.bold ?? defaultFmt.bold) ? "bold " : "";
  const size = fmt.size ?? defaultFmt.size ?? 12;
  const font = fmt.font ?? defaultFmt.font ?? "Times New Roman";
  return `${italic}${bold}${size}px "${font}"`;
}

// ---------------------------------------------------------------------------
// AntiAliasType
// ---------------------------------------------------------------------------

export class AntiAliasType {
  static readonly ADVANCED = "advanced";
  static readonly NORMAL = "normal";
}

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
// TextFormatAlign
// ---------------------------------------------------------------------------

export class TextFormatAlign {
  static readonly CENTER = "center";
  static readonly JUSTIFY = "justify";
  static readonly LEFT = "left";
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
  _alwaysShowSelection = false;
  _antiAliasType = "normal";
  _autoSize: string = TextFieldAutoSize.NONE;
  _background = false;
  _backgroundColor = 0xffffff;
  _border = false;
  _borderColor = 0x000000;
  _bottomScrollV = 1;
  _caretIndex = 0;
  _condenseWhite = false;
  _defaultTextFormat: TextFormat = new TextFormat();
  _displayAsPassword = false;
  _embedFonts = false;
  _gridFitType = "pixel";
  _maxChars = 0;
  _maxScrollH = 0;
  _maxScrollV = 1;
  _mouseWheelEnabled = true;
  _multiline = false;
  _numLines = 1;
  _restrict: string | null = null;
  _scrollH = 0;
  _scrollV = 1;
  _selectable = true;
  _selectionBeginIndex = 0;
  _selectionEndIndex = 0;
  _sharpness = 0;
  _styleSheet: object | null = null;
  _textColor = 0x000000;
  _textHeight = 0;
  _textWidth = 0;
  _thickness = 0;
  _type: string = TextFieldType.DYNAMIC;
  _useRichTextClipboard = false;
  _wordWrap = false;

  get alwaysShowSelection() { return this._alwaysShowSelection; }
  set alwaysShowSelection(v: boolean) { this._alwaysShowSelection = v; }
  get antiAliasType() { return this._antiAliasType; }
  set antiAliasType(v: string) { this._antiAliasType = v; }
  get autoSize() { return this._autoSize; }
  set autoSize(v: string) { this._autoSize = v; }
  get background() { return this._background; }
  set background(v: boolean) { this._background = v; }
  get backgroundColor() { return this._backgroundColor; }
  set backgroundColor(v: number) { this._backgroundColor = v; }
  get border() { return this._border; }
  set border(v: boolean) { this._border = v; }
  get borderColor() { return this._borderColor; }
  set borderColor(v: number) { this._borderColor = v; }
  get bottomScrollV() { return this._bottomScrollV; }
  set bottomScrollV(v: number) { this._bottomScrollV = v; }
  get caretIndex() { return this._caretIndex; }
  set caretIndex(v: number) { this._caretIndex = v; }
  get condenseWhite() { return this._condenseWhite; }
  set condenseWhite(v: boolean) { this._condenseWhite = v; }
  get defaultTextFormat() { return this._defaultTextFormat; }
  set defaultTextFormat(v: TextFormat) { this._defaultTextFormat = v; }
  get displayAsPassword() { return this._displayAsPassword; }
  set displayAsPassword(v: boolean) { this._displayAsPassword = v; }
  get embedFonts() { return this._embedFonts; }
  set embedFonts(v: boolean) { this._embedFonts = v; }
  get gridFitType() { return this._gridFitType; }
  set gridFitType(v: string) { this._gridFitType = v; }
  get maxChars() { return this._maxChars; }
  set maxChars(v: number) { this._maxChars = v; }
  get maxScrollH() { return this._maxScrollH; }
  set maxScrollH(v: number) { this._maxScrollH = v; }
  get maxScrollV() { return this._maxScrollV; }
  set maxScrollV(v: number) { this._maxScrollV = v; }
  get mouseWheelEnabled() { return this._mouseWheelEnabled; }
  set mouseWheelEnabled(v: boolean) { this._mouseWheelEnabled = v; }
  get multiline() { return this._multiline; }
  set multiline(v: boolean) { this._multiline = v; }
  get numLines() { return this._numLines; }
  set numLines(v: number) { this._numLines = v; }
  get restrict() { return this._restrict; }
  set restrict(v: string | null) { this._restrict = v; }
  get scrollH() { return this._scrollH; }
  set scrollH(v: number) { this._scrollH = v; }
  get scrollV() { return this._scrollV; }
  set scrollV(v: number) { this._scrollV = v; }
  get selectable() { return this._selectable; }
  set selectable(v: boolean) { this._selectable = v; }
  get selectionBeginIndex() { return this._selectionBeginIndex; }
  set selectionBeginIndex(v: number) { this._selectionBeginIndex = v; }
  get selectionEndIndex() { return this._selectionEndIndex; }
  set selectionEndIndex(v: number) { this._selectionEndIndex = v; }
  get sharpness() { return this._sharpness; }
  set sharpness(v: number) { this._sharpness = v; }
  get styleSheet() { return this._styleSheet; }
  set styleSheet(v: object | null) { this._styleSheet = v; }
  get textColor() { return this._textColor; }
  set textColor(v: number) { this._textColor = v; }
  get textHeight() { return this._textHeight; }
  set textHeight(v: number) { this._textHeight = v; }
  get textWidth() { return this._textWidth; }
  set textWidth(v: number) { this._textWidth = v; }
  get thickness() { return this._thickness; }
  set thickness(v: number) { this._thickness = v; }
  get type() { return this._type; }
  set type(v: string) { this._type = v; }
  get useRichTextClipboard() { return this._useRichTextClipboard; }
  set useRichTextClipboard(v: boolean) { this._useRichTextClipboard = v; }
  get wordWrap() { return this._wordWrap; }
  set wordWrap(v: boolean) { this._wordWrap = v; }

  private _text = "";
  private _htmlText = "";
  private _formats: { begin: number; end: number; format: TextFormat }[] = [];

  get text(): string {
    return this._text;
  }
  set text(value: string) {
    this._text = value;
    this._htmlText = value;
    this._numLines = (value.match(/\n/g) || []).length + 1;
  }

  get htmlText(): string {
    return this._htmlText;
  }
  set htmlText(value: string) {
    this._htmlText = value;
    // Strip tags for the plain text representation.
    this._text = value.replace(/<[^>]*>/g, "");
    this._numLines = (this._text.match(/\n/g) || []).length + 1;
  }

  get length(): number {
    return this._text.length;
  }

  appendText(newText: string): void {
    this.text = this._text + newText;
  }

  getTextFormat(beginIndex = -1, endIndex = -1): TextFormat {
    if (beginIndex < 0 && endIndex < 0) {
      if (this._formats.length > 0) return this._formats[0].format;
      return this.defaultTextFormat;
    }
    const begin = beginIndex < 0 ? 0 : beginIndex;
    const end = endIndex < 0 ? this._text.length : endIndex;
    // Find overlapping format entries and merge attributes.
    const result = new TextFormat();
    // Start with defaults.
    const def = this.defaultTextFormat;
    Object.assign(result, {
      font: def.font, size: def.size, color: def.color, bold: def.bold,
      italic: def.italic, underline: def.underline, align: def.align,
      leftMargin: def.leftMargin, rightMargin: def.rightMargin,
      indent: def.indent, leading: def.leading,
    });
    for (const entry of this._formats) {
      if (entry.end <= begin || entry.begin >= end) continue;
      const f = entry.format;
      if (f.font != null) result.font = f.font;
      if (f.size != null) result.size = f.size;
      if (f.color != null) result.color = f.color;
      if (f.bold != null) result.bold = f.bold;
      if (f.italic != null) result.italic = f.italic;
      if (f.underline != null) result.underline = f.underline;
      if (f.align != null) result.align = f.align;
      if (f.leftMargin != null) result.leftMargin = f.leftMargin;
      if (f.rightMargin != null) result.rightMargin = f.rightMargin;
      if (f.indent != null) result.indent = f.indent;
      if (f.leading != null) result.leading = f.leading;
      if (f.letterSpacing != null) result.letterSpacing = f.letterSpacing;
      if (f.kerning != null) result.kerning = f.kerning;
    }
    return result;
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
    if (charIndex < 0 || charIndex >= this._text.length) return null;
    const lineIdx = this.getLineIndexOfChar(charIndex);
    const lineOffset = this.getLineOffset(lineIdx);
    const lineText = this.getLineText(lineIdx);
    const fmt = this.getTextFormat(charIndex, charIndex + 1);
    const ctx = getMeasureCtx();
    ctx.font = _fontString(fmt, this.defaultTextFormat);
    // Measure text before this char for x position.
    const beforeText = lineText.substring(0, charIndex - lineOffset);
    const xPos = ctx.measureText(beforeText).width;
    const charWidth = ctx.measureText(this._text[charIndex]).width;
    const metrics = this.getLineMetrics(lineIdx);
    // y = line index * line height
    const yPos = lineIdx * metrics.height;
    return new Rectangle(xPos, yPos, charWidth, metrics.height);
  }

  getCharIndexAtPoint(x: number, y: number): number {
    const lineIdx = this.getLineIndexAtPoint(x, y);
    const lineText = this.getLineText(lineIdx);
    const lineOffset = this.getLineOffset(lineIdx);
    const fmt = this.getTextFormat(lineOffset, lineOffset + lineText.length);
    const ctx = getMeasureCtx();
    ctx.font = _fontString(fmt, this.defaultTextFormat);
    // Linear scan to find the char under x.
    let accWidth = 0;
    for (let i = 0; i < lineText.length; i++) {
      const charWidth = ctx.measureText(lineText[i]).width;
      if (accWidth + charWidth > x) return lineOffset + i;
      accWidth += charWidth;
    }
    return lineOffset + lineText.length - 1;
  }

  getLineIndexAtPoint(_x: number, y: number): number {
    const lines = this._text.split("\n");
    const metrics = this.getLineMetrics(0);
    const lineHeight = metrics.height;
    const idx = Math.floor(y / lineHeight);
    return Math.max(0, Math.min(idx, lines.length - 1));
  }

  getLineIndexOfChar(charIndex: number): number {
    const lines = this._text.split("\n");
    let offset = 0;
    for (let i = 0; i < lines.length; i++) {
      if (charIndex < offset + lines[i].length + 1) return i;
      offset += lines[i].length + 1;
    }
    return lines.length - 1;
  }

  getLineLength(lineIndex: number): number {
    const lines = this._text.split("\n");
    return lines[lineIndex]?.length ?? 0;
  }

  getLineMetrics(lineIndex: number): any {
    const lineText = this.getLineText(lineIndex);
    const lineOffset = this.getLineOffset(lineIndex);
    const fmt = this.getTextFormat(lineOffset, lineOffset + Math.max(lineText.length, 1));
    const ctx = getMeasureCtx();
    ctx.font = _fontString(fmt, this.defaultTextFormat);
    const tm = ctx.measureText(lineText || "M");
    const ascent = tm.actualBoundingBoxAscent ?? (fmt.size ?? this.defaultTextFormat.size ?? 12) * 0.8;
    const descent = tm.actualBoundingBoxDescent ?? (fmt.size ?? this.defaultTextFormat.size ?? 12) * 0.2;
    const leading = fmt.leading ?? this.defaultTextFormat.leading ?? 0;
    const height = ascent + descent + leading;
    return { ascent, descent, height, leading, width: tm.width, x: 0 };
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

  static isFontCompatible(fontName: string, fontStyle: string): boolean {
    return Font.enumerateFonts().some(
      (f) => f.fontName === fontName && f.fontStyle === fontStyle,
    );
  }
}

// ---------------------------------------------------------------------------
// Font
// ---------------------------------------------------------------------------

export class Font {
  _fontName = "";
  _fontStyle = "regular";
  _fontType = "embedded";

  get fontName() { return this._fontName; }
  set fontName(v: string) { this._fontName = v; }
  get fontStyle() { return this._fontStyle; }
  set fontStyle(v: string) { this._fontStyle = v; }
  get fontType() { return this._fontType; }
  set fontType(v: string) { this._fontType = v; }

  /** @internal */
  static _registry: Font[] = [];

  hasGlyphs(_str: string): boolean {
    // Cannot reliably check without actual font tables; assume true.
    return true;
  }

  static enumerateFonts(_enumerateDeviceFonts = false): Font[] {
    return [...Font._registry];
  }

  static registerFont(fontClass: any): void {
    const instance = new fontClass();
    Font._registry.push(instance);
  }
}

// Register TextField as a timeline-child factory for display.ts auto-creation.
registerTimelineFactory("TextField", () => new TextField());
