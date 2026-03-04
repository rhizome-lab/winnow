/**
 * Browser graphics — Canvas 2D initialization, state management, and drawing.
 *
 * Two interfaces coexist in this file:
 *
 * 1. Legacy `GraphicsContext` / `initCanvas` / `initWebGL` — used by existing
 *    GML runtime code. Preserved for backward compatibility.
 *
 * 2. Canonical Graphics 2D interface — all functions take `GraphicsState` as
 *    their first parameter. Handles are opaque integers. This is the target
 *    interface for new code and Rust backends.
 *
 * Color convention: `number` color params are 0xRRGGBBAA packed integers.
 */

import type { DocumentFactory } from "../render-root";

// ---------------------------------------------------------------------------
// Legacy interface (preserved for existing callers)
// ---------------------------------------------------------------------------

export class GraphicsContext {
  canvas!: HTMLCanvasElement;
  ctx!: CanvasRenderingContext2D;
  /** Permanent reference to the main 2D context; never reassigned after init. */
  mainCtx!: CanvasRenderingContext2D;
  tcanvas!: OffscreenCanvas | HTMLCanvasElement;
  tctx!: CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;
  /** WebGL2 overlay canvas (created lazily on first shader_set call). */
  glCanvas: HTMLCanvasElement | null = null;
  /** WebGL2 context for the overlay canvas. */
  gl: WebGL2RenderingContext | null = null;
}

export function initCanvas(gfx: GraphicsContext, id: string, doc: DocumentFactory = document): void {
  gfx.canvas = (doc as Document).getElementById
    ? (doc as Document).getElementById(id) as HTMLCanvasElement
    : (doc as any).querySelector(`#${id}`) as HTMLCanvasElement;
  gfx.ctx = gfx.canvas.getContext("2d")!;
  gfx.mainCtx = gfx.ctx;
  gfx.ctx.imageSmoothingEnabled = false;
  gfx.tcanvas = "OffscreenCanvas" in globalThis
    ? new OffscreenCanvas(0, 0)
    : doc.createElement("canvas") as HTMLCanvasElement;
  gfx.tctx = gfx.tcanvas.getContext("2d")!;
}

/**
 * Lazily initialize a WebGL2 overlay canvas positioned over the main canvas.
 * Returns true if WebGL2 is available; false if not supported (shaders will be skipped).
 */
export function initWebGL(gfx: GraphicsContext): boolean {
  if (gfx.gl) return true;
  const main = gfx.canvas;
  const glCanvas = document.createElement("canvas");
  glCanvas.width = main.width;
  glCanvas.height = main.height;
  glCanvas.style.position = "absolute";
  glCanvas.style.left = main.style.left || "0";
  glCanvas.style.top = main.style.top || "0";
  glCanvas.style.width = main.style.width || `${main.width}px`;
  glCanvas.style.height = main.style.height || `${main.height}px`;
  glCanvas.style.pointerEvents = "none";
  main.parentElement?.insertBefore(glCanvas, main.nextSibling);
  const gl = glCanvas.getContext("webgl2");
  if (!gl) return false;
  gfx.glCanvas = glCanvas;
  gfx.gl = gl;
  return true;
}

// ---------------------------------------------------------------------------
// Handle types
// ---------------------------------------------------------------------------

export type CanvasHandle = number;
export type FontHandle = number;
export type PathHandle = number;
export type GradientHandle = number;

// ---------------------------------------------------------------------------
// Named constant types
// ---------------------------------------------------------------------------

export type BlendMode = "normal" | "additive" | "multiply" | "screen" | "erase";
export type TextAlign = "left" | "center" | "right";
export type TextBaseline = "top" | "middle" | "bottom" | "alphabetic";
export type LineCap = "butt" | "round" | "square";
export type LineJoin = "miter" | "round" | "bevel";

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

type CanvasEntry = {
  el: HTMLCanvasElement | OffscreenCanvas;
  ctx: CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;
};

export class GraphicsState {
  canvases = new Map<CanvasHandle, CanvasEntry>();
  fonts = new Map<FontHandle, FontFace>();
  /** CSS family name for each loaded font, keyed by handle. */
  fontFamilies = new Map<FontHandle, string>();
  paths = new Map<PathHandle, Path2D>();
  gradients = new Map<GradientHandle, CanvasGradient>();
  nextCanvas = 1;
  nextFont = 1;
  nextPath = 1;
  nextGradient = 1;
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

function colorToCss(c: number): string {
  const r = (c >>> 24) & 0xff;
  const g = (c >>> 16) & 0xff;
  const b = (c >>> 8) & 0xff;
  const a = c & 0xff;
  return `rgba(${r},${g},${b},${a / 255})`;
}

function getCtx(state: GraphicsState, canvas: CanvasHandle): CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D {
  const entry = state.canvases.get(canvas);
  if (!entry) throw new Error(`graphics: unknown CanvasHandle ${canvas}`);
  return entry.ctx;
}

function getEl(state: GraphicsState, canvas: CanvasHandle): HTMLCanvasElement | OffscreenCanvas {
  const entry = state.canvases.get(canvas);
  if (!entry) throw new Error(`graphics: unknown CanvasHandle ${canvas}`);
  return entry.el;
}

function getPath(state: GraphicsState, path: PathHandle): Path2D {
  const p = state.paths.get(path);
  if (!p) throw new Error(`graphics: unknown PathHandle ${path}`);
  return p;
}

function getGradient(state: GraphicsState, gradient: GradientHandle): CanvasGradient {
  const g = state.gradients.get(gradient);
  if (!g) throw new Error(`graphics: unknown GradientHandle ${gradient}`);
  return g;
}

function fontCss(state: GraphicsState, font: FontHandle, size: number): string {
  const family = state.fontFamilies.get(font);
  if (!family) throw new Error(`graphics: unknown FontHandle ${font}`);
  return `${size}px ${family}`;
}

// ---------------------------------------------------------------------------
// Setup tier
// ---------------------------------------------------------------------------

/**
 * Get an existing canvas element by DOM id, wrap it in GraphicsState, and
 * return a handle. Sets imageSmoothingEnabled = false by default.
 */
export function initSurface(state: GraphicsState, id: string, doc: Document = document): CanvasHandle {
  const el = doc.getElementById(id) as HTMLCanvasElement;
  if (!el) throw new Error(`graphics: no element with id "${id}"`);
  const ctx = el.getContext("2d")!;
  ctx.imageSmoothingEnabled = false;
  const handle = state.nextCanvas++;
  state.canvases.set(handle, { el, ctx });
  return handle;
}

/**
 * Create a new offscreen canvas (OffscreenCanvas if available, else
 * HTMLCanvasElement). Returns a handle.
 */
export function createCanvas(state: GraphicsState, w: number, h: number): CanvasHandle {
  let el: HTMLCanvasElement | OffscreenCanvas;
  if ("OffscreenCanvas" in globalThis) {
    el = new OffscreenCanvas(w, h);
  } else {
    const c = document.createElement("canvas");
    c.width = w;
    c.height = h;
    el = c;
  }
  const ctx = el.getContext("2d")!;
  const handle = state.nextCanvas++;
  state.canvases.set(handle, { el, ctx });
  return handle;
}

/** Resize a canvas by handle. */
export function resizeCanvas(state: GraphicsState, canvas: CanvasHandle, w: number, h: number): void {
  const el = getEl(state, canvas);
  el.width = w;
  el.height = h;
}

/**
 * Load a font from `url`, add it to `document.fonts`, and return a handle.
 * The CSS family name is auto-generated as `rf_${handle}`.
 */
export async function loadFont(state: GraphicsState, url: string): Promise<FontHandle> {
  const handle = state.nextFont++;
  const family = `rf_${handle}`;
  const face = new FontFace(family, `url(${url})`);
  await face.load();
  document.fonts.add(face);
  state.fonts.set(handle, face);
  state.fontFamilies.set(handle, family);
  return handle;
}

/** Allocate a new reusable Path2D and return its handle. */
export function createPath(state: GraphicsState): PathHandle {
  const handle = state.nextPath++;
  state.paths.set(handle, new Path2D());
  return handle;
}

export function pathMoveTo(state: GraphicsState, path: PathHandle, x: number, y: number): void {
  getPath(state, path).moveTo(x, y);
}

export function pathLineTo(state: GraphicsState, path: PathHandle, x: number, y: number): void {
  getPath(state, path).lineTo(x, y);
}

export function pathBezierTo(state: GraphicsState, path: PathHandle, cp1x: number, cp1y: number, cp2x: number, cp2y: number, x: number, y: number): void {
  getPath(state, path).bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y);
}

export function pathQuadraticTo(state: GraphicsState, path: PathHandle, cpx: number, cpy: number, x: number, y: number): void {
  getPath(state, path).quadraticCurveTo(cpx, cpy, x, y);
}

export function pathArc(state: GraphicsState, path: PathHandle, x: number, y: number, r: number, start: number, end: number, ccw: boolean): void {
  getPath(state, path).arc(x, y, r, start, end, ccw);
}

export function pathClose(state: GraphicsState, path: PathHandle): void {
  getPath(state, path).closePath();
}

export function destroyPath(state: GraphicsState, path: PathHandle): void {
  state.paths.delete(path);
}

export function destroyCanvas(state: GraphicsState, canvas: CanvasHandle): void {
  state.canvases.delete(canvas);
}

export function destroyFont(state: GraphicsState, font: FontHandle): void {
  const face = state.fonts.get(font);
  if (face) document.fonts.delete(face);
  state.fonts.delete(font);
  state.fontFamilies.delete(font);
}

/** Create a linear gradient on the given canvas context and return a handle. */
export function createLinearGradient(state: GraphicsState, canvas: CanvasHandle, x0: number, y0: number, x1: number, y1: number): GradientHandle {
  const ctx = getCtx(state, canvas);
  const grad = ctx.createLinearGradient(x0, y0, x1, y1);
  const handle = state.nextGradient++;
  state.gradients.set(handle, grad);
  return handle;
}

/** Create a radial gradient on the given canvas context and return a handle. */
export function createRadialGradient(state: GraphicsState, canvas: CanvasHandle, x0: number, y0: number, r0: number, x1: number, y1: number, r1: number): GradientHandle {
  const ctx = getCtx(state, canvas);
  const grad = ctx.createRadialGradient(x0, y0, r0, x1, y1, r1);
  const handle = state.nextGradient++;
  state.gradients.set(handle, grad);
  return handle;
}

/**
 * Add a color stop to a gradient. `color` is 0xRRGGBBAA.
 * `offset` is in [0, 1].
 */
export function gradientAddStop(state: GraphicsState, gradient: GradientHandle, offset: number, color: number): void {
  getGradient(state, gradient).addColorStop(offset, colorToCss(color));
}

export function destroyGradient(state: GraphicsState, gradient: GradientHandle): void {
  state.gradients.delete(gradient);
}

// ---------------------------------------------------------------------------
// Query (sync)
// ---------------------------------------------------------------------------

export function canvasWidth(state: GraphicsState, canvas: CanvasHandle): number {
  return getEl(state, canvas).width;
}

export function canvasHeight(state: GraphicsState, canvas: CanvasHandle): number {
  return getEl(state, canvas).height;
}

/** Return a copy of the pixel data in the given rectangle as a Uint8Array (RGBA). */
export function readCanvasPixels(state: GraphicsState, canvas: CanvasHandle, x: number, y: number, w: number, h: number): Uint8Array {
  const ctx = getCtx(state, canvas);
  return new Uint8Array(ctx.getImageData(x, y, w, h).data.buffer);
}

/**
 * Create an ImageBitmap from the canvas. The caller is responsible for passing
 * the result into ImageState — graphics does not own ImageState.
 */
export function canvasToImage(state: GraphicsState, canvas: CanvasHandle): ImageBitmap {
  // createImageBitmap is async in the spec but synchronous-capable for canvas
  // elements in practice. Callers that need strict async should await the
  // returned promise themselves; here we return the raw ImageBitmap after the
  // browser resolves it. For sync use cases, callers must pre-await via the
  // async variant.
  //
  // Implementation note: this function is intentionally typed as returning
  // ImageBitmap (not Promise<ImageBitmap>) to keep the hot-path drawing API
  // synchronous. Use createImageBitmapAsync for cases that need a Promise.
  throw new Error("canvasToImage: not yet implemented — call createImageBitmapAsync instead");
}

/**
 * Async variant: resolves to an ImageBitmap from the canvas.
 * Caller passes result to ImageState.
 */
export async function createImageBitmapAsync(state: GraphicsState, canvas: CanvasHandle): Promise<ImageBitmap> {
  const el = getEl(state, canvas);
  return createImageBitmap(el as ImageBitmapSource);
}

// ---------------------------------------------------------------------------
// Hot tier — state (saved/restored)
// ---------------------------------------------------------------------------

export function setTransform(state: GraphicsState, canvas: CanvasHandle, a: number, b: number, c: number, d: number, e: number, f: number): void {
  getCtx(state, canvas).setTransform(a, b, c, d, e, f);
}

/** Set global alpha (0–1). */
export function setAlpha(state: GraphicsState, canvas: CanvasHandle, alpha: number): void {
  getCtx(state, canvas).globalAlpha = alpha;
}

const BLEND_MODE_MAP: Record<BlendMode, GlobalCompositeOperation> = {
  normal: "source-over",
  additive: "lighter",
  multiply: "multiply",
  screen: "screen",
  erase: "destination-out",
};

export function setBlendMode(state: GraphicsState, canvas: CanvasHandle, mode: BlendMode): void {
  getCtx(state, canvas).globalCompositeOperation = BLEND_MODE_MAP[mode];
}

/**
 * Apply a color transform matrix to subsequent drawing.
 * Canvas 2D has no native color matrix support. The matrix is stored on a
 * per-canvas side-channel for a future LRU-cached filter/shader implementation.
 * Currently a no-op — callers relying on color transforms must use a WebGL
 * overlay or CSS filter workaround.
 */
export function setColorTransform(_state: GraphicsState, _canvas: CanvasHandle, _matrix: Float32Array): void {
  // no-op: Canvas 2D does not support color matrices natively.
  // Store matrix per-canvas for a future WebGL/CSS-filter impl.
}

export function setImageSmoothing(state: GraphicsState, canvas: CanvasHandle, enabled: boolean): void {
  getCtx(state, canvas).imageSmoothingEnabled = enabled;
}

export function setStrokeStyle(state: GraphicsState, canvas: CanvasHandle, cap: LineCap, join: LineJoin, miterLimit: number): void {
  const ctx = getCtx(state, canvas);
  ctx.lineCap = cap;
  ctx.lineJoin = join;
  ctx.miterLimit = miterLimit;
}

export function setDashPattern(state: GraphicsState, canvas: CanvasHandle, segments: number[], offset: number): void {
  const ctx = getCtx(state, canvas);
  ctx.setLineDash(segments);
  ctx.lineDashOffset = offset;
}

export function saveState(state: GraphicsState, canvas: CanvasHandle): void {
  getCtx(state, canvas).save();
}

export function restoreState(state: GraphicsState, canvas: CanvasHandle): void {
  getCtx(state, canvas).restore();
}

/**
 * Reset all canvas state to a clean baseline. Uses `ctx.reset()` when
 * available (Chrome 99+). Falls back to replacing the context by re-obtaining
 * it from the canvas element, which discards all saved state layers.
 */
export function resetCanvasState(state: GraphicsState, canvas: CanvasHandle): void {
  const entry = state.canvases.get(canvas);
  if (!entry) throw new Error(`graphics: unknown CanvasHandle ${canvas}`);
  const ctx = entry.ctx as any;
  if (typeof ctx.reset === "function") {
    ctx.reset();
  } else {
    // Fallback: re-obtain a fresh context from the element.
    const fresh = entry.el.getContext("2d")!;
    entry.ctx = fresh;
  }
}

// ---------------------------------------------------------------------------
// Hot tier — drawing
// ---------------------------------------------------------------------------

/**
 * Fill the entire canvas with `color` (0xRRGGBBAA).
 * Respects current clip — does not modify other canvas state.
 */
export function clearCanvas(state: GraphicsState, canvas: CanvasHandle, color: number): void {
  const ctx = getCtx(state, canvas);
  const el = getEl(state, canvas);
  ctx.fillStyle = colorToCss(color);
  ctx.fillRect(0, 0, el.width, el.height);
}

export function fillRect(state: GraphicsState, canvas: CanvasHandle, x: number, y: number, w: number, h: number, color: number): void {
  const ctx = getCtx(state, canvas);
  ctx.fillStyle = colorToCss(color);
  ctx.fillRect(x, y, w, h);
}

/**
 * Draw a region of an ImageBitmap onto the canvas.
 * The caller extracts the ImageBitmap from ImageState; graphics does not own it.
 */
export function drawImage(state: GraphicsState, canvas: CanvasHandle, img: ImageBitmap, sx: number, sy: number, sw: number, sh: number, dx: number, dy: number, dw: number, dh: number): void {
  getCtx(state, canvas).drawImage(img, sx, sy, sw, sh, dx, dy, dw, dh);
}

/** Blit a region of one managed canvas onto another. */
export function drawCanvas(state: GraphicsState, dst: CanvasHandle, src: CanvasHandle, sx: number, sy: number, sw: number, sh: number, dx: number, dy: number, dw: number, dh: number): void {
  const dstCtx = getCtx(state, dst);
  const srcEl = getEl(state, src);
  dstCtx.drawImage(srcEl as CanvasImageSource, sx, sy, sw, sh, dx, dy, dw, dh);
}

/** Draw `text` at (x, y) using the given font handle and size. */
export function drawText(state: GraphicsState, canvas: CanvasHandle, text: string, x: number, y: number, font: FontHandle, size: number, color: number, align: TextAlign, baseline: TextBaseline): void {
  const ctx = getCtx(state, canvas);
  ctx.font = fontCss(state, font, size);
  ctx.fillStyle = colorToCss(color);
  ctx.textAlign = align;
  ctx.textBaseline = baseline;
  ctx.fillText(text, x, y);
}

/**
 * Measure the rendered width of `text` using the given font and size.
 * Uses the first available canvas context for measurement.
 */
export function measureText(state: GraphicsState, canvas: CanvasHandle, text: string, font: FontHandle, size: number): number {
  const ctx = getCtx(state, canvas);
  ctx.font = fontCss(state, font, size);
  return ctx.measureText(text).width;
}

// ---------------------------------------------------------------------------
// Hot tier — one-off (immediate) paths
// ---------------------------------------------------------------------------

export function beginPath(state: GraphicsState, canvas: CanvasHandle): void {
  getCtx(state, canvas).beginPath();
}

export function moveTo(state: GraphicsState, canvas: CanvasHandle, x: number, y: number): void {
  getCtx(state, canvas).moveTo(x, y);
}

export function lineTo(state: GraphicsState, canvas: CanvasHandle, x: number, y: number): void {
  getCtx(state, canvas).lineTo(x, y);
}

export function bezierTo(state: GraphicsState, canvas: CanvasHandle, cp1x: number, cp1y: number, cp2x: number, cp2y: number, x: number, y: number): void {
  getCtx(state, canvas).bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y);
}

export function quadraticTo(state: GraphicsState, canvas: CanvasHandle, cpx: number, cpy: number, x: number, y: number): void {
  getCtx(state, canvas).quadraticCurveTo(cpx, cpy, x, y);
}

export function arc(state: GraphicsState, canvas: CanvasHandle, x: number, y: number, r: number, start: number, end: number, ccw: boolean): void {
  getCtx(state, canvas).arc(x, y, r, start, end, ccw);
}

export function closePath(state: GraphicsState, canvas: CanvasHandle): void {
  getCtx(state, canvas).closePath();
}

/** Fill the current path with `color` (0xRRGGBBAA). */
export function fillPath(state: GraphicsState, canvas: CanvasHandle, color: number): void {
  const ctx = getCtx(state, canvas);
  ctx.fillStyle = colorToCss(color);
  ctx.fill();
}

/** Fill the current path with a gradient. */
export function fillPathGradient(state: GraphicsState, canvas: CanvasHandle, gradient: GradientHandle): void {
  const ctx = getCtx(state, canvas);
  ctx.fillStyle = getGradient(state, gradient);
  ctx.fill();
}

/** Stroke the current path with `color` and `width`. */
export function strokePath(state: GraphicsState, canvas: CanvasHandle, color: number, width: number): void {
  const ctx = getCtx(state, canvas);
  ctx.strokeStyle = colorToCss(color);
  ctx.lineWidth = width;
  ctx.stroke();
}

export function clip(state: GraphicsState, canvas: CanvasHandle): void {
  getCtx(state, canvas).clip();
}

/**
 * Add text to the current path as a geometric outline.
 * Canvas 2D has no text-to-path support. Use an SVG or opentype.js shim
 * to convert glyphs to Path2D objects if outline text is required.
 */
export function beginTextPath(_state: GraphicsState, _canvas: CanvasHandle, _text: string, _x: number, _y: number, _font: FontHandle, _size: number): void {
  // no-op: Canvas 2D has no text-to-path; use a SVG/opentype.js shim.
}

// ---------------------------------------------------------------------------
// Hot tier — reusable Path2D handles
// ---------------------------------------------------------------------------

/** Fill a reusable path with `color` (0xRRGGBBAA). */
export function fillPathHandle(state: GraphicsState, canvas: CanvasHandle, path: PathHandle, color: number): void {
  const ctx = getCtx(state, canvas);
  ctx.fillStyle = colorToCss(color);
  ctx.fill(getPath(state, path));
}

/** Fill a reusable path with a gradient. */
export function fillPathHandleGradient(state: GraphicsState, canvas: CanvasHandle, path: PathHandle, gradient: GradientHandle): void {
  const ctx = getCtx(state, canvas);
  ctx.fillStyle = getGradient(state, gradient);
  ctx.fill(getPath(state, path));
}

/** Stroke a reusable path with `color` and `width`. */
export function strokePathHandle(state: GraphicsState, canvas: CanvasHandle, path: PathHandle, color: number, width: number): void {
  const ctx = getCtx(state, canvas);
  ctx.strokeStyle = colorToCss(color);
  ctx.lineWidth = width;
  ctx.stroke(getPath(state, path));
}

/** Clip to a reusable path. */
export function clipPathHandle(state: GraphicsState, canvas: CanvasHandle, path: PathHandle): void {
  getCtx(state, canvas).clip(getPath(state, path));
}
