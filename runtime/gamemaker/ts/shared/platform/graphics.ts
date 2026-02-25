/** Browser graphics — Canvas 2D initialization and context management. */

import type { DocumentFactory } from "../render-root";

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

export function createCanvas(doc: DocumentFactory = document): HTMLCanvasElement {
  return doc.createElement("canvas") as HTMLCanvasElement;
}

export function resizeCanvas(gfx: GraphicsContext, w: number, h: number): void {
  gfx.canvas.width = w;
  gfx.canvas.height = h;
}
