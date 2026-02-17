/** Browser graphics — Canvas 2D initialization and context management. */

import type { DocumentFactory } from "../../../../shared/ts/render-root";

export class GraphicsContext {
  canvas!: HTMLCanvasElement;
  ctx!: CanvasRenderingContext2D;
  tcanvas!: OffscreenCanvas | HTMLCanvasElement;
  tctx!: CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;
}

export function initCanvas(gfx: GraphicsContext, id: string, doc: DocumentFactory = document): void {
  gfx.canvas = (doc as Document).getElementById
    ? (doc as Document).getElementById(id) as HTMLCanvasElement
    : (doc as any).querySelector(`#${id}`) as HTMLCanvasElement;
  gfx.ctx = gfx.canvas.getContext("2d")!;
  gfx.ctx.imageSmoothingEnabled = false;
  gfx.tcanvas = "OffscreenCanvas" in globalThis
    ? new OffscreenCanvas(0, 0)
    : doc.createElement("canvas") as HTMLCanvasElement;
  gfx.tctx = gfx.tcanvas.getContext("2d")!;
}

export function createCanvas(doc: DocumentFactory = document): HTMLCanvasElement {
  return doc.createElement("canvas") as HTMLCanvasElement;
}

export function resizeCanvas(gfx: GraphicsContext, w: number, h: number): void {
  gfx.canvas.width = w;
  gfx.canvas.height = h;
}
