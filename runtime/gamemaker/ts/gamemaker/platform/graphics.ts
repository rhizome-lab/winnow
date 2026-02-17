/** Browser graphics — Canvas 2D initialization and context management. */

export class GraphicsContext {
  canvas!: HTMLCanvasElement;
  ctx!: CanvasRenderingContext2D;
  tcanvas!: OffscreenCanvas | HTMLCanvasElement;
  tctx!: CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;
}

export function initCanvas(gfx: GraphicsContext, id: string): void {
  gfx.canvas = document.getElementById(id) as HTMLCanvasElement;
  gfx.ctx = gfx.canvas.getContext("2d")!;
  gfx.ctx.imageSmoothingEnabled = false;
  gfx.tcanvas = "OffscreenCanvas" in window
    ? new OffscreenCanvas(0, 0)
    : document.createElement("canvas");
  gfx.tctx = gfx.tcanvas.getContext("2d")!;
}

export function resizeCanvas(gfx: GraphicsContext, w: number, h: number): void {
  gfx.canvas.width = w;
  gfx.canvas.height = h;
}
