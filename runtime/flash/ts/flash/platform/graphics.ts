/** Browser graphics — canvas creation and 2D context. */

export function initCanvas(id: string): {
  canvas: HTMLCanvasElement;
  ctx: CanvasRenderingContext2D;
} {
  const canvas = document.getElementById(id) as HTMLCanvasElement;
  const ctx = canvas.getContext("2d")!;
  return { canvas, ctx };
}

export function createMeasureContext(): CanvasRenderingContext2D {
  const c = document.createElement("canvas");
  return c.getContext("2d")!;
}
