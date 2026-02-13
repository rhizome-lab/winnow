/** Browser platform implementation — Canvas 2D, DOM events, image loading. */

let canvas: HTMLCanvasElement;
let ctx: CanvasRenderingContext2D;
let tcanvas: OffscreenCanvas | HTMLCanvasElement;
let tctx: CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D;

export function initCanvas(id: string): { canvas: HTMLCanvasElement; ctx: CanvasRenderingContext2D } {
  canvas = document.getElementById(id) as HTMLCanvasElement;
  ctx = canvas.getContext("2d")!;
  ctx.imageSmoothingEnabled = false;
  tcanvas = "OffscreenCanvas" in window
    ? new OffscreenCanvas(0, 0)
    : document.createElement("canvas");
  tctx = tcanvas.getContext("2d")!;
  return { canvas, ctx };
}

export function getCanvas(): HTMLCanvasElement { return canvas; }
export function getCtx(): CanvasRenderingContext2D { return ctx; }
export function getTintCanvas(): OffscreenCanvas | HTMLCanvasElement { return tcanvas; }
export function getTintCtx(): CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D { return tctx; }

export function resizeCanvas(w: number, h: number): void {
  canvas.width = w;
  canvas.height = h;
}

export function loadImage(src: string): Promise<HTMLImageElement> {
  return new Promise((resolve, reject) => {
    const img = new Image();
    img.onload = () => resolve(img);
    img.onerror = reject;
    img.src = src;
  });
}

export function onMouseMove(cb: (x: number, y: number) => void): void {
  canvas.addEventListener("mousemove", (e) => cb(e.offsetX, e.offsetY));
}

export function onMouseDown(cb: (button: number) => void): void {
  canvas.addEventListener("mousedown", (e) => cb(e.button));
  canvas.addEventListener("contextmenu", (e) => { e.preventDefault(); e.stopPropagation(); });
}

export function onMouseUp(cb: (button: number) => void): void {
  canvas.addEventListener("mouseup", (e) => cb(e.button));
}

export function onKeyDown(cb: (key: string, keyCode: number) => void): void {
  canvas.addEventListener("keydown", (e) => cb(e.key, e.keyCode));
}

export function onKeyUp(cb: (key: string, keyCode: number) => void): void {
  canvas.addEventListener("keyup", (e) => cb(e.key, e.keyCode));
}

export function scheduleFrame(cb: () => void, delay: number): number {
  return window.setTimeout(cb, delay) as unknown as number;
}

export function cancelFrame(handle: number): void {
  clearTimeout(handle);
}
