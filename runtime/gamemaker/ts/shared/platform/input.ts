/** Browser input — mouse and keyboard event binding. */

export function onMouseMove(canvas: HTMLCanvasElement, cb: (x: number, y: number) => void): void {
  canvas.addEventListener("mousemove", (e) => cb(e.offsetX, e.offsetY));
}

export function onMouseDown(canvas: HTMLCanvasElement, cb: (button: number) => void): void {
  canvas.addEventListener("mousedown", (e) => cb(e.button));
  canvas.addEventListener("contextmenu", (e) => { e.preventDefault(); e.stopPropagation(); });
}

export function onMouseUp(canvas: HTMLCanvasElement, cb: (button: number) => void): void {
  canvas.addEventListener("mouseup", (e) => cb(e.button));
}

export function onKeyDown(canvas: HTMLCanvasElement, cb: (key: string, keyCode: number) => void): void {
  canvas.addEventListener("keydown", (e) => cb(e.key, e.keyCode));
}

export function onKeyUp(canvas: HTMLCanvasElement, cb: (key: string, keyCode: number) => void): void {
  canvas.addEventListener("keyup", (e) => cb(e.key, e.keyCode));
}

export function onScroll(canvas: HTMLCanvasElement, cb: (delta: number) => void): void {
  canvas.addEventListener("wheel", (e) => { e.preventDefault(); cb(e.deltaY); }, { passive: false });
}
