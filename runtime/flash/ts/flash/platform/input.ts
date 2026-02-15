/** Browser input — DOM event binding and coordinate conversion. */

export function addCanvasEventListener(
  canvas: HTMLCanvasElement,
  type: string,
  handler: (e: any) => void,
): void {
  canvas.addEventListener(type, handler);
}

export function addDocumentEventListener(
  type: string,
  handler: (e: any) => void,
): void {
  document.addEventListener(type, handler);
}

export function getCanvasBounds(canvas: HTMLCanvasElement): DOMRect {
  return canvas.getBoundingClientRect();
}
