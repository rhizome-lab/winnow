/**
 * Browser platform implementation — Canvas 2D, DOM events, fetch,
 * localStorage, timers, image loading.
 *
 * This is the concrete platform for browser deployment. To swap platforms
 * (e.g., for testing or a WebGL backend), change the re-export in index.ts.
 */

// ---------------------------------------------------------------------------
// Graphics — canvas creation and 2D context
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Input — DOM event binding and coordinate conversion
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Network — HTTP resource loading
// ---------------------------------------------------------------------------

export function fetchResource(
  url: string,
  options?: { method?: string; signal?: AbortSignal },
): Promise<Response> {
  return globalThis.fetch(url, options);
}

export function hasFetch(): boolean {
  return typeof globalThis.fetch === "function";
}

// ---------------------------------------------------------------------------
// Persistence — key-value storage
// ---------------------------------------------------------------------------

export function loadLocal(key: string): string | null {
  if (typeof localStorage === "undefined") return null;
  return localStorage.getItem(key);
}

export function saveLocal(key: string, value: string): void {
  if (typeof localStorage === "undefined") return;
  localStorage.setItem(key, value);
}

export function removeLocal(key: string): void {
  if (typeof localStorage === "undefined") return;
  localStorage.removeItem(key);
}

// ---------------------------------------------------------------------------
// Timing — interval scheduling
// ---------------------------------------------------------------------------

export function scheduleInterval(
  callback: () => void,
  ms: number,
): number {
  return setInterval(callback, ms) as unknown as number;
}

export function cancelScheduledInterval(id: number): void {
  clearInterval(id);
}

// ---------------------------------------------------------------------------
// Images — bitmap creation from binary data
// ---------------------------------------------------------------------------

export function loadImageBitmap(
  source: Blob | ImageData,
): Promise<ImageBitmap> {
  return createImageBitmap(source);
}

// ---------------------------------------------------------------------------
// Files — download trigger (FileReference.save)
// ---------------------------------------------------------------------------

export function triggerDownload(blob: Blob, filename: string): void {
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  a.click();
  URL.revokeObjectURL(url);
}
