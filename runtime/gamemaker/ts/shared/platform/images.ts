/** Browser images — ImageBitmap-based image resource management. */

export type ImageHandle = number;

interface ImageEntry {
  bitmap: ImageBitmap;
  x: number;
  y: number;
  w: number;
  h: number;
  refCount: number;
  parent?: ImageHandle;
  pendingDestroy?: boolean;
}

export class ImageState {
  private entries = new Map<ImageHandle, ImageEntry>();
  private nextHandle = 1;

  allocate(entry: ImageEntry): ImageHandle {
    const handle = this.nextHandle++;
    this.entries.set(handle, entry);
    return handle;
  }

  get(handle: ImageHandle): ImageEntry | undefined {
    return this.entries.get(handle);
  }

  release(handle: ImageHandle): void {
    this.entries.delete(handle);
  }
}

export async function createImage(state: ImageState, w: number, h: number): Promise<ImageHandle> {
  const bitmap = await createImageBitmap(new ImageData(w, h));
  return state.allocate({ bitmap, x: 0, y: 0, w, h, refCount: 0 });
}

export async function loadImageUrl(state: ImageState, url: string): Promise<ImageHandle> {
  const blob = await fetch(url).then(r => r.blob());
  const bitmap = await createImageBitmap(blob);
  return state.allocate({ bitmap, x: 0, y: 0, w: bitmap.width, h: bitmap.height, refCount: 0 });
}

export async function loadImageBytes(state: ImageState, data: Uint8Array<ArrayBuffer>, format: string | null): Promise<ImageHandle> {
  const mime = format ?? "image/png";
  const blob = new Blob([data], { type: mime });
  const bitmap = await createImageBitmap(blob);
  return state.allocate({ bitmap, x: 0, y: 0, w: bitmap.width, h: bitmap.height, refCount: 0 });
}

export function createSubImage(state: ImageState, parent: ImageHandle, x: number, y: number, w: number, h: number): ImageHandle {
  const parentEntry = state.get(parent);
  if (!parentEntry) throw new Error(`createSubImage: invalid parent handle ${parent}`);
  parentEntry.refCount++;
  return state.allocate({ bitmap: parentEntry.bitmap, x: parentEntry.x + x, y: parentEntry.y + y, w, h, refCount: 0, parent });
}

export function imageWidth(state: ImageState, handle: ImageHandle): number {
  const entry = state.get(handle);
  if (!entry) throw new Error(`imageWidth: invalid handle ${handle}`);
  return entry.w;
}

export function imageHeight(state: ImageState, handle: ImageHandle): number {
  const entry = state.get(handle);
  if (!entry) throw new Error(`imageHeight: invalid handle ${handle}`);
  return entry.h;
}

export function readPixels(state: ImageState, handle: ImageHandle, x: number, y: number, w: number, h: number): Uint8Array {
  const entry = state.get(handle);
  if (!entry) throw new Error(`readPixels: invalid handle ${handle}`);
  const canvas = new OffscreenCanvas(w, h);
  const ctx = canvas.getContext("2d");
  if (!ctx) throw new Error("readPixels: failed to get 2D context");
  ctx.drawImage(entry.bitmap, entry.x + x, entry.y + y, w, h, 0, 0, w, h);
  const imageData = ctx.getImageData(0, 0, w, h);
  return new Uint8Array(imageData.data.buffer);
}

export function writePixels(state: ImageState, handle: ImageHandle, x: number, y: number, w: number, h: number, data: Uint8Array): void {
  const entry = state.get(handle);
  if (!entry) throw new Error(`writePixels: invalid handle ${handle}`);
  if (entry.parent !== undefined) throw new Error("writePixels: cannot write to sub-image view");
  const canvas = new OffscreenCanvas(entry.w, entry.h);
  const ctx = canvas.getContext("2d");
  if (!ctx) throw new Error("writePixels: unsupported");
  ctx.drawImage(entry.bitmap, 0, 0);
  const imageData = new ImageData(new Uint8ClampedArray(data.buffer, data.byteOffset, data.byteLength), w, h);
  ctx.putImageData(imageData, x, y);
  const newBitmap = canvas.transferToImageBitmap();
  if (!newBitmap) throw new Error("writePixels: unsupported");
  entry.bitmap.close();
  entry.bitmap = newBitmap;
}

export function destroyImage(state: ImageState, handle: ImageHandle): void {
  const entry = state.get(handle);
  if (!entry) return;

  if (entry.parent !== undefined) {
    // Sub-image: decrement parent refCount.
    state.release(handle);
    const parentEntry = state.get(entry.parent);
    if (parentEntry) {
      parentEntry.refCount--;
      if (parentEntry.refCount === 0 && parentEntry.pendingDestroy) {
        parentEntry.bitmap.close();
        state.release(entry.parent);
      }
    }
  } else {
    // Root image.
    if (entry.refCount > 0) {
      entry.pendingDestroy = true;
    } else {
      entry.bitmap.close();
      state.release(handle);
    }
  }
}
