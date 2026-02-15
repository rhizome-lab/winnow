/** Browser images — bitmap creation from binary data. */

export function loadImageBitmap(
  source: Blob | ImageData,
): Promise<ImageBitmap> {
  return createImageBitmap(source);
}
