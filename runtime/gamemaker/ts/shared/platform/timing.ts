/** Browser timing — timeout scheduling via setTimeout. */

export function scheduleTimeout(cb: () => void, delay: number): number {
  return window.setTimeout(cb, delay) as unknown as number;
}

export function cancelTimeout(handle: number): void {
  clearTimeout(handle);
}
