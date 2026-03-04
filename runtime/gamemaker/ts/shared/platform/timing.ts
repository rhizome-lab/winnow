/** Browser timing — schedule callbacks, drive vsync, and read monotonic/wall clocks. */

// Branded handle types prevent accidentally mixing timeout, interval, and rAF handles.
export type DelayHandle = number & { readonly __brand: unique symbol };
export type RecurringHandle = number & { readonly __brand: unique symbol };
export type FrameHandle = number & { readonly __brand: unique symbol };

/** Schedule a one-shot callback to fire after `delayMs` milliseconds. */
export function scheduleDelayed(cb: () => void, delayMs: number): DelayHandle {
  return setTimeout(cb, delayMs) as unknown as DelayHandle;
}

/** Cancel a pending one-shot callback. No-op if already fired or handle is invalid. */
export function cancelDelayed(handle: DelayHandle): void {
  clearTimeout(handle);
}

/** Schedule a repeating callback to fire every `intervalMs` milliseconds until cancelled. */
export function scheduleRecurring(cb: () => void, intervalMs: number): RecurringHandle {
  return setInterval(cb, intervalMs) as unknown as RecurringHandle;
}

/** Cancel a repeating callback. */
export function cancelRecurring(handle: RecurringHandle): void {
  clearInterval(handle);
}

/**
 * Request a vsync-aligned frame callback. Fires every frame until cancelled.
 * `timeMs` is the DOMHighResTimeStamp passed by requestAnimationFrame — monotonic,
 * suitable for delta computation.
 */
export function requestFrame(cb: (timeMs: number) => void): FrameHandle {
  return requestAnimationFrame(cb) as unknown as FrameHandle;
}

/** Cancel a pending frame callback. */
export function cancelFrame(handle: FrameHandle): void {
  cancelAnimationFrame(handle);
}

/** Monotonic clock in milliseconds (performance.now()). Use for game timing and delta computation. */
export function currentTimeMs(): number {
  return performance.now();
}

/** Wall clock in milliseconds (Date.now()). Use for save file timestamps and real-world time. */
export function currentWallTimeMs(): number {
  return Date.now();
}
