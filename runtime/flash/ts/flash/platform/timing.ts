/** Browser timing — interval scheduling. */

export function scheduleInterval(
  callback: () => void,
  ms: number,
): number {
  return setInterval(callback, ms) as unknown as number;
}

export function cancelScheduledInterval(id: number): void {
  clearInterval(id);
}
