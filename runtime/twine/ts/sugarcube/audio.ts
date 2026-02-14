/** SugarCube audio macro stubs.
 *
 * Dynamic method dispatch — the method name comes from the macro name
 * (cacheaudio, audio, masteraudio, etc.). Uses an index signature so
 * any method name can be called.
 */

export function cacheaudio(...args: any[]): void {
  console.log("[audio:cacheaudio]", ...args);
}

export function audio(...args: any[]): void {
  console.log("[audio:audio]", ...args);
}

export function masteraudio(...args: any[]): void {
  console.log("[audio:masteraudio]", ...args);
}

export function playlist(...args: any[]): void {
  console.log("[audio:playlist]", ...args);
}

export function removeaudio(...args: any[]): void {
  console.log("[audio:removeaudio]", ...args);
}

export function waitforaudio(...args: any[]): void {
  console.log("[audio:waitforaudio]", ...args);
}

export function createaudiogroup(...args: any[]): void {
  console.log("[audio:createaudiogroup]", ...args);
}

export function createplaylist(...args: any[]): void {
  console.log("[audio:createplaylist]", ...args);
}

export function removeaudiogroup(...args: any[]): void {
  console.log("[audio:removeaudiogroup]", ...args);
}

export function removeplaylist(...args: any[]): void {
  console.log("[audio:removeplaylist]", ...args);
}
