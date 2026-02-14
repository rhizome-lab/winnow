/** SugarCube output rendering stubs.
 *
 * These functions are called by translated passage code to produce output.
 * The actual rendering integration with a SugarCube-compatible UI is a
 * future concern — for now these log to console for verification.
 */

/** Emit plain text. */
export function text(s: string): void {
  console.log("[text]", s);
}

/** Print a value (<<print expr>>). */
export function print(v: any): void {
  console.log("[print]", v);
}

/** Emit raw HTML. */
export function html(s: string): void {
  console.log("[html]", s);
}

/** Emit a line break. */
// Using a name that avoids JS reserved word conflicts in import context.
export { lineBreak as break };
function lineBreak(): void {
  console.log("[break]");
}

/** Emit a link (<<link>>). */
export function link(variant: string, text: string, passage?: string): void {
  console.log("[link]", variant, text, passage);
}

/** Start a link block (<<link>> with body). */
export function link_block_start(variant: string, text: string, passage?: string): void {
  console.log("[link_block_start]", variant, text, passage);
}

/** End a link block. */
export function link_block_end(): void {
  console.log("[link_block_end]");
}

/** Start a timed/transition block (<<timed>>, <<repeat>>, etc.). */
export function timed_start(...args: any[]): void {
  console.log("[timed_start]", ...args);
}

/** End a timed/transition block. */
export function timed_end(): void {
  console.log("[timed_end]");
}

/** Start a repeat block. */
export function repeat_start(...args: any[]): void {
  console.log("[repeat_start]", ...args);
}

/** End a repeat block. */
export function repeat_end(): void {
  console.log("[repeat_end]");
}

/** Start a type block (<<type>>). */
export function type_start(...args: any[]): void {
  console.log("[type_start]", ...args);
}

/** End a type block. */
export function type_end(): void {
  console.log("[type_end]");
}
