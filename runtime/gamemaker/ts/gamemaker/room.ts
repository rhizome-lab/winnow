/** GML room functions — now methods on GameRuntime, re-exported for module resolution. */

// Room navigation functions (room_goto, room_goto_next, etc.) are now methods
// on GameRuntime. Generated code destructures them from this._rt.
// This module exists for backwards compatibility with function_modules resolution.
export {};
