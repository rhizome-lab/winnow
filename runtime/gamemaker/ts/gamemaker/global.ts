/** GML global variable object and helpers. */

import type { GameRuntime } from "./runtime";

export function createGlobalAPI(rt: GameRuntime) {
  return {
    variable_global_exists(key: string): boolean {
      return key in rt.global;
    },
    variable_global_get(key: string): any {
      return rt.global[key];
    },
    variable_global_set(key: string, value: any): void {
      rt.global[key] = value;
    },
  };
}
