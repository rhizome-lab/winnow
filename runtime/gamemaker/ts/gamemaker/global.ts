/** GML global variable object and helpers. */

export const global: Record<string, any> = {
  score: 0,
  health: 0,
  lives: 0,
  async_load: -1,
};

export function variable_global_exists(key: string): boolean {
  return key in global;
}

export function variable_global_get(key: string): any {
  return global[key];
}

export function variable_global_set(key: string, value: any): void {
  global[key] = value;
}
