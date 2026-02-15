/** Browser network — HTTP resource loading. */

export function fetchResource(
  url: string,
  options?: { method?: string; signal?: AbortSignal },
): Promise<Response> {
  return globalThis.fetch(url, options);
}

export function hasFetch(): boolean {
  return typeof globalThis.fetch === "function";
}
