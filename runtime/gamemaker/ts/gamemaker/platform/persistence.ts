/** Abstract key-value persistence — backed by localStorage. */

export function saveItem(key: string, data: string): void {
  localStorage.setItem(key, data);
}

export function loadItem(key: string): string | null {
  return localStorage.getItem(key);
}

export function removeItem(key: string): void {
  localStorage.removeItem(key);
}
