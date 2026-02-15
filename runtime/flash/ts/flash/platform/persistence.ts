/** Browser persistence — key-value storage via localStorage. */

export function loadLocal(key: string): string | null {
  if (typeof localStorage === "undefined") return null;
  return localStorage.getItem(key);
}

export function saveLocal(key: string, value: string): void {
  if (typeof localStorage === "undefined") return;
  localStorage.setItem(key, value);
}

export function removeLocal(key: string): void {
  if (typeof localStorage === "undefined") return;
  localStorage.removeItem(key);
}
