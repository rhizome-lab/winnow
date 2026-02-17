/** Browser persistence — localStorage wrapper. */

import type { SaveBackend } from "./save";

export function loadLocal(key: string): string | null {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
}

export function saveLocal(key: string, value: string): void {
  try {
    localStorage.setItem(key, value);
  } catch {
    // Storage full or unavailable — silent fail
  }
}

export function removeLocal(key: string): void {
  try {
    localStorage.removeItem(key);
  } catch {
    // Unavailable — silent fail
  }
}

/** Default SaveBackend backed by localStorage. */
export function localStorageBackend(): SaveBackend {
  return { load: loadLocal, save: saveLocal, remove: removeLocal };
}
