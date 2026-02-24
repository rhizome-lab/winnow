/**
 * Persistence platform — OPFS-backed with localStorage fallback.
 *
 * OPFS (Origin Private File System) is available in Chrome 86+, Firefox 111+, Safari 15.2+.
 * It provides a sandboxed filesystem with no size-limit dialogs, no user permission prompts,
 * and higher storage quotas than localStorage.
 *
 * Design: write-through in-memory cache.
 *   - initPersistence() preloads OPFS (or skips; localStorage is the always-available fallback).
 *   - saveItem: cache + localStorage (sync) + OPFS (async, fire-and-forget).
 *   - loadItem: cache first, then localStorage on miss (transparently migrates old data).
 *   - removeItem: cache + localStorage (sync) + OPFS (async).
 *
 * OPFS filenames cannot contain "/" so keys are encoded with encodeURIComponent.
 */

const _cache = new Map<string, string>();
let _opfsDir: FileSystemDirectoryHandle | null = null;

function keyToFilename(key: string): string {
  return encodeURIComponent(key);
}
function filenameToKey(name: string): string {
  try { return decodeURIComponent(name); } catch { return name; }
}

/**
 * Initialize persistence. Await once at startup before the game loop begins.
 * Loads all OPFS files into the in-memory cache for subsequent sync reads.
 * Safe to skip — localStorage reads will work without it, just without OPFS durability.
 */
export async function initPersistence(): Promise<void> {
  if (typeof navigator === "undefined" || !("storage" in navigator)) return;
  try {
    const dir = await navigator.storage.getDirectory();
    _opfsDir = dir;
    // Preload all entries into cache so reads are sync after init.
    for await (const [filename, handle] of dir.entries()) {
      if (handle.kind === "file") {
        try {
          const file = await (handle as FileSystemFileHandle).getFile();
          _cache.set(filenameToKey(filename), await file.text());
        } catch { /* skip unreadable files */ }
      }
    }
  } catch {
    // OPFS unavailable (older browser, incognito with restrictions, etc.).
    // localStorage fallback handles reads on-demand via loadItem.
  }
}

/** Write a string value under key. Sync to cache + localStorage; async to OPFS. */
export function saveItem(key: string, data: string): void {
  _cache.set(key, data);
  try { localStorage.setItem(key, data); } catch { /* storage quota exceeded */ }
  if (_opfsDir) {
    const filename = keyToFilename(key);
    _opfsDir.getFileHandle(filename, { create: true })
      .then(fh => fh.createWritable())
      .then(w => w.write(data).then(() => w.close()))
      .catch(() => { /* OPFS write failed; localStorage retains the data */ });
  }
}

/** Read a string value by key. Returns null if not found. */
export function loadItem(key: string): string | null {
  if (_cache.has(key)) return _cache.get(key)!;
  // Cache miss: fall back to localStorage (also handles data written before OPFS init).
  const val = localStorage.getItem(key);
  if (val !== null) _cache.set(key, val); // promote to cache for future reads
  return val;
}

/** Remove a key from all storage layers. */
export function removeItem(key: string): void {
  _cache.delete(key);
  try { localStorage.removeItem(key); } catch { /* ignore */ }
  if (_opfsDir) {
    _opfsDir.removeEntry(keyToFilename(key)).catch(() => {});
  }
}
