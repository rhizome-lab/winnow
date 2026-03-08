/**
 * Persistence platform — OPFS-backed with localStorage fallback.
 *
 * OPFS (Origin Private File System) is available in Chrome 86+, Firefox 111+, Safari 15.2+.
 * It provides a sandboxed filesystem with no size-limit dialogs, no user permission prompts,
 * and higher storage quotas than localStorage.
 *
 * Design: write-through in-memory cache.
 *   - init() preloads all OPFS entries (or localStorage entries) into the in-memory cache
 *     so that all reads after init() are synchronous.
 *   - store: cache + localStorage (sync, base64-encoded) + OPFS (async, raw bytes,
 *     write-to-temp-then-rename for atomicity). Throws on failure.
 *   - fetch: sync read from in-memory cache; null if not found.
 *   - remove: cache + localStorage (sync) + OPFS (async). Throws on failure.
 *   - list: enumerate cached keys with a given prefix.
 *
 * OPFS filenames cannot contain "/" so keys are encoded with encodeURIComponent.
 * localStorage is string-based; bytes are stored as base64.
 */

export class PersistenceState {
  cache = new Map<string, Uint8Array<ArrayBuffer>>();
  opfsDir: FileSystemDirectoryHandle | null = null;
}

function keyToFilename(key: string): string {
  return encodeURIComponent(key);
}
function filenameToKey(name: string): string {
  try { return decodeURIComponent(name); } catch { return name; }
}

function bytesToBase64(data: Uint8Array<ArrayBuffer>): string {
  let binary = "";
  for (let i = 0; i < data.length; i++) binary += String.fromCharCode(data[i]);
  return btoa(binary);
}

function base64ToBytes(b64: string): Uint8Array<ArrayBuffer> {
  const binary = atob(b64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return bytes;
}

/**
 * Initialize persistence. Await once at startup before the game loop begins.
 * Loads all OPFS files into the in-memory cache for subsequent sync reads.
 * Falls back to preloading from localStorage when OPFS is unavailable.
 */
export async function init(state: PersistenceState): Promise<void> {
  if (typeof navigator === "undefined" || !("storage" in navigator)) return;
  try {
    const dir = await navigator.storage.getDirectory();
    state.opfsDir = dir;
    // Preload all OPFS entries into cache so reads are sync after init.
    for await (const [filename, handle] of dir.entries()) {
      if (handle.kind === "file") {
        try {
          const file = await (handle as FileSystemFileHandle).getFile();
          const buf = await file.arrayBuffer();
          state.cache.set(filenameToKey(filename), new Uint8Array(buf));
        } catch { /* skip unreadable files */ }
      }
    }
  } catch {
    // OPFS unavailable (older browser, incognito with restrictions, etc.).
    // Preload from localStorage so reads are still sync after init.
    if (typeof localStorage !== "undefined") {
      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i);
        if (key === null) continue;
        const val = localStorage.getItem(key);
        if (val !== null) {
          try { state.cache.set(key, base64ToBytes(val)); } catch { /* skip malformed entries */ }
        }
      }
    }
  }
}

/**
 * Write bytes under key. Sync to cache + localStorage (base64); async to OPFS (raw bytes).
 * Throws if localStorage write fails (quota exceeded, permission denied, etc.).
 * OPFS write uses write-to-temp-then-rename for atomicity.
 */
export function store(state: PersistenceState, key: string, data: Uint8Array<ArrayBuffer>): void {
  state.cache.set(key, data);
  // localStorage is string-based; encode as base64.
  localStorage.setItem(key, bytesToBase64(data));
  if (state.opfsDir) {
    const dir = state.opfsDir;
    const filename = keyToFilename(key);
    const tmpFilename = filename + ".tmp";
    // Write-to-temp-then-rename for atomicity.
    dir.getFileHandle(tmpFilename, { create: true })
      .then(fh => fh.createWritable())
      .then(w => w.write(data).then(() => w.close()))
      .then(() => dir.getFileHandle(tmpFilename).then(fh =>
        // Rename by moving: read back and write to final name, then remove tmp.
        // The Web FileSystem Access API has no rename(); approximate with copy+delete.
        fh.getFile()
          .then(f => f.arrayBuffer())
          .then(buf => dir.getFileHandle(filename, { create: true })
            .then(dst => dst.createWritable())
            .then(w => w.write(buf).then(() => w.close()))
          )
          .then(() => dir.removeEntry(tmpFilename).catch(() => {}))
      ))
      .catch(() => { /* OPFS write failed; localStorage retains the data */ });
  }
}

/**
 * Read bytes by key. Returns null if not found.
 * All reads are sync after init() has resolved.
 */
export function fetch(state: PersistenceState, key: string): Uint8Array<ArrayBuffer> | null {
  const cached = state.cache.get(key);
  if (cached !== undefined) return cached;
  // Cache miss: fall back to localStorage (handles data written before OPFS init).
  if (typeof localStorage !== "undefined") {
    const val = localStorage.getItem(key);
    if (val !== null) {
      try {
        const bytes = base64ToBytes(val);
        state.cache.set(key, bytes); // promote to cache for future reads
        return bytes;
      } catch {
        return null; // malformed base64
      }
    }
  }
  return null;
}

/**
 * Remove a key from all storage layers.
 * Throws if localStorage removal fails (permission denied, etc.).
 */
export function remove(state: PersistenceState, key: string): void {
  state.cache.delete(key);
  localStorage.removeItem(key);
  if (state.opfsDir) {
    state.opfsDir.removeEntry(keyToFilename(key)).catch(() => {});
  }
}

/**
 * List all cached keys that start with the given prefix.
 * Only reflects what is in the in-memory cache; call init() first for complete results.
 */
export function list(state: PersistenceState, prefix: string): string[] {
  const result: string[] = [];
  for (const key of state.cache.keys()) {
    if (key.startsWith(prefix)) result.push(key);
  }
  return result;
}
