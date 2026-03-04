/** Flash save shim — JSON slot-based persistence via localStorage. */

export class SaveShim {
  constructor(private readonly _prefix = "reincarnate:") {}

  save(slot: string, data: unknown): void {
    localStorage.setItem(this._prefix + slot, JSON.stringify(data));
  }

  load(slot: string): unknown | null {
    const raw = localStorage.getItem(this._prefix + slot);
    if (raw === null) return null;
    return JSON.parse(raw);
  }

  delete(slot: string): void {
    localStorage.removeItem(this._prefix + slot);
  }

  list_slots(): string[] {
    const slots: string[] = [];
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (key !== null && key.startsWith(this._prefix)) {
        slots.push(key.slice(this._prefix.length));
      }
    }
    return slots;
  }
}
