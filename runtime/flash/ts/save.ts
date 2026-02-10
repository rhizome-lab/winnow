const PREFIX = "reincarnate:";

export const save = {
  save(slot: string, data: unknown): void {
    localStorage.setItem(PREFIX + slot, JSON.stringify(data));
  },

  load(slot: string): unknown | null {
    const raw = localStorage.getItem(PREFIX + slot);
    if (raw === null) return null;
    return JSON.parse(raw);
  },

  delete(slot: string): void {
    localStorage.removeItem(PREFIX + slot);
  },

  list_slots(): string[] {
    const slots: string[] = [];
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (key !== null && key.startsWith(PREFIX)) {
        slots.push(key.slice(PREFIX.length));
      }
    }
    return slots;
  },
};
