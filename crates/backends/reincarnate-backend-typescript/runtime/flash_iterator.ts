/** Flash.Iterator — AVM2 for-in / for-each iteration. */

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Flash_Iterator = {
  hasNext(obj: any, index: number): boolean {
    const keys = Object.keys(obj);
    return index < keys.length;
  },

  /** AVM2 HasNext2 — advances index, returns whether there are more items. */
  hasNext2(obj: any, index: number): [any, number, boolean] {
    const keys = Object.keys(obj);
    if (index < keys.length) {
      return [obj, index + 1, true];
    }
    return [obj, 0, false];
  },

  nextName(obj: any, index: number): string {
    const keys = Object.keys(obj);
    // AVM2 indices are 1-based during iteration.
    return keys[index - 1] ?? "";
  },

  nextValue(obj: any, index: number): any {
    const keys = Object.keys(obj);
    return obj[keys[index - 1]];
  },
};
