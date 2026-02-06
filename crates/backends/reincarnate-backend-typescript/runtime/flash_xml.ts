/** Flash.XML — E4X / XML operations. */

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Flash_XML = {
  escapeAttribute(value: any): string {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&apos;");
  },

  escapeElement(value: any): string {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
  },

  checkFilter(value: any): any {
    // E4X filtering predicate check. If the value is XML, return it;
    // otherwise throw a TypeError like AVM2 does.
    if (value === null || value === undefined) {
      throw new TypeError("Cannot filter null or undefined");
    }
    return value;
  },

  getDescendants(obj: any, name: string): any {
    // E4X descendant access (obj..name). Without a real XML type,
    // fall back to property access.
    if (obj === null || obj === undefined) return undefined;
    return obj[name];
  },

  setDefaultNamespace(ns: any): void {
    // In AVM2 this sets the default XML namespace for the current scope.
    // No-op in lifted code — E4X is rarely used in practice.
  },
};
