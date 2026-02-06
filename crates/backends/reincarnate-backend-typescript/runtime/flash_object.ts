/** Flash.Object — AVM2 object model operations. */

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Flash_Object = {
  typeOf(value: unknown): string {
    if (value === null || value === undefined) return "object";
    const t = typeof value;
    if (t === "function") return "function";
    if (t === "number") return "number";
    if (t === "boolean") return "boolean";
    if (t === "string") return "string";
    if (t === "undefined") return "undefined";
    if (t === "bigint") return "number";
    return "object";
  },

  deleteProperty(obj: any, name: string): boolean {
    try {
      return delete obj[name];
    } catch {
      return false;
    }
  },

  construct(ctor: any, ...args: any[]): any {
    return new ctor(...args);
  },

  newObject(...pairs: any[]): Record<string, any> {
    const obj: Record<string, any> = {};
    for (let i = 0; i < pairs.length; i += 2) {
      obj[pairs[i] as string] = pairs[i + 1];
    }
    return obj;
  },

  newFunction(name: string): any {
    // Stub: in a full AVM2 runtime this would create a closure from a
    // method reference. For now return a named no-op.
    const fn_ = function (this: any, ..._args: any[]) {};
    Object.defineProperty(fn_, "name", { value: name });
    return fn_;
  },

  applyType(base: any, ...typeArgs: any[]): any {
    // AVM2 ApplyType creates a parameterized type (e.g., Vector.<int>).
    // In TypeScript land, just return the base — generics are erased.
    return base;
  },

  hasProperty(obj: any, name: string): boolean {
    if (obj === null || obj === undefined) return false;
    return name in Object(obj);
  },
};
