/** Flash.Scope — AVM2 scope chain operations. */

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Flash_Scope = {
  getOuterScope(): any {
    // In AVM2, the outer scope is the enclosing activation.
    // In lifted TS, there's no scope chain — return the global object.
    return globalThis;
  },

  findPropStrict(name: string): any {
    // Walk the scope chain for a property, throw if not found.
    // In lifted code, all names are resolved at compile time via imports.
    // Fallback: check globalThis.
    if (name in globalThis) return (globalThis as any)[name];
    throw new ReferenceError(`${name} is not defined`);
  },

  findProperty(name: string): any {
    // Like findPropStrict but returns undefined instead of throwing.
    if (name in globalThis) return (globalThis as any)[name];
    return undefined;
  },

  findDef(name: string): any {
    // Find a class/interface definition by name.
    if (name in globalThis) return (globalThis as any)[name];
    return undefined;
  },

  newActivation(): Record<string, any> {
    // Create a new activation object (local scope container).
    return {};
  },
};
