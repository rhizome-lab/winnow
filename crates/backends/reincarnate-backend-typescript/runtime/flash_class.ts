/** Flash.Class — AVM2 class hierarchy operations. */

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Flash_Class = {
  getSuper(obj: any, name: string): any {
    const proto = Object.getPrototypeOf(Object.getPrototypeOf(obj));
    if (proto === null) return undefined;
    const desc = Object.getOwnPropertyDescriptor(proto, name);
    if (desc && desc.get) return desc.get.call(obj);
    return proto[name];
  },

  setSuper(obj: any, name: string, value: any): void {
    const proto = Object.getPrototypeOf(Object.getPrototypeOf(obj));
    if (proto === null) return;
    const desc = Object.getOwnPropertyDescriptor(proto, name);
    if (desc && desc.set) {
      desc.set.call(obj, value);
    } else {
      obj[name] = value;
    }
  },

  callSuper(obj: any, name: string, ...args: any[]): any {
    const proto = Object.getPrototypeOf(Object.getPrototypeOf(obj));
    if (proto === null || typeof proto[name] !== "function") return undefined;
    return proto[name].apply(obj, args);
  },

  constructSuper(obj: any, ...args: any[]): void {
    // In AVM2, ConstructSuper calls the parent class's constructor.
    // With JS prototypes, this is handled by `super()` in constructors.
    // When we're emitting flat functions, this is a no-op — the super
    // constructor ran at allocation time.
  },

  initClass(base: any, name: string): any {
    // Static initializer for a class. In the lifted output, classes are
    // plain interfaces + factory functions, so this is a no-op stub.
    return base;
  },
};
