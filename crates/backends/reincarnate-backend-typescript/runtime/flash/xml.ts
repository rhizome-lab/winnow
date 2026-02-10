/** Flash.XML — E4X / XML operations. */

export function escapeAttribute(value: any): string {
  return String(value)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&apos;");
}

export function escapeElement(value: any): string {
  return String(value)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

export function checkFilter(value: any): any {
  // E4X filtering predicate check. If the value is XML, return it;
  // otherwise throw a TypeError like AVM2 does.
  if (value === null || value === undefined) {
    throw new TypeError("Cannot filter null or undefined");
  }
  return value;
}

// ---------------------------------------------------------------------------
// XMLList proxy — emulates E4X XMLList behavior for describeType results
// ---------------------------------------------------------------------------

const XML_LIST_TAG = Symbol("xmlList");

/** Create a Proxy-backed array that behaves like an AS3 XMLList. */
export function xmlList(items: any[]): any {
  const arr = [...items];
  (arr as any)[XML_LIST_TAG] = true;
  return new Proxy(arr, xmlListHandler);
}

function isXmlList(v: any): boolean {
  return Array.isArray(v) && v[XML_LIST_TAG] === true;
}

const xmlListHandler: ProxyHandler<any[]> = {
  get(target, prop, receiver) {
    // Numeric index access
    if (typeof prop === "string" && /^\d+$/.test(prop)) {
      return target[Number(prop)];
    }
    // length() as a method call (AS3 XMLList.length() is a method)
    if (prop === "length") {
      return () => target.length;
    }
    // contains(value) — checks if any item matches
    if (prop === "contains") {
      return (value: any) => target.some((item) => item === value || String(item) === String(value));
    }
    // toString() — join item string representations
    if (prop === "toString") {
      return () => target.map(String).join("");
    }
    // Symbol.iterator for for-of and spread
    if (prop === Symbol.iterator) {
      return target[Symbol.iterator].bind(target);
    }
    // Property projection: .name on an XMLList returns a new XMLList
    // of each item's .name property.
    if (typeof prop === "string") {
      const projected = target.map((item) => item?.[prop]);
      return xmlList(projected);
    }
    return Reflect.get(target, prop, receiver);
  },
  // Support Object.keys / Object.values / for-in enumeration.
  // Must include "length" — it's non-configurable on arrays and the
  // Proxy invariant requires ownKeys to report all non-configurable keys.
  ownKeys(target) {
    const keys: (string | symbol)[] = target.map((_, i) => String(i));
    keys.push("length");
    return keys;
  },
  getOwnPropertyDescriptor(target, prop) {
    if (prop === "length") {
      return { value: target.length, writable: true, enumerable: false, configurable: false };
    }
    if (typeof prop === "string" && /^\d+$/.test(prop)) {
      const idx = Number(prop);
      if (idx < target.length) {
        return { value: target[idx], writable: true, enumerable: true, configurable: true };
      }
    }
    return undefined;
  },
};

// ---------------------------------------------------------------------------
// Descendant access
// ---------------------------------------------------------------------------

export function getDescendants(obj: any, name: string): any {
  if (obj === null || obj === undefined) return xmlList([]);
  // Strip namespace prefix: "ns::localname" → "localname"
  const local = name.includes("::") ? name.split("::").pop()! : name;
  // If the object has the property (e.g. describeType result), return it
  const val = obj[local];
  if (val !== undefined) {
    return isXmlList(val) ? val : xmlList(Array.isArray(val) ? val : [val]);
  }
  return xmlList([]);
}

export function setDefaultNamespace(ns: any): void {
  // In AVM2 this sets the default XML namespace for the current scope.
  // No-op in lifted code — E4X is rarely used in practice.
}
