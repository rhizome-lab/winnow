/**
 * AMF3 (Action Message Format 3) serialization for ByteArray.readObject/writeObject.
 *
 * Implements the subset needed for Flash save systems: undefined, null, booleans,
 * integers, doubles, strings, arrays (dense + associative), and dynamic objects.
 * Reference tables are maintained per serialization context for strings and objects.
 */

import { ByteArray } from "./utils";

// ---------------------------------------------------------------------------
// AMF3 type markers
// ---------------------------------------------------------------------------

const AMF3_UNDEFINED = 0x00;
const AMF3_NULL = 0x01;
const AMF3_FALSE = 0x02;
const AMF3_TRUE = 0x03;
const AMF3_INTEGER = 0x04;
const AMF3_DOUBLE = 0x05;
const AMF3_STRING = 0x06;
const AMF3_ARRAY = 0x09;
const AMF3_OBJECT = 0x0a;

// ---------------------------------------------------------------------------
// U29 variable-length integer encoding
// ---------------------------------------------------------------------------

function writeU29(ba: ByteArray, value: number): void {
  // U29 uses 1-4 bytes with LSB continuation bit.
  // Bits 0-6: 1 byte, bit 7 = 0
  // Bits 0-13: 2 bytes, bit 7 = 1 in first
  // Bits 0-20: 3 bytes, bit 7 = 1 in first two
  // Bits 0-28: 4 bytes, bit 7 = 1 in first three, full 8 bits in last
  value &= 0x1fffffff; // 29 bits max
  if (value < 0x80) {
    ba.writeByte(value);
  } else if (value < 0x4000) {
    ba.writeByte(((value >> 7) & 0x7f) | 0x80);
    ba.writeByte(value & 0x7f);
  } else if (value < 0x200000) {
    ba.writeByte(((value >> 14) & 0x7f) | 0x80);
    ba.writeByte(((value >> 7) & 0x7f) | 0x80);
    ba.writeByte(value & 0x7f);
  } else {
    ba.writeByte(((value >> 22) & 0x7f) | 0x80);
    ba.writeByte(((value >> 15) & 0x7f) | 0x80);
    ba.writeByte(((value >> 8) & 0x7f) | 0x80);
    ba.writeByte(value & 0xff);
  }
}

function readU29(ba: ByteArray): number {
  let result = 0;
  for (let i = 0; i < 3; i++) {
    const b = ba.readUnsignedByte();
    if (b < 0x80) return (result << 7) | b;
    result = (result << 7) | (b & 0x7f);
  }
  // 4th byte uses all 8 bits.
  const b = ba.readUnsignedByte();
  return (result << 8) | b;
}

// ---------------------------------------------------------------------------
// Read context (reference tables)
// ---------------------------------------------------------------------------

class ReadContext {
  strings: string[] = [];
  objects: any[] = [];

  readString(ba: ByteArray): string {
    const ref = readU29(ba);
    if ((ref & 1) === 0) {
      // Reference: index = ref >> 1
      return this.strings[ref >> 1];
    }
    const len = ref >> 1;
    if (len === 0) return "";
    const str = ba.readUTFBytes(len);
    this.strings.push(str);
    return str;
  }

  readValue(ba: ByteArray): any {
    const marker = ba.readUnsignedByte();
    switch (marker) {
      case AMF3_UNDEFINED:
        return undefined;
      case AMF3_NULL:
        return null;
      case AMF3_FALSE:
        return false;
      case AMF3_TRUE:
        return true;
      case AMF3_INTEGER: {
        let n = readU29(ba);
        // Sign-extend from 29 bits.
        if (n >= 0x10000000) n -= 0x20000000;
        return n;
      }
      case AMF3_DOUBLE:
        return ba.readDouble();
      case AMF3_STRING:
        return this.readString(ba);
      case AMF3_ARRAY:
        return this.readArray(ba);
      case AMF3_OBJECT:
        return this.readObject(ba);
      default:
        throw new Error(`Unsupported AMF3 marker: 0x${marker.toString(16)}`);
    }
  }

  readArray(ba: ByteArray): any {
    const ref = readU29(ba);
    if ((ref & 1) === 0) {
      return this.objects[ref >> 1];
    }
    const denseCount = ref >> 1;
    const result: any = [];
    this.objects.push(result);

    // Read associative (string-keyed) portion first.
    for (;;) {
      const key = this.readString(ba);
      if (key === "") break;
      result[key] = this.readValue(ba);
    }

    // Read dense portion.
    for (let i = 0; i < denseCount; i++) {
      result[i] = this.readValue(ba);
    }
    return result;
  }

  readObject(ba: ByteArray): any {
    const ref = readU29(ba);
    if ((ref & 1) === 0) {
      return this.objects[ref >> 1];
    }
    // We only support dynamic objects with no sealed traits.
    // ref >> 1 encodes trait info; for dynamic anonymous objects:
    // traits-ref has low bit 1 (inline), next bit 1 (dynamic), next bit 0 (no externalizable),
    // upper bits = sealed member count (0).
    // So traits header = 0b1011 = 11. But we also accept references to previously seen traits.
    const traitsRef = ref >> 1;
    if ((traitsRef & 1) === 0) {
      // Traits reference — skip reading trait info, just read dynamic.
      // For simplicity, treat as fully dynamic with 0 sealed members.
    }
    // For inline traits: externalizable = (traitsRef >> 1) & 1, dynamic = (traitsRef >> 2) & 1
    // sealedCount = traitsRef >> 3
    // Skip the class name string.
    if (traitsRef & 1) {
      this.readString(ba); // class name (empty for anonymous)
      const sealedCount = traitsRef >> 3;
      // Read sealed member names (we store them but treat object as plain).
      const sealedNames: string[] = [];
      for (let i = 0; i < sealedCount; i++) {
        sealedNames.push(this.readString(ba));
      }
      const obj: any = {};
      this.objects.push(obj);
      // Read sealed member values.
      for (let i = 0; i < sealedCount; i++) {
        obj[sealedNames[i]] = this.readValue(ba);
      }
      // Read dynamic members if dynamic flag is set.
      const isDynamic = (traitsRef >> 2) & 1;
      if (isDynamic) {
        for (;;) {
          const key = this.readString(ba);
          if (key === "") break;
          obj[key] = this.readValue(ba);
        }
      }
      return obj;
    } else {
      // Traits reference to a previously seen class — we don't track trait definitions
      // separately in this minimal implementation; treat as empty dynamic object.
      const obj: any = {};
      this.objects.push(obj);
      // Read dynamic members.
      for (;;) {
        const key = this.readString(ba);
        if (key === "") break;
        obj[key] = this.readValue(ba);
      }
      return obj;
    }
  }
}

// ---------------------------------------------------------------------------
// Write context (reference tables)
// ---------------------------------------------------------------------------

class WriteContext {
  strings: Map<string, number> = new Map();
  objects: Map<any, number> = new Map();

  writeString(ba: ByteArray, str: string): void {
    if (str === "") {
      writeU29(ba, 1); // Inline, length 0: (0 << 1) | 1 = 1
      return;
    }
    const ref = this.strings.get(str);
    if (ref !== undefined) {
      writeU29(ba, ref << 1); // Reference: low bit 0
      return;
    }
    this.strings.set(str, this.strings.size);
    const encoded = new TextEncoder().encode(str);
    writeU29(ba, (encoded.length << 1) | 1); // Inline: low bit 1
    for (let i = 0; i < encoded.length; i++) {
      ba.writeByte(encoded[i]);
    }
  }

  writeValue(ba: ByteArray, value: any): void {
    if (value === undefined) {
      ba.writeByte(AMF3_UNDEFINED);
      return;
    }
    if (value === null) {
      ba.writeByte(AMF3_NULL);
      return;
    }
    if (value === true) {
      ba.writeByte(AMF3_TRUE);
      return;
    }
    if (value === false) {
      ba.writeByte(AMF3_FALSE);
      return;
    }
    if (typeof value === "number") {
      // Use integer for values that fit in 29-bit signed range with no fractional part.
      if (Number.isInteger(value) && value >= -0x10000000 && value <= 0x0fffffff) {
        ba.writeByte(AMF3_INTEGER);
        writeU29(ba, value & 0x1fffffff);
      } else {
        ba.writeByte(AMF3_DOUBLE);
        ba.writeDouble(value);
      }
      return;
    }
    if (typeof value === "string") {
      ba.writeByte(AMF3_STRING);
      this.writeString(ba, value);
      return;
    }
    if (Array.isArray(value)) {
      this.writeArray(ba, value);
      return;
    }
    if (typeof value === "object") {
      this.writeObject(ba, value);
      return;
    }
    // Fallback: coerce to string.
    ba.writeByte(AMF3_STRING);
    this.writeString(ba, String(value));
  }

  writeArray(ba: ByteArray, arr: any[]): void {
    const ref = this.objects.get(arr);
    if (ref !== undefined) {
      ba.writeByte(AMF3_ARRAY);
      writeU29(ba, ref << 1); // Reference
      return;
    }
    this.objects.set(arr, this.objects.size);
    ba.writeByte(AMF3_ARRAY);

    // Separate dense indices from string keys.
    const denseCount = arr.length;
    writeU29(ba, (denseCount << 1) | 1); // Inline

    // Write associative (non-index) keys first.
    for (const key of Object.keys(arr)) {
      const idx = Number(key);
      if (Number.isInteger(idx) && idx >= 0 && idx < denseCount) continue;
      this.writeString(ba, key);
      this.writeValue(ba, arr[key as any]);
    }
    this.writeString(ba, ""); // End of associative portion.

    // Write dense portion.
    for (let i = 0; i < denseCount; i++) {
      this.writeValue(ba, arr[i]);
    }
  }

  writeObject(ba: ByteArray, obj: any): void {
    const ref = this.objects.get(obj);
    if (ref !== undefined) {
      ba.writeByte(AMF3_OBJECT);
      writeU29(ba, ref << 1); // Reference
      return;
    }
    this.objects.set(obj, this.objects.size);
    ba.writeByte(AMF3_OBJECT);

    // Inline traits: dynamic anonymous object, 0 sealed members.
    // traitsRef = (0 << 3) | (1 << 2) | (0 << 1) | 1 = 0b0101 = 5
    // Then shifted: (5 << 1) | 1 = 11
    writeU29(ba, 11);
    this.writeString(ba, ""); // Empty class name (anonymous).

    // Write dynamic key-value pairs.
    for (const key of Object.keys(obj)) {
      this.writeString(ba, key);
      this.writeValue(ba, obj[key]);
    }
    this.writeString(ba, ""); // End of dynamic members.
  }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export function readAMF3(ba: ByteArray): any {
  const ctx = new ReadContext();
  return ctx.readValue(ba);
}

export function writeAMF3(ba: ByteArray, value: any): void {
  const ctx = new WriteContext();
  ctx.writeValue(ba, value);
}
