/**
 * flash.utils package — ByteArray, Timer, Proxy, getQualifiedClassName.
 */

import { EventDispatcher } from "./display";
import { TimerEvent } from "./events";
import { readAMF3, writeAMF3 } from "./amf";
import { inflateRaw, deflateRaw, zlibCompress, zlibDecompress } from "./deflate";
import { xmlList } from "./xml";

// ---------------------------------------------------------------------------
// Qualified-name symbol + utility functions
// ---------------------------------------------------------------------------

export const QN_KEY = Symbol("as3:qualifiedName");

const _classRegistry = new Map<string, Function>();

export function registerClass(ctor: Function): void {
  const name = (ctor as any)[QN_KEY];
  if (typeof name === "string") _classRegistry.set(name, ctor);
}

export function getQualifiedClassName(value: any): string {
  if (value == null) return "null";
  const ctor = typeof value === "function" ? value : value.constructor;
  return ctor?.[QN_KEY] ?? ctor?.name ?? typeof value;
}

export function getQualifiedSuperclassName(value: any): string | null {
  if (value == null) return null;
  const ctor = typeof value === "function" ? value : value.constructor;
  if (!ctor) return null;
  const parent = Object.getPrototypeOf(ctor.prototype)?.constructor;
  if (!parent || parent === Object) return null;
  return parent[QN_KEY] ?? parent.name ?? null;
}

export function getDefinitionByName(name: string): any {
  const cls = _classRegistry.get(name);
  if (cls) return cls;
  // Fall back: strip package prefix and try as global.
  const short = name.includes("::") ? name.split("::").pop()! : name;
  for (const [qn, c] of _classRegistry) {
    const s = qn.includes("::") ? qn.split("::").pop()! : qn;
    if (s === short) return c;
  }
  throw new ReferenceError(`getDefinitionByName: '${name}' is not defined`);
}

// ---------------------------------------------------------------------------
// Trait registry — populated by registerClassTraits() calls from emitted code
// ---------------------------------------------------------------------------

interface TraitInfo {
  name: string;
  kind: "constant" | "variable" | "method" | "accessor";
  type?: string;
  access?: "readonly" | "writeonly" | "readwrite";
  declaredBy?: string;
  isStatic?: boolean;
}

interface ClassTraits {
  instanceTraits: TraitInfo[];
  staticTraits: TraitInfo[];
}

const _traitRegistry = new Map<Function, ClassTraits>();

export function registerClassTraits(ctor: Function, instance: TraitInfo[], staticT: TraitInfo[]): void {
  _traitRegistry.set(ctor, { instanceTraits: instance, staticTraits: staticT });
}

/** Wraps a type name string with .toString() and .indexOf() like AS3 XML text nodes. */
class TraitTypeName {
  private _value: string;
  constructor(value: string) { this._value = value; }
  toString(): string { return this._value; }
  indexOf(s: string): number { return this._value.indexOf(s); }
  valueOf(): string { return this._value; }
}

function traitNode(name: string, type?: string, meta?: any): any {
  const node: any = { name };
  if (type !== undefined) node.type = new TraitTypeName(type);
  node.metadata = meta ?? xmlList([]);
  return node;
}

export function describeType(value: any): any {
  if (value == null) return { constant: xmlList([]) };

  if (typeof value === "function") {
    // Class constructor — top-level = static traits, factory = instance traits
    const registered = _traitRegistry.get(value);
    const constants: any[] = [];
    const variables: any[] = [];
    const methods: any[] = [];
    const accessors: any[] = [];

    // Static traits from registry
    if (registered) {
      for (const t of registered.staticTraits) {
        const node = traitNode(t.name, t.type);
        if (t.kind === "constant") constants.push(node);
        else if (t.kind === "variable") variables.push(node);
        else if (t.kind === "method") methods.push(node);
        else if (t.kind === "accessor") { node.access = t.access; accessors.push(node); }
      }
    }

    // Supplement with runtime introspection for constants not in registry
    const registeredNames = new Set(registered?.staticTraits.map((t) => t.name) ?? []);
    for (const name of Object.getOwnPropertyNames(value)) {
      if (name === "prototype" || name === "length" || name === "name"
          || name === "arguments" || name === "caller" || name === QN_KEY.toString()) continue;
      if (typeof name === "symbol") continue;
      if (registeredNames.has(name)) continue;
      if (typeof value[name] === "function") {
        methods.push(traitNode(name));
      } else {
        constants.push(traitNode(name));
      }
    }

    // Instance traits from registry (exposed via factory)
    const iConstants: any[] = [];
    const iVariables: any[] = [];
    const iMethods: any[] = [];
    const iAccessors: any[] = [];
    if (registered) {
      for (const t of registered.instanceTraits) {
        const node = traitNode(t.name, t.type);
        if (t.kind === "constant") iConstants.push(node);
        else if (t.kind === "variable") iVariables.push(node);
        else if (t.kind === "method") iMethods.push(node);
        else if (t.kind === "accessor") { node.access = t.access; iAccessors.push(node); }
      }
    }

    return {
      constant: xmlList(constants),
      variable: xmlList(variables),
      method: xmlList(methods),
      accessor: xmlList(accessors),
      factory: {
        constant: xmlList(iConstants),
        variable: xmlList(iVariables),
        method: xmlList(iMethods),
        accessor: xmlList(iAccessors),
      },
    };
  }

  // Instance — describe its class's instance traits directly
  const ctor = value.constructor;
  const registered = ctor ? _traitRegistry.get(ctor) : undefined;
  const constants: any[] = [];
  const variables: any[] = [];
  const methods: any[] = [];
  const accessors: any[] = [];
  if (registered) {
    for (const t of registered.instanceTraits) {
      const node = traitNode(t.name, t.type);
      if (t.kind === "constant") constants.push(node);
      else if (t.kind === "variable") variables.push(node);
      else if (t.kind === "method") methods.push(node);
      else if (t.kind === "accessor") { node.access = t.access; accessors.push(node); }
    }
  }
  return {
    constant: xmlList(constants),
    variable: xmlList(variables),
    method: xmlList(methods),
    accessor: xmlList(accessors),
  };
}

// ---------------------------------------------------------------------------
// ByteArray
// ---------------------------------------------------------------------------

const BIG_ENDIAN = "bigEndian";
const LITTLE_ENDIAN = "littleEndian";

export class ByteArray {
  private _buffer: ArrayBuffer;
  private _view: DataView;
  private _length: number;
  position = 0;
  endian: string = BIG_ENDIAN;
  objectEncoding = 3;

  constructor() {
    this._buffer = new ArrayBuffer(64);
    this._view = new DataView(this._buffer);
    this._length = 0;
  }

  get length(): number {
    return this._length;
  }

  set length(value: number) {
    this._ensureCapacity(value);
    if (value < this._length) {
      // Zero out truncated region.
      const u8 = new Uint8Array(this._buffer);
      u8.fill(0, value, this._length);
    }
    this._length = value;
    if (this.position > value) this.position = value;
  }

  get bytesAvailable(): number {
    return this._length - this.position;
  }

  // -- Read methods --

  readBoolean(): boolean {
    return this.readUnsignedByte() !== 0;
  }

  readByte(): number {
    this._checkRead(1);
    const v = this._view.getInt8(this.position);
    this.position += 1;
    return v;
  }

  readShort(): number {
    this._checkRead(2);
    const v = this._view.getInt16(this.position, this._le());
    this.position += 2;
    return v;
  }

  readInt(): number {
    this._checkRead(4);
    const v = this._view.getInt32(this.position, this._le());
    this.position += 4;
    return v;
  }

  readFloat(): number {
    this._checkRead(4);
    const v = this._view.getFloat32(this.position, this._le());
    this.position += 4;
    return v;
  }

  readDouble(): number {
    this._checkRead(8);
    const v = this._view.getFloat64(this.position, this._le());
    this.position += 8;
    return v;
  }

  readUnsignedByte(): number {
    this._checkRead(1);
    const v = this._view.getUint8(this.position);
    this.position += 1;
    return v;
  }

  readUnsignedShort(): number {
    this._checkRead(2);
    const v = this._view.getUint16(this.position, this._le());
    this.position += 2;
    return v;
  }

  readUnsignedInt(): number {
    this._checkRead(4);
    const v = this._view.getUint32(this.position, this._le());
    this.position += 4;
    return v;
  }

  readUTF(): string {
    const len = this.readUnsignedShort();
    return this.readUTFBytes(len);
  }

  readUTFBytes(length: number): string {
    this._checkRead(length);
    const bytes = new Uint8Array(this._buffer, this.position, length);
    this.position += length;
    return new TextDecoder("utf-8").decode(bytes);
  }

  readMultiByte(length: number, charSet: string): string {
    this._checkRead(length);
    const bytes = new Uint8Array(this._buffer, this.position, length);
    this.position += length;
    try {
      return new TextDecoder(charSet).decode(bytes);
    } catch {
      return new TextDecoder("utf-8").decode(bytes);
    }
  }

  readObject(): any {
    return readAMF3(this);
  }

  readBytes(bytes: ByteArray, offset = 0, length = 0): void {
    const len = length === 0 ? this.bytesAvailable : length;
    this._checkRead(len);
    bytes._ensureCapacity(offset + len);
    const src = new Uint8Array(this._buffer, this.position, len);
    new Uint8Array(bytes._buffer).set(src, offset);
    this.position += len;
    if (offset + len > bytes._length) bytes._length = offset + len;
  }

  // -- Write methods --

  writeBoolean(value: boolean): void {
    this.writeByte(value ? 1 : 0);
  }

  writeByte(value: number): void {
    this._ensureCapacity(this.position + 1);
    this._view.setInt8(this.position, value);
    this.position += 1;
    if (this.position > this._length) this._length = this.position;
  }

  writeShort(value: number): void {
    this._ensureCapacity(this.position + 2);
    this._view.setInt16(this.position, value, this._le());
    this.position += 2;
    if (this.position > this._length) this._length = this.position;
  }

  writeInt(value: number): void {
    this._ensureCapacity(this.position + 4);
    this._view.setInt32(this.position, value, this._le());
    this.position += 4;
    if (this.position > this._length) this._length = this.position;
  }

  writeFloat(value: number): void {
    this._ensureCapacity(this.position + 4);
    this._view.setFloat32(this.position, value, this._le());
    this.position += 4;
    if (this.position > this._length) this._length = this.position;
  }

  writeDouble(value: number): void {
    this._ensureCapacity(this.position + 8);
    this._view.setFloat64(this.position, value, this._le());
    this.position += 8;
    if (this.position > this._length) this._length = this.position;
  }

  writeUnsignedInt(value: number): void {
    this._ensureCapacity(this.position + 4);
    this._view.setUint32(this.position, value, this._le());
    this.position += 4;
    if (this.position > this._length) this._length = this.position;
  }

  writeUTF(value: string): void {
    const encoded = new TextEncoder().encode(value);
    this.writeShort(encoded.length);
    this.writeUTFBytes(value);
  }

  writeUTFBytes(value: string): void {
    const encoded = new TextEncoder().encode(value);
    this._ensureCapacity(this.position + encoded.length);
    new Uint8Array(this._buffer).set(encoded, this.position);
    this.position += encoded.length;
    if (this.position > this._length) this._length = this.position;
  }

  writeMultiByte(value: string, charSet: string): void {
    const cs = charSet.toLowerCase().replace(/[^a-z0-9]/g, "");
    if (cs === "iso88591" || cs === "latin1") {
      // Direct codepoint-to-byte mapping for ISO-8859-1/Latin-1.
      this._ensureCapacity(this.position + value.length);
      const u8 = new Uint8Array(this._buffer);
      for (let i = 0; i < value.length; i++) {
        u8[this.position++] = value.charCodeAt(i) & 0xff;
      }
      if (this.position > this._length) this._length = this.position;
    } else {
      this.writeUTFBytes(value);
    }
  }

  writeObject(object: any): void {
    writeAMF3(this, object);
  }

  writeBytes(bytes: ByteArray, offset = 0, length = 0): void {
    const len = length === 0 ? bytes._length - offset : length;
    this._ensureCapacity(this.position + len);
    const src = new Uint8Array(bytes._buffer, offset, len);
    new Uint8Array(this._buffer).set(src, this.position);
    this.position += len;
    if (this.position > this._length) this._length = this.position;
  }

  // -- Utility methods --

  clear(): void {
    this._buffer = new ArrayBuffer(64);
    this._view = new DataView(this._buffer);
    this._length = 0;
    this.position = 0;
  }

  compress(algorithm = "zlib"): void {
    const input = new Uint8Array(this._buffer, 0, this._length);
    let result: Uint8Array;
    if (algorithm === "deflate") {
      result = deflateRaw(input);
    } else {
      result = zlibCompress(input);
    }
    this._buffer = new ArrayBuffer(result.length);
    new Uint8Array(this._buffer).set(result);
    this._view = new DataView(this._buffer);
    this._length = result.length;
    this.position = 0;
  }

  uncompress(algorithm = "zlib"): void {
    const input = new Uint8Array(this._buffer, 0, this._length);
    let result: Uint8Array;
    if (algorithm === "deflate") {
      result = inflateRaw(input);
    } else {
      result = zlibDecompress(input);
    }
    this._buffer = new ArrayBuffer(result.length);
    new Uint8Array(this._buffer).set(result);
    this._view = new DataView(this._buffer);
    this._length = result.length;
    this.position = 0;
  }

  deflate(): void {
    this.compress("deflate");
  }

  inflate(): void {
    this.uncompress("deflate");
  }

  toString(): string {
    return new TextDecoder("utf-8").decode(
      new Uint8Array(this._buffer, 0, this._length),
    );
  }

  // -- Internal helpers --

  private _le(): boolean {
    return this.endian === LITTLE_ENDIAN;
  }

  private _checkRead(bytes: number): void {
    if (this.position + bytes > this._length) {
      throw new RangeError("End of ByteArray was encountered");
    }
  }

  private _ensureCapacity(needed: number): void {
    if (needed <= this._buffer.byteLength) return;
    let newSize = this._buffer.byteLength;
    while (newSize < needed) newSize *= 2;
    const next = new ArrayBuffer(newSize);
    new Uint8Array(next).set(new Uint8Array(this._buffer));
    this._buffer = next;
    this._view = new DataView(this._buffer);
  }
}

// ---------------------------------------------------------------------------
// Timer
// ---------------------------------------------------------------------------

export class Timer extends EventDispatcher {
  private _delay: number;
  private _repeatCount: number;
  private _currentCount = 0;
  private _running = false;
  private _intervalId: any = null;

  constructor(delay: number, repeatCount = 0) {
    super();
    this._delay = delay;
    this._repeatCount = repeatCount;
  }

  get currentCount(): number {
    return this._currentCount;
  }

  get delay(): number {
    return this._delay;
  }
  set delay(value: number) {
    this._delay = value;
    if (this._running) {
      this._stopInterval();
      this._startInterval();
    }
  }

  get repeatCount(): number {
    return this._repeatCount;
  }
  set repeatCount(value: number) {
    this._repeatCount = value;
  }

  get running(): boolean {
    return this._running;
  }

  reset(): void {
    this.stop();
    this._currentCount = 0;
  }

  start(): void {
    if (this._running) return;
    this._running = true;
    this._startInterval();
  }

  stop(): void {
    if (!this._running) return;
    this._running = false;
    this._stopInterval();
  }

  private _startInterval(): void {
    this._intervalId = setInterval(() => {
      this._currentCount++;
      this.dispatchEvent(new TimerEvent(TimerEvent.TIMER));
      if (this._repeatCount > 0 && this._currentCount >= this._repeatCount) {
        this.stop();
        this.dispatchEvent(new TimerEvent(TimerEvent.TIMER_COMPLETE));
      }
    }, this._delay);
  }

  private _stopInterval(): void {
    if (this._intervalId !== null) {
      clearInterval(this._intervalId);
      this._intervalId = null;
    }
  }
}

// ---------------------------------------------------------------------------
// Proxy
// ---------------------------------------------------------------------------

export class Proxy {
  callProperty(_name: string, ..._rest: any[]): any {
    return undefined;
  }

  deleteProperty(_name: string): boolean {
    return false;
  }

  getDescendants(_name: string): any {
    return undefined;
  }

  getProperty(_name: string): any {
    return undefined;
  }

  hasProperty(_name: string): boolean {
    return false;
  }

  isAttribute(_name: string): boolean {
    return false;
  }

  nextName(_index: number): string {
    return "";
  }

  nextNameIndex(_index: number): number {
    return 0;
  }

  nextValue(_index: number): any {
    return undefined;
  }

  setProperty(_name: string, _value: any): void {}
}
