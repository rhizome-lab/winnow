/**
 * flash.utils package — ByteArray, Timer, Proxy.
 */

import { EventDispatcher } from "./flash_display";
import { TimerEvent } from "./flash_events";

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
    // AMF decoding is not implemented; return null.
    return null;
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

  writeMultiByte(value: string, _charSet: string): void {
    // TextEncoder only supports UTF-8; other charsets are not available.
    this.writeUTFBytes(value);
  }

  writeObject(_object: any): void {
    // AMF encoding is not implemented.
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

  compress(_algorithm = "zlib"): void {
    // Compression requires a zlib implementation; no-op in this stub.
  }

  uncompress(_algorithm = "zlib"): void {
    // Decompression requires a zlib implementation; no-op in this stub.
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
