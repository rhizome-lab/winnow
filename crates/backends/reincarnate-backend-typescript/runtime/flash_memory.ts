/** Flash.Memory — Alchemy / domain memory operations (typed array access). */

const HEAP_SIZE = 1024 * 1024; // 1MB default
const heap = new ArrayBuffer(HEAP_SIZE);
const i8 = new Int8Array(heap);
const i16 = new Int16Array(heap);
const i32 = new Int32Array(heap);
const u8 = new Uint8Array(heap);
const f32 = new Float32Array(heap);
const f64 = new Float64Array(heap);

// DataView for unaligned access.
const dv = new DataView(heap);

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Flash_Memory = {
  load_i8(addr: number): number {
    return dv.getInt8(addr);
  },

  load_i16(addr: number): number {
    return dv.getInt16(addr, true);
  },

  load_i32(addr: number): number {
    return dv.getInt32(addr, true);
  },

  load_f32(addr: number): number {
    return dv.getFloat32(addr, true);
  },

  load_f64(addr: number): number {
    return dv.getFloat64(addr, true);
  },

  store_i8(addr: number, val: number): void {
    dv.setInt8(addr, val);
  },

  store_i16(addr: number, val: number): void {
    dv.setInt16(addr, val, true);
  },

  store_i32(addr: number, val: number): void {
    dv.setInt32(addr, val, true);
  },

  store_f32(addr: number, val: number): void {
    dv.setFloat32(addr, val, true);
  },

  store_f64(addr: number, val: number): void {
    dv.setFloat64(addr, val, true);
  },

  /** Sign-extend 1-bit value to i32. */
  sxi1(val: number): number {
    return (val & 1) ? -1 : 0;
  },

  /** Sign-extend 8-bit value to i32. */
  sxi8(val: number): number {
    return (val << 24) >> 24;
  },

  /** Sign-extend 16-bit value to i32. */
  sxi16(val: number): number {
    return (val << 16) >> 16;
  },
};
