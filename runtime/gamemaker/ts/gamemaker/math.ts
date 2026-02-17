/** GML math functions — PRNG, trig (degrees), standard math. */

import type { GameRuntime } from "./runtime";

// ---- Seedable PRNG (xorshift128) ----

const UINT32_MAX = 4294967295;
const UINT32_OFFSET = 2147483648;

class XorGen {
  x: number[];
  i: number;

  constructor(seed: number) {
    this.x = [];
    this.i = 0;

    if (seed === (seed | 0)) {
      this.x[0] = seed;
    }
    while (this.x.length < 8) this.x.push(0);
    let found = false;
    for (let j = 0; j < 8; j++) {
      if (this.x[j] !== 0) { found = true; break; }
    }
    if (!found) this.x[7] = -1;

    // Warm up
    for (let j = 0; j < 256; j++) this.next();
  }

  next(): number {
    const X = this.x;
    let i = this.i;
    let t = X[i]; t ^= (t >>> 7);
    let v = t ^ (t << 24);
    t = X[(i + 1) & 7]; v ^= t ^ (t >>> 10);
    t = X[(i + 3) & 7]; v ^= t ^ (t >>> 3);
    t = X[(i + 4) & 7]; v ^= t ^ (t << 7);
    t = X[(i + 7) & 7]; t = t ^ (t << 13); v ^= t ^ (t << 9);
    X[i] = v;
    this.i = (i + 1) & 7;
    return v;
  }
}

export class MathState {
  prng = new XorGen(0);
}

// ---- PRNG API (stateful — needs runtime) ----

export function createMathAPI(rt: GameRuntime) {
  function random_set_seed(seed: number): void {
    rt._math.prng = new XorGen(seed);
  }

  function randomize(): void {
    rt._math.prng = new XorGen(Date.now());
  }

  function random(max: number): number {
    return (rt._math.prng.next() + UINT32_OFFSET) * max / UINT32_MAX;
  }

  function random_range(min: number, max: number): number {
    return min + (rt._math.prng.next() + UINT32_OFFSET) * (max - min) / UINT32_MAX;
  }

  function irandom(max: number): number {
    const maxp1 = max + 1;
    let res: number;
    do {
      res = Math.floor((rt._math.prng.next() + UINT32_OFFSET) * maxp1 / UINT32_MAX);
    } while (res > max);
    return res;
  }

  function irandom_range(min: number, max: number): number {
    const deltap1 = max - min + 1;
    let res: number;
    do {
      res = min + Math.floor((rt._math.prng.next() + UINT32_OFFSET) * deltap1 / UINT32_MAX);
    } while (res > max);
    return res;
  }

  function choose(...args: any[]): any {
    return args[irandom(args.length - 1)];
  }

  return {
    random_set_seed, randomize, random, random_range,
    irandom, irandom_range, choose,
  };
}

// ---- Standard math (pure — no runtime needed) ----

export const { floor, ceil, round, abs, sin, cos, tan, sqrt, exp, log: ln, log2, log10, pow: power, max, min, sign } = Math;

export function frac(n: number): number { return n % 1; }
export function sqr(val: number): number { return val * val; }
export function clamp(val: number, lo: number, hi: number): number { return val < lo ? lo : val > hi ? hi : val; }
export function lerp(a: number, b: number, amt: number): number { return a * (1 - amt) + b * amt; }
export function mean(...nums: number[]): number { return nums.reduce((p, c) => p + c, 0) / nums.length; }
export function point_distance(x1: number, y1: number, x2: number, y2: number): number {
  const dx = x2 - x1, dy = y2 - y1;
  return Math.sqrt(dx * dx + dy * dy);
}
export function point_direction(x1: number, y1: number, x2: number, y2: number): number {
  return Math.atan2(y1 - y2, x2 - x1) * 180 / Math.PI;
}

// ---- Degree-based trig ----

export function dsin(val: number): number { return Math.sin(val * Math.PI / 180); }
export function dcos(val: number): number { return Math.cos(val * Math.PI / 180); }
export function dtan(val: number): number { return Math.tan(val * Math.PI / 180); }
export function darcsin(val: number): number { return Math.asin(val) * 180 / Math.PI; }
export function darccos(val: number): number { return Math.acos(val) * 180 / Math.PI; }
export function darctan(val: number): number { return Math.atan(val) * 180 / Math.PI; }
export function darctan2(y: number, x: number): number { return Math.atan2(y, x) * 180 / Math.PI; }
export function degtorad(deg: number): number { return deg * Math.PI / 180; }
export function radtodeg(rad: number): number { return rad * 180 / Math.PI; }
export function lengthdir_x(len: number, dir: number): number { return len * dcos(dir); }
export function lengthdir_y(len: number, dir: number): number { return -len * dsin(dir); }

export function logn(n: number, val: number): number { return Math.log(val) / Math.log(n); }
export function int64(n: number): number { return n | 0; }

// ---- Type conversion ----

/** Truncate to 32-bit signed integer (GML int / AS3 int). */
export function int(n: any): number { return n | 0; }
/** Truncate to 32-bit unsigned integer (AS3 uint). */
export function uint(n: any): number { return n >>> 0; }
/** Convert to number (GML real). */
export function real(n: any): number { return Number(n); }
/** Convert to string (GML string). */
export function string(n: any): string { return String(n); }
export function median(...nums: number[]): number {
  const sorted = nums.slice().sort((a, b) => a - b);
  const mid = sorted.length >> 1;
  return sorted.length % 2 === 0 ? (sorted[mid - 1] + sorted[mid]) / 2 : sorted[mid];
}
export function arctan2(y: number, x: number): number { return Math.atan2(y, x); }
