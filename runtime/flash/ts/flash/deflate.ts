/**
 * RFC 1951 DEFLATE inflate/deflate + zlib wrappers for ByteArray.compress/uncompress.
 *
 * Inflate: full RFC 1951 decoder (stored, fixed Huffman, dynamic Huffman blocks).
 * Deflate: stored blocks only (valid DEFLATE, no compression). Can be upgraded later.
 */

// ---------------------------------------------------------------------------
// Bit reader for inflate
// ---------------------------------------------------------------------------

class BitReader {
  private data: Uint8Array;
  private pos: number;
  private bitBuf = 0;
  private bitCount = 0;

  constructor(data: Uint8Array, pos = 0) {
    this.data = data;
    this.pos = pos;
  }

  readBits(n: number): number {
    while (this.bitCount < n) {
      if (this.pos >= this.data.length) throw new Error("Unexpected end of deflate data");
      this.bitBuf |= this.data[this.pos++] << this.bitCount;
      this.bitCount += 8;
    }
    const val = this.bitBuf & ((1 << n) - 1);
    this.bitBuf >>>= n;
    this.bitCount -= n;
    return val;
  }

  /** Align to byte boundary (discard remaining bits in current byte). */
  alignToByte(): void {
    this.bitBuf = 0;
    this.bitCount = 0;
  }

  readByte(): number {
    this.alignToByte();
    if (this.pos >= this.data.length) throw new Error("Unexpected end of deflate data");
    return this.data[this.pos++];
  }

  readUint16LE(): number {
    const lo = this.readByte();
    const hi = this.readByte();
    return lo | (hi << 8);
  }
}

// ---------------------------------------------------------------------------
// Huffman table
// ---------------------------------------------------------------------------

interface HuffmanTable {
  counts: Uint16Array;  // number of codes of each bit length
  symbols: Uint16Array; // symbols sorted by code
}

function buildHuffmanTable(codeLens: Uint8Array | number[], maxSymbol: number): HuffmanTable {
  const MAX_BITS = 15;
  const counts = new Uint16Array(MAX_BITS + 1);
  for (let i = 0; i < maxSymbol; i++) {
    if (codeLens[i]) counts[codeLens[i]]++;
  }
  // Compute offsets.
  const offsets = new Uint16Array(MAX_BITS + 1);
  for (let i = 1; i < MAX_BITS; i++) {
    offsets[i + 1] = offsets[i] + counts[i];
  }
  const total = offsets[MAX_BITS] + counts[MAX_BITS];
  const symbols = new Uint16Array(total);
  for (let i = 0; i < maxSymbol; i++) {
    if (codeLens[i]) {
      symbols[offsets[codeLens[i]]++] = i;
    }
  }
  return { counts, symbols };
}

function decodeSymbol(br: BitReader, table: HuffmanTable): number {
  let code = 0;
  let first = 0;
  let index = 0;
  for (let len = 1; len <= 15; len++) {
    code |= br.readBits(1);
    const count = table.counts[len];
    if (code < first + count) {
      return table.symbols[index + (code - first)];
    }
    index += count;
    first = (first + count) << 1;
    code <<= 1;
  }
  throw new Error("Invalid Huffman code");
}

// ---------------------------------------------------------------------------
// Fixed Huffman tables (RFC 1951 §3.2.6)
// ---------------------------------------------------------------------------

let fixedLitLen: HuffmanTable | null = null;
let fixedDist: HuffmanTable | null = null;

function getFixedTables(): [HuffmanTable, HuffmanTable] {
  if (!fixedLitLen) {
    const lens = new Uint8Array(288);
    let i = 0;
    for (; i < 144; i++) lens[i] = 8;
    for (; i < 256; i++) lens[i] = 9;
    for (; i < 280; i++) lens[i] = 7;
    for (; i < 288; i++) lens[i] = 8;
    fixedLitLen = buildHuffmanTable(lens, 288);

    const dlens = new Uint8Array(32);
    for (i = 0; i < 32; i++) dlens[i] = 5;
    fixedDist = buildHuffmanTable(dlens, 32);
  }
  return [fixedLitLen, fixedDist!];
}

// ---------------------------------------------------------------------------
// Length and distance tables (RFC 1951 §3.2.5)
// ---------------------------------------------------------------------------

const LENGTH_BASE = [
  3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
  35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258,
];
const LENGTH_EXTRA = [
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
  3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0,
];
const DIST_BASE = [
  1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
  257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577,
];
const DIST_EXTRA = [
  0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
  7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13,
];

// Code length order for dynamic Huffman (RFC 1951 §3.2.7)
const CL_ORDER = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15];

// ---------------------------------------------------------------------------
// Inflate (raw DEFLATE, no zlib/gzip header)
// ---------------------------------------------------------------------------

export function inflateRaw(input: Uint8Array): Uint8Array {
  const br = new BitReader(input);
  const output: number[] = [];
  let bfinal = 0;

  while (!bfinal) {
    bfinal = br.readBits(1);
    const btype = br.readBits(2);

    if (btype === 0) {
      // Stored block
      br.alignToByte();
      const len = br.readUint16LE();
      const nlen = br.readUint16LE();
      if ((len ^ nlen) !== 0xffff) throw new Error("Invalid stored block lengths");
      for (let i = 0; i < len; i++) {
        output.push(br.readByte());
      }
    } else if (btype === 1 || btype === 2) {
      let litLenTable: HuffmanTable;
      let distTable: HuffmanTable;

      if (btype === 1) {
        [litLenTable, distTable] = getFixedTables();
      } else {
        // Dynamic Huffman tables
        const hlit = br.readBits(5) + 257;
        const hdist = br.readBits(5) + 1;
        const hclen = br.readBits(4) + 4;

        const clLens = new Uint8Array(19);
        for (let i = 0; i < hclen; i++) {
          clLens[CL_ORDER[i]] = br.readBits(3);
        }
        const clTable = buildHuffmanTable(clLens, 19);

        const totalCodes = hlit + hdist;
        const codeLens: number[] = [];
        while (codeLens.length < totalCodes) {
          const sym = decodeSymbol(br, clTable);
          if (sym < 16) {
            codeLens.push(sym);
          } else if (sym === 16) {
            const repeat = br.readBits(2) + 3;
            const prev = codeLens[codeLens.length - 1] || 0;
            for (let i = 0; i < repeat; i++) codeLens.push(prev);
          } else if (sym === 17) {
            const repeat = br.readBits(3) + 3;
            for (let i = 0; i < repeat; i++) codeLens.push(0);
          } else {
            const repeat = br.readBits(7) + 11;
            for (let i = 0; i < repeat; i++) codeLens.push(0);
          }
        }

        litLenTable = buildHuffmanTable(codeLens.slice(0, hlit), hlit);
        distTable = buildHuffmanTable(codeLens.slice(hlit), hdist);
      }

      // Decode symbols.
      for (;;) {
        const sym = decodeSymbol(br, litLenTable);
        if (sym === 256) break; // End of block
        if (sym < 256) {
          output.push(sym);
        } else {
          // Length-distance pair
          const lenIdx = sym - 257;
          const length = LENGTH_BASE[lenIdx] + br.readBits(LENGTH_EXTRA[lenIdx]);
          const distSym = decodeSymbol(br, distTable);
          const distance = DIST_BASE[distSym] + br.readBits(DIST_EXTRA[distSym]);
          // Copy from sliding window.
          const srcStart = output.length - distance;
          for (let i = 0; i < length; i++) {
            output.push(output[srcStart + i]);
          }
        }
      }
    } else {
      throw new Error("Invalid DEFLATE block type 3");
    }
  }

  return new Uint8Array(output);
}

// ---------------------------------------------------------------------------
// Deflate (raw DEFLATE, stored blocks only — valid but uncompressed)
// ---------------------------------------------------------------------------

export function deflateRaw(input: Uint8Array): Uint8Array {
  const MAX_BLOCK = 65535;
  const blockCount = Math.ceil(input.length / MAX_BLOCK) || 1;
  // Each block: 1 (header) + 2 (len) + 2 (nlen) + data
  const out = new Uint8Array(blockCount * 5 + input.length);
  let pos = 0;
  let offset = 0;

  while (offset < input.length || pos === 0) {
    const remaining = input.length - offset;
    const len = Math.min(remaining, MAX_BLOCK);
    const isFinal = offset + len >= input.length;

    out[pos++] = isFinal ? 0x01 : 0x00; // BFINAL=1 for last, BTYPE=00 (stored)
    out[pos++] = len & 0xff;
    out[pos++] = (len >> 8) & 0xff;
    out[pos++] = ~len & 0xff;
    out[pos++] = (~len >> 8) & 0xff;
    out.set(input.subarray(offset, offset + len), pos);
    pos += len;
    offset += len;

    if (isFinal) break;
  }

  return out.subarray(0, pos);
}

// ---------------------------------------------------------------------------
// Adler-32 checksum
// ---------------------------------------------------------------------------

function adler32(data: Uint8Array): number {
  let a = 1;
  let b = 0;
  for (let i = 0; i < data.length; i++) {
    a = (a + data[i]) % 65521;
    b = (b + a) % 65521;
  }
  return ((b << 16) | a) >>> 0;
}

// ---------------------------------------------------------------------------
// Zlib wrappers (header + raw DEFLATE + Adler-32)
// ---------------------------------------------------------------------------

export function zlibCompress(input: Uint8Array): Uint8Array {
  const raw = deflateRaw(input);
  const checksum = adler32(input);
  const out = new Uint8Array(2 + raw.length + 4);
  out[0] = 0x78; // CMF: CM=8 (deflate), CINFO=7 (32K window)
  out[1] = 0x01; // FLG: FCHECK makes CMF*256+FLG divisible by 31
  out.set(raw, 2);
  const ckPos = 2 + raw.length;
  out[ckPos] = (checksum >> 24) & 0xff;
  out[ckPos + 1] = (checksum >> 16) & 0xff;
  out[ckPos + 2] = (checksum >> 8) & 0xff;
  out[ckPos + 3] = checksum & 0xff;
  return out;
}

export function zlibDecompress(input: Uint8Array): Uint8Array {
  // Skip 2-byte zlib header.
  if (input.length < 6) throw new Error("Invalid zlib data: too short");
  const raw = input.subarray(2, input.length - 4);
  const result = inflateRaw(raw);
  // Verify Adler-32 checksum.
  const ckPos = input.length - 4;
  const expected =
    ((input[ckPos] << 24) | (input[ckPos + 1] << 16) | (input[ckPos + 2] << 8) | input[ckPos + 3]) >>> 0;
  const actual = adler32(result);
  if (actual !== expected) {
    throw new Error(`Adler-32 mismatch: expected 0x${expected.toString(16)}, got 0x${actual.toString(16)}`);
  }
  return result;
}
