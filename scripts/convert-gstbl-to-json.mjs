#!/usr/bin/env node
/**
 * Convert a Yahtzee .gstbl binary table to a canonical-order JSON file.
 *
 * Usage:
 *   node scripts/convert-gstbl-to-json.mjs <input.gstbl> <output.json>
 *
 * The converter:
 * - Reads the binary as doubles in both LE and BE
 * - Calibrates which endianness + boolean-block order yields a plausible
 *   expected value at InitialGameState (~250)
 * - Remaps values into a canonical linear index where the 11 booleans are
 *   packed in normal order (Aces..Sixes, 3-kind..LgStraight), followed by
 *   i12 (0..2), Chance (0/1), and usneed (0..63)
 */

import { readFile, writeFile } from 'node:fs/promises';
import { basename } from 'node:path';

if (process.argv.length < 4) {
  console.error('Usage: node scripts/convert-gstbl-to-json.mjs <input.gstbl> <output.json>');
  process.exit(1);
}

const inputPath = process.argv[2];
const outputPath = process.argv[3];

function dvFromBuffer(buf) {
  const ab = buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength);
  return new DataView(ab);
}

function gstIndexFromFlags(flags, rev) {
  // flags: {i1..i11, i12, i13, us}
  const { i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11, i12, i13, us } = flags;
  let idxBool;
  if (!rev) {
    idxBool = (((((((((((i1<<1)|i2)<<1)|i3)<<1)|i4)<<1)|i5)<<1)|i6)<<1)|i7;
    idxBool = (((idxBool<<1)|i8)<<1)|i9;
    idxBool = (((idxBool<<1)|i10)<<1)|i11;
  } else {
    idxBool = (((((((((((i11<<1)|i10)<<1)|i9)<<1)|i8)<<1)|i7)<<1)|i6)<<1)|i5;
    idxBool = (((idxBool<<1)|i4)<<1)|i3;
    idxBool = (((idxBool<<1)|i2)<<1)|i1;
  }
  let idx = idxBool * 3 + i12;
  idx = (idx << 1) | i13;
  idx = idx * 64 + us;
  return idx;
}

function initialFlags() {
  // InitialGameState: all categories free, usneed=63, chip=false
  return { i1:1,i2:1,i3:1,i4:1,i5:1,i6:1, i7:1,i8:1,i9:1,i10:1,i11:1,
           i12: 2*1 + 0, i13:1, us:63 };
}

function plausibleEV(x) {
  return Number.isFinite(x) && x > 100 && x < 400;
}

function* flagIterator() {
  for (let i1=0;i1<=1;i1++)
  for (let i2=0;i2<=1;i2++)
  for (let i3=0;i3<=1;i3++)
  for (let i4=0;i4<=1;i4++)
  for (let i5=0;i5<=1;i5++)
  for (let i6=0;i6<=1;i6++)
  for (let i7=0;i7<=1;i7++)
  for (let i8=0;i8<=1;i8++)
  for (let i9=0;i9<=1;i9++)
  for (let i10=0;i10<=1;i10++)
  for (let i11=0;i11<=1;i11++)
  for (let i12=0;i12<=2;i12++)
  for (let i13=0;i13<=1;i13++)
  for (let us=0;us<=63;us++) {
    yield { i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11, i12, i13, us };
  }
}

async function main() {
  const buf = await readFile(inputPath);
  const dv = dvFromBuffer(buf);
  const n = dv.byteLength / 8;
  if (n !== 786432) {
    console.warn('Warning: unexpected #doubles:', n, '(expected 786432)');
  }
  const le = new Float64Array(n);
  const be = new Float64Array(n);
  for (let i=0;i<n;i++) {
    const off = i*8;
    le[i] = dv.getFloat64(off, true);
    be[i] = dv.getFloat64(off, false);
  }

  // Calibrate endianness + boolean-block order using InitialGameState
  const init = initialFlags();
  const combos = [
    { name: 'LE-normal', arr: le, rev: false },
    { name: 'LE-rev',    arr: le, rev: true  },
    { name: 'BE-normal', arr: be, rev: false },
    { name: 'BE-rev',    arr: be, rev: true  },
  ];
  let chosen = combos[0];
  for (const c of combos) {
    const v = c.arr[gstIndexFromFlags(init, c.rev)];
    if (plausibleEV(v)) { chosen = c; break; }
  }
  console.log('Chosen mapping:', chosen.name, 'initEV=', chosen.arr[gstIndexFromFlags(init, chosen.rev)]);

  // Remap to canonical order (normal boolean order) into linear array
  const out = new Float64Array(n);
  let i = 0;
  for (const f of flagIterator()) {
    const srcIdx = gstIndexFromFlags(f, chosen.rev);
    out[i++] = chosen.arr[srcIdx];
  }

  const payload = {
    version: 1,
    source: basename(inputPath),
    order: 'canonical-boolean-order',
    length: out.length,
    data: Array.from(out, (x)=> Number.isFinite(x) ? +x : null),
  };
  await writeFile(outputPath, JSON.stringify(payload));
  console.log('Wrote JSON:', outputPath, 'size=', (await (await import('node:fs/promises')).stat(outputPath)).size, 'bytes');
}

main().catch((e)=>{ console.error(e); process.exit(1); });

