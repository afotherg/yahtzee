// Core Yahtzee logic + loader for OptEScore-Official.gstbl
// Implements: game state representation, gstbl loader, scoring, DP to get optimal
// keep/score decisions given a between-turn GameState.

export const Categories = {
  Aces: 0,
  Twos: 1,
  Threes: 2,
  Fours: 3,
  Fives: 4,
  Sixes: 5,
  ThreeOfAKind: 6,
  FourOfAKind: 7,
  FullHouse: 8,
  SmallStraight: 9,
  LargeStraight: 10,
  Yahtzee: 11,
  Chance: 12,
};

export const CategoryNames = [
  'Aces','Twos','Threes','Fours','Fives','Sixes',
  '3-kind','4-kind','FullHouse','SmStraight','LgStraight',
  'Yahtzee','Chance'
];

// Official rules constants used in Pascal code
const BonusThreshold = 63;
const UpperSectionBonus = 35;
const ExtraYahtzeeBonus = 100;
const JokerRule = true;

// State shape for between-turns GameState
// free: Set of category indices still open
// usneed: int [0..63]
// chip: boolean (whether extra-Yahtzee bonus chip active; true after scoring 50 in Yahtzee)

export function initialGameState() {
  const free = new Set(Object.values(Categories));
  return { free, usneed: BonusThreshold, chip: false };
}

// Build an index into the gstbl Float64Array
// gst shape dimensions (in memory order, rightmost fastest):
// [Aces..Sixes] (6 bool) x [Three..LargeStraight] (5 bool) x [i12] (0..2) x [Chance] (bool) x [usneed 0..63]
// i12 = 2*ord(Yahtzee in free) + ord(chip)
let __useReversedBoolOrder = false;

function gstIndexFromGameState(gs) {
  const b = (cat) => gs.free.has(cat) ? 1 : 0;
  const i1 = b(Categories.Aces);
  const i2 = b(Categories.Twos);
  const i3 = b(Categories.Threes);
  const i4 = b(Categories.Fours);
  const i5 = b(Categories.Fives);
  const i6 = b(Categories.Sixes);
  const i7 = b(Categories.ThreeOfAKind);
  const i8 = b(Categories.FourOfAKind);
  const i9 = b(Categories.FullHouse);
  const i10 = b(Categories.SmallStraight);
  const i11 = b(Categories.LargeStraight);
  const yaFree = b(Categories.Yahtzee);
  const i12 = 2 * yaFree + (gs.chip ? 1 : 0);
  const i13 = b(Categories.Chance);
  const us = gs.usneed;

  // compute linear index, allowing reversed order for the 11-boolean block
  let idxBoolBlock;
  if (!__useReversedBoolOrder) {
    idxBoolBlock = (((((((((((i1<<1)|i2)<<1)|i3)<<1)|i4)<<1)|i5)<<1)|i6)<<1)|i7;
    idxBoolBlock = (((idxBoolBlock<<1)|i8)<<1)|i9;
    idxBoolBlock = (((idxBoolBlock<<1)|i10)<<1)|i11;
  } else {
    idxBoolBlock = (((((((((((i11<<1)|i10)<<1)|i9)<<1)|i8)<<1)|i7)<<1)|i6)<<1)|i5;
    idxBoolBlock = (((idxBoolBlock<<1)|i4)<<1)|i3;
    idxBoolBlock = (((idxBoolBlock<<1)|i2)<<1)|i1;
  }
  let idx = idxBoolBlock * 3 + i12;     // 0..2
  idx = (idx << 1) | i13;               // Chance
  idx = idx * 64 + us;                  // usneed
  return idx;
}

// Loader for gstbl (Float64 array length 786432)
export async function loadGstbl(url) {
  const resp = await fetch(url);
  if (!resp.ok) throw new Error(`Failed to load gstbl: ${resp.status}`);
  const buf = await resp.arrayBuffer();
  const dv = new DataView(buf);
  const n = dv.byteLength / 8;
  if (n !== 786432) console.warn('Unexpected gstbl length (doubles):', n);
  const le = new Float64Array(n);
  const be = new Float64Array(n);
  for (let i=0;i<n;i++) {
    const off = i*8;
    le[i] = dv.getFloat64(off, true);
    be[i] = dv.getFloat64(off, false);
  }
  const gsInit = initialGameState();
  const plausible = (v)=> Number.isFinite(v) && v>100 && v<400; // expected score ~250
  const combos = [];
  __useReversedBoolOrder = false; combos.push({name:'LE-normal', arr: le, val: le[gstIndexFromGameState(gsInit)], rev:false});
  __useReversedBoolOrder = true;  combos.push({name:'LE-rev',    arr: le, val: le[gstIndexFromGameState(gsInit)], rev:true});
  __useReversedBoolOrder = false; combos.push({name:'BE-normal', arr: be, val: be[gstIndexFromGameState(gsInit)], rev:false});
  __useReversedBoolOrder = true;  combos.push({name:'BE-rev',    arr: be, val: be[gstIndexFromGameState(gsInit)], rev:true});
  let chosen = combos.find(c=>plausible(c.val)) || combos[0];
  __useReversedBoolOrder = chosen.rev;
  console.log('gstbl mapping chosen:', chosen.name, 'initialEV=', chosen.val);
  return chosen.arr;
}

export async function loadGstblFromJson(url) {
  const resp = await fetch(url);
  if (!resp.ok) throw new Error(`Failed to load JSON table: ${resp.status}`);
  const j = await resp.json();
  if (!j || !Array.isArray(j.data)) {
    throw new Error('Invalid JSON table format');
  }
  // JSON is in canonical order; ensure we use normal boolean order mapping
  __useReversedBoolOrder = false;
  const arr = new Float64Array(j.data.length);
  for (let i=0;i<j.data.length;i++) arr[i] = j.data[i];
  return arr;
}

// Utility: convert a 5-dice roll to counts array [6] where sum=5
export function countsOfRoll(roll) {
  // roll: array of 5 integers in [1..6]
  const cnt = [0,0,0,0,0,0];
  for (const v of roll) cnt[v-1]++;
  return cnt;
}

// Utility: add two 6-counts arrays
function addCounts(a, b) {
  const r = new Array(6);
  for (let i=0;i<6;i++) r[i] = a[i] + b[i];
  return r;
}

// Utility: check if b is a sub-bag of B (b[i] <= B[i] for all i)
function isSubbag(b, B) {
  for (let i=0;i<6;i++) if (b[i] > B[i]) return false;
  return true;
}

// Size (weight) of a counts bag
function bagSize(b) { return b[0]+b[1]+b[2]+b[3]+b[4]+b[5]; }

// Key for Map for bag (comma-joined)
function bagKey(b) { return b.join(','); }

// Enumerate all 6-part compositions summing to m (non-negative parts)
function enumerateBagsOfSize(m, out=[]) {
  function rec(i, left, cur) {
    if (i === 5) { cur[5] = left; out.push(cur.slice()); return; }
    for (let v=0; v<=left; v++) { cur[i]=v; rec(i+1,left-v,cur); }
  }
  rec(0, m, [0,0,0,0,0,0]);
  return out;
}

// Multinomial coefficient n!/(c1!...c6!)
function multinomial(counts) {
  // n<=5 so we can compute directly
  const n = bagSize(counts);
  let num = fact(n);
  for (const c of counts) num /= fact(c);
  return num;
}
const fact = (n)=>[1,1,2,6,24,120][n];

// Precompute bags of size 0..5 and their probabilities
const BagsBySize = new Map(); // size -> array of bags
const ProbByBagSize = new Map(); // size -> Map(key->prob)
for (let m=0;m<=5;m++) {
  const arr = enumerateBagsOfSize(m);
  BagsBySize.set(m, arr);
  const map = new Map();
  const denom = Math.pow(6, m);
  for (const b of arr) {
    const p = multinomial(b) / denom;
    map.set(bagKey(b), p);
  }
  ProbByBagSize.set(m, map);
}

// Scoring for a bag
export function scoreCategory(bag, cat) {
  const weight = bag[0]*1 + bag[1]*2 + bag[2]*3 + bag[3]*4 + bag[4]*5 + bag[5]*6;
  const minNonZeroMult = Math.min(...bag.filter(x=>x>0), 5);
  const support = bag.filter(x=>x>0).length;
  const maxMult = Math.max(...bag);
  const count = (face)=>bag[face-1];
  const maxInterval = (()=>{
    // longest run of consecutive faces with count>0
    let best=0, cur=0;
    for (let i=0;i<6;i++) {
      if (bag[i]>0) { cur++; if (cur>best) best=cur; }
      else cur=0;
    }
    return best;
  })();
  switch (cat) {
    case Categories.Aces: return 1*count(1);
    case Categories.Twos: return 2*count(2);
    case Categories.Threes: return 3*count(3);
    case Categories.Fours: return 4*count(4);
    case Categories.Fives: return 5*count(5);
    case Categories.Sixes: return 6*count(6);
    case Categories.ThreeOfAKind: return maxMult>=3 ? weight : 0;
    case Categories.FourOfAKind: return maxMult>=4 ? weight : 0;
    case Categories.FullHouse: return (minNonZeroMult>=2 && support===2) ? 25 : 0;
    case Categories.SmallStraight: return maxInterval>=4 ? 30 : 0;
    case Categories.LargeStraight: return maxInterval===5 ? 40 : 0;
    case Categories.Yahtzee: return maxMult===5 ? 50 : 0;
    case Categories.Chance: return weight;
    default: return 0;
  }
}

// Determine if bag is Yahtzee: returns face category or null
function yahtzeeFaceCategory(bag) {
  const idx = bag.findIndex(c=>c===5);
  if (idx>=0) return idx; // 0..5 -> Aces..Sixes
  return null;
}

function cloneGS(gs) {
  return { free: new Set(gs.free), usneed: gs.usneed, chip: gs.chip };
}

// Apply scoring event for category on bag; returns { score, gsNew }
function applyScoreEvent(gs, bag, cat) {
  const gsNew = cloneGS(gs);
  const yaIdx = yahtzeeFaceCategory(bag); // 0..5 or null
  const isYahtzee = yaIdx!==null;

  // Joker rule applies?
  const evcat = cat;
  const jokerSpecial = (evcat===Categories.FullHouse||evcat===Categories.SmallStraight||evcat===Categories.LargeStraight);
  const upperCatOfYa = yaIdx!==null ? yaIdx : -1; // 0..5
  const yaUpperInFree = upperCatOfYa>=0 && gs.free.has(upperCatOfYa);
  const yaCatInFree = gs.free.has(Categories.Yahtzee);
  const jka = JokerRule && isYahtzee && (!yaUpperInFree) && (!yaCatInFree) && jokerSpecial;

  let baseScore = jka ? (
    evcat===Categories.FullHouse ? 25 : evcat===Categories.SmallStraight ? 30 : evcat===Categories.LargeStraight ? 40 : 0
  ) : scoreCategory(bag, evcat);

  // Update free set
  gsNew.free.delete(evcat);

  // Upper section bonus tracking
  if (evcat <= Categories.Sixes) {
    if (gsNew.usneed > 0) {
      if (baseScore < gsNew.usneed) gsNew.usneed -= baseScore;
      else { gsNew.usneed = 0; baseScore += UpperSectionBonus; }
    }
  }

  // Extra Yahtzee bonus
  if (gsNew.chip && isYahtzee) baseScore += ExtraYahtzeeBonus;

  // Chip update when scoring Yahtzee category
  if (evcat === Categories.Yahtzee) {
    gsNew.chip = (scoreCategory(bag, Categories.Yahtzee) > 0);
  }

  return { score: baseScore, gsNew };
}

// Build DP tables for a given GameState: returns object with
// - ctbl[0][B] = best score choice for last roll; also stbl[B] best category
// - ctbl[1][B], ctbl[2][B] for decisions after 1 or 2 previous keeps
// - ktbl[1][B], ktbl[2][B] argmax keepers
export function buildTurnDP(gs, gstbl) {
  // Map bagKey -> value or keepers bag
  const ctbl = [new Map(), new Map(), new Map()];
  const rtbl = [null, new Map(), new Map()];
  const stbl = new Map();
  const ktbl = [null, new Map(), new Map()];

  // Precompute ctbl[0]
  for (const B of BagsBySize.get(5)) {
    const keyB = bagKey(B);
    let opt = -1e9; let bestCat = null;
    for (let c=0;c<=Categories.Chance;c++) {
      if (!gs.free.has(c)) continue;
      const { score, gsNew } = applyScoreEvent(gs, B, c);
      const idx = gstIndexFromGameState(gsNew);
      const rem = gstbl[idx];
      const val = score + rem;
      if (val > opt) { opt = val; bestCat = c; }
    }
    ctbl[0].set(keyB, opt);
    stbl.set(keyB, bestCat);
  }

  // For i = 1..2: rtbl[i][K] = E over completions; ctbl[i][B] = max_K<=B rtbl[i][K]
  for (let i=1;i<=2;i++) {
    // rtbl for all keepers K of size 0..5
    for (const K of BagsBySize.get(0).concat(
      BagsBySize.get(1), BagsBySize.get(2), BagsBySize.get(3), BagsBySize.get(4), BagsBySize.get(5))) {
      const m = bagSize(K);
      const Emap = ProbByBagSize.get(5-m);
      let e = 0;
      for (const [kE, p] of Emap.entries()) {
        const Ebag = kE.split(',').map(x=>parseInt(x,10));
        const B = addCounts(K, Ebag);
        e += p * ctbl[i-1].get(bagKey(B));
      }
      rtbl[i].set(bagKey(K), e);
    }
    // ctbl[i][B] with stable tie-break: prefer keeping more dice
    const EPS = 1e-12;
    for (const B of BagsBySize.get(5)) {
      let opt = -1e9; let bestK = null; let bestSize = -1;
      // iterate K sizes from large to small to prefer more keeping when tied
      for (let m = 5; m >= 0; m--) {
        for (const K of BagsBySize.get(m)) {
          if (!isSubbag(K, B)) continue;
          const v = rtbl[i].get(bagKey(K));
          if (v > opt + EPS || (Math.abs(v - opt) <= EPS && m > bestSize)) {
            opt = v; bestK = K; bestSize = m;
          }
        }
      }
      ctbl[i].set(bagKey(B), opt);
      ktbl[i].set(bagKey(B), bestK);
    }
  }

  return { ctbl, rtbl, stbl, ktbl };
}

function bestCategoryFinal(gs, bag, gstbl) {
  let opt = -1e9; let bestCat = null;
  for (let c=0;c<=Categories.Chance;c++) {
    if (!gs.free.has(c)) continue;
    const { score, gsNew } = applyScoreEvent(gs, bag, c);
    const idx = gstIndexFromGameState(gsNew);
    const rem = gstbl[idx];
    const val = score + rem;
    if (val > opt) { opt = val; bestCat = c; }
  }
  return { category: bestCat, expected: opt };
}

export function finalCategoryEVs(gs, roll, gstbl) {
  const bag = countsOfRoll(roll);
  const out = [];
  for (let c=0;c<=Categories.Chance;c++) {
    if (!gs.free.has(c)) continue;
    const { score, gsNew } = applyScoreEvent(gs, bag, c);
    const idx = gstIndexFromGameState(gsNew);
    const rem = gstbl[idx];
    out.push({ category:c, ev: score + rem });
  }
  out.sort((a,b)=>b.ev-a.ev);
  return out;
}

export function adviceForRoll(gs, roll, rollsLeft, gstbl) {
  const B = countsOfRoll(roll);
  if (rollsLeft > 0) {
    const { keep, expected } = bestKeepersFor(gs, B, Math.min(rollsLeft,2), gstbl);
    return { type: 'keep', keep, expected };
  } else {
    // Compute final category directly for robustness
    const { category, expected } = bestCategoryFinal(gs, B, gstbl);
    return { type: 'score', category, expected };
  }
}

function bestKeepersFor(gs, B, rollsLeft, gstbl){
  // rollsLeft in {1,2}
  const dp = buildTurnDP(gs, gstbl);
  const EPS = 1e-12;

  if (rollsLeft === 1){
    const c0 = dp.ctbl[0];
    let bestK = null; let bestVal = -1e9; let bestSize = -1;
    for (let m=5; m>=0; m--) {
      for (const K of BagsBySize.get(m)) {
        if (!isSubbag(K, B)) continue;
        const Emap = ProbByBagSize.get(5-m);
        let e = 0;
        for (const [kE, p] of Emap.entries()){
          const Ebag = kE.split(',').map(x=>parseInt(x,10));
          const Bnext = addCounts(K, Ebag);
          e += p * c0.get(bagKey(Bnext));
        }
        if (e > bestVal + EPS || (Math.abs(e-bestVal) <= EPS && m > bestSize)){
          bestVal = e; bestK = K; bestSize = m;
        }
      }
    }
    return { keep: bestK, expected: bestVal };
  }

  // rollsLeft === 2: build c1 locally from c0, then evaluate E of c1[K+E]
  const c0 = dp.ctbl[0];
  const c1 = new Map(); // key(B) -> value
  for (const B1 of BagsBySize.get(5)){
    let opt = -1e9; let bestSize = -1;
    for (let m=5; m>=0; m--){
      for (const K2 of BagsBySize.get(m)){
        if (!isSubbag(K2, B1)) continue;
        const Emap2 = ProbByBagSize.get(5-m);
        let e2 = 0;
        for (const [kE2,p2] of Emap2.entries()){
          const E2 = kE2.split(',').map(x=>parseInt(x,10));
          const B2 = addCounts(K2, E2);
          e2 += p2 * c0.get(bagKey(B2));
        }
        if (e2 > opt + EPS || (Math.abs(e2-opt) <= EPS && m > bestSize)){
          opt = e2; bestSize = m;
        }
      }
    }
    c1.set(bagKey(B1), opt);
  }

  let bestK = null; let bestVal = -1e9; let bestSize = -1;
  for (let m=5; m>=0; m--){
    for (const K of BagsBySize.get(m)){
      if (!isSubbag(K, B)) continue;
      const Emap1 = ProbByBagSize.get(5-m);
      let e1 = 0;
      for (const [kE1,p1] of Emap1.entries()){
        const E1 = kE1.split(',').map(x=>parseInt(x,10));
        const B1 = addCounts(K, E1);
        e1 += p1 * c1.get(bagKey(B1));
      }
      if (e1 > bestVal + EPS || (Math.abs(e1-bestVal) <= EPS && m > bestSize)){
        bestVal = e1; bestK = K; bestSize = m;
      }
    }
  }
  return { keep: bestK, expected: bestVal };
}

// Play a full AI turn: simulate random dice with optimal keeps and final scoring.
export function playAiTurn(gs, rng, gstbl) {
  let roll = [0,0,0,0,0];
  // first roll
  for (let i=0;i<5;i++) roll[i] = 1 + Math.floor(rng()*6);
  let rollsLeft = 2;
  // first keep
  let adv = adviceForRoll(gs, roll, rollsLeft, gstbl);
  if (adv.type !== 'keep') throw new Error('expected keep');
  const keepCounts = adv.keep;
  // build kept faces list
  const kept = [];
  for (let face=1; face<=6; face++) {
    for (let k=0;k<keepCounts[face-1];k++) kept.push(face);
  }
  // reroll others
  for (let i=kept.length;i<5;i++) kept.push(1+Math.floor(rng()*6));
  roll = kept.slice();
  rollsLeft--;

  // second keep
  adv = adviceForRoll(gs, roll, rollsLeft, gstbl);
  if (adv.type !== 'keep') throw new Error('expected keep');
  kept.length = 0;
  for (let face=1; face<=6; face++) {
    for (let k=0;k<adv.keep[face-1];k++) kept.push(face);
  }
  for (let i=kept.length;i<5;i++) kept.push(1+Math.floor(rng()*6));
  roll = kept.slice();

  // score
  adv = adviceForRoll(gs, roll, 0, gstbl);
  const bag = countsOfRoll(roll);
  const { score, gsNew } = applyScoreEvent(gs, bag, adv.category);
  return { finalRoll: roll, category: adv.category, score, gsNew };
}

// Utility for UI: format keepers suggestion
export function formatKeepers(bag) {
  let parts = [];
  for (let i=0;i<6;i++) if (bag[i]>0) parts.push(`${i+1}×${bag[i]}`);
  return parts.length ? parts.join(' ') : '(re-roll all)';
}

// Testing helpers (explicitly exported for unit tests)
export { applyScoreEvent, gstIndexFromGameState };
