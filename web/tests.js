import {
  Categories,
  CategoryNames,
  initialGameState,
  countsOfRoll,
  adviceForRoll,
  buildTurnDP,
  formatKeepers,
  playAiTurn,
  loadGstbl,
  // test-only exports
  applyScoreEvent,
  gstIndexFromGameState,
} from './logic.js';

const out = document.getElementById('results');
const logEl = document.getElementById('log');
function log(msg){ logEl.textContent += msg + '\n'; }
function row(ok, name){ const div=document.createElement('div'); div.className=ok?'ok':'fail'; div.textContent=(ok?'✓ ':'✗ ')+name; out.appendChild(div); }
function assert(cond, msg){ if(!cond) throw new Error(msg||'assert failed'); }

// Simple zeroed gstbl for DP tests
const Z = new Float64Array(786432); // 2^11 * 3 * 2 * 64

function makeBag(vals){ const b=[0,0,0,0,0,0]; for(const v of vals) b[v-1]++; return b; }

async function run(){
  try {
    // 1) Dimensions sanity
    row(Z.length === (Math.pow(2,11)*3*2*64), 'gstbl length matches expected dimensions');

    // 2) Index mapping within range
    const gs0 = initialGameState();
    const idx0 = gstIndexFromGameState(gs0);
    row(idx0 >= 0 && idx0 < Z.length, 'gst index for initial state within range');

    // 3) Scoring: Yahtzee category yields 50 for [1,1,1,1,1]
    {
      const gs = initialGameState();
      const bag = makeBag([1,1,1,1,1]);
      const { score, gsNew } = applyScoreEvent(gs, bag, Categories.Yahtzee);
      assert(score === 50, 'Yahtzee should score 50');
      assert(gsNew.chip === true, 'Scoring Yahtzee sets chip true');
      row(true, 'Scoring: Yahtzee 50 and chip=true');
    }

    // 4) Joker rule: with Yahtzee ones and both Yahtzee and Aces not free, scoring Full House gives 25
    {
      const gs = initialGameState();
      gs.free.delete(Categories.Yahtzee);
      gs.free.delete(Categories.Aces);
      const bag = makeBag([1,1,1,1,1]);
      const { score } = applyScoreEvent(gs, bag, Categories.FullHouse);
      assert(score === 25, 'Joker Full House should be 25');
      row(true, 'Joker rule: Full House=25 when Yahtzee + upper used');
    }

    // 5) Upper bonus award when crossing threshold
    {
      const gs = initialGameState();
      gs.usneed = 3;
      const bag = makeBag([1,1,1,1,1]); // Aces=5
      const { score, gsNew } = applyScoreEvent(gs, bag, Categories.Aces);
      assert(score === 5 + 35, 'Upper bonus adds 35 when threshold met');
      assert(gsNew.usneed === 0, 'usneed becomes 0 after awarding');
      row(true, 'Upper section bonus when crossing threshold');
    }

    // 6) DP final-roll best category checks (gstbl zero => immediate scoring)
    {
      const gs = initialGameState();
      // Large straight 2-3-4-5-6
      const roll = [2,3,4,5,6];
      const dp = buildTurnDP(gs, Z);
      const Bkey = countsOfRoll(roll).join(',');
      // best cat should be LargeStraight (40)
      const cat = dp.stbl.get(Bkey);
      assert(cat === Categories.LargeStraight, 'Best category is Large Straight');
      row(true, 'DP: Large Straight best on 2-3-4-5-6 (final roll)');
    }

    // 6b) Final-roll best category on Full House (2-2-2-6-6) is Full House
    {
      const gs = initialGameState();
      const roll = [2,2,2,6,6];
      const adv = adviceForRoll(gs, roll, 0, Z);
      assert(adv.type==='score' && adv.category===Categories.FullHouse, 'Best category should be Full House');
      row(true, 'Final-roll: 2-2-2-6-6 -> Full House');
    }

    // 6c) If Full House is not free, prefer Three-of-a-kind (18) over Twos (6)
    {
      const gs = initialGameState();
      gs.free.delete(Categories.FullHouse);
      const roll = [2,2,2,6,6];
      const adv = adviceForRoll(gs, roll, 0, Z);
      assert(adv.category===Categories.ThreeOfAKind || adv.category===Categories.Chance, 'Expect 3-kind or Chance (both 18) when Full House unavailable');
      row(true, 'Final-roll: 2-2-2-6-6 without Full House -> prefers 3-kind/Chance over Twos');
    }

    // 7) Advice keepers: for Yahtzee 1s on first/second roll, keep all
    {
      const gs = initialGameState();
      const roll = [1,1,1,1,1];
      const adv = adviceForRoll(gs, roll, 2, Z);
      const keep = adv.keep;
      assert(keep[0] === 5, 'Keep all ones');
      row(true, 'Advice: keep all on Yahtzee roll');
    }

    // 8) Basic AI turn simulation with zero table runs without error
    {
      const gs = initialGameState();
      const rng = Math.random;
      const res = playAiTurn(gs, rng, Z);
      assert(res.score >= 0, 'AI scores non-negative with zero table');
      row(true, 'AI turn simulation runs');
    }

    // 9) Keeper advice on first roll: 6,1,6,6,6 should keep four 6s (not reroll all)
    {
      const gs = initialGameState();
      const roll = [6,1,6,6,6];
      const adv = adviceForRoll(gs, roll, 2, Z);
      const keep = adv.keep;
      const keptSixes = keep[5];
      assert(keptSixes === 4, `Expected keep four 6s, got ${keptSixes}`);
      row(true, 'Advice: first roll 6,1,6,6,6 -> keep 6×4');
    }

    // 10) Reported case: first roll 1,4,3,5,4 should not advise reroll all
    {
      const gs = initialGameState();
      const roll = [1,4,3,5,4];
      const adv = adviceForRoll(gs, roll, 2, Z);
      const keep = adv.keep;
      const keptCount = keep.reduce((a,b)=>a+b,0);
      assert(keptCount > 0, 'Advice should keep something (not reroll all)');
      row(true, 'Advice: first roll 1,4,3,5,4 keeps at least one die');
    }

    // 11) Reported: first roll 3,3,6,5,6 should keep two sixes (structural check with zero table)
    {
      const gs = initialGameState();
      const roll = [3,3,6,5,6];
      const adv = adviceForRoll(gs, roll, 2, Z);
      const keep = adv.keep;
      // With zero table, exact EV match may differ; ensure it keeps at least one 6.
      assert(keep[5] >= 1, `Expected to keep at least one 6, got ${keep[5]}`);
      row(true, 'Advice: first roll 3,3,6,5,6 -> keeps at least one 6 (zero table)');
    }

    log('All tests executed.');
  } catch (e) {
    row(false, e.message || e);
    console.error(e);
  }
}

run();

// Extended tests using the real gstbl to verify table interpretation
(async function runWithTable(){
  try {
    const gstbl = await loadGstbl('../gstbl/OptEScore-Official.gstbl');
    row(true, 'Loaded real gstbl for mapping checks');

    // Final-state entries should be ~0 for any usneed/chip/ya-free combination where free=[]
    {
      const gs = initialGameState();
      gs.free.clear();
      gs.usneed = 0;
      gs.chip = false;
      const idx = gstIndexFromGameState(gs);
      const v = gstbl[idx];
      assert(Math.abs(v) < 1e-6, `Final-state EV should be 0, got ${v}`);
      row(true, 'Mapping: free=[] state maps to ~0 EV');
    }

    // Inspect EV at InitialGameState directly from table
    {
      const gs = initialGameState();
      const idx = gstIndexFromGameState(gs);
      const v = gstbl[idx];
      log('EV(gstbl[InitialGameState]) = ' + v);
    }

    // Advice with real table: first roll 3,3,6,5,6 -> keep 6x2
    {
      const gs = initialGameState();
      const roll = [3,3,6,5,6];
      const adv = adviceForRoll(gs, roll, 2, gstbl);
      const keep = adv.keep;
      assert(keep[5] === 2, `With table: expected keep 6x2, got ${keep[5]}`);
      row(true, 'With table: first roll 3,3,6,5,6 -> keep 6×2');
    }

    // With real table: final roll 2,2,2,6,6 -> Full House (if free)
    {
      const gs = initialGameState();
      const roll = [2,2,2,6,6];
      const adv = adviceForRoll(gs, roll, 0, gstbl);
      assert(adv.category===Categories.FullHouse, `With table: expected Full House, got ${CategoryNames[adv.category]}`);
      row(true, 'With table: final 2-2-2-6-6 -> Full House');
    }

    // With real table: first roll 2,2,2,1,2 -> keep 2x4
    {
      const gs = initialGameState();
      const roll = [2,2,2,1,2];
      const adv = adviceForRoll(gs, roll, 2, gstbl);
      const keep = adv.keep;
      assert(keep[1] === 4, `With table: expected keep 2x4, got ${keep[1]}`);
      row(true, 'With table: first roll 2,2,2,1,2 -> keep 2×4');
    }

  } catch (e) {
    row(false, 'Real-table checks failed: ' + (e.message||e));
    console.error(e);
  }
})();

// Debug helper: compute and print EVs for all keepers on a given roll
async function debugKeeperEVs(roll, rollsLeft){
  try {
    const gstbl = await loadGstbl('../gstbl/OptEScore-Official.gstbl');
    const gs = initialGameState();
    const { adviceForRoll } = await import('./logic.js');
    const { default: _ } = { default: null };
    const B = countsOfRoll(roll);
    const m = rollsLeft;
    // Rebuild DP and target table
    const { buildTurnDP } = await import('./logic.js');
    const dp = buildTurnDP(gs, gstbl);
    const targetTbl = dp.ctbl[m-1];

    function addCounts(A,B){ const r=new Array(6); for(let i=0;i<6;i++) r[i]=(A[i]||0)+(B[i]||0); return r; }
    function bagKey(b){ return b.join(','); }
    const BagsBySize = new Map();
    function enumerate(m){ const out=[]; (function rec(i,left,cur){ if(i===5){cur[5]=left; out.push(cur.slice()); return;} for(let v=0;v<=left;v++){cur[i]=v; rec(i+1,left-v,cur);} })(0,m,[0,0,0,0,0,0]); return out; }
    for(let k=0;k<=5;k++) BagsBySize.set(k, enumerate(k));
    function isSubbag(b,B){ for(let i=0;i<6;i++) if(b[i]>B[i]) return false; return true; }
    function multinomial(counts){ const f=[1,1,2,6,24,120]; let n=counts.reduce((a,b)=>a+b,0); let num=f[n]; for(const c of counts) num/=f[c]; return num; }
    const ProbBySize = new Map();
    for(let k=0;k<=5;k++){ const map=new Map(); const arr=BagsBySize.get(k); const denom=Math.pow(6,k); for(const b of arr){ const p=multinomial(b)/denom; map.set(bagKey(b), p);} ProbBySize.set(k,map); }

    const rows=[];
    for(let sz=5; sz>=0; sz--){
      for(const K of BagsBySize.get(sz)){
        if(!isSubbag(K,B)) continue;
        let e=0; const Emap=ProbBySize.get(5-sz);
        for(const [kE,p] of Emap.entries()){
          const Eb=kE.split(',').map(x=>parseInt(x,10));
          const Bn=addCounts(K,Eb);
          e += p * (targetTbl.get(bagKey(Bn)) || 0);
        }
        rows.push({K,ev:e});
      }
    }
    rows.sort((a,b)=>b.ev-a.ev);
    log('Keeper EVs for roll '+roll.join(',')+' with '+rollsLeft+' rolls left:');
    for(const r of rows.slice(0,10)){
      const keep = r.K.map((c,i)=> c? (i+1)+'×'+c : null).filter(Boolean).join(' ');
      log('  '+(keep||'(none)')+' -> '+r.ev.toFixed(5));
    }
  } catch (e) {
    log('debugKeeperEVs error: '+(e.message||e));
  }
}
// Uncomment to debug specific cases in the console:
// debugKeeperEVs([3,3,6,5,6], 2);
// debugKeeperEVs([2,2,2,1,2], 2);

// Expose helper to window for console use
// Usage in console on tests page:
//   debugKeeperEVs([3,3,6,5,6], 2)
//   debugKeeperEVs([2,2,2,1,2], 2)
window.debugKeeperEVs = debugKeeperEVs;
