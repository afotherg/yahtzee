import { loadGstbl, loadGstblFromJson, initialGameState, Categories, CategoryNames, countsOfRoll, adviceForRoll, playAiTurn, formatKeepers, applyScoreEvent, finalCategoryEVs } from './logic.js?v=__VER__';

const el = (id)=>document.getElementById(id);
const logEl = el('log');
function log(msg){ logEl.textContent += msg + '\n'; logEl.scrollTop = logEl.scrollHeight; }

let gstbl = null;
let rng = Math.random;

// Game state
let humanGS = null;
let aiGS = null;
let humanScoreCard = {}; // cat->score or null
let aiScoreCard = {};
let humanRoll = [1,1,1,1,1];
let keptMask = [false,false,false,false,false];
let rollsLeft = 3;
let humansTurn = true;
let suggestedBestCategory = null;
let showAdvice = true;
let rollingDiceMask = null; // which dice are animating this render

// Simple cookie helpers
function setCookie(name, value, days=365){
  const expires = new Date(Date.now()+days*864e5).toUTCString();
  document.cookie = name+"="+encodeURIComponent(value)+"; Expires="+expires+"; Path=/";
}
function getCookie(name){
  const m = document.cookie.match('(?:^|; )'+name.replace(/([.$?*|{}()\[\]\\/+^])/g,'\\$1')+'=([^;]*)');
  return m ? decodeURIComponent(m[1]) : null;
}

function resetScoreCard(sc){
  for (let c=0;c<=Categories.Chance;c++) sc[c]=null;
}

function sumUpper(sc){ let s=0; for (let c=0;c<=Categories.Sixes;c++) s += sc[c]||0; return s; }
function grandTotal(sc){
  // Category scores already include any US bonus or extra Yahtzee bonuses
  let s=0; for (let c=0;c<=Categories.Chance;c++) s += sc[c]||0;
  return s;
}

function renderScoreboard(target, sc){
  const container = el(target);
  container.innerHTML = '';
  for (let c=0;c<=Categories.Chance;c++) {
    const row = document.createElement('div'); row.className='row';
    const lab = document.createElement('div'); lab.className='label'; lab.textContent = CategoryNames[c];
    const val = document.createElement('div'); val.className='value'; val.textContent = sc[c]==null ? '—' : sc[c];
    row.appendChild(lab); row.appendChild(val);
    container.appendChild(row);
  }
  const totalRow = document.createElement('div'); totalRow.className='row';
  const lab = document.createElement('div'); lab.className='label'; lab.textContent = 'Total';
  const val = document.createElement('div'); val.className='value'; val.textContent = grandTotal(sc);
  totalRow.appendChild(lab); totalRow.appendChild(val);
  container.appendChild(totalRow);
}

function renderDice(){
  const dice = el('dice'); dice.innerHTML='';
  for (let i=0;i<5;i++){
    const d = document.createElement('div'); d.className='die' + (keptMask[i]?' kept':'');
    if (rollingDiceMask && rollingDiceMask[i]) d.classList.add('roll-anim');
    // Show question marks before the first roll of a turn; otherwise render pips
    const showUnknown = humansTurn && rollsLeft===3;
    renderDieFace(d, showUnknown ? null : humanRoll[i]);
    const k = document.createElement('div'); k.className='keep'; k.textContent = keptMask[i]?'kept':''; d.appendChild(k);
    d.addEventListener('click',()=>{
      if (!humansTurn || rollsLeft===3) return; // cannot keep before first roll
      if (rollsLeft===0) return;
      keptMask[i] = !keptMask[i];
      renderDice();
      updateAdvice();
    });
    dice.appendChild(d);
  }
}

function renderDieFace(container, value){
  // value: 1..6 or null for unknown
  container.innerHTML = '';
  if (value == null){
    const unk = document.createElement('div');
    unk.className = 'unknown';
    unk.textContent = '?';
    container.appendChild(unk);
    return;
  }
  // Draw pips as absolutely positioned dots inside a face container
  const face = document.createElement('div');
  face.className = 'die-face';
  const xs = [25, 50, 75];
  const ys = [25, 50, 75];
  const add = (r,c)=>{
    const p=document.createElement('div');
    p.className='pip';
    p.style.left = xs[c-1] + '%';
    p.style.top  = ys[r-1] + '%';
    face.appendChild(p);
  };
  switch (value){
    case 1: add(2,2); break;
    case 2: add(1,1); add(3,3); break;
    case 3: add(1,1); add(2,2); add(3,3); break;
    case 4: add(1,1); add(1,3); add(3,1); add(3,3); break;
    case 5: add(1,1); add(1,3); add(2,2); add(3,1); add(3,3); break;
    case 6: add(1,1); add(1,3); add(2,1); add(2,3); add(3,1); add(3,3); break;
  }
  container.appendChild(face);
}

function renderCategories(){
  const cont = el('categories'); cont.innerHTML='';
  for (let c=0;c<=Categories.Chance;c++){
    const b = document.createElement('div'); b.className='cat'; b.textContent = CategoryNames[c];
    const disabled = !humanGS.free.has(c);
    if (disabled) b.classList.add('disabled');
    if (showAdvice && !disabled && rollsLeft===0 && suggestedBestCategory===c) b.classList.add('best');
    b.addEventListener('click', ()=>{
      // Allow scoring on any roll after the first (standard Yahtzee rule)
      if (!humansTurn || disabled || rollsLeft===3) return;
      scoreHuman(c);
    });
    cont.appendChild(b);
  }
}

function setButtons(){
  el('rollBtn').disabled = !humansTurn || (rollsLeft===0 && !canRoll());
  // Enable scoring after at least one roll (rollsLeft < 3)
  el('scoreBtn').disabled = !humansTurn || (rollsLeft===3);
}

function canRoll(){ return rollsLeft>0; }

function updateAdvice(){
  const adviceEl = el('advice');
  suggestedBestCategory = null;
  if (!gstbl) { adviceEl.textContent='—'; el('adviceRow').style.display='none'; return; }
  if (!showAdvice) { adviceEl.textContent=''; el('adviceRow').style.display='none'; renderEVPanel(); return; }
  if (!humansTurn) { adviceEl.textContent='Computer is thinking…'; return; }
  if (rollsLeft===3) { adviceEl.textContent='Press Roll to start turn.'; return; }
  const adv = adviceForRoll(humanGS, humanRoll, rollsLeft, gstbl);
  if (adv.type==='keep') adviceEl.textContent = 'Keep ' + formatKeepers(adv.keep);
  else { adviceEl.textContent = 'Score ' + CategoryNames[adv.category]; suggestedBestCategory = adv.category; }
  renderEVPanel();
  el('adviceRow').style.display='';
}

function newGame(){
  humanGS = initialGameState(); aiGS = initialGameState();
  resetScoreCard(humanScoreCard); resetScoreCard(aiScoreCard);
  humansTurn = true; rollsLeft = 3; keptMask = [false,false,false,false,false];
  for (let i=0;i<5;i++) humanRoll[i]=1;
  renderAll();
  log('New game. Human starts.');
}

function renderAll(){
  renderScoreboard('humanScore', humanScoreCard);
  renderScoreboard('aiScore', aiScoreCard);
  renderDice();
  renderCategories();
  el('rollsLeft').textContent = rollsLeft;
  el('turnStatus').textContent = humansTurn ? 'Your turn' : 'Computer turn';
  setButtons();
  updateAdvice();
  const tgl = el('adviceToggle');
  if (tgl) tgl.checked = !!showAdvice;
}

function renderEVPanel(){
  const panel = el('evPanel');
  const list = el('evList');
  if (!showAdvice || !gstbl || !humansTurn || rollsLeft>0){ panel.style.display='none'; list.innerHTML=''; return; }
  const evs = finalCategoryEVs(humanGS, humanRoll, gstbl);
  list.innerHTML = '';
  for (const {category, ev} of evs){
    const div = document.createElement('div');
    div.textContent = `${CategoryNames[category]}: ${ev.toFixed(2)}`;
    if (category===suggestedBestCategory) div.style.color = '#2ecc71';
    list.appendChild(div);
  }
  panel.style.display = 'block';
}

function doRoll(){
  if (!canRoll()) return;
  // Build keepers based on keptMask; re-roll others
  const willReroll = [];
  for (let i=0;i<5;i++){
    willReroll[i] = !keptMask[i];
    if (!keptMask[i]) humanRoll[i] = 1 + Math.floor(rng()*6);
  }
  rollingDiceMask = willReroll;
  rollsLeft--;
  if (rollsLeft===2) {
    // First roll happened; now can toggle keeps
    keptMask = humanRoll.map(()=>false);
  }
  renderAll();
  // Clear animation mask so it only applies for this render
  setTimeout(()=>{ rollingDiceMask = null; }, 0);
}

function scoreHuman(cat){
  // apply scoring to humanGS; compute score with current roll
  const bag = countsOfRoll(humanRoll);
  const chosen = cat;
  const { score, gsNew } = applyScoreEvent(humanGS, bag, chosen);
  humanScoreCard[chosen] = score;
  humanGS = gsNew;
  log(`Human scores ${score} in ${CategoryNames[chosen]}`);
  // next turn to AI
  nextTurn();
}

function nextTurn(){
  // Reset human turn state
  keptMask = [false,false,false,false,false];
  rollsLeft = 3;
  for (let i=0;i<5;i++) humanRoll[i]=1;
  humansTurn = false; renderAll();
  setTimeout(()=>aiTurnAnimated(), 250);
}

function bagToKeepMask(roll, keepCounts){
  // Convert face-count keep suggestion into a mask over the current rolled dice
  const remaining = keepCounts.slice(); // length 6
  const mask = [false,false,false,false,false];
  for (let i=0;i<5;i++){
    const face = roll[i];
    const idx = face-1;
    if (remaining[idx] > 0){ mask[i] = true; remaining[idx]--; }
  }
  return mask;
}

function aiTurnAnimated(){
  // Use the main dice UI to show AI’s turn
  let roll = [0,0,0,0,0];
  for (let i=0;i<5;i++) roll[i] = 1 + Math.floor(rng()*6);
  humanRoll = roll.slice(); // display
  keptMask = [false,false,false,false,false];
  rollsLeft = 2; renderAll();

  const step1 = () => {
    const adv1 = adviceForRoll(aiGS, roll, 2, gstbl);
    keptMask = bagToKeepMask(roll, adv1.keep);
    renderAll();
    setTimeout(()=>{
      // Reroll non-kept
      const keptFaces = [];
      for (let i=0;i<5;i++) if (keptMask[i]) keptFaces.push(roll[i]);
      roll = keptFaces.slice();
      while (roll.length<5) roll.push(1+Math.floor(rng()*6));
      humanRoll = roll.slice(); keptMask = [false,false,false,false,false]; rollsLeft = 1; renderAll();
      setTimeout(step2, 500);
    }, 600);
  };

  const step2 = () => {
    const adv2 = adviceForRoll(aiGS, roll, 1, gstbl);
    keptMask = bagToKeepMask(roll, adv2.keep);
    renderAll();
    setTimeout(()=>{
      const keptFaces = [];
      for (let i=0;i<5;i++) if (keptMask[i]) keptFaces.push(roll[i]);
      roll = keptFaces.slice();
      while (roll.length<5) roll.push(1+Math.floor(rng()*6));
      humanRoll = roll.slice(); keptMask = [false,false,false,false,false]; rollsLeft = 0; renderAll();
      setTimeout(scoreStep, 600);
    }, 600);
  };

  const scoreStep = () => {
    const adv3 = adviceForRoll(aiGS, roll, 0, gstbl);
    const bag = countsOfRoll(roll);
    const { score, gsNew } = applyScoreEvent(aiGS, bag, adv3.category);
    aiScoreCard[adv3.category] = score;
    aiGS = gsNew;
    log(`AI rolled ${roll.join(' ')} and scored ${score} in ${CategoryNames[adv3.category]}`);
    // back to human
    humansTurn = true; rollsLeft = 3; keptMask = [false,false,false,false,false];
    for (let i=0;i<5;i++) humanRoll[i]=1;
    renderAll();
    checkGameEnd();
  };

  setTimeout(step1, 500);
}

function isGameOver(){ return humanGS.free.size===0 && aiGS.free.size===0; }

function checkGameEnd(){
  if (!isGameOver()) return;
  const h = grandTotal(humanScoreCard);
  const a = grandTotal(aiScoreCard);
  log(`Game over. Human ${h} vs AI ${a}. ${h>a?'Human wins!':h<a?'AI wins!':'Draw.'}`);
}

// (no local scoring mirror; app uses applyScoreEvent from logic.js)

// Wire up UI
el('newGameBtn').addEventListener('click', newGame);
el('rollBtn').addEventListener('click', doRoll);
el('scoreBtn').addEventListener('click', ()=>{
  // quick-score best category when human clicks button
  if (rollsLeft>0) return;
  const adv = adviceForRoll(humanGS, humanRoll, 0, gstbl);
  scoreHuman(adv.category);
});

(async function init(){
  const status = el('loadStatus');
  status.textContent = 'Loading optimal table…';
  const v = '__VER__';
  const jsonPaths = [ `gstbl/OptEScore-Official.json?v=${v}`, `../gstbl/OptEScore-Official.json?v=${v}` ];
  const binPaths = [ `gstbl/OptEScore-Official.gstbl?v=${v}`, `../gstbl/OptEScore-Official.gstbl?v=${v}` ];
  let lastErr = null;
  for (const p of jsonPaths) {
    try {
      gstbl = await loadGstblFromJson(p);
      status.textContent = 'Ready';
      newGame();
      return;
    } catch (e) { lastErr = e; }
  }
  for (const p of binPaths) {
    try {
      gstbl = await loadGstbl(p);
      status.textContent = 'Ready';
      newGame();
      return;
    } catch (e) { lastErr = e; }
  }
  status.textContent = 'Failed to load tables';
  console.error(lastErr);
})();
