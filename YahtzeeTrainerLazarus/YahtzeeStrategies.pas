unit      YahtzeeStrategies;
  { Various strategies to play Yahtzee,
    including optimized for expected final score and for minimum score }

  { Copyright (c) 1999-2005 by Tom Verhoeff.

    This file is part of YahtzeeTrainer.  YahtzeeTrainer helps play and analyze
    solitaire games of Yahtzee.

    YahtzeeTrainer is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    YahtzeeTrainer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scronpy; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Author's contact information: <mailto:Tom.Verhoeff@acm.org>.
  }

{$MODE Delphi}{$H+}

interface

uses
  GeneralStuff,
  DiceStuff,
  YahtzeeConcepts,
  YahtzeeStateTables;
  
const
  MaxShortNameLength = 5; { determined by column width in WriteGameRecording! }

type
  StrategyType = (Manual, SimpleMinded, OptimalEScore, OptimalMScore,
    KarstenAndTobias);
  
  AuxCTable = array [ ValueBagRank ] of Category;
  AuxRTable = array [ ValueBagRank ] of ValueBagRank;

  CacheState = record { to speed up analysis and optimization }
    cName: stdstring;
    target: Aspect;
    cGS: GameState;
      { rtbl, ctbl (and in some cases also ktbl and stbl)
        store values for game state cGS, if cGS.free<>[ ] }
    rtbl: array [ 1..MaxRollsPerTurn ] of AuxETable;
      { rtbl[i][r] = value in turn state (i, ToRoll, r) }
    ctbl: array [ 0..MaxModificationAttempts ] of AuxETable;
      { ctbl[i][r] = value in turn state (i, ToChoose, r) }
    ktbl: array [ 1..MaxModificationAttempts ] of AuxRTable;
      { ktbl[i][r] = keepers for keeping in turn state (i, ToChoose, r) }
    stbl: AuxCTable;
      { stbl[r] = category for scoring in turn state (0, ToChoose, r) }
    gstbl: GameStateTable;
    nKnownEntries: Cardinal;
      { line added in YahtzeeTrainer 1.2 for progressbar }
      { # Known entries in gstbl, also equals NKnownGameStateTable(gstbl)}
  end; { record }

  Cache = ^CacheState;

  YahtzeeStrategyState = record 
    name, shortname: stdstring; { length(shortname) <= MaxShortNameLength }
    ysYR: YahtzeeRules;
    ECache: Cache; { for ExpectedScore }
    MCache: Cache; { for MinimumScore }
    ZCache: Cache; { for ZeroScoreProbability }
    VCache: Cache; { for VarianceScore }
    case stype: StrategyType of
      Manual: (
        pGS: GameState; { previous game state, for prompting }
        )
    ; SimpleMinded: ( )
    ; OptimalEScore: (
        OptECache: Cache; { consider possibility to share with ECache }
        )
    ; OptimalMScore: (
        OptMCache: Cache; { consider possibility to share with MCache }
        )
    ; KarstenAndTobias: ( )
    end; { record }

  YahtzeeStrategy = ^YahtzeeStrategyState;

var
  ManualYS,
  SimpleYS,
  OptimalYS,
  OptMinYS,
  KarstenAndTobiasYS: YahtzeeStrategy;

procedure CreateCache (var {out} C: Cache);
procedure InitCache (var C: Cache; nm: stdstring; t: Aspect; fname: stdstring);
function  CacheDescription (C: Cache): stdstring;

procedure InitYahtzeeStrategy ({out} YS: YahtzeeStrategy);
procedure CreateYahtzeeStrategy (var {out} YS: YahtzeeStrategy;
                                 nm, shnm: stdstring; st: StrategyType;
                                 YR: YahtzeeRules);

function  OptExpectedScore (GS: GameState): real;
procedure EnsureOptECache (GS: GameState);
function  OptEScore (GS: GameState; TS: TurnState): real;
procedure AllOptExpectedScores (YS: YahtzeeStrategy);
function  OptMinimumScore (YS: YahtzeeStrategy;
                           GS: GameState; TS: TurnState): real;
procedure UpdateOptMCache (YS: YahtzeeStrategy; GS: GameState);

procedure StrategyChoice (YS: YahtzeeStrategy;
                          GS: GameState; TS: TurnState; total: integer;
                          var {out} ev: Event; verbose: boolean);

//procedure InitYahtzeeStrategies;

implementation

uses
  Main { for progressbar added in YahtzeeTrainer 1.2 };

procedure CreateCache (var {out} C: Cache);
  { pre:  C~=nil ==> C~ unallocated
    post: C allocated }
  begin
    if C = nil then begin
      new(C)
    ; if not(C <> nil) then Fail('CreateCache', 'C <> nil')
    end { if }
  end; { CreateCache }

procedure InitCache (var C: Cache; nm: stdstring; t: Aspect; fname: stdstring);
  { pre:  C~=nil ==> C~ unallocated
    post: C allocated /\ C^.cName = nm /\ C^.target = t /\
          C^.gstbl initialized to Unknown if fname = ''
            else read from file fname }
  begin
    CreateCache(C)
  ; with C^ do begin
      cName := nm
    ; target := t
    ; cGS.free := [ ]
    ; if fname = '' then begin
        InitGameStateTable(gstbl)
      ; nKnownEntries := 0
          { line added in YahtzeeTrainer 1.2 for progressbar }
      end
      else begin
        LoadGameStateTable(fname, gstbl)
      ; nKnownEntries := NKnownGameStateTable(gstbl)
          { line added in YahtzeeTrainer 1.2 for progressbar }
      end { if fname }
    end { with C^ }
  end; { InitCache }

function  CacheDescription (C: Cache): stdstring;
  { pre: C allocated }
  begin
    if not(C<>nil) then Fail('WriteCacheDescription', 'C<>nil')
  ; with C^ do
      CacheDescription := cName + ' for ' + StringFromAspect(target)
  end; { CacheDescription }

procedure InitYahtzeeStrategy ({out} YS: YahtzeeStrategy);
  { pre: YS^ has been allocated }
  begin
    with YS^ do begin
      case stype of
        Manual: pGS.free := [ ]
      ; SimpleMinded: { skip }
      ; OptimalEScore: { skip }
      ; OptimalMScore: { skip }
      ; KarstenAndTobias: { skip }
      end { case stype }
    end { with YS^ }
  end; { InitYahtzeeStrategy }

procedure CreateYahtzeeStrategy (var {out} YS: YahtzeeStrategy;
                                 nm, shnm: stdstring; st: StrategyType;
                                 YR: YahtzeeRules);
  begin
    new(YS)
  ; if not(YS<>nil) then Fail('CreateYahtzeeStrategy', 'YS<>nil')
  ; with YS^ do begin
      name := nm
    ; if not(length(shnm)<=MaxShortNameLength) then
        Fail('CreateYahtzeeStrategy',
            'length(shnm)<=MaxShortNameLength')
    ; shortname := shnm
    ; if not(YR<>nil) then Fail('CreateYahtzeeStrategy', 'YR<>nil')
    ; ysYR := YR
    ; ECache := nil
    ; MCache := nil
    ; ZCache := nil
    ; VCache := nil
    ; stype := st
    end { with }
  ; InitYahtzeeStrategy(YS)
  end; { CreateYahtzeeStrategy }

function  OptExpectedScore (GS: GameState): real;
  { glob var: OptimalYS }
  { pre: true
    ret: optimal expected score based on OptECache^.gstbl }
  var
    TS: TurnState;
    ev: Event;
    c: Category;
    r: ValueBagRank;
    dSC: ScoreCard;
    GSnew: GameState;
    h, opt: real;
    ci: ValueBagCompletionIndex;
    sbi: ValueSubBagIndex;
    ertbl, { actually only ValueBags of size 5 used as index for ertbl }
    ectbl: AuxETable; { MUST be local because of recursion! }

  begin { OptExpectedScore }
    with GS, TS, ev, dSC, OptimalYS^,
         OptECache^.gstbl[ Aces in free, Twos in free, Threes in free,
                Fours in free, Fives in free, Sixes in free,
                ThreeOfAKind in free, FourOfAKind in free,
                FullHouse in free,
                SmallStraight in free, LargeStraight in free,
                2*ord(Yahtzee in free)+ord(chip), Chance in free, usneed
              ] do begin
      if x < 0 then begin { not yet known }
        if free = [ ] {IsFinalGameState(GS)} then { no further score to earn }
          x := 0.0
        else begin { compute optimal expected partial score (backwards) }

          rollsleft := 0
{       ; phase := ToChoose }

          { compute for all ValueBags B of size NDice (dice after last roll):
              ect[B] = (max c: c in free: EventScore(B, c)+OptE(GSnew)) }
          ; kind := Scoring
        ; with ValueBagRanks[NDice] do
            for r := firstvbr to lastvbr do begin { r repr. B }
              tsroll := r
            ; opt := -maxint
            ; for c := FirstCategory to LastCategory do begin
                evcat := c
              ; if c in free then begin
                  ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
                  { N.B. Must determine GSnew before calling OptExpectedScore,
                    hence h := EventScore(..., GSnew) + OptExpectedScore(GSnew)
                    is not good enough }
                ; h := box[OptECache^.target] + OptExpectedScore(GSnew)
                ; if h > opt then opt := h
                end { if }
              end { for c }
            ; ectbl[tsroll] := opt
            end { for r }
{
;if CardinalityCategorySet(free)>1 then
  WriteAuxETable(errout, 'ect', ect, NValueBag4Ranks, MaxValueBagRank)
}

        ; while rollsleft <> NModificationAttempts do begin

            rollsleft := succ(rollsleft)
{         ; phase := ToRoll }

            { compute for all ValueBags K ("Keepers"):
                ertbl[K] = (+ E: #K+#E=NDice: Pr(E) * ectbl[K+E]) }
          ; for r := 0 to MaxValueBagRank do begin { r repr. K }
              with ValueBagCompletionIndices[r] do begin
                h := 0.0
              ; for ci := firstvbci to lastvbci do { ci repr. E, K+E }
                  with ValueBagCompletions[ci] do
                    h := h + cpr*ectbl[sum]
              ; ertbl[r] := h
              end { with }
            end { for r }

{         ; phase := ToChoose }

            { compute for all ValueBags B of size NDice:
                ectbl[B] = (max K: K <= B: ertbl[K]) }
          ; for r := NValueBag4Ranks to MaxValueBagRank do begin { r repr. B }
              with ValueSubBagIndices[r] do begin
                opt := -maxint
              ; for sbi := firstvsbi to lastvsbi do begin { sbi repr. K }
                  h := ertbl[ValueSubBags[sbi].sr]
                ; if h > opt then opt := h
                end { for sbi }
              ; ectbl[r] := opt
              end { with }
            end { for r }

          end { while }

{       ; rollsleft := succ(rollsleft) }
{       ; phase := ToRoll }

          { compute optimal expected score as
              (+ B: #B=NDice: Pr(B) * ectbl[B]) }
        ; with ValueBagRanks[NDice] do begin
            opt := 0.0
          ; for r := firstvbr to lastvbr do { r repr. B }
              with ValueBagTbl[r]{: B} do
                opt := opt + {B.}pr*ectbl[r]
          end { with }

        ; x := opt
        end { else }
        { x has been determined }
      ; with OptECache^ do begin
          nKnownEntries := nKnownEntries + 1
        ; if (nKnownEntries mod 1024) = 0 then
            TableForm.PBOptE.StepIt
        end { with OptECache }
        { with clausule added in YahtzeeTrainer 1.2 for progressbar }
      end { if }
      { x is known }
    ; OptExpectedScore := x
    end { with }
  end; { OptExpectedScore }

procedure EnsureOptECache (GS: GameState);
  { glob var: OptimalYS }
  { pre:  OptimalYS^.stype = OptimalEScore /\ not IsFinalGameState(GS~)
    post: OptimalYS^.OptECache is up to date for GS }
  var
    TS: TurnState;
    ev: Event;
    c, c_opt: Category;
    r, sr_opt: ValueBagRank;
    dSC: ScoreCard;
    GSnew: GameState;
    h, opt: real;
    ci: ValueBagCompletionIndex;
    sbi: ValueSubBagIndex;
  begin
    if not IsEqualGameState(GS, OptimalYS^.OptECache^.cGS) then begin
      with OptimalYS^, OptECache^, GS, TS, ev, dSC do begin
        { compute optimal auxiliary tables for cache }

        rollsleft := 0
      ; phase := ToChoose

        { compute for all ValueBags B3 of size NDice (dice after last roll):
            ct[0,B] = (max c: c in free: EventScore(B, c)+OptE(GSnew)) }
      ; kind := Scoring
      ; with ValueBagRanks[NDice] do
          for r := firstvbr to lastvbr do begin { r repr. B }
            tsroll := r
          ; opt := -maxint
          ; for c := FirstCategory to LastCategory do begin
              evcat := c
            ; if c in free then begin
                ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
                { N.B. Must determine GSnew before calling OptExpectedScore,
                  hence h := EventScore(..., GSnew) + OptExpectedScore(GSnew)
                  is not good enough }
              ; h := box[target] + OptExpectedScore(GSnew)
              ; if h > opt then begin
                  opt := h
                ; c_opt := c
                end { if }
              end { if }
            end { for c }
          ; ctbl[rollsleft, r] := opt
          ; stbl[r] := c_opt
          end { for r }

      ; while rollsleft <> NModificationAttempts do begin

          rollsleft := succ(rollsleft)
{       ; phase := ToRoll }

          { compute for all ValueBags K2 ("Keepers" after roll ma):
              rt[i,K] = (+ E: #K+#E=NDice: Pr(E) * ct[i-1,K+E]) }
        ; for r := 0 to MaxValueBagRank do begin { r repr. K }
            with ValueBagCompletionIndices[r] do begin
              h := 0.0
            ; for ci := firstvbci to lastvbci do { ci repr. E, K+E }
                with ValueBagCompletions[ci] do
                  h := h + cpr*ctbl[pred(rollsleft), sum]
            ; rtbl[rollsleft, r] := h
            end { with }
          end { for r }

{       ; phase := ToChoose }

          { compute for all ValueBags B of size NDice:
              ct[rollsleft,B] = (max K: K <= B: rt[rollsleft,K]) }
        ; for r := NValueBag4Ranks to MaxValueBagRank do begin { r repr. B }
            with ValueSubBagIndices[r] do begin
              opt := -maxint
            ; for sbi := lastvsbi downto firstvsbi do
                              { prefer to keep as much as possible }
                with ValueSubBags[sbi] do begin { sr repr. K }
                h := rtbl[rollsleft, sr]
              ; if h > opt then begin
                  opt := h
                ; sr_opt := sr
                end { if }
              end { for sbi }
            ; ctbl[rollsleft, r] := opt
            ; ktbl[rollsleft, r] := sr_opt
            end { with }
          end { for r }

        end { while }

      ; rollsleft := succ(rollsleft)
{     ; phase := ToRoll }

        { compute optimal expected score as
            rt[rollsleft,0] = (+ B: #B=NDice: Pr(B)*ct[rollsleft-1,B]) }
      ; with ValueBagRanks[NDice] do begin
          opt := 0.0
        ; for r := firstvbr to lastvbr do { r repr. B }
            with ValueBagTbl[r] do
              opt := opt + pr*ctbl[pred(rollsleft), r]
        ; rtbl[rollsleft, EmptyValueBagRank] := opt
        end { with }

      ; cGS := GS
      end { with }
    end { if }
    { OptECache is up to date for GS }
  end; { EnsureOptECache }

function  OptEScore (GS: GameState; TS: TurnState): real;
  { glob var: OptimalYS }
  { pre: OptimalYS^.stype = OptimalEScore
    ret: expected final score in state GS~, TS~ }
  { side effect: cache updated }
  begin
  ; EnsureOptECache(GS) { actually there is no need to waste cache
      if TS=initial turn state }
  ; with OptimalYS^, OptECache^, GS, TS do begin
    ; case phase of
        ToRoll:   OptEScore := rtbl[rollsleft, tskeepers]
      ; ToChoose: OptEScore := ctbl[rollsleft, tsroll]
      end { case }
    end { with }
  end; { OptEScore }

procedure AllOptExpectedScores (YS: YahtzeeStrategy);
  { pre:  true
    post: YS^.OptECache^.gstbl is completely determined }
  var GS: GameState; x: real;
  begin
    GS := InitialGameState
  ; repeat
      x := OptExpectedScore(GS)
    until not NextGameState(GS)
  end; { AllOptExpectedScores }

function  OptMinimumScore (YS: YahtzeeStrategy;
                           GS: GameState; TS: TurnState): real;
  { pre: YS~^.OptMCache^ properly initialized
    ret: opimal minimum final score of YS~ in state GS~,TS~ }
  begin
    with YS^, OptMCache^, TS do begin
      if not IsEqualGameState(GS, cGS) or (GS.free = [ ]) then begin
        { OptMCache not up-to-date }
        if rollsleft = NRollsPerTurn {IsInitialTurnState(TS)} then begin
          with GS,
               gstbl[ Aces in free, Twos in free, Threes in free,
                      Fours in free, Fives in free, Sixes in free,
                      ThreeOfAKind in free, FourOfAKind in free,
                      FullHouse in free,
                      SmallStraight in free, LargeStraight in free,
                      2*ord(Yahtzee in free)+ord(chip), Chance in free, usneed
                    ] do begin
            if x < 0 then begin { not yet known }
              if free = [ ] {IsFinalGameState(GS)} then
                x := 0.0
              else begin
                UpdateOptMCache(YS, GS)
              ; x := rtbl[rollsleft, tskeepers]
              end { else }
              { x has been determined }
            end { if }
            { x is known }
          ; OptMinimumScore := x
          ; Exit {return}
          end { with }
        end { then }
        else { not IsInitialTurnState(TS) }
          UpdateOptMCache(YS, GS)
      end { if }
      { OptMCache up-to-date }
    ; case phase of
        ToRoll:   OptMinimumScore := rtbl[rollsleft, tskeepers]
      ; ToChoose: OptMinimumScore := ctbl[rollsleft, tsroll]
      end { case }
    end { with }
  end; { OptMinimumScore }

procedure UpdateOptMCache (YS: YahtzeeStrategy; GS: GameState);
  { pre:  not IsFinalGameState(GS~)
    post: YS^.OptMCache is up to date for GS~ }
  var
    TS: TurnState;
    ev: Event;
    c, c_opt: Category;
    r, sr_opt: ValueBagRank;
    dSC: ScoreCard;
    GSnew: GameState;
    ci: ValueBagCompletionIndex;
    sbi: ValueSubBagIndex;
    opt, m, h: real;
    hctbl: AuxETable;
    hstbl: AuxCTable;
  begin
    with YS^, OptMCache^, GS, TS, ev, dSC do begin

      rollsleft := 0
    ; phase := ToChoose

      { compute for all ValueBags B3 of size NDice (dice after last roll):
          ct[0,B] = (max c: c in free: EventScore(B, c) + OptMinScore(GSnew)) }
    ; with ValueBagRanks[NDice] do
        for r := firstvbr to lastvbr do begin { r repr. B }
          tsroll := r
        ; opt := -maxint
        ; for c := FirstCategory to LastCategory do begin
            evcat := c
          ; if c in free then begin
              ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
              { N.B. Must determine GSnew before calling MinimumScore,
                hence EventScore(..., GSnew) + MinimumScore(GSnew)
                is not good enough }
              ; h := box[target] + OptMinimumScore(YS, GSnew, InitialTurnState)
              ; if h > opt then begin
                  opt := h
                ; c_opt := c
                end { if }
              end { if }
            end { for c }
        ; hctbl[r] := opt
        ; hstbl[r] := c_opt
        end { for r }
      { From this point on, OptMCache^ is being filled in.
        N.B. Preceding calls to MinimumScore affect OptMCache^ }
    ; ctbl[rollsleft] := hctbl
    ; stbl := hstbl

    ; while rollsleft <> NModificationAttempts do begin

        rollsleft := succ(rollsleft)
      ; phase := ToRoll

        { compute for all ValueBags K2 ("Keepers"):
            rt[i,K] = (min E: #K+#E=NDice: ct[i-1,K+E]) }
      ; for r := 0 to MaxValueBagRank do begin { r repr. K }
          with ValueBagCompletionIndices[r] do begin
            m := maxint
          ; for ci := firstvbci to lastvbci do { ci repr. E, K+E }
              with ValueBagCompletions[ci] do begin
                h := ctbl[pred(rollsleft), sum]
              ; if h < m then
                  m := h
              end { with }
          ; rtbl[rollsleft, r] := m
          end { with }
        end { for r }

      ; phase := ToChoose

        { compute for all ValueBags B of size NDice:
            ct[rollsleft,B] = (max K: K <= B: rt[rollsleft,K]) }
      ; for r := NValueBag4Ranks to MaxValueBagRank do begin { r repr. B }
          with ValueSubBagIndices[r] do begin
            opt := -maxint
          ; for sbi := lastvsbi downto firstvsbi do
                            { prefer to keep as much as possible }
              with ValueSubBags[sbi] do begin { sr repr. K }
              h := rtbl[rollsleft, sr]
            ; if h > opt then begin
                opt := h
              ; sr_opt := sr
              end { if }
            end { for sbi }
          end { with }
        ; ctbl[rollsleft, r] := opt
        ; ktbl[rollsleft, r] := sr_opt
        end { for r }

      end { while }

    ; rollsleft := succ(rollsleft)
    ; phase := ToRoll

      { compute expected final score as
          rt[rollsleft,0] = (min B: #B=NDice: ct[rollsleft-1,B]) }
    ; with ValueBagRanks[NDice] do begin
        m := maxint
      ; for r := firstvbr to lastvbr do begin { r repr. B }
          h := ctbl[pred(rollsleft), r]
        ; if h < m then
            m := h
        end { for r }
      end { with }
    ; rtbl[rollsleft, EmptyValueBagRank] := m

    ; cGS := GS
    end { with }
    { OptMCache is up to date for GS }
  end; { UpdateOptMCache }

procedure StrategyChoice (YS: YahtzeeStrategy;
                          GS: GameState; TS: TurnState; total: integer;
                          var {out} ev: Event; verbose: boolean);
  { pre:  not IsFinalGameState(GS~) /\ TS~.phase = ToChoose /\
          total = total score collected up to GS
    post: (TS~.rollsleft<>0 /\ ev.kind=Keeping /\ ev.evkeepers [= TS~.tsroll)
       \/ (ev.kind=Scoring /\ ev.evcat in GS~.free)
    N.B. ev.evsc need not be filled in }

  procedure ManualPlay;
    { pre: YS^.stype = Manual }
    { Read choice ev from input, for given state TS~ ,GS~ }
    { If verbose, show game state and, if applicable, score on output. }
    var
      prompt: stdstring;
      b: boolean;
      dSC: ScoreCard;
      GSnew: GameState;
    begin
      with YS^, TS, ValueBagTbl[tsroll], ev do begin
        if verbose then begin
          if not IsEqualGameState(GS, pGS) then begin
            writeln
          ; writeln('Game state: ', StringFromGameState(GS),
                    ', Total= ', total:1)
          end { if }
        ; writeln(  '      Roll: ', StringFromTurnState(TS, false, true))
        end { if }
      ; prompt := '  Choice ['
      ; if rollsleft <> 0 then
          prompt := prompt + 'k <keepers> | '
      ; prompt := prompt + 's <category>]? '
      ; repeat
          ReadEvent(input, prompt, ev)
        ; readln
        ; case kind of
            Rolling: b := false
          ; Keeping: b := (rollsleft <> 0) and
                          IsSubValueBag(ValueBagTbl[evkeepers].fvb, fvb)
          ; Scoring: b := evcat in GS.free
          end { case kind }
        until b
      ; if verbose and (kind = Scoring) then begin
          ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew {ignored})
        ; WriteEvent(output, '  ', ev)
        end { if }
      ; pGS := GS
      end { with }
    end; { ManualPlay }

  procedure SimpleMindedPlay;
    { pre: YS^.stype = Simple }
    begin
      with TS, ev do begin
        kind := Scoring
      ; evcat := MinCategorySet(GS.free)
      end { with }
    end; { SimpleMindedPlay }

  procedure SequentialPlay;
  { only for play in Upper Section, cf. Junior variant }
    var c: Category; i: DieValue; keepers: ValueBag;
    begin
      with TS, ValueBagTbl[tsroll], ev do begin
      ; c := MinCategorySet(GS.free)
      ; i := ValueFromCategory(c)
      ; if rollsleft <> 0 then begin
          kind := Keeping
        ; keepers := EmptyValueBag
        ; ExtendValueBag(keepers, i, fvb[i])
        ; evkeepers := RankFromValueBag(keepers)
        end { then }
        else begin
          kind := Scoring
        ; evcat := c
        end { else }
      end { with }
    end; { SequentialPlay }

  procedure HighestFreeValueMult (thr: ValueMultiplicity);
    { only for play in Upper Section, cf. Junior variant }
    var i, j, k: DieValue; m: integer; keepers: ValueBag;
    begin
      with TS, ValueBagTbl[tsroll], ev do begin
        m := -1
        { inv: j = (min a: fvb[a]=M: a) /\ k = (max a: fvb[a]=M: a)
          where M = (max a: CategoryFromValue(a) in A: fvb[a]) }
      ; for i := MinValue to MaxValue do begin
          if CategoryFromValue(i) in GS.free then begin
            if fvb[i] >= m then begin
              k := i
            ; if fvb[i] > m then begin
                j := i
              ; m := fvb[i]
              end { if }
            end { if }
          end { if }
        end { for i }
      ; if m >= thr then i := k
        else i := j
      ; if rollsleft <> 0 then begin
          kind := Keeping
        ; keepers := EmptyValueBag
        ; ExtendValueBag(keepers, i, fvb[i])
        ; evkeepers := RankFromValueBag(keepers)
        end { then }
        else begin
          kind := Scoring
        ; evcat := CategoryFromValue(i)
        end { else }
      end { with }
    end; { HighestFreeValueMult }

  procedure OptimalEScorePlay;
    { pre: YS^.stype=OptimalEScore }
    { side effect: cache is updated }
    begin
      EnsureOptECache(GS)
    ; with YS^, OptECache^, GS, TS, ev do begin
        if rollsleft <> 0 then begin { may keep or score }
          if ctbl[0, tsroll] >= ctbl[rollsleft, tsroll] then begin
            { immediate scoring is already optimal }
            kind := Scoring
          ; evcat := stbl[tsroll]
          end { then }
          else begin { keep some dice }
            kind := Keeping
          ; evkeepers := ktbl[rollsleft, tsroll]
          end { else }
        end { then }
        else { rollsleft = 0 } begin { must score }
          kind := Scoring
        ; evcat := stbl[tsroll]
        end { else }
      end { with }
    end; { OptimalEScorePlay }

  procedure OptimalMScorePlay;
    { pre: YS^.stype=OptimalMScore }
    { side effect: cache is updated }
    begin
      with YS^, OptMCache^, GS, TS, ev do begin
        if not IsEqualGameState(GS, cGS) or (GS.free = [ ]) then
          UpdateOptMCache(YS, GS)
      ; if rollsleft <> 0 then begin { may keep or score }
          if ctbl[0, tsroll] >= ctbl[rollsleft, tsroll] then begin
            { immediate scoring is already optimal }
            kind := Scoring
          ; evcat := stbl[tsroll]
          end { then }
          else begin { keep some dice }
            kind := Keeping
          ; evkeepers := ktbl[rollsleft, tsroll]
          end { else }
        end { then }
        else { rollsleft = 0 } begin { must score }
          kind := Scoring
        ; evcat := stbl[tsroll]
        end { else }
      end { with }
    end; { OptimalMScorePlay }

//#include "ktstrategy.p"

  begin { StrategyChoice }
{
with TS do begin
if not(not IsFinalGameState(GS)) then Fail('StrategyChoice',
  'not IsFinalGameState(GS)');
if not(phase = ToChoose) then Fail('StrategyChoice', 'TS.phase = ToChoose');
end;
}
    with YS^ do
      case stype of
        Manual: ManualPlay
      ; SimpleMinded: SimpleMindedPlay
        {SequentialPlay}
        {HighestFreeValueMult(RollIndexOfTurnState(TS))}
      ; OptimalEScore: OptimalEScorePlay
      ; OptimalMScore: OptimalMScorePlay
//      ; KarstenAndTobias: KarstenAndTobiasStrategy
      end { case }
{
;with TS, ev do
  case kind of
   Keeping:
    if not((rollsleft <> 0) and
           (IsSubValueBag(ValueBagTbl[evkeepers].fvb,
                          ValueBagTbl[tsroll].fvb))) then
      Fail('StrategyChoice', 'postcondition for Keeping')
  ;Scoring:
    if not(evcat in GS.free) then Fail('StrategyChoice',
      'postcondition for Scoring')
  end
}
  end; { StrategyChoice }

begin { initilization }

{$IFDEF TEST}
  writeln ( LogFile, 'Initializing YahtzeeStrategies ...')
; Flush ( LogFile )
{$ENDIF}
; CreateYahtzeeStrategy(ManualYS, 'User', 'You', Manual, OfficialYR)
; CreateYahtzeeStrategy(SimpleYS, 'Simple Minded', 'Simpl', SimpleMinded,
                        OfficialYR)
; CreateYahtzeeStrategy(OptimalYS, 'Optimal Official Yahtzee', 'OSYP',
    OptimalEScore, OfficialYR)
{ To boostrap gstbl, the program is first run without loading gstbl from file,
  and then determining gstbl and saving it to file. }
; CreateYahtzeeStrategy(OptMinYS, 'Optimal MinScore Yahtzee', 'OMSYP',
    OptimalMScore, OfficialYR)
{ To boostrap gstbl, the program is first run without loading gstbl from file,
  and then determining gstbl and saving it to file. }
; CreateYahtzeeStrategy(KarstenAndTobiasYS, 'Karsten and Tobias', 'KandT',
                        KarstenAndTobias, OfficialYR)
{$IFDEF TEST}
; writeln ( LogFile, 'YahtzeeStrategies initialized')
; Flush ( LogFile )
{$ENDIF}

end. { unit YahtzeeStrategies }

