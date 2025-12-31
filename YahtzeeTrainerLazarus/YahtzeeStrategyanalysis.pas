unit      YahtzeeStrategyAnalysis;
  { Various ways to analyze a Yahtzee strategy }

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
  MathStuff,
  DiceStuff,
  YahtzeeConcepts,
  YahtzeeStrategies;

function  ExpectedScore (YS: YahtzeeStrategy;
                         GS: GameState; TS: TurnState): real;
procedure UpdateECache (YS: YahtzeeStrategy; GS: GameState);
procedure AllExpectedScores (YS: YahtzeeStrategy);

function  MinimumScore (YS: YahtzeeStrategy;
                        GS: GameState; TS: TurnState): real;
procedure UpdateMCache (YS: YahtzeeStrategy; GS: GameState);
procedure AllMinimumScores (YS: YahtzeeStrategy);
procedure WriteMinimumScoreGame (var f: text; YR: YahtzeeRules;
                                 YS: YahtzeeStrategy);

function  ZeroScoreProbability (YS: YahtzeeStrategy;
                                GS: GameState; TS: TurnState): real;
procedure UpdateZCache (YS: YahtzeeStrategy; GS: GameState);
procedure AllZeroScoreProbabilities (YS: YahtzeeStrategy);

function  VarianceScore (YS: YahtzeeStrategy;
                         GS: GameState; TS: TurnState): real;
procedure UpdateVCache (YS: YahtzeeStrategy; GS: GameState);
procedure AllVarianceScores (YS: YahtzeeStrategy);

procedure WriteFirstTurnTable (var f: text; YR: YahtzeeRules;
                               YS: YahtzeeStrategy);
procedure WriteLastTurnTable (var f: text; YR: YahtzeeRules;
                               YS: YahtzeeStrategy);

procedure WriteEarliestTurns (var f: text; YR: YahtzeeRules;
                              YS: YahtzeeStrategy);

procedure AnalyzeStrategy (var f: text; YR: YahtzeeRules; YS: YahtzeeStrategy);

implementation

uses
  YahtzeeStateTables,
  Main { for progressbar added in YahtzeeTrainer 1.2 };

function  ExpectedScore (YS: YahtzeeStrategy;
                         GS: GameState; TS: TurnState): real;
  { pre: YS~^.ECache^ properly initialized /\ YS~ is deterministic
    ret: final expected score of YS~ in state GS~,TS~
    side effect: ECache updated when necessary }
  begin
    with YS^, ECache^, TS do begin
      if not IsEqualGameState(GS, cGS) or (GS.free = [ ]) then begin
        { ECache not up-to-date }
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
                UpdateECache(YS, GS)
              ; x := rtbl[rollsleft, tskeepers]
              end { else }
              { x has been determined }
            end { if }
            { x is known }
          ; ExpectedScore := x
          ; Exit { gpc }
{          ; return { Sun Pascal }
          end { with }
        end { then }
        else { not IsInitialTurnState(TS) }
          UpdateECache(YS, GS)
      end { if }
      { ECache up-to-date }
    ; case phase of
        ToRoll:   ExpectedScore := rtbl[rollsleft, tskeepers]
      ; ToChoose: ExpectedScore := ctbl[rollsleft, tsroll]
      end { case }
    end { with }
  end; { ExpectedScore }

procedure UpdateECache (YS: YahtzeeStrategy; GS: GameState);
  { pre:  not IsFinalGameState(GS~)
    post: YS^.ECache is up to date for GS~ }
  var
    TS: TurnState;
    ev: Event;
    r: ValueBagRank;
    dSC: ScoreCard;
    GSnew: GameState;
    ci: ValueBagCompletionIndex;
    h: real;
    hctbl: AuxETable;
    hstbl: AuxCTable;
  begin
{
if not(YS<>nil) then Fail('UpdateECache', 'YS<>nil');
if not(YS^.ECache<>nil) then Fail('UpdateECache', 'YS^.ECache<>nil');
}
    with YS^, ECache^, TS, ev, dSC do begin

      rollsleft := 0
    ; phase := ToChoose

      { compute for all ValueBags B3 of size NDice (dice after last roll):
          ct[0,B] = EventScore(B, c) + E(GSnew), where c chosen by YS }
    ; with ValueBagRanks[NDice] do
        for r := firstvbr to lastvbr do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
          { N.B. Must determine GSnew before calling ExpectedScore,
            hence EventScore(..., GSnew) + ExpectedScore(GSnew)
            is not good enough }
        ; hctbl[r] := box[target] + ExpectedScore(YS, GSnew, InitialTurnState)
{
;if (hctbl[r]>5) then begin
 writeln(errout, r, ', ', hctbl[r]:1:5, ', ', box[target], ', ',
      StringFromGameState(GSnew),
      StringFromTurnState(InitialTurnState, true, false))
end
}
        ; hstbl[r] := evcat
        end { for r }
{
;if target=Aces then
  WriteAuxETable(errout, 'hctbl', hctbl, NValueBag4Ranks, MaxValueBagRank)
}
      { From this point on, ECache^ is being filled in.
        N.B. Preceding calls to ExpectedScore affect ECache^ }
    ; ctbl[rollsleft] := hctbl
    ; stbl := hstbl

    ; while rollsleft <> NModificationAttempts do begin

        rollsleft := succ(rollsleft)
      ; phase := ToRoll

        { compute for all ValueBags K2 ("Keepers"):
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

      ; phase := ToChoose

        { compute for all ValueBags B of size NDice:
            ct[rollsleft,B] = rt[rollsleft,K], where K chosen by YS }
      ; for r := NValueBag4Ranks to MaxValueBagRank do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; if kind = Scoring then begin
            kind := Keeping
          ; evkeepers := r
          end { if }
        ; ctbl[rollsleft, r] := rtbl[rollsleft, evkeepers]
        ; ktbl[rollsleft, r] := evkeepers
        end { for r }

      end { while }

    ; rollsleft := succ(rollsleft)
    ; phase := ToRoll

      { compute expected final score as
          rt[rollsleft,0] = (+ B: #B=NDice: Pr(B)*ct[rollsleft-1,B]) }
    ; with ValueBagRanks[NDice] do begin
        h := 0.0
      ; for r := firstvbr to lastvbr do { r repr. B }
          with ValueBagTbl[r] do
            h := h + pr*ctbl[pred(rollsleft), r]
      end { with }
    ; rtbl[rollsleft, EmptyValueBagRank] := h

    ; cGS := GS
    end { with }
  { ECache is up to date for GS }
  end; { UpdateECache }

procedure AllExpectedScores (YS: YahtzeeStrategy);
  { pre:  true
    post: YS^.ECache^.gstbl is completely determined }
  var GS: GameState; x: real;
  begin
    GS := InitialGameState
  ; repeat
      x := ExpectedScore(YS, GS, InitialTurnState)
    until not NextGameState(GS)
  end; { AllExpectedScores }

function  MinimumScore (YS: YahtzeeStrategy;
                        GS: GameState; TS: TurnState): real;
  { pre: YS~^.MCache^ properly initialized /\ YS~ is deterministic
    ret: minimum score of YS~ in state GS~,TS~ }
  { N.B. Key assumption (fulfilled in game of Yahtzee): scores are nonnegative }
  begin
    with YS^, MCache^, TS do begin
      if not IsEqualGameState(GS, cGS) or (GS.free = [ ]) then begin
        { MCache not up-to-date }
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
                UpdateMCache(YS, GS)
              ; x := rtbl[rollsleft, tskeepers]
              end { else }
              { x has been determined }
            end { if }
            { x is known }
          ; MinimumScore := x
          ; Exit { gpc }
{          ; return { Sun Pascal }
          end { with }
        end { then }
        else { not IsInitialTurnState(TS) }
          UpdateMCache(YS, GS)
      end { if }
      { MCache up-to-date }
    ; case phase of
        ToRoll:   MinimumScore := rtbl[rollsleft, tskeepers]
      ; ToChoose: MinimumScore := ctbl[rollsleft, tsroll]
      end { case }
    end { with }
  end; { MinimumScore }

procedure UpdateMCache (YS: YahtzeeStrategy; GS: GameState);
  { pre:  not IsFinalGameState(GS~)
    post: YS^.MCache is up to date for GS~ }
  var
    TS: TurnState;
    ev: Event;
    r: ValueBagRank;
    dSC: ScoreCard;
    GSnew: GameState;
    ci: ValueBagCompletionIndex;
    m, h: real;
    hctbl: AuxETable;
    hstbl: AuxCTable;
  begin
    with YS^, MCache^, TS, ev, dSC do begin

      rollsleft := 0
    ; phase := ToChoose

      { compute for all ValueBags B3 of size NDice (dice after last roll):
          ct[0,B] = EventScore(B, c) + MinScore(GSnew), where c chosen by YS }
    ; with ValueBagRanks[NDice] do
        for r := firstvbr to lastvbr do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
          { N.B. Must determine GSnew before calling MinimumScore,
            hence EventScore(..., GSnew) + MinimumScore(GSnew)
            is not good enough }
        ; hctbl[r] := box[target]
                    + MinimumScore(YS, GSnew, InitialTurnState)
        ; hstbl[r] := evcat
        end { for r }
      { From this point on, MCache^ is being filled in.
        N.B. Preceding calls to MinimumScore affect MCache^ }
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
            ct[rollsleft,B] = rt[rollsleft,K], where K chosen by YS }
      ; for r := NValueBag4Ranks to MaxValueBagRank do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; if kind = Scoring then begin
            kind := Keeping
          ; evkeepers := r
          end { if }
        ; ctbl[rollsleft, r] := rtbl[rollsleft, evkeepers]
        ; ktbl[rollsleft, r] := evkeepers
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
    { MCache is up to date for GS }
  end; { UpdateMCache }

procedure AllMinimumScores (YS: YahtzeeStrategy);
  { pre:  true
    post: YS^.MCache^.gstbl is completely determined }
  var GS: GameState; x: real;
  begin
    GS := InitialGameState
  ; repeat
      x := MinimumScore(YS, GS, InitialTurnState)
    until not NextGameState(GS)
  end; { AllMinimumScores }

procedure WriteMinimumScoreGame (var f: text; YR: YahtzeeRules;
                                 YS: YahtzeeStrategy);
  { pre:  true
    post: write to f, a game that achieves minimum score for YS }
  var
    m: real;
    GS, GSnew: GameState;
    TS: TurnState;
    ev: Event;
    SC, dSC: ScoreCard;
  begin
    writeln(f)
  ; with YS^ do begin
      InitCache(MCache, 'Minimum Final Score', GrandTotal, '')
    ; writeln('Game achieving ', CacheDescription(MCache), ' by ', name)
    end { with }
  ; GS := InitialGameState
  ; SC := EmptyScoreCard
  ; while not IsFinalGameState(GS) do begin
      m := MinimumScore(YS, GS, InitialTurnState)
    ; with TS, ValueBagRanks[NDice] do begin
        rollsleft := 0
      ; phase := ToChoose
      ; tsroll := firstvbr { Linear Search for occurrence of m }
      ; while MinimumScore(YS, GS, TS) <> m do
          tsroll := succ(tsroll)
      end { with }
    ; StrategyChoice(YS, GS, TS, 0, ev, false)
    ; ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
    ; UpdateScoreCard(SC, dSC)
    ; writeln(f, StringFromGameState(GS), '  ',
                 StringFromTurnState(TS, false, false), '  ',
                 StringFromEvent(ev, true, false))
    ; GS := GSnew
    end { while }
  ; WriteScoreCard(f, SC, OfficialYR)
  end; { WriteMinimumScoreGame }

function  ZeroScoreProbability (YS: YahtzeeStrategy;
                                GS: GameState; TS: TurnState): real;
  { pre: YS~^.MCache^ properly initialized /\ YS~ is deterministic
    ret: probability for zero score of YS~ in state GS~,TS~ }
  begin
    with YS^, ZCache^, TS do begin
      if not IsEqualGameState(GS, cGS) or (GS.free = [ ]) then begin
        { ZCache not up-to-date }
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
                x := 1.0
              else begin
                UpdateZCache(YS, GS)
              ; x := rtbl[rollsleft, tskeepers]
              end { else }
              { x has been determined }
            end { if }
            { x is known }
          ; ZeroScoreProbability := x
          ; Exit { gpc }
{          ; return { Sun Pascal }
          end { with }
        end { then }
        else { not IsInitialTurnState(TS) }
          UpdateZCache(YS, GS)
      end { if }
      { ZCache up-to-date }
    ; case phase of
        ToRoll:   ZeroScoreProbability := rtbl[rollsleft, tskeepers]
      ; ToChoose: ZeroScoreProbability := ctbl[rollsleft, tsroll]
      end { case }
    end { with }
  end; { ZeroScoreProbability }

procedure UpdateZCache (YS: YahtzeeStrategy; GS: GameState);
  { pre:  not IsFinalGameState(GS~)
    post: YS^.ZCache is up to date for GS~ }
  var
    TS: TurnState;
    ev: Event;
    r: ValueBagRank;
    dSC: ScoreCard;
    GSnew: GameState;
    ci: ValueBagCompletionIndex;
    h: real;
    hctbl: AuxETable;
//    hstbl: AuxCTable;
  begin
    with YS^, ZCache^, TS, ev, dSC do begin

      rollsleft := 0
    ; phase := ToChoose

      { compute for all ValueBags B3 of size NDice (dice after last roll):
          ct[0,B] = Z(GSnew) if EventScore(B,c)=0 else 0,
            where c chosen by YS }
    ; with ValueBagRanks[NDice] do
        for r := firstvbr to lastvbr do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
          { N.B. Must determine GSnew before calling ZeroScoreProbability }
        ; if box[target] = 0 then
            hctbl[r] := ZeroScoreProbability(YS, GSnew, InitialTurnState)
          else
            hctbl[r] := 0.0
        end { for r }
      { From this point on, ZCache^ is being filled in.
        N.B. Preceding calls to ZeroScoreProbability affect ZCache^ }
    ; ctbl[rollsleft] := hctbl

    ; while rollsleft <> NModificationAttempts do begin

        rollsleft := succ(rollsleft)
      ; phase := ToRoll

        { compute for all ValueBags K2 ("Keepers"):
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

      ; phase := ToChoose

        { compute for all ValueBags B of size NDice:
            ct[rollsleft,B] = rt[rollsleft,K], where K chosen by YS }
      ; for r := NValueBag4Ranks to MaxValueBagRank do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; if kind = Scoring then begin
            kind := Keeping
          ; evkeepers := r
          end { if }
        ; ctbl[rollsleft, r] := rtbl[rollsleft, evkeepers]
        ; ktbl[rollsleft, r] := evkeepers
        end { for r }

      end { while }

    ; rollsleft := succ(rollsleft)
    ; phase := ToRoll

      { compute probability of zero score as
          rt[rollsleft,0] = (+ B: #B=NDice: Pr(B)*ct[rollsleft-1,B]) }
    ; with ValueBagRanks[NDice] do begin
        h := 0.0
      ; for r := firstvbr to lastvbr do { r repr. B }
          with ValueBagTbl[r] do
            h := h + pr*ctbl[pred(rollsleft), r]
      end { with }
    ; rtbl[rollsleft, EmptyValueBagRank] := h

    ; cGS := GS
    end { with }
    { ZCache is up to date for GS }
  end; { UpdateZCache }

procedure AllZeroScoreProbabilities (YS: YahtzeeStrategy);
  { pre:  true
    post: YS^.ZCache^.gstbl is completely determined }
  var GS: GameState; x: real;
  begin
    GS := InitialGameState
  ; repeat
      x := ZeroScoreProbability(YS, GS, InitialTurnState)
    until not NextGameState(GS)
  end; { AllZeroScoreProbabilities }

function  VarianceScore (YS: YahtzeeStrategy;
                         GS: GameState; TS: TurnState): real;
  { pre: YS~^.VCache^,ECache^ properly initialized for the same target /\
         YS~ is deterministic
    ret: variance in final score of YS~ in state GS~,TS~ }
  begin
    with YS^, VCache^, TS do begin
      if not IsEqualGameState(GS, cGS) or (GS.free = [ ]) then begin
        { VCache not up-to-date }
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
                UpdateVCache(YS, GS)
              ; x := rtbl[rollsleft, tskeepers]
              end { else }
              { x has been determined }
            ; nKnownEntries := nKnownEntries + 1
            ; if (nKnownEntries mod 1024) = 0 then
                TableForm.PBV.StepIt
              { last two rules added in YahtzeeTrainer 1.2 for progressbar }
            end { if }
            { x is known }
          ; VarianceScore := x
          ; Exit { gpc }
{          ; return { Sun Pascal }
          end { with }
        end { then }
        else { not IsInitialTurnState(TS) }
          UpdateVCache(YS, GS)
      end { if }
      { VCache up-to-date }
    ; case phase of
        ToRoll:   VarianceScore := rtbl[rollsleft, tskeepers]
      ; ToChoose: VarianceScore := ctbl[rollsleft, tsroll]
      end { case }
    end { with }
  end; { VarianceScore }

procedure UpdateVCache (YS: YahtzeeStrategy; GS: GameState);
  { pre:  not IsFinalGameState(GS~) /\ YS^.ECache,VCache properly initialized
    post: YS^.VCache is up to date for GS~ }
  var
    TS: TurnState;
    ev: Event;
    r: ValueBagRank;
    dSC: ScoreCard;
    GSnew: GameState;
    ci: ValueBagCompletionIndex;
    h, es, es1: real;
    hctbl: AuxETable;
  begin
{
if not(YS<>nil) then Fail('UpdateVCache', 'YS<>nil');
if not(YS^.VCache<>nil) then Fail('UpdateVCache', 'YS^.VCache<>nil');
}
    with YS^, VCache^, TS, ev, dSC do begin

      rollsleft := 0
    ; phase := ToChoose

      { compute for all ValueBags B3 of size NDice (dice after last roll):
          ct[0,B] = (EventScore(B,c)+E(F|gc)-E(F|g))^2 + V(GSnew),
            where c chosen by YS }
    ; with ValueBagRanks[NDice] do
        for r := firstvbr to lastvbr do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
        ; hctbl[r] := VarianceScore(YS, GSnew, InitialTurnState)
        end { for r }
      { From this point on, VCache^ is being filled in.
        N.B. Preceding calls to VarianceScore affect VCache^ }
    ; ctbl[rollsleft] := hctbl

    ; while rollsleft <> NModificationAttempts do begin

        rollsleft := succ(rollsleft)
      ; phase := ToRoll

        { compute for all ValueBags K ("Keepers"):
            rt[i,K] = (+ E: #K+#E=NDice:
                            Pr(E) *((E(F|gE)-E(F|g))^2+ct[i-1,K+E])) }
      ; for r := 0 to MaxValueBagRank do begin { r repr. K }
          with ValueBagCompletionIndices[r] do begin
            tskeepers := r
          ; es := ExpectedScore(YS, GS, TS)
          ; rollsleft := pred(rollsleft)
          ; phase := ToChoose
          ; h := 0.0
          ; for ci := firstvbci to lastvbci do { ci repr. E, K+E }
              with ValueBagCompletions[ci] do begin
                tsroll := sum
              ; es1 := ExpectedScore(YS, GS, TS)
              ; h := h + cpr*(sqr(es1-es) + ctbl[rollsleft, sum])
              end { with }
          ; rollsleft := succ(rollsleft)
          ; phase := ToRoll
          ; rtbl[rollsleft, r] := h
          end { with }
        end { for r }

      ; phase := ToChoose

        { compute for all ValueBags B of size NDice:
            ct[rollsleft,B] = (E(F|gK)-E(F|g))^2 + rt[rollsleft,K],
              where K chosen by YS }
      ; for r := NValueBag4Ranks to MaxValueBagRank do begin { r repr. B }
          tsroll := r
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; if kind = Scoring then begin
            kind := Keeping
          ; evkeepers := r
          end { if }
        ; ctbl[rollsleft, r] := rtbl[rollsleft, evkeepers]
        end { for r }

      end { while }

    ; rollsleft := succ(rollsleft)
    ; phase := ToRoll
    ; tskeepers := EmptyValueBagRank

    ; es := ExpectedScore(YS, GS, TS)
      { compute variance in final score as
          rt[rollsleft,0] = (+ B: #B=NDice: Pr(B)*((E(F|gB)-E(F|g))^2 +
                                                   ct[rollsleft-1,B]) }
    ; with TS, ValueBagRanks[NDice] do begin
        rollsleft := pred(rollsleft)
      ; phase := ToChoose
      ; h := 0.0
      ; for r := firstvbr to lastvbr do begin { r repr. B }
          tsroll := r
        ; es1 := ExpectedScore(YS, GS, TS)
        ; with ValueBagTbl[r] do
            h := h + pr*(sqr(es1-es) + ctbl[rollsleft, r])
        end { for r }
      ; rollsleft := succ(rollsleft)
      ; phase := ToRoll
      ; tskeepers := EmptyValueBagRank
      end { with }
    ; rtbl[rollsleft, tskeepers] := h

    ; cGS := GS
    end { with }
    { VCache is up to date for GS }
  end; { UpdateVCache }

procedure AllVarianceScores (YS: YahtzeeStrategy);
  { pre:  true
    post: YS^.VCache^.gstbl is completely determined }
  var GS: GameState; x: real;
  begin
    GS := InitialGameState
  ; repeat
      x := VarianceScore(YS, GS, InitialTurnState)
    until not NextGameState(GS)
  end; { AllVarianceScores }

procedure WriteFirstTurnTable (var f: text; YR: YahtzeeRules;
                               YS: YahtzeeStrategy);
  { pre:  true
    post: For each roll r and number of rollsleft in the first turn,
          the best choice is written to f }
  var
    GS, GSnew: GameState;
    TS: TurnState;
    ev: Event;
    dSC: ScoreCard;
    rl: RollsLeftCount;
    r: ValueBagRank;
  begin
    GS := InitialGameState
  ; with YS^, TS do begin
      phase := ToChoose
    ; writeln(f)
    ; writeln(f, 'First Turn Choices by ', name)
    ; writeln(f, '       Choice for Rolls left =')
    ; write  (f, 'Roll ')
    ; for rl := NRollsPerTurn downto 1 do
        write(f, rl:5, '  ')
    ; writeln(f)
    ; writeln(f, '-----  -----  -----  -----')
    ; for r := NValueBag4Ranks to MaxValueBagRank do begin
        tsroll := r
      ; write(f, StringFromValueBag(ValueBagTbl[r].fvb, false))
      ; for rl := NModificationAttempts downto 0 do begin
          rollsleft := rl
        ; StrategyChoice(YS, GS, TS, 0, ev, false)
        ; if ev.kind = Scoring then
            ScoreEvent(YR, GS, TS, ev, dSC {ignored}, GSnew {ignored})
        ; write(f, '  ', StringFromEvent(ev, false, false))
        end { for rl }
      ; writeln(f)
      end { for r }
    end { with }
  end; { WriteFirstTurnTable }

procedure WriteLastTurnTable (var f: text; YR: YahtzeeRules;
                               YS: YahtzeeStrategy);
  { pre:  YS^.ECache has been set up
    post: For each number of rollsleft and category c, the expected final score
          on the last turn with free=[c] is written to f }
  var
    GS: GameState;
    TS: TurnState;
    c: Category;
    rl: RollsLeftCount;
  begin
  ; with YS^, GS, TS do begin
      InitCache(ECache, 'Expected Final Score', GrandTotal, '')
    ; InitCache(VCache, 'Variance in Final Score', GrandTotal, '')
    ; InitCache(ZCache, 'Probability of Zero Score', GrandTotal, '')
    ; writeln(f)
    ; writeln(f, 'Last Turn Values by ', name)
    ; for rl := NRollsPerTurn downto 1 do begin
        writeln(f)
      ; writeln(f, 'Rolls Left: ', rl:1)
      ; writeln(f, 'Category              Exp.Score  Std.Dev.  % Zero')
      ; writeln(f, '--------------------  ---------  --------  ------')
      ; usneed := 0
      ; chip := false
      ; rollsleft := rl
      ; phase := ToRoll
      ; tskeepers := EmptyValueBagRank
      ; for c := FirstCategory to LastCategory do begin
          if c in RelevantAspects then begin
            free := [ c ]
          ; write(f, PaddedString(StringFromAspect(c), 20))
          ; write(f, ExpectedScore(YS, GS, TS):11:5)
          ; write(f, sqrt(VarianceScore(YS, GS, TS)):10:5)
          ; write(f, 100*ZeroScoreProbability(YS, GS, TS):8:3)
          ; writeln(f)
          end { if }
        end { for c }
      end { for rl }
    end { with GS, TS }
  ; flush(f)
  end; { WriteLastTurnTable }

var
  RTbl: ReachTable;

procedure WriteEarliestTurns (var f: text; YR: YahtzeeRules;
                              YS: YahtzeeStrategy);
  { post: For each category,
      earliest turn in which it is scored and
      earliest turn in which it is zeroed by YS
      have been written to stdout }
  var
    n: longint;
    ti: TurnIndexPlus;
    r: ValueBagRank;
    nonzeroing, zeroing: GameStateRollPerCategory;
    GS, GSnew: GameState;
    TS: TurnState;
    ev: Event;
    dSC: ScoreCard;
  begin
    writeln(f)
  ; writeln(f, 'Earliest Scoring of each Category')
  ; InitReachTable(RTbl)
  ; InitGSR(nonzeroing)
  ; InitGSR(zeroing)
  ; n := 0 { # reachable states processed }
    { N.B. The traversal order of all game states below must be such that
      if g -r-> g' then g is encountered earlier then g';
      in particular, the traversal must start with the initial state }
  ; GS := InitialGameState
  ; UpdateReachTable(RTbl, GS)
  ; with TS, ev, dSC do begin
      rollsleft := 0
    ; phase := ToChoose
    ; repeat
        ti := TurnIndexOfGameState(GS)
{
;writeln(errout, ti:1, ' ', StringFromGameState(GS))
}
      ; if ti <> LastTurnPlus then begin { game not ended yet }
          if IsReachable(RTbl, GS) then begin { reachable state }
            n := succ(n)
            { consider all last rolls }
          ; for r := NValueBag4Ranks to MaxValueBagRank do begin
              tsroll := r
            ; StrategyChoice(YS, GS, TS, 0, ev, true)
{
; if not(kind=Scoring) then Fail('WriteEarliestGameStates', 'kind=Scoring')
}
            ; ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
            ; if box[evcat] = 0 then
                UpdateGSR(zeroing, evcat, ti, GS, r)
              else
                UpdateGSR(nonzeroing, evcat, ti, GS, r)
              { N.B. ScoreEvent must always have been called, to set GSnew }
            ; UpdateReachTable(RTbl, GSnew)
            end { for r }
          end { if }
        end { if }
      until not NextGameState(GS)
    end { with TS, ev, dSC }
  ; writeln(f, '  # reachable states = ', n:1)
  ; WriteGSR(f, 'Earliest Nonzero Scoring', nonzeroing)
  ; WriteGSR(f, 'Earliest Zero Scoring', zeroing)
  end; { WriteEarliestTurns }

procedure AnalyzeStrategy (var f: text; YR: YahtzeeRules; YS: YahtzeeStrategy);
  { pre:  YS^.ECache,MCache,ZCache,VCache not allocated
    post: Tables written to f for
            final turns,
            earliest turns,
            expected score, variance, zero percentage per aspect,
            minimum score }
  var a: Aspect;
  begin
    with YS^ do begin
      writeln(f)
    ; writeln(f, 'Analyzing the ', name, ' Strategy')
{ N.B. Following causes havoc if ECache was aliased to OptECache }
    ; InitCache(ECache, 'Expected Final Score', GrandTotal, '')
    ; WriteLastTurnTable(f, YR, YS)
    ; WriteEarliestTurns(f, YR, YS)
    ; writeln(f)
    ; writeln(f, 'Aspect                Exp.Score  Std.Dev.  % Zero')
    ; writeln(f, '--------------------  ---------  --------  ------')
    ; for a := FirstAspect to LastAspect do begin
        if a in RelevantAspects then begin
          write(f, PaddedString(StringFromAspect(a), 20))
        ; flush(f)
        ; InitCache(ECache, 'Expected Final Score', a, '')
        ; write(f, ExpectedScore(YS, InitialGameState, InitialTurnState):11:5)
        ; flush(f)
        ; InitCache(VCache, 'Variance in Final Score', a, '')
        ; write(f, sqrt(VarianceScore(YS, InitialGameState, InitialTurnState)):10:5)
        ; flush(f)
        ; ZCache := VCache ; VCache := nil
        ; InitCache(ZCache, 'Probability of Zero Score', a, '')
        ; write(f, 100*ZeroScoreProbability(YS, InitialGameState, InitialTurnState):8:3)
        ; VCache := ZCache ; ZCache := nil
        ; writeln(f)
        ; flush(f)
        end { if }
      end { for a }
    ; writeln(f)
    ; MCache := VCache ; VCache := nil
    ; InitCache(MCache, 'Minimum Final Score', GrandTotal, '')
    ; writeln(f, 'Minimum score = ',
              MinimumScore(YS, InitialGameState, InitialTurnState):1:0)
    ; WriteMinimumScoreGame(f, YR, YS)
    end { with YS^ }
  end; { AnalyzeStrategy }

end. { unit YahtzeeStrategyAnalysis }

