unit   YahtzeeTesterMain;
  { OBSOLETE (Apply some ad hoc test features) }
  { In future: Should be distributed over separate unit test programs
    and should provide better coverage }

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

procedure TestAll;

implementation

uses
  GeneralStuff,
  MathStuff,
  PseudoRandomGenerator,
  DiceStuff,
  YahtzeeConcepts{$IFDEF False},
  YahtzeeStateTables,
  YahtzeeStrategies,
  Statistics,
  YahtzeeStrategyAnalysis,
  DiceDevils,
  YahtzeeStatistics,
  YahtzeeEventLists,
  YahtzeeStateAnalyzer,
  YahtzeeStrategySampler,
  YahtzeeStrategyExplorer,
  YahtzeeGameRecordings,
  YahtzeeProficiencyTests{$ENDIF};

{$IFDEF False}
procedure TestRandomValue (n: longint; verbose: boolean);
  var
    i: longint;
    v: DieValue;
    vd: Distribution;
    ST: SerialTest;
  begin
    if verbose then
      MakeEmptyDistribution(vd, 'Value', MinValue, MaxValue)
  ; InitSerialTest(ST)
  ; for i := 1 to n do begin
      v := RandomValue
    ; if verbose then
        UpdateDistribution(vd, v, 1)
    ; UpdateSerialTest(ST, v)
    end { for i }
  ; if verbose then begin
      WriteDistribution(output, vd, 1, DetailAll)
    ; WriteDistributionCharacteristics(output, vd)
    end { if }
  ; WriteSerialTest(output, ST, verbose)
  end; { TestRandomValue }
{$ENDIF}

procedure TestValueListValueBag;
  var vl: ValueList; vb: ValueBag;
  begin
    repeat
      ReadValueList(input, 'ValueList: ', vl)
    ; readln
    ; WriteValueList(output, '   vl=', vl)
    ; ValueBagFromValueList(vb, vl)
    ; WriteValueBag(output, '  vb= ', vb)
    until ValueBagSize(vb) = 0
  end; { TestValueListValueBag }

procedure TestValueListTraversal;
  var
    s: DiceCount;
    vl: ValueList;
  begin
    writeln('Testing ValueList Traversal')
  ; write('Size: ')
  ; readln(s)
  ; FirstValueList(vl, s)
  ; repeat
      WriteValueList(output, 'vl= ', vl)
    ; AwaitEnter('')
  ; until not NextValueList(vl)
  end; { TestValueListTraversal }

procedure TestValueBagTraversal;
  var
    s: ValueMultiplicity;
    vb: ValueBag;
  begin
    writeln('Testing ValueBag Traversal')
  ; write('Size: ')
  ; readln(s)
  ; FirstValueBag(vb, s)
  ; repeat
      WriteValueBag(output, 'vb= ', vb)
    ; AwaitEnter('')
  ; until not NextValueBag(vb)
  end; { TestValueBagTraversal }

procedure TestSubBagTraversal;
  var
    vb, vc: ValueBag;
  begin
    writeln('Testing ValueSubBag Traversal')
  ; vb[1] := 1
  ; vb[2] := 0
  ; vb[3] := 1
  ; vb[4] := 0
  ; vb[5] := 2
  ; vb[6] := 1
  ; WriteValueBag(output, 'vb= ', vb)
  ; FirstValueSubBag(vc)
  ; repeat
      WriteValueBag(output, 'vc= ', vc)
    ; AwaitEnter('')
  ; until not NextValueSubBag(vc, vb)
  end; { TestBagTraversal }

{$IFDEF False}
procedure TestGameStateTraversal (verbose: boolean);
  var GS: GameState; t: longint;
  begin
    if verbose then
      writeln('Testing Game State Traversal')
  ; GS := InitialGameState
  ; t := 0
  ; repeat
      if verbose and ((t mod 100000) = 0) then
        writeln('  state ', t:10, ': ', StringFromGameState(GS))
    ; t := succ(t)
    until not NextGameState(GS)
  ; if verbose then
      writeln('  end   ', t:10, ': ', StringFromGameState(GS))
  ; if not(t=786432{2^13*64*3/2}) then Fail('TestGameStateTraversal',
          't=2^13*64*3/2')
  end; { TestGameStateTraversal }

procedure TestGSR;
  var gsr: GameStateRollPerCategory;
  begin
    InitGSR(gsr)
  ; WriteGSR(output, 'Initialized GSR', gsr)
  end; { TestGSR }

procedure TestGameRecording;
  { pre: OptimalYS defined }
  var
    YR: YahtzeeRules;
    GR: GameRecording;
    GS, GSnew: GameState;
    TS: TurnState;
    ev: Event;
    dSC: ScoreCard;
  begin
    YR := OfficialYR
  ; GS := InitialGameState
  ; TS := InitialTurnState
  ; with TS do begin
      phase := ToChoose
    ; tsroll := NValueBag4Ranks
    end { with TS }
  ; MakeEmptyGameRecording(GR)
  ; WriteGameRecording(output, GR, 'Test', YR, false, OptimalYS)
  ; WriteGameRecording(output, GR, 'Test', YR, true, OptimalYS)

  ;   MakeKeepingEvent(ev, TS.tsroll)
  ; ExtendGameRecording(GR, GS, 0, TS, ev)
  ; ExtendGameRecording(GR, GS, 0, TS, ev)
  ;   MakeScoringEvent(ev, Yahtzee)
  ;   ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
  ; ExtendGameRecording(GR, GS, 0, TS, ev)

  ; GS := GSnew
  ;   with TS do tsroll := succ(tsroll)
  ;   MakeKeepingEvent(ev, TS.tsroll)
  ; ExtendGameRecording(GR, GS, 100, TS, ev)
  ;   MakeScoringEvent(ev, Chance)
  ;   ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
  ; ExtendGameRecording(GR, GS, 100, TS, ev)

  ; GS := GSnew
  ;   with TS do tsroll := succ(tsroll)
  ;   MakeScoringEvent(ev, Aces)
  ;   ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
  ; ExtendGameRecording(GR, GS, 200, TS, ev)

  ; WriteGameRecording(output, GR, 'Test', YR, false, OptimalYS)
  ; WriteGameRecording(output, GR, 'Test', YR, true, OptimalYS)
  end; { TestGameRecording }

procedure TestVariance (YS: YahtzeeStrategy);
  var a: Aspect; GS: GameState; TS: TurnState;

  procedure WriteResults;
    var v: real;
    begin
      with YS^ do begin
        writeln('In state ', StringFromGameState(GS), '  ',
                StringFromTurnState(TS, true, false))
      ; write  ('  ', CacheDescription(ECache), ' = ')
      ; writeln(ExpectedScore(YS, GS, TS):1:5)
      ; write  ('  ', CacheDescription(VCache), ' = ')
      ; v := VarianceScore(YS, GS, TS)
      ; writeln(v:1:5, ', Std.dev. = ', sqrt(v):1:5)
      end { with YS^ }
    end; { WriteResults }

  begin
  ; if not(YS<>nil) then Fail('TestVariance', 'YS<>nil')
  ; with YS^ do begin
      writeln('Final score for ', name, ' Strategy')
    ; InitCache(ECache, 'Expected Final Score', GrandTotal, '')
    ; InitCache(VCache, 'Variance in Final Score', GrandTotal, '')
    ; for a := Sixes to Sixes do begin
      ; MakeGameState(GS, [ a ], 0, false)
      ; MakeToRollTurnState(TS, 1, EmptyValueBagRank)
      ; WriteResults
      ; MakeToRollTurnState(TS, 2, EmptyValueBagRank)
      ; WriteResults
      ; MakeToRollTurnState(TS, 3, EmptyValueBagRank)
      ; WriteResults
      end { for a }
    ; for a := FirstCategory to FirstCategory do begin
        InitCache(ECache, 'Expected Final Score', a, '')
      ; InitCache(VCache, 'Variance in Final Score', a, '')
      ; MakeGameState(GS, [ a ], 0, false)
      ; MakeToRollTurnState(TS, 1, EmptyValueBagRank)
      ; WriteResults
      ; MakeToRollTurnState(TS, 2, EmptyValueBagRank)
      ; WriteResults
      ; MakeToRollTurnState(TS, 3, EmptyValueBagRank)
      ; WriteResults
      end { for a }
    end { with }
  end; { TestVariance }
{$ENDIF}

procedure TestAll;
begin
  writeln('TESTS')
{$IFDEF False}
; TestRandomValue(1000000, true)
{$ENDIF}
; TestValueListTraversal
; TestValueBagTraversal
; TestSubBagTraversal
{$IFDEF False}
; TestGameStateTraversal(true)
; TestGSR
; TestGameRecording
; TestVariance(OptimalYS)
{$ENDIF}
end; { TestAll }

end. { program YahtzeeTester }
