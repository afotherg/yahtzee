unit      YahtzeeEventLists;
  { Manipulate annotated lists of events applying to the same state;
    annotation concerns expected final score and standard deviation  }

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
  DiceStuff,
  YahtzeeConcepts,
  YahtzeeStrategies;

const
  MaxEListIndex = NValueBag5Ranks - NValueBag4Ranks;
    { accommodate at least all value bags of size 5 }

type
  EListIndex = 1..MaxEListIndex;
  EListElement = record
    Es, sd: real; { expectation and standard deviation of score }
    ev: Event;
    end; { record }
  EList = record
    gs: GameState;
    ts: TurnState;
    len: 0..MaxEListIndex;
    list: array [ EListIndex ] of EListElement;
    end; { record }
    { For cl: EList, cl.list[0..cl.len) is defined }

procedure MakeEmptyEList (var {out} EL: EList; g: GameState; t: TurnState);
procedure ExtendEList (var EL: EList; x, y: real; e: Event);
procedure SortEList (var EL: EList);
procedure WriteEList (var f: text; {var} const EL: EList);
procedure WriteExtremesEList (var f: text; {var} const EL: EList);
procedure WriteEListCompact (var f: text; {var} const EL: EList;
                             {var} const vl: ValueList; header: boolean);
procedure FindEList ({var} const EL: EList; e: Event; var i: EListIndex);
procedure MakeChoiceEList (var {out} EL: EList; YS: YahtzeeStrategy;
                           GS: GameState; TS: TurnState; total: Score);
procedure MakeRollEList (var {out} EL: EList; YS: YahtzeeStrategy;
                         GS: GameState; TS: TurnState; total: integer);

implementation

uses
  GeneralStuff,
  MathStuff,
  YahtzeeStrategyAnalysis;

procedure MakeEmptyEList (var {out} EL: EList; g: GameState; t: TurnState);
  begin
    with EL do begin
      gs := g
    ; ts := t
    ; len := 0
    end { with EL }
  end; { MakeEmptyEList }

procedure ExtendEList (var EL: EList; x, y: real; e: Event);
  { pre:  # EList < MaxEListIndex
    post: EL = EL~ ++ [ (x,y,e) ] }
  begin
    with EL do begin
      if not(len<MaxEListIndex) then Fail('ExtendEList', 'len<MaxEListIndex')
    ; len := succ(len)
    ; with list[len] do begin
        Es := x
      ; sd := y
      ; ev := e
      end { with }
    end { with EL }
  end; { ExtendEList }

procedure SortEList (var EL: EList);
  { pre:  true
    post: EL = EL~ sorted descending on Es values, preferably stable }
  var
     i, j, k: integer; { EListIndex, but Sun Pascal then fails }
    elelt: EListElement; 
  begin
    with EL do begin
      { stable insertion sort }
      { ghost variable L := list }
      { inv: list[1..i) = sort(L[1..i)) /\ list[i..len] = L[i..len] }
      for i := 2 to len do begin { sort list[i] into list[1..i) }
        elelt := list[i] { ghost variable H := list }
      ; j := 1 ; k := i
        { inv: list[1..k) ++ list(k..i] = H[1..i) /\ cr.Es > list(k..i] }
      ; while j <> k do begin
          if elelt.Es > list[pred(k)].Es then begin
            list[k] := list[pred(k)]
          ; k := pred(k)
          end { then }
          else
            j := k
        end { while }
      ; list[k] := elelt
      end { for i }
    end { with EL }
  end; { SortEList }

procedure WriteEList (var f: text; {var} const EL: EList);
  var i: integer; { actually EventIndex, but Sun Pascal then fails }
  begin
    with EL do begin
      writeln(f, len:1, ' options')
    ; for i := 1 to len do begin
        with list[i] do begin
          write(f, Es:10:5)
        ; if sd >= 0 then
            write(' +/- ', sd:6:2)
        ; WriteEvent(f, ' for ', ev)
        end { with }
      end { for i }
    end { with EL }
  ; flush(f)
  end; { WriteEList }

procedure WriteExtremesEList (var f: text; {var} const EL: EList);
  begin
    with EL do begin
      writeln(f, len:1, ' options')
    ; if 1 <= len then
        with list[1] do begin
          write(f, Es:10:5)
        ; if sd >= 0 then
            write(' +/- ', sd:6:2)
        ; WriteEvent(f, ' for ', ev)
        end { with }
    ; if 1 < len then
        with list[len] do begin
          write(f, Es:10:5)
        ; if sd >= 0 then
            write(' +/- ', sd:6:2)
        ; WriteEvent(f, ' for ', ev)
        end { with }
    end { with EL }
  ; flush(f)
  end; { WriteExtremesEList }

procedure WriteEListCompact (var f: text; {var} const EL: EList;
                             {var} const vl: ValueList; header: boolean);
  { write keeping bags as list relative to roll vl }
  var i: integer; { EventIndex, but Sun Pascal then fails }
  begin
    with EL do begin
      if header then begin
        writeln(f, 'List of expected final scores for ', len:1, ' events in')
      ; WriteGameState(f, '  game state: ', gs)
      ; WriteTurnState(f, '  turn state: ', ts)
      end { then }
      else
        writeln(f, len:1, ' options')
    ; for i := 1 to len do begin
        with list[i] do begin
          write(f, StringFromEventRel(ev, vl))
          { when ev.kind = Scoring, output of evsc is needed for OSYP on WWW }
        ; write(Es:7:2)
        ; if sd >= 0 then
            write(' +/- ', sd:1:0)
        ; writeln(f)
        end { with }
      end { for i }
    end { with EL }
  ; flush(f)
  end; { WriteEListCompact }

procedure FindEList ({var} const EL: EList; e: Event; var i: EListIndex);
  { pre: ev~ occurs in EL~
    post: 1<=i<=EL~.len /\ EL~.list[i].ev~ = e~ }
  begin
    with EL do begin
      i := 1
    ; while not IsEqualEvent(list[i].ev, e) do
        i := succ(i)
    end { with EL }
  end; { FindEList }

procedure MakeChoiceEList (var {out} EL: EList; YS: YahtzeeStrategy;
                           GS: GameState; TS: TurnState; total: Score);
  { pre:  not IsFinalGameState(GS~) /\ TS~.phase = ToChoose
    post: EL = list of all possible choices and their expected final score
          in state GS~, TS~ }
  { N.B. TS is modified locally, so cannot make it var param }
  var
    ev: Event;
    dSC: ScoreCard;
    GSnew: GameState;
    c: Category;
    es, sd: real;
    sbi: ValueSubBagIndex;
  begin
    MakeEmptyEList(EL, GS, TS)
  ; with YS^, GS, TS, ev, dSC do begin
      kind := Scoring
    ; for c := FirstCategory to LastCategory do begin
        evcat := c
      ; if c in free then begin
          ScoreEvent(ysYR, GS, TS, ev, dSC, GSnew)
          { N.B. Must determine GSnew before calling ExpectedScore,
            hence es := EventScore(..., GSnew) + ExpectedScore(GSnew)
            is not good enough }
        ; es := total + box[ECache^.target]
                      + ExpectedScore(YS, GSnew, InitialTurnState)
        ; if VCache = nil then
            sd := Unknown
          else
            sd := sqrt(VarianceScore(YS, GSnew, InitialTurnState))
        ; ExtendEList(EL, es, sd, ev)
        end { if }
      end { for c }
    ; if rollsleft <> 0 then begin
        { from now on TS plays role of next state after TS' }
        phase := ToRoll
      ; kind := Keeping
      ; with ValueSubBagIndices[tsroll] do
          { traverse all subbags, except the whole bag }
          for sbi := pred(lastvsbi) downto firstvsbi do
            with ValueSubBags[sbi] do begin
              evkeepers := sr
            ; tskeepers := sr
            ; es := total + ExpectedScore(YS, GS, TS)
            ; if VCache = nil then
                sd := Unknown
              else
                sd := sqrt(VarianceScore(YS, GS, TS))
            ; ExtendEList(EL, es, sd, ev)
            end { with }
        end { if }
    end { with }
  end; { MakeChoiceEList }

procedure MakeRollEList (var {out} EL: EList; YS: YahtzeeStrategy;
                         GS: GameState; TS: TurnState; total: integer);
  { pre: not IsFinalGameState(GS~) /\ TS~.phase=ToRoll }
  { post: EL = list of values under YS
          for all possible rolls in state GS~ ,TS~ }
  { N.B. TS is modified locally, so cannot make it var param }
  var
    ci: ValueBagCompletionIndex;
    ev: Event;
    es, sd: real;
  begin
  ; MakeEmptyEList(EL, GS, TS)
  ; with TS, ValueBagCompletionIndices[tskeepers] do begin
      { N.B. tskeepers above is determined once, BEFORE following,
        where TS is set up for new turn state }
      rollsleft := pred(rollsleft)
    ; phase := ToChoose
    ; for ci := firstvbci to lastvbci do
        with ValueBagCompletions[ci] do begin
          MakeRollingEvent(ev, ValueBagTbl[sum].vlr)
        ; tsroll := sum
        ; es := total + ExpectedScore(YS, GS, TS)
        ; if YS^.VCache = nil then
            sd := Unknown
          else
            sd := sqrt(VarianceScore(YS, GS, TS))
        ; ExtendEList(EL, es, sd, ev)
        end { with }
    end { with }
  end; { MakeRollEList }

end. { unit YahtzeeEventLists }
