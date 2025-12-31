unit      DiceDevils;
  { Various ways to generate dice rolls }

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
  YahtzeeConcepts;

function  RandomValue: DieValue;

type
  DevilType = (Interactive, { rolls read from stdin }
               BuiltInList, { rolls equiprobable by list }
               BuiltInListFast, { rolls equiprobable by list, single draw }
               BuiltInBag,  { rolls equiprobable by bag }
               Demonic,     { rolls worst for OptimalEScore strategy }
               Angelic,     { rolls best for OptimalEScore strategy }
               Special,     { rolls generated in a special way }
               StuckAt      { all rolls generate the same value }
              );

  DiceDevilState = record
    name: stdstring;
    case dtype: DevilType of
      Interactive
    , BuiltInList
    , BuiltInListFast
    , BuiltInBag: ()
    ; Demonic: ( {dYS: YahtzeeStrategy; { demonic for dYS, currently for Opt })
    ; Angelic: ( {dYS: YahtzeeStrategy; { angelic for dYS, currently for Opt })
    ; Special: ( nextvalue: DieValue; )
    ; StuckAt: ( thevalue: DieValue; )
  end; { record }

  DiceDevil = ^DiceDevilState;

var
  InteractiveDD,
  BuiltInListDD,
  BuiltInListFastDD,
  BuiltInBagDD,
  DemonicDD,
  AngelicDD,
  SpecialDD,
  StuckAtDD: DiceDevil;

procedure CreateDiceDevil (var {out} DD: DiceDevil;
                           nm: stdstring; rt: DevilType);
procedure InitDiceDevil (DD: DiceDevil);
procedure TurnStateAfterEvent (var TS: TurnState; ev: Event);
procedure TickleDiceDevil (DD: DiceDevil; GS: GameState; TS: TurnState;
                           var {out} ev: Event);

implementation

uses
  PseudoRandomGenerator,
  YahtzeeStrategies;

{ using Sun Pascal built-in version of random }
{function  RandomValue: DieValue;}
{  var v: integer; dummy: integer;}
{  begin}
{    repeat}
{      v := succ(trunc(random(dummy)*6))  { random: Sun Pascal }
{      { random=1.0 is possible, unfortunately }
{    until v <= MaxValue}
{  ; RandomValue := v}
{  end; { RandomValue }

function  RandomValue: DieValue;
  begin
    RandomValue := genrand1_6 { Random(6) + 1 }
  end; { RandomValue }

procedure CreateDiceDevil (var {out} DD: DiceDevil;
                           nm: stdstring; rt: DevilType);
  begin
    new(DD)
  ; if not(DD<>nil) then Fail('CreateDiceDevil', 'DD<>nil')
  ; with DD^ do begin
      name := nm
    ; dtype := rt
    end { with }
  end; { CreateDiceDevil }

procedure InitDiceDevil (DD: DiceDevil);
  { pre: DD^ has been allocated }
  begin
    with DD^ do begin
      case dtype of
        Special: nextvalue := MinValue
      ; StuckAt: thevalue := MaxValue
      ; else {otherwise} { skip }
      end { case dtype }
    end { with DD^ }
  end; { InitDiceDevil }

procedure TurnStateAfterEvent (var TS: TurnState; ev: Event);
  { pre:  TS~.phase = ToRoll /\ ev~.kind <> Scoring
    post: case ev~.kind of
            Rolling: TS.rollsleft=pred(TS~.rollsleft) /\ TS.phase=ToChoose /\
                     TS.tsroll = TS~.tskeepers (+) ev~.evreroll
            Keeping: TS.tskeepers = ev~.evkeepers
          endcase }
  var ci: ValueBagCompletionIndex;
  begin
    with TS, ev do begin
      case kind of
        Rolling: begin
            rollsleft := pred(rollsleft)
          ; phase := ToChoose
          ; with ValueListTbl[evreroll] do
              ci := ValueBagCompletionIndices[tskeepers].firstvbci +
                    (vbr - ValueBagRanks[sz].firstvbr)
          ; tsroll := ValueBagCompletions[ci].sum
          end { Rolling }
      ; Keeping: begin
            phase := ToRoll
          ; tskeepers := evkeepers
          end { Keeping }
      end { case kind }
    end { with TS }
  end; { TurnStateAfterEvent }

procedure TickleDiceDevil (DD: DiceDevil; GS: GameState; TS: TurnState;
                           var {out} ev: Event);
  { pre:  TS~.phase = ToRoll
    post: ev.kind=Rolling /\ #(TS~.tskeepers (+) ev.tsreroll) = NDice }
  { GS~ only relevant for Demonic }
  var
    s: ValueMultiplicity; { # dice to roll }
    v: DieValue;
    vl: ValueList;
    { following are for Demonic }
    ci, worstci, bestci: ValueBagCompletionIndex;
    mines, maxes, es: real; { (min/max) expected additional score }
    r: ValueBagRank;
  begin
    with DD^, ev do begin
      kind := Rolling
    ; with ValueBagTbl[TS.tskeepers] do begin
        s := NDice - sz { # dice to roll }
      end { with }
    ; if s = 0 then
        evreroll := EmptyValueListRank
      else { s <> 0 } begin
      ; case dtype of

          Interactive: begin
              with vl do begin
                repeat { until siz = s }
                  ReadValueList(input,
                             'Enter dice roll of '+StringFromInt(s,1)+' dice: ',
                                vl)
                ; readln
                until siz = s
              end { with vl }
            ; evreroll := RankFromValueList(vl)
            end { Interactive }

        ; BuiltInList: begin
              { inv: s + ValueBagSize(vl) = NDice }
              vl := EmptyValueList
            ; repeat { until s = 0 }
                v := RandomValue
              ; ExtendValueList(vl, v, 1)
              ; s := pred(s)
              until s = 0
            ; evreroll := RankFromValueList(vl)
            end { BuiltInList }

        ; BuiltInListFast: begin
              with ValueListRanks[s] do begin
                evreroll := firstvlr + genrand_mod{Random}(nvl)
              end { with }
            end { BuiltInListFast }

        ; BuiltInBag: begin
              with ValueBagRanks[s] do begin
                evreroll := ValueBagTbl[firstvbr + genrand_mod{Random}(nvb)].vlr
              end { with }
            end { BuiltInBag }

        ; Demonic: begin
              with TS, ValueBagCompletionIndices[tskeepers] do begin
                { N.B. tskeepers above is determined once, BEFORE following,
                  where TS is set up for new turn state }
                rollsleft := pred(rollsleft)
              ; phase := ToChoose
              ; mines := MaxScore { minimum expected score so far }
              ; for ci := firstvbci to lastvbci do
                  with ValueBagCompletions[ci] do begin
                  ; tsroll := sum
                  ; es := OptEScore(GS, TS)
                  ; if es < mines then begin
                      mines := es
                    ; worstci := ci
                    end { if }
                  end { with }
              ; r := worstci - firstvbci + ValueBagRanks[s].firstvbr
              ; evreroll := ValueBagTbl[r].vlr
              end { with }
            end { Demonic }

        ; Angelic: begin
              with TS, ValueBagCompletionIndices[tskeepers] do begin
                { N.B. tskeepers above is determined once, BEFORE following,
                  where TS is set up for new turn state }
                rollsleft := pred(rollsleft)
              ; phase := ToChoose
              ; maxes := 0 { maximum expected score so far }
              ; for ci := firstvbci to lastvbci do
                  with ValueBagCompletions[ci] do begin
                  ; tsroll := sum
                  ; es := OptEScore(GS, TS)
                  ; if es > maxes then begin
                      maxes := es
                    ; bestci := ci
                    end { if }
                  end { with }
              ; r := bestci - firstvbci + ValueBagRanks[s].firstvbr
              ; evreroll := ValueBagTbl[r].vlr
              end { with }
            end { Angelic }

        ; Special: begin
              { inv: s + ValueBagSize(roll) = NDice }
              vl := EmptyValueList
            ; repeat { until s = 0 }
                v := nextvalue
              ; nextvalue := succ(nextvalue mod MaxValue)
              ; ExtendValueList(vl, v, 1)
              ; s := pred(s)
              until s = 0
            ; evreroll := RankFromValueList(vl)
            end { Special }

        ; StuckAt: begin
              { inv: s + ValueBagSize(roll) = NDice }
              vl := EmptyValueList
            ; repeat { until s = 0 }
                v := thevalue
              ; ExtendValueList(vl, v, 1)
              ; s := pred(s)
              until s = 0
            ; evreroll := RankFromValueList(vl)
            end { StuckAt }

        end { case dtype }
      end { else }
    end { with DD^, ev }
  end; { TickleDiceDevil }

begin
{$IFDEF TEST}
  writeln ( LogFile, 'Initializing DiceDevils ...')
; Flush ( LogFile )
{$ENDIF}
; CreateDiceDevil(InteractiveDD, 'User', Interactive)
; CreateDiceDevil(BuiltInListDD, 'PRNG (per dice)', BuiltInList)
; CreateDiceDevil(BuiltInListFastDD, 'PRNG (per list)', BuiltInListFast)
; CreateDiceDevil(BuiltInBagDD, 'PRNG (per bag)', BuiltInBag)
; CreateDiceDevil(DemonicDD, 'Demon for OSYP', Demonic)
; CreateDiceDevil(AngelicDD, 'Angel for OSYP', Angelic)
; CreateDiceDevil(SpecialDD, 'Special dice', Special)
; CreateDiceDevil(StuckAtDD, 'Stuck-at dice', StuckAt)
{$IFDEF TEST}
; writeln ( LogFile, 'DiceDevils initialized')
; Flush ( LogFile )
{$ENDIF}
end. { unit DiceDevils }
