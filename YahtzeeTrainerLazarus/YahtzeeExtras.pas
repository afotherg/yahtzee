unit YahtzeeExtras;
  { Various extra routines for the GUI }

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
  Classes,
  GeneralStuff, DiceStuff, YahtzeeConcepts, YahtzeeEventLists;

type
  ReportEventMethod = procedure( ev: Event ) of object;

procedure SkipFieldPadding (const s: stdstring; var i: Integer);
procedure ValueListFromString (var vl: ValueList;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
procedure IntegerFromString (var n: Integer; lo, hi: Integer;
    const name: stdstring;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
procedure AspectFromString (var a: Aspect;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
procedure CategorySetFromString ( var cs: CategorySet;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
procedure GameStateFromString ( var GS: GameState;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
procedure TurnStateFromString ( var TS: TurnState; var vl: ValueList;
    const s: stdstring; var i: Integer; var errmsg: stdstring);

procedure EListToTStr (str: TStrings; {var} const EL: EList;
                       {var} const vl: ValueList; header: boolean);

implementation

uses
  SysUtils;

procedure SkipFieldPadding (const s: stdstring; var i: Integer);
  { pre: i is valid index in s }
  { post: i is smallest index >= i~ such that s[i~..i) is FieldPadding }
  begin

    while (i <= Length(s)) and (s[i] in FieldPadding) do begin
      i := succ ( i )
    end { while }

  end; { SkipFieldPadding }

procedure ValueListFromString (var vl: ValueList;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
  const
    DiceValueChars = [ chr(ord('0')+MinValue) .. chr(ord('0')+MaxValue) ];
  var
    v: DieValue;
  begin
    if errmsg <> '' then Exit
  ; with vl do begin
      MakeEmptyValueList(vl)
    ; SkipFieldPadding(s, i)

    ; while (i <= Length(s)) and (vl.siz < NDice) and
            not (s[i] in FieldDelimiters) do begin
        if not (s[i] in DiceValueChars) then begin
          errmsg := 'Dice value or delimiter (;) expected'
        ; Exit
        end { if }
      ; v := DigitFromChar ( s[i] )
      ; i := succ ( i )
      ; ExtendValueList(vl, v, 1)
      ; SkipFieldPadding(s, i)
      end { while }

    ; if (i <= Length(s)) and (vl.siz = NDice) and
         not (s[i] in FieldDelimiters) then begin
        if (s[i] in DiceValueChars) then
          errmsg := 'Too many dice values'
        else
          errmsg := 'Delimiter (;) expected'
      ; Exit
      end { if }
    end { with vl }
  ; if i <= Length(s) then
      i := succ ( i )
  end; { ReadValueList }

procedure IntegerFromString (var n: Integer; lo, hi: Integer;
    const name: stdstring;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
  { pre: nonnegative integer at s[i..) }
  const
    Digits = ['0'..'9'];
  begin
    if errmsg <> '' then Exit
  ; SkipFieldPadding(s, i)
  ; n := 0
  ; if (i > Length(s)) or not (s[i] in Digits) then begin
      errmsg := 'Digit expected for ' + name
    ; Exit
    end { if }

  ; repeat
      n := 10 * n + DigitFromChar(s[i])
    ; if n > hi then begin
        errmsg := Format('%s > %d', [name, hi])
      ; Exit
      end { if }
    ; i := succ ( i )
    until (i > Length(s)) or not (s[i] in Digits)
    
  ; if n < lo then begin
      errmsg := Format('%s < %d', [name, lo])
    ; Exit
    end { if }
  end; { IntegerFromString }

procedure AspectFromString (var a: Aspect; const s: stdstring; var i: Integer;
    var errmsg: stdstring);
  { pre:  i is valid index in s
    post: no change if errmsg <> '', else
          a is assigned the aspect at s[i..] and i is incremented past a
          if an error occurred then errmsg <> '' explains error }
  begin
    if errmsg <> '' then Exit
  ; SkipFieldPadding ( s, i )
  ; case UpChar(s[i]) of
      '1': a := Aces
    ; '2': a := Twos
    ; '3': a := Threes
    ; '4': a := Fours
    ; '5': a := Fives
    ; '6': a := Sixes
    ; 'T': a := ThreeOfAKind
    ; 'F': a := FourOfAKind
    ; 'H': a := FullHouse
    ; 'S': a := SmallStraight
    ; 'L': a := LargeStraight
    ; 'Y': a := Yahtzee
    ; 'C': a := Chance
    ; 'U': a := USBonus
    ; 'E': a := EYBonus
    ; 'G': a := GrandTotal
    ; 'Z': a := YahtzeesRolled
    ; 'J': a := JokersApplied
    ; 'X': a := SpecialAspect
    ; else errmsg := 'Invalid aspect'
    end { case }
  ; if errmsg = '' then
      i := succ ( i )
  end; { AspectFromChar }

procedure CategorySetFromString ( var cs: CategorySet;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
  var
    a: Aspect;
    c: Category;
  begin
    if errmsg <> '' then Exit
  ; cs := [ ]
  ; SkipFieldPadding ( s, i )

  ; while (i <= Length(s)) and not ( s[i] in FieldDelimiters ) do begin
      AspectFromString ( a, s, i, errmsg )
    ; if errmsg <> '' then Exit
    ; if not ( (FirstCategory<=a) and (a<=LastCategory) ) then begin
        errmsg := 'Aspect out of range'
      ; i := pred ( i )
      ; Exit
      end { if }
    ; c := a
    ; cs := cs + [ c ]
    ; SkipFieldPadding ( s, i )
    end { while }

  ; if i <= Length(s) then
      i := succ ( i )
  end; { CategorySetFromString }

procedure GameStateFromString ( var GS: GameState;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
  var
    n: Integer; { for output of IntegerFromString }
  begin
    with GS do begin
      CategorySetFromString ( free, s, i, errmsg )
    ; IntegerFromString ( n, 0, BonusThreshold, 'Upper Section Need',
                          s, i, errmsg )
    ; if errmsg <> '' then Exit
    ; usneed := n
    ; SkipFieldPadding(s, i)
    ; if (i > Length(s)) or not (s[i] in ['-', '+']) then begin
        errmsg := 'Chip (+ or -) expected'
      ; Exit
      end { if }
    ; chip := (s[i] = '+')
    ; i := succ ( i )
    end { with GS }
  end; { GameStateFromString }

procedure TurnStateFromString ( var TS: TurnState; var vl: ValueList;
    const s: stdstring; var i: Integer; var errmsg: stdstring);
  var
    vb: ValueBag;
    n: Integer; { for output of IntegerFromString }
  begin
    ValueListFromString(vl, s, i, errmsg)
  ; IntegerFromString(n, 0, NRollsPerTurn, 'Rolls left', s, i, errmsg)
  ; if errmsg <> '' then Exit
  ; with TS do begin
      rollsleft := n
    ; if (rollsleft = NRollsPerTurn) and (vl.siz <> 0) then begin
        errmsg := 'Before first roll, no dice can be kept'
      ; Exit
      end { if }
    ; if (rollsleft = 0) and (vl.siz <> NDice) then begin
        errmsg := 'After last roll, no dice may re-rolled'
      ; Exit
      end { if }
    ; ValueBagFromValueList(vb, vl)
    ; if vl.siz = NDice then begin
        phase := ToChoose
      ; tsroll := RankFromValueBag(vb)
      end { then }
      else begin
        phase := ToRoll
      ; tskeepers := RankFromValueBag(vb)
      end { else }
    end { with TS }
  end; { TurnStateFromString }

procedure EListToTStr (str: TStrings; {var} const EL: EList;
                       {var} const vl: ValueList; header: boolean);
  { write keeping bags as list relative to roll vl }
  var
    i: EListIndex;
    s: String;
  begin
    with EL, str do begin
      if header then begin
        Append(Format('List of expected final scores for %d events in', [len]))
      ; Append(Format('  game state: %s', [StringFromGameState(gs)]))
      ; Append(Format('  turn state: %s', [StringFromTurnState(ts, True, True)]))
      ; Append('')
      end { then }
      else begin
        Append(Format('%d options', [len]))
        
      ; for i := 1 to len do begin
          with list[i] do begin
            s := StringFromEventRel(ev, vl)
            { when ev.kind = Scoring, output of evsc is needed for OSYP on WWW }
          ; s := s + Format('%7.2f', [Es])
          ; if sd >= 0 then
              s := s + Format(' +/- %1.0f', [sd])
          ; Append(s)
          end { with }
        end { for i }
        
      end { else }
    end { with EL }
  end; { EListToTStr }

end.

