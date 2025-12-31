unit      YahtzeeStateTables;
  { Various tables involving Yahtzee states }

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
  MathStuff, { only for Unknown }
  DiceStuff,
  YahtzeeConcepts;

type
  GameStateTable = { table with real for each "between-turns" game state }
    array [ boolean, boolean, boolean, boolean, boolean, boolean,
            boolean, boolean, boolean, boolean, boolean, 0..2, boolean,
            0..MaxBonusThreshold
          ] of
      record { to enable use of with statement, avoiding index recomputation }
        x: real
      end; { record }
  { isomorphic to `array [ GameState ] of real'
    see InspectGameStateTable for explanation }

function  InspectGameStateTable ({var} const gst: GameStateTable;
                                 GS: GameState): real;
procedure InitGameStateTable (var {out} gst: GameStateTable);
procedure WriteGameStateTable (var f: text; s: stdstring;
                               {var} const gst: GameStateTable);
procedure SaveGameStateTable (name: stdstring; {var} const gst: GameStateTable);
procedure LoadGameStateTable (name: stdstring; var {out} gst: GameStateTable);
function  NKnownGameStateTable ({var} const gst: GameStateTable): longint;

type
  AuxNTable = array [ ValueBagRank ] of longint;
  AuxETable = array [ ValueBagRank ] of real;

procedure WriteAuxNTable (var f: text; s: stdstring; {var} const at: AuxNTable;
                          lo, hi: ValueBagRank);
procedure WriteAuxETable (var f: text; s: stdstring; {var} const at: AuxETable;
                          lo, hi: ValueBagRank);

function  NAssemblies: longint;
  { ret: # ways, per turn, for assembling a ValueBag in 3 rolls incl. keeping }

type
  ReachTable =
    array [ boolean, boolean, boolean, boolean, boolean, boolean,
            boolean, boolean, boolean, boolean, boolean, 0..2, boolean,
            0..MaxBonusThreshold
          ] of
      record
        reachable: boolean;
      end; { record }
    { isomorphic to `array [ GameState ] of boolean',
      records per GameState whether it was reached }

procedure InitReachTable (var {out} rt: ReachTable);
function  IsReachable ({var} const rt: ReachTable; GS: GameState): boolean;
procedure UpdateReachTable (var rt: ReachTable; GS: GameState);

type
  GameStateRollPerCategory = array [ Category ] of
    record
      t: TurnIndexPlus; { t = TurnIndexOfGameState(g) }
      g: GameState;
      r: ValueBagRank;
    end; { record }
  { records per Category a GameState and a dice roll }

procedure InitGSR (var {out} gsr: GameStateRollPerCategory);
procedure UpdateGSR (var gsr: GameStateRollPerCategory; c: Category;
                     ti: TurnIndexPlus; GS: GameState; rk: ValueBagRank);
procedure WriteGSR (var f: text; s: stdstring;
                    {var} const gsr: GameStateRollPerCategory);

implementation

type
  GameStateTableP = ^GameStateTable;
  GSTFile = file of GameStateTable;

function  InspectGameStateTable ({var} const gst: GameStateTable;
                                 GS: GameState): real;
  begin
    with GS, gst[ Aces in free, Twos in free, Threes in free,
                  Fours in free, Fives in free, Sixes in free,
                  ThreeOfAKind in free, FourOfAKind in free,
                  FullHouse in free,
                  SmallStraight in free, LargeStraight in free,
                  2*ord(Yahtzee in free)+ord(chip), Chance in free, usneed
                ] do begin
      InspectGameStateTable := x
    end { with GS, gstbl[...] }
  end; { InspectGameStateTable }

procedure InitGameStateTable (var {out} gst: GameStateTable);
  var
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i13: boolean;
    i12, us: integer;
  begin
    for i1 := false to true do
      for i2 := false to true do
        for i3 := false to true do
          for i4 := false to true do
            for i5 := false to true do
              for i6 := false to true do
                for i7 := false to true do
                  for i8 := false to true do
                    for i9 := false to true do
                      for i10 := false to true do
                        for i11 := false to true do
                          for i12 := 0 to 2 do
                            for i13 := false to true do
                              for us := 0 to BonusThreshold do
                                gst[i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,us].x := Unknown
  end; { InitGameStateTable }

procedure WriteGameStateTable (var f: text; s: stdstring;
                               {var} const gst: GameStateTable);
  var
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14: boolean;
    us: integer;

  procedure WriteBoolean (b: boolean);
    begin
      case b of
        false: write(f, '.')
      ; true : write(f, 'X')
      end { case }
    end; { WriteBoolean }

  begin
    for i1 := true downto false do
      for i2 := false{true} downto false do
        for i3 := false{true} downto false do
          for i4 := false{true} downto false do
            for i5 := false{true} downto false do
              for i6 := true downto false do
                for i7 := false{true} downto false do
                  for i8 := false{true} downto false do
                    for i9 := false{true} downto false do
                      for i10 := false{true} downto false do
                        for i11 := false{true} downto false do
                          for i12 := false{true} downto false do
                            for i13 := false{true} downto false do
                              for i14 := false to false{not i12} do
                                for us := BonusThreshold downto 0 do
                                  with gst[i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,2*ord(i12)+ord(i14),i13,us] do begin
                                    write(f, s, '[')
                                  ; WriteBoolean(i1)
                                  ; WriteBoolean(i2)
                                  ; WriteBoolean(i3)
                                  ; WriteBoolean(i4)
                                  ; WriteBoolean(i5)
                                  ; WriteBoolean(i6)
                                  ; write(f, ' ')
                                  ; WriteBoolean(i7)
                                  ; WriteBoolean(i8)
                                  ; WriteBoolean(i9)
                                  ; write(f, ' ')
                                  ; WriteBoolean(i10)
                                  ; WriteBoolean(i11)
                                  ; write(f, 2*ord(i12)+ord(i14):1)
                                  ; WriteBoolean(i13)
                                  ; write(f, us:2, ']= ')
                                  ; writeln(f, x:10:5)
                                  end { with gst... }
  ; flush(f)
  end; { WriteGameStateTable }

procedure SaveGameStateTable (name: stdstring; {var} const gst: GameStateTable);
  var f: GSTFile;
  begin
    writeln(LogFile, 'Saving GameStateTable to file named "', name, '"')
  ; Flush ( LogFile )
  ; AssignFile(f, name)
  ; rewrite(f)
  ; write(f, gst)
  ; CloseFile(f)
  end; { SaveGameStateTable }

procedure LoadGameStateTable (name: stdstring; var {out} gst: GameStateTable);
  var f: GSTFile;
  begin
    writeln(LogFile, 'Loading GameStateTable from file named "', name, '"')
  ; Flush ( LogFile )
  ; AssignFile(f, name)
  ; reset(f)
  ; read(f, gst)
  ; CloseFile(f)
  end; { LoadGameStateTable }

function  NKnownGameStateTable ({var} const gst: GameStateTable): longint;
  var
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i13: boolean;
    i12, us: integer;
    n: longint;
  begin
    n := 0 { # Known entries in gst scanned so far }
  ; for i1 := false to true do
      for i2 := false to true do
        for i3 := false to true do
          for i4 := false to true do
            for i5 := false to true do
              for i6 := false to true do
                for i7 := false to true do
                  for i8 := false to true do
                    for i9 := false to true do
                      for i10 := false to true do
                        for i11 := false to true do
                          for i12 := 0 to 2 do
                            for i13 := false to true do
                              for us := 0 to BonusThreshold do
                                with gst[i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,us] do
                                  n := n + ord(x >= 0)
  ; NKnownGameStateTable := n
  end; { NKnownGameStateTable }


procedure WriteAuxNTable (var f: text; s: stdstring; {var} const at: AuxNTable;
                          lo, hi: ValueBagRank);
  var r: ValueBagRank;
  begin
    for r := lo to hi do
      writeln(f, s, '[', r:3, ']= ', at[r]:10)
  ; flush(f)
  end; { WriteAuxNTable }

procedure WriteAuxETable (var f: text; s: stdstring; {var} const at: AuxETable;
                          lo, hi: ValueBagRank);
  var r: ValueBagRank;
  begin
    for r := lo to hi do
      writeln(f, s, '[', r:3, ']= ', at[r]:10:5)
  ; flush(f)
  end; { WriteAuxETable }

function  NAssemblies: longint;
  { ret: # ways, per turn, for assembling a ValueBag in 3 rolls incl. keeping }
  var
    r, rr: ValueBagRank;
    h: longint;
    va: ValueBag;
    n0, n1, n2, n3: AuxNTable;
  begin

    { compute for all ValueBags K2 ("Keepers" after second roll):
        n3(K2) = (+ E: #K2+#E=NDice: 1) }
  ; for r := 0 to MaxValueBagRank do begin { r repr. K2 }
      with ValueBagTbl[r]{: K2}, ValueBagRanks[NDice - {K2.}sz] do
        n3[r] := lastvbr - firstvbr + 1
    end { for r }

    { compute for all ValueBags B2 of size NDice (dice after second roll):
        n2(B2) = (+ K2: K2 <= B2: n3(K2)) }
  ; with ValueBagRanks[NDice] do
      for r := firstvbr to lastvbr do begin { r repr. B2 }
        with ValueBagTbl[r]{: B2} do begin
          h := 0
        ; FirstValueSubBag(va) { va repr. K2 }
        ; repeat
            h := h + n3[RankFromValueBag(va)]
          until not NextValueSubBag(va, {B2.}fvb)
        ; n2[r] := h
        end { with }
      end { for r }

    { compute for all ValueBags K1 ("Keepers" after first roll):
        n1(K1) = (+ D: #K1+#D=NDice: n2(K1+D)) }
  ; for r := 0 to MaxValueBagRank do begin { r repr. K1 }
      with ValueBagTbl[r]{: K1}, ValueBagRanks[NDice - {K1.}sz] do begin
        h := 0
      ; for rr := firstvbr to lastvbr do begin { rr repr. D }
        ; {with ValueBagTbl[rr]: D do}
            AddValueBag(va, {K1.}fvb, ValueBagTbl[rr].fvb{=D.fvb})
        ; h := h + n2[RankFromValueBag(va)]
        end { for rr }
      ; n1[r] := h
      end { with }
    end { for r }

    { compute for all ValueBags B1 of size NDice (dice after first roll):
        n0(B1) = (+ K1: K1 <= B1: e1(K1)) }
  ; with ValueBagRanks[NDice] do
      for r := firstvbr to lastvbr do { r repr. B1 }
        with ValueBagTbl[r]{: B1} do begin
          h := 0
        ; FirstValueSubBag(va) { va repr. K1 }
        ; repeat
            h := h + n1[RankFromValueBag(va)]
          until not NextValueSubBag(va, {B1.}fvb)
        ; n0[r] := h
        end { with }

    { compute number of possible Yahtzee assemblies per turn as
        (+ B1: #B1=NDice: n0(B1)) }
  ; with ValueBagRanks[NDice] do begin
      h := 0
    ; for r := firstvbr to lastvbr do { r repr. B1 }
        with ValueBagTbl[r]{: B1} do
          h := h + n0[r]
    end { with }
  ; NAssemblies := h
  end; { NAssemblies }

procedure InitReachTable (var {out} rt: ReachTable);
  var
    i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i13: boolean;
    i12, us: integer; 
  begin 
    for i1 := false to true do
      for i2 := false to true do
        for i3 := false to true do
          for i4 := false to true do
            for i5 := false to true do
              for i6 := false to true do
                for i7 := false to true do
                  for i8 := false to true do
                    for i9 := false to true do
                      for i10 := false to true do
                        for i11 := false to true do
                          for i12 := 0 to 2 do
                            for i13 := false to true do
                              for us := 0 to BonusThreshold do
                                rt[i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,us].reachable := false
  end; { InitReachTable }

function  IsReachable ({var} const rt: ReachTable; GS: GameState): boolean;
  begin
    with GS, rt[ Aces in free, Twos in free, Threes in free,
                 Fours in free, Fives in free, Sixes in free,
                 ThreeOfAKind in free, FourOfAKind in free,
                 FullHouse in free,
                 SmallStraight in free, LargeStraight in free,
                 2*ord(Yahtzee in free)+ord(chip), Chance in free,
                 usneed
               ] do begin
      IsReachable := reachable
    end { with GS, rt }
  end; { IsReachable }

procedure UpdateReachTable (var rt: ReachTable; GS: GameState);
  begin
    with GS, rt[ Aces in free, Twos in free, Threes in free,
                 Fours in free, Fives in free, Sixes in free,
                 ThreeOfAKind in free, FourOfAKind in free,
                 FullHouse in free,
                 SmallStraight in free, LargeStraight in free,
                 2*ord(Yahtzee in free)+ord(chip), Chance in free,
                 usneed
               ] do begin
      reachable := true
    end { with GS, rt }
  end; { UpdateReachTable }

procedure InitGSR (var {out} gsr: GameStateRollPerCategory);
  { post: all gsr[_] set to a specific final game state }
  var c: Category;
  begin
    for c := FirstCategory to LastCategory do
      with gsr[c], g do begin
        t := LastTurnPlus
      ; free := [ ] ; usneed := 0 ; chip := true
      ; r := EmptyValueBagRank
      end { with }
  end; { InitGSR }

procedure UpdateGSR (var gsr: GameStateRollPerCategory; c: Category;
                     ti: TurnIndexPlus; GS: GameState; rk: ValueBagRank);
  { pre: ti~ = TurnIndexOfGameState(GS~) }
  begin
    with gsr[c] do begin
      if ti < t then begin
        t := ti
      ; g := GS
      ; r := rk
      end { if }
    end { with }
  end; { UpdateGSR }

procedure WriteGSR (var f: text; s: stdstring;
                    {var} const gsr: GameStateRollPerCategory);
  var c: Category;
  begin
    writeln(f)
  ; writeln(f, s)
  ; writeln(f, '  Cat | Turn GameState        Roll')
  ; writeln(f, '  --- + ---- ---------        ----')
  ; for c := FirstCategory to LastCategory do
      with gsr[c], g do begin
        writeln(f, CharFromCategory(c):4, '  |',
                t:4, StringFromGameState(g):18,
                StringFromValueBag(ValueBagTbl[r].fvb, false):6)
      end { with }
  ; flush(f)
  end; { WriteGSR }

end. { unit YahtzeeStateTables }
