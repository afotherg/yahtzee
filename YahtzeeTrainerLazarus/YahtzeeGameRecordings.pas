unit      YahtzeeGameRecordings;
  { Manipulate recordings of Yahtzee games, including annotated writing }

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
  Classes, { for TStrings }
  GeneralStuff,
  YahtzeeConcepts,
  YahtzeeStrategies;

const
  MaxGameRecordingIndex = NTurnsPerGame * MaxRollsPerTurn;

type
  GameRecordingIndex = 1..MaxGameRecordingIndex;

  GameRecording = record
    len: 0..MaxGameRecordingIndex; { list[1..len] is defined }
    list: array [ GameRecordingIndex ] of record
      GS: GameState; { not a final state }
      total: Score;  { needed in case events are not in game sequence }
      TS: TurnState; { phase = ToChoose }
      ev: Event; { kind <> Rolling, scores need not be defined }
    end; { record }
  end; { record }

procedure MakeEmptyGameRecording (var {out} GR: GameRecording);
procedure ExtendGameRecording (var GR: GameRecording; g: GameState;
                               tot: Score; t: TurnState; e: Event);
procedure WriteGameRecording (var f: text; {var} const GR: GameRecording;
                              {var} const name: stdstring; YR: YahtzeeRules;
                              annotate: boolean; RefYS: YahtzeeStrategy);
procedure ReadGameRecording (var f: text; var {out} GR: GameRecording);
procedure GameRecordingToTStr ({var} str: TStrings; {var} const GR: GameRecording;
                               {var} const name: stdstring; YR: YahtzeeRules;
                               annotate: boolean; RefYS: YahtzeeStrategy);

implementation

uses
  SysUtils, { for Format }
  DiceStuff,
  YahtzeeEventLists;

procedure MakeEmptyGameRecording (var {out} GR: GameRecording);
  begin
    with GR do begin
      len := 0
    end { with GR }
  end; { MakeEmptyGameRecording }

procedure ExtendGameRecording (var GR: GameRecording; g: GameState;
                               tot: Score; t: TurnState; e: Event);
  { pre:  # GR~ < MaxGameRecordingIndex /\ t~.phase=ToChoose
    post: GR = GR~ ++ [ (g~,tot~,t~,e~) ] }
  begin
    if not(GR.len < MaxGameRecordingIndex) then Fail('ExtendGameRecording',
          'GR.len < MaxGameRecordingIndex')
  ; if not(t.phase = ToChoose) then Fail('ExtendGameRecording',
          't.phase = ToChoose')
  ; with GR do begin
      len := succ(len)
    ; with list[len] do begin
      ; GS := g
      ; total := tot
      ; TS := t
      ; ev := e
      end { with }
    end { with GR }
  end; { ExtendGameRecording }

procedure WriteGameRecording (var f: text; {var} const GR: GameRecording;
                              {var} const name: stdstring; YR: YahtzeeRules;
                              annotate: boolean; RefYS: YahtzeeStrategy);
  { side-effect: scores filled in in events of GR }
  const
    thr_large  = 5.0;
    thr_medium = 1.0;
    thr_small  = 0.01;
    dr = 2; { # decimals displayed in reals }
    ws = 4+1+dr; { field width for displaying expected scores and deltas }
    wt = 1+1+dr; { field width for displaying delta thresholds }

  var
    ndlarge: integer;  { # choices with delta in [thr_large, inf) }
    ndmedium: integer; { # choices with delta in [thr_medium, thr_large) }
    ndsmall: integer;  { # choices with delta in [thr_small, thr_medium) }
    ndeps: integer;    { # choices with delta in [0.0, thr_small) }
    ndopt: integer;    { # choices equal to reference strategy }
    nc: integer;       { # choices = ndlarge+ndmedium+ndsmall+ndeps+ndopt }
    totalscore: integer;
    GSold: GameState;
    ti: integer; { turn index }
    i: {GameRecordingIndex} integer;
    expsc: real;
    delta, delta_tot: real;
    eli: EListIndex;
    SC, dSC: ScoreCard;
    GSnew: GameState;
    EL: EList;
 
  begin { WriteGameRecording }
    writeln(f)
  ; write(f, 'Game Details')
  ; if annotate then
      write(f, ' and Analysis')
  ; writeln(f)
  ; write(f, '==== =======')
  ; if annotate then
      write(f, ' === ========')
  ; writeln(f)
  ; writeln(f)
  ; write(f,
' # Game State        Tot Roll     ', name:MaxShortNameLength)
  ; if annotate then
      write(f,                         '  Expect SD  ',
            RefYS^.shortname:MaxShortNameLength,         '  Expect SD  Delta')
  ; writeln(f)
  ; write(f,
'-- ----------------- --- -------  -----')
  ; if annotate then
      write(f,                         '  ------ --  -----  ------ --  -----')
  ; writeln(f)
  ; ndlarge := 0 ; ndmedium := 0 ; ndsmall := 0 ; ndeps := 0 ; ndopt := 0
  ; nc := 0 ; totalscore := 0 ; ti := 0 ; delta_tot := 0.0
  ; GSold.free := [ ]
  ; SC := EmptyScoreCard
  ; with GR do begin
      for i := 1 to len do begin
        with list[i] do begin
          if not IsEqualGameState(GSold, GS) then begin
            ti := succ(ti)
          ; write(f, ti:2, ' ', StringFromGameState(GS), total:4, ' ')
          end { then }
          else
            write(f, ';':2+1+13+1, '':3+0+4+1)
        ; write(f, StringFromTurnState(TS, false, false), '  ')
        ; if ev.kind = Scoring then begin
            ScoreEvent(YR, GS, TS, ev, dSC, GSnew {ignored})
          ; UpdateScoreCard(SC, dSC)
          end { if }
        ; write(f, StringFromEvent(ev, false, false), ' ')
        ; if annotate then begin
            MakeChoiceEList(EL, RefYS, GS, TS, total)
          ; SortEList(EL)
          ; FindEList(EL, ev, eli)
          ; with EL.list[eli] do begin
              expsc := EL.list[eli].Es
            ; write(f, expsc:ws:dr, ' ')
            ; if sd < 0.0 then
                write(f, ' ?  ')
              else
                write(f, sd:2:0, '  ')
            end { with }
          ; if EL.len > 1 then begin { more than one option to choose from }
              if IsEqualEvent(ev, EL.list[1].ev) then begin
                ndopt := succ(ndopt)
              end { then }
              else { RefYS choice differs } begin
                with EL.list[1] do begin
                  write(f, StringFromEvent(ev, false, false), ' ')
                ; write(f, Es:ws:dr, ' ')
                ; delta := Es - expsc
                ; if sd < 0.0 then
                    write(f, ' ?')
                  else
                    write(f, sd:2:0)
                end { with }
              ; write(f, delta:ws:dr, ' ')
              ; delta_tot := delta_tot + delta
              ; if delta >= thr_medium then begin
                  if delta >= thr_large then begin
                    write(f, '***')
                  ; ndlarge  := succ(ndlarge)
                  end { then }
                  else begin
                    write(f, '**')
                  ; ndmedium  := succ(ndmedium)
                  end { else }
                end { then }
                else begin
                  if delta >= thr_small then begin
                    write(f, '*')
                  ; ndsmall  := succ(ndsmall)
                  end { then }
                  else begin
                    write(f, '<>')
                  ; ndeps := succ(ndeps)
                  end { else }
                end { else }
              end { else }
            ; nc := succ(nc)
            end { if E.len > 1 }
          end { if annotate }
        ; writeln(f)
        ; with ev, dSC do begin
            if kind = Scoring then
              totalscore := totalscore + box[GrandTotal]
          end { with ev }
        ; GSold := GS
        end { with list[i] }
      end { for i }
    end { with GR }
  ; write(f,
'-- ----------------- --- -------  -----')
  ; if annotate then
      write(f,                         '  ------ --  -----  ------ --  -----')
  ; writeln(f)
  ; write(f,
'                                  ', totalscore:5)
  ; if annotate then
      write(f,                         '                             ',
            delta_tot:ws:dr)
  ; writeln(f)
  ; if annotate and (nc <> 0) then begin
      writeln(f)
    ; writeln(f, '  # Choices (one option = no choice)  :', nc:4, ' total')
    ; writeln(f, '  # Choices identical to ',
                 RefYS^.shortname:MaxShortNameLength,
                 '       ',
                 ' :', ndopt:4)
    ; writeln(f, '  # Choices with ',
                 ' ':wt,          '    delta < ', thr_small :wt:dr,
                 ' :', ndeps:4, ' <>')
    ; writeln(f, '  # Choices with ',
                 thr_small :wt:dr, ' <= delta < ', thr_medium:wt:dr,
                 ' :', ndsmall:4, ' *')
    ; writeln(f, '  # Choices with ',
                 thr_medium:wt:dr, ' <= delta < ', thr_large:wt:dr,
                 ' :', ndmedium:4, ' **')
    ; writeln(f, '  # Choices with ',
                 thr_large :wt:dr, ' <= delta   ', ' ':wt,
                 ' :', ndlarge:4, ' ***')
    ; writeln(f, '  Mean delta per choice               :',
                 delta_tot/nc:ws+1:dr+1)
    end { if }
  ; WriteScoreCard(f, SC, YR)
  ; flush(f)
  end; { WriteGameRecording }

procedure ReadGameRecording (var f: text; var {out} GR: GameRecording);
  { pre: f contains sequence of game choice states and choice events,
         one per line, terminated by an empy line;
         free=[ ] copies previous game state
         Format: free; usneed chip total roll;rollsleft  choice-event
         score is optional in choice event (ignored)
         Example:
      __345_TFH_LY_; 37 false   77  12366;2   k66___
                   ;                11666;1   k666__
                   ;                34666;0   sT  25
      __345__FH_LY_; 37 false  102  44456;2   k444__
    post: GR contains game from f
  }
  var
    GS, pGS: GameState;
    tot, ptot: Score;
    TS: TurnState;
    ev: Event;
    vb: ValueBag;
  begin
    MakeEmptyGameRecording(GR)
  ; pGS.free := [ ]
  ; with GS, TS, ev do begin
    ; phase := ToChoose
    ; while not eoln do begin
        ReadCategorySet(f, '', free)
      ; if not(not(IsFinalGameState(pGS) and IsFinalGameState(GS))) then
          Fail('ReadGameRecording',
              '(not(IsFinalGameState(pGS) and IsFinalGameState(GS)')
      ; if IsFinalGameState(GS) then begin
          GS := pGS
        ; tot := ptot
        end { then }
        else begin
          read(f, usneed)
        ; ReadBoolean(f, chip)
{
;WriteGameState(output, 'GS: ', GS)
}
        ; read(f, tot)
        ; pGS := GS
        ; ptot := tot
        end { else }
      ; ReadValueBag(f, '', vb)
      ; tsroll := RankFromValueBag(vb)
      ; read(f, rollsleft)
{
;WriteTurnState(output, 'TS: ', TS)
}
      ; if not(ValueBagSize(vb) = NDice) then Fail('ReadGameRecording',
              'roll size = NDice')
      ; ReadEvent(f, '', ev)
{
;WriteEvent(output, 'ev:', ev)
}
      ; if not(kind <> Rolling) then Fail('ReadGameRecording',
              'ev.kind <> Rolling')
      ; if not(IsSubValueBag(ValueBagTbl[tskeepers].fvb,
                             ValueBagTbl[tsroll].fvb)) then
          Fail('ReadGameRecording',
               'keepers subbag of roll')
      ; readln(f)
      ; ExtendGameRecording(GR, GS, tot, TS, ev)
      end { while }
    end { with TS }
  end; { ReadGameRecording }

procedure GameRecordingToTStr ({var} str: TStrings; {var} const GR: GameRecording;
                               {var} const name: stdstring; YR: YahtzeeRules;
                               annotate: boolean; RefYS: YahtzeeStrategy);
  { side-effect: scores filled in in events of GR }
  const
    thr_large  = 5.0;
    thr_medium = 1.0;
    thr_small  = 0.01;
    dr = 2; { # decimals displayed in reals }
    ws = 4+1+dr; { field width for displaying expected scores and deltas }
    wt = 1+1+dr; { field width for displaying delta thresholds }

  var
    ndlarge: integer;  { # choices with delta in [thr_large, inf) }
    ndmedium: integer; { # choices with delta in [thr_medium, thr_large) }
    ndsmall: integer;  { # choices with delta in [thr_small, thr_medium) }
    ndeps: integer;    { # choices with delta in [0.0, thr_small) }
    ndopt: integer;    { # choices equal to reference strategy }
    nc: integer;       { # choices = ndlarge+ndmedium+ndsmall+ndeps+ndopt }
    totalscore: integer;
    GSold: GameState;
    ti: integer; { turn index }
    i: {GameRecordingIndex} integer;
    expsc: real;
    delta, delta_tot: real;
    eli: EListIndex;
    SC, dSC: ScoreCard;
    GSnew: GameState;
    EL: EList;
    s: String;
 
  begin { GameRecordingToTStr }
    { to do?: check existence of str }
    str.Append('')
  ; s := 'Game Details'
  ; if annotate then
      s := s + ' and Analysis'
  ; str.Append(s)
  ; s := '==== ======='
  ; if annotate then
      s := s + ' === ========'
  ; str.Append(s)
  ; str.Append('')
  ; s := Format (
' # Game State        Tot Roll     %*s',
                  [ MaxShortNameLength, name ] )
  ; if annotate then
      s := s + Format (                '  Expect SD  %*s  Expect SD  Delta',
                        [ MaxShortNameLength, RefYS^.shortname ] )
  ; str.Append(s)
  ; s :=
'-- ----------------- --- -------  -----'
  ; if annotate then
      s := s +                         '  ------ --  -----  ------ --  -----'
  ; str.Append(s)
  ; ndlarge := 0 ; ndmedium := 0 ; ndsmall := 0 ; ndeps := 0 ; ndopt := 0
  ; nc := 0 ; totalscore := 0 ; ti := 0 ; delta_tot := 0.0
  ; GSold.free := [ ]
  ; SC := EmptyScoreCard
  ; with GR do begin
      for i := 1 to len do begin
        with list[i] do begin
          if not IsEqualGameState(GSold, GS) then begin
            ti := succ(ti)
          ; s := Format ( '%2d %s%4d ',
                          [ ti, StringFromGameState(GS), total ] )
          end { then }
          else
            s := Format ( '%*s%*s', [ 2+1+13+1, ';', 3+0+4+1, '' ] )
        ; s := s + StringFromTurnState(TS, false, false) + '  '
        ; if ev.kind = Scoring then begin
            ScoreEvent(YR, GS, TS, ev, dSC, GSnew {ignored})
          ; UpdateScoreCard(SC, dSC)
          end { if }
        ; s := s + StringFromEvent(ev, false, false) + ' '
        ; if annotate then begin
            MakeChoiceEList(EL, RefYS, GS, TS, total)
          ; SortEList(EL)
          ; FindEList(EL, ev, eli)
          ; with EL.list[eli] do begin
              expsc := EL.list[eli].Es
            ; s := s + Format ( '%*.*f ', [ ws, dr, expsc  ] )
            ; if sd < 0.0 then
                s := s + ' ?  '
              else
                s := s + Format ( '%2.0f  ', [ sd ] )
            end { with }
          ; if EL.len > 1 then begin { more than one option to choose from }
              if IsEqualEvent(ev, EL.list[1].ev) then begin
                ndopt := succ(ndopt)
              end { then }
              else { RefYS choice differs } begin
                with EL.list[1] do begin
                  s := s + StringFromEvent(ev, false, false) + ' '
                ; s := s + Format ( '%*.*f ', [ ws, dr, Es ] )
                ; delta := Es - expsc
                ; if sd < 0.0 then
                    s := s + ' ?'
                  else
                    s := s + Format ( '%2.0f', [ sd ] )
                end { with }
              ; s := s + Format ( '%*.*f ', [ ws, dr, delta ] )
              ; delta_tot := delta_tot + delta
              ; if delta >= thr_medium then begin
                  if delta >= thr_large then begin
                    s := s + '***'
                  ; ndlarge  := succ(ndlarge)
                  end { then }
                  else begin
                    s := s + '**'
                  ; ndmedium  := succ(ndmedium)
                  end { else }
                end { then }
                else begin
                  if delta >= thr_small then begin
                    s := s + '*'
                  ; ndsmall  := succ(ndsmall)
                  end { then }
                  else begin
                    s := s + '<>'
                  ; ndeps := succ(ndeps)
                  end { else }
                end { else }
              end { else }
            ; nc := succ(nc)
            end { if E.len > 1 }
          end { if annotate }
        ; str.Append(s)
        ; with ev, dSC do begin
            if kind = Scoring then
              totalscore := totalscore + box[GrandTotal]
          end { with ev }
        ; GSold := GS
        end { with list[i] }
      end { for i }
    end { with GR }
  ; s :=
'-- ----------------- --- -------  -----'
  ; if annotate then
      s := s +                         '  ------ --  -----  ------ --  -----'
  ; str.Append(s)
  ; s := Format (
'                                  %5d', [ totalscore ] )
  ; if annotate then
      s := s + Format (                '                             %*.*f',
            [ ws, dr, delta_tot ] )
  ; str.Append(s)
  ; if annotate and (nc <> 0) then begin
      str.Append('')
    ; s := Format ( '  # Choices (one option = no choice)  :%4d total', [ nc ] )
    ; str.Append(s)
    ; s := Format ( '  # Choices identical to %*s        :%4d',
                    [ MaxShortNameLength, RefYS^.shortname, ndopt ] )
    ; str.Append(s)
    ; s := Format ( '  # Choices with %*s    delta < %*.*f :%4d %s',
                    [ wt, '', wt, dr, thr_small, ndeps, '<>' ] )
    ; str.Append(s)
    ; s := Format ( '  # Choices with %*.*f <= delta < %*.*f :%4d %s',
                    [  wt, dr, thr_small, wt, dr, thr_medium, ndsmall, '*' ] )
    ; str.Append(s)
    ; s := Format ( '  # Choices with %*.*f <= delta < %*.*f :%4d %s',
                    [ wt, dr, thr_medium, wt, dr, thr_large, ndmedium, '**' ] )
    ; str.Append(s)
    ; s := Format ( '  # Choices with %*.*f <= delta   %*s :%4d %s',
                    [ wt, dr, thr_large, wt, '', ndlarge, '***' ] )
    ; str.Append(s)
    ; s := Format ( '  Mean delta per choice               :%*.*f',
                    [ ws+1, dr+1, delta_tot/nc ] )
    ; str.Append(s)
    end { if }
  ; ScoreCardToTStr(str, SC, YR)
  end; { GameRecordingToTStr }

end. { unit YahtzeeGameRecordings }
