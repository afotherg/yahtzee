unit      YahtzeeStatistics;
  { Statistics specific to Yahtzee games }

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

interface

uses
  YahtzeeConcepts,
  Statistics;

type
  GameStatistics = record
    rolls: integer; { # rolls in current game }
    vd: Distribution; { for rolled dice values }
    st: SerialTest;   { for rolled dice values }
    rd: Distribution; { for rolls }
    kd: Distribution; { for kept dice values }
    ad: array [ Aspect ] of Distribution;
  end; { record }

procedure MakeEmptyGameStatistics (var {out} GSX: GameStatistics);
procedure UpdateGameStatistics (var GSX: GameStatistics; ev: Event);
procedure FiniGameStatistics (var GSX: GameStatistics;
                              {var} const SC: ScoreCard);
procedure WriteGameStatisticsSummary (var f: text;
                                      var GSX: GameStatistics);
procedure WriteGameStatistics (var f: text; var GSX: GameStatistics;
                               verbose: boolean);

implementation

uses
  DiceStuff;

procedure MakeEmptyGameStatistics (var {out} GSX: GameStatistics);
  var a: Aspect;
  begin
    with GSX do begin
    ; rolls := 0
    ; MakeEmptyDistribution(vd, 'Rolled Values', MinValue, MaxValue)
    ; InitSerialTest(st)
    ; MakeEmptyDistribution(rd, '# Rolls',
        NTurnsPerGame, NRollsPerTurn*NTurnsPerGame)
    ; MakeEmptyDistribution(kd, 'Kept Values', MinValue, MaxValue)
    ; for a := FirstAspect to LastAspect do begin
        if a in RelevantAspects then begin
          MakeEmptyDistribution(ad[a], StringFromAspect(a),
                                0, MaxAspectScore[a])
        end { if }
      end { for a }
    end { with GSX }
  end; { MakeEmptyGameStatistics }

procedure UpdateGameStatistics (var GSX: GameStatistics; ev: Event);
  { pre: ev~ is completely defined (incl. scores, if kind=Scoring)
    post: GSX updated according to ev~.ev*sc }
  var
    i: DiceCount; { actually DiceIndex, but Sun Pascal then fails }
    v: DieValue;
  begin
    with GSX, ev do begin
      case kind of
        Rolling: begin
            with ValueListTbl[evreroll], fvl do
              for i := 1 to siz do begin
                UpdateDistribution(vd, val[i], 1)
              ; UpdateSerialTest(st, val[i])
              end { for i }
          ; rolls := succ(rolls)
          end { Rolling }
      ; Keeping: begin
            with ValueBagTbl[evkeepers] do
              for v := MinValue to MaxValue do
                UpdateDistribution(kd, v, fvb[v])
          end { Keeping }
      ; Scoring: begin
          { skip }
          end { Scoring }
      end { case kind }
    end { with GSX }
  end; { UpdateGameStatistics }

procedure FiniGameStatistics (var GSX: GameStatistics;
                              {var} const SC: ScoreCard);
  { call after each game; also resets for next game }
  var a: Aspect;
  begin
    with GSX, SC do begin
      UpdateDistribution(rd, rolls, 1)
    ; for a := FirstAspect to LastAspect do
        if a in RelevantAspects then
          UpdateDistribution(ad[a], box[a], 1)
    ; rolls := 0
    end { with GSX }
  end; { FiniGameStatistics }

procedure WriteGameStatisticsSummary (var f: text;
                                      var GSX: GameStatistics);
  { side-effect: GSX distributions characterized }
  var a: Aspect;
  begin
    with GSX do begin
      WriteDistributionSummaryHead(f)
    ; WriteDistributionSummaryLine(f, vd)
    ; WriteDistributionSummaryLine(f, rd)
    ; WriteDistributionSummaryLine(f, kd)
    ; for a := FirstAspect to LastAspect do begin
        if a in RelevantAspects then
          WriteDistributionSummaryLine(f, ad[a])
      end { for a }
    end { with GSX }
  end; { WriteGameStatisticsSummary }

procedure WriteGameStatistics (var f: text; var GSX: GameStatistics;
                               verbose: boolean);
  { side-effect: GSX distributions characterized }
  var a: Aspect;
  begin
    with GSX do begin
      WriteDistribution(f, vd, 1, DetailAll)
    ; if verbose then
        WriteDistributionCharacteristics(f, vd)
    ; WriteSerialTest(f, st, verbose)
    ; WriteDistribution(f, rd, 1, DetailNoZeroes)
    ; if verbose then
        WriteDistributionCharacteristics(f, rd)
    ; WriteDistribution(f, kd, 1, DetailAll)
    ; if verbose then
        WriteDistributionCharacteristics(f, kd)
    ; for a := FirstAspect to LastAspect do begin
        if a in RelevantAspects then begin
          if verbose then begin
            WriteDistribution(f, ad[a], 1, DetailNoZeroes)
          ; if a = GrandTotal then
              WriteDistribution(f, ad[a], 20, DetailNoZeroes)
          end { if }
        ; WriteDistributionCharacteristics(f, ad[a])
        end { if }
      end { for a }
    end { with GSX }
  end; { WriteGameStatistics }

end. { unit YahtzeeStatistics }
