unit      YahtzeeStrategySampler;
  { Play a number of Yahtzee games with a given strategy and
    collect statistics }

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
  YahtzeeStrategies,
  DiceDevils;

procedure SampleStrategy (YR: YahtzeeRules; DD: DiceDevil; YS: YahtzeeStrategy;
                          n: longint; verbositylevel: integer);

implementation

uses
  YahtzeeStatistics;

procedure SampleStrategy (YR: YahtzeeRules; DD: DiceDevil; YS: YahtzeeStrategy;
                          n: longint; verbositylevel: integer);
  { pre: n~ >= 0
  { post: A sample of n~ games for strategy YS under YR,DD has been drawn.
    Along the way, some statistics are collected and printed.
    N.B. Outputting start value of random seed is caller's responsibility.
    Intended to be fast. }
  var
    g: longint; { game counter }
    GS, GSnew: GameState;
    TS: TurnState;
    ev: Event;
    SC, dSC: ScoreCard;
    GSX: GameStatistics;
  begin
    writeln
  ; writeln('Collecting sample of ', n:1, ' games for ', YS^.name, ' Strategy')
  ; writeln('  with ', DD^.name, ' as Dice Devil')
  ; flush(output)
  ; InitDiceDevil(DD)
  ; MakeEmptyGameStatistics(GSX)
  ; for g := 1 to n do begin { play g-th game }
{
      if verbositylevel >= 2 then
        writeln('Game ', g:10)
}
    ; GS := InitialGameState
    ; SC := EmptyScoreCard
    ; InitYahtzeeStrategy(YS)
    ; with TS, ev do begin
        while GS.free <> [ ] {not IsFinalGameState(GS)} do begin
{
          if verbositylevel>=5 then
            writeln('  GS: ', StringFromGameState(GS))
}
        ; TS := InitialTurnState
        ; repeat { until kind = Scoring }
            TickleDiceDevil(DD, GS, TS, ev)
          ; UpdateGameStatistics(GSX, ev)
          ; TurnStateAfterEvent(TS, ev)
{
          ; if verbositylevel >= 5 then
              writeln('        ', StringFromTurnState(TS, true, false))
}
          ; StrategyChoice(YS, GS, TS, SC.box[GrandTotal], ev, true)
          ; if kind = Keeping then begin
              UpdateGameStatistics(GSX, ev)
            ; TurnStateAfterEvent(TS, ev)
{
            ; if verbositylevel >= 5 then
                writeln('        ', StringFromTurnState(TS, true, false))
}
            end { if }
          until kind = Scoring
        ; ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
{
        ; if verbositylevel >= 4 then
            WriteEvent(output, '        ', ev)
}
        ; UpdateScoreCard(SC, dSC)
{       ; UpdateGameStatistics(GSX, ev) }
        ; GS := GSnew
        end { while }
        { game ended }
{
      ; if verbositylevel >= 3 then
          writeln('  Grand Total= ', GSX.total:4)
}
      ; FiniGameStatistics(GSX, SC)
      end { with TS, ev }
    end { for g }
{
  ; if verbositylevel >= 3 then
      AwaitEnter('Sampling done')
}
  ; WriteGameStatisticsSummary(output, GSX)
  ; WriteGameStatistics(output, GSX, (verbositylevel >= 1))
  end; { SampleStrategy }

end. { unit YahtzeeStrategySampler }
