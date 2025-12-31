unit      YahtzeeProficiencyTests;
  { Yahtzee Proficiency Tests }

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
  DiceDevils,
  YahtzeeGameRecordings;

procedure PlayStrategy (YR: YahtzeeRules; DD: DiceDevil; YS: YahtzeeStrategy;
                        var {out} SC: ScoreCard; var {out} GR: GameRecording);
procedure YahtzeeProficiencyTest (YR: YahtzeeRules; DD: DiceDevil;
                                  YS, RefYS: YahtzeeStrategy);

implementation

uses
  GeneralStuff,
  PseudoRandomGenerator,
  DiceStuff;

procedure PlayStrategy (YR: YahtzeeRules; DD: DiceDevil; YS: YahtzeeStrategy;
                        var {out} SC: ScoreCard; var {out} GR: GameRecording);
  { Let YS play a game of Yahtzee while DD rolls dice according to rules YR.
    The whole game is recorded in SC, GR. }
  var
    GS, GSnew: GameState;
    TS: TurnState;
    ev: Event;
    dSC: ScoreCard;
  begin
    writeln
  ; writeln('The ', YS^.name, ' Strategy now plays a game of Yahtzee...')
  ; InitDiceDevil(DD)
  ; MakeEmptyGameRecording(GR)
  ; GS := InitialGameState
  ; SC := EmptyScoreCard
  ; InitYahtzeeStrategy(YS)
  ; with TS, ev do begin
      while not IsFinalGameState(GS) do begin
        TS := InitialTurnState
      ; repeat { until kind = Scoring }
          TickleDiceDevil(DD, GS, TS, ev)
        ; TurnStateAfterEvent(TS, ev)
        ; StrategyChoice(YS, GS, TS, SC.box[GrandTotal], ev, true)
        ; if kind = Keeping then begin
            ExtendGameRecording(GR, GS, SC.box[GrandTotal], TS, ev)
          ; TurnStateAfterEvent(TS, ev)
          end { if }
        until kind = Scoring
      ; ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
      ; ExtendGameRecording(GR, GS, SC.box[GrandTotal], TS, ev)
      ; UpdateScoreCard(SC, dSC)
      ; GS := GSnew
      end { while }
    end { with }
  ; writeln
  ; writeln('GAME ENDED')
  end; { PlayStrategy }

procedure YahtzeeProficiencyTest (YR: YahtzeeRules; DD: DiceDevil;
                                  YS, RefYS: YahtzeeStrategy);
  { Let YS play a game of Yahtzee while DD rolls dice according rules YR.
    The whole game is recorded.
    Afterwards, the whole game is listed,
    together with choices by RefYS and delta between YS and RefYS. }
  var
    SC: ScoreCard;
    GR: GameRecording;
    initialseed: integer;
  begin
    writeln
  ; writeln('The ', YS^.name,
            ' Strategy now takes the Yahtzee Proficiency Test.')
  ; initialseed := wallclock
  ; InitRandom(initialseed)
  ; PlayStrategy(YR, DD, YS, SC, GR)
  ; writeln
  ; writeln('Yahtzee Proficiency Test Result for the ', YS^.name, ' Strategy')
  ; write('  with random seed ', initialseed:10)
  ; WriteCurrentDateTime(output, '')
  ; WriteGameRecording(output, GR, YS^.shortname, YR, true, RefYS)
  end; { YahtzeeProficiencyTest }

end. { unit YahtzeeProficiencyTests }
