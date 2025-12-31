unit      YahtzeeStrategyExplorer;
  { Dice rolls are generated internally or read from input.
    The game state is either input or automatically maintained.
    The strategy choices are generated and output.
  }

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

procedure ExploreStrategy (YR: YahtzeeRules; DD: DiceDevil;
                           YS, RefYS: YahtzeeStrategy);

implementation

uses
  GeneralStuff,
  Statistics,
  YahtzeeStatistics,
  YahtzeeEventLists,
  YahtzeeStrategyAnalysis;

procedure ReadCommand (var f: text; prompt: stdstring;
                       commands: CharSet; default: char;
                       var {out} ch: char);
  { pre:  all char in commands~ are upper case /\ default~ in command
    post: ch in commands~, input is converted to upper case }
  begin
    repeat
      ; write(prompt, ' [', default, ']? ')
      ; if eoln(f) then ch := default
        else ReadChar(f, ch)
      ; readln(f)
      ; ch := UpChar(ch)
    until ch in commands
  end; { ReadCommand }

procedure ExploreStrategy (YR: YahtzeeRules; DD: DiceDevil;
                           YS, RefYS: YahtzeeStrategy);
  { Confront YS and DD under YR, while user can modify game-turn state, and
    advise from RefYS is shown (RefYS.ECache <> nil desirable). }
  { N.B. This version deals only with abstract state. }

  var
    SC, dSC: ScoreCard;
    GS, GSnew: GameState;
    TS: TurnState;
    ev: Event;
    EL: EList;
    ch: char;
    GSX: GameStatistics;

  procedure InitExplorer;
    begin
      GS := InitialGameState
    ; TS := InitialTurnState
    ; SC := EmptyScoreCard
    ; InitYahtzeeStrategy(YS)
    end; { InitExplorer }

  procedure ReadState;
    var tot: longint;
    begin
      repeat
        ReadGameState(input, 'New (nonfinal) game state: ', GS, true)
      until not IsFinalGameState(GS)
    ; ReadTurnState(input, 'New turn state: ', TS, false, true)
    ; write('  total? ')
    ; readln(tot)
    ; SC := EmptyScoreCard
    ; SC.box[GrandTotal] := tot
    ; if GS.usneed=0 then
        SC.box[USBonus] := UpperSectionBonus
    end; { ReadState }

  begin { ExploreStrategy }
    writeln
  ; writeln('Let''s play some games of Yahtzee')
  ; writeln('  The ', DD^.name, ' plays the Dice Devil.')
  ; writeln('  The ', YS^.name, ' Strategy makes the choices.')
  ; MakeEmptyGameStatistics(GSX)
  ; InitDiceDevil(DD)
  ; InitExplorer
  ; repeat { until ch='S' }
      repeat { until ch in ['C', 'S'] }
        writeln
      ; WriteGameState(output, 'Game state: ', GS)
      ; WriteTurnState(output, 'Turn state: ', TS)
      ; write('Total score = ', SC.box[GrandTotal]:1)
      ; if RefYS <> nil then begin
          write(', Expected score= ',
                  SC.box[GrandTotal] + ExpectedScore(RefYS, GS, TS):1:5)
                  { was: + OptEScore(GS, TS):1:5) (why?) }
        ; if RefYS^.VCache<>nil then
            write(' +/- ', sqrt(VarianceScore(RefYS, GS, TS)):1:2)
        end { if }
      ; writeln
      ; ReadCommand(input,
                    'Use: C(urrent state, I(nitial state, R(ead state, S(top',
                    ['C', 'I', 'R', 'S'], 'C', ch)
      ; case ch of
          'C': { skip }
        ; 'I': begin
            writeln('Reset to initial state')
          ; InitExplorer
          end { 'I' }
        ; 'R': ReadState
        ; 'S': { skip }
        end { case }
      until ch in ['C', 'S']

    ; if ch<>'S' then begin
        with TS, ev do begin
          case phase of

            ToRoll: begin
                MakeRollEList(EL, RefYS, GS, TS, SC.box[GrandTotal])
              ; SortEList(EL)
              ; WriteExtremesEList(output, EL)
              ; TickleDiceDevil(DD, GS, TS, ev)
              ; WriteEvent(output, 'Roll: ', ev)
              ; UpdateGameStatistics(GSX, ev)
              ; TurnStateAfterEvent(TS, ev)
              end { ToRoll }

          ; ToChoose: begin
                MakeChoiceEList(EL, RefYS, GS, TS, SC.box[GrandTotal])
              ; SortEList(EL)
              ; WriteEList(output, EL)
              ; StrategyChoice(YS, GS, TS, SC.box[GrandTotal], ev, false)
              ; case kind of
                  Keeping: begin
                      WriteEvent(output, 'Choice: ', ev)
                    ; UpdateGameStatistics(GSX, ev)
                    ; TurnStateAfterEvent(TS, ev)
                    end { Keeping }
                ; Scoring: begin
                      ScoreEvent(YR, GS, TS, ev, dSC, GSnew)
                    ; UpdateScoreCard(SC, dSC)
                    ; UpdateGameStatistics(GSX, ev)
                    ; WriteEvent(output, 'Choice: ', ev)
                    ; if IsFinalGameState(GSnew) then begin
                        writeln('GAME ENDED')
                      ; WriteScoreCard(output, SC, YR)
                      ; FiniGameStatistics(GSX, SC)
                      ; InitExplorer
                      end { then }
                      else begin
                        GS := GSnew
                      ; TS := InitialTurnState
                      end { else }
                    end { Scoring }
                end { case kind }
              end { ToChoose }

          end { case phase }
        end { with TS, ev }
      end { if }
    until ch='S'
  ; writeln('Explore time is over')
  ; WriteGameStatisticsSummary(output, GSX)
  end; { ExploreStrategy }

end. { unit YahtzeeStrategyExplorer }
