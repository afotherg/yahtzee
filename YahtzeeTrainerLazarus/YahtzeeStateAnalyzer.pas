unit      YahtzeeStateAnalyzer;
  { Analyze Yahtzee state }

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
  YahtzeeStrategies;

procedure SingleStep (YS: YahtzeeStrategy);

implementation

uses
  GeneralStuff,
  DiceStuff,
  YahtzeeConcepts,
  YahtzeeEventLists;

procedure SingleStep (YS: YahtzeeStrategy);
  { pre: stdin contains free,usneed,chip,RollIndex,roll,score
    post: stdout contains: EList (compact), sorted on expected final score
  }
  var
    EL: EList;
    GS: GameState;
    TS: TurnState;
    vl: ValueList;
    vb: ValueBag; { vb equiv vl }
    total: Score;
  begin
    writeln('Enter 5 lines:')
  ; writeln('  free patterns from 123456tfhslyc ('' '', ''_'' ignored)')
  ; writeln('  bonus need in [0..', BonusThreshold:1, ']')
  ; writeln('  chip in [false..true] (extra Yahtzee scores bonus)')
  ; writeln('  rolls left in [0..', NModificationAttempts:1, ']')
  ; writeln('  roll in [', MinValue:1, '..', MaxValue:1, ']^', NDice:1,
            ' ('' '', ''_'' ignored)')
  ; writeln('  total score in [0..', MaxScore:1, ']')
  ; writeln(CutLine)
  ; ReadGameState(input, '', GS, true)
  ; with TS do begin
      readln(TS.rollsleft)
    ; phase := ToChoose
    ; ReadValueList(input, '', vl)
    ; readln
    ; ValueBagFromValueList(vb, vl)
    ; tsroll := RankFromValueBag(vb)
    end { with TS }
  ; readln(total)
  ; MakeChoiceEList(EL, YS, GS, TS, total)
  ; SortEList(EL)
  ; WriteEListCompact(output, EL, vl, false)
  end; { SingleStep }

end. { unit YahtzeeStateAnalyzer }
