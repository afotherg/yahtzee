UNIT      YahtzeeConcepts;
  { Basic concepts to define the game of Yahtzee }

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
  Classes, { for Delphi: TStrings }
  GeneralStuff,
  DiceStuff;

const
  MaxScore    =        1575;
  MaxScoreLength    =     4; { max. # digits in a Score }
  MaxBonusThreshold =    63; { 0 if not applicable, reduces memory requirement }

{ put following in YahtzeeRules }
  BonusThreshold    =    63; { <= MaxBonusThreshold, 0 if not applicable }
  UpperSectionBonus =    35; { 0 if not applicable, also set BonusThreshold=0 }
  ExtraYahtzeeBonus =   100; { 0 if not applicable }
  JokerRule         =  true;
    { when rolling a Yahtzee AND Yahtzee category is no longer free AND
      the corresponding upper section box has been used,
      then score a Yahtzee according to Joker Rule,
      i.e. obtain full score for a Full House or Sm/Lg Straight }
{ end to be put in YahtzeeRules }

type
  Aspect = ( Aces {Ones}, Twos, Threes, Fours, Fives, Sixes,
             ThreeOfAKind, FourOfAKind {Carre}, FullHouse,
             SmallStraight, LargeStraight, Yahtzee, Chance,
             USBonus, EYBonus, GrandTotal, YahtzeesRolled, JokersApplied,
             SpecialAspect );

const
  FirstAspect   = Aces;
  LastAspect    = SpecialAspect;
    RelevantAspects = [ FirstAspect .. JokersApplied ]; { incorporate in YR }
    {RelevantAspects = [ FirstAspect .. USBonus, GrandTotal, YahtzeesRolled ];}
  FirstCategory = Aces;
  LastCategory  = Chance;

type
  Category = FirstCategory .. LastCategory; { to be chosen by player }

  CategorySet = set of Category;

const
  UpperSection  = [ Aces .. Sixes ];
  LowerSection  = [ ThreeOfAKind .. Chance ];
  JokerSpecials = [ FullHouse .. LargeStraight ];
  JuniorCategories = UpperSection;
  AllCategories = UpperSection + LowerSection;

type
  Score = 0..MaxScore { integer16 { Sun Pascal };

  ScoreCard = record { Full state between turns }
    box: array [ Aspect ] of Score;
      { categories beyond GrandTotal are for statistics only }
  end; { record }

const
  FirstTurn = 1;
  LastTurn = succ(ord(LastCategory)-ord(FirstCategory));
  LastTurnPlus = succ(LastTurn);
  NTurnsPerGame = LastTurnPlus - FirstTurn;
  MaxRollsPerTurn = 5; { to experiment with more rolls than officially allowed }
  MaxModificationAttempts = pred(MaxRollsPerTurn);
    NRollsPerTurn = 3; { <= MaxRollsPerTurn }
    NModificationAttempts = pred(NRollsPerTurn);
  FirstRoll = 1;
    LastRoll  = FirstRoll + NModificationAttempts;

type
  TurnIndex = FirstTurn..LastTurn;
  TurnIndex0= 0..LastTurn;
  TurnIndexPlus = FirstTurn..succ(LastTurn);

  RollsLeftCount = 0..MaxRollsPerTurn { within a turn };
  RollIndex = FirstRoll..LastRoll;
  RollIndex0= 0..LastRoll;

  TurnPhase = (ToRoll, ToChoose);
  EventKind = (Rolling, Keeping, Scoring);

  Event = record
    case kind: EventKind of
      Rolling: (evreroll: ValueListRank) { the new values of re-rolled dice;
                                           in roll order, for serial test }
    ; Keeping: (evkeepers: ValueBagRank) { the values of kept dice }
    ; Scoring: (evcat: Category;
                evsc: Score)
  end; { record }

  GameState = record { state between turns, abstraction of ScoreCard,
                       sufficiently detailed for evaluating all future choices }
    free: CategorySet; { Categories remaining to be scored }
    usneed: Score; { needed in upper section for bonus }
    chip: boolean; { chip = (Yahtzee score <> 0); implies Yahtzee not in free }
  end; { record }

  TurnState = record { state within a turn }
    rollsleft: RollsLeftCount;
    case phase: TurnPhase of
      ToRoll: (tskeepers: ValueBagRank;) { to be completed }
    ; ToChoose: (tsroll: ValueBag5Rank;) { to be subbagged }
  end; { record }

  YahtzeeRulesState = record { NOT YET USED }
    name: stdstring;
    NTurnsPerGame: integer; { cf. InitialGameState }
    NRollsPerTurn: integer; { cf. InitialTurnState }
    RelevantAspects: set of Aspect;
    BonusThreshold: 0..MaxBonusThreshold; { 0 if not applicable }
    UpperSectionBonus: Score; { 0 if not applicable, also set BonusThreshold=0 }
    ExtraYahtzeeBonus: Score; { 0 if not applicable }
    JokerRule: boolean;
      { when rolling a Yahtzee AND Yahtzee category is no longer free AND
        the corresponding upper section box has been used,
        then score a Yahtzee according to Joker Rule,
        i.e. obtain full score for a Full House or Sm/Lg Straight }

    InitialGameState: GameState;
    InitialTurnState: TurnState;

    CategoryScores: array [ ValueBag5Rank ] of record
      ya: Category;
        { self[r].ya = common value of r if r is a Yahtzee else Chance }
      sc: array [ Category ] of Score;
        { self[r].sc[c] = score of roll r in category c }
      end; { record }

    MaxAspectScore: array [ Aspect ] of Score;
      { self[a] = (max r:: CategoryScores[r].sc[a]) for a in Category;
        it is specially initialized for other aspects }
  end; { record }

  YahtzeeRules = ^YahtzeeRulesState;

var
  EmptyScoreCard: ScoreCard;
  OfficialYR: YahtzeeRules;

  InitialGameState: GameState;
  InitialTurnState: TurnState;

  CategoryScores: array [ ValueBag5Rank ] of record
    ya: Category;
      { self[r].ya = common value of r if r is a Yahtzee else Chance }
    sc: array [ Category ] of Score;
      { self[r].sc[c] = score of roll r in category c }
    end; { record }

  MaxAspectScore: array [ Aspect ] of Score;
    { self[a] = (max r:: CategoryScores[r].sc[a]) for a in Category;
      it is specially initialized for other aspects }

procedure WriteConfiguration (var f: text);

function  CharFromCategory (c: Category): char;
function  StringFromAspect (a: Aspect): stdstring;
procedure ReadAspect (var f: text; prompt: stdstring; var {out} a: Aspect);

function  StringFromCategorySet (CS: CategorySet): stdstring;
procedure WriteCategorySet (var f: text; s: stdstring; CS: CategorySet;
                            verbose: boolean);
procedure ReadCategorySet (var f: text; prompt: stdstring;
                           var {out} CS: CategorySet);

function  ValueFromCategory (c: Category): DieValue;
function  CategoryFromValue (i: DieValue): Category;

function  CardinalityCategorySet (CS: CategorySet): integer;
function  MinCategorySet (CS: CategorySet): Category;

function  StringFromTurnPhase (ph: TurnPhase; wide: boolean): stdstring;
procedure ReadTurnPhase (var f: text; prompt: stdstring;
                         var {out} ph: TurnPhase);

function  StringFromEventKind (k: EventKind; wide: boolean): stdstring;
procedure ReadEventKind (var f: text; prompt: stdstring;
                         var {out} k: EventKind);

procedure MakeRollingEvent (var {out} ev: Event; r: ValueListRank);
procedure MakeKeepingEvent (var {out} ev: Event; r: ValueBagRank);
procedure MakeScoringEvent (var {out} ev: Event; c: Category);
function  StringFromEvent (ev: Event; complete, wide: boolean): stdstring;
function  StringFromEventRel (ev: Event; vl: ValueList): stdstring;
procedure WriteEvent (var f: text; s: stdstring; ev: Event);
procedure ReadEvent (var f: text; prompt: stdstring; var {out} ev: Event);
function  IsEqualEvent (e1, e2: Event): boolean;

procedure MakeGameState (var {out} GS: GameState; fr: CategorySet; usn: Score;
                         chp: boolean);
procedure MakeInitialGameState (var {out} GS: GameState);
function  IsFinalGameState (GS: GameState): boolean;
function  IsEqualGameState (GS1, GS2: GameState): boolean;
function  NextGameState (var GS: GameState): boolean;
function  StringFromGameState (GS: GameState): stdstring;
procedure WriteGameState (var f: text; s: stdstring; GS: GameState);
procedure ReadGameState (var f: text; prompt: stdstring;
                         var {out} GS: GameState;
                         multiline: boolean);
function  TurnIndexOfGameState (GS: GameState): TurnIndexPlus;

procedure MakeInitialTurnState (var {out} TS: TurnState);
function  IsInitialTurnState (TS: TurnState): boolean;
function  StringFromTurnState (TS: TurnState; complete, wide: boolean):
    stdstring;
procedure WriteTurnState (var f: text; s: stdstring; TS: TurnState);
procedure ReadTurnState (var f: text; prompt: stdstring;
                         var {out} TS: TurnState;
                         force, multiline: boolean);
function  RollIndexOfTurnState (TS: TurnState): RollIndex;
procedure MakeToRollTurnState (var {out} TS: TurnState;
                               rl: RollsLeftCount; keepers: ValueBagRank);
procedure MakeToChooseTurnState (var {out} TS: TurnState;
                                 rl: RollsLeftCount; roll: ValueBag5Rank);

procedure ScoreEvent (YR: YahtzeeRules;
                      GS: GameState; TS: TurnState; var ev: Event;
                      var {out} dSC: ScoreCard; var {out} GSnew: GameState);
procedure MakeEmptyScoreCard (var {out} SC: ScoreCard);
procedure WriteScoreCard (var f: text; {var} const SC: ScoreCard;
                          {var} const YR: YahtzeeRules);
procedure ScoreCardToTStr ({var} str: TStrings; {var} const SC: ScoreCard;
                          {var} const YR: YahtzeeRules);
procedure UpdateScoreCard (var SC: ScoreCard; {var} const dSC: ScoreCard);

procedure CreateYahtzeeRules (var YR: YahtzeeRules);
procedure WriteYahtzeeRules (var f: text; YR: YahtzeeRules);
procedure ReadYahtzeeRules (var f: text; YR: YahtzeeRules);

//procedure InitYahtzeeConcepts;

implementation

uses
  SysUtils, { for Delphi: Format }
  MathStuff;

procedure WriteConfiguration (var f: text);
  const w = 7;
  begin
    writeln(f)
  ; writeln(f, 'CONFIGURATION:')
  ; writeln(f, 'MaxSmallNat        = ', MaxSmallNat:w)
  ; writeln(f, 'MinValue           = ', MinValue:w)
  ; writeln(f, 'MaxValue           = ', MaxValue:w)
  ; writeln(f, 'NDice              = ', NDice:w)
  ; writeln(f, 'MaxRollsPerTurn    = ', MaxRollsPerTurn:w)
  ; writeln(f, 'MaxBonusThreshold  = ', MaxBonusThreshold:w)
  end; { WriteConfiguration }

function  CharFromCategory (c: Category): char;
  begin
    case c of
      Aces:          CharFromCategory := '1'
    ; Twos:          CharFromCategory := '2'
    ; Threes:        CharFromCategory := '3'
    ; Fours:         CharFromCategory := '4'
    ; Fives:         CharFromCategory := '5'
    ; Sixes:         CharFromCategory := '6'
    ; ThreeOfAKind:  CharFromCategory := 'T'
    ; FourOfAKind:   CharFromCategory := 'F'
    ; FullHouse:     CharFromCategory := 'H'
    ; SmallStraight: CharFromCategory := 'S'
    ; LargeStraight: CharFromCategory := 'L'
    ; Yahtzee:       CharFromCategory := 'Y'
    ; Chance:        CharFromCategory := 'C'
    end { case }
  end; { CharFromCategory }

function  StringFromAspect (a: Aspect): stdstring;
  begin
    case a of
      Aces:          StringFromAspect := 'Aces'
    ; Twos:          StringFromAspect := 'Twos'
    ; Threes:        StringFromAspect := 'Threes'
    ; Fours:         StringFromAspect := 'Fours'
    ; Fives:         StringFromAspect := 'Fives'
    ; Sixes:         StringFromAspect := 'Sixes'
    ; ThreeOfAKind:  StringFromAspect := 'Three of a Kind'
    ; FourOfAKind:   StringFromAspect := 'Four of a Kind'
    ; FullHouse:     StringFromAspect := 'Full House'
    ; SmallStraight: StringFromAspect := 'Small Straight'
    ; LargeStraight: StringFromAspect := 'Large Straight'
    ; Yahtzee:       StringFromAspect := 'Yahtzee'
    ; Chance:        StringFromAspect := 'Chance'
    ; USBonus:       StringFromAspect := 'UPPER SECTION BONUS'
    ; EYBonus:       StringFromAspect := 'EXTRA YAHTZEE BONUS'
    ; GrandTotal:    StringFromAspect := 'GRAND TOTAL'
    ; YahtzeesRolled:StringFromAspect := 'Yahtzees Rolled'
    ; JokersApplied: StringFromAspect := 'Jokers Applied'
    ; SpecialAspect: StringFromAspect := 'Special'
    end { case }
  end; { StringFromAspect }

procedure ReadAspect (var f: text; prompt: stdstring; var {out} a: Aspect);
  var ch: char;
  begin
    write(prompt)
  ; ReadChar(f, ch)
  ; case UpChar(ch) of
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
    ; else { skip }
    end { case }
  end; { ReadAspect }

function  StringFromCategorySet (CS: CategorySet): stdstring;
  var c: Category; s: stdstring;
  begin
    s := ''
  ; for c := FirstCategory to LastCategory do
      if c in CS then
        s := s + CharFromCategory(c)
      else
        s := s + '_'
  ; StringFromCategorySet := s
  end; { StringFromCategorSet }

procedure WriteCategorySet (var f: text; s: stdstring; CS: CategorySet;
                            verbose: boolean);
  { end-of-line only appended when verbose }
  var c: Category; sep: stdstring;
  begin
    if verbose then begin
      write(f, s, ' [')
    ; sep := ' '
    ; for c := FirstCategory to LastCategory do
        if c in CS then begin
          write(f, sep, StringFromAspect(c))
        ; sep := ', '
        end { if }
    ; writeln(f, ' ]')
    end { then }
    else
      write(f, s, ' ', StringFromCategorySet(CS))
  end; { WriteCategorySet }

procedure ReadCategorySet (var f: text; prompt: stdstring;
                           var {out} CS: CategorySet);
  var a: Aspect; c: Category;
  begin
    if prompt <> '' then
      write(prompt)
  ; CS := [ ]
  ; while not EndOfField(f) do begin
      ReadAspect(f, '', a)
    ; Assert((FirstCategory<=a) and (a<=LastCategory),
             'ReadCategorySet', 'Aspect out of range')
    ; c := a
    ; CS := CS + [ c ]
    end { while }
  ; ReadEndOfField(f)
  end; { ReadCategorySet }

function  ValueFromCategory (c: Category): DieValue;
  { pre: c in LowerSection
    ret: succ(ord(c~)-ord(FirstCategory) }
  begin
    ValueFromCategory := succ(ord(c)-ord(FirstCategory))
  end; { ValueFromCategory }

function  CategoryFromValue (i: DieValue): Category;
  { pre: true
    ret: c such that ValueFromCategory(c) = i~ }
  begin
    case i of
      1: CategoryFromValue := Aces ;
      2: CategoryFromValue := Twos ;
      3: CategoryFromValue := Threes ;
      4: CategoryFromValue := Fours ;
      5: CategoryFromValue := Fives ;
      6: CategoryFromValue := Sixes ;
    end { case }
  end; { CategoryFromValue }

function  CardinalityCategorySet (CS: CategorySet): integer;
  { pre: true
    ret: # CS~ }
  var
    c: Category;
    card: integer;
  begin
    card := 0
  ; for c := FirstCategory to LastCategory do
      card := card + ord(c in CS)
  ; CardinalityCategorySet := card
  end; { CardinalityCategorySet }

function  MinCategorySet (CS: CategorySet): Category;
  { pre: CS <> [ ];
    ret: min CS~ }
  var c: Category;
  begin
    c := FirstCategory
  ; while not (c in CS) do c := succ(c)
  ; MinCategorySet := c
  end; { MinCategorySet }

procedure InitScoresTables;
  { pre:  true
    post: CategoryScore and MaxAspectScore defined,
          according to current Playing Rules }
  var
    c: Category;
    r: ValueBagRank;
    weight: integer;
    support: integer;
    minnonzeromult: integer;
    maxmult: integer;
    maxinterval: integer;
    s: Score;
  begin
    with ValueBagRanks[NDice] do begin
      for c := FirstCategory to LastCategory do
        MaxAspectScore[c] := 0
      { inv: MaxAspectScore[c] = (max u: u<r: CategoryScores[u].sc[c]) }
    ; for r := firstvbr to lastvbr do begin
        with ValueBagTbl[r], CategoryScores[r] do begin
          { fvb traverses all ValueBags with size=NDice }
          support := ValueBagSupport(fvb)
        ; weight := ValueBagWeight(fvb)
        ; minnonzeromult := ValueBagMinNonZeroMult(fvb)
        ; maxmult := ValueBagMaxMult(fvb)
        ; maxinterval := ValueBagMaxInterval(fvb)
{
;WriteValueBag(errout, 'vb= ', fvb);
;writeln(errout, 'support       = ', support:2)
;writeln(errout, 'weight        = ', weight:2)
;writeln(errout, 'minnonzeromult= ', minnonzeromult:2)
;writeln(errout, 'maxmult       = ', maxmult:2)
;writeln(errout, 'maxinterval   = ', maxinterval:2)
}
        ; if maxmult = NDice { fvb is Yahtzee }
          then ya := CategoryFromValue(ValueBagMinValue(fvb))
          else ya := Chance
        ; for c := FirstCategory to LastCategory do begin
            { determine s = CategoryScores[r].sc[c] }
            case c of
              Aces..Sixes:
                s := ValueFromCategory(c) * fvb[ValueFromCategory(c)]
            ; ThreeOfAKind:
                if maxmult>=3               then s := weight else s := 0
            ; FourOfAKind:
                if maxmult>=4               then s := weight else s := 0
            ; FullHouse:
                if (minnonzeromult>=2) and (support=2)
                                            then s := 25     else s := 0
                { some interpret rules as not requiring support=2 }
            ; SmallStraight:
                if maxinterval>=pred(NDice) then s := 30     else s := 0
            ; LargeStraight:
                if maxinterval=NDice        then s := 40     else s := 0
            ; Yahtzee:
                if maxmult=NDice            then s := 50     else s := 0
            ; Chance:
                s := weight
            end { case }
          ; sc[c] := s
{
;writeln('score         = ', s:2, ' in ', c)
}
          ; if s > MaxAspectScore[c] then
              MaxAspectScore[c] := s
          end { for c }
{
; AwaitEnter('')
}
        end { with }
      end { for r }
    end { with }
  ; MaxAspectScore[USBonus] := UpperSectionBonus
  ; MaxAspectScore[EYBonus] := pred(NTurnsPerGame) * ExtraYahtzeeBonus
  ; MaxAspectScore[GrandTotal] := MaxScore
  ; MaxAspectScore[YahtzeesRolled] := NTurnsPerGame
  ; MaxAspectScore[JokersApplied] := CardinalityCategorySet(JokerSpecials)
  ; MaxAspectScore[SpecialAspect] := MaxScore
  end; { InitScoresTables }

function  StringFromTurnPhase (ph: TurnPhase; wide: boolean): stdstring;
  { results should align in columns }
  begin
    if wide then
      case ph of
        ToRoll:   StringFromTurnPhase := 'Roll into '
      ; ToChoose: StringFromTurnPhase := 'Choose for'
      end { case ph }
    else
      case ph of
        ToRoll:   StringFromTurnPhase := 'R'
      ; ToChoose: StringFromTurnPhase := 'C'
      end { case ph }
  end; { StringFromTurnPhase }

procedure ReadTurnPhase (var f: text; prompt: stdstring;
                         var {out} ph: TurnPhase);
  var ch: char;
  begin
    if prompt <> '' then
      write(prompt)
  ; ReadChar(f, ch)
  ; case UpChar(ch) of
      'R': ph := ToRoll
    ; 'C': ph := ToChoose
    ; else { skip }
    end { case }
  end; { ReadTurnPhase }

function  StringFromEventKind (k: EventKind; wide: boolean): stdstring;
  begin
    if wide then
      case k of
        Rolling: StringFromEventKind := 'Rolling'
      ; Keeping: StringFromEventKind := 'Keeping'
      ; Scoring: StringFromEventKind := 'Scoring'
      end { case }
    else
      case k of
        Rolling: StringFromEventKind := 'R'
      ; Keeping: StringFromEventKind := 'K'
      ; Scoring: StringFromEventKind := 'S'
      end { case }
  end; { StringFromEventKind }

procedure ReadEventKind (var f: text; prompt: stdstring;
                         var {out} k: EventKind);
  var ch: char;
  begin
    write(prompt)
  ; ReadChar(f, ch)
  ; case UpChar(ch) of
      'R': k := Rolling
    ; 'K': k := Keeping
    ; 'S': k := Scoring
    ; else { skip }
    end { case }
  end; { ReadEventKind }

procedure MakeRollingEvent (var {out} ev: Event; r: ValueListRank);
  begin
    with ev do begin
      kind := Rolling
    ; evreroll := r
    end { with ev }
  end; { MakeRollingEvent }

procedure MakeKeepingEvent (var {out} ev: Event; r: ValueBagRank);
  begin
    with ev do begin
      kind := Keeping
    ; evkeepers := r
    end { with ev }
  end; { MakeKeepingEvent }

procedure MakeScoringEvent (var {out} ev: Event; c: Category);
  { post: ev.evsc undefined }
  begin
    with ev do begin
      kind := Scoring
    ; evcat := c
    end { with ev }
  end; { MakeScoringEvent }

function  StringFromEvent (ev: Event; complete, wide: boolean): stdstring;
  var s: stdstring;
  begin
  ; with ev do begin
      if complete then begin
        s := StringFromEventKind(kind, wide)
      ; if wide then s := s + ' '
      end { then }
      else begin
        s := ''
      end { else }
    ; case kind of
        Rolling: begin
            s := s + StringFromValueList(ValueListTbl[evreroll].fvl, wide)
          end { Rolling }
      ; Keeping: begin
            s := s + StringFromValueBag(ValueBagTbl[evkeepers].fvb, wide)
          end { Rolling }
      ; Scoring: begin
            if wide then s := s + '  '
          ; s := s + CharFromCategory(evcat) + StringFromInt(evsc, 4)
          ; if wide then s := s + '   '
          end { Scoring }
      end { case }
    end { with ev }
  ; StringFromEvent := s
  end; { StringFromEvent }

function  StringFromEventRel (ev: Event; vl: ValueList): stdstring;
  { convert evkeepers bags relative to vl }
  begin
    with ev do begin
      case kind of
        Rolling:
          StringFromEventRel :=
            'Rolling ' + StringFromValueList(ValueListTbl[evreroll].fvl, true) +
            ' '
      ; Keeping:
          StringFromEventRel :=
            'Keeping ' + StringFromValueBagRel(ValueBagTbl[evkeepers].fvb, vl)
      ; Scoring:
          StringFromEventRel :=
            'Scoring   ' + CharFromCategory(evcat) + ' ' +
            StringFromInt(evsc, 3) + '   '
      end { case }
    end { with ev }
  end; { StringFromEventRel }

procedure WriteEvent (var f: text; s: stdstring; ev: Event);
  begin
    write(f, s)
  ; with ev do begin
      case kind of
        Rolling: WriteValueList(f, 'rolling ', ValueListTbl[evreroll].fvl)
      ; Keeping: WriteValueBag(f, 'keeping ', ValueBagTbl[evkeepers].fvb)
      ; Scoring: writeln(f, 'scoring ', evsc:3, ' via ',
                            StringFromAspect(evcat))
      end { case }
    end { with ev }
  end; { WriteEvent }

procedure ReadEvent (var f: text; prompt: stdstring; var {out} ev: Event);
  var vl: ValueList; vb: ValueBag; a: Aspect;
  begin
    with ev do begin
      ReadEventKind(f, prompt, kind)
    ; case kind of
        Rolling: begin
            ReadValueList(f, '', vl)
          ; evreroll := RankFromValueList(vl)
          end { Rolling }
      ; Keeping: begin
            ReadValueBag(f, '', vb)
          ; evkeepers := RankFromValueBag(vb)
          end { Keeping }
      ; Scoring: begin
            ReadAspect(f, '', a)
          ; Assert((FirstCategory<=a) and (a<=LastCategory),
                  'ReadEvent', 'Aspect out of range')
          ; evcat := a
          end { Scoring }
      end { case kind }
    end { with }
  end; { ReadEvent }

function  IsEqualEvent (e1, e2: Event): boolean;
  { pre: true; ret: e1~ = e2~ (disregarding scores) }
  begin
    if e1.kind = e2.kind then
      case e1.kind of
        Rolling: IsEqualEvent := e1.evreroll = e2.evreroll
      ; Keeping: IsEqualEvent := e1.evkeepers = e2.evkeepers
      ; Scoring: IsEqualEvent := e1.evcat = e2.evcat
      end { case }
    else
      IsEqualEvent := false
  end; { IsEqualEvent }

procedure MakeGameState (var {out} GS: GameState; fr: CategorySet; usn: Score;
                         chp: boolean);
  begin
    with GS do begin
      free := fr
    ; usneed := usn
    ; chip := chp
    end { with }
  end; { MakeGameState }

procedure MakeInitialGameState (var {out} GS: GameState);
  begin
    MakeGameState(GS, AllCategories * RelevantAspects, BonusThreshold, false)
  end; { MakeInitialGameState }

function  IsFinalGameState (GS: GameState): boolean;
  { pre: true; ret: GS~.free = [ ] }
  begin
    IsFinalGameState := GS.free  = [ ]
  end; { IsFinalGameState }

function  IsEqualGameState (GS1, GS2: GameState): boolean;
  begin
    IsEqualGameState := (GS1.free = GS2.free) and
                        (GS1.usneed = GS2.usneed) and
                        (GS1.chip = GS2.chip)
  end; { IsEqualGameState }

function  NextGameState (var GS: GameState): boolean;
  { pre:  GS = g
    post: if g is last game state then GS = InitialGameState
          else GS = lexicographic successor of g
    ret:  "g is not last game state" }
  { N.B. The order in which all game states are traversed is such that
         if g -r-> g' then g is encountered earlier then g';
         provided that the traversal starts with the initial state }
  var c: Category;
  begin
    NextGameState := true { anticipate not last game state }
  ; with GS do begin
      if not (Yahtzee in free) and not chip then
        chip := true
      else { (Yahtzee in free) or chip } begin
        chip := false
      ; if usneed > 0 then
          usneed := pred(usneed)
        else { usneed = 0 } begin
          usneed := BonusThreshold
        ; if free = [ ] {IsFinalGameState(GS)} then begin
            free := AllCategories
          ; NextGameState := false
          end { then }
          else { free <> [ ] } begin
            { Linear Search for least c in free }
            c := FirstCategory
          ; while not (c in free) do begin
              free := free + [c]
            ; c := succ(c)
            end { while }
            { c in free }
          ; free := free - [c]
          end { else }
        end { else }
      end { else }
    end { with GS }
  end; { NextGameState }

function  StringFromGameState (GS: GameState): stdstring;
  var s: stdstring;
  begin
    with GS do begin
      s := StringFromCategorySet(free) + ';' + StringFromInt(usneed, 2)
    ; if chip then s := s + '+'
      else s := s + '-'
    end { with GS }
  ; StringFromGameState := s
  end; { StringFromGameState }

procedure WriteGameState (var f: text; s: stdstring; GS: GameState);
  begin
    with GS do begin
      WriteCategorySet(f, s+'free= ', free, false)
    ; writeln(f, ', upper section need= ', usneed:1, ', chip= ', chip)
    end { with }
  end; { WriteGameState }

procedure ReadGameState (var f: text; prompt: stdstring;
                         var {out} GS: GameState;
                         multiline: boolean);
  begin
    if prompt <> '' then
      writeln(prompt)
  ; with GS do begin
    ; if prompt <> '' then
        ReadCategorySet(f, '  free= ', free)
      else
        ReadCategorySet(f, '', free)
    ; if multiline then
        readln(f)
    ; if prompt <> '' then
        write('  upper section need= ')
    ; read(f, usneed)
    ; if multiline then
        readln(f)
    ; if prompt <> '' then
        write('  chip= ')
    ; ReadBoolean(f, chip)
    ; if multiline then
        readln(f)
    end { with GS }
  end; { ReadGameState }

function  TurnIndexOfGameState (GS: GameState): TurnIndexPlus;
  { pre: true
    ret: turn index of GS~ }
  begin
    TurnIndexOfGameState := LastTurnPlus - CardinalityCategorySet(GS.free)
  end; { Turn }

procedure MakeInitialTurnState (var {out} TS: TurnState);
  begin
    with TS do begin
      rollsleft := NRollsPerTurn
    ; phase := ToRoll
    ; tskeepers  := EmptyValueBagRank
    end { with TS }
  end; { MakeInitialTurnState }

function  IsInitialTurnState (TS: TurnState): boolean;
  { pre: true; ret: TS~ is initial turn state }
  begin
    IsInitialTurnState := TS.rollsleft = NRollsPerTurn
  end; { IsInitialTurnState }

function  StringFromTurnState (TS: TurnState; complete, wide: boolean):
    stdstring;
  var s: stdstring;
  begin
    with TS do begin
      if complete then
        s := StringFromTurnPhase(phase, wide) + ' '
      else
        s := ''
    ; case phase of
        ToRoll: s := s + StringFromValueBag(ValueBagTbl[tskeepers].fvb, wide)
      ; ToChoose: s := s + StringFromValueBag(ValueBagTbl[tsroll].fvb, wide)
      end { case }
    ; StringFromTurnState  := s + ';' + StringFromInt(rollsleft,1) 
    end { with TS }
  end; { StringFromTurnState }

procedure WriteTurnState (var f: text; s: stdstring; TS: TurnState);
  begin
    with TS do begin
      write(f, s, 'rolls left= ', rollsleft:1,
                  ', phase= ', StringFromTurnPhase(phase, true), ', dice= ')
    ; case phase of
        ToRoll: writeln(f, StringFromValueBag(ValueBagTbl[tskeepers].fvb, true))
      ; ToChoose: writeln(f, StringFromValueBag(ValueBagTbl[tsroll].fvb, true))
      end { case }
    end { with TS }
  end; { WriteTurnState }

procedure ReadTurnState (var f: text; prompt: stdstring;
                         var {out} TS: TurnState;
                         force, multiline: boolean);
  { post: force~ => TS.phase = ToChoose }
  var vb: ValueBag;
  begin
    if prompt <> '' then
      writeln(prompt)
  ; with TS do begin
      if prompt <> '' then
        write('  rolls left= ')
    ; read(f, rollsleft)
    ; if multiline then
        readln(f)
    ; if force then
        phase := ToChoose
      else begin
        if prompt <> '' then
          ReadTurnPhase(f, '  phase= ', phase)
        else
          ReadTurnPhase(f, '', phase)
      ; if multiline then
          readln(f)
      end { else }
    ; case phase of
        ToRoll:
          if (rollsleft = NRollsPerTurn) then
            tskeepers := EmptyValueBagRank
          else begin
            if prompt <> '' then
              ReadValueBag(f, '  keepers= ', vb)
            else
              ReadValueBag(f, '', vb)
          ; tskeepers := RankFromValueBag(vb)
          end { else }
      ; ToChoose: begin
            if prompt <> '' then
              ReadValueBag(f, '  roll= ', vb)
            else
              ReadValueBag(f, '', vb)
          ; tsroll := RankFromValueBag(vb)
          end { ToChoose }
      end { case }
    ; if multiline then
        readln(f)
    end { with TS }
  end; { ReadTurnState }

function  RollIndexOfTurnState (TS: TurnState): RollIndex;
  { pre: true
    ret: roll index of TS~ }
  begin
    with TS do begin
      case phase of
        ToRoll:   RollIndexOfTurnState := succ(NRollsPerTurn-rollsleft)
      ; ToChoose: RollIndexOfTurnState := succ(NModificationAttempts-rollsleft)
      end { case phase }
    end { with TS }
  end; { Turn }

procedure MakeToRollTurnState (var {out} TS: TurnState;
                               rl: RollsLeftCount; keepers: ValueBagRank);
  begin
    with TS do begin
      rollsleft := rl
    ; phase := ToRoll
    ; tskeepers := keepers
    end { with ev }
  end; { MakeToRollTurnState }

procedure MakeToChooseTurnState (var {out} TS: TurnState;
                                 rl: RollsLeftCount; roll: ValueBag5Rank);
  begin
    with TS do begin
      rollsleft := rl
    ; phase := ToChoose
    ; tsroll := roll
    end { with ev }
  end; { MakeToRollTurnState }

//procedure WriteScoreCard (var f: text; {var} const SC: ScoreCard;
//                          {var} const YR: YahtzeeRules); forward;

procedure ScoreEvent (YR: YahtzeeRules;
                      GS: GameState; TS: TurnState; var ev: Event;
                      var {out} dSC: ScoreCard; var {out} GSnew: GameState);
  { pre:  TS~.phase=ToChoose /\ 
          ev~.kind=Scoring /\ ev~.evcat in GS~.free;
    post: ev.kind=ev~.kind /\ ev.evcat=ev~.evcat /\
          ev.evsc = total score
          dSC contains partial scores of ev~ in state GS,TS /\
          GSnew = new state after scoring
  }
  var jka: boolean; { Joker applied }
  begin
    dSC := EmptyScoreCard
  ; GSnew := GS
{
;if not(TS.phase = ToChoose) then Fail('ScoreEvent', 'TS.phase = ToChoose');
if not(ev.kind = Scoring) then Fail('ScoreEvent', 'ev.kind = Scoring');
if not(ev.evcat in GS.free) then Fail('ScoreEvent', 'ev.evcat in GS.free');
if Aces in GS.free then begin
 WriteGameState(errout, 'Current game state: ', GS)
;WriteTurnState(errout, 'Current turn state: ', TS)
;writeln(errout, 'category= ', ev.evcat)
end;
}
  ; with GSnew, TS, CategoryScores[tsroll], ev, dSC do begin
      { ya<>Chance means tsroll is a ya-Yahtzee }
      jka := JokerRule and (ya<>Chance) and 
             not (ya in free) and not (Yahtzee in free)
             and (evcat in JokerSpecials)
    ; if jka then { Joker applied }
        box[evcat] := MaxAspectScore[evcat]
      else { normal scoring }
        box[evcat] := sc[evcat]
    ; free := free - [evcat]
    ; if evcat<=Sixes {evcat in UpperSection} then begin
        if usneed > 0 then begin
        ; if box[evcat] < usneed then { no bonus (yet) }
            usneed := usneed - box[evcat]
          else begin { bonus earned }
            box[USBonus] := UpperSectionBonus
          ; usneed := 0
          end { else }
        end { if }
      end { if }
    ; if chip and (ya<>Chance) then
        box[EYBonus] := ExtraYahtzeeBonus
    ; if evcat = Yahtzee then
        chip := box[evcat] <> 0
    ; box[GrandTotal] := box[evcat] + box[USBonus] + box[EYBonus]
    ; box[YahtzeesRolled] := ord(ya <> Chance)
    ; box[JokersApplied] := ord(jka)
    ; box[SpecialAspect] := box[GrandTotal] + 4*box[EYBonus]
    ; evsc := box[GrandTotal]
{
;if Aces in GS.free then begin
;WriteEvent(errout, '', ev)
;WriteScoreCard(errout, dSC, YR)
;WriteGameState(errout, 'New game state    : ', GSnew);
;AwaitEnter('')
end
}
    end { with }
  end; { ScoreEvent }

procedure MakeEmptyScoreCard (var {out} SC: ScoreCard);
  var a: Aspect;
  begin
    with SC do begin
      for a := FirstAspect to LastAspect do
        {if a in RelevantAspects then}
          box[a] := 0
    end { with SC }
  end; { MakeEmptyScoreCard }

procedure WriteScoreCard (var f: text; {var} const SC: ScoreCard;
                          {var} const YR: YahtzeeRules);
  const w = 5;

  procedure WriteScoreCardLine (a: Aspect);
    begin
      if a in RelevantAspects then
        writeln(f, SC.box[a]:w, ' ', StringFromAspect(a))
    end; { WriteScoreCardLine }

  var a: Aspect; i: integer;

  begin
    writeln(f)
  ; writeln(f, 'Score Card')
  ; writeln(f, '===== ====')
  ; {with YR^ do begin}
      for a := Aces to Sixes do
        WriteScoreCardLine(a)
    ; WriteScoreCardLine(USBonus)
    ; for a := ThreeOfAKind to Chance do
        WriteScoreCardLine(a)
    ; WriteScoreCardLine(EYBonus)
    ; for i := 1 to w do
        write(f, '-')
    ; writeln(f, ' -------------------')
    ; WriteScoreCardLine(GrandTotal)
    ; WriteScoreCardLine(YahtzeesRolled)
    ; WriteScoreCardLine(JokersApplied)
    ; WriteScoreCardLine(SpecialAspect)
{    end { with YR^ }
  ; flush(f)
  end; { WriteScoreCard }

procedure ScoreCardToTStr ({var} str: TStrings; {var} const SC: ScoreCard;
                          {var} const YR: YahtzeeRules);
  const w = 5;

  procedure ScoreCardLine (a: Aspect);
    begin
      if a in RelevantAspects then
        str.Append ( Format ( '%*d %s',
                              [ w, SC.box[a], StringFromAspect(a) ] ) )
    end; { ScoreCardLine }

  var a: Aspect;

  begin
    str.Append('')
  ; str.Append('Score Card')
  ; str.Append('===== ====')
  ; {with YR^ do begin}
      for a := Aces to Sixes do
        ScoreCardLine(a)
    ; ScoreCardLine(USBonus)
    ; for a := ThreeOfAKind to Chance do
        ScoreCardLine(a)
    ; ScoreCardLine(EYBonus)
    ; str.Append(RepStr('-', w) + ' -------------------')
    ; ScoreCardLine(GrandTotal)
    ; ScoreCardLine(YahtzeesRolled)
    ; ScoreCardLine(JokersApplied)
    ; ScoreCardLine(SpecialAspect)
{    end { with YR^ }
  end; { ScoreCardToTStr }

procedure UpdateScoreCard (var SC: ScoreCard; {var} const dSC: ScoreCard);
  { pre:  true
    post: SC has been updated according to dSC~ }
  var a: Aspect;
  begin
    with SC do begin
    ; for a := FirstAspect to LastAspect do
        if a in RelevantAspects then
          box[a] := box[a] + dSC.box[a]
    end { with SC }
  end; { UpdateScoreCard }

procedure CreateYahtzeeRules (var YR: YahtzeeRules);
  begin
    new(YR)
  ; with YR^ do begin
      name := 'Official'
    end { with YR^ }
  end; { CreateYahtzeeRules }

procedure WriteYahtzeeRules (var f: text; YR: YahtzeeRules);
  { pre: YR <> nil }
  const w = 5;
  begin
    if not(YR <> nil) then Fail('WriteYahtzeeRules', 'YR <> nil')
  ; with YR^ do begin
      writeln(f)
    ; writeln(f, name, ' Yahtzee Rules')
end
    ; writeln(f, 'NTurnsPerGame      = ', NTurnsPerGame:w)
    ; writeln(f, 'NRollsPerTurn      = ', NRollsPerTurn:w)
    ; writeln(f, 'BonusThreshold     = ', BonusThreshold:w)
    ; writeln(f, 'UpperSectionBonus  = ', UpperSectionBonus:w)
    ; writeln(f, 'ExtraYahtzeeBonus  = ', ExtraYahtzeeBonus:w)
    ; writeln(f, 'JokerRule          = ', JokerRule:w)
    ; WriteGameState(f, 'InitialGameState: ', InitialGameState)
    ; WriteTurnState(f, 'InitialTurnState: ', InitialTurnState)
{    ; writeln(f, RelevantAspects) }
{    ; writeln(f, MaxAspectScore) }
{    end { with YR^ }
  end; { WriteYahtzeeRules }

procedure ReadYahtzeeRules (var f: text; YR: YahtzeeRules);
  { pre: YR <> nil }
  begin
    if not(YR <> nil) then Fail('WriteYahtzeeRules', 'YR <> nil')
  ; with YR^ do begin
      Fail('ReadYahtzeeRules', '')
    end { with YR^ }
  end; { ReadYahtzeeRules }

begin { initialization }

{$IFDEF TEST}
  writeln ( LogFile, 'Initializing YahtzeeConcepts ...')
; Flush ( LogFile )
{$ENDIF}
; MakeInitialGameState(InitialGameState)
; MakeInitialTurnState(InitialTurnState)
{;AwaitEnter('InitScoresTables')}
; InitScoresTables
; MakeEmptyScoreCard(EmptyScoreCard)
; CreateYahtzeeRules(OfficialYR)
{$IFDEF TEST}
; writeln ( LogFile, 'YahtzeeConcepts initialized')
; Flush ( LogFile )
{$ENDIF}

end. { unit YahtzeeConcepts }
