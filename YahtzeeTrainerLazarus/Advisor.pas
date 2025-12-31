unit Advisor;
  { Optimal Solitaire Yahtzee Advisor forms }

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
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  YahtzeeConcepts, SynMemo;

type

  { TAdvisorForm }

  TAdvisorForm = class(TForm)
    AnalyzeButton: TButton;
    StateEdit: TEdit;
    txtAdvice: TSynMemo;
    UpdateButton: TButton;
    procedure Init(AState: String);
    procedure AnalyzeButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
  private
    { Private declarations }
    GS: GameState;
    Total: Score;
    TS: TurnState;
  public
    { Public declarations }
    procedure AnalyzeState;
  end;

var
  AdvisorForm: TAdvisorForm;

implementation

uses
  LResources,
  GeneralStuff, DiceStuff, YahtzeeStrategies, YahtzeeEventLists,
  Main, YahtzeeExtras;

procedure TAdvisorForm.Init(AState: String);
  begin
    StateEdit.Text := AState
  end; { Init }

procedure TAdvisorForm.AnalyzeState;
  var
    s: stdstring;
    i: Integer;
    errmsg: stdstring;
    n: Integer; { for output of IntegerFromString }
    EL: EList;
    vl: ValueList;
  begin
    txtAdvice.Lines.Clear
{    GS := InitialGameState {test}
  ; s := Trim(StateEdit.Text)
  ; i := 1
  ; errmsg := ''
  ; GameStateFromString(GS, s, i, errmsg)
{  ; Total := 0 {test}
  ; IntegerFromString(n, 0, MaxScore, 'Total score so far', s, i, errmsg)
  ; if errmsg = '' then
      Total := n
{  ; with TS do begin
      rollsleft := 2
    ; phase := ToChoose
    ; tsroll := MaxValueBagRank
    end { with } {test}
  ; TurnStateFromString(TS, vl, s, i, errmsg)
  ; if (errmsg = '') and (i <= Length(s)) then
      errmsg := 'Extra characters at end'
  ; if errmsg <> '' then begin
      with txtAdvice.Lines do begin
        Append(Format('Error in Abstract Yahtzee State at position %d', [i]))
      ; Append(s)
      ; Append(Format('%*s', [i, '^']))
      ; Append(errmsg)
      end { with }
    ; Exit
    end { if }

  ; case TS.phase of
      ToChoose: MakeChoiceEList(EL, OptimalYS, GS, TS, Total)
    ; ToRoll: MakeRollEList(EL, OptimalYS, GS, TS, Total)
    end { case }
  ; SortEList(EL)
  ; EListToTStr ( txtAdvice.Lines, EL, vl, False)
  end; { AnalyzeState }

procedure TAdvisorForm.AnalyzeButtonClick(Sender: TObject);
begin
  AnalyzeState
end;

procedure TAdvisorForm.UpdateButtonClick(Sender: TObject);
begin
  Init(MainForm.GetState)
; AnalyzeState
end;

initialization
  {$I Advisor.lrs}

end.
