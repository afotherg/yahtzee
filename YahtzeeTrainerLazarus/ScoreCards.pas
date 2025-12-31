unit ScoreCards;
  { Score Card form }

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
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  YahtzeeConcepts, YahtzeeExtras;

type
  TScoreCardForm = class(TForm)
    LabelUSTotal: TLabel;
    LabelEYBonus: TLabel;
    LabelGrandTotal: TLabel;
    PanelUpperSection: TPanel;
    LabelAces: TLabel;
    LabelTwos: TLabel;
    LabelThrees: TLabel;
    LabelFours: TLabel;
    LabelFives: TLabel;
    LabelSixes: TLabel;
    LabelUSBonus: TLabel;
    PanelLowerSection: TPanel;
    LabelThreeOfAKind: TLabel;
    LabelChance: TLabel;
    LabelYahtzee: TLabel;
    LabelLargeStraight: TLabel;
    LabelSmallStraight: TLabel;
    LabelFullHouse: TLabel;
    LabelFourOfAKind: TLabel;
    EditAces: TEdit;
    EditTwos: TEdit;
    EditThrees: TEdit;
    EditFours: TEdit;
    EditFives: TEdit;
    EditSixes: TEdit;
    ButAces: TButton;
    ButTwos: TButton;
    ButThrees: TButton;
    ButFours: TButton;
    ButFives: TButton;
    ButSixes: TButton;
    EditThreeOfAKind: TEdit;
    ButThreeOfAKind: TButton;
    ButFourOfAkind: TButton;
    EditFourOfAKind: TEdit;
    EditFullHouse: TEdit;
    ButFullHouse: TButton;
    ButSmallStraight: TButton;
    EditSmallStraight: TEdit;
    EditLargeStraight: TEdit;
    ButLargeStraight: TButton;
    EditYahtzee: TEdit;
    EditChance: TEdit;
    ButYahtzee: TButton;
    ButChance: TButton;
    EditUSTotal: TEdit;
    EditUSBonus: TEdit;
    EditGrandTotal: TEdit;
    EditEYBonus: TEdit;
    EditTurnIndex: TEdit;
    LabelTurnIndex: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ScoreButtonClick(Sender: TObject);

  private
    { Private declarations }
    ScoreCardEntries: array [ Aspect ] of record
      LabelScore: TLabel;
      EditScore: TEdit;
      ButScore: TButton;
    end;
    ReportEvent: ReportEventMethod;
  public
    { Public declarations }
    SC: Scorecard;
    procedure Init(REM: ReportEventMethod);
    procedure UpdateView( const GS: GameState );
    function CategoryFromSender( Sender: TObject ): Category;
    procedure DisableScoreButtons ();
    procedure EnableScoreButtons ();
    procedure SetAspectHint ( a: Aspect; const h: String );
    procedure SetAllShowHint ( state: Boolean );
  end;

var
  ScoreCardForm: TScoreCardForm;

implementation

uses
  LResources;

function ScoreToStr ( sc: Score ): String;
  begin
    Result := Format ( '%*d', [ MaxScoreLength, sc ] )
  end; { ScoreToStr }

procedure TScoreCardForm.FormCreate(Sender: TObject);
  begin
    ScoreCardEntries[ Aces ].EditScore := EditAces
  ; ScoreCardEntries[ Twos ].EditScore := EditTwos
  ; ScoreCardEntries[ Threes ].EditScore := EditThrees
  ; ScoreCardEntries[ Fours ].EditScore := EditFours
  ; ScoreCardEntries[ Fives ].EditScore := EditFives
  ; ScoreCardEntries[ Sixes ].EditScore := EditSixes
  ; ScoreCardEntries[ ThreeOfAKind ].EditScore := EditThreeOfAKind
  ; ScoreCardEntries[ FourOfAKind ].EditScore := EditFourOfAKind
  ; ScoreCardEntries[ FullHouse ].EditScore := EditFullHouse
  ; ScoreCardEntries[ SmallStraight ].EditScore := EditSmallStraight
  ; ScoreCardEntries[ LargeStraight ].EditScore := EditLargeStraight
  ; ScoreCardEntries[ Yahtzee ].EditScore := EditYahtzee
  ; ScoreCardEntries[ Chance ].EditScore := EditChance
  ; ScoreCardEntries[ USBonus ].EditScore := EditUSBonus
  ; ScoreCardEntries[ EYBonus ].EditScore := EditEYBonus
  ; ScoreCardEntries[ GrandTotal ].EditScore := EditGrandTotal

  ; ScoreCardEntries[ Aces ].ButScore := ButAces
  ; ScoreCardEntries[ Twos ].ButScore := ButTwos
  ; ScoreCardEntries[ Threes ].ButScore := ButThrees
  ; ScoreCardEntries[ Fours ].ButScore := ButFours
  ; ScoreCardEntries[ Fives ].ButScore := ButFives
  ; ScoreCardEntries[ Sixes ].ButScore := ButSixes
  ; ScoreCardEntries[ ThreeOfAKind ].ButScore := ButThreeOfAKind
  ; ScoreCardEntries[ FourOfAKind ].ButScore := ButFourOfAKind
  ; ScoreCardEntries[ FullHouse ].ButScore := ButFullHouse
  ; ScoreCardEntries[ SmallStraight ].ButScore := ButSmallStraight
  ; ScoreCardEntries[ LargeStraight ].ButScore := ButLargeStraight
  ; ScoreCardEntries[ Yahtzee ].ButScore := ButYahtzee
  ; ScoreCardEntries[ Chance ].ButScore := ButChance
  ; ScoreCardEntries[ USBonus ].ButScore := nil
  ; ScoreCardEntries[ EYBonus ].ButScore := nil
  ; ScoreCardEntries[ GrandTotal ].ButScore := nil

  ; ScoreCardEntries[ Aces ].LabelScore := LabelAces
  ; ScoreCardEntries[ Twos ].LabelScore := LabelTwos
  ; ScoreCardEntries[ Threes ].LabelScore := LabelThrees
  ; ScoreCardEntries[ Fours ].LabelScore := LabelFours
  ; ScoreCardEntries[ Fives ].LabelScore := LabelFives
  ; ScoreCardEntries[ Sixes ].LabelScore := LabelSixes
  ; ScoreCardEntries[ ThreeOfAKind ].LabelScore := LabelThreeOfAKind
  ; ScoreCardEntries[ FourOfAKind ].LabelScore := LabelFourOfAKind
  ; ScoreCardEntries[ FullHouse ].LabelScore := LabelFullHouse
  ; ScoreCardEntries[ SmallStraight ].LabelScore := LabelSmallStraight
  ; ScoreCardEntries[ LargeStraight ].LabelScore := LabelLargeStraight
  ; ScoreCardEntries[ Yahtzee ].LabelScore := LabelYahtzee
  ; ScoreCardEntries[ Chance ].LabelScore := LabelChance
  ; ScoreCardEntries[ USBonus ].LabelScore := LabelUSBonus
  ; ScoreCardEntries[ EYBonus ].LabelScore := LabelEYBonus
  ; ScoreCardEntries[ GrandTotal ].LabelScore := LabelGrandTotal
  end;

procedure TScoreCardForm.ScoreButtonClick(Sender: TObject);
  var
    ev : Event;
  begin
    with ev do begin
      kind := Scoring
    ; evcat := CategoryFromSender( Sender )
    ; evsc := 0
    end
  ; ReportEvent( ev )
  end;

procedure TScoreCardForm.Init (REM: ReportEventMethod);
  var
    a : Aspect;
  begin
  ; ReportEvent := REM
  ; for a:= Aces to GrandTotal do begin
      ScoreCardEntries[a].EditScore.Text := ''
    end { for a }
  ; DisableScoreButtons
  ; SC := EmptyScoreCard
  ; UpdateView ( InitialGameState )
  ; EditTurnIndex.Text := '1'
  end; { Init }

procedure TScoreCardForm.UpdateView ( const GS: GameState );
  var
    a: Aspect;
    USTotal : Score;
  begin
    USTotal := 0
  ; for a := Aces to GrandTotal do begin
      if a <= Sixes then USTotal := USTotal + SC.box[a]
    ; with ScoreCardEntries[a] do begin
        if (a <= LastCategory) and (a in GS.free) then
          EditScore.Text := ''
        else
          EditScore.Text := ScoreToStr( SC.box[a] )
      end
    end
  ; EditUSTotal.Text := ScoreToStr( USTotal )
  end;

function TScorecardForm.CategoryFromSender( Sender: TObject ): Category;
  begin
    if Sender = ButAces then result := Aces
    else if Sender = ButTwos then result := Twos
    else if Sender = ButThrees then result := Threes
    else if Sender = ButFours then result := Fours
    else if Sender = ButFives then result := Fives
    else if Sender = ButSixes then result := Sixes
    else if Sender = ButThreeOfAKind then result := ThreeOfAKind
    else if Sender = ButFourOfAKind then result := FourOfAKind
    else if Sender = ButFullHouse then result := FullHouse
    else if Sender = ButSmallStraight then result := SmallStraight
    else if Sender = ButLargeStraight then result := LargeStraight
    else if Sender = ButYahtzee then result := Yahtzee
    else if Sender = ButChance then result := Chance
    else begin
      messagedlg( 'CategoryFromSender: Inappropriate Sender', mtInformation, [mbOk], 0 )
    ; result := Yahtzee
    end
  end;

procedure TScoreCardForm.DisableScoreButtons ();
  var
    a: Aspect;
  begin

    for a := Aces to GrandTotal do begin
      with ScoreCardEntries[a] do begin
        if ButScore <> nil then
          ButScore.Enabled := False
      end { with }
    end { for a }

  end; { DisableScoreButtons }

procedure TScoreCardForm.EnableScoreButtons ();
  var
    a: Aspect;
  begin

    for a := Aces to GrandTotal do begin
      with ScoreCardEntries[a] do begin
        if (ButScore <> nil) and (EditScore.Text = '') then
          ButScore.Enabled := True
      end { with }
    end { for a }

  end; { EnableScoreButtons }

procedure TScoreCardForm.SetAspectHint ( a: Aspect; const h: String );
  begin
    with ScoreCardEntries [ a ] do begin
      if ButScore <> nil then
        ButScore.Hint := h
    end { with }
  end; { SetAspectHint }

procedure TScoreCardForm.SetAllShowHint ( state: Boolean );
  var
    a: Aspect;
    ev: Event;
  begin
    EditTurnIndex.ShowHint := state

  ; for a := Aces to GrandTotal do begin
      with ScoreCardEntries [ a ] do begin
        LabelScore.ShowHint := state
      ; EditScore.ShowHint := state
      ; if ButScore <> nil then
          ButScore.ShowHint := state
      end { with }
    end { for a }

  end;

initialization
  {$I ScoreCards.lrs}

end.




