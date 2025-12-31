unit DiceSet;
  { Dice Set form }

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
  DiceStuff, YahtzeeConcepts, DiceDevils, YahtzeeExtras;

type

  { TDiceSetForm }

  TDiceSetForm = class(TForm)
    EditDice1: TEdit;
    EditDice2: TEdit;
    EditDice3: TEdit;
    EditDice4: TEdit;
    EditDice5: TEdit;
    ButRoll: TButton;
    CheckDice1: TCheckBox;
    CheckDice2: TCheckBox;
    CheckDice3: TCheckBox;
    CheckDice4: TCheckBox;
    CheckDice5: TCheckBox;
    HeldLabelDice1: TLabel;
    HeldLabelDice2: TLabel;
    HeldLabelDice3: TLabel;
    HeldLabelDice4: TLabel;
    HeldLabelDice5: TLabel;
    lblInfo: TLabel;
    EditRollIndex: TEdit;
    procedure ButRollClick(Sender: TObject);
    procedure CheckDice1Change(Sender: TObject);
    procedure CheckDice2Change(Sender: TObject);
    procedure CheckDice3Change(Sender: TObject);
    procedure CheckDice4Change(Sender: TObject);
    procedure CheckDice5Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    DiceSetEntries : Array [ DiceIndex ] of record
      EditDice: TEdit;
      CheckDice: TCheckBox;
      HeldLabelDice: TLabel;
    end;
    ReportEvent : ReportEventMethod;
    function RankFromKeepers: ValueListRank;
  public
    { Public declarations }
    DD: DiceDevil;
    FirstRoll : Boolean;
    procedure Init (REM: ReportEventMethod);
    procedure SetDD ( Sender: TObject );
    function  GetState: String;
    procedure UpdateView( ev: Event );
    procedure ClearDice;
    procedure NewTurn;
    procedure SetCheckStateEnabled ( State: Boolean );
    procedure SetAllShowHint ( state: Boolean );
    procedure Disable;
    procedure Roll ( const GS: GameState; var TS: TurnState );
  end;

var
  DiceSetForm: TDiceSetForm;

implementation

uses
  LResources, Menus;

procedure TDiceSetForm.FormCreate(Sender: TObject);
  var
    i: DiceIndex;
  begin
    DiceSetEntries[ 1 ].EditDice := EditDice1
  ; DiceSetEntries[ 1 ].CheckDice := CheckDice1
  ; DiceSetEntries[ 1 ].HeldLabelDice := HeldLabelDice1
  ; DiceSetEntries[ 2 ].EditDice := EditDice2
  ; DiceSetEntries[ 2 ].CheckDice := CheckDice2
  ; DiceSetEntries[ 2 ].HeldLabelDice := HeldLabelDice2
  ; DiceSetEntries[ 3 ].EditDice := EditDice3
  ; DiceSetEntries[ 3 ].CheckDice := CheckDice3
  ; DiceSetEntries[ 3 ].HeldLabelDice := HeldLabelDice3
  ; DiceSetEntries[ 4 ].EditDice := EditDice4
  ; DiceSetEntries[ 4 ].CheckDice := CheckDice4
  ; DiceSetEntries[ 4 ].HeldLabelDice := HeldLabelDice4
  ; DiceSetEntries[ 5 ].EditDice := EditDice5
  ; DiceSetEntries[ 5 ].CheckDice := CheckDice5
  ; DiceSetEntries[ 5 ].HeldLabelDice := HeldLabelDice5

    { The following initialization should be in the .lfm file,
      but currently Lazarus does not support that. }
  ; for i := 1 to NDice do begin
      with DiceSetEntries[ i ].EditDice.Font do begin
        CharSet := 1;
        Color := clWindowText;
        Height := -19;
        Name := 'default';
        Pitch := fpDefault;
        Size := 18;
        Style := [ fsBold ];
      end { with }
    end { for i }
    
  end;

procedure TDiceSetForm.Init (REM: ReportEventMethod);
  begin
    ReportEvent := REM
  ; ClearDice
  ; NewTurn
  end;

procedure TDiceSetForm.SetDD ( Sender: TObject );
  { called from Dice menu item click }
  var
    c: String; { cleaned copy of Sender.Caption }
    p: Integer; { for result of Pos }
  begin
    with Sender as TMenuItem do begin
      Checked := True
    ; case DevilType(Tag) of
        BuiltInList: DD := BuiltInListDD
      ; Demonic: DD := DemonicDD
      ; Angelic: DD := AngelicDD
      end { case }
    ; c := Caption
    end { with }
  ; p := Pos('&', c)
  ; if p > 0 then
      Delete(c, p, 1)
  ; Caption := 'Dice Set [' + c + ']'
  ; initDiceDevil(DD)
  end; { SetDD }

function  TDiceSetForm.GetState: String;
  var
    i: DiceIndex;
  begin
    Result := ''

  ; for i := 1 to NDice do begin
      Result := Result + Trim(DiceSetEntries[i].EditDice.Text)
    end { for i }

  end; { GetState }

procedure TDiceSetForm.UpdateView( ev: Event );
  var
    i : DiceIndex; { loops DiceSetEntries }
    j : DiceIndex; { loops evreroll }
  begin
    with ValueListTbl[ev.evreroll].fvl do begin
      { Assert ( siz = #unckecked dice ) }
      j := 1
      
    ; for i := 1 to NDice do begin
        if not DiceSetEntries[i].CheckDice.Checked then begin
          DiceSetEntries[i].EditDice.Text := Format('  %1d', [ val[j] ] )
        ; if j < NDice then j := j + 1
        end
      end { for i }
      
    end
  end;

procedure TDiceSetForm.ClearDice;
  var
    i: DiceIndex;
  begin

    for i := 1 to NDice do begin
      DiceSetEntries[i].EditDice.Text := ''
    end { for i }

  end; { ClearDice }

procedure TDiceSetForm.NewTurn;
  var
    i : DiceIndex;
  begin
  
    for i := 1 to NDice do begin
      with DiceSetEntries[i] do begin
        CheckDice.Checked := False
      ; HeldLabelDice.Visible := False
      end
    end { for i }
    
  ; SetCheckStateEnabled ( False )
  ; EditRollIndex.Text := ''
  ; FirstRoll := True
  ; ButRoll.Enabled := True
  end; { NewTurn }

function TDiceSetForm.RankFromKeepers: ValueListRank;
  var
    vb: ValueBag; { keepers }
    i : DiceIndex;
    v : DieValue;
  begin
    MakeEmptyValueBag ( vb )
    
  ; for i := 1 to NDice do begin
      with DiceSetEntries[i] do begin
        if CheckDice.Checked then begin
          v := StrToInt( EditDice.Text )
        ; ExtendValueBag ( vb, v, 1 )
        end
      end
    end { for i }
    
  ; Result := RankFromValueBag ( vb )
  end; { RankFromKeepers }

procedure TDiceSetForm.ButRollClick(Sender: TObject);
  var
    ev: Event;
  begin
    if not FirstRoll then begin
      with ev do begin
        Kind := Keeping
      ; evkeepers := RankFromKeepers
      end
    ; ReportEvent( ev )
    end
  ; ev.kind := Rolling
  ; ReportEvent ( ev )
  ; FirstRoll := False
  end;

procedure TDiceSetForm.CheckDice1Change(Sender: TObject);
begin
  HeldLabelDice1.Visible := CheckDice1.Checked
end;

procedure TDiceSetForm.CheckDice2Change(Sender: TObject);
begin
  HeldLabelDice2.Visible := CheckDice2.Checked
end;

procedure TDiceSetForm.CheckDice3Change(Sender: TObject);
begin
  HeldLabelDice3.Visible := CheckDice3.Checked
end;

procedure TDiceSetForm.CheckDice4Change(Sender: TObject);
begin
  HeldLabelDice4.Visible := CheckDice4.Checked
end;

procedure TDiceSetForm.CheckDice5Change(Sender: TObject);
begin
  HeldLabelDice5.Visible := CheckDice5.Checked
end;

procedure TDiceSetForm.SetCheckStateEnabled ( State : Boolean );
  var
    i : DiceIndex;
  begin
  
    for i := 1 to NDice do begin
      DiceSetEntries[i].CheckDice.Enabled := State
    end { for i }
    
  end; { SetCheckStateEnabled }

procedure TDiceSetForm.SetAllShowHint ( state: Boolean );
  var
    i: DiceIndex;
  begin
    EditRollIndex.ShowHint := state
  ; ButRoll.ShowHint := state

  ; for i := 1 to NDice do begin
      with DiceSetEntries[i] do begin
        EditDice.ShowHint := state
      ; CheckDice.ShowHint := state
      end { with }
    end { for i }
    
  end; { SetAllShowHint }

procedure TDiceSetForm.Disable;
  begin
    ButRoll.Enabled := False
  ; SetCheckStateEnabled ( False )
  end; { Disable }

procedure TDiceSetForm.Roll ( const GS: GameState; var TS: TurnState );
  var
    ev: Event;
  begin
    TickleDiceDevil( DD, GS, TS, ev )
  ; UpdateView ( ev )
  ; TurnStateAfterEvent( TS, ev )
  ; EditRollIndex.Text := IntToStr( 3 - TS.rollsleft )
  ; if TS.rollsleft = 0 then begin
      Disable
    end { then }
    else begin
      SetCheckStateEnabled ( True )
    end { else }
  end; { Roll }

initialization
  {$I DiceSet.lrs}
  
end.

