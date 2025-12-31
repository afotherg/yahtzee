unit GameRecordings;
  { Game Recording forms }

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
  Dialogs, StdCtrls, ComCtrls, Buttons,
  YahtzeeGameRecordings, SynMemo;

type

  { TGameRecForm }

  TGameRecForm = class(TForm)
    SaveButton: TButton;
    SaveDialog1: TSaveDialog;
    txtGameRec: TSynMemo;
    procedure SaveButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Unsaved: Boolean;
  public
    { Public declarations }
    procedure Init;
    procedure GameStarted;
    procedure ShowGameRecording(const GR: GameRecording);
    procedure UpdateCaption(const filename: String);
    function IsUnsaved: Boolean;
  end;

var
  GameRecForm: TGameRecForm;
  LastSaveDir: String; { directory of last save }

implementation

uses
  LResources,
  YahtzeeConcepts, YahtzeeStrategies, Main;

procedure TGameRecForm.Init;
  begin
    Unsaved := False
  end; { Init }

procedure TGameRecForm.GameStarted;
  begin
    with txtGameRec.Lines do begin
      Clear
    ; Append ( 'Game in progress.' )
    ; Append ( '' )
    ; Append ( 'Game analysis will be shown here at end.' )
    end { with }
  end; { GameStarted }

procedure TGameRecForm.ShowGameRecording(const GR: GameRecording);
  begin
    txtGameRec.Lines.Clear
  ; GameRecordingToTStr(txtGameRec.Lines, GR, 'You', OfficialYR, true, OptimalYS)
  ; Unsaved := True
  ; SaveButton.Enabled := True
  end; { ShowGameRecording }

procedure TGameRecForm.UpdateCaption(const filename: String);
  var
    c: String; { copy of Caption to transform }
    p: Integer; { result of Pos }
  begin
    c := Caption
  ; p := Pos ( '[', c )
  ; if p <> 0 then begin
      Delete(c, p, Length(Caption)) { deletes everything from '[' }
    end { if }
  ; Caption := Trim(c) + ' [' + filename + ']'
  end; { UpdateCaption }

function TGameRecForm.IsUnsaved: Boolean;
  begin
    Result := Unsaved
  end; { IsUnsaved }

procedure TGameRecForm.SaveButtonClick(Sender: TObject);
  begin
    with SaveDialog1 do begin
      Title := 'Save Game Recording in Text File: '
    ; InitialDir := LastSaveDir
    ; if Execute then begin
        txtGameRec.Lines.SaveToFile(FileName)
      ; Unsaved := False
      ; LastSaveDir := ExtractFileDir(FileName)
      ; UpdateCaption ( ExtractFileName ( FileName ) )
      end { if }
    end { with }
  end;

procedure TGameRecForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  Answer: Word;  { for result of MessageDlg }
begin
  Action := caNone { anticipate that window will not be closed }
; if Unsaved then begin
    Answer := MessageDlg('Discard unsaved ' + Caption + '?',
                         mtConfirmation, mbOKCancel, 0)
  ; if Answer <> mrOK then
      Exit
  end { if }
; MainForm.ClosingGameRecForm(Self)
; Action := caFree { proceed with closure }
end;

initialization
  {$I GameRecordings.lrs}
  LastSaveDir := ExtractFileDir(Application.ExeName)
end.

