unit TableSettings;
  { Table Settings form }

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
  Dialogs, StdCtrls, ComCtrls, Buttons;

type
  TCreateTableForm = class(TForm)
    OKButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    textLocation: TEdit;
    ChangeLocationButton: TButton;
    GroupBox1: TGroupBox;
    ExpectedCreate: TButton;
    ExpectedInfo: TButton;
    GroupBox2: TGroupBox;
    VarianceCreate: TButton;
    VarianceInfo: TButton;
    OptExpectedText: TLabel;
    VarianceText: TLabel;
    PBOptE: TProgressBar;
    PBV: TProgressBar;
    BothCreate: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeLocationButtonClick(Sender: TObject);
    procedure ExpectedCreateClick(Sender: TObject);
    procedure VarianceCreateClick(Sender: TObject);
    procedure ExpectedInfoClick(Sender: TObject);
    procedure VarianceInfoClick(Sender: TObject);
    procedure BothCreateClick(Sender: TObject);
  private
    { Private declarations }
    procedure setTableInformationOnForm;
  public
    { Public declarations }
  end;

var
  CreateTableForm: TCreateTableForm;

function TablesLoaded: Boolean;
procedure LoadTables;

implementation

uses
  LResources, FileCtrl,
  GeneralStuff, YahtzeeConcepts, YahtzeeStrategies, YahtzeeStateTables,
  YahtzeeStrategyAnalysis, YahtzeeEventLists, YahtzeeStateAnalyzer,
  settings;

const
  TableOptExpected = 'OptEScore-Official.gstbl';
  TableVariance = 'OptEScore-Official-V.gstbl';

var
  Loaded: Boolean;   // table optexpected loaded ?

function TableExists ( name: String ): Boolean;
  begin
    Result := FileExists(getTableLocation + DirectorySeparator + name)
  end;

function ConfirmOverwrite ( name: String ): Boolean;
  var
    fname: String;
    Answer: Word; { for result of MessageDlg }
  begin
    fname := getTableLocation + DirectorySeparator + name
  ; if FileExists ( fname ) then begin
      Answer := MessageDlg( 'Overwrite existing table "'+fname+'"?',
                             mtConfirmation, mbOKCancel, 0)
    ; Result := ( Answer = mrOK )
    end
    else begin
      Result := True
    end
  end;

procedure LoadTables;
  var
    fname: String;
    ETableFound: Boolean;
  begin
    with OptimalYS^ do begin
      fname := getTableLocation + DirectorySeparator + TableOptExpected
    ; ETableFound := TableExists( TableOptExpected )
    ; if ETableFound then begin
        InitCache(OptECache, 'Optimal Expected Final Score', GrandTotal, fname)
      ; Loaded := True
      end
      else begin
        ShowMessage('Could not find ' + fname + '. Use Settings in the Table menu to configure your tables.' )
      end
    ; ECache := OptECache
    ; fname := getTableLocation + DirectorySeparator + TableVariance
    ; if TableExists( TableVariance ) then begin
        InitCache(VCache, 'Variance in Final Score', GrandTotal, fname)
      end
      else begin
        if ETableFound then ShowMessage('Could not find ' + fname + '. Standard deviations suppressed. Use Settings in the Table menu to configure your tables.' )
      end
    end { with }
  end;

function TablesLoaded: Boolean;
  begin
    Result := Loaded
  end;

{ the following procedure is needed to set Loaded from a class method }
procedure SetLoaded ( b: Boolean );
  begin
    Loaded := b
  end;

procedure TCreateTableForm.setTableInformationOnForm;
  begin
    textLocation.Text := getTableLocation
  ; optExpectedText.Width := 200;
  ; VarianceText.Width := 200;
  ; if TableExists( TableOptExpected ) then begin
      ExpectedInfo.Enabled := True
    ; OptExpectedText.Caption := 'Table found.'#13#10'Click Info for information about this table.'#13#10'Click Create to recreate the table.';
    end
    else begin
      ExpectedInfo.Enabled := False
    ; OptExpectedText.Caption := 'Table not found.'#13#10'This table is required, either click Create to create this table or change the location.';
    end
  ; if TableExists( TableVariance ) then begin
      VarianceInfo.Enabled := True
    ; if TableExists ( TableOptExpected ) then begin
        VarianceCreate.Enabled := True
      ; VarianceText.Caption := 'Table found.'#13#10'Click Info for information about this table.'#13#10'Click Create to recreate the table.';
      end
      else begin
        VarianceText.Caption := 'Table found.'#13#10'You must first create the Optimal Expected Score Table before you can use the Yahtzee Trainer.'
      ; VarianceCreate.Enabled := False
      end
    end
    else begin
      VarianceInfo.Enabled := False
    ; if TableExists( TableOptExpected ) then begin
        VarianceText.Caption := 'Table not found.'#13#10'This table is optional. If you want to use it click Create to create the table.'
      ; VarianceCreate.Enabled := True
      end
      else begin
        VarianceText.Caption := 'Table not found.'#13#10'You must first create the Optimal Expected Score Table before you can compute the Variance.'
      ; VarianceCreate.Enabled := False
      end
    end
  end;

procedure TCreateTableForm.FormCreate(Sender: TObject);
  begin
    setTableInformationOnForm
  end;

procedure TCreateTableForm.OKButtonClick(Sender: TObject);
  begin
    Close
  end;

procedure TCreateTableForm.ChangeLocationButtonClick(Sender: TObject);
  var
    TableDir: String; { for windows }
   { TableDir : WideString } { For linux }
  begin
 //   SelectDirectory('Select Table Location', 'Desktop', TableDir) // function for all platforms
    TableDir := getTableLocation
  ; if not DirectoryExists ( TableDir ) then begin
      TableDir := getDefaultPath + DirectorySeparator + '..'
    end { if }
  ; if SelectDirectory( TableDir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0 ) then begin
      textLocation.Text := TableDir
    ; setTableLocation( TableDir )
    ; writeln ( LogFile, 'Table location set to "', TableDir, '"' )
    ; Flush ( LogFile )
    ; LoadTables
    ; setTableInformationOnForm
    end
  end;

procedure TCreateTableForm.ExpectedCreateClick(Sender: TObject);
  var
    t0, t1:  TDateTime;
  begin
    if not ConfirmOverwrite ( TableOptExpected ) then begin
      Exit
    end { if }
  ; Screen.Cursor := crHourGlass
  ; ExpectedCreate.Visible := False
  ; ExpectedInfo.Visible := False
  ; OptExpectedText.Caption := 'Creating table. Please wait...'
  ; PBOptE.Visible := True
  ; Refresh
  ; Writeln(LogFile, 'Creating Optimal Expected Final Score Table' )
  ; with OptimalYS^ do begin
      InitCache(OptECache, 'Optimal Expected Final Score', GrandTotal, '')
    ; with OptECache^ do begin
      ; writeln(LogFile, '# Known entries in OptECache^.gstbl = ')
      ; writeln(LogFile, '  ', NKnownGameStateTable(gstbl):1)
      ; writeln(LogFile, 'Computing all choices for optimal Yahtzee strategy ...')
      ; Flush ( LogFile )
      ; t0 := Now
{$IFDEF TEST}
      ; sleep(1000)
{$ELSE}
      ; AllOptExpectedScores(OptimalYS)
{$ENDIF}
      ; t1 := Now
      ; if t1 > t0 then begin
          writeln(LogFile, 'Wall clock time = ', FormatDateTime('h:nn:ss', t1-t0))
        end { if }
      ; writeln(LogFile, '# Known entries in OptECache^.gstbl = ')
      ; writeln(LogFile, '  ', NKnownGameStateTable(gstbl):1)
      ; writeln(LogFile, cName, ' of ', shortname,' for ', StringFromAspect(target), ' = ')
{$IFNDEF TEST}
      ; writeln(LogFile,'  ', OptExpectedScore(InitialGameState):1:5)
{$ENDIF}
      ; SaveGameStateTable( getTableLocation + DirectorySeparator + TableOptExpected, gstbl)
      end { with OptECache^ }
    ; ECache := OptECache { expectation needed for variance computation }
    end { with OptimalYS^ }
  ; SetLoaded ( True )
  ; PBOptE.Visible := False
//  ; LoadTables
  ; setTableInformationOnForm
  ; ExpectedCreate.Visible := True
  ; ExpectedInfo.Visible := True
  ; Screen.Cursor := crDefault
  end; { TCreateTableForm.ExpectedCreateClick }

procedure TCreateTableForm.ExpectedInfoClick(Sender: TObject);
  var
    infoString: String;
  begin
    with OptimalYS^ do
      with OptECache^ do
          infoString := 'Information about Optimal Expected Score Table'#13#10#13#10'# Known entries in table = '
                + IntToStr(NKnownGameStateTable(gstbl)) + #13#10
                + cName + ' of ' + shortname + ' for ' + StringFromAspect(target) + ' = '
{$IFNDEF TEST}
                + FloatToStr(OptExpectedScore(InitialGameState))
{$ENDIF}
  ; ShowMessage( infoString )
  end; { TCreateTableForm.ExpectedInfoClick }

procedure TCreateTableForm.VarianceCreateClick(Sender: TObject);
  var
    t0, t1: TDateTime;
    v: real; { variance }
  begin
    if not ConfirmOverwrite ( TableVariance ) then begin
      Exit
    end { if }
  ; VarianceCreate.Visible := False
  ; VarianceInfo.Visible := False
  ; VarianceText.Caption := 'Creating table. Please wait...'
  ; PBV.Visible := True
  ; Screen.Cursor := crHourGlass
  ; Refresh
  ; Writeln(LogFile, 'Creating Variance-in-Final-Score Table' )
  ; with OptimalYS^ do begin
    ; InitCache(VCache, 'Variance in Final Score', GrandTotal, '')
    ; with VCache^ do begin
      ; writeln(LogFile, '# Known entries in VCache^.gstbl = ')
      ; writeln(LogFile, '  ', NKnownGameStateTable(gstbl):1)
      ; writeln(LogFile, 'Computing all variances for optimal choices ...')
      ; Flush ( LogFile )
      ; t0 := Now
{$IFDEF TEST}
      ; sleep(61000)
{$ELSE}
      ; AllVarianceScores(OptimalYS)
{$ENDIF}
      ; t1 := Now
      ; if t1 > t0 then begin
          writeln(LogFile, 'Wall clock time = ', FormatDateTime('h:nn:ss', t1-t0))
        end { if }
      ; writeln(LogFile, '# Known entries in VCache^.gstbl = ')
      ; writeln(LogFile, '  ', NKnownGameStateTable(gstbl):1)
      ; writeln(LogFile, cName, ' of ', shortname, ' for ', StringFromAspect(target), ' = ')
{$IFDEF TEST}
      ; v := 0.0
{$ELSE}
      ; v := VarianceScore(OptimalYS, InitialGameState, InitialTurnState)
{$ENDIF}
      ; writeln(LogFile, v:1:5, ' = ', sqrt(v):1:5, '^2')
      ; SaveGameStateTable(getTableLocation + DirectorySeparator + TableVariance, gstbl)
      end { with VCache^ }
    end { with OptimalYS^ }
  ; PBV.Visible := False
//  ; LoadTables
  ; setTableInformationOnForm
  ; VarianceCreate.Visible := True
  ; VarianceInfo.Visible := True
  ; Screen.Cursor := crDefault
  end; { TCreateTableForm.VarianceCreateClick }

procedure TCreateTableForm.VarianceInfoClick(Sender: TObject);
  var
    infoString: String;
  begin
    with OptimalYS^ do
      with VCache^ do
        infoString := 'Information about Variance-in-Score Table'#13#10#13#10'# Known entries in table = '
              + IntToStr(NKnownGameStateTable(gstbl)) + #13#10
              + cName + ' of ' + shortname + ' for ' + StringFromAspect(target) + ' = '
{$IFNDEF TEST}
              + FloatToStr(sqrt(VarianceScore(OptimalYS, InitialGameState, InitialTurnState)))+ '^2'
{$ENDIF}
  ; ShowMessage( infoString )
  end; { TCreateTableForm.VarianceInfoClick }

procedure TCreateTableForm.BothCreateClick(Sender: TObject);
begin
  ExpectedCreateClick(Sender)
; VarianceCreateClick(Sender)
end;

initialization
  {$I TableSettings.lrs}
  Loaded := False
end.
