unit Main;
  { The Main form with menus and some buttons }

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
  LResources, Dialogs, StdCtrls, Buttons, Menus, Contnrs,
  YahtzeeConcepts, YahtzeeGameRecordings,
  ScoreCards, DiceSet, GameRecordings, Advisor, TableSettings;

const
  Version = '1.3 beta (Lazarus)';
  Copyright = '(c) 1999-2005, Tom Verhoeff';

type
  TGamePhase = ( gpGameNotStarted, gpGameInProgress, gpGameOver );

  { TMainForm }
  
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenuItem: TMenuItem;
    WindowsMenu: TMenuItem;
    NewGameButton: TButton;
    GameMenu: TMenuItem;
    NewGameMenuItem: TMenuItem;
    HintsMenuItem: TMenuItem;
    DiceMenu: TMenuItem;
    FairMenuItem: TMenuItem;
    DemonicMenuItem: TMenuItem;
    AngelicMenuItem: TMenuItem;
    AdvisorMenu: TMenuItem;
    NewAdvisorMenuItem: TMenuItem;
    NewAdvisorButton: TButton;
    HintsButton: TButton;
    TablesMenu: TMenuItem;
    SettingsMenuItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure NewGameButtonClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure NewGameMenuItemClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure HintsMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FairMenuItemClick(Sender: TObject);
    procedure DemonicMenuItemClick(Sender: TObject);
    procedure AngelicMenuItemClick(Sender: TObject);
    procedure NewAdvisorMenuItemClick(Sender: TObject);
    procedure NewAdvisorButtonClick(Sender: TObject);
    procedure HintsButtonClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);
    procedure UpdateWindowsMenu;
    procedure WindowsMenuItemClick(Sender: TObject);
  private
    { Private declarations }
    GamePhase: TGamePhase;
    GS: GameState;
    TS: TurnState;
    GR: GameRecording;
    CurrentGameRecForm: TGameRecForm;
    GameIndex: Integer;
    OpenGameRecForms: TComponentList;
    AdvisorIndex: Integer;
    procedure SetButtonsAndMenus( Status: Boolean );
    procedure InitProgram;
  public
    { Public declarations }
    procedure OnApplicationIdle ( Sender: TObject );
    procedure SetGamePhase ( newGamePhase: TGamePhase );
    procedure NewGame;
    procedure OpenGameRecForm(new: Boolean);
    procedure ClosingGameRecForm(AGameRecForm: TGameRecForm);
    procedure ReportEvent( ev: Event );
    procedure SetScoreButtonHints;
    procedure SetAllShowHint ( state: Boolean );
    function CountUnsavedGameRecForms: Integer;
    function GetState: String;
    procedure NewAdvisorForm;
  end;

var
  MainForm: TMainForm;
  TableForm : TCreateTableForm;

implementation

uses
  GeneralStuff,
  YahtzeeStrategies, DiceDevils,
  Settings;

procedure TMainForm.FormCreate(Sender: TObject);
var
  iniVersion: String;
begin
  // initialisation
{$IFDEF TEST}
; writeln ( LogFile, 'Initializing Main Form ...' )
; Flush ( LogFile )
; MainForm.Caption := MainForm.Caption + ' [TEST MODE]'
{$ENDIF}
; setDefaultPath ( ExtractFileDir ( Application.ExeName ))
; writeln ( LogFile, Copyright )
; writeln ( LogFile, 'Version = ', Version )
; writeln ( LogFile, 'ExePath = "', getDefaultPath, '"' )
; iniVersion := getVersion
; if iniVersion = '' then begin
    writeln ( LogFile, '.INI file did not exist' )
  ; setVersion ( Version )
  end
  else begin
    writeln ( LogFile, '.INI file version = ', iniVersion )
  end
; WriteConfiguration(LogFile)
; WriteYahtzeeRules(LogFile, OfficialYR)
; Flush ( LogFile )
; Application.AddOnIdleHandler(OnApplicationIdle, True)
; LoadTables;
; if TablesLoaded then begin
    InitProgram
  ; SetButtonsAndMenus( True )
  end
  else begin
    SetButtonsAndMenus( False )
  end
end;

procedure TMainForm.OnApplicationIdle(Sender: TObject);
begin
  UpdateWindowsMenu
end;

procedure TMainForm.InitProgram;
  begin
    ScoreCardForm := TScoreCardForm.Create(Application)
  ; DiceSetForm := TDiceSetForm.Create(Application)
  ; DiceSetForm.SetDD(FairMenuItem);
  ; OpenGameRecForms := TComponentList.Create
  ; GameIndex := 0
  ; AdvisorIndex := 0
  ; SetGamePhase ( gpGameNotStarted )
  end;

procedure TMainform.SetButtonsAndMenus ( Status: Boolean );
  begin
    GameMenu.Enabled := Status
  ; DiceMenu.Enabled := Status
  ; AdvisorMenu.Enabled := Status
  ; newAdvisorButton.Enabled := Status
  ; HintsButton.Enabled := Status
  end;

procedure TMainForm.SetGamePhase ( newGamePhase: TGamePhase );
  begin
    case newGamePhase of
      gpGameNotStarted: begin
        GS := InitialGameState
      ; TS := InitialTurnState
//      ; TS.phase := ToChoose   { test }
//      ; TS.tsroll := 210       { test }
      ; MakeEmptyGameRecording( GR )
      ; DiceSetForm.Init ( Self.ReportEvent )
      ; ScoreCardForm.Init ( Self.ReportEvent )
      ; OpenGameRecForm(True)
      ; with NewGameButton do begin
          Caption := 'New Game'
        ; Enabled := False
        end { with }
      ; with NewGameMenuItem do begin
          Caption := 'New Game'
        ; Enabled := False
        end { with }
      end { gpGameNotStarted }
    ; gpGameInProgress: begin
        with NewGameButton do begin
          Caption := 'Abort Game'
        ; Hint := 'Abort and analyze current game'
        ; Enabled := True
        end { with }
      ; with NewGameMenuItem do begin
          Caption := 'Abort Game'
        ; Hint := 'Abort and analyze current game'
        ; Enabled := True
        end { with }
      ; if CurrentGameRecForm <> nil then
          CurrentGameRecForm.GameStarted
      end { gpGameInProgress }
    ; gpGameOver: begin
        ScoreCardForm.DisableScoreButtons
      ; DiceSetForm.Disable
      ; with NewGameButton do begin
          Caption := 'New Game'
        ; Hint := 'Start a new game'
        ; Enabled := True
        end { with }
      ; with NewGameMenuItem do begin
          Caption := 'New Game'
        ; Hint := 'Start a new game'
        ; Enabled := True
        end { with }
      ; if CurrentGameRecForm = nil then
          OpenGameRecForm(False)
      ; CurrentGameRecForm.ShowGameRecording(GR)
      ; if IsFinalGameState(GS) then
          ShowMessage ( 'Game Over!' )
      end { gpGameOver }
    end { case }
  ; GamePhase := newGamePhase
  end; { SetGamePhase }

procedure TMainForm.NewGame;
  { pre: GamePhase <> gpGameNotStarted }
  var
    Answer: Word; { for result of MessageDlg }
  begin
    case GamePhase of
      gpGameNotStarted: Fail ( 'TMainForm.NewGame',
                               'GamePhase <> gpGameNotStarted' )
    ; gpGameInProgress: begin
        Answer := MessageDlg('Abort and analyze current game?',
                             mtConfirmation, mbOKCancel, 0)
      ; if Answer = mrOK then
          SetGamePhase ( gpGameOver )
      end { gpGameInProgress }
    ; gpGameOver:
        SetGamePhase ( gpGameNotStarted )
    end { case }
  end; { NewGame }

procedure TMainForm.OpenGameRecForm(new: Boolean);
  { pre: ? }
begin
  if new then
    GameIndex := succ ( GameIndex )
; CurrentGameRecForm := TGameRecForm.Create( Application )
; OpenGameRecForms.Add(CurrentGameRecForm)
; with CurrentGameRecForm do begin
    Top := Top + (GameIndex - 1) * 20
  ; Left := Left + (GameIndex - 1) * 20
  ; Caption := Caption + ' ' + IntToStr ( GameIndex )
  ; Init
  ; Show
  end
end; { OpenGameRecForm }

procedure TMainForm.ClosingGameRecForm(AGameRecForm: TGameRecForm);
  begin
    if AGameRecForm = CurrentGameRecForm then
      CurrentGameRecForm := nil
  { entry in OpenGameRecForms should be handled automatically }
  end; { ClosingGameRecForm }

procedure TMainForm.ReportEvent(ev: Event);
  var
    dSC : ScoreCard;
    GSnew: GameState;
  begin
    with ev do begin
      case kind of
        Rolling : begin
                    if GamePhase = gpGameNotStarted then
                      SetGamePhase ( gpGameInProgress )
                  ; DiceSetForm.Roll ( GS, TS )
                  ; SetScoreButtonHints
                  ; ScoreCardForm.EnableScoreButtons
                  end
      ; Keeping : begin
                    ExtendGameRecording( GR, GS,
                                         ScoreCardForm.SC.box[GrandTotal],
                                         TS, ev )
                  ; TurnStateAfterEvent(TS, ev)
                  end
      ; Scoring : begin
                    ScoreCardForm.DisableScoreButtons
                  ; ScoreEvent(OfficialYR, GS, TS, ev, dSC, GSnew)
                  ; ExtendGameRecording( GR, GS,
                                         ScoreCardForm.SC.box[GrandTotal],
                                         TS, ev )
                  ; UpdateScoreCard(ScoreCardForm.SC, dSC)
                  ; ScoreCardForm.UpdateView(GSnew)
                  ; GS := GSnew
                  ; if IsFinalGameState ( GS ) then begin
                      SetGamePhase ( gpGameOver )
                    end
                    else begin
                      TS := InitialTurnState
                    ; DiceSetForm.NewTurn
                    ; ScoreCardForm.EditTurnIndex.Text := IntToStr( 14 - CardinalityCategorySet( GS.free ))
                    end
                  end
      end
    end
  end; { ReportEvent }

function DeltaScoreCardToStr ( const dSC: ScoreCard ): String;
  { ret: short description of dSC }
  var
    a: Aspect;
    op: String;
    n: Integer; { number of nonzeroes }
  begin
    Result := ''
  ; op := ''
  ; n := 0
  ; with dSC do begin

      for a := Aces to EYBonus do begin
        if box [ a ] <> 0 then begin
          Result := Result + op + IntToStr ( box [ a ] )
        ; op := '+'
        ; n := succ(n)
        end { if }
      end { for a }

    ; if n = 0 then
        Result := '0'
      else if n > 1 then
        Result := Result + '=' + IntToStr(box[GrandTotal])
    end { with SC }
  end; { DeltaScoreCardToStr }

procedure TMainForm.SetScoreButtonHints;
  var
    a: Aspect;
    ev: Event;
    GSnew: GameState; { ignored }
    dSC: ScoreCard;
    hintText: String;
  begin

    for a := Aces to Chance do begin
      with ev do begin
        kind := Scoring
      ; evcat := a
      ; evsc := 0
      end { with ev }
    ; ScoreEvent(OfficialYR, GS, TS, ev, dSC, GSnew {ignored})
    ; hintText := 'Score ' + DeltaScoreCardToStr(dSC) + ' now'
    ; if dSC.box[YahtzeesRolled] <> 0 then
        hintText := 'Yahtzee: ' + hintText
    ; if dSC.box[JokersApplied] <> 0 then
        hintText := 'Joker ' + hintText
    ; ScoreCardForm.SetAspectHint ( a, hintText )
    end { for a }

  end; { SetScoreButtonHints }

procedure TMainForm.SetAllShowHint ( state: Boolean );
  begin
    NewGameButton.ShowHint := state
  ; HintsButton.ShowHint := state
  ; NewAdvisorButton.ShowHint := state
  ; ScoreCardForm.SetAllShowHint ( state )
  ; DiceSetForm.SetAllShowHint ( state )
  end; { SetAllShowHint }

function TMainForm.CountUnsavedGameRecForms: Integer;
  var
    i: Integer; { traverses OpenGameRecForms }
  begin
    Result := 0
  ; with OpenGameRecForms do begin
      Pack

    ; for i := 0 to Count - 1 do with Items[i] as TGameRecForm do begin
        Result := Result + ord ( IsUnsaved )
      end { for i, with }

    end { with }
  end; { CountUnsavedGameRecForms }

function  TMainForm.GetState: String;
  begin
    Result := Format( '%s%s  %s;%d',
                    [ StringFromGameState(GS),
                      ScoreCardForm.EditGrandTotal.Text,
                      DiceSetForm.GetState,
                      TS.rollsleft ]
                  )
  end; { GetState }

procedure TMainForm.NewAdvisorForm;
  var
    af: TAdvisorForm;
  begin
    AdvisorIndex := succ ( AdvisorIndex )
  ; af := TAdvisorForm.Create(Application)
  ; with af do begin
      Top := Top + (AdvisorIndex - 1) * 20
    ; Left := Left + (AdvisorIndex - 1) * 20
    ; Caption := Caption + ' ' + IntToStr ( AdvisorIndex )
    ; Init(GetState)
    ; Show
    end
  end; { NewAdvisorForm }

procedure TMainForm.NewGameButtonClick(Sender: TObject);
  begin
    NewGame
  end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close
end;

procedure TMainForm.NewGameMenuItemClick(Sender: TObject);
begin
  NewGame
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  ShowMessage ( 'Version ' + Version + #13#10 +
                'www.win.tue.nl/~wstomv/misc/yahtzee/'#13#10 +
                'yahtzee@win.tue.nl'#13#10 +
                'Copyright ' + Copyright + #13#10 +
                'With cooperation from Rob van Esch' )
end;

{$IFDEF TEST}
procedure WriteApplicationForms; { used for testing only }
var
  i: Integer;
begin
  writeln('Application Forms:')
; for i := 0 to Screen.FormCount - 1 do begin
    writeln('  ', Screen.Forms[i].Caption)
  end { for i }
end;
{$ENDIF}

procedure TMainForm.HintsMenuItemClick(Sender: TObject);
begin
  SetAllShowHint ( HintsMenuItem.Checked )
; if HintsMenuItem.Checked then
    HintsButton.Caption := 'Disable Hints'
  else
    HintsButton.Caption := 'Enable Hints'
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  msg: String; { for MessageDlg }
  aux: String;
  c: Integer; { result of CountUnsavedGameRecForms }
  Answer: Word; { for result of MessageDlg }
begin
  if TablesLoaded then begin
    msg := ''
  ; aux := 'Discard '
  ; if GamePhase = gpGameInProgress then begin
      msg := aux + 'game in progress'
    ; aux := ' and '
    end { if }
  ; c := CountUnsavedGameRecForms
  ; if c > 0 then begin
      msg := msg + aux + IntToStr(c) + ' unsaved game recording(s)'
    end { if }
  ; CloseAction := caFree { anticipate closure }
  ; if msg <> '' then begin
      { ask for confirmation }
      Answer := MessageDlg(msg + ', and exit?',
                           mtConfirmation, mbOKCancel, 0)
    ; if Answer <> mrOK then
        CloseAction := caNone
    end { if }
  end
end;

procedure TMainForm.FairMenuItemClick(Sender: TObject);
begin
  DiceSetForm.SetDD(Sender)
end;

procedure TMainForm.DemonicMenuItemClick(Sender: TObject);
begin
  DiceSetForm.SetDD(Sender)
end;

procedure TMainForm.AngelicMenuItemClick(Sender: TObject);
begin
  DiceSetForm.SetDD(Sender)
end;

procedure TMainForm.NewAdvisorMenuItemClick(Sender: TObject);
begin
  NewAdvisorForm
end;

procedure TMainForm.NewAdvisorButtonClick(Sender: TObject);
begin
  NewAdvisorForm
end;

procedure TMainForm.HintsButtonClick(Sender: TObject);
begin
  HintsMenuItem.Click
end;

procedure TMainForm.WindowsMenuItemClick(Sender: TObject);
var
  i: Integer; { to traverse screen forms }
begin
  for i := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[i].Caption = (Sender as TMenuItem).Caption then begin
      Screen.Forms[i].BringToFront
    ; Exit
    end { if }
  end { for i }
end;

procedure TMainForm.SettingsMenuItemClick(Sender: TObject);
var
  Answer: Word; { for result of MessageDlg }
  ChangeSettings: Boolean; // if a game is in progress ask wheter to stop or to continu
  StartGame : Boolean; // save old settings for tables, TablesLoaded = False => Program not initialised, no forms created
begin
  StartGame := TablesLoaded
; ChangeSettings := True

; if StartGame then
    if ( GamePhase = gpGameInProgress ) then begin
      Answer := MessageDlg('Changing tables settings will abort current game. Do you want to continue?',
                                   mtConfirmation, mbOKCancel, 0)
    ; if Answer = mrOK then
        SetGamePhase ( gpGameOver )
      else
        ChangeSettings := False
    end { if }

; if ChangeSettings then begin
    TableForm := TCreateTableForm.Create(self)
  ; TableForm.ShowModal
  ; TableForm.Free
  ; if ( not StartGame ) and TablesLoaded then begin
      // program was not initialized because tables weren't found
      InitProgram
    ; SetButtonsAndMenus(True)
    end  { if }
    else if TablesLoaded then
      if GamePhase <> gpGameNotStarted then
        newGame
  end { if }

  { After changing settings it will be possible that tables first found are no
    longer available. This is no problem, because the first loaded information
    is not erased. After program restart the program tolds the user the tables
    were not found. }
end;

procedure TMainForm.UpdateWindowsMenu;
var
  WindowsList: TList; { windows to include in Windows menu }
  i: Integer; { to traverse Screen.CustomForms and WindowsList }
  CurMenuItem: TMenuItem; { menu item being updated }
  VForm: TForm; { form in Screen.Forms }
begin
  WindowsList := TList.Create
  { construct list of windows for menu, starting with Score Cards and Dice Set }
; if (ScoreCardForm <> nil) and ScoreCardForm.Visible then
    WindowsList.Add(ScoreCardForm)
; if (DiceSetForm <> nil) and DiceSetForm.Visible then
    WindowsList.Add(DiceSetForm)

; for i := 0 to Screen.FormCount - 1 do begin
    VForm := Screen.Forms[i]
  ; if (VForm <> MainForm) and VForm.Visible and
       (WindowsList.IndexOf(VForm) < 0) then begin
      WindowsList.Add(VForm)
    end { if }
  end { for i }

  { update/create menu items }
; for i := 0 to WindowsList.Count - 1 do begin
    if i < WindowsMenu.Count then begin
      CurMenuItem := WindowsMenu.Items[i]
    end { then }
    else begin
      CurMenuItem := TMenuItem.Create(WindowsMenu)
    ; WindowsMenu.Add(CurMenuItem)
    ; CurMenuItem.OnClick := WindowsMenuItemClick
    end { else }
  ; CurMenuItem.Caption := TCustomForm(WindowsList[i]).Caption
  end { for i }

  { remove unused menu items }
; while WindowsMenu.Count > WindowsList.Count do
    WindowsMenu.Items[WindowsMenu.Count - 1].Free
    
  { clean up }
; WindowsList.Free
end; { UpdateWindowsMenu }

initialization
  {$I Main.lrs}
  
end.

