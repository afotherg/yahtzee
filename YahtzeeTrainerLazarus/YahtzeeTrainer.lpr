program YahtzeeTrainer;
  { main program that starts the GUI application }

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

uses
  Interfaces, Forms, LCLProc,
  Main {MainForm},
  GameRecordings {GameRecForm},
  YahtzeeStrategies,
  GeneralStuff,
  PseudoRandomGenerator,
  YahtzeeConcepts,
  DiceDevils,
//  Advisor {AdvisorForm},
  TableSettings {CreateTableForm},
  Settings;

begin
  {$IFDEF TEST}
  writeln ( LogFile, 'YahtzeeTrainer starting up ...' );
  Flush ( LogFile );
  {$ENDIF}
  Application.Initialize;
  Application.Title := 'Yahtzee Trainer';
  Application.ShowHint := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
