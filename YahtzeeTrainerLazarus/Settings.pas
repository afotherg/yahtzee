unit Settings;
  { Access settings stored in .ini file }

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

procedure setDefaultPath ( path: String );
function getDefaultPath: String;
function getVersion : String;
procedure setVersion( ver: String );
procedure setTableLocation( loc: String );
function getTableLocation: String;

implementation

uses
  SysUtils, IniFiles, GeneralStuff;

const
  FileName = 'YahtzeeTrainerSettings.ini';
  DefaultLocation = 'gstbl';

var
  SettingsFile : TInifile;
  DefaultPath: String;

procedure OpenIniFile;
  begin
    settingsFile := TIniFile.Create(DefaultPath + DirectorySeparator + FileName )
  end;

procedure CloseIniFile;
  begin
    settingsFile.Free
  end;

function getDefaultPath : String;
  begin
    Result := DefaultPath
  end;

procedure setDefaultPath ( path: String );
  begin
    DefaultPath := path
  end;

function getVersion: String;
  begin
    OpenIniFile
  ; Result := settingsFile.ReadString('General', 'Version', '')
  ; CloseIniFile
  end;

procedure setVersion( ver: String );
  begin
    OpenIniFile
  ; settingsFile.WriteString('General', 'Version', ver)
  ; CloseIniFile
  end;

function getTableLocation : String;
  begin
    OpenIniFile
  ; Result := settingsFile.ReadString('Tables', 'Location', '')
  ; if Result = '' then begin
      { N.B. The returned directory must exist! }
      Result := DefaultPath + DirectorySeparator + '..' + DirectorySeparator + DefaultLocation
    ; if not DirectoryExists ( Result ) then begin
        Result := DefaultPath
      end { if }
    end { if }
  ; CloseIniFile
  end;

procedure setTableLocation( loc: String );
  begin
    // note: directory exists because it is selected from a dialog box
    OpenIniFile
  ; settingsFile.WriteString('Tables', 'Location', loc)
  ; CloseIniFile
  end;

end.
