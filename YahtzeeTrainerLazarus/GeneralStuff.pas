unit      GeneralStuff;
  { Various general purpose routines }

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

const
  maxlongint = maxint; { for Sun Pascal } { for gpc }

type
{ longint = integer32; { for Sun Pascal }
{ stdstring = varying [80] of char; { for Sun Pascal }
{ stdstring = string (80); { for gpc }
  stdstring = string; { for Delphi }

const
//  DirectorySeparator = '\'; { Windows }
  FieldDelimiters = [ ':', ';' ];
  FieldPadding    = [ ' ', ',', '_' ];
  CutLine         = '--8<--';

type
  CharSet = set of char;

var
  LogFile: Text;

procedure Fail (place: stdstring; s: stdstring);
procedure Assert (b: boolean; place: stdstring; s: stdstring);
procedure AwaitEnter (s: stdstring);
function  UpChar (ch: char): char;
function  UpString (s: stdstring): stdstring;
function  PaddedString (s: stdstring; w: integer): stdstring;
function  RepStr (const s: stdstring; n: integer): stdstring;
function  EndOfField(var f: text): boolean;
procedure ReadEndOfField (var f: text);
procedure ReadChar (var f: text; var ch: char);
procedure ReadBoolean (var f: text; var b: boolean);
function  DigitFromChar (ch: char): integer;
function  CharFromDigit (d: integer): char;
function  StringFromInt (i: longint; width: integer): stdstring;
function  wallclock: integer; { for gpc }
function  clock: integer;
procedure WriteTime (var f: text; s: stdstring; t: longint);
procedure WriteCurrentDateTime (var f: Text; s: stdstring);

implementation

{ uses gpc; { for gpc }
uses
  SysUtils, DateUtils, Forms, Dialogs; { for Delphi }

var
  LookAheadBufferValid: boolean;
  LookAheadBuffer: char; { for file input only }

procedure Fail (place: stdstring; s: stdstring);
  begin
    //writeln({stderr,} place, ': ', s, ' FAILED')
  ; ShowMessage( place + ': ' + s + ' FAILED' )
  ; halt
{ ; assert(false) { this is Sun Pascal for dump trace & halt }
  end; { Fail }

procedure Assert (b: boolean; place: stdstring; s: stdstring);
  begin
    if not b then Fail(place, s)
  end; { Assert }

procedure AwaitEnter (s: stdstring);
  begin
    writeln({stderr,} s, ' [Type Enter to Continue] ')
    { this is Sun Pascal for write to errout(=stderr) and flush }
  ; readln
  end; { AwaitEnter }

function  UpChar (ch: char): char;
  begin
    if ch in ['a'..'z'] then
      UpChar := chr( ord(ch) - ord('a') + ord('A') )
    else
      UpChar := ch
  end; { UpChar }

function  UpString (s: stdstring): stdstring;
  var t: stdstring; i: integer;
  begin
    t := ''
  ; for i := 1 to length(s) do
      t := t + UpChar(s[i])
  ; UpString := t
  end; { UpString }

function  PaddedString (s: stdstring; w: integer): stdstring;
  { pre: 0 <= width
    ret: s padded with minimum number of spaces on the right to make length 
         at least w }
  begin
  ; while length(s) < w do
      s := s + ' '
  ; PaddedString := s
  end; { PaddedString }

function  RepStr (const s: stdstring; n: integer): stdstring;
  { pre: 0 <= n
    ret: s * n }
  var
    r: stdstring;
  begin
    r := ''

  ; while n <> 0 do begin
      r := r + s
    ; n := pred ( n )
    end { while }

  ; RepStr := r
  end; { RepStr }

function LookAheadChar: char; { for Delphi }
  begin
    if not LookAheadBufferValid then begin
      read(LookAheadBuffer)
    ; LookAheadBufferValid := true
    end { if }
  ; LookAheadChar := LookAheadBuffer
  end; { LookAheadChar }

function  EndOfField(var f: text): boolean;
  begin
    while not eoln(f) and ({f^}LookAheadChar in FieldPadding) do
      {get(f)} LookAheadBufferValid := false
  ; EndOfField := eoln(f) or ({f^}LookAheadChar in FieldDelimiters)
  end; { EndOfField }

procedure ReadEndOfField (var f: text);
  { pre: f = input }
  begin
    if {f^} LookAheadChar in FieldDelimiters then
      {get(f)} LookAheadBufferValid := false
  end; { ReadEndOfField }

procedure ReadChar (var f: text; var ch: char);
  { pre: f = input }
  { read first nonignored char }
  begin
    repeat
      {read(f, ch)} ch := LookAheadChar
    ; LookAheadBufferValid := false
    until not (ch in FieldPadding)
  end; { ReadChar }

procedure ReadBoolean (var f: text; var b: boolean);
  var ch: char;
  begin
    repeat
      read(f, ch)
    until ch <> ' '
  ; ch := UpChar(ch)
  ; if not (ch in ['F','T']) then
      Fail('ReadBoolean', 'ch in [''F'',''T'']')
    else begin
      b := (ch = 'T')
    ; while not eoln(f) and (ch <> ' ') do
        read(f, ch)
    end { else }
  end; { ReadBoolean }

function  DigitFromChar (ch: char): integer;
  { pre: '0' <= ch <= '9' }
  begin
    DigitFromChar := ord(ch) - ord('0')
  end; { DigitFromChar }

function  CharFromDigit (d: integer): char;
  { pre: 0 <= d < 10 }
  begin
    CharFromDigit := chr(ord('0') + d)
  end; { CharFromDigit }

function  StringFromInt (i: longint; width: integer): stdstring;
  var s, t: stdstring;
  begin
  ; if i = 0 then
      s := '0'
    else if i > 0 then
      s := ''
    else { i < 0 } begin
      s := '-'
    ; i := -i
    end { else }
  ; t := ''
  ; while i <> 0 do begin
    ; t := CharFromDigit(i mod 10) + t
    ; i := i div 10
    end { while }
  ; s := s + t
  ; while length(s) < width do
      s := ' ' + s
  ; StringFromInt := s
  end; { StringFromInt }

function  wallclock: integer; { for gpc }
  { get current time in seconds since Jan. 1, 1970 }
  begin
    wallclock := SecondOfTheYear(Now) {GetUnixTime(null){gpc}
  end; { wallclock }

function  clock: longint;
  { get user time in milliseconds }
  var t: TTimeStamp;
  begin
    t := DateTimeToTimeStamp(Now)
  ; clock := t.Time { ignore Date }
  end; { clock }

procedure WriteTime (var f: text; s: stdstring; t: longint);
  { pre:  t is time in milliseconds }
  { post: t written in seconds, minutes, hours and days }
  var h: real;
  begin
    if s <> '' then
      write(f, s)
{ ; h := t*(3/5000) { Sun Pascal; why factor 3/5000?
                      according to documentation it should be t/1000 }
  ; h := t/1000 { gpc }
  ; write(f, h:1:3, 's = ')
  ; h := h/60
  ; write(f, h:1:3, 'm = ')
  ; h := h/60
  ; write(f, h:1:3, 'h = ')
  ; h := h/24
  ; write(f, h:1:3, 'd')
  ; writeln(f)
  ; flush(f)
  end; { WriteTime }

procedure WriteCurrentDateTime (var f: Text; s: stdstring);
{  var }
{    d, t: stdstring; }
{    ts: TimeStamp; { gpc }
  begin
{    date(d) ; time(t) { Sun Pascal; should, of course, be an atomic pair! }
{    GetTimeStamp(ts) { gpc }
{  ; d := date(ts) ; t := time(ts) { gpc }
{  ; writeln(s, ' on ', d, ' at ', t) }
  ; writeln(f, s, ' on ', DateTimeToStr(Now))
  end; { WriteCurrentDateTime }

initialization
  AssignFile ( LogFile, ChangeFileExt ( Application.ExeName, '.log' ) )
; Rewrite ( LogFile )
; WriteCurrentDateTime ( LogFile, 'Run Started' )
{$IFDEF TEST}
; writeln ( LogFile, 'TEST MODE ACTIVE' )
{$ENDIF}
; Flush ( LogFile )
; LookAheadBufferValid := false

finalization
  WriteCurrentDateTime ( LogFile, 'Run Stopped' )
; CloseFile ( LogFile )
end. { unit GeneralStuff }
