unit JcfUnicode;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfUnicode, released March 2007.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2007 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

type
  AnsiCharSet = set of AnsiChar;

function WideCharIsReturn(const C: WideChar): Boolean;
function WideCharIsDigit(const wc: WideChar): Boolean;
function WideCharIsAlpha(const wc: WideChar): Boolean;
function WideCharIsAlphaNum(const wc: WideChar): Boolean;

function WideCharIsHexDigitDot(const wc: WideChar): Boolean;

function WideCharIsPuncChar(const wc: WideChar): boolean;
function WideCharIsWordChar(const wc: WideChar): Boolean;
function WideCharIsWhiteSpaceNoReturn(const wc: WideChar): boolean;

function WideCharInSet(const wc: WideChar; const charSet: AnsiCharSet): Boolean;

function WideStringRepeat(const ws: WideString; const count: integer): WideString;

const
  WideNullChar = WideChar(#0);

  WideLineFeed = WideChar(#10);
  WideCarriageReturn = WideChar(#13);

  WideSpace = WideChar(#32);

  {$IFDEF MSWINDOWS}

    {$IFDEF DELPHI7}
      WideLineBreak = WideString(WideCarriageReturn) + WideString(WideLineFeed);
    {$ELSE}
      WideLineBreak = WideCarriageReturn + WideLineFeed;
    {$ENDIF DELPHI7}

  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  WideLineBreak = WideLineFeed;
  {$ENDIF UNIX}

implementation

uses
  { Delphi }
  Classes, SysUtils,
  { local }
  JcfStringUtils;

const
  MaxAnsiChar = 127;


// true when the char is not in the ansi char set
function WideCharIsHigh(const wc: WideChar): Boolean;
var
  index: integer;
begin
  index := integer(wc);
  Result := (Index > MaxAnsiChar);
end;

function WideCharIsReturn(const C: WideChar): Boolean;
begin
  Result := (C = WideLineFeed) or (C = WideCarriageReturn);
end;

function WideCharIsDigit(const wc: WideChar): Boolean;
var
  ch: char;
begin

  if WideCharIsHigh(wc) then
  begin
    Result := False;
    exit;
  end;

  ch := char(wc);
  Result := CharIsDigit(ch);
end;

function WideCharIsAlpha(const wc: WideChar): Boolean;
var
  ch: char;
begin
  if WideCharIsHigh(wc) then
  begin
    Result := False;
    exit;
  end;

  ch := char(wc);
  Result := CharIsAlpha(ch);
end;

function WideCharIsAlphaNum(const wc: WideChar): Boolean;
var
  ch: char;
begin
  if WideCharIsHigh(wc) then
  begin
    Result := False;
    exit;
  end;

  ch := char(wc);
  Result := CharIsAlpha(ch) or CharIsDigit(ch);
end;

function WideCharIsHexDigitDot(const wc: WideChar): Boolean;
const
  HexDigits: set of AnsiChar = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F',
    'a', 'b', 'c', 'd', 'e', 'f'];
var
  ch: AnsiChar;
begin
  if WideCharIsHigh(wc) then
  begin
    Result := False;
    exit;
  end;

  ch := AnsiChar(wc);
  Result := (ch in HexDigits) or (ch = '.');
end;


function WideCharIsWordChar(const wc: WideChar): Boolean;
var
  ch: char;
begin
  if WideCharIsHigh(wc) then
  begin
    Result := False;
    exit;
  end;

  ch := char(wc);
  Result := CharIsAlpha(ch) or (ch = '_');
end;

function WideCharIsPuncChar(const wc: WideChar): boolean;
var
  ch: char;
begin
  Result := False;

  if WideCharIsHigh(wc) then
  begin
    exit;
  end;

  ch := char(wc);

  if CharIsWhiteSpace(ch) then
    exit;
  if CharIsAlphaNum(ch) then
    exit;
  if CharIsReturn(ch) then
    exit;

  if CharIsControl(ch) then
    exit;

  Result := True;
end;

function WideCharIsWhiteSpaceNoReturn(const wc: WideChar): boolean;
var
  ch: char;
begin
  Result := False;

  if WideCharIsHigh(wc) then
  begin
    exit;
  end;

  // null chars
  if wc = WideNullChar then
    exit;


  if WideCharIsReturn(wc) then
    exit;

  ch := char(wc);

  { 7 April 2004 following sf snag 928460 and discussion in newsgroups
    must accept all other chars < 32 as white space }

  // Result := CharIsWhiteSpace(ch) and (ch <> AnsiLineFeed) and (ch <> AnsiCarriageReturn);

  Result := (ord(ch) <= Ord(NativeSpace));
end;

{
 Returnh true if the widechar is in the ansi char set
}
function WideCharInSet(const wc: WideChar; const charSet: AnsiCharSet): Boolean;
var
  ch: AnsiChar;
begin
  Result := False;

  if WideCharIsHigh(wc) then
  begin
    exit;
  end;

  // null chars
  if wc = WideNullChar then
    exit;

  ch := AnsiChar(wc);

  Result := ch in charSet;
end;

function WideStringRepeat(const ws: WideString; const count: integer): WideString;
var
  liLoop: integer;
begin
  Result := '';
  for liLoop := 0 to count - 1 do
  begin
    Result := Result + ws;
  end;
    

end;

end.
