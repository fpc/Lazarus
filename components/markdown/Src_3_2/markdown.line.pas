{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown single markdown line

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit MarkDown.Line;

{$mode ObjFPC}{$H+}

interface

uses Markdown.Utils;

type
  { TMarkDownLine }

  TMarkDownLine = class
  private
    FLine : AnsiString;
    FLineNo : integer;
    FCursor : integer;
    FMark : integer;
  public
    constructor create(aLine : AnsiString; aLineNo : integer);
    procedure Reset;
    procedure Mark;
    procedure Rewind;
    procedure Advance(len : integer); // advance x number of spaces or characters
    // Skip whitespace from the current cursor position. Returns first non-whitespace character
    Function SkipWhiteSpace : Char;
    // Is the cursor at end of line ?
    function IsEmpty : boolean;
    // Remainder of the line, starting at cursor
    function Remainder : AnsiString;
    // Returns count of whitespaces from cursor. Tab acts as 4 position tab.
    function LeadingWhitespace : integer; inline;
    // Returns count of whitespaces from cursor. Tab acts as 4 position tab.
    // returns first non-whitespace character
    function LeadingWhitespace(out aFirstNonWhitespaceChar : Char) : integer;
    function isWhitespace : boolean; // if everything after cursor is whitespace
    // Line is the text with initial tabs replaced by spaces.
    property Line : AnsiString Read FLine;
    property LineNo : integer Read FLineNo;
    Property CursorPos : Integer Read FCursor;
  end;
  TMarkDownLineList = class (specialize TGFPObjectList<TMarkDownLine>);

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}  

{ TMarkDownLine }

constructor TMarkDownLine.create(aLine: AnsiString; aLineNo: integer);

begin
  inherited create;
  FLine:=TransformTabs(aline);
  FLineNo:=aLineNo;
  Reset;
end;


procedure TMarkDownLine.Reset;

begin
  FCursor:=1;
end;


procedure TMarkDownLine.Rewind;
begin
  FCursor:=FMark;
end;


procedure TMarkDownLine.Advance(len: integer);
begin
  inc(FCursor,len);
end;


function TMarkDownLine.isEmpty: boolean;

begin
  Result:=FCursor>Length(FLine);
end;


function TMarkDownLine.isWhitespace: boolean;

var
  i,lLen: integer;

begin
  Result:=true;
  lLen:=Length(FLine);
  i:=FCursor;
  While Result and (i<=lLen) do
    begin
    Result:=isWhitespaceChar(Fline[i]);
    inc(I);
    end;
end;

procedure TMarkDownLine.Mark;
begin
  FMark:=FCursor;
end;


function TMarkDownLine.LeadingWhitespace : integer;
var
  lDummy : char;
begin
  Result:=LeadingWhitespace(lDummy);
end;

function TMarkDownLine.LeadingWhitespace(out aFirstNonWhitespaceChar: Char) : integer;

var
  lIndex,lLen : integer;

begin
  Result:=0;
  lIndex:=FCursor;
  lLen:=Length(FLine);
  while (lIndex<=lLen) and isWhitespaceChar(FLine[lindex]) do
    begin
    inc(Result,1);
    inc(lIndex);
    end;
  if (lIndex<=lLen) then
    aFirstNonWhitespaceChar:=Fline[lIndex]
  else
    aFirstNonWhitespaceChar:=#0;
end;


function TMarkDownLine.Remainder: AnsiString;

begin
  Result:=Copy(FLine,FCursor,Length(FLine)-FCursor+1);
end;


function TMarkDownLine.SkipWhiteSpace: Char;

var
  lLen : Integer;

begin
  lLen:=Length(FLine);
  while (FCursor<=lLen) and isWhitespaceChar(FLine[FCursor]) do
    inc(FCursor);
  if FCursor<lLen then
    Result:=FLine[FCursor+1]
  else
    Result:=#0;
end;


end.

