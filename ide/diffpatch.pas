{  $Id$  }
{
 /***************************************************************************
   diffpatch.pas - functions to extract differences between texts
                   (diffs, patches) and apply them (patching).

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

}
unit DiffPatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TTextDiffFlag = (
    tdfIgnoreSpaceCharAmount, // ignore if space chars were added or removed
                              // except if all spaces were removed
    tdfIgnoreSpaceChars,      // ignore spaces (newline chars not included)
    tdfIgnoreHeadingSpaces,   // ignore spaces at start of line
    tdfIgnoreTrailingSpaces,  // ignore spaces at end of line
    tdfIgnoreEmptyLineChanges,// ignore if empty lines were added or removed
    tdfIgnoreLineEnds,        // ignore if line chars differ (e.g. #10 = #13#10)
    tdfIgnoreCase             // ignore case of letters
  );
  TTextDiffFlags = set of TTextDiffFlag;

function CreateTextDiff(const Text1, Text2: string; Flags: TTextDiffFlags
  ): string;


implementation

var
  IsSpaceChars: array[char] of boolean;
  UpperCaseChars: array[char] of char;

procedure GetLineExtends(const s: string; LineStart: integer;
  var LineEnd, NextLineStart: integer);
var Len: integer;
begin
  Len:=length(s);
  LineEnd:=LineStart;
  while (LineEnd<=Len) do begin
    if (not (s[LineEnd] in [#10,#13])) then begin
      inc(LineEnd);
    end else begin
      NextLineStart:=LineEnd+1;
      if (NextLineStart<=Len) and (s[NextLineStart] in [#10,#13])
      and (s[LineStart]<>s[NextLineStart]) then
        inc(NextLineStart);
      exit;
    end;
  end;
  // this was the last line and it has no line end
  NextLineStart:=LineEnd;
end;

function IsEmptyLine(const s: string; LineStart, LineEnd: integer;
  Flags: TTextDiffFlags): boolean;
var
  i: integer;
begin
  if ([tdfIgnoreSpaceCharAmount,tdfIgnoreSpaceChars,tdfIgnoreHeadingSpaces,
    tdfIgnoreTrailingSpaces]*Flags)<>[] then
  begin
    Result:=true;
    for i:=LineStart to LineEnd-1 do begin
      if not IsSpaceChars[s[i]] then begin
        Result:=false;
        exit;
      end;
    end;
  end else begin
    Result:=(LineEnd=LineStart);
  end;
end;

function LinesAreEqual(
  const Text1: string; Line1Start, Line1End, NextLine1Start: integer;
  const Text2: string; Line2Start, Line2End, NextLine2Start: integer;
  Flags: TTextDiffFlags): boolean;
var
  Start1, End1, Pos1,
  Start2, End2, Pos2: integer;
begin
  Start1:=Line1Start;
  End1:=Line1End;
  Start2:=Line2Start;
  End2:=Line2End;
  if [tdfIgnoreHeadingSpaces,tdfIgnoreSpaceChars]*Flags<>[] then begin
    // ignore spaces at start of line
    while (Start1<End1) and IsSpaceChars[Text1[Start1]] do inc(Start1);
    while (Start2<End2) and IsSpaceChars[Text2[Start2]] do inc(Start2);
  end;
  if [tdfIgnoreTrailingSpaces,tdfIgnoreSpaceChars]*Flags<>[] then begin
    // ignore spaces at end of line
    while (Start1<End1) and IsSpaceChars[Text1[End1-1]] do dec(End1);
    while (Start2<End2) and IsSpaceChars[Text2[End2-1]] do dec(End2);
  end;
  
  // compare line content (i.e. the chars without the line end)
  Pos1:=Start1;
  Pos2:=Start2;
  while (Pos1<End1) and (Pos2<End2) do begin
    if not IsSpaceChars[Text1[Pos1]] then begin
      // Text1 contains a normal char
      if not IsSpaceChars[Text1[Pos1]] then begin
        // Text2 contains a normal char
        if tdfIgnoreCase in Flags then begin
          // compare case insensitive
          if UpperCaseChars[Text1[Pos1]]=UpperCaseChars[Text2[Pos2]] then begin
            // no diff -> proceed with next chars
            inc(Pos1);
            inc(Pos2);
          end else begin
            // diff found -> lines differ
            Result:=false;
            exit;
          end;
        end else begin
          // compare case sensitive
          if Text1[Pos1]=Text2[Pos2] then begin
            // no diff -> proceed with next chars
            inc(Pos1);
            inc(Pos2);
          end else begin
            // diff found -> lines differ
            Result:=false;
            exit;
          end;
        end;
      end else begin
        // Text2 contains a space
        if not (tdfIgnoreSpaceChars in Flags) then begin
          // diff found -> lines differ
          Result:=false;
          exit;
        end else begin
          // skip all spaces in Text2 and proceed the search
          repeat
            inc(Pos2);
          until (Pos2>=End2) or (IsSpaceChars[Text2[Pos2]]);
        end;
      end;
    end else begin
      // Text1 contains a space
      if not IsSpaceChars[Text2[Pos2]] then begin
        // Text2 contains a normal char
        if not (tdfIgnoreSpaceChars in Flags) then begin
          // diff found -> lines differ
          Result:=false;
          exit;
        end else begin
          // skip all spaces in Text1 and proceed the search
          repeat
            inc(Pos1);
          until (Pos1>=End1) or (IsSpaceChars[Text1[Pos1]]);
        end;
      end else begin
        // Text2 contains a space
        if [tdfIgnoreSpaceChars,tdfIgnoreSpaceCharAmount]*Flags<>[] then begin
          // skip all spaces in Text1 and Text2 and proceed the search
          repeat
            inc(Pos1);
          until (Pos1>=End1) or (IsSpaceChars[Text1[Pos1]]);
          repeat
            inc(Pos2);
          until (Pos2>=End2) or (IsSpaceChars[Text2[Pos2]]);
        end else begin
          // compare the space chars
          if Text1[Pos1]=Text2[Pos2] then begin
            // no diff -> proceed with next chars
            inc(Pos1);
            inc(Pos2);
          end else begin
            // diff found -> lines differ
            Result:=false;
            exit;
          end;
        end;
      end;
    end;
  end;
  if (Pos1<End1) or (Pos2<End2) then begin
    // one line is longer -> lines differ
    Result:=false;
    exit;
  end;
  // compare line ends
  if not (tdfIgnoreLineEnds in Flags) then begin
    Pos1:=Line1End;
    Pos2:=Line2End;
    while (Pos1<NextLine1Start) and (Pos2<NextLine2Start)
    and (Text1[Pos1]=Text2[Pos2]) do begin
      inc(Pos1);
      inc(Pos2);
    end;
    Result:=(Pos1=NextLine1Start) and (Pos2=NextLine2Start);
  end else begin
    Result:=true;
  end;
end;

function CreateTextDiff(const Text1, Text2: string; Flags: TTextDiffFlags
  ): string;
var
  LineNumber1, LineNumber2: integer;
  Len1, Line1Start, Line1End, NextLine1Start: integer;
  Len2, Line2Start, Line2End, NextLine2Start: integer;
  DiffMemStream: TMemoryStream;
begin
  DiffMemStream:=TMemoryStream.Create;
  try
    Len1:=length(Text1);
    Len2:=length(Text2);
    LineNumber1:=1;
    LineNumber2:=1;
    Line1Start:=1;
    Line2Start:=1;
    // search for a differing line ...
    repeat
      // skip empty lines in Text1 and get line1 extends ...
      while (Line1Start<=Len1) do begin
        GetLineExtends(Text1,Line1Start,Line1End,NextLine1Start);
        if not (tdfIgnoreEmptyLineChanges in Flags)
        or not IsEmptyLine(Text1,Line1Start,Line1End,Flags) then
          break;
        Line1Start:=NextLine1Start;
        inc(LineNumber1);
      end;
      // skip empty lines in Text2 and get line2 extends ...
      while (Line2Start<=Len2) do begin
        GetLineExtends(Text2,Line2Start,Line2End,NextLine2Start);
        if not (tdfIgnoreEmptyLineChanges in Flags)
        or not IsEmptyLine(Text2,Line2Start,Line2End,Flags) then
          break;
        Line2Start:=NextLine2Start;
        inc(LineNumber2);
      end;
      // skip equal lines ...
      if (Line1Start<=Len1) and (Line2Start<=Len2) then begin
        if not LinesAreEqual(Text1,Line1Start,Line1End,NextLine1Start,
                             Text2,Line2Start,Line2End,NextLine2Start,
                             Flags)
        then
          break;
        Line1Start:=NextLine1Start;
        inc(LineNumber1);
        Line2Start:=NextLine2Start;
        inc(LineNumber2);
      end else begin
        // one text is longer than the other
        
        // ToDo:
        
        //AddDiff(Text1,Text2,Line1Start,Len1,Line2Start,Len2);
        exit;
      end;
    until false;
    // differing line found -> search next equal line
    
    // ToDo:
    
  finally
    SetLength(Result,DiffMemStream.Size);
    DiffMemStream.Position:=0;
    if Result<>'' then
      DiffMemStream.Read(Result[1],length(Result));
    DiffMemStream.Free;
  end;
end;

procedure InternalInit;
var c: char;
begin
  for c:=Low(char) to High(char) do begin
    IsSpaceChars[c]:=c in [' ',#9];
    UpperCaseChars[c]:=upcase(c);
  end;
end;

initialization
  InternalInit;

finalization


end.

