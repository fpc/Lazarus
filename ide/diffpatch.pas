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
  
  Abstract:
    Methods for creating diffs and ToDo: applying them (patching).

}
unit DiffPatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TTextDiffFlag = (
    tdfIgnoreCase,            // ignore case of letters
    tdfIgnoreEmptyLineChanges,// ignore if empty lines were added or removed
    tdfIgnoreHeadingSpaces,   // ignore spaces at start of line
    tdfIgnoreLineEnds,        // ignore if line end chars differ (e.g. #10 = #13#10)
    tdfIgnoreSpaceCharAmount, // ignore if space chars were added or removed
                              // except if all spaces were removed
    tdfIgnoreSpaceChars,      // ignore spaces (newline chars not included)
    tdfIgnoreTrailingSpaces   // ignore spaces at end of line
  );
  TTextDiffFlags = set of TTextDiffFlag;

function CreateTextDiff(const Text1, Text2: string; Flags: TTextDiffFlags
  ): string;

const
  TextDiffFlagNames: array[TTextDiffFlag] of string = (
    'IgnoreCase',
    'IgnoreEmptyLineChanges',
    'IgnoreHeadingSpaces',
    'IgnoreLineEnds',
    'IgnoreSpaceCharAmount',
    'IgnoreSpaceChars',
    'IgnoreTrailingSpaces'
    );

implementation


const
  LineBreak = {$IFDEF win32}#13+{$ENDIF}#10;

type
  TLineExtends = record
    LineStart: integer;
    LineEnd: integer;
    LineNumber: integer;
    NextLineStart: integer;
  end;

var
  IsSpaceChars: array[char] of boolean;
  UpperCaseChars: array[char] of char;

function TextToLine(const s: string): string;
var
  i: integer;
  OrdStr: string;
begin
  Result:=s;
  i:=1;
  while i<=length(Result) do begin
    if ord(Result[i])>=ord(' ') then begin
      inc(i);
    end else begin
      OrdStr:='#'+IntToStr(ord(Result[i]));
      Result:=LeftStr(Result,i-1)+OrdStr+RightStr(Result,length(Result)-i);
    end;
  end;
end;
  
function LineExtendsToStr(LineExtends: TLineExtends; const s: string): string;
begin
  with LineExtends do
    Result:='(Start='+IntToStr(LineStart)+' End='+IntToStr(LineEnd)
      +' Next='+IntToStr(NextLineStart)+' Number='+IntToStr(LineNumber)
      +' Text="'+TextToLine(copy(s,LineStart,NextLineStart-LineStart))+'")';
end;

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
      and (s[LineEnd]<>s[NextLineStart]) then
        inc(NextLineStart);
      exit;
    end;
  end;
  // this was the last line and it has no line end
  NextLineStart:=LineEnd;
end;

procedure GetLineExtends(const s: string; var LineExtends: TLineExtends);
begin
  GetLineExtends(s,LineExtends.LineStart,LineExtends.LineEnd,
                 LineExtends.NextLineStart);
end;

function CountLineEnds(const s: string; StartPos, EndPos: integer): integer;
begin
  Result:=0;
  while (StartPos<EndPos) do begin
    if not (s[StartPos] in [#10,#13]) then begin
      inc(StartPos);
    end else begin
      inc(Result);
      inc(StartPos);
      if (StartPos<EndPos) and (s[StartPos] in [#10,#13])
      and (s[StartPos]<>s[StartPos-1]) then
        inc(StartPos);
    end;
  end;
end;

function CountLinesTillEnd(const s: string; StartPos: integer): integer;
var Len: integer;
begin
  Len:=length(s);
  Result:=CountLineEnds(s,StartPos,Len+1);
  if (StartPos<=Len) and (not (s[Len] in [#10,#13])) then
    inc(Result);
end;

function IsEmptyLine(const s: string; LineStart, LineEnd: integer;
  Flags: TTextDiffFlags): boolean;
var
  i: integer;
begin
  if LineStart<=length(s) then begin
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
  end else begin
    Result:=true;
    exit;
  end;
end;

function GotoNextLine(var LineExtends: TLineExtends): boolean;
begin
  with LineExtends do begin
    if LineStart<NextLineStart then begin
      inc(LineNumber);
      LineStart:=NextLineStart;
      Result:=true;
    end else
      Result:=false;
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
      if not IsSpaceChars[Text2[Pos2]] then begin
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
          until (Pos2>=End2) or (not IsSpaceChars[Text2[Pos2]]);
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
          until (Pos1>=End1) or (not IsSpaceChars[Text1[Pos1]]);
        end;
      end else begin
        // Text2 contains a space
        if [tdfIgnoreSpaceChars,tdfIgnoreSpaceCharAmount]*Flags<>[] then begin
          // skip all spaces in Text1 and Text2 and proceed the search
          repeat
            inc(Pos1);
          until (Pos1>=End1) or (not IsSpaceChars[Text1[Pos1]]);
          repeat
            inc(Pos2);
          until (Pos2>=End2) or (not IsSpaceChars[Text2[Pos2]]);
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

function LinesAreEqual(
  const Text1: string; Line1Extends: TLineExtends;
  const Text2: string; Line2Extends: TLineExtends;
  Flags: TTextDiffFlags): boolean;
begin
  Result:=LinesAreEqual(Text1,Line1Extends.LineStart,Line1Extends.LineEnd,
                              Line1Extends.NextLineStart,
                        Text2,Line2Extends.LineStart,Line2Extends.LineEnd,
                              Line2Extends.NextLineStart,
                        Flags);
end;

procedure GetNextLineExtends(const s: string;
  var LineStart, LineEnd, NextLineStart, LineNumber: integer;
  Flags: TTextDiffFlags);
var Len: integer;
begin
  Len:=length(s);
  repeat
    GetLineExtends(s,LineStart,LineEnd,NextLineStart);
    if (LineStart>Len)
    or (not (tdfIgnoreEmptyLineChanges in Flags))
    or (not IsEmptyLine(s,LineStart,LineEnd,Flags)) then
      break;
    LineStart:=NextLineStart;
    inc(LineNumber);
  until false;
end;

procedure GetNextLineExtends(const s: string;
  var LineExtends: TLineExtends; Flags: TTextDiffFlags);
begin
  GetNextLineExtends(s,LineExtends.LineStart,LineExtends.LineEnd,
                     LineExtends.NextLineStart,LineExtends.LineNumber,
                     Flags);
end;

procedure WriteStrToStream(Stream: TStream; const s: string);
begin
  if s='' then exit;
  Stream.Write(s[1],length(s));
end;

procedure AddDiff(DiffStream: TStream;
  const Text1: string; Start1, End1: TLineExtends;
  const Text2: string; Start2, End2: TLineExtends);
{ Start1/2 is the first line that is different
  End1/2 is the first line that is equal
  
  The diff output:
    - There are three types of diffs: insertions, deletions and replacements
    - Insertions:
      - An insertion starts with a
        <lines>a<lines>
        followed by the lines of text 2.
      - A deletion starts with a
        <lines>d<lines>
        followed by the lines of text 1.
      - A replacement starts with a
        <lines>c<lines>
        followed by the lines of text 1, folowed by
        ---
        followed by the lines of text 2.
    - <lines> can be single decimal number or a range. For example:
       1
       2,3
    - The lines of text 1 are always prefixed with '< '
      If the lines of text 1 do not end with a newline char it ends with a line
      \ No newline at end
    - The lines of text 2 are always prefixed with '> '
      If the lines of text 1 do not end with a newline char it ends with a line
      \ No newline at end
}
var
  DiffStartLine1, DiffEndLine1: integer;
  DiffStartLine2, DiffEndLine2: integer;
  ActionChar: char;
  
  procedure WriteActionLine;
  begin
    // write line numbers of text 1
    WriteStrToStream(DiffStream,IntToStr(DiffStartLine1));
    if DiffEndLine1>DiffStartLine1 then begin
      WriteStrToStream(DiffStream,',');
      WriteStrToStream(DiffStream,IntToStr(DiffEndLine1));
    end;
    // write action character 'a', 'd' or 'c'
    if (Start1.LineStart<End1.LineStart) then begin
      // part of text 1 is replaced
      if (Start2.LineStart<End2.LineStart) then
        ActionChar:='c'  // replacement
      else
        ActionChar:='d'; // deletion
    end else begin
      // insertion
      ActionChar:='a';
    end;
    DiffStream.Write(ActionChar,1);
    // write line numbers of text 2
    WriteStrToStream(DiffStream,IntToStr(DiffStartLine2));
    if DiffEndLine2>DiffStartLine2 then begin
      WriteStrToStream(DiffStream,',');
      WriteStrToStream(DiffStream,IntToStr(DiffEndLine2));
    end;
    // write <newline>
    WriteStrToStream(DiffStream,LineBreak);
  end;
  
  procedure WriteLinesOfText(const s, Prefix: string;
    StartLine, EndLine: TLineExtends);
  var
    Line: TLineExtends;
  begin
    Line:=StartLine;
    while (Line.LineStart<EndLine.LineStart) do begin
      WriteStrToStream(DiffStream,Prefix);
      if (Line.LineEnd>Line.LineStart) then
        DiffStream.Write(s[Line.LineStart],Line.LineEnd-Line.LineStart);
      if (Line.NextLineStart>Line.LineEnd) then
        DiffStream.Write(s[Line.LineEnd],Line.NextLineStart-Line.LineEnd)
      else begin
        WriteStrToStream(DiffStream,LineBreak);
        WriteStrToStream(DiffStream,'\ No newline at end');
        WriteStrToStream(DiffStream,LineBreak);
      end;
      if not GotoNextLine(Line) then break;
      GetLineExtends(s,Line);
    end;
  end;
  
begin
  if (Start1.LineStart>length(Text1))
  and (Start2.LineStart>length(Text2)) then
    // no diff
    exit;
  DiffStartLine1:=Start1.LineNumber;
  DiffEndLine1:=End1.LineNumber-1;
  if DiffStartLine1>DiffEndLine1 then
    DiffStartLine1:=DiffEndLine1;
  DiffStartLine2:=Start2.LineNumber;
  DiffEndLine2:=End2.LineNumber-1;
  if DiffStartLine2>DiffEndLine2 then
    DiffStartLine2:=DiffEndLine2;
  WriteActionLine;
  WriteLinesOfText(Text1,'< ',Start1,End1);
  if ActionChar='c' then begin
    WriteStrToStream(DiffStream,'---');
    WriteStrToStream(DiffStream,LineBreak);
  end;
  WriteLinesOfText(Text2,'> ',Start2,End2);
end;

procedure AddRestDiff(DiffStream: TStream;
  const Text1: string; Start1: TLineExtends;
  const Text2: string; Start2: TLineExtends);
var
  End1, End2: TLineExtends;
begin
  End1.LineStart:=length(Text1)+1;
  End1.LineEnd:=End1.LineStart;
  End1.NextLineStart:=End1.LineStart;
  End1.LineNumber:=Start1.LineNumber+CountLinesTillEnd(Text1,Start1.LineStart);
  End2.LineStart:=length(Text2)+1;
  End2.LineEnd:=End2.LineStart;
  End2.NextLineStart:=End2.LineStart;
  End2.LineNumber:=Start2.LineNumber+CountLinesTillEnd(Text2,Start2.LineStart);
  AddDiff(DiffStream,  Text1,Start1,End1,  Text2,Start2,End2);
end;

procedure FindNextEqualLine(
  const Text1: string; Start1: TLineExtends;
  const Text2: string; Start2: TLineExtends;
  Flags: TTextDiffFlags;
  var EqualLine1, EqualLine2: TLineExtends
  );
var
  Max1, Max2, Cur1, Cur2: TLineExtends;
begin
  Max1:=Start1;
  Max2:=Start2;

  Cur1:=Start1;
  Cur2:=Start2;
  try
    if LinesAreEqual(Text1,Cur1,Text2,Cur2,Flags) then exit;
    repeat
      // increase Max1
      if GotoNextLine(Max1) then begin
        GetLineExtends(Text1,Max1);
        // search Max1 Line in Text2
        if Max1.LineStart<Max1.NextLineStart then begin
          Cur1:=Max1;
          Cur2:=Start2;
          repeat
            if LinesAreEqual(Text1,Cur1,Text2,Cur2,Flags) then exit;
            if Cur2.LineStart>=Max2.LineStart then break;
            Cur2.LineStart:=Cur2.NextLineStart;
            inc(Cur2.LineNumber);
            GetLineExtends(Text2,Cur2);
          until false;
        end;
      end;
      // increase Max2
      if GotoNextLine(Max2) then begin
        GetLineExtends(Text2,Max2);
        // search Max2 Line in Text1
        if Max2.LineStart<Max2.NextLineStart then begin
          Cur1:=Start1;
          Cur2:=Max2;
          repeat
            if LinesAreEqual(Text1,Cur1,Text2,Cur2,Flags) then exit;
            if Cur1.LineStart>=Max1.LineStart then break;
            Cur1.LineStart:=Cur1.NextLineStart;
            inc(Cur1.LineNumber);
            GetLineExtends(Text1,Cur1);
          until false;
        end;
      end;
    until (Max1.LineStart>=Max1.NextLineStart)
      and (Max2.LineStart>=Max2.NextLineStart);
    // no equal line found
    Cur1:=Max1;
    Cur2:=Max2;
  finally
    EqualLine1:=Cur1;
    EqualLine2:=Cur2;
  end;
end;

function CreateTextDiff(const Text1, Text2: string; Flags: TTextDiffFlags
  ): string;
var
  Line1, Line2, EqualLine1, EqualLine2: TLineExtends;
  Len1, Len2: integer;
  DiffStream: TMemoryStream;
begin
  DiffStream:=TMemoryStream.Create;
  try
    Len1:=length(Text1);
    Len2:=length(Text2);
    Line1.LineStart:=1;
    Line1.LineNumber:=1;
    Line2.LineStart:=1;
    Line2.LineNumber:=1;
    repeat
      // search for a differing line ...
      repeat
        // skip empty lines in Text1 and get line1 extends ...
        GetNextLineExtends(Text1,Line1,Flags);
        // skip empty lines in Text2 and get line2 extends ...
        GetNextLineExtends(Text2,Line2,Flags);
        // skip equal lines ...
        if (Line1.LineStart<=Len1) and (Line2.LineStart<=Len2) then begin
          if not LinesAreEqual(Text1,Line1,Text2,Line2,Flags)
          then
            break;
          Line1.LineStart:=Line1.NextLineStart;
          inc(Line1.LineNumber);
          Line2.LineStart:=Line2.NextLineStart;
          inc(Line2.LineNumber);
        end else begin
          if (Line1.LineStart<=Len1) or (Line2.LineStart<=Len2) then begin
            // one text is longer than the other
            AddRestDiff(DiffStream,  Text1,Line1,  Text2,Line2);
          end else begin
            // no more diff found
          end;
          exit;
        end;
      until false;
      // differing line found -> search next equal line
      FindNextEqualLine(Text1,Line1, Text2,Line2, Flags, EqualLine1,EqualLine2);
      AddDiff(DiffStream,
              Text1,Line1,EqualLine1,
              Text2,Line2,EqualLine2
              );
      // continue the search ...
      Line1:=EqualLine1;
      GotoNextLine(Line1);
      Line2:=EqualLine2;
      GotoNextLine(Line2);
    until false;
  finally
    SetLength(Result,DiffStream.Size);
    DiffStream.Position:=0;
    if Result<>'' then
      DiffStream.Read(Result[1],length(Result));
    DiffStream.Free;
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

