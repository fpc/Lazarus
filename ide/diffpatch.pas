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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, LazLoggerBase;

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
  
  TTextDiffOutputType = (
    tdoContext,
    tdoRCS
    );

  TLineExtends = record
    LineStart: integer;
    LineEnd: integer;
    LineNumber: integer; // starting at 1
    NextLineStart: integer;
  end;

  TDiffOutput = class;

  { TDiffPart }

  TDiffPart = class
  private
    fOwner: TDiffOutput;
    fSource: string;
    fStartLine: integer; // starting at 1
    fEndLine: integer;   // starting at 1
    fPosition: TLineExtends;
    fStream: TStream;
  public
    constructor Create(aOwner: TDiffOutput; const aSource: string);
    destructor Destroy; override;
    procedure Init(StartExt: TLineExtends);
    procedure Write(const HeaderPrefix, HeaderSuffix: string);
    procedure Write2(const StartExt, EndExt: TLineExtends;
      OtherPartHasChangedLines: boolean; CharForInsertDeletion: char);
    procedure WriteLinesOfText(aStream: TStream; const aPrefix: string;
                               const aStartLine: TLineExtends; aEndPos: integer);
    function LineExtendsToStr(const LineExtends: TLineExtends): string;
    procedure GetLineExtends(LineStart: integer; var LineEnd, NextLineStart: integer);
    procedure GetLineExtends(var LineExtends: TLineExtends);
    procedure GetPrevLineExtends(LineStart: integer; var PrevLineStart, PrevLineEnd: integer);
    procedure GetPrevLineExtends(var LineExtends: TLineExtends);
    function CountLineEnds(StartPos, EndPos: integer): integer;
    function CountLinesTillEnd(StartPos: integer): integer;
    function IsEmptyLine(LineStart, LineEnd: integer): boolean;
    procedure GetNextLineExtends(var LineStart, LineEnd, NextLineStart, LineNumber: integer);
    procedure GetNextLineExtends(var LineExtends: TLineExtends);
  end;

  TProgressEvent = procedure(aPosition: Integer) of object;

  { TDiffOutput }

  TDiffOutput = class
  private
    fText1, fText2: string;
    fOutputType: TTextDiffOutputType;
    fOnProgressPos: TProgressEvent;
    fFlags: TTextDiffFlags;
    fDiffStream: TStream;
    fPart1, fPart2: TDiffPart;
    procedure FindNextEqualLine(const Start1, Start2: TLineExtends;
      out EqualLine1, EqualLine2: TLineExtends);
    function LinesAreEqual(Line1Start, Line1End, NextLine1Start: integer;
                           Line2Start, Line2End, NextLine2Start: integer): boolean;
    function LinesAreEqual(const Line1Extends, Line2Extends: TLineExtends): boolean;
    procedure AddDefaultDiff(const Start1, End1, Start2, End2: TLineExtends);
    procedure AddContextDiff(const Start1, End1, Start2, End2: TLineExtends);
    procedure FinishOldContextBlock;
    procedure FinishDiff;
    procedure AddRestDiff(const Start1, Start2: TLineExtends);
    procedure AddDiff(const Start1, End1, Start2, End2: TLineExtends);
    procedure UpdateProgressBar(const Line: TLineExtends);
  public
    constructor Create(const aText1, aText2: string; aFlags: TTextDiffFlags);
    destructor Destroy; override;
    function GetProgressMax: Integer;
    function CreateTextDiff: string;
  public
    property OutputType: TTextDiffOutputType read fOutputType write fOutputType;
    property OnProgressPos: TProgressEvent read fOnProgressPos write fOnProgressPos;
  end;


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
  LineBreak = #10;
  ContextLineCount = 3;

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
  
function GotoNextLine(var LineExtends: TLineExtends): boolean;
begin
  with LineExtends do begin
    Result:=LineStart<NextLineStart;
    if Result then begin
      inc(LineNumber);
      LineStart:=NextLineStart;
    end;
  end;
end;

procedure WriteStrToStream(Stream: TStream; const s: string);
begin
  if s='' then exit;
  Stream.Write(s[1],length(s));
end;

{ TDiffPart }

constructor TDiffPart.Create(aOwner: TDiffOutput; const aSource: string);
begin
  fOwner:=aOwner;
  fSource:=aSource;
end;

destructor TDiffPart.Destroy;
begin
  fStream.Free;
  inherited Destroy;
end;

procedure TDiffPart.Init(StartExt: TLineExtends);
begin
  if fStream=nil then
    fStream:=TMemoryStream.Create
  else
    fStream.Size:=0;
  fStartLine:=StartExt.LineNumber-ContextLineCount;
  if fStartLine<1 then fStartLine:=1;
  fPosition:=StartExt;
  while fPosition.LineNumber>fStartLine do
    GetPrevLineExtends(fPosition);
end;

procedure TDiffPart.Write(const HeaderPrefix, HeaderSuffix: string);
begin
  // check if part contains any changed lines
  if fPosition.LineNumber > fStartLine then begin
    // part contains changed lines -> append end context
    while (fPosition.LineNumber <= fEndLine)
    and (fPosition.LineStart < length(fSource)) do begin
      WriteLinesOfText(fStream, '  ', fPosition, fPosition.NextLineStart);
      if not GotoNextLine(fPosition) then
        break;
      GetLineExtends(fPosition);
    end;
  end else begin
    // part does not contain changed lines -> skip
  end;
  fStream.Position:=0;
  // write part
  WriteStrToStream(fOwner.fDiffStream, HeaderPrefix +
       IntToStr(fStartLine) + ',' + IntToStr(fEndLine) + HeaderSuffix + LineBreak);
  if fStream.Size<>0 then
    fOwner.fDiffStream.CopyFrom(fStream, fStream.Size);
end;

procedure TDiffPart.Write2(const StartExt, EndExt: TLineExtends;
  OtherPartHasChangedLines: boolean; CharForInsertDeletion: char);
begin
  // check if there are changed lines
  if StartExt.LineStart < EndExt.LineStart then begin
    // write lines
    while fPosition.LineStart < EndExt.LineStart do begin
      if fPosition.LineStart < StartExt.LineStart then
        // this is an unchanged line in front of the changed lines
        WriteStrToStream(fStream, '  ')
      else begin
        // this is a changed line
        if OtherPartHasChangedLines then
          WriteStrToStream(fStream, '! ')
        else
          WriteStrToStream(fStream, CharForInsertDeletion+' ');
      end;
      if fPosition.LineStart <> fPosition.NextLineStart then
        fStream.Write(fSource[fPosition.LineStart], fPosition.NextLineStart-fPosition.LineStart);
      if not GotoNextLine(fPosition) then
        break;
      GetLineExtends(fPosition);
    end;
  end;
end;

procedure TDiffPart.WriteLinesOfText(aStream: TStream;
  const aPrefix: string; const aStartLine: TLineExtends; aEndPos: integer);
{ Write all lines in front of EndLine, starting with StartLine }
var
  Line: TLineExtends;
begin
  Line:=aStartLine;
  while (Line.LineStart<aEndPos) do begin
    WriteStrToStream(aStream,aPrefix);
    if (Line.LineEnd>Line.LineStart) then
      aStream.Write(fSource[Line.LineStart],Line.LineEnd-Line.LineStart);
    if (Line.NextLineStart>Line.LineEnd) then
      aStream.Write(fSource[Line.LineEnd],Line.NextLineStart-Line.LineEnd)
    else begin
      WriteStrToStream(aStream,LineBreak);
      WriteStrToStream(aStream,'\ No newline at end');
      WriteStrToStream(aStream,LineBreak);
    end;
    if not GotoNextLine(Line) then break;
    GetLineExtends(Line);
  end;
end;

function TDiffPart.LineExtendsToStr(const LineExtends: TLineExtends): string;
begin
  with LineExtends do
    Result:='(Start='+IntToStr(LineStart)+' End='+IntToStr(LineEnd)
      +' Next='+IntToStr(NextLineStart)+' Number='+IntToStr(LineNumber)
      +' Text="'+TextToLine(copy(fSource,LineStart,NextLineStart-LineStart))+'")';
end;

procedure TDiffPart.GetLineExtends(LineStart: integer; var LineEnd, NextLineStart: integer);
var
  Len: integer;
begin
  Len:=length(fSource);
  LineEnd:=LineStart;
  while LineEnd<=Len do begin
    if (not (fSource[LineEnd] in [#10,#13])) then begin
      inc(LineEnd);
    end else begin
      NextLineStart:=LineEnd+1;
      if (NextLineStart<=Len) and (fSource[NextLineStart] in [#10,#13])
      and (fSource[LineEnd]<>fSource[NextLineStart]) then
        inc(NextLineStart);
      exit;
    end;
  end;
  // this was the last line and it has no line end
  NextLineStart:=LineEnd;
end;

procedure TDiffPart.GetLineExtends(var LineExtends: TLineExtends);
begin
  GetLineExtends(LineExtends.LineStart, LineExtends.LineEnd, LineExtends.NextLineStart);
end;

procedure TDiffPart.GetPrevLineExtends(LineStart: integer;
  var PrevLineStart, PrevLineEnd: integer);
begin
  // read line end
  PrevLineEnd:=LineStart;
  if (PrevLineEnd>1) and (fSource[PrevLineEnd-1] in [#10,#13]) then
    dec(PrevLineEnd);
  if (PrevLineEnd>1) and (fSource[PrevLineEnd-1] in [#10,#13])
  and (fSource[PrevLineEnd]<>fSource[PrevLineEnd-1]) then
    dec(PrevLineEnd);
  // read line content
  PrevLineStart:=PrevLineEnd;
  while (PrevLineStart>1) and (not (fSource[PrevLineStart-1] in [#10,#13])) do
    dec(PrevLineStart);
end;

procedure TDiffPart.GetPrevLineExtends(var LineExtends: TLineExtends);
begin
  if LineExtends.LineStart<1 then exit;
  LineExtends.NextLineStart:=LineExtends.LineStart;
  GetPrevLineExtends(LineExtends.LineStart,LineExtends.LineStart,LineExtends.LineEnd);
  dec(LineExtends.LineNumber);
end;

function TDiffPart.CountLineEnds(StartPos, EndPos: integer): integer;
begin
  Result:=0;
  while (StartPos<EndPos) do begin
    if not (fSource[StartPos] in [#10,#13]) then begin
      inc(StartPos);
    end else begin
      inc(Result);
      inc(StartPos);
      if (StartPos<EndPos) and (fSource[StartPos] in [#10,#13])
      and (fSource[StartPos]<>fSource[StartPos-1]) then
        inc(StartPos);
    end;
  end;
end;

function TDiffPart.CountLinesTillEnd(StartPos: integer): integer;
var
  Len: integer;
begin
  Len:=length(fSource);
  Result:=CountLineEnds(StartPos,Len+1);
  if (StartPos<=Len) and (not (fSource[Len] in [#10,#13])) then
    inc(Result);
end;

function TDiffPart.IsEmptyLine(LineStart, LineEnd: integer): boolean;
var
  i: integer;
begin
  if LineStart<=length(fSource) then begin
    if ([tdfIgnoreSpaceCharAmount,tdfIgnoreSpaceChars,tdfIgnoreHeadingSpaces,
      tdfIgnoreTrailingSpaces]*fOwner.fFlags)<>[] then
    begin
      Result:=true;
      for i:=LineStart to LineEnd-1 do begin
        if not IsSpaceChars[fSource[i]] then begin
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

procedure TDiffPart.GetNextLineExtends(var LineStart, LineEnd, NextLineStart, LineNumber: integer);
var
  Len: integer;
begin
  Len:=length(fSource);
  repeat
    GetLineExtends(LineStart,LineEnd,NextLineStart);
    if (LineStart>Len)
    or (not (tdfIgnoreEmptyLineChanges in fOwner.fFlags))
    or (not IsEmptyLine(LineStart,LineEnd)) then
      break;
    LineStart:=NextLineStart;
    inc(LineNumber);
  until false;
end;

procedure TDiffPart.GetNextLineExtends(var LineExtends: TLineExtends);
begin
  GetNextLineExtends(LineExtends.LineStart, LineExtends.LineEnd,
                     LineExtends.NextLineStart, LineExtends.LineNumber);
end;


{ TDiffOutput }

function TDiffOutput.LinesAreEqual(
  Line1Start, Line1End, NextLine1Start: integer;
  Line2Start, Line2End, NextLine2Start: integer): boolean;
var
  Start1, End1, Pos1,
  Start2, End2, Pos2: integer;
begin
  Start1:=Line1Start;
  End1:=Line1End;
  Start2:=Line2Start;
  End2:=Line2End;
  if [tdfIgnoreHeadingSpaces,tdfIgnoreSpaceChars]*fFlags<>[] then begin
    // ignore spaces at start of line
    while (Start1<End1) and IsSpaceChars[fText1[Start1]] do inc(Start1);
    while (Start2<End2) and IsSpaceChars[fText2[Start2]] do inc(Start2);
  end;
  if [tdfIgnoreTrailingSpaces,tdfIgnoreSpaceChars]*fFlags<>[] then begin
    // ignore spaces at end of line
    while (Start1<End1) and IsSpaceChars[fText1[End1-1]] do dec(End1);
    while (Start2<End2) and IsSpaceChars[fText2[End2-1]] do dec(End2);
  end;
  
  // compare line content (i.e. the chars without the line end)
  Pos1:=Start1;
  Pos2:=Start2;
  while (Pos1<End1) and (Pos2<End2) do begin
    if not IsSpaceChars[fText1[Pos1]] then begin
      // fText1 contains a normal char
      if not IsSpaceChars[fText2[Pos2]] then begin
        // fText2 contains a normal char
        if tdfIgnoreCase in fFlags then begin
          // compare case insensitive
          if UpperCaseChars[fText1[Pos1]]=UpperCaseChars[fText2[Pos2]] then begin
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
          if fText1[Pos1]=fText2[Pos2] then begin
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
        // fText2 contains a space
        if not (tdfIgnoreSpaceChars in fFlags) then begin
          // diff found -> lines differ
          Result:=false;
          exit;
        end else begin
          // skip all spaces in fText2 and proceed the search
          repeat
            inc(Pos2);
          until (Pos2>=End2) or (not IsSpaceChars[fText2[Pos2]]);
        end;
      end;
    end else begin
      // fText1 contains a space
      if not IsSpaceChars[fText2[Pos2]] then begin
        // fText2 contains a normal char
        if not (tdfIgnoreSpaceChars in fFlags) then begin
          // diff found -> lines differ
          Result:=false;
          exit;
        end else begin
          // skip all spaces in fText1 and proceed the search
          repeat
            inc(Pos1);
          until (Pos1>=End1) or (not IsSpaceChars[fText1[Pos1]]);
        end;
      end else begin
        // fText2 contains a space
        if [tdfIgnoreSpaceChars,tdfIgnoreSpaceCharAmount]*fFlags<>[] then begin
          // skip all spaces in fText1 and fText2 and proceed the search
          repeat
            inc(Pos1);
          until (Pos1>=End1) or (not IsSpaceChars[fText1[Pos1]]);
          repeat
            inc(Pos2);
          until (Pos2>=End2) or (not IsSpaceChars[fText2[Pos2]]);
        end else begin
          // compare the space chars
          if fText1[Pos1]=fText2[Pos2] then begin
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
  if not (tdfIgnoreLineEnds in fFlags) then begin
    Pos1:=Line1End;
    Pos2:=Line2End;
    while (Pos1<NextLine1Start) and (Pos2<NextLine2Start)
    and (fText1[Pos1]=fText2[Pos2]) do begin
      inc(Pos1);
      inc(Pos2);
    end;
    Result:=(Pos1=NextLine1Start) and (Pos2=NextLine2Start);
  end else begin
    Result:=true;
  end;
end;

function TDiffOutput.LinesAreEqual(const Line1Extends, Line2Extends: TLineExtends): boolean;
begin
  Result:=LinesAreEqual(Line1Extends.LineStart, Line1Extends.LineEnd,
                        Line1Extends.NextLineStart,
                        Line2Extends.LineStart, Line2Extends.LineEnd,
                        Line2Extends.NextLineStart);
end;

procedure TDiffOutput.FindNextEqualLine(const Start1, Start2: TLineExtends;
  out EqualLine1, EqualLine2: TLineExtends);
var
  Max1, Max2, Cur1, Cur2: TLineExtends;
begin
  Max1:=Start1;
  Max2:=Start2;
  Cur1:=Start1;
  Cur2:=Start2;
  try
    if LinesAreEqual(Cur1,Cur2)
    and (not fPart1.IsEmptyLine(Cur1.LineStart, Cur1.LineEnd)) then
      exit;
    repeat
      // increase Max1
      if GotoNextLine(Max1) then begin
        fPart1.GetLineExtends(Max1);
        // search Max1 Line in fText2
        if Max1.LineStart<Max1.NextLineStart then begin
          Cur1:=Max1;
          Cur2:=Start2;
          repeat
            if LinesAreEqual(Cur1,Cur2)
            and (not fPart1.IsEmptyLine(Cur1.LineStart, Cur1.LineEnd)) then
              exit;
            if Cur2.LineStart>=Max2.LineStart then break;
            Cur2.LineStart:=Cur2.NextLineStart;
            inc(Cur2.LineNumber);
            fPart2.GetLineExtends(Cur2);
          until false;
        end;
        UpdateProgressBar(Max1);
      end;
      // increase Max2
      if GotoNextLine(Max2) then begin
        fPart2.GetLineExtends(Max2);
        // search Max2 Line in fText1
        if Max2.LineStart<Max2.NextLineStart then begin
          Cur1:=Start1;
          Cur2:=Max2;
          repeat
            if LinesAreEqual(Cur1,Cur2)
            and (not fPart1.IsEmptyLine(Cur1.LineStart,Cur1.LineEnd)) then
              exit;
            if Cur1.LineStart>=Max1.LineStart then break;
            Cur1.LineStart:=Cur1.NextLineStart;
            inc(Cur1.LineNumber);
            fPart1.GetLineExtends(Cur1);
          until false;
        end;
      end;
    until (Max1.LineStart>=Max1.NextLineStart)
      and (Max2.LineStart>=Max2.NextLineStart);
    // no equal line found
    Cur1:=Max1;
    Cur2:=Max2;
  finally
    repeat
      EqualLine1:=Cur1;
      EqualLine2:=Cur2;
      // chomp empty lines at end
      fPart1.GetPrevLineExtends(Cur1);
      fPart2.GetPrevLineExtends(Cur2);
    until not LinesAreEqual(Cur1,Cur2);
  end;
end;

function TDiffOutput.CreateTextDiff: string;
var
  Line1, Line2, EqualLine1, EqualLine2: TLineExtends;
  Len1, Len2: integer;
begin
  Result := '';
  try
  try
    Len1:=length(fText1);
    Len2:=length(fText2);
    Line1.LineStart:=1;
    Line1.LineNumber:=1;
    Line2.LineStart:=1;
    Line2.LineNumber:=1;
    repeat
      // search for a difference line ...
      repeat
        // skip empty lines in fText1 and get line1 extends ...
        fPart1.GetNextLineExtends(Line1);
        // skip empty lines in fText2 and get line2 extends ...
        fPart2.GetNextLineExtends(Line2);
        // skip equal lines ...
        if (Line1.LineStart<=Len1) and (Line2.LineStart<=Len2) then begin
          if not LinesAreEqual(Line1,Line2) then
            break;
          Line1.LineStart:=Line1.NextLineStart;
          inc(Line1.LineNumber);
          Line2.LineStart:=Line2.NextLineStart;
          inc(Line2.LineNumber);
        end else begin
          if (Line1.LineStart<=Len1) or (Line2.LineStart<=Len2) then begin
            // one text is longer than the other
            AddRestDiff(Line1, Line2);
          end else begin
            // no more diff found
          end;
          exit;
        end;
        UpdateProgressBar(Line1);
      until false;
      // difference line found -> search next equal line
      FindNextEqualLine(Line1, Line2, EqualLine1, EqualLine2);
      AddDiff(Line1, EqualLine1, Line2, EqualLine2);
      // continue the search ...
      Line1:=EqualLine1;
      GotoNextLine(Line1);
      Line2:=EqualLine2;
      GotoNextLine(Line2);
      UpdateProgressBar(Line1);
    until false;
  except
    on E: Exception do begin
      DebugLogger.DebugLn('CreateTextDiff ',E.Message);
    end;
  end;
  finally
    FinishDiff;
    SetLength(Result,fDiffStream.Size);
    fDiffStream.Position:=0;
    if Result<>'' then
      fDiffStream.Read(Result[1],length(Result));
  end;
end;

procedure TDiffOutput.AddRestDiff(const Start1, Start2: TLineExtends);
var
  End1, End2: TLineExtends;
begin
  End1.LineStart:=length(fText1)+1;
  End1.LineEnd:=End1.LineStart;
  End1.NextLineStart:=End1.LineStart;
  End1.LineNumber:=Start1.LineNumber+fPart1.CountLinesTillEnd(Start1.LineStart);
  End2.LineStart:=length(fText2)+1;
  End2.LineEnd:=End2.LineStart;
  End2.NextLineStart:=End2.LineStart;
  End2.LineNumber:=Start2.LineNumber+fPart2.CountLinesTillEnd(Start2.LineStart);
  AddDiff(Start1,End1,  Start2,End2);
end;

procedure TDiffOutput.AddDiff(const Start1, End1, Start2, End2: TLineExtends);
begin
  if (Start1.LineStart>length(fText1)) and (Start2.LineStart>length(fText2)) then
    exit;                             // no diff
  case fOutputType of
    tdoContext:
      AddContextDiff(Start1,End1,Start2,End2);
    else
      AddDefaultDiff(Start1,End1,Start2,End2);
  end;
end;

procedure TDiffOutput.UpdateProgressBar(const Line: TLineExtends);
begin
  if Assigned(OnProgressPos) then
    OnProgressPos(Line.LineStart);
end;

procedure TDiffOutput.FinishOldContextBlock;
begin
  if fPart1.fStream <> nil then begin
    fPart1.Write('*** ',' ****');
    fPart2.Write('--- ',' ----');
  end;
end;

procedure TDiffOutput.FinishDiff;
begin
  case fOutputType of
    tdoContext: FinishOldContextBlock;
  end;
end;

procedure TDiffOutput.AddDefaultDiff(const Start1, End1, Start2, End2: TLineExtends);
{ Start1/2 is the first line that is different
  End1/2 is the first line that is equal

  The diff output:
    - There are three types of diffs: insertions, deletions and replacements
    - Insertions:
      - An insertion/addition starts with a
        <lines>a<lines>
        followed by the lines of text 2.
      - A deletion starts with a
        <lines>d<lines>
        followed by the lines of text 1.
      - A replacement/change starts with a
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
    WriteStrToStream(fDiffStream,IntToStr(DiffStartLine1));
    if DiffEndLine1>DiffStartLine1 then begin
      WriteStrToStream(fDiffStream,',');
      WriteStrToStream(fDiffStream,IntToStr(DiffEndLine1));
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
    fDiffStream.Write(ActionChar,1);
    // write line numbers of text 2
    WriteStrToStream(fDiffStream,IntToStr(DiffStartLine2));
    if DiffEndLine2>DiffStartLine2 then begin
      WriteStrToStream(fDiffStream,',');
      WriteStrToStream(fDiffStream,IntToStr(DiffEndLine2));
    end;
    // write <newline>
    WriteStrToStream(fDiffStream,LineBreak);
  end;

begin
  DiffStartLine1:=Start1.LineNumber;
  DiffEndLine1:=End1.LineNumber-1;
  if DiffStartLine1>DiffEndLine1 then
    DiffStartLine1:=DiffEndLine1;
  DiffStartLine2:=Start2.LineNumber;
  DiffEndLine2:=End2.LineNumber-1;
  if DiffStartLine2>DiffEndLine2 then
    DiffStartLine2:=DiffEndLine2;
  WriteActionLine;
  fPart1.WriteLinesOfText(fDiffStream,'< ',Start1,End1.LineStart);
  if ActionChar='c' then begin
    WriteStrToStream(fDiffStream,'---');
    WriteStrToStream(fDiffStream,LineBreak);
  end;
  fPart2.WriteLinesOfText(fDiffStream,'> ',Start2,End2.LineStart);
end;

procedure TDiffOutput.AddContextDiff(const Start1, End1, Start2, End2: TLineExtends);
{ Start1/2 is the first line that is different
  End1/2 is the first line that is equal

  The diff output:
    - Every diff block starts with 15 stars
      ***************
    - Every diff block has two parts. One for each text.
    - The first text part starts with
      *** StartLine,EndLine ****
    - The second text part starts with
      --- StartLine,EndLine ----
    - In front of and behind each changed line there are unchanged line. These
      lines are the context.
    - At the beginning and at the end there are at least
      ContextLineCount number of unchanged lines.
    - Between two changed lines there are at most 2x ContextLineCount number of
      unchanged lines. If there are too many, the a new block is started
    - Changed lines starts with '! ', unchanged with '  '.
    - If a part contains no changed lines, its lines can be left out
}

var
  Part1HasChangedLines: boolean;
  Part2HasChangedLines: boolean;
begin
  if (fPart1.fStream<>nil)
  and (Start1.LineNumber-ContextLineCount<=fPart1.fEndLine-1)
  and (Start2.LineNumber-ContextLineCount<=fPart2.fEndLine-1) then begin
    // append the new difference
  end else begin
    // start a new block    // StartContextBlock(Start1,Start2);
    FinishOldContextBlock;
    WriteStrToStream(fDiffStream,'***************'+LineBreak);
    fPart1.Init(Start1);
    fPart2.Init(Start2);
  end;
  fPart1.fEndLine:=End1.LineNumber+ContextLineCount-1;
  fPart2.fEndLine:=End2.LineNumber+ContextLineCount-1;
  Part1HasChangedLines:=End1.LineStart>Start1.LineStart;
  Part2HasChangedLines:=End2.LineStart>Start2.LineStart;
  fPart1.Write2(Start1,End1,Part2HasChangedLines,'-');
  fPart2.Write2(Start2,End2,Part1HasChangedLines,'+');
end;

function TDiffOutput.GetProgressMax: Integer;
begin
  Result := Length(fText1); // + Length(fText2);
end;

constructor TDiffOutput.Create(const aText1, aText2: string; aFlags: TTextDiffFlags);
begin
  fText1:=aText1;
  fText2:=aText2;
  fFlags:=aFlags;
  fOutputType:=tdoContext;          // Default OutputType, can be changed later
  fDiffStream:=TMemoryStream.Create;
  fPart1:=TDiffPart.Create(Self, fText1);
  fPart2:=TDiffPart.Create(Self, fText2);
end;

destructor TDiffOutput.Destroy;
begin
  if Assigned(OnProgressPos) then
    OnProgressPos(0);
  fPart2.Free;
  fPart1.Free;
  fDiffStream.Free;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
procedure InternalInit;
var
  c: char;
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

