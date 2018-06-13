{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditSearch.pas, released 2000-04-07.

The Original Code is based on the mwEditSearch.pas file from the mwEdit
component suite by Martin Waldenburg and other developers.
Portions created by Martin Waldenburg are Copyright 1999 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditSearch;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, RegExpr,
  // LCL
  LCLIntf, LCLType,
  // LazUtils
  LazUTF8, LazLoggerBase,
  // SynEdit
  SynEditMiscProcs, SynEditTypes;

procedure MakeCompTable;

type
  TByteArray256 = array[#0..#255] of Byte;
  PByteArray256 = ^TByteArray256;

  TSynEditSearchResult = class
  public
    Start: integer;
    Len: integer;
    Replace: string;
    constructor Create(NewStart, NewLen: integer; const NewReplace: string);
  end;

  { TSynEditSearch }

  TSynEditSearch = class(TObject)
  private
    Run: PChar;
    Origin: PChar;
    TheEnd: PChar;
    Pat: string;
    fCount: Integer;
    fTextLen: Integer;
    Look_At: Integer;
    PatLen, PatLenPlus: Integer;
    Shift: array[0..255] of Integer;
    fSensitive: Boolean;
    fWhole: Boolean;
    fResults: TList;
    fShiftInitialized: boolean;
    FIdentChars: TSynIdentChars;
    FoundLen: integer;
    RegExprEngine : TRegExpr;
    fRegExpr: Boolean;
    fRegExprMultiLine: boolean;
    fRegExprReplace: string;
    fReplacement: string;
    FBackwards: boolean;
    CompTable: PByteArray256;
    function GetResultLen(Index: integer): integer;
    procedure SetRegExpr(const NewValue: boolean);
    function GetFinished: Boolean;
    function GetResult(Index: integer): integer;
    function GetResultCount: integer;
    procedure InitShiftTable;
    procedure SetPattern(const Value: string);
    procedure SetSensitive(const Value: Boolean);
  protected
    function TestWholeWord: boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function FindAll(const NewText: string): integer;
    function FindFirstUTF8(const NewText: string): Integer;
    procedure FixResults(First, Delta: integer);
    function Next: Integer;
    function FindNextOne(Lines: TStrings; StartPos, EndPos: TPoint;
                         out FoundStartPos, FoundEndPos: TPoint; ASupportUnicodeCase: Boolean=False): boolean;
    property Count: Integer read fCount write fCount;
    property Finished: Boolean read GetFinished;
    property Pattern: string read Pat write SetPattern;// search for this text
    property Results[Index: integer]: integer read GetResult;
    property ResultCount: integer read GetResultCount;
    property Sensitive: Boolean read fSensitive write SetSensitive;// case sensitive
    property Whole: Boolean read fWhole write fWhole;// whole words
  public
    procedure ClearResults;
    procedure ResetIdentChars;
    function GetReplace(Index: integer): string;
    property RegularExpressions: Boolean read fRegExpr write SetRegExpr;
    property ResultLengths[Index: integer]: integer read GetResultLen;
    property RegExprMultiLine: Boolean read fRegExprMultiLine
                                        write fRegExprMultiLine;
    property RegExprReplace: string read fRegExprReplace write fRegExprReplace;
    property Replacement: string read fReplacement write fReplacement;
    property Backwards: boolean read FBackwards write FBackwards;
    property IdentChars: TSynIdentChars read FIdentChars write FIdentChars;
  end;

function GetLineCountOfString(const aText: string): integer;
function GetLastLineLength(const aText: string): integer;
function AdjustPositionAfterReplace(const p, ReplaceStart, ReplaceEnd: TPoint;
  const AReplacement: string): TPoint;

implementation

var
  CompTableSensitive: TByteArray256;
  CompTableNoneSensitive: TByteArray256;

procedure MakeCompTable;
var
  I: Char;
begin
  for I := #0 to #255 do CompTableSensitive[I] := ord(I);
  for I := #0 to #255 do CompTableNoneSensitive[I] := ord(uppercase(I)[1]);
end;

function GetLineCountOfString(const aText: string): integer;
// returns the number of line separators
var
  i: Integer;
begin
  Result:=0;
  i:=1;
  while i<=length(aText) do begin
    if aText[i] in [#10,#13] then begin
      inc(Result);
      inc(i);
      if (i<=length(aText)) and (aText[i] in [#10,#13])
      and (aText[i]<>aText[i-1]) then
        inc(i);
    end else
      inc(i);
  end;
end;

function GetLastLineLength(const aText: string): integer;
// returns the length of the last line of aText
// = the number of characters at end other than #10,#13
var
  p: Integer;
begin
  Result:=0;
  p:=length(aText);
  while (p>0) and (not (aText[p] in [#10,#13])) do
    dec(p);
  Result:=length(aText)-p;
end;

function AdjustPositionAfterReplace(const p, ReplaceStart, ReplaceEnd: TPoint;
  const AReplacement: string): TPoint;
// p is the position before replacing a block of text and Result is
// the same position after the replacement.
// if p is right or below of ReplaceEnd, then adjust Result accordingly
var
  aLineCount: LongInt;
begin
  Result:=p;
  if (Result.y>ReplaceEnd.y) then begin
    Result.y:=Result.y-(ReplaceEnd.y-ReplaceStart.y)
                       +GetLineCountOfString(AReplacement);
  end
  else if (Result.y=ReplaceEnd.y) and (Result.x>=ReplaceEnd.x) then begin
    // cursor in last line of replacement
    aLineCount:=GetLineCountOfString(AReplacement);
    Result.Y:=ReplaceStart.Y+aLineCount;
    if ReplaceStart.Y=ReplaceEnd.Y then begin
      if aLineCount=0 then begin
        // replace word with word
        Result.X:=Result.X-(ReplaceEnd.X-ReplaceStart.X)
                           +length(AReplacement);
      end else begin
        // replace word with lines
        Result.X:=1+GetLastLineLength(AReplacement)+(Result.X-ReplaceEnd.X);
      end;
    end else begin
      if aLineCount=0 then begin
        // replace lines with word
        Result.X:=ReplaceStart.X+length(AReplacement)+(Result.X-ReplaceEnd.X);
      end else begin
        // replace lines with lines
        Result.X:=1+GetLastLineLength(AReplacement)+(Result.X-ReplaceEnd.X);
      end;
    end;
  end;
end;

{ TSynEditSearch }

constructor TSynEditSearch.Create;
begin
  inherited Create;
  fSensitive := False;
  CompTable := @CompTableNoneSensitive;
  fResults := TList.Create;
  RegExprEngine:=TRegExpr.Create;
  ResetIdentChars;
end;

function TSynEditSearch.GetFinished: Boolean;
begin
  Result := (Run >= TheEnd) or (PatLen >= fTextLen);
end;

function TSynEditSearch.GetResult(Index: integer): integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < fResults.Count) then
    Result := TSynEditSearchResult(fResults[Index]).Start;
end;

function TSynEditSearch.GetResultCount: integer;
begin
  Result := fResults.Count;
end;

procedure TSynEditSearch.FixResults(First, Delta: integer);
var
  i: integer;
begin
  if (Delta <> 0) and (fResults.Count > 0) then begin
    i := Pred(fResults.Count);
    while i >= 0 do begin
      if GetResult(i)>=First then begin
        dec(TSynEditSearchResult(fResults[i]).Start,Delta);
        if GetResult(i)<First then begin
          // search result overlaps with deletion -> delete this result
          TSynEditSearchResult(fResults[i]).Free;
          fResults.Delete(i);
        end;
      end;
      Dec(i);
    end;
  end;
end;

procedure TSynEditSearch.InitShiftTable;
var
  I: Byte;
begin
  PatLen := Length(Pat);
  if Patlen = 0 then raise Exception.Create('Pattern is empty');
  PatLenPlus := PatLen + 1;
  Look_At := 1;
  for I := 0 to 255 do Shift[I] := PatLenPlus;
  for I := 1 to PatLen do Shift[CompTable^[Pat[i]]] := PatLenPlus - I;
  while Look_at < PatLen do
  begin
    if CompTable^[Pat[PatLen]] = CompTable^[Pat[PatLen - (Look_at)]] then exit;
    inc(Look_at);
  end;
  fShiftInitialized := TRUE;
end;

function TSynEditSearch.TestWholeWord: boolean;
var
  Test: PChar;
begin
  Test := Run - PatLen;
  Result := ((Test < Origin) or (not (Test[0] in FIdentChars))) and
    ((Run >= TheEnd) or (not (Run[1] in FIdentChars)));
end;

function TSynEditSearch.Next: Integer;
var
  I: Integer;
  J: PChar;
begin
  Result := 0;
  if not fRegExpr then begin
    inc(Run, PatLen);
    FoundLen:=PatLen;
    while Run < TheEnd do
    begin
      if CompTable^[Pat[Patlen]] <> CompTable^[Run^] then
        inc(Run, Shift[CompTable^[(Run + 1)^]])
      else
      begin
        J := Run - PatLen + 1;
        I := 1;
        while CompTable^[Pat[I]] = CompTable^[J^] do
        begin
          if I = PatLen then
          begin
            Case fWhole of
              True: if not TestWholeWord then break;
            end;
            inc(fCount);
            Result := Run - Origin - Patlen + 2;
            exit;
          end;
          inc(I);
          inc(J);
        end;
{begin}                                                                         //mh 2000-08-29
//        inc(Run, Look_At + Shift[CompTable^[(Run + Look_at)^]] - 1);
        Inc(Run, Look_At);
        if Run >= TheEnd then
          break;
        Inc(Run, Shift[CompTable^[Run^]] - 1);
{end}                                                                           //mh 2000-08-29
      end;
    end;
  end else begin
    // regular expressions
    inc(Run);
    if RegExprEngine.ExecPos(Run-Origin+1) then begin
      Result:=RegExprEngine.MatchPos[0];
      FoundLen:=RegExprEngine.MatchLen[0];
      Run:=Origin+Result-1;
    end else begin
      Result:=0;
      FoundLen:=0;
      Run:=TheEnd;
    end;
  end;
end;

// ASupportUnicodeCase -> If we will support Unicode lowercase/uppercase
//   by default this is off to increase the speed of the routine
function TSynEditSearch.FindNextOne(Lines: TStrings; StartPos, EndPos: TPoint;
  out FoundStartPos, FoundEndPos: TPoint; ASupportUnicodeCase: Boolean=False): boolean;
// Note: all points are 1 based
// only local variables are 0 based
var
  x: LongInt; // 0 based
  y: LongInt; // 0 based
  MaxY: LongInt;// 0 based
  Line: PChar;
  LineLen: Integer;
  LineStr: string;
  SearchFor: PChar;
  SearchLen: Integer;
  MinY: LongInt;
  IsFirstLine: boolean;
  SearchLineEndPos: LongInt;
  FirstPattern: String;
  MaxPos: Integer;
  xStep: Integer;
  IsMultiLinePattern: Boolean;

  procedure FixRange;
  var
    aLine: string;
  begin
    if StartPos.Y<1 then
      StartPos:=Point(1,1)
    else if StartPos.Y>Lines.Count then
      StartPos:=Point(1,Lines.Count+1)
    else if StartPos.X<1 then
      StartPos.X:=1
    else begin
      aLine:=Lines[StartPos.Y-1];
      if StartPos.X>length(aLine) then
        StartPos:=Point(1,StartPos.Y+1);
    end;
    if CompareCarets(StartPos,EndPos)<0 then
      EndPos:=StartPos
    else if EndPos.Y>Lines.Count then
      EndPos:=Point(1,Lines.Count+1)
    else if EndPos.X<1 then
      EndPos.X:=1
    else begin
      aLine:=Lines[EndPos.Y-1];
      if EndPos.X>length(aLine) then
        EndPos:=Point(1,EndPos.Y+1);
    end;
  end;

  function FindNextPatternLineEnd(const s: string; StartPos: integer): integer;
  begin
    Result:=StartPos;
    while (Result<=length(s)) and (not (s[Result] in [#10,#13])) do inc(Result);
  end;

  function FindPrevPatternLineEnd(const s: string; StartPos: integer): integer;
  begin
    Result:=StartPos;
    while (Result>=1) and (not (s[Result] in [#10,#13])) do dec(Result);
  end;

  function SearchRegExprInLine(p: integer; const Line: string): boolean;
  var
    FoundPos: LongInt;
    l,r,m: LongInt;
  begin
    Result:=false;
    //DebugLn(['SearchRegExprInLine p=',p,' Line="',Line,'" ']);
    if not FBackwards then begin
      // forward search
      RegExprEngine.InputString:=LineStr;
      if RegExprEngine.ExecPos(p) then exit(true);
    end else begin
      // backward search
      RegExprEngine.InputString:=copy(LineStr,1,p);
      // RegExprEngine can only search forward
      if not RegExprEngine.ExecPos(1) then begin
        // the line does not contain the pattern
        //DebugLn(['SearchRegExprInLine backwards: not found']);
        exit;
      end;
      // the line contains the pattern
      Result:=true;
      FoundPos:=RegExprEngine.MatchPos[0];
      // now search the last with binary search
      l:=FoundPos;
      r:=p;
      while (l<r) do begin
        m:=(l+r) div 2;
        if m=l then break;
        //DebugLn(['SearchRegExprInLine l=',l,' m=',m,' r=',r]);
        if RegExprEngine.ExecPos(m) then begin
          // found the pattern nearer to p
          FoundPos:=RegExprEngine.MatchPos[0];
          // search nearer to p
          l:=FoundPos+1;
        end else begin
          // pattern not found => search further from p
          r:=m;
        end;
      end;
      // move the RegExprEngine to FoundPos
      RegExprEngine.ExecPos(FoundPos);
    end;
  end;

  function CompareContent(p1, p2: PChar; Count: integer): boolean;
  begin
    while Count>0 do begin
      if fSensitive then begin
        if p1^<>p2^ then exit(false);
      end else begin
        if CompTable^[p1^]<>CompTable^[p2^] then exit(false);
      end;
      inc(p1);
      inc(p2);
      dec(Count);
    end;
    Result:=true;
  end;

  function WholeWordAtStartFits: boolean;
  var
    CurLine: string;
  begin
    if (not fWhole) then
      Result:=true
    else begin
      // check for word boundary
      if FoundStartPos.X<1 then
        Result:=true
      else if (FoundStartPos.y=y+1) then begin
        Result:=not (Line[FoundStartPos.X] in FIdentChars);// line is PChar
      end else if FoundStartPos.y>0 then begin
        CurLine:=Lines[FoundStartPos.Y-1];
        Result:=not (CurLine[FoundStartPos.X-1] in FIdentChars); // CurLine is string
      end else
        Result:=true;
    end;
  end;

  function WholeWordAtEndFits: boolean;
  var
    CurLine: string;
  begin
    if (not fWhole) then
      Result:=true
    else begin
      // check for word boundary
      if (FoundEndPos.y=y+1) then begin
        //DebugLn(['WholeWordAtEndFits Line="',Line,'" FoundEndPos=',dbgs(FoundEndPos),' Line[FoundEndPos.X-1]=',Line[FoundEndPos.X-1]]);
        Result:=(FoundEndPos.X>LineLen)
                or (not (Line[FoundEndPos.X-1] in FIdentChars));// Line is PChar
      end else if FoundEndPos.y<=Lines.Count then begin
        CurLine:=Lines[FoundEndPos.Y-1];
        //DebugLn(['WholeWordAtEndFits CurLine="',CurLine,'" FoundEndPos=',dbgs(FoundEndPos)]);
        Result:=(FoundEndPos.X>length(CurLine))
                or (not (CurLine[FoundEndPos.X] in FIdentChars));// CurLine is string
      end else
        Result:=true;
    end;
  end;

  function MultiLinePatternFits: boolean;
  // check the rest of the multi line pattern
  var
    LineStartPos: LongInt;// 1 based, first character
    LineEndPos: LongInt;// 1 based, last character
    CurY: LongInt;
    CurLineStr: string;
    CompareStartPos: Integer;
    CompareEndPos: Integer;
  begin
    //DebugLn(['MultiLinePatternFits FBackwards=',FBackwards,' y=',y,' FirstPattern="',FirstPattern,'"']);
    Result:=false;
    if FBackwards then begin
      // backwards
      if x>0 then begin
        //DebugLn(['MultiLinePatternFits Backwards: last pattern line not found at start of line']);
        exit;
      end;
      if not WholeWordAtEndFits then begin
        //DebugLn(['MultiLinePatternFits Backwards: end pos not word boundary']);
        exit;
      end;
      //SearchLineEndPos points at the LAST char of LineEnding
      LineStartPos:=SearchLineEndPos+1;
      LineEndPos:=length(Pat);
    end else begin
      // forwards
      if x<MaxPos then begin
        //DebugLn(['MultiLinePatternFits Forwards: first pattern line not found at end of line']);
        exit;
      end;
      // Note: word boundary at start was already checked
      //SearchLineEndPos points at the FIRST char of LineEnding
      LineStartPos:=1;
      LineEndPos:=SearchLineEndPos-1;
    end;
    CurY:=Y;

    // next line
    repeat
      if FBackwards then begin
        // search backwards
        dec(CurY);
        if CurY<0 then exit;
        LineEndPos:=LineStartPos-1;
        if (LineEndPos>=length(LineEnding)) and
           CompareMem(@Pat[LineEndPos+1-length(LineEnding)], PChar(LineEnding), length(LineEnding))
          then
          dec(LineEndPos, length(LineEnding));
        CurLineStr:=Lines[CurY];
        if LineEndPos = 0 then begin // match empty string
          FoundStartPos:=Point(length(CurLineStr)+1,CurY+1);
          exit(WholeWordAtStartFits);
        end;
        LineStartPos:=FindPrevPatternLineEnd(Pat,LineEndPos)+1;
        //DebugLn(['MultiLinePatternFits Backward: CurLineStr="',CurLineStr,'" CurPattern="',copy(Pat,LineStartPos,LineEndPos-LineStartPos+1),'"']);
        CompareStartPos:=length(CurLineStr)-(LineEndPos-LineStartPos);
        if CompareStartPos<1 then
          exit; // line too short
        //DebugLn(['MultiLinePatternFits Backward: not too short']);
        if (LineStartPos>1) and (CompareStartPos>1) then
          exit; // line too long
        //DebugLn(['MultiLinePatternFits Backward: not too long']);
        CompareEndPos:=LineEndPos-LineStartPos+1;
        if (CurLineStr<>'')
        and (not CompareContent(@CurLineStr[CompareStartPos],@Pat[LineStartPos],
                                CompareEndPos))
        then
          exit;// pattern not found
        //DebugLn(['MultiLinePatternFits Backward: current line fits']);
        if LineStartPos<=1 then begin
          // whole pattern fits
          FoundStartPos:=Point(CompareStartPos,CurY+1);
          //DebugLn(['MultiLinePatternFits Backwards: SUCCESS']);
          exit(WholeWordAtStartFits);
        end;
      end else begin
        // search forwards
        inc(CurY);
        if CurY>=Lines.Count then begin
          //DebugLn(['MultiLinePatternFits end of lines reached']);
          exit;
        end;
        LineStartPos:=LineEndPos+1;
        if (LineStartPos>0) and (LineStartPos+Length(LineEnding)-1<=length(Pat)) and
           CompareMem(@Pat[LineStartPos], PChar(LineEnding), length(LineEnding))
          then
          inc(LineStartPos, length(LineEnding));
        if (LineStartPos > length(Pat)) then begin // Empty string
          FoundEndPos:=Point(1,CurY+1);
          exit(WholeWordAtEndFits);
        end;
        LineEndPos:=FindNextPatternLineEnd(Pat,LineStartPos)-1;
        CurLineStr:=Lines[CurY];
        //DebugLn(['MultiLinePatternFits Forward: CurLineStr="',CurLineStr,'" CurPattern="',copy(Pat,LineStartPos,LineEndPos-LineStartPos+1),'"']);
        CompareEndPos:=LineEndPos-LineStartPos+1;
        if CompareEndPos>length(CurLineStr) then begin
          //DebugLn(['MultiLinePatternFits Forward: line too short']);
          exit; // line too short
        end;
        If (CompareEndPos <> length(CurLineStr)) and
           (LineEndPos+1+Length(LineEnding) <= length(Pat)) and
           CompareMem(@Pat[LineEndPos+1], PChar(LineEnding), length(LineEnding))
        then
          exit; // Not last line, and not same length
        if (not CompareContent(PChar(CurLineStr),@Pat[LineStartPos],
                                CompareEndPos))
        then begin
          //DebugLn(['MultiLinePatternFits Forward: line mismatches']);
          exit;// pattern not found
        end;
        if LineEndPos>=length(Pat) then begin
          // whole pattern fits
          FoundEndPos:=Point(CompareEndPos+1,CurY+1);
          //DebugLn(['MultiLinePatternFits Forwards: SUCCESS']);
          exit(WholeWordAtEndFits);
        end;
      end;
    until false;
  end;

  function GetTextRange: string;
  // get the search text range as one string
  // returns whole lines. That means if StartPos start in the middle of a line
  // it still returns the whole line. This is needed for the regular expression
  // search to know if it is the start of the line. Same for EndPos.
  var
    CurLine: string;
    NeededLen: Integer;
    e: string;
    p: Integer;
    CopyLen: Integer;
    i: Integer;
  begin
    //DebugLn(['GetTextRange StartPos=',dbgs(StartPos),' EndPos=',dbgs(EndPos)]);
    //DebugLn(Lines.Text);

    if EndPos.Y<StartPos.Y then begin
      Result:='';
      exit;
    end;
    CurLine:=Lines[StartPos.Y-1];
    if StartPos.y=EndPos.Y then
      Result:=CurLine
    else begin
      // multi line
      // first calculate needed space
      NeededLen:=0;
      for i:=MinY to MaxY do
        inc(NeededLen,length(Lines[i]));
      e:=LineEnding;
      inc(NeededLen,length(e)*(MaxY-MinY+1));
      // copy lines
      SetLength(Result,NeededLen);
      p:=1;
      // copy middle lines
      //DebugLn(['GetTextRange MinY=',MinY,' MaxY=',MaxY]);
      for i:=MinY to MaxY do begin
        CurLine:=Lines[i];
        CopyLen:=length(CurLine);
        if CopyLen>0 then
          System.Move(CurLine[1],Result[p],CopyLen);
        inc(p,CopyLen);
        System.Move(e[1],Result[p],length(e));
        inc(p,length(e));
      end;
      // check
      if p-1<>length(Result) then
        RaiseGDBException('inconsistency');
    end;
  end;

  function PosToLineCol(const s: string; const Offset: TPoint; p: integer
    ): TPoint;
  var
    i: Integer;
  begin
    Result:=Offset;
    i:=1;
    while i<p do begin
      if s[i] in [#10,#13] then begin
        inc(Result.y);
        Result.X:=1;
        inc(i);
        if (i<p) and (s[i] in [#10,#13]) and (s[i]<>s[i-1]) then inc(i);
      end else begin
        inc(Result.X);
        inc(i);
      end;
    end;
  end;

  function SearchRegExprMultiLine: boolean;
  var
    s: String;
    Offset: TPoint;
  begin
    if FBackwards then
      raise Exception.Create('not implemented yet: searching backwards multiple lines with regular expressions');
    s:=GetTextRange;
    //DebugLn(['SearchRegExprMultiLine s="',DbgStr(s),'"']);
    RegExprEngine.ModifierM:=true;
    RegExprEngine.InputString:=s;
    Result:=RegExprEngine.ExecPos(StartPos.X);
    if not Result then exit;
    Offset:=Point(1,StartPos.Y);
    // regular expression found
    FoundStartPos:=PosToLineCol(s,Offset,RegExprEngine.MatchPos[0]);
    FoundEndPos:=PosToLineCol(s,Offset,
                           RegExprEngine.MatchPos[0]+RegExprEngine.MatchLen[0]);
    fRegExprReplace:=RegExprEngine.Substitute(Replacement);
  end;

  function CheckFound: boolean;
  begin
    if ((not IsMultiLinePattern) and WholeWordAtEndFits)
    or MultiLinePatternFits then begin
      // the whole pattern fits
      Result:=(CompareCarets(FoundEndPos,EndPos)>=0)
             and (CompareCarets(FoundStartPos,StartPos)<=0);
      //DebugLn(['CheckFound Found=',dbgs(FoundStartPos),'..',dbgs(FoundEndPos),' Range=',dbgs(StartPos),'..',dbgs(EndPos)]);
    end else
      Result:=false;
  end;

var
  i: integer;
  SearchForStr: string;
  FCondition: Boolean;
begin
  Result:=false;
  if Pattern='' then exit;
  if Lines.Count=0 then begin
    //DebugLn(['TSynEditSearch.FindNextOne Lines.Count=0']);
    exit;
  end;
  FixRange;
  if StartPos.Y>Lines.Count then begin
    //DebugLn(['TSynEditSearch.FindNextOne StartPos.Y>Lines.Count']);
    exit;
  end;
  MinY:=Max(0,StartPos.Y-1);
  MaxY:=Min(Lines.Count,EndPos.Y-1);
  if MinY>MaxY then exit;
  if Backwards then begin
    // backwards
    y:=MaxY;
    IsFirstLine:=(MaxY=EndPos.Y-1);
    xStep:=-1;
  end else begin
    // forwards
    y:=MinY;
    IsFirstLine:=(MinY=StartPos.Y-1);
    xStep:=1;
  end;

  IsMultiLinePattern:=fRegExpr and fRegExprMultiLine;
  SearchLineEndPos:=FindNextPatternLineEnd(Pat,1);
  if SearchLineEndPos>length(Pat) then begin
    // normal pattern
    if Pat='' then exit;
    FirstPattern:=Pat;
  end else begin
    // multi line pattern
    IsMultiLinePattern:=true;
    if FBackwards then begin
      SearchLineEndPos:=FindPrevPatternLineEnd(Pat,length(Pat));
      FirstPattern:=copy(Pat,SearchLineEndPos+1,length(Pat));
    end else
      FirstPattern:=copy(Pat,1,SearchLineEndPos-1);
  end;
  if ASupportUnicodeCase then
  begin
    SearchForStr := FirstPattern;
    if not fSensitive then SearchForStr := UTF8LowerCase(SearchForStr);
    SearchFor:=PChar(SearchForStr);
    SearchLen:=Length(SearchForStr);
  end
  else
  begin
    SearchFor:=PChar(FirstPattern);
    SearchLen:=length(FirstPattern);
  end;

  if fRegExpr then begin
    RegExprEngine.ModifierI:=not fSensitive;
    RegExprEngine.ModifierM:=IsMultiLinePattern;
    if Whole then
      RegExprEngine.Expression:='\b'+Pat+'\b'
    else
      RegExprEngine.Expression:=Pat;
    if IsMultiLinePattern then begin
      Result:=SearchRegExprMultiLine;
      exit;
    end;
  end;

  //DebugLn(['TSynEditSearch.FindNextOne IsMultiLinePattern=',IsMultiLinePattern,' RegExpr=',fRegExpr,' Sensitive=',Sensitive,' StartPos=',dbgs(StartPos),' EndPos=',dbgs(EndPos)]);

  // Working: case sensitive, backwards, whole word, regex whole word,
  //          multi line pattern forward, multi line pattern backward
  //          regex case insensitive, regex multi line forward,
  //          regex multi line whole word

  if fRegExpr then begin
    // ************ RegExp ************
    // prepare for firs iteration
    LineStr:=Lines[y];
    LineLen:=length(LineStr);
    Line:=PChar(LineStr);
    if IsFirstLine then begin
      if FBackwards then
        x:=EndPos.X-2
      else
        x:=StartPos.X-1;
    end else begin
      if FBackwards then
        x:=LineLen-1
      else
        x:=0;
    end;
    x:=MinMax(x,0,LineLen-1);
    //DebugLn(['TSynEditSearch.FindNextOne Line="',LineStr,'" x=',x,' LineLen=',LineLen]);

    repeat
      // search in the line
      if SearchRegExprInLine(Max(1,x+1),LineStr) then begin
        //DebugLn(['TSynEditSearch.FindNextOne Found RegExpr']);
        FoundStartPos:=Point(RegExprEngine.MatchPos[0],y+1);
        FoundEndPos:=
                 Point(RegExprEngine.MatchPos[0]+RegExprEngine.MatchLen[0],y+1);
        Result:=(CompareCarets(FoundEndPos,EndPos)>=0)
               and (CompareCarets(FoundStartPos,StartPos)<=0);
        if Result then
          fRegExprReplace:=RegExprEngine.Substitute(Replacement);
        exit;
      end;

      // next line
      if FBackwards then
        dec(y)
      else
        inc(y);

      LineStr:=Lines[y];
      LineLen:=length(LineStr);
      Line:=PChar(LineStr);
      if FBackwards then
        x:=LineLen-1
      else
        x:=0;
      x:=MinMax(x,0,LineLen-1);
      //DebugLn(['TSynEditSearch.FindNextOne Line="',LineStr,'" x=',x,' LineLen=',LineLen]);
    until (y<MinY) or (y>MaxY);

  end // fRegExpr
  else begin
    // ************ NOT RegExp ************
    // prepare for firs iteration
    LineStr:=Lines[y];
    if ASupportUnicodeCase and (not fSensitive) then LineStr := UTF8LowerCase(LineStr);
    LineLen:=length(LineStr);
    Line:=PChar(LineStr);
    if IsFirstLine then begin
      if FBackwards then
        x:=EndPos.X-SearchLen-1
      else
        x:=StartPos.X-1;
      if IsMultiLinePattern then begin
        if FBackwards then
          x:=Min(x, 0)   // keep negative to indicate pattern does not fit into line
        else
          x:=Max(x, LineLen-SearchLen)  // keep higher x, if pattern does not fit
      end;
    end else begin
      if FBackwards xor IsMultiLinePattern then
        x:=LineLen-SearchLen
      else
        x:=0;
    end;
    //DebugLn(['TSynEditSearch.FindNextOne Line="',LineStr,'" x=',x,' LineLen=',LineLen]);

    repeat
      // search in the line
      MaxPos:=LineLen-SearchLen;
      if (SearchLen=0) and ((LineLen=0) or IsMultiLinePattern) then
      begin
        // first (last if backwards) line of pattern is empty line
        if FBackwards then begin
          FoundStartPos:=Point(1,y+1);
          x:=0; // FoundStartPos.x-1;
        end else begin
          FoundStartPos:=Point(LineLen+1,y+1);
          x:=MaxPos;  //FoundStartPos.x-1;
        end;
        FoundEndPos:=FoundStartPos;
        if CheckFound then exit(true);
      end else
      if (x >= 0) and (x+SearchLen <= LineLen) then // otherwise searchterm does not fit in this line
      begin
        //DebugLn(['TSynEditSearch.FindNextOne x=',x,' MaxPos=',MaxPos,' Line="',Line,'"']);
        while (x>=0) and (x<=MaxPos) do begin
          //DebugLn(['TSynEditSearch.FindNextOne x=',x]);
          if ASupportUnicodeCase then FCondition := (SearchLen=0) or (Line[x]=SearchFor^)
          else FCondition := (SearchLen=0) or (CompTable^[Line[x]]=CompTable^[SearchFor^]);
          if FCondition then
          begin
            //DebugLn(['TSynEditSearch.FindNextOne First character found x=',x,' Line[x]=',Line[x]]);
            if (not fWhole) or (x=0) or (not (Line[x-1] in FIdentChars)) then
            begin
              i:=1;

              if ASupportUnicodeCase then
              begin
                while (i<SearchLen) and (Line[x+i]=SearchFor[i]) do
                  inc(i);
              end
              else
              begin
                while (i<SearchLen) and (CompTable^[Line[x+i]]=CompTable^[SearchFor[i]]) do
                  inc(i);
              end;

              //DebugLn(['TSynEditSearch.FindNextOne x=',x,' SearchLen=',SearchLen,' i=',i]);
              if i=SearchLen then begin
                // the pattern fits to this position
                FoundStartPos:=Point(x+1,y+1);
                FoundEndPos:=Point(x+i+1,y+1);
                if CheckFound then begin
                  //DebugLn(['TSynEditSearch.FindNextOne CheckFound success']);
                  exit(true);
                end else begin
                  //DebugLn(['TSynEditSearch.FindNextOne CheckFound failed']);
                end;
              end;
            end;
          end;
          inc(x,xStep);
        end;
      end;

      // next line
      if FBackwards then
        dec(y)
      else
        inc(y);

      LineStr:=Lines[y];
      if ASupportUnicodeCase and (not fSensitive) then LineStr := UTF8LowerCase(LineStr);
      LineLen:=length(LineStr);
      Line:=PChar(LineStr);
      // IsMultiLinePattern must be at other end of line (continues in next/prev line)
      if FBackwards xor IsMultiLinePattern then
        x:=LineLen-SearchLen
      else
        x:=0;
      //DebugLn(['TSynEditSearch.FindNextOne Line="',LineStr,'" x=',x,' LineLen=',LineLen]);
    until (y<MinY) or (y>MaxY);

  end; // NOT RegExp
end;

destructor TSynEditSearch.Destroy;
begin
  ClearResults;
  RegExprEngine.Free;
  fResults.Free;
  inherited Destroy;
end;

procedure TSynEditSearch.SetPattern(const Value: string);
begin
  if Pat <> Value then begin
    Pat := Value;
    fShiftInitialized := FALSE;
    PatLen:=length(Pat);
  end;
  fCount := 0;
end;

procedure TSynEditSearch.SetSensitive(const Value: Boolean);
begin
  if fSensitive <> Value then begin
    fSensitive := Value;
    if fSensitive
    then CompTable := @CompTableSensitive
    else CompTable := @CompTableNoneSensitive;
    fShiftInitialized := FALSE;
    RegExprEngine.ModifierI:=not fSensitive;
  end;
end;

function TSynEditSearch.FindAll(const NewText: string): integer;
var
  Found: integer;
  TheReplace: string;
begin
  if not fShiftInitialized then
    InitShiftTable;
  ClearResults;
  Found := FindFirstUTF8(NewText);
  while Found > 0 do
  begin
    TheReplace:=Replacement;
    if fRegExpr then
      TheReplace:=RegExprEngine.Substitute(Replacement);
    fResults.Add(TSynEditSearchResult.Create(Found,FoundLen,TheReplace));
    Found := Next;
  end;
  Result := fResults.Count;
end;

function TSynEditSearch.FindFirstUTF8(const NewText: string): Integer;
begin
  Result := 0;
  fTextLen := Length(NewText);
  if fTextLen=0 then exit;
  if fRegExpr then begin
    RegExprEngine.ModifierI:=not fSensitive;
    RegExprEngine.ModifierM:=fRegExprMultiLine;
    RegExprEngine.Expression:=Pat;
    RegExprEngine.InputString:=NewText;
  end;
  if (fTextLen >= PatLen) or fRegExpr then
  begin
    Origin := PChar(NewText);
    TheEnd := Origin + fTextLen;
    Run := (Origin - 1);
    Result := Next;
  end;
end;

procedure TSynEditSearch.ClearResults;
var
  i: Integer;
begin
  for i:=0 to fResults.Count-1 do
    TSynEditSearchResult(fResults[i]).Free;
  fResults.Clear;
end;

procedure TSynEditSearch.ResetIdentChars;
var
  c: Char;
begin
  FIdentChars:=[];
  for c := #0 to #255 do
    if IsCharAlphaNumeric(c) then
      Include(FIdentChars,c);
end;

function TSynEditSearch.GetReplace(Index: integer): string;
begin
  if (Index >= 0) and (Index < fResults.Count) then
    Result := TSynEditSearchResult(fResults[Index]).Replace
  else
    Result := Replacement;
end;

function TSynEditSearch.GetResultLen(Index: integer): integer;
begin
  if (Index>=0) and (Index<fResults.Count) then
    Result := TSynEditSearchResult(fResults[Index]).Len
  else
    Result:=FoundLen;
end;

procedure TSynEditSearch.SetRegExpr(const NewValue: boolean);
begin
  if NewValue=fRegExpr then exit;
  fRegExpr:=NewValue;
end;

{ TSynEditSearchResult }

constructor TSynEditSearchResult.Create(NewStart, NewLen: integer;
  const NewReplace: string);
begin
  Start:=NewStart;
  Len:=NewLen;
  Replace:=NewReplace;
end;

initialization
  MakeCompTable;
end.

