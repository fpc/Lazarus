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

    The differences are computed line based with the Histogram algorithm
    (as used by JGit and git diff --histogram):
    - common prefix and suffix lines are unchanged
    - in the remaining range the matching lines with the lowest number of
      occurrences in the old text are searched and extended to a region of
      equal lines. Then the ranges in front and behind are diffed the same way.
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

  TTextDiffOutputType = (
    tdoContext,
    tdoRCS
    );

  TDiffChangeKind = (
    dckNone,   // unchanged lines
    dckAdd,    // lines inserted in new text
    dckDelete, // lines removed from old text
    dckModify  // old lines replaced by new lines
    );

  { TDiffChange
    Line numbers start at 1.
    An empty range has Last=First-1, where First is the position of the
    insertion/deletion, e.g. adding two lines behind old line 3 gives
    OldFirst=4, OldLast=3, NewFirst=4, NewLast=5. }

  TDiffChange = class
  public
    Kind: TDiffChangeKind;
    OldFirst,
    OldLast,
    NewFirst,
    NewLast: integer; // 1-based line number
    function AsString: string; // e.g. 'Add 4-3 4-5'
  end;

  TDiffLine = record
    StartPos: integer;  // position of first char
    EndPos: integer;    // position of line end chars
    NextStart: integer; // position behind line end chars
  end;
  TDiffLines = array of TDiffLine;

  { TDiffOutput }

  TDiffOutput = class
  private
    fText1, fText2: string;
    fOutputType: TTextDiffOutputType;
    fFlags: TTextDiffFlags;
    fChanges: TFPList; // list of TDiffChange
    fComputed: boolean;
    fLines1, fLines2: TDiffLines;
    function GetChanges(Index: integer): TDiffChange;
    function GetCount: integer;
    function GetKindCount(Index: integer): integer;
    function IsEmptyLine(const Src: string; const Line: TDiffLine): boolean;
    function GetLineKey(const Src: string; const Line: TDiffLine): string;
    procedure AddChange(aKind: TDiffChangeKind; aOldFirst, aOldLast, aNewFirst, aNewLast: integer);
    procedure WriteLines(Stream: TStream; const Prefix: string; const Src: string;
      const Lines: TDiffLines; First, Last: integer);
    procedure WriteRCSDiff(Stream: TStream);
    procedure WriteContextDiff(Stream: TStream);
  public
    constructor Create(const aText1, aText2: string; aFlags: TTextDiffFlags);
    destructor Destroy; override;
    procedure Clear;
    procedure Compute;
    function CreateTextDiff: string;
  public
    property Text1: string read fText1;
    property Text2: string read fText2;
    property Flags: TTextDiffFlags read fFlags;
    property OutputType: TTextDiffOutputType read fOutputType write fOutputType;
    property Count: integer read GetCount;
    property Changes[Index: integer]: TDiffChange read GetChanges; default;
    // number of changes (blocks) of each kind:
    property AddCount: integer index ord(dckAdd) read GetKindCount;
    property DeleteCount: integer index ord(dckDelete) read GetKindCount;
    property ModifyCount: integer index ord(dckModify) read GetKindCount;
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

  DiffChangeKindNames: array[TDiffChangeKind] of string = (
    'None',
    'Add',
    'Delete',
    'Modify'
    );

function SplitDiffLines(const Src: string): TDiffLines;

implementation

const
  LineBreak = #10;
  ContextLineCount = 3;
  MaxChainLength = 64; // lines occurring more often are not used as anchors

type
  TIntegerArray = array of integer;

  { TDiffKeyMap - maps line keys to integer ids }

  TDiffKeyMap = class
  private
    fKeys: array of string;
    fNextKey: TIntegerArray;
    fBuckets: TIntegerArray;
    fCount: integer;
    fMask: integer;
  public
    constructor Create(Capacity: integer);
    function GetId(const Key: string): integer;
    property Count: integer read fCount;
  end;

  { THistogramDiff }

  THistogramDiff = class
  private
    OldId, NewId: TIntegerArray; // line ids
    Head: TIntegerArray;  // per id: last occurrence in current old range, -1 if none
    Cnt: TIntegerArray;   // per id: number of occurrences in current old range
    Next: TIntegerArray;  // per old line: previous occurrence of same id in range
    procedure MatchRange(aLo, aHi, bLo, bHi: integer);
    function FindRegion(aLo, aHi, bLo, bHi: integer;
      out RegA, RegB, RegLen: integer): boolean;
  public
    Match: TIntegerArray; // per old line: index of matching new line, or -1
    procedure Execute(const OldIds, NewIds: TIntegerArray; IdCount: integer);
  end;

var
  IsSpaceChars: array[char] of boolean;
  UpperCaseChars: array[char] of char;

function SplitDiffLines(const Src: string): TDiffLines;
var
  Len, LineCount, p: integer;
  Line: TDiffLine;
begin
  Result:=nil;
  Len:=length(Src);
  if Len=0 then exit;
  SetLength(Result,16);
  LineCount:=0;
  p:=1;
  while p<=Len do begin
    Line.StartPos:=p;
    while (p<=Len) and not (Src[p] in [#10,#13]) do
      inc(p);
    Line.EndPos:=p;
    if p<=Len then begin
      inc(p);
      if (p<=Len) and (Src[p] in [#10,#13]) and (Src[p]<>Src[p-1]) then
        inc(p);
    end;
    Line.NextStart:=p;
    if LineCount=length(Result) then
      SetLength(Result,LineCount*2);
    Result[LineCount]:=Line;
    inc(LineCount);
  end;
  SetLength(Result,LineCount);
end;

procedure WriteStrToStream(Stream: TStream; const s: string);
begin
  if s='' then exit;
  Stream.Write(s[1],length(s));
end;

function RangeToStr(First, Last: integer): string;
begin
  if Last>First then
    Result:=IntToStr(First)+','+IntToStr(Last)
  else
    Result:=IntToStr(Last);
end;

{ TDiffChange }

function TDiffChange.AsString: string;
begin
  Result:=DiffChangeKindNames[Kind]
    +' '+IntToStr(OldFirst)+'-'+IntToStr(OldLast)
    +' '+IntToStr(NewFirst)+'-'+IntToStr(NewLast);
end;

{ TDiffKeyMap }

constructor TDiffKeyMap.Create(Capacity: integer);
var
  Size, i: integer;
begin
  Size:=16;
  while Size<Capacity*2 do
    Size:=Size*2;
  fMask:=Size-1;
  SetLength(fBuckets,Size);
  for i:=0 to Size-1 do
    fBuckets[i]:=-1;
  SetLength(fKeys,Capacity+1);
  SetLength(fNextKey,Capacity+1);
end;

function TDiffKeyMap.GetId(const Key: string): integer;
var
  h: LongWord;
  i, Bucket: integer;
begin
  // FNV-1a
  h:=2166136261;
  for i:=1 to length(Key) do begin
    h:=h xor ord(Key[i]);
    h:=h*16777619;
  end;
  Bucket:=integer(h and LongWord(fMask));
  Result:=fBuckets[Bucket];
  while Result>=0 do begin
    if fKeys[Result]=Key then exit;
    Result:=fNextKey[Result];
  end;
  Result:=fCount;
  inc(fCount);
  if Result>=length(fKeys) then begin
    SetLength(fKeys,Result*2+1);
    SetLength(fNextKey,Result*2+1);
  end;
  fKeys[Result]:=Key;
  fNextKey[Result]:=fBuckets[Bucket];
  fBuckets[Bucket]:=Result;
end;

{ THistogramDiff }

function THistogramDiff.FindRegion(aLo, aHi, bLo, bHi: integer; out RegA,
  RegB, RegLen: integer): boolean;
var
  i, Id, b, NextB, a, As_, Bs, Ae, Be, RegCnt, BestCnt: integer;
begin
  RegA:=-1;
  RegB:=-1;
  RegLen:=0;
  // build histogram of old range
  for i:=aLo to aHi-1 do begin
    Id:=OldId[i];
    inc(Cnt[Id]);
    Next[i]:=Head[Id];
    Head[Id]:=i;
  end;
  BestCnt:=MaxChainLength+1;
  b:=bLo;
  while b<bHi do begin
    NextB:=b+1;
    Id:=NewId[b];
    if (Cnt[Id]>0) and (Cnt[Id]<=BestCnt) then begin
      a:=Head[Id];
      while a>=0 do begin
        // extend the match a=b backwards and forwards
        RegCnt:=Cnt[Id];
        As_:=a;
        Bs:=b;
        while (As_>aLo) and (Bs>bLo) and (OldId[As_-1]=NewId[Bs-1]) do begin
          dec(As_);
          dec(Bs);
          if Cnt[OldId[As_]]<RegCnt then
            RegCnt:=Cnt[OldId[As_]];
        end;
        Ae:=a+1;
        Be:=b+1;
        while (Ae<aHi) and (Be<bHi) and (OldId[Ae]=NewId[Be]) do begin
          if Cnt[OldId[Ae]]<RegCnt then
            RegCnt:=Cnt[OldId[Ae]];
          inc(Ae);
          inc(Be);
        end;
        if (RegCnt<BestCnt) or ((RegCnt=BestCnt) and (Ae-As_>RegLen)) then begin
          BestCnt:=RegCnt;
          RegA:=As_;
          RegB:=Bs;
          RegLen:=Ae-As_;
        end;
        if NextB<Be then
          NextB:=Be;
        a:=Next[a];
      end;
    end;
    b:=NextB;
  end;
  // reset histogram
  for i:=aLo to aHi-1 do begin
    Id:=OldId[i];
    Cnt[Id]:=0;
    Head[Id]:=-1;
  end;
  Result:=RegLen>0;
end;

procedure THistogramDiff.MatchRange(aLo, aHi, bLo, bHi: integer);
var
  Stack: TIntegerArray;
  StackLen, RegA, RegB, RegLen, i: integer;

  procedure Push(a1, a2, b1, b2: integer);
  begin
    if (a1>=a2) or (b1>=b2) then exit; // one side empty -> no matches
    if StackLen+4>length(Stack) then
      SetLength(Stack,length(Stack)*2+16);
    Stack[StackLen]:=a1;
    Stack[StackLen+1]:=a2;
    Stack[StackLen+2]:=b1;
    Stack[StackLen+3]:=b2;
    inc(StackLen,4);
  end;

begin
  Stack:=nil;
  StackLen:=0;
  Push(aLo,aHi,bLo,bHi);
  while StackLen>0 do begin
    dec(StackLen,4);
    aLo:=Stack[StackLen];
    aHi:=Stack[StackLen+1];
    bLo:=Stack[StackLen+2];
    bHi:=Stack[StackLen+3];
    // common prefix
    while (aLo<aHi) and (bLo<bHi) and (OldId[aLo]=NewId[bLo]) do begin
      Match[aLo]:=bLo;
      inc(aLo);
      inc(bLo);
    end;
    // common suffix
    while (aLo<aHi) and (bLo<bHi) and (OldId[aHi-1]=NewId[bHi-1]) do begin
      dec(aHi);
      dec(bHi);
      Match[aHi]:=bHi;
    end;
    if (aLo>=aHi) or (bLo>=bHi) then continue;
    if not FindRegion(aLo,aHi,bLo,bHi,RegA,RegB,RegLen) then
      continue; // no anchor -> whole range is changed
    for i:=0 to RegLen-1 do
      Match[RegA+i]:=RegB+i;
    Push(aLo,RegA,bLo,RegB);
    Push(RegA+RegLen,aHi,RegB+RegLen,bHi);
  end;
end;

procedure THistogramDiff.Execute(const OldIds, NewIds: TIntegerArray;
  IdCount: integer);
var
  i: integer;
begin
  OldId:=OldIds;
  NewId:=NewIds;
  SetLength(Match,length(OldId));
  for i:=0 to length(OldId)-1 do
    Match[i]:=-1;
  SetLength(Head,IdCount);
  SetLength(Cnt,IdCount);
  for i:=0 to IdCount-1 do begin
    Head[i]:=-1;
    Cnt[i]:=0;
  end;
  SetLength(Next,length(OldId));
  MatchRange(0,length(OldId),0,length(NewId));
end;

{ TDiffOutput }

function TDiffOutput.GetChanges(Index: integer): TDiffChange;
begin
  if not fComputed then
    Compute;
  Result:=TDiffChange(fChanges[Index]);
end;

function TDiffOutput.GetCount: integer;
begin
  if not fComputed then
    Compute;
  Result:=fChanges.Count;
end;

function TDiffOutput.GetKindCount(Index: integer): integer;
var
  i: Integer;
begin
  if not fComputed then
    Compute;
  Result:=0;
  for i:=0 to fChanges.Count-1 do
    if ord(TDiffChange(fChanges[i]).Kind)=Index then
      inc(Result);
end;

function TDiffOutput.IsEmptyLine(const Src: string; const Line: TDiffLine
  ): boolean;
var
  i: integer;
begin
  if [tdfIgnoreSpaceCharAmount,tdfIgnoreSpaceChars,tdfIgnoreHeadingSpaces,
    tdfIgnoreTrailingSpaces]*fFlags<>[] then
  begin
    for i:=Line.StartPos to Line.EndPos-1 do
      if not IsSpaceChars[Src[i]] then
        exit(false);
    Result:=true;
  end else
    Result:=Line.EndPos=Line.StartPos;
end;

function TDiffOutput.GetLineKey(const Src: string; const Line: TDiffLine
  ): string;
var
  StartPos, EndPos, i, l: integer;
  c: Char;
begin
  StartPos:=Line.StartPos;
  EndPos:=Line.EndPos;
  if [tdfIgnoreHeadingSpaces,tdfIgnoreSpaceChars]*fFlags<>[] then
    while (StartPos<EndPos) and IsSpaceChars[Src[StartPos]] do
      inc(StartPos);
  if [tdfIgnoreTrailingSpaces,tdfIgnoreSpaceChars]*fFlags<>[] then
    while (StartPos<EndPos) and IsSpaceChars[Src[EndPos-1]] do
      dec(EndPos);
  SetLength(Result,EndPos-StartPos+Line.NextStart-Line.EndPos);
  l:=0;
  i:=StartPos;
  while i<EndPos do begin
    c:=Src[i];
    inc(i);
    if IsSpaceChars[c] then begin
      if tdfIgnoreSpaceChars in fFlags then
        continue;
      if tdfIgnoreSpaceCharAmount in fFlags then begin
        while (i<EndPos) and IsSpaceChars[Src[i]] do
          inc(i);
        c:=' ';
      end;
    end else if tdfIgnoreCase in fFlags then
      c:=UpperCaseChars[c];
    inc(l);
    Result[l]:=c;
  end;
  if not (tdfIgnoreLineEnds in fFlags) then begin
    for i:=Line.EndPos to Line.NextStart-1 do begin
      inc(l);
      Result[l]:=Src[i];
    end;
  end;
  SetLength(Result,l);
end;

procedure TDiffOutput.AddChange(aKind: TDiffChangeKind; aOldFirst, aOldLast,
  aNewFirst, aNewLast: integer);
var
  Change: TDiffChange;
begin
  if (aOldLast<aOldFirst) and (aNewLast<aNewFirst) then exit;
  if fChanges.Count>0 then begin
    Change:=TDiffChange(fChanges[fChanges.Count-1]);
    if Change.Kind=aKind then begin
      // extend
      Change.OldLast:=aOldLast;
      Change.NewLast:=aNewLast;
      exit;
    end;
  end;
  Change:=TDiffChange.Create;
  Change.Kind:=aKind;
  Change.OldFirst:=aOldFirst;
  Change.OldLast:=aOldLast;
  Change.NewFirst:=aNewFirst;
  Change.NewLast:=aNewLast;
  fChanges.Add(Change);
end;

procedure TDiffOutput.Compute;
var
  Idx1, Idx2: TIntegerArray; // indices of compared lines
  Ids1, Ids2: TIntegerArray;
  KeyMap: TDiffKeyMap;
  Histogram: THistogramDiff;
  Matched: TIntegerArray; // per old line: matching new line, or -1

  procedure CollectLines(const Src: string; const Lines: TDiffLines;
    out Idx, Ids: TIntegerArray);
  var
    i, n: integer;
  begin
    SetLength(Idx,length(Lines));
    SetLength(Ids,length(Lines));
    n:=0;
    for i:=0 to length(Lines)-1 do begin
      if (tdfIgnoreEmptyLineChanges in fFlags) and IsEmptyLine(Src,Lines[i]) then
        continue;
      Idx[n]:=i;
      Ids[n]:=KeyMap.GetId(GetLineKey(Src,Lines[i]));
      inc(n);
    end;
    SetLength(Idx,n);
    SetLength(Ids,n);
  end;

  function OnlyEmptyLines(const Src: string; const Lines: TDiffLines;
    FromIndex, ToIndex: integer): boolean;
  var
    i: integer;
  begin
    for i:=FromIndex to ToIndex do
      if not IsEmptyLine(Src,Lines[i]) then
        exit(false);
    Result:=true;
  end;

  procedure AddGap(OldFirst, OldLast, NewFirst, NewLast: integer);
  // line indices starting at 0
  var
    Kind: TDiffChangeKind;
  begin
    if (OldLast<OldFirst) and (NewLast<NewFirst) then exit;
    if (tdfIgnoreEmptyLineChanges in fFlags)
        and OnlyEmptyLines(fText1,fLines1,OldFirst,OldLast)
        and OnlyEmptyLines(fText2,fLines2,NewFirst,NewLast) then
      Kind:=dckNone
    else if OldLast<OldFirst then
      Kind:=dckAdd
    else if NewLast<NewFirst then
      Kind:=dckDelete
    else
      Kind:=dckModify;
    AddChange(Kind,OldFirst+1,OldLast+1,NewFirst+1,NewLast+1);
  end;

var
  i, OldLine, NewLine, NextOld, NextNew: integer;
begin
  Clear;
  fComputed:=true;
  fLines1:=SplitDiffLines(fText1);
  fLines2:=SplitDiffLines(fText2);

  Histogram:=nil;
  KeyMap:=TDiffKeyMap.Create(length(fLines1)+length(fLines2));
  try
    CollectLines(fText1,fLines1,Idx1,Ids1);
    CollectLines(fText2,fLines2,Idx2,Ids2);
    Histogram:=THistogramDiff.Create;
    Histogram.Execute(Ids1,Ids2,KeyMap.Count);
    // map matches to lines
    SetLength(Matched,length(fLines1));
    for i:=0 to length(Matched)-1 do
      Matched[i]:=-1;
    for i:=0 to length(Idx1)-1 do
      if Histogram.Match[i]>=0 then
        Matched[Idx1[i]]:=Idx2[Histogram.Match[i]];
  finally
    Histogram.Free;
    KeyMap.Free;
  end;

  // create changes
  NextOld:=0;
  NextNew:=0;
  for OldLine:=0 to length(fLines1)-1 do begin
    NewLine:=Matched[OldLine];
    if NewLine<0 then continue;
    AddGap(NextOld,OldLine-1,NextNew,NewLine-1);
    AddChange(dckNone,OldLine+1,OldLine+1,NewLine+1,NewLine+1);
    NextOld:=OldLine+1;
    NextNew:=NewLine+1;
  end;
  AddGap(NextOld,length(fLines1)-1,NextNew,length(fLines2)-1);
end;

procedure TDiffOutput.WriteLines(Stream: TStream; const Prefix: string;
  const Src: string; const Lines: TDiffLines; First, Last: integer);
// First, Last start at 1
var
  i: integer;
begin
  for i:=First-1 to Last-1 do begin
    WriteStrToStream(Stream,Prefix);
    with Lines[i] do begin
      if NextStart>StartPos then
        Stream.Write(Src[StartPos],NextStart-StartPos);
      if NextStart=EndPos then begin
        WriteStrToStream(Stream,LineBreak);
        WriteStrToStream(Stream,'\ No newline at end');
        WriteStrToStream(Stream,LineBreak);
      end;
    end;
  end;
end;

procedure TDiffOutput.WriteRCSDiff(Stream: TStream);
{ The diff output:
    - There are three types of diffs: insertions, deletions and replacements
    - An insertion/addition starts with a
      <lines>a<lines>
      followed by the lines of text 2.
    - A deletion starts with a
      <lines>d<lines>
      followed by the lines of text 1.
    - A replacement/change starts with a
      <lines>c<lines>
      followed by the lines of text 1, followed by
      ---
      followed by the lines of text 2.
    - <lines> can be single decimal number or a range. For example:
       1
       2,3
      An empty range gives the line in front.
    - The lines of text 1 are always prefixed with '< '
      If the lines of text 1 do not end with a newline char it ends with a line
      \ No newline at end
    - The lines of text 2 are always prefixed with '> '
      If the lines of text 2 do not end with a newline char it ends with a line
      \ No newline at end
}
const
  ActionChars: array[TDiffChangeKind] of char = (' ','a','d','c');
var
  i: integer;
  Change: TDiffChange;
begin
  for i:=0 to fChanges.Count-1 do begin
    Change:=TDiffChange(fChanges[i]);
    if Change.Kind=dckNone then continue;
    WriteStrToStream(Stream,RangeToStr(Change.OldFirst,Change.OldLast)
      +ActionChars[Change.Kind]
      +RangeToStr(Change.NewFirst,Change.NewLast)+LineBreak);
    WriteLines(Stream,'< ',fText1,fLines1,Change.OldFirst,Change.OldLast);
    if Change.Kind=dckModify then
      WriteStrToStream(Stream,'---'+LineBreak);
    WriteLines(Stream,'> ',fText2,fLines2,Change.NewFirst,Change.NewLast);
  end;
end;

procedure TDiffOutput.WriteContextDiff(Stream: TStream);
{ The diff output:
    - Every diff block (hunk) starts with 15 stars
      ***************
    - Every hunk has two parts. One for each text.
    - The first text part starts with
      *** StartLine,EndLine ****
    - The second text part starts with
      --- StartLine,EndLine ----
    - In front of and behind the changed lines there are up to
      ContextLineCount unchanged lines. These lines are the context.
    - If two changes are separated by at most 2x ContextLineCount unchanged
      lines, they are in the same hunk.
    - Changed lines start with '! ', deleted with '- ', added with '+ ' and
      unchanged with '  '.
    - If a part contains no changed lines, its lines are left out.
}
var
  FirstChange, LastChange: integer;

  procedure WritePart(const HeaderPrefix, HeaderSuffix: string;
    IsOld: boolean);
  var
    Src: string;
    Lines: TDiffLines;
    i, First, Last, ChangeFirst, ChangeLast, Line: integer;
    HasChanges: boolean;
    Change: TDiffChange;
    Prefix: string;
  begin
    if IsOld then begin
      Src:=fText1;
      Lines:=fLines1;
    end else begin
      Src:=fText2;
      Lines:=fLines2;
    end;
    // range with context
    Change:=TDiffChange(fChanges[FirstChange]);
    if IsOld then
      First:=Change.OldFirst-ContextLineCount
    else
      First:=Change.NewFirst-ContextLineCount;
    if First<1 then
      First:=1;
    Change:=TDiffChange(fChanges[LastChange]);
    if IsOld then
      Last:=Change.OldLast+ContextLineCount
    else
      Last:=Change.NewLast+ContextLineCount;
    if Last>length(Lines) then
      Last:=length(Lines);
    WriteStrToStream(Stream,HeaderPrefix+RangeToStr(First,Last)+HeaderSuffix+LineBreak);

    // check if this part has changed lines
    HasChanges:=false;
    for i:=FirstChange to LastChange do begin
      Change:=TDiffChange(fChanges[i]);
      if Change.Kind=dckNone then continue;
      if IsOld then
        HasChanges:=Change.OldLast>=Change.OldFirst
      else
        HasChanges:=Change.NewLast>=Change.NewFirst;
      if HasChanges then break;
    end;
    if not HasChanges then exit;

    Line:=First;
    for i:=FirstChange to LastChange do begin
      Change:=TDiffChange(fChanges[i]);
      if Change.Kind=dckNone then continue;
      if IsOld then begin
        ChangeFirst:=Change.OldFirst;
        ChangeLast:=Change.OldLast;
      end else begin
        ChangeFirst:=Change.NewFirst;
        ChangeLast:=Change.NewLast;
      end;
      // context in front
      WriteLines(Stream,'  ',Src,Lines,Line,ChangeFirst-1);
      // changed lines
      if Change.Kind=dckModify then
        Prefix:='! '
      else if IsOld then
        Prefix:='- '
      else
        Prefix:='+ ';
      WriteLines(Stream,Prefix,Src,Lines,ChangeFirst,ChangeLast);
      Line:=ChangeLast+1;
    end;
    // context behind
    WriteLines(Stream,'  ',Src,Lines,Line,Last);
  end;

var
  i, Gap: integer;
  Change: TDiffChange;
begin
  i:=0;
  while i<fChanges.Count do begin
    Change:=TDiffChange(fChanges[i]);
    if Change.Kind=dckNone then begin
      inc(i);
      continue;
    end;
    // collect changes of hunk
    FirstChange:=i;
    LastChange:=i;
    inc(i);
    while i+1<fChanges.Count do begin
      // fChanges[i] is unchanged, fChanges[i+1] is a change
      Change:=TDiffChange(fChanges[i]);
      Gap:=Change.OldLast-Change.OldFirst+1;
      if Gap<Change.NewLast-Change.NewFirst+1 then
        Gap:=Change.NewLast-Change.NewFirst+1;
      if Gap>2*ContextLineCount then break;
      LastChange:=i+1;
      inc(i,2);
    end;
    WriteStrToStream(Stream,'***************'+LineBreak);
    WritePart('*** ',' ****',true);
    WritePart('--- ',' ----',false);
  end;
end;

function TDiffOutput.CreateTextDiff: string;
var
  Stream: TMemoryStream;
begin
  Result:='';
  if not fComputed then
    Compute;
  Stream:=TMemoryStream.Create;
  try
    case fOutputType of
    tdoContext: WriteContextDiff(Stream);
    else
      WriteRCSDiff(Stream);
    end;
    SetLength(Result,Stream.Size);
    if Result<>'' then
      Move(Stream.Memory^,Result[1],length(Result));
  finally
    Stream.Free;
  end;
end;

constructor TDiffOutput.Create(const aText1, aText2: string; aFlags: TTextDiffFlags);
begin
  fText1:=aText1;
  fText2:=aText2;
  fFlags:=aFlags;
  fOutputType:=tdoContext; // Default OutputType, can be changed later
  fChanges:=TFPList.Create;
end;

destructor TDiffOutput.Destroy;
begin
  Clear;
  FreeAndNil(fChanges);
  inherited Destroy;
end;

procedure TDiffOutput.Clear;
var
  i: integer;
begin
  for i:=0 to fChanges.Count-1 do
    TObject(fChanges[i]).Free;
  fChanges.Clear;
  fComputed:=false;
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

end.

