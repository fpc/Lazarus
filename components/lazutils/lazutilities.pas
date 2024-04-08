{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazUtilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo;

type
  TGetSkipCheckByKey = function(AKey: String): Boolean;
  TStringsSortCompare = function(const Item1, Item2: string): Integer;

function GetSkipCheckByKey(AKey: String): Boolean;
procedure SetSkipCheckByKeyProc(AProc: TGetSkipCheckByKey);

procedure FreeThenNil(var obj);
function ComparePointers(p1, p2: Pointer): integer; inline;
function CompareBoolean(b1, b2: boolean): integer;

function GetEnumValueDef(TypeInfo: PTypeInfo; const Name: string;
                         const DefaultValue: Integer): Integer;

function RoundToInt(e: Extended): integer; inline;
function RoundToCardinal(e: Extended): cardinal; inline;
function TruncToInt(e: Extended): integer; inline;
function TruncToCardinal(e: Extended): cardinal; inline;
function StrToDouble(const s: string): double; inline;

// identifier
function CreateFirstIdentifier(const Identifier: string): string;
function CreateNextIdentifier(const Identifier: string): string;

// sort so that for each i is OnCompare(List[i],List[i+1])<=0
procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare); overload;
procedure MergeSort(List: TFPList; StartIndex, EndIndex: integer; const OnCompare: TListSortCompare); overload;
procedure MergeSort(List: TStrings; const OnCompare: TStringsSortCompare); overload;
{ MergeSortWithLen:
  sort ascending, e.g. Compare(List[0],List[1])<0
  keeping order (for each i<j and Compare(List[i],List[j])=0) }
procedure MergeSortWithLen(List: PPointer; ListLength: PtrInt;
                           const Compare: TListSortCompare);

var
  ConsoleVerbosity: integer = 0; // 0=normal, -1=quiet, 1=verbose, 2=very verbose

implementation

var
  FGetSkipCheckByKeyProc: TGetSkipCheckByKey;

function GetSkipCheckByKey(AKey: String): Boolean;
begin
  Result := FGetSkipCheckByKeyProc <> nil;
  if Result then
    Result := FGetSkipCheckByKeyProc(AKey);
end;

procedure SetSkipCheckByKeyProc(AProc: TGetSkipCheckByKey);
begin
  FGetSkipCheckByKeyProc := AProc;
end;

procedure FreeThenNil(var obj);
begin
  if Pointer(obj) <> nil then
  begin
    TObject(obj).Free;
    Pointer(obj) := nil;
  end;
end;

function ComparePointers(p1, p2: Pointer): integer;
begin
  if p1>p2 then
    Result:=1
  else if p1<p2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareBoolean(b1, b2: boolean): integer;
begin
  if b1=b2 then
    Result:=0
  else if b1 then
    Result:=1
  else
    Result:=-1;
end;

function GetEnumValueDef(TypeInfo: PTypeInfo; const Name: string;
  const DefaultValue: Integer): Integer;
begin
  Result:=GetEnumValue(TypeInfo,Name);
  if Result<0 then
    Result:=DefaultValue;
end;

function RoundToInt(e: Extended): integer;
begin
  Result:=integer(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToInt ',e,' ',Result);
  {$ENDIF}
end;

function RoundToCardinal(e: Extended): cardinal;
begin
  Result:=cardinal(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function TruncToInt(e: Extended): integer;
begin
  Result:=integer(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToInt ',e,' ',Result);
  {$ENDIF}
end;

function TruncToCardinal(e: Extended): cardinal;
begin
  Result:=cardinal(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function StrToDouble(const s: string): double;
begin
  {$IFDEF VerboseRound}
  DebugLn('StrToDouble "',s,'"');
  {$ENDIF}
  Result:=Double(StrToFloat(s));
end;

function CreateFirstIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident1
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)+'1';
end;

function CreateNextIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident60
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)
          +IntToStr(1+StrToIntDef(copy(Identifier,p+1,length(Identifier)-p),0));
end;

procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare);
begin
  if List=nil then exit;
  MergeSort(List,0,List.Count-1,OnCompare);
end;

procedure MergeSort(List: TFPList; StartIndex, EndIndex: integer;
  const OnCompare: TListSortCompare);
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PPointer;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: Pointer;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:PtrInt;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

var
  Cnt: Integer;
begin
  if (List=nil) then exit;
  Cnt:=List.Count;
  if StartIndex<0 then StartIndex:=0;
  if EndIndex>=Cnt then EndIndex:=Cnt-1;
  if StartIndex>=EndIndex then exit;
  MergeList:=GetMem(List.Count*SizeOf(Pointer));
  Sort(StartIndex,EndIndex);
  Freemem(MergeList);
end;

procedure MergeSort(List: TStrings; const OnCompare: TStringsSortCompare);
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PAnsiString;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: string;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:integer;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

var
  CurSize: PtrInt;
  i: PtrInt;
begin
  if (List=nil) or (List.Count<=1) then exit;
  CurSize:=PtrInt(List.Count)*SizeOf(Pointer);
  MergeList:=GetMem(CurSize);
  FillChar(MergeList^,CurSize,0);
  Sort(0,List.Count-1);
  for i:=0 to List.Count-1 do MergeList[i]:='';
  Freemem(MergeList);
end;

procedure MergeSortWithLen(List: PPointer; ListLength: PtrInt;
  const Compare: TListSortCompare);
var
  MergeList: PPointer;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var
    Src1Pos, Src2Pos, DestPos, cmp, i: PtrInt;
  begin
    while (Pos3>=Pos2) and (Compare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=Compare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for i:=DestPos+1 to Pos3 do
      List[i]:=MergeList[i];
  end;

  procedure Sort(const Pos1, Pos2: PtrInt);
  // sort List from Pos1 to Pos2, using MergeList as temporary buffer
  var
    cmp, mid: PtrInt;
    p: Pointer;
  begin
    if Pos1>=Pos2 then begin
      // one element is always sorted -> nothing to do
    end else if Pos1+1=Pos2 then begin
      // two elements can be sorted easily
      cmp:=Compare(List[Pos1],List[Pos2]);
      if cmp>0 then begin
        p:=List[Pos1];
        List[Pos1]:=List[Pos2];
        List[Pos2]:=p;
      end;
    end else begin
      mid:=(Pos1+Pos2) shr 1;
      Sort(Pos1,mid);
      Sort(mid+1,Pos2);
      Merge(Pos1,mid+1,Pos2);
    end;
  end;

// sort ascending
begin
  if ListLength<=1 then exit;
  GetMem(MergeList,SizeOf(Pointer)*ListLength);
  try
    Sort(0,ListLength-1);
  finally
    FreeMem(MergeList);
  end;
end;

end.

