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
  Classes, SysUtils;

procedure FreeThenNil(var obj);
function ComparePointers(p1, p2: Pointer): integer; inline;
function CompareBoolean(b1, b2: boolean): integer;

{ MergeSortWithLen:
  sort ascending, e.g. Compare(List[0],List[1])<0
  keeping order (for each i<j and Compare(List[i],List[j])=0) }
procedure MergeSortWithLen(List: PPointer; ListLength: PtrInt;
                           const Compare: TListSortCompare);

var
  ConsoleVerbosity: integer = 0; // 0=normal, -1=quiet, 1=verbose, 2=very verbose

implementation

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

