unit lresources;
{
  Author: Mattias Gaertner

  Abstract:
    This unit maintains and stores all lazarus resources in the global list
    named LazarusResources.
    A lazarus resource is an ansistring, with a name and a valuetype. Both, name
    and valuetype, are ansistrings.
    Lazarus resources are normally included via an include directive in the
    initialization part of a unit. To create such include files use the
    BinaryToLazarusResourceCode procedure.
    To create a LFC file from an LFM file use the LFMtoLFCfile function which
    transforms the LFM text to binary format and stores it as Lazarus resource
    include file.

  ToDo:

}
{$mode objfpc}

interface

uses
  Classes, SysUtils, Strings;

type
  TLResource = class
  public
    Name: AnsiString;
    ValueType: AnsiString;
    Value: AnsiString;
  end;

  TLResourceList = class(TObject)
  public //private
    FList:TList;  // main list with all resource pointer
    FMergeList:TList; // list needed for mergesort
    FSortedCount:integer; // 0 .. FSortedCount-1 resources are sorted
    function FindPosition(Name:AnsiString):integer;
    procedure Sort;
    procedure MergeSort(List,MergeList:TList;Pos1,Pos2:integer);
    procedure Merge(List,MergeList:TList;Pos1,Pos2,Pos3:integer);
  public
    procedure Add(Name,ValueType,Value:AnsiString);
    function Find(Name:AnsiString):TLResource;
    constructor Create;
    destructor Destroy;  override;
  end;

procedure BinaryToLazarusResourceCode(BinStream,ResStream:TStream;
  ResourceName, ResourceType:AnsiString);
function LFMtoLFCfile(LFMfilename:ansistring):boolean;
 // returns true if successful
function LFMtoLFCstream(LFMStream,LFCStream:TStream):boolean;
 // returns true if successful
function FindLFMClassName(LFMStream:TStream):AnsiString;

var LazarusResources:TLResourceList;


implementation

procedure BinaryToLazarusResourceCode(BinStream,ResStream:TStream;
  ResourceName, ResourceType:AnsiString);
{ example ResStream:
  LazarusResources.Add('ResourceName','ResourceType',
    #123#45#34#78#18#72#45#34#78#18#72#72##45#34#78#45#34#78#184#34#78#145#34#78
    +#83#187#6#78#83
  );
}
const LineEnd:ShortString=#10;
  RightMargin:integer=79;
var s,Indent:ShortString;
  p,x:integer;
  c,h:char;
  RangeString,NewRangeString:boolean;
begin
  Indent:='  ';
  s:=Indent+'LazarusResources.Add('''+ResourceName+''','''+ResourceType+''','
    +LineEnd;
  ResStream.Write(s[1],length(s));
  p:=0;
  Indent:='  '+Indent;
  ResStream.Write(Indent[1],length(Indent));
  x:=length(Indent);
  RangeString:=false;
  while p<BinStream.Size do begin
    BinStream.Read(c,1);
    NewRangeString:=(ord(c)>=32) and (ord(c)<=127);
    if NewRangeString then begin
      if RangeString then
        s:=''
      else begin
        s:='''';
      end;
      s:=s+c;
      if c='''' then s:=s+'''';
    end else begin
      if RangeString then begin
        s:='''';
      end else
        s:='';
      s:=s+'#'+IntToStr(ord(c));
    end;
    inc(x,length(s));
    if (x>RightMargin) or ((NewRangeString) and (x=RightMargin)) then begin
      if RangeString then begin
        h:='''';
        ResStream.Write(h,1);
        if NewRangeString then
          s:=''''+s
        else begin
          s:=copy(s,2,length(s)-1);
        end;
      end;
      ResStream.Write(LineEnd[1],length(LineEnd));
      s:=Indent+'+'+s;
      x:=length(s);
    end;
    ResStream.Write(s[1],length(s));
    RangeString:=NewRangeString;
    inc(p);
  end;
  if RangeString then begin
    h:='''';
    ResStream.Write(h,1);
  end;
  Indent:=copy(Indent,3,length(Indent)-2);
  s:=LineEnd+Indent+');'+LineEnd;
  ResStream.Write(s[1],length(s));
end;

function FindLFMClassName(LFMStream:TStream):ansistring;
// the classname is the last word of the first line
var c:char;
  StartPos,EndPos:integer;
begin
  Result:='';
  StartPos:=-1;
  c:=' ';
  repeat
    if (not (c in ['a'..'z','A'..'Z','0'..'9','_'])) then
      StartPos:=LFMStream.Position;
    LFMStream.Read(c,1);
    if LFMStream.Position>1000 then exit;
  until c in [#10,#13];
  if StartPos<0 then exit;
  EndPos:=LFMStream.Position-1;
  SetLength(Result,EndPos-StartPos);
  LFMStream.Position:=StartPos;
  LFMStream.Read(Result[1],length(Result));
  LFMStream.Position:=0;
end;

function LFMtoLFCfile(LFMfilename:ansistring):boolean;
// returns true if successful
var
  LFMFileStream,LFCFileStream:TFileStream;
  LFMMemStream,LFCMemStream:TMemoryStream;
  LFCfilename,LFMfilenameExt:ansistring;
begin
  Result:=true;
  try
    LFMFileStream:=TFileStream.Create(LFMfilename,fmOpenRead);
    LFMMemStream:=TMemoryStream.Create;
    LFCMemStream:=TMemoryStream.Create;
    try
      LFMMemStream.CopyFrom(LFMFileStream,LFMFileStream.Size);
      LFMMemStream.Position:=0;
      LFMfilenameExt:=ExtractFileExt(LFMfilename);
      LFCfilename:=copy(LFMfilename,1,
                    length(LFMfilename)-length(LFMfilenameExt))+'.lfc';
      Result:=LFMtoLFCstream(LFMMemStream,LFCMemStream);
      if not Result then exit;
      LFCMemStream.Position:=0;
      LFCFileStream:=TFileStream.Create(LFCfilename,fmCreate);
      try
        LFCFileStream.CopyFrom(LFCMemStream,LFCMemStream.Size);
      finally
        LFCFileStream.Free;
      end;
    finally
      LFMMemStream.Free;
      LFCMemStream.Free;
      LFMFileStream.Free;
    end;
  except
    Result:=false;
  end;
end;

function LFMtoLFCstream(LFMStream,LFCStream:TStream):boolean;
// returns true if successful
var FormClassName:ansistring;
  BinStream:TMemoryStream;
begin
  Result:=true;
  try
    FormClassName:=FindLFMClassName(LFMStream);
    BinStream:=TMemoryStream.Create;
    try
      ObjectTextToBinary(LFMStream,BinStream);
      BinStream.Position:=0;
      BinaryToLazarusResourceCode(BinStream,LFCStream,FormClassName
        ,'FORMDATA');
    finally
      BinStream.Free;
    end;
  except
    Result:=false;
  end;
end;

//==============================================================================

{ TLResourceList }

constructor TLResourceList.Create;
begin
  FList:=TList.Create;
  FMergeList:=TList.Create;
  FSortedCount:=0;
end;

destructor TLResourceList.Destroy;
var a:integer;
begin
  for a:=0 to FList.Count-1 do
    TLResource(FList[a]).Free;
  FList.Free;
  FMergeList.Free;
end;

procedure TLResourceList.Add(Name,ValueType,Value:AnsiString);
var
  NewLResource:TLResource;
begin
  NewLResource:=TLResource.Create;
  NewLResource.Name:=Name;
  NewLResource.ValueType:=uppercase(ValueType);
  NewLResource.Value:=Value;
  FList.Add(NewLResource);
end;

function TLResourceList.Find(Name:AnsiString):TLResource;
var p:integer;
begin
  p:=FindPosition(Name);
  if (p>=0) and (p<FList.Count) and (AnsiCompareText(TLResource(FList[p]).Name,Name)=0) then
    begin
      Result:=TLResource(FList[p]);
    end
    else
    begin
//      Writeln('returning nil');
      Result:=nil;
    end;
end;

function TLResourceList.FindPosition(Name:AnsiString):integer;
var l,r,cmp:integer;
begin
  if FSortedCount<FList.Count then
     Sort;
  Result:=-1;
  l:=0;
  r:=FList.Count-1;
  while (l<=r) do begin
    Result:=(l+r) shr 1;
//    Writeln(Format('l,r,Name,FList[Result].Name = %d,%d,%s,%s',[l,r,Name,TLResource(FList[Result]).Name]));
    cmp:=AnsiCompareText(Name,TLResource(FList[Result]).Name);
    if cmp<0 then
      r:=Result-1
    else
    if cmp>0 then
      l:=Result+1
    else
      exit;
  end;
end;

procedure TLResourceList.Sort;
begin
  if FSortedCount=FList.Count then exit;
  // sort the unsorted elements
  FMergeList.Count:=FList.Count;
  MergeSort(FList,FMergeList,FSortedCount,FList.Count-1);
  // merge both
  Merge(FList,FMergeList,0,FSortedCount,FList.Count-1);
  FSortedCount:=FList.Count;
end;

procedure TLResourceList.MergeSort(List,MergeList:TList; Pos1,Pos2:integer);
var cmp,mid:integer;
begin
  if Pos1=Pos2 then begin
  end else if Pos1+1=Pos2 then begin
    cmp:=AnsiCompareText(
           TLResource(List[Pos1]).Name,TLResource(List[Pos2]).Name);
    if cmp>0 then begin
      MergeList[Pos1]:=List[Pos1];
      List[Pos1]:=List[Pos2];
      List[Pos2]:=MergeList[Pos1];
    end;
  end else begin
    if Pos2>Pos1 then begin
      mid:=(Pos1+Pos2) shr 1;
      MergeSort(List,MergeList,Pos1,mid);
      MergeSort(List,MergeList,mid+1,Pos2);
      Merge(List,MergeList,Pos1,mid+1,Pos2);
    end;
  end;
end;

procedure TLResourceList.Merge(List,MergeList:TList;Pos1,Pos2,Pos3:integer);
// merge two sorted arrays
// the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
var Src1Pos,Src2Pos,DestPos,cmp,a:integer;
begin
  if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
  Src1Pos:=Pos2-1;
  Src2Pos:=Pos3;
  DestPos:=Pos3;
  while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
    cmp:=AnsiCompareText(
           TLResource(List[Src1Pos]).Name,TLResource(List[Src2Pos]).Name);
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

initialization
  LazarusResources:=TLResourceList.Create;

finalization
  LazarusResources.Free;

end.

