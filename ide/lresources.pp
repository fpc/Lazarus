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
    The ResourceList is currently a sorted list, which is okay for hundreds
    of resources. But stringtables consist normally of thousands of entries.
    Therefore a special StringTable Resource is needed.

}
{$mode objfpc}

interface

uses
  Classes, SysUtils, Strings;

type
  PLResource = ^LResource;
  LResource = record
    Name: AnsiString;
    ValueType: AnsiString;
    Value: AnsiString;
  end;

  TLResourceList = class(TObject)
    FList:TList;
    function FindPosition(Name:AnsiString):integer;
  public
    procedure Add(Name,ValueType,Value:AnsiString);
    function Find(Name:AnsiString):LResource;
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
const LineEnd:string=#10;
  RightMargin:integer=79;
var s,Indent:string;
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
    if (not (c in ['a'..'z','A'..'Z'])) then
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
var LFMStream,LFCStream:TFileStream;
  LFCfilename,LFMfilenameExt:ansistring;
begin
  Result:=true;
  try
    LFMStream:=TFileStream.Create(LFMfilename,fmOpenRead);
    try
      LFMfilenameExt:=ExtractFileExt(LFMfilename);
      LFCfilename:=copy(LFMfilename,1,
                    length(LFMfilename)-length(LFMfilenameExt))+'.lfc';
      LFCStream:=TFileStream.Create(LFCfilename,fmCreate);
      try
        Result:=LFMtoLFCstream(LFMStream,LFCStream);
      finally
        LFCStream.Free;
      end;
    finally
      LFMStream.Free;
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

{ TLResourceList }

constructor TLResourceList.Create;
begin
  FList:=TList.Create;
end;

destructor TLResourceList.Destroy;
var a:integer;
  p:PLResource;
begin
  for a:=0 to FList.Count-1 do begin
    p:=FList[a];
    LResource(p^).Name:='';
    LResource(p^).ValueType:='';
    LResource(p^).Value:='';
    FreeMem(p);
  end;
  FList.Free;
end;

procedure TLResourceList.Add(Name,ValueType,Value:AnsiString);
var NewPLResource:PLResource;
  NewPos,cmp:integer;
begin
  GetMem(NewPLResource,SizeOf(LResource));
  NewPLResource^.Name:=Name;
  NewPLResource^.ValueType:=uppercase(ValueType);
  NewPLResource^.Value:=Value;
  if FList.Count=0 then begin
    FList.Add(NewPLResource);
  end else begin
    NewPos:=FindPosition(Name);
    if (NewPos<0) then begin
      NewPos:=0;
    end else if (NewPos<FList.Count) then begin
      cmp:=AnsiCompareText(LResource(FList[NewPos]^).Name,Name);
      if cmp=0 then begin
        // resource already exists
        // ToDo: replace with an exception
        writeln('[TLResourceList.Add] ERROR: LResource '''+Name+''' already exists.');
        halt;
      end else if cmp<0 then begin
        inc(NewPos);
      end;
    end;
    FList.Insert(NewPos,NewPLResource);
  end;
end;

function TLResourceList.Find(Name:AnsiString):LResource;
var p:integer;
begin
  p:=FindPosition(Name);
  if (p>=0) and (p<FList.Count)
  and (AnsiCompareText(LResource(FList[p]^).Name,Name)=0) then
    Result:=LResource(FList[p]^)
  else begin
    Result.Name:=Name;
    Result.Value:='';
    Result.ValueType:='';
  end;
end;

function TLResourceList.FindPosition(Name:AnsiString):integer;
var l,r,cmp:integer;
begin
  Result:=-1;
  l:=0;
  r:=FList.Count-1;
  while (l<=r) do begin
    Result:=(l+r) shr 1;
    cmp:=AnsiCompareText(Name,LResource(FList[Result]^).Name);
    if cmp<0 then
      r:=Result-1
    else if cmp>0 then
      l:=Result+1
    else
      exit;
  end;
end;

initialization
  LazarusResources:=TLResourceList.Create;

finalization
  LazarusResources.Free;

end.

