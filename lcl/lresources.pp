{
  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Abstract:
    This unit maintains and stores all lazarus resources in the global list
    named LazarusResources.
    A lazarus resource is an ansistring, with a name and a valuetype. Both, name
    and valuetype, are ansistrings.
    Lazarus resources are normally included via an include directive in the
    initialization part of a unit. To create such include files use the
    BinaryToLazarusResourceCode procedure.
    To create a LRS file from an LFM file use the LFMtoLRSfile function which
    transforms the LFM text to binary format and stores it as Lazarus resource
    include file.

}
unit LResources;

{$mode objfpc}{$H+}

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
    function FindPosition(const Name:AnsiString):integer;
    procedure Sort;
    procedure MergeSort(List,MergeList:TList;Pos1,Pos2:integer);
    procedure Merge(List,MergeList:TList;Pos1,Pos2,Pos3:integer);
  public
    procedure Add(const Name, ValueType, Value: AnsiString);
    procedure Add(const Name, ValueType: AnsiString; Values: array of string);
    function Find(const Name: AnsiString):TLResource;
    constructor Create;
    destructor Destroy;  override;
  end;

procedure BinaryToLazarusResourceCode(BinStream,ResStream:TStream;
  ResourceName, ResourceType:AnsiString);
function LFMtoLRSfile(LFMfilename:ansistring):boolean;
 // returns true if successful
function LFMtoLRSstream(LFMStream,LFCStream:TStream):boolean;
 // returns true if successful
function FindLFMClassName(LFMStream:TStream):AnsiString;

type
  TDelphiStreamOriginalFormat = (sofUnknown, sofBinary, sofText);
  
procedure DelphiObjectBinaryToText(Input, Output: TStream);
procedure DelphiObjectToText(Input, Output: TStream;
  var OriginalFormat: TDelphiStreamOriginalFormat);

procedure DelphiObjectResourceToText(Input, Output: TStream);
procedure DelphiObjectResToText(Input, Output: TStream;
  var OriginalFormat: TDelphiStreamOriginalFormat);
  
function TestFormStreamFormat(Stream: TStream): TDelphiStreamOriginalFormat;
procedure FormDataToText(FormStream, TextStream: TStream);


var LazarusResources:TLResourceList;


implementation


const LineEnd:ShortString={$IFDEF win32}#13+{$ENDIF}#10;

{function UTF8Decode(const S: UTF8String): WideString;
begin

end;}

procedure BinaryToLazarusResourceCode(BinStream,ResStream:TStream;
  ResourceName, ResourceType:AnsiString);
{ example ResStream:
  LazarusResources.Add('ResourceName','ResourceType',
    #123#45#34#78#18#72#45#34#78#18#72#72##45#34#78#45#34#78#184#34#78#145#34#78
    +#83#187#6#78#83
  );
}
var s, Indent: ShortString;
  p, x: integer;
  c, h: char;
  RangeString, NewRangeString: boolean;
  RightMargin, CurLine: integer;
begin
  // fpc is not optimized for building a constant string out of thousands of
  // lines. It needs huge amounts of memory and becomes very slow. Therefore big
  // files are split into several strings.

  Indent:='';
  s:=Indent+'LazarusResources.Add('''+ResourceName+''','''+ResourceType+''',['
    +LineEnd;
  ResStream.Write(s[1],length(s));
  p:=0;
  Indent:='  '+Indent;
  ResStream.Write(Indent[1],length(Indent));
  x:=length(Indent);
  RangeString:=false;
  CurLine:=1;
  RightMargin:=80;
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
      inc(CurLine);
      if (CurLine and 63)<>1 then
        s:=Indent+'+'+s
      else
        s:=Indent+','+s;
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
  s:=LineEnd+Indent+']);'+LineEnd;
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

function LFMtoLRSfile(LFMfilename:ansistring):boolean;
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
                    length(LFMfilename)-length(LFMfilenameExt))+'.lrs';
      Result:=LFMtoLRSstream(LFMMemStream,LFCMemStream);
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
    on E: Exception do begin
      writeln('LFMtoLRSfile ',E.Message);
      Result:=false;
    end;
  end;
end;

function LFMtoLRSstream(LFMStream,LFCStream:TStream):boolean;
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
    on E: Exception do begin
      writeln('LFMtoLFCstream ',E.Message);
      Result:=false;
    end;
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

procedure TLResourceList.Add(const Name,ValueType: AnsiString;
  Values: array of string);
var
  NewLResource: TLResource;
  i, TotalLen, ValueCount, p: integer;
begin
  NewLResource:=TLResource.Create;
  NewLResource.Name:=Name;
  NewLResource.ValueType:=uppercase(ValueType);
  
  ValueCount:=High(Values)-Low(Values)+1;
  case ValueCount of
    0: raise Exception.Create('TLResourceList.Add: no values');
    1: NewLResource.Value:=Values[0];
  else
    TotalLen:=0;
    for i:=Low(Values) to High(Values) do begin
      inc(TotalLen,length(Values[i]));
    end;
    SetLength(NewLResource.Value,TotalLen);
    p:=1;
    for i:=Low(Values) to High(Values) do begin
      if length(Values[i])>0 then begin
        Move(Values[i][1],NewLResource.Value[p],length(Values[i]));
        inc(p,length(Values[i]));
      end;
    end;
  end;
  
  FList.Add(NewLResource);
end;

function TLResourceList.Find(const Name:AnsiString):TLResource;
var p:integer;
begin
  p:=FindPosition(Name);
  if (p>=0) and (p<FList.Count)
  and (AnsiCompareText(TLResource(FList[p]).Name,Name)=0) then begin
    Result:=TLResource(FList[p]);
  end
  else
  begin
    Result:=nil;
  end;
end;

function TLResourceList.FindPosition(const Name:AnsiString):integer;
var l,r,cmp:integer;
begin
  if FSortedCount<FList.Count then
     Sort;
  Result:=-1;
  l:=0;
  r:=FList.Count-1;
  while (l<=r) do begin
    Result:=(l+r) shr 1;
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

procedure TLResourceList.Add(const Name, ValueType, Value: AnsiString);
begin
  Add(Name,ValueType,[Value]);
end;

//------------------------------------------------------------------------------
// Delphi object streams

type
  TDelphiValueType = (dvaNull, dvaList, dvaInt8, dvaInt16, dvaInt32, dvaExtended,
    dvaString, dvaIdent, dvaFalse, dvaTrue, dvaBinary, dvaSet, dvaLString,
    dvaNil, dvaCollection, dvaSingle, dvaCurrency, dvaDate, dvaWString,
    dvaInt64, dvaUTF8String);
    
  //UTF8String = ansistring;

  TDelphiReader = class
  private
    FStream: TStream;
  protected
    procedure SkipBytes(Count: Integer);
    procedure SkipSetBody;
    procedure SkipProperty;
  public
    constructor Create(Stream: TStream);
    procedure ReadSignature;
    procedure Read(var Buf; Count: Longint);
    function ReadInteger: Longint;
    function ReadValue: TDelphiValueType;
    function NextValue: TDelphiValueType;
    function ReadStr: string;
    function EndOfList: Boolean;
    procedure SkipValue;
    procedure CheckValue(Value: TDelphiValueType);
    procedure ReadListEnd;
    procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer); virtual;
    function ReadFloat: Extended;
    function ReadSingle: Single;
    function ReadCurrency: Currency;
    function ReadDate: TDateTime;
    function ReadString: string;
    //function ReadWideString: WideString;
    function ReadInt64: Int64;
    function ReadIdent: string;
  end;

  TDelphiWriter = class
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream);
    procedure Write(const Buf; Count: Longint);
  end;
  
{function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;}

{ TDelphiReader }

procedure ReadError(Msg: string);
begin
  raise EReadError.Create(Msg);
end;

procedure PropValueError;
begin
  ReadError('Invalid property value' {@SInvalidPropertyValue});
end;

procedure PropertyNotFound(const Name: string);
begin
  raise EReadError.CreateFmt('Property %s does not exist' {@SUnknownProperty},
                             [Name]);
end;

procedure TDelphiReader.SkipBytes(Count: Integer);
begin
  FStream.Position:=FStream.Position+Count;
end;

procedure TDelphiReader.SkipSetBody;
begin
  while ReadStr <> '' do ;
end;

procedure TDelphiReader.SkipProperty;
begin
  ReadStr; { Skips property name }
  SkipValue;
end;

constructor TDelphiReader.Create(Stream: TStream);
begin
  FStream:=Stream;
end;

procedure TDelphiReader.ReadSignature;
var
  Signature: Longint;
begin
  Read(Signature, SizeOf(Signature));
  if Signature <> Longint(FilerSignature) then
    ReadError('Invalid stream format'{@SInvalidImage});
end;

procedure TDelphiReader.Read(var Buf; Count: Longint);
begin
  FStream.Read(Buf,Count);
end;

function TDelphiReader.ReadInteger: Longint;
var
  S: Shortint;
  I: Smallint;
begin
  case ReadValue of
    dvaInt8:
      begin
        Read(S, SizeOf(Shortint));
        Result := S;
      end;
    dvaInt16:
      begin
        Read(I, SizeOf(I));
        Result := I;
      end;
    dvaInt32:
      Read(Result, SizeOf(Result));
  else
    PropValueError;
  end;
end;

function TDelphiReader.ReadValue: TDelphiValueType;
var b: byte;
begin
  Read(b,1);
  Result:=TDelphiValueType(b);
end;

function TDelphiReader.NextValue: TDelphiValueType;
begin
  Result := ReadValue;
  FStream.Position:=FStream.Position-1;
end;

function TDelphiReader.ReadStr: string;
var
  L: Byte;
begin
  Read(L, SizeOf(Byte));
  SetLength(Result, L);
  if L>0 then
    Read(Result[1], L);
end;

function TDelphiReader.EndOfList: Boolean;
begin
  Result := (ReadValue = dvaNull);
  FStream.Position:=FStream.Position-1;
end;

procedure TDelphiReader.SkipValue;

  procedure SkipList;
  begin
    while not EndOfList do SkipValue;
    ReadListEnd;
  end;

  procedure SkipBinary(BytesPerUnit: Integer);
  var
    Count: Longint;
  begin
    Read(Count, SizeOf(Count));
    SkipBytes(Count * BytesPerUnit);
  end;

  procedure SkipCollection;
  begin
    while not EndOfList do
    begin
      if NextValue in [dvaInt8, dvaInt16, dvaInt32] then SkipValue;
      SkipBytes(1);
      while not EndOfList do SkipProperty;
      ReadListEnd;
    end;
    ReadListEnd;
  end;

begin
  case ReadValue of
    dvaNull: { no value field, just an identifier };
    dvaList: SkipList;
    dvaInt8: SkipBytes(SizeOf(Byte));
    dvaInt16: SkipBytes(SizeOf(Word));
    dvaInt32: SkipBytes(SizeOf(LongInt));
    dvaExtended: SkipBytes(SizeOf(Extended));
    dvaString, dvaIdent: ReadStr;
    dvaFalse, dvaTrue: { no value field, just an identifier };
    dvaBinary: SkipBinary(1);
    dvaSet: SkipSetBody;
    dvaLString: SkipBinary(1);
    dvaCollection: SkipCollection;
    dvaSingle: SkipBytes(Sizeof(Single));
    dvaCurrency: SkipBytes(SizeOf(Currency));
    dvaDate: SkipBytes(Sizeof(TDateTime));
    dvaWString: SkipBinary(Sizeof(WideChar));
    dvaInt64: SkipBytes(Sizeof(Int64));
    dvaUTF8String: SkipBinary(1);
  end;
end;

procedure TDelphiReader.CheckValue(Value: TDelphiValueType);
begin
  if ReadValue <> Value then
  begin
    FStream.Position:=FStream.Position-1;
    SkipValue;
    PropValueError;
  end;
end;

procedure TDelphiReader.ReadListEnd;
begin
  CheckValue(dvaNull);
end;

procedure TDelphiReader.ReadPrefix(var Flags: TFilerFlags;
  var AChildPos: Integer);
var
  Prefix: Byte;
begin
  Flags := [];
  if Byte(NextValue) and $F0 = $F0 then
  begin
    Prefix := Byte(ReadValue);
    if (Prefix and $01)>0 then
      Include(Flags,ffInherited);
    if (Prefix and $02)>0 then
      Include(Flags,ffChildPos);
    if (Prefix and $04)>0 then
      Include(Flags,ffInline);
    if ffChildPos in Flags then AChildPos := ReadInteger;
  end;
end;

function TDelphiReader.ReadFloat: Extended;
begin
  if ReadValue = dvaExtended then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadSingle: Single;
begin
  if ReadValue = dvaSingle then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadCurrency: Currency;
begin
  if ReadValue = dvaCurrency then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadDate: TDateTime;
begin
  if ReadValue = dvaDate then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadString: string;
var
  L: Integer;
begin
  if NextValue in [dvaWString, dvaUTF8String] then begin
    ReadError('TDelphiReader.ReadString: not implemented yet: NextValue is a wide or utf8 string');
    //Result := ReadWideString;
  end else
  begin
    L := 0;
    case ReadValue of
      dvaString:
        Read(L, SizeOf(Byte));
      dvaLString:
        Read(L, SizeOf(Integer));
    else
      PropValueError;
    end;
    SetLength(Result, L);
    Read(Pointer(Result)^, L);
  end;
end;

{function TDelphiReader.ReadWideString: WideString;
var
  L: Integer;
  Temp: UTF8String;
begin
  if NextValue in [dvaString, dvaLString] then
    Result := ReadString
  else
  begin
    L := 0;
    case ReadValue of
      dvaWString:
        begin
          Read(L, SizeOf(Integer));
          SetLength(Result, L);
          Read(Pointer(Result)^, L * 2);
        end;
      dvaUtf8String:
        begin
          Read(L, SizeOf(Integer));
          SetLength(Temp, L);
          Read(Pointer(Temp)^, L);
          Result := Utf8Decode(Temp);
        end;
    else
      PropValueError;
    end;
  end;
end;}

function TDelphiReader.ReadInt64: Int64;
begin
  if NextValue = dvaInt64 then
  begin
    ReadValue;
    Read(Result, Sizeof(Result));
  end
  else
    Result := ReadInteger;
end;

function TDelphiReader.ReadIdent: string;
var
  L: Byte;
begin
  case ReadValue of
    dvaIdent:
      begin
        Read(L, SizeOf(Byte));
        SetLength(Result, L);
        Read(Result[1], L);
      end;
    dvaFalse:
      Result := 'False';
    dvaTrue:
      Result := 'True';
    dvaNil:
      Result := 'nil';
    dvaNull:
      Result := 'Null';
  else
    PropValueError;
  end;
end;

{ TDelphiWriter }

{ MultiByte Character Set (MBCS) byte type }
type
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);

function ByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result := mbSingleByte;
  { ToDo:
    if SysLocale.FarEast then
      Result := ByteTypeTest(PChar(S), Index-1);
  }
end;

constructor TDelphiWriter.Create(Stream: TStream);
begin
  FStream:=Stream;
end;

procedure TDelphiWriter.Write(const Buf; Count: Longint);
begin
  FStream.Write(Buf,Count);
end;

procedure DelphiObjectBinaryToText(Input, Output: TStream);
var
  NestingLevel: Integer;
  SaveSeparator: Char;
  Reader: TDelphiReader;
  Writer: TDelphiWriter;
  ObjectName, PropName: string;

  procedure WriteIndent;
  const
    Blanks: array[0..1] of Char = '  ';
  var
    I: Integer;
  begin
    for I := 1 to NestingLevel do Writer.Write(Blanks, SizeOf(Blanks));
  end;

  procedure WriteStr(const S: string);
  begin
    Writer.Write(S[1], Length(S));
  end;

  procedure NewLine;
  begin
    WriteStr(LineEnd);
    WriteIndent;
  end;

  procedure ConvertValue; forward;

  procedure ConvertHeader;
  var
    ClassName: string;
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Reader.ReadPrefix(Flags, Position);
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;
    WriteIndent;
    if ffInherited in Flags then
      WriteStr('inherited ')
    else if ffInline in Flags then
      WriteStr('inline ')
    else
      WriteStr('object ');
    if ObjectName <> '' then
    begin
      WriteStr(ObjectName);
      WriteStr(': ');
    end;
    WriteStr(ClassName);
    if ffChildPos in Flags then
    begin
      WriteStr(' [');
      WriteStr(IntToStr(Position));
      WriteStr(']');
    end;

    if ObjectName = '' then
      ObjectName := ClassName;  // save for error reporting

    WriteStr(LineEnd);
  end;

  procedure BinToHex(Buffer, Text: PChar; BufSize: Integer);
  const
    Convert: array[0..15] of Char = '0123456789ABCDEF';
  var
    I: Integer;
  begin
    for I := 0 to BufSize - 1 do
    begin
      Text[0] := Convert[Byte(Buffer[I]) shr 4];
      Text[1] := Convert[Byte(Buffer[I]) and $F];
      Inc(Text, 2);
    end;
  end;

  procedure ConvertBinary;
  const
    BytesPerLine = 32;
  var
    MultiLine: Boolean;
    I: Integer;
    Count: Longint;
    Buffer: array[0..BytesPerLine - 1] of Char;
    Text: array[0..BytesPerLine * 2 - 1] of Char;
  begin
    Reader.ReadValue;
    WriteStr('{');
    Inc(NestingLevel);
    Reader.Read(Count, SizeOf(Count));
    MultiLine := Count >= BytesPerLine;
    while Count > 0 do
    begin
      if MultiLine then NewLine;
      if Count >= 32 then I := 32 else I := Count;
      Reader.Read(Buffer, I);
      BinToHex(Buffer, Text, I);
      Writer.Write(Text, I * 2);
      Dec(Count, I);
    end;
    Dec(NestingLevel);
    WriteStr('}');
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue;
  const
    LineLength = 64;
  var
    I, J, K, L: Integer;
    S: string;
    //W: WideString;
    LineBreak: Boolean;
  begin
    case Reader.NextValue of
      dvaList:
        begin
          Reader.ReadValue;
          WriteStr('(');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            ConvertValue;
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteStr(')');
        end;
      dvaInt8, dvaInt16, dvaInt32:
        WriteStr(IntToStr(Reader.ReadInteger));
      dvaExtended:
        WriteStr(FloatToStr(Reader.ReadFloat));
      dvaSingle:
        WriteStr(FloatToStr(Reader.ReadSingle) + 's');
      dvaCurrency:
        WriteStr(FloatToStr(Reader.ReadCurrency * 10000) + 'c');
      dvaDate:
        WriteStr(FloatToStr(Reader.ReadDate) + 'd');
      dvaWString, dvaUTF8String:
        begin
          ReadError('TDelphiReader: not implemented yet: wide/utf8 string support');
          {W := Reader.ReadWideString;
          ToDo: L := Length(W);
          if L = 0 then WriteStr('''''') else
          begin
            I := 1;
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (W[I] >= ' ') and (W[I] <> '''') and (Ord(W[i]) <= 127) then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (W[I] < ' ') or (W[I] = '''') or
                    ((I - K) >= LineLength) or (Ord(W[i]) > 127);
                  if ((I - K) >= LineLength) then LineBreak := True;
                  WriteStr('''');
                  while J < I do
                  begin
                    WriteStr(Char(W[J]));
                    Inc(J);
                  end;
                  WriteStr('''');
                end else
                begin
                  WriteStr('#');
                  WriteStr(IntToStr(Ord(W[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;}
        end;
      dvaString, dvaLString:
        begin
          S := Reader.ReadString;
          L := Length(S);
          if L = 0 then WriteStr('''''') else
          begin
            I := 1;
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (S[I] >= ' ') and (S[I] <> '''') then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (S[I] < ' ') or (S[I] = '''') or
                    ((I - K) >= LineLength);
                  if ((I - K) >= LineLength) then
                  begin
                    LineBreak := True;
                    if ByteType(S, I) = mbTrailByte then Dec(I);
                  end;
                  WriteStr('''');
                  Writer.Write(S[J], I - J);
                  WriteStr('''');
                end else
                begin
                  WriteStr('#');
                  WriteStr(IntToStr(Ord(S[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      dvaIdent, dvaFalse, dvaTrue, dvaNil, dvaNull:
        WriteStr(Reader.ReadIdent);
      dvaBinary:
        ConvertBinary;
      dvaSet:
        begin
          Reader.ReadValue;
          WriteStr('[');
          I := 0;
          while True do
          begin
            S := Reader.ReadStr;
            if S = '' then Break;
            if I > 0 then WriteStr(', ');
            WriteStr(S);
            Inc(I);
          end;
          WriteStr(']');
        end;
      dvaCollection:
        begin
          Reader.ReadValue;
          WriteStr('<');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            WriteStr('item');
            if Reader.NextValue in [dvaInt8, dvaInt16, dvaInt32] then
            begin
              WriteStr(' [');
              ConvertValue;
              WriteStr(']');
            end;
            WriteStr(LineEnd);
            Reader.CheckValue(dvaList);
            Inc(NestingLevel);
            while not Reader.EndOfList do ConvertProperty;
            Reader.ReadListEnd;
            Dec(NestingLevel);
            WriteIndent;
            WriteStr('end');
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteStr('>');
        end;
      dvaInt64:
        WriteStr(IntToStr(Reader.ReadInt64));
    else
      raise EReadError.CreateFmt('Error reading %s%s%s: %s'{@sPropertyException},
        [ObjectName, '.', PropName, Ord(Reader.NextValue)]);
    end;
  end;

  procedure ConvertProperty;
  begin
    WriteIndent;
    PropName := Reader.ReadStr;  // save for error reporting
    WriteStr(PropName);
    WriteStr(' = ');
    ConvertValue;
    WriteStr(LineEnd);
  end;

  procedure ConvertObject;
  begin
    ConvertHeader;
    Inc(NestingLevel);
    while not Reader.EndOfList do ConvertProperty;
    Reader.ReadListEnd;
    while not Reader.EndOfList do ConvertObject;
    Reader.ReadListEnd;
    Dec(NestingLevel);
    WriteIndent;
    WriteStr('end' + LineEnd);
  end;

begin
  NestingLevel := 0;
  Reader := TDelphiReader.Create(Input);
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Writer := TDelphiWriter.Create(Output);
    try
      Reader.ReadSignature;
      ConvertObject;
    finally
      Writer.Free;
    end;
  finally
    DecimalSeparator := SaveSeparator;
    Reader.Free;
  end;
end;

function TestFormStreamFormat(Stream: TStream): TDelphiStreamOriginalFormat;
var
  Pos: Integer;
  Signature: Integer;
begin
  Pos := Stream.Position;
  Signature := 0;
  Stream.Read(Signature, sizeof(Signature));
  Stream.Position := Pos;
  if (Byte(Signature) = $FF) or (Signature = Integer(FilerSignature)) then
    Result := sofBinary
    // text format may begin with "object", "inherited", or whitespace
  else if Char(Signature) in ['o','O','i','I',' ',#13,#11,#9] then
    Result := sofText
  else
    Result := sofUnknown;
end;

type
  TObjectTextConvertProc = procedure (Input, Output: TStream);

procedure InternalDelphiBinaryToText(Input, Output: TStream;
  var OriginalFormat: TDelphiStreamOriginalFormat;
  ConvertProc: TObjectTextConvertProc;
  BinarySignature: Integer; SignatureLength: Byte);
var
  Pos: Integer;
  Signature: Integer;
begin
  Pos := Input.Position;
  Signature := 0;
  if SignatureLength > sizeof(Signature) then
    SignatureLength := sizeof(Signature);
  Input.Read(Signature, SignatureLength);
  Input.Position := Pos;
  if Signature = BinarySignature then
  begin     // definitely binary format
    if OriginalFormat = sofBinary then
      Output.CopyFrom(Input, Input.Size - Input.Position)
    else
    begin
      if OriginalFormat = sofUnknown then
        Originalformat := sofBinary;
      ConvertProc(Input, Output);
    end;
  end
  else  // might be text format
  begin
    if OriginalFormat = sofBinary then
      ConvertProc(Input, Output)
    else
    begin
      if OriginalFormat = sofUnknown then
      begin   // text format may begin with "object", "inherited", or whitespace
        if Char(Signature) in ['o','O','i','I',' ',#13,#11,#9] then
          OriginalFormat := sofText
        else    // not binary, not text... let it raise the exception
        begin
          ConvertProc(Input, Output);
          Exit;
        end;
      end;
      if OriginalFormat = sofText then
        Output.CopyFrom(Input, Input.Size - Input.Position);
    end;
  end;
end;

procedure DelphiObjectToText(Input, Output: TStream;
  var OriginalFormat: TDelphiStreamOriginalFormat);
begin
  InternalDelphiBinaryToText(Input, Output, OriginalFormat,
    @DelphiObjectBinaryToText, Integer(FilerSignature), sizeof(Integer));
end;

procedure DelphiObjectResToText(Input, Output: TStream;
  var OriginalFormat: TDelphiStreamOriginalFormat);
begin
  InternalDelphiBinaryToText(Input, Output, OriginalFormat,
    @DelphiObjectResourceToText, $FF, 1);
end;

procedure DelphiObjectResourceToText(Input, Output: TStream);
begin
  Input.ReadResHeader;
  DelphiObjectBinaryToText(Input, Output);
end;

procedure FormDataToText(FormStream, TextStream: TStream);
begin
  case TestFormStreamFormat(FormStream) of
  sofBinary:
    DelphiObjectResourceToText(FormStream, TextStream);

  sofText:
    TextStream.CopyFrom(FormStream,FormStream.Size);

  else
    raise Exception.Create('invalid Form object stream');
  end;
end;

//------------------------------------------------------------------------------
initialization
  LazarusResources:=TLResourceList.Create;

finalization
  LazarusResources.Free;

end.

