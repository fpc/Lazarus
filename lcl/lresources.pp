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
    named LazarusResources and provides methods and types to stream components.

    A lazarus resource is an ansistring, with a name and a valuetype. Both, name
    and valuetype, are ansistrings as well.
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
  Classes, SysUtils, FPCAdds, TypInfo, LCLProc, LCLStrConsts;

type
  { TLResourceList }

  TLResource = class
  public
    Name: AnsiString;
    ValueType: AnsiString;
    Value: AnsiString;
  end;

  TLResourceList = class(TObject)
  private
    FList: TList;  // main list with all resource pointers
    FMergeList: TList; // list needed for mergesort
    FSortedCount: integer; // 0 .. FSortedCount-1 resources are sorted
    function FindPosition(const Name: AnsiString):integer;
    function GetItems(Index: integer): TLResource;
    procedure Sort;
    procedure MergeSort(List, MergeList: TList; Pos1, Pos2: integer);
    procedure Merge(List, MergeList: TList; Pos1, Pos2, Pos3: integer);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Add(const Name, ValueType, Value: AnsiString);
    procedure Add(const Name, ValueType: AnsiString; Values: array of string);
    function Find(const Name: AnsiString):TLResource;
    function Count: integer;
    property Items[Index: integer]: TLResource read GetItems;
  end;
  
{$IFDEF TRANSLATESTRING}
  { TAbstractTranslator}
  TAbstractTranslator = class(TObject)//Should it be somewhat more than TObject?
  public
    procedure TranslateStringProperty(Sender:TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content:string);virtual;abstract;
   //seems like we need nothing more here
  end;


var LRSTranslator: TAbstractTranslator;
type
{$ENDIF}
   { TLRSObjectReader }
  TLRSObjectReader = class(TAbstractObjectReader)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    procedure Read(var Buf; Count: LongInt);
    procedure SkipProperty;
    procedure SkipSetBody;
    function ReadIntegerContent: integer;
  public
    constructor Create(Stream: TStream; BufSize: Integer); virtual;
    destructor Destroy; override;

    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;
    function BeginProperty: String; override;

    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadFloat: Extended; override;
    function ReadSingle: Single; override;
    {$ifdef HASCURRENCY}
    function ReadCurrency: Currency; override;
    {$endif HASCURRENCY}
    function ReadDate: TDateTime; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    {$ifdef HASWIDESTRING}
    function ReadWideString: WideString;override;
    {$endif HASWIDESTRING}
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  end;
  TLRSObjectReaderClass = class of TLRSObjectReader;
  

  { TLRSObjectWriter }

  TLRSObjectWriter = class(TAbstractObjectWriter)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FSignatureWritten: Boolean;
  protected
    procedure FlushBuffer;
    procedure Write(const Buffer; Count: Longint);
    procedure WriteValue(Value: TValueType);
    procedure WriteStr(const Value: String);
    procedure WriteIntegerContent(i: integer);
    procedure WriteWordContent(w: word);
    procedure WriteInt64Content(i: int64);
    procedure WriteSingleContent(s: single);
    procedure WriteDoubleContent(d: Double);
    procedure WriteExtendedContent(e: Extended);
    {$ifdef HASCURRENCY}
    procedure WriteCurrencyContent(c: Currency);
    {$endif HASCURRENCY}
    {$ifdef HASWIDESTRING}
    procedure WriteWideStringContent(ws: WideString);
    {$endif HASWIDESTRING}
    procedure WriteWordsReversed(p: PWord; Count: integer);
    procedure WriteNulls(Count: integer);
  public
    constructor Create(Stream: TStream; BufSize: Integer); virtual;
    destructor Destroy; override;

    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;

    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    {$ifdef HASCURRENCY}
    procedure WriteCurrency(const Value: Currency); override;
    {$endif HASCURRENCY}
    procedure WriteDate(const Value: TDateTime); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
    {$ifdef HASWIDESTRING}
    procedure WriteWideString(const Value: WideString); override;
    {$endif HASWIDESTRING}
  end;
  TLRSObjectWriterClass = class of TLRSObjectWriter;
  
var
  LazarusResources: TLResourceList;

  LRSObjectReaderClass: TLRSObjectReaderClass;
  LRSObjectWriterClass: TLRSObjectWriterClass;

function InitLazResourceComponent(Instance: TComponent;
                                  RootAncestor: TClass): Boolean;
function CreateLRSReader(s: TStream; var DestroyDriver: boolean): TReader;
function CreateLRSWriter(s: TStream; var DestroyDriver: boolean): TWriter;

procedure BinaryToLazarusResourceCode(BinStream, ResStream: TStream;
  const ResourceName, ResourceType: String);
function LFMtoLRSfile(const LFMfilename: string): boolean;
 // returns true if successful
function LFMtoLRSstream(LFMStream, LRSStream: TStream): boolean;
 // returns true if successful
function FindLFMClassName(LFMStream: TStream):AnsiString;
function CreateLFMFile(AComponent: TComponent; LFMStream: TStream): integer;

type
  TLRSStreamOriginalFormat = (sofUnknown, sofBinary, sofText);
  
procedure LRSObjectBinaryToText(Input, Output: TStream);
procedure LRSObjectTextToBinary(Input, Output: TStream);
procedure LRSObjectToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);

procedure LRSObjectResourceToText(Input, Output: TStream);
procedure LRSObjectResToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);
  
function TestFormStreamFormat(Stream: TStream): TLRSStreamOriginalFormat;
procedure FormDataToText(FormStream, TextStream: TStream);


procedure ReverseBytes(p: Pointer; Count: integer);
procedure ReverseByteOrderInWords(p: PWord; Count: integer);
function ConvertLRSExtendedToDouble(p: Pointer): Double;
procedure ConvertEndianBigDoubleToLRSExtended(BigEndianDouble,
                                              LRSExtended: Pointer);

function ReadLRSWord(s: TStream): word;
function ReadLRSInteger(s: TStream): integer;
function ReadLRSCardinal(s: TStream): cardinal;
function ReadLRSInt64(s: TStream): int64;
function ReadLRSSingle(s: TStream): Single;
function ReadLRSDouble(s: TStream): Double;
function ReadLRSExtended(s: TStream): Extended;
{$ifdef HASCURRENCY}
function ReadLRSCurrency(s: TStream): Currency;
{$endif HASCURRENCY}
{$ifdef HASWIDESTRING}
function ReadLRSWideString(s: TStream): WideString;
{$endif HASWIDESTRING}
function ReadLRSEndianLittleExtendedAsDouble(s: TStream): Double;

procedure WriteLRSWord(s: TStream; const w: word);
procedure WriteLRSInteger(s: TStream; const i: integer);
procedure WriteLRSCardinal(s: TStream; const c: cardinal);
procedure WriteLRSSingle(s: TStream; const si: Single);
procedure WriteLRSDouble(s: TStream; const d: Double);
procedure WriteLRSExtended(s: TStream; const e: extended);
procedure WriteLRSInt64(s: TStream; const i: int64);
{$ifdef HASCURRENCY}
procedure WriteLRSCurrency(s: TStream; const c: Currency);
{$endif HASCURRENCY}
{$ifdef HASWIDESTRING}
procedure WriteLRSWideStringContent(s: TStream; const w: WideString);
{$endif HASWIDESTRING}

procedure WriteLRSReversedWord(s: TStream; w: word);
procedure WriteLRS4BytesReversed(s: TStream; p: Pointer);
procedure WriteLRS8BytesReversed(s: TStream; p: Pointer);
procedure WriteLRS10BytesReversed(s: TStream; p: Pointer);
procedure WriteLRSNull(s: TStream; Count: integer);
procedure WriteLRSEndianBigDoubleAsEndianLittleExtended(s: TStream;
  EndBigDouble: PByte);
procedure WriteLRSReversedWords(s: TStream; p: Pointer; Count: integer);


implementation


const
  LineEnd: ShortString = LineEnding;

var
  ByteToStr: array[char] of shortstring;
  ByteToStrValid: boolean;
  
procedure InitByteToStr;
var
  c: Char;
begin
  if ByteToStrValid then exit;
  for c:=Low(char) to High(char) do
    ByteToStr[c]:=IntToStr(ord(c));
  ByteToStrValid:=true;
end;

procedure BinaryToLazarusResourceCode(BinStream,ResStream:TStream;
  const ResourceName, ResourceType: String);
{ example ResStream:
  LazarusResources.Add('ResourceName','ResourceType',
    #123#45#34#78#18#72#45#34#78#18#72#72##45#34#78#45#34#78#184#34#78#145#34#78
    +#83#187#6#78#83
  );
}
const
  ReadBufSize = 4096;
  WriteBufSize = 4096;
var
  s, Indent: string;
  x: integer;
  c: char;
  RangeString, NewRangeString: boolean;
  RightMargin, CurLine: integer;
  WriteBufStart, Writebuf: PChar;
  WriteBufPos: Integer;
  ReadBufStart, ReadBuf: PChar;
  ReadBufPos, ReadBufLen: integer;
  MinCharCount: Integer;
  
  procedure FillReadBuf;
  begin
    ReadBuf:=ReadBufStart;
    ReadBufPos:=0;
    ReadBufLen:=BinStream.Read(ReadBuf^,ReadBufSize);
  end;
  
  procedure InitReadBuf;
  begin
    GetMem(ReadBufStart,ReadBufSize);
    FillReadBuf;
  end;

  function ReadChar(var c: char): boolean;
  begin
    if ReadBufPos>=ReadBufLen then begin
      FillReadBuf;
      if ReadBufLen=0 then begin
        Result:=false;
        exit;
      end;
    end;
    c:=ReadBuf^;
    inc(ReadBuf);
    inc(ReadBufPos);
    Result:=true;
  end;

  procedure InitWriteBuf;
  begin
    GetMem(WriteBufStart,WriteBufSize);
    WriteBuf:=WriteBufStart;
    WriteBufPos:=0;
  end;

  procedure FlushWriteBuf;
  begin
    if WriteBufPos>0 then begin
      ResStream.Write(WriteBufStart^,WriteBufPos);
      WriteBuf:=WriteBufStart;
      WriteBufPos:=0;
    end;
  end;
  
  procedure WriteChar(c: char);
  begin
    WriteBuf^:=c;
    inc(WriteBufPos);
    inc(WriteBuf);
    if WriteBufPos>=WriteBufSize then
      FlushWriteBuf;
  end;
  
  procedure WriteString(const s: string);
  var
    i: Integer;
  begin
    for i:=1 to length(s) do WriteChar(s[i]);
  end;

  procedure WriteShortString(const s: string);
  var
    i: Integer;
  begin
    for i:=1 to length(s) do WriteChar(s[i]);
  end;

begin
  // fpc is not optimized for building a constant string out of thousands of
  // lines. It needs huge amounts of memory and becomes very slow. Therefore big
  // files are split into several strings.

  InitReadBuf;
  InitWriteBuf;
  InitByteToStr;

  Indent:='';
  s:=Indent+'LazarusResources.Add('''+ResourceName+''','''+ResourceType+''',['
    +LineEnd;
  WriteString(s);
  Indent:='  '+Indent;
  WriteString(Indent);
  x:=length(Indent);
  RangeString:=false;
  CurLine:=1;
  RightMargin:=80;
  while ReadChar(c) do begin
    NewRangeString:=(ord(c)>=32) and (ord(c)<=127);
    // check if new char fits into line or if a new line must be started
    if NewRangeString then begin
      if RangeString then
        MinCharCount:=2 // char plus '
      else
        MinCharCount:=3; // ' plus char plus '
      if c='''' then inc(MinCharCount);
    end else begin
      MinCharCount:=1+length(ByteToStr[c]); // # plus number
      if RangeString then
        inc(MinCharCount); // plus ' for ending last string constant
    end;
    if x+MinCharCount>RightMargin then begin
      // break line
      if RangeString then begin
        // end string constant
        WriteChar('''');
      end;
      // write line ending
      WriteShortString(LineEnd);
      x:=0;
      inc(CurLine);
      // write indention
      WriteString(Indent);
      inc(x,length(Indent));
      // write operator
      if (CurLine and 63)<>1 then
        WriteChar('+')
      else
        WriteChar(',');
      inc(x);
      RangeString:=false;
    end;
    // write converted byte
    if RangeString<>NewRangeString then begin
      WriteChar('''');
      inc(x);
    end;
    if NewRangeString then begin
      WriteChar(c);
      inc(x);
      if c='''' then begin
        WriteChar(c);
        inc(x);
      end;
    end else begin
      WriteChar('#');
      inc(x);
      WriteShortString(ByteToStr[c]);
      inc(x,length(ByteToStr[c]));
    end;
    // next
    RangeString:=NewRangeString;
  end;
  if RangeString then begin
    WriteChar('''');
  end;
  Indent:=copy(Indent,3,length(Indent)-2);
  s:=LineEnd+Indent+']);'+LineEnd;
  WriteString(s);
  FlushWriteBuf;
  FreeMem(ReadBufStart);
  FreeMem(WriteBufStart);
end;

function FindLFMClassName(LFMStream:TStream):ansistring;
{ examples:
  object Form1: TForm1
  inherited AboutBox2: TAboutBox2

  -> the classname is the last word of the first line
}
var c:char;
  StartPos, EndPos: TStreamSeekType;
begin
  Result:='';
  StartPos:=-1;
  c:=' ';
  // read till end of line
  repeat
    // remember last non identifier char position
    if (not (c in ['a'..'z','A'..'Z','0'..'9','_'])) then
      StartPos:=LFMStream.Position;
    if LFMStream.Read(c,1)<>1 then exit;
    if LFMStream.Position>1000 then exit;
  until c in [#10,#13];
  if StartPos<0 then exit;
  EndPos:=LFMStream.Position-1;
  if EndPos-StartPos>255 then exit;
  SetLength(Result,EndPos-StartPos);
  LFMStream.Position:=StartPos;
  LFMStream.Read(Result[1],length(Result));
  LFMStream.Position:=0;
  if (Result='') or (not IsValidIdent(Result)) then
    Result:='';
end;

function LFMtoLRSfile(const LFMfilename: string):boolean;
// returns true if successful
var
  LFMFileStream, LRSFileStream: TFileStream;
  LFMMemStream, LRSMemStream: TMemoryStream;
  LRSfilename, LFMfilenameExt: string;
begin
  Result:=true;
  try
    LFMFileStream:=TFileStream.Create(LFMfilename,fmOpenRead);
    LFMMemStream:=TMemoryStream.Create;
    LRSMemStream:=TMemoryStream.Create;
    try
      LFMMemStream.CopyFrom(LFMFileStream,LFMFileStream.Size);
      LFMMemStream.Position:=0;
      LFMfilenameExt:=ExtractFileExt(LFMfilename);
      LRSfilename:=copy(LFMfilename,1,
                    length(LFMfilename)-length(LFMfilenameExt))+'.lrs';
      Result:=LFMtoLRSstream(LFMMemStream,LRSMemStream);
      if not Result then exit;
      LRSMemStream.Position:=0;
      LRSFileStream:=TFileStream.Create(LRSfilename,fmCreate);
      try
        LRSFileStream.CopyFrom(LRSMemStream,LRSMemStream.Size);
      finally
        LRSFileStream.Free;
      end;
    finally
      LFMMemStream.Free;
      LRSMemStream.Free;
      LFMFileStream.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('LFMtoLRSfile ',E.Message);
      Result:=false;
    end;
  end;
end;

function LFMtoLRSstream(LFMStream, LRSStream: TStream):boolean;
// returns true if successful
var FormClassName:ansistring;
  BinStream:TMemoryStream;
begin
  Result:=true;
  try
    FormClassName:=FindLFMClassName(LFMStream);
    BinStream:=TMemoryStream.Create;
    try
      LRSObjectTextToBinary(LFMStream,BinStream);
      BinStream.Position:=0;
      BinaryToLazarusResourceCode(BinStream,LRSStream,FormClassName
        ,'FORMDATA');
    finally
      BinStream.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('LFMtoLRSstream ',E.Message);
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

function TLResourceList.Count: integer;
begin
  if (Self<>nil) and (FList<>nil) then
    Result:=FList.Count
  else
    Result:=0;
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
    0:
      begin
        NewLResource.Free;
        exit;
      end;
    
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

function TLResourceList.GetItems(Index: integer): TLResource;
begin
  Result:=TLResource(FList[Index]);
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
  
{ TDelphiReader }

procedure ReadError(Msg: string);
begin
  raise EReadError.Create(Msg);
end;

procedure PropValueError;
begin
  ReadError(rsInvalidPropertyValue);
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
    ReadError(rsInvalidStreamFormat);
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
    ReadError('TDelphiReader.ReadString: WideString and UTF8String are not implemented yet');
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

function CreateLFMFile(AComponent: TComponent; LFMStream: TStream): integer;
// 0 = ok
// -1 = error while streaming AForm to binary stream
// -2 = error while streaming binary stream to text file
var
  BinStream: TMemoryStream;
  DestroyDriver: Boolean;
  Writer: TWriter;
begin
  Result:=0;
  BinStream:=TMemoryStream.Create;
  try
    try
      // write component to binary stream
      DestroyDriver:=false;
      Writer:=CreateLRSWriter(BinStream,DestroyDriver);
      try
        Writer.WriteDescendent(AComponent,nil);
      finally
        if DestroyDriver then Writer.Driver.Free;
        Writer.Free;
      end;
    except
      Result:=-1;
      exit;
    end;
    try
      // transform binary to text
      BinStream.Position:=0;
      LRSObjectBinaryToText(BinStream,LFMStream);
    except
      Result:=-2;
      exit;
    end;
  finally
    BinStream.Free;
  end;
end;

procedure LRSObjectBinaryToText(Input, Output: TStream);

  procedure OutStr(const s: String);
  {$IFDEF VerboseLRSObjectBinaryToText}
  var
    i: Integer;
  {$ENDIF}
  begin
    {$IFDEF VerboseLRSObjectBinaryToText}
    for i:=1 to length(s) do begin
      if (s[i] in [#0..#8,#11..#12,#14..#31]) then begin
        DbgOut('#'+IntToStr(ord(s[i])));
        RaiseGDBException('ObjectLRSToText: Invalid character');
      end else
        DbgOut(s[i]);
    end;
    {$ENDIF}
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure OutLn(const s: String);
  begin
    OutStr(s + #13#10);
  end;

  procedure OutString(const s: String);
  var
    res, NewStr: String;
    i: Integer;
    InString, NewInString: Boolean;
  begin
    res := '';
    InString := False;
    for i := 1 to Length(s) do begin
      NewInString := InString;
      case s[i] of
        #0..#31: begin
            if InString then
              NewInString := False;
            NewStr := '#' + IntToStr(Ord(s[i]));
          end;
        '''':
            if InString then NewStr := ''''''
            else NewStr := '''''''';
        else begin
          if not InString then
            NewInString := True;
          NewStr := s[i];
        end;
      end;
      if NewInString <> InString then begin
        NewStr := '''' + NewStr;
        InString := NewInString;
      end;
      res := res + NewStr;
    end;
    if InString then res := res + '''';
    OutStr(res);
  end;

  function ReadInt(ValueType: TValueType): LongInt;
  var
    w: Word;
  begin
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: begin
          w:=ReadLRSWord(Input);
          DebugLn('ReadInt vaInt16 w=',IntToStr(w));
          Result := SmallInt(ReadLRSWord(Input));
        end;
      vaInt32: Result := ReadLRSInteger(Input);
    end;
  end;

  function ReadInt: LongInt;
  begin
    Result := ReadInt(TValueType(Input.ReadByte));
  end;

  function ReadShortString: String;
  var
    len: Byte;
  begin
    len := Input.ReadByte;
    SetLength(Result, len);
    Input.Read(Result[1], len);
  end;

  function ReadLongString: String;
  var
    len: integer;
  begin
    len := ReadLRSInteger(Input);
    SetLength(Result, len);
    Input.Read(Result[1], len);
  end;

  procedure ReadPropList(const indent: String);

    procedure ProcessValue(ValueType: TValueType; const Indent: String);

      procedure Stop(const s: String);
      begin
        RaiseGDBException('ObjectLRSToText '+s);
      end;
      
      procedure UnknownValueType;
      var
        HintStr, s: String;
        HintLen: Int64;
      begin
        s:='';
        case ValueType of
        vaNull: s:='vaNull';
        vaList: s:='vaList';
        vaInt8: s:='vaInt8';
        vaInt16: s:='vaInt16';
        vaInt32: s:='vaInt32';
        vaExtended: s:='vaExtended';
        vaString: s:='vaString';
        vaIdent: s:='vaIdent';
        vaFalse: s:='vaFalse';
        vaTrue: s:='vaTrue';
        vaBinary: s:='vaBinary';
        vaSet: s:='vaSet';
        vaLString: s:='vaLString';
        vaNil: s:='vaNil';
        vaCollection: s:='vaCollection';
        vaSingle: s:='vaSingle';
        vaCurrency: s:='vaCurrency';
        vaDate: s:='vaDate';
        vaWString: s:='vaWString';
        vaInt64: s:='vaInt64';
        end;
        if s<>'' then
          s:='Unimplemented ValueType='+s
        else
          s:='Unknown ValueType='+dbgs(Ord(ValueType));
        HintLen:=Output.Position;
        if HintLen>50 then HintLen:=50;
        SetLength(HintStr,HintLen);
        if HintStr<>'' then begin
          try
            Output.Position:=Output.Position-length(HintStr);
            Output.Read(HintStr[1],length(HintStr));
            debugln('ObjectLRSToText:');
            debugln(DbgStr(HintStr));
          except
          end;
        end;
        s:=s+' ';
        Stop(s);
      end;

      procedure ProcessBinary;
      var
        ToDo, DoNow, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
      begin
        ToDo := ReadLRSCardinal(Input);
        OutLn('{');
        while ToDo > 0 do begin
          DoNow := ToDo;
          if DoNow > 32 then DoNow := 32;
          Dec(ToDo, DoNow);
          s := Indent + '  ';
          Input.Read(lbuf, DoNow);
          for i := 0 to DoNow - 1 do
            s := s + IntToHex(lbuf[i], 2);
          OutLn(s);
        end;
        OutStr(indent);
        OutLn('}');
      end;

    var
      s: String;
      IsFirst: Boolean;
      ext: Extended;
      ASingle: single;
      ADate: TDateTime;
      {$ifdef HASCURRENCY}
      ACurrency: Currency;
      {$endif HASCURRENCY}
      {$ifdef HASWIDESTRING}
      AWideString: WideString;
      {$endif HASWIDESTRING}

    begin
      //DbgOut('ValueType="',dbgs(ord(ValueType)),'"');
      case ValueType of
        vaList: begin
            OutStr('(');
            IsFirst := True;
            while True do begin
              ValueType := TValueType(Input.ReadByte);
              if ValueType = vaNull then break;
              if IsFirst then begin
                OutLn('');
                IsFirst := False;
              end;
              OutStr(Indent + '  ');
              ProcessValue(ValueType, Indent + '  ');
            end;
            OutLn(Indent + ')');
          end;
        vaInt8: begin
            // MG: IntToStr has a bug with ShortInt, therefore these typecasts
            OutLn(IntToStr(Integer(ShortInt(Input.ReadByte))));
          end;
        vaInt16: OutLn(IntToStr(SmallInt(ReadLRSWord(Input))));
        vaInt32: OutLn(IntToStr(ReadLRSInteger(Input)));
        vaInt64: OutLn(IntToStr(ReadLRSInt64(Input)));
        vaExtended: begin
            ext:=ReadLRSExtended(Input);
            OutLn(FloatToStr(ext));
          end;
        vaString: begin
            OutString(ReadShortString);
            OutLn('');
          end;
        vaIdent: OutLn(ReadShortString);
        vaFalse: OutLn('False');
        vaTrue: OutLn('True');
        vaBinary: ProcessBinary;
        vaSet: begin
            OutStr('[');
            IsFirst := True;
            while True do begin
              s := ReadShortString;
              if Length(s) = 0 then break;
              if not IsFirst then OutStr(', ');
              IsFirst := False;
              OutStr(s);
            end;
            OutLn(']');
          end;
        vaLString: begin
            OutString(ReadLongString);
            OutLn('');
          end;
        vaNil:
          OutLn('nil');
        vaCollection: begin
            OutStr('<');
            while Input.ReadByte <> 0 do begin
              OutLn(Indent);
              Input.Seek(-1, soFromCurrent);
              OutStr(indent + '  item');
              ValueType := TValueType(Input.ReadByte);
              if ValueType <> vaList then
                OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
              OutLn('');
              ReadPropList(indent + '    ');
              OutStr(indent + '  end');
            end;
            OutLn('>');
          end;
        vaSingle: begin
            ASingle:=ReadLRSSingle(Input);
            OutLn(FloatToStr(ASingle));
          end;
        vaDate: begin
            ADate:=TDateTime(ReadLRSDouble(Input));
            OutLn(FloatToStr(ADate));
          end;
        {$ifdef HASCURRENCY}
        vaCurrency: begin
            ACurrency:=ReadLRSCurrency(Input);
            OutLn(FloatToStr(ACurrency));
          end;
        {$endif HASCURRENCY}
        {$ifdef HASWIDESTRING}
        vaWString: begin
            AWideString:=ReadLRSWideString(Input);
            OutLn(AWideString);
          end;
        {$endif HASWIDESTRING}
        else
          if ord(ValueType)=20 then begin
            // vaUTF8String
            // Delphi saves widestrings as UTF8 strings
            // The LCL does not use widestrings, but UTF8 directly
            // so, simply read the string
            OutString(ReadLongString);
            OutLn('');
          end else
            UnknownValueType;
      end;
    end;

  begin
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      OutStr(indent + ReadShortString + ' = ');
      ProcessValue(TValueType(Input.ReadByte), Indent);
    end;
  end;

  procedure ReadObject(const indent: String);
  var
    b: Byte;
    ObjClassName, ObjName: String;
    ChildPos: LongInt;
  begin
    // Check for FilerFlags
    b := Input.ReadByte;
    if (b and $f0) = $f0 then begin
      if (b and 2) <> 0 then ChildPos := ReadInt;
    end else begin
      b := 0;
      Input.Seek(-1, soFromCurrent);
    end;

    ObjClassName := ReadShortString;
    ObjName := ReadShortString;

    OutStr(Indent);
    if (b and 1) <> 0 then OutStr('inherited')
    else OutStr('object');
    OutStr(' ');
    if ObjName <> '' then
      OutStr(ObjName + ': ');
    OutStr(ObjClassName);
    if (b and 2) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
    OutLn('');

    ReadPropList(indent + '  ');

    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      ReadObject(indent + '  ');
    end;
    OutLn(indent + 'end');
  end;

begin
  // Endian note: comparing 2 cardinals is endian independent
  if Input.ReadDWord <> PCardinal(@FilerSignature[1])^ then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
  ReadObject('');
end;

function TestFormStreamFormat(Stream: TStream): TLRSStreamOriginalFormat;
var
  Pos: TStreamSeekType;
  Signature: Integer;
begin
  Pos := Stream.Position;
  Signature := 0;
  Stream.Read(Signature, SizeOf(Signature));
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

procedure InternalLRSBinaryToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat;
  ConvertProc: TObjectTextConvertProc;
  BinarySignature: Integer; SignatureLength: Byte);
var
  Pos: TStreamSeekType;
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

procedure LRSObjectTextToBinary(Input, Output: TStream);
var
  parser: TParser;

  procedure WriteShortString(const s: String);
  var
    Size: Integer;
  begin
    Size:=length(s);
    if Size>255 then Size:=255;
    Output.WriteByte(byte(Size));
    if Size > 0 then
      Output.Write(s[1], Size);
  end;

  procedure WriteLongString(const s: String);
  begin
    WriteLRSInteger(Output,Length(s));
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure WriteInteger(value: LongInt);
  begin
    if (value >= -128) and (value <= 127) then begin
      Output.WriteByte(Ord(vaInt8));
      Output.WriteByte(Byte(value));
    end else if (value >= -32768) and (value <= 32767) then begin
      Output.WriteByte(Ord(vaInt16));
      WriteLRSWord(Output,Word(value));
    end else begin
      Output.WriteByte(ord(vaInt32));
      WriteLRSInteger(Output,value);
    end;
  end;

  procedure WriteInt64(const Value: Int64);
  begin
    if (Value >= -$80000000) and (Value <= $7fffffff) then
      WriteInteger(Integer(Value))
    else begin
      Output.WriteByte(ord(vaInt64));
      WriteLRSInt64(Output,Value);
    end;
  end;

  procedure WriteIntegerStr(const s: string);
  begin
    if length(s)>7 then
      WriteInt64(StrToInt64(s))
    else
      WriteInteger(StrToInt(s));
  end;

  procedure ProcessProperty; forward;

  procedure ProcessValue;
  var
    flt: Extended;
    s: String;
    stream: TMemoryStream;
    BinDataSize: LongInt;
  begin
    case parser.Token of
      toInteger:
        begin
          WriteIntegerStr(parser.TokenString);
          parser.NextToken;
        end;
      toFloat:
        begin
          Output.WriteByte(Ord(vaExtended));
          flt := Parser.TokenFloat;
          WriteLRSExtended(Output,flt);
          parser.NextToken;
        end;
      toString:
        begin
          s := parser.TokenString;
          while parser.NextToken = '+' do
          begin
            parser.NextToken;   // Get next string fragment
            parser.CheckToken(toString);
            s := s + parser.TokenString;
          end;
          if length(s)<256 then begin
            Output.WriteByte(Ord(vaString));
            WriteShortString(s);
          end else begin
            Output.WriteByte(Ord(vaLString));
            WriteLongString(s);
          end;
        end;
      toSymbol:
        begin
          if CompareText(parser.TokenString, 'True') = 0 then
            Output.WriteByte(Ord(vaTrue))
          else if CompareText(parser.TokenString, 'False') = 0 then
            Output.WriteByte(Ord(vaFalse))
          else if CompareText(parser.TokenString, 'nil') = 0 then
            Output.WriteByte(Ord(vaNil))
          else
          begin
            Output.WriteByte(Ord(vaIdent));
            WriteShortString(parser.TokenComponentIdent);
          end;
          Parser.NextToken;
        end;
      // Set
      '[':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaSet));
          if parser.Token <> ']' then
            while True do
            begin
              parser.CheckToken(toSymbol);
              WriteShortString(parser.TokenString);
              parser.NextToken;
              if parser.Token = ']' then
                break;
              parser.CheckToken(',');
              parser.NextToken;
            end;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // List
      '(':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaList));
          while parser.Token <> ')' do
            ProcessValue;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // Collection
      '<':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaCollection));
          while parser.Token <> '>' do
          begin
            parser.CheckTokenSymbol('item');
            parser.NextToken;
            // ConvertOrder
            Output.WriteByte(Ord(vaList));
            while not parser.TokenSymbolIs('end') do
              ProcessProperty;
            parser.NextToken;   // Skip 'end'
            Output.WriteByte(0);
          end;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // Binary data
      '{':
        begin
          Output.WriteByte(Ord(vaBinary));
          stream := TMemoryStream.Create;
          try
            parser.HexToBinary(stream);
            BinDataSize:=integer(stream.Size);
            WriteLRSInteger(Output,BinDataSize);
            Output.Write(Stream.Memory^, BinDataSize);
          finally
            stream.Free;
          end;
          parser.NextToken;
        end;
      else
        parser.Error('Invalid Property');
    end;
  end;

  procedure ProcessProperty;
  var
    name: String;
  begin
    // Get name of property
    parser.CheckToken(toSymbol);
    name := parser.TokenString;
    while True do begin
      parser.NextToken;
      if parser.Token <> '.' then break;
      parser.NextToken;
      parser.CheckToken(toSymbol);
      name := name + '.' + parser.TokenString;
    end;
    WriteShortString(name);
    parser.CheckToken('=');
    parser.NextToken;
    ProcessValue;
  end;

  procedure ProcessObject;
  var
    IsInherited: Boolean;
    ObjectName, ObjectType: String;
  begin
    if parser.TokenSymbolIs('OBJECT') then
      IsInherited := False
    else begin
      parser.CheckTokenSymbol('INHERITED');
      IsInherited := True;
    end;
    if IsInherited then ;
    parser.NextToken;
    parser.CheckToken(toSymbol);
    ObjectName := '';
    ObjectType := parser.TokenString;
    parser.NextToken;
    if parser.Token = ':' then begin
      parser.NextToken;
      parser.CheckToken(toSymbol);
      ObjectName := ObjectType;
      ObjectType := parser.TokenString;
      parser.NextToken;
    end;
    WriteShortString(ObjectType);
    WriteShortString(ObjectName);

    // Convert property list
    while not (parser.TokenSymbolIs('END') or
      parser.TokenSymbolIs('OBJECT') or
      parser.TokenSymbolIs('INHERITED')) do
      ProcessProperty;
    Output.WriteByte(0);        // Terminate property list

    // Convert child objects
    while not parser.TokenSymbolIs('END') do ProcessObject;
    parser.NextToken;           // Skip end token
    Output.WriteByte(0);        // Terminate property list
  end;

begin
  parser := TParser.Create(Input);
  try
    Output.Write(FilerSignature, SizeOf(FilerSignature));
    ProcessObject;
  finally
    parser.Free;
  end;
end;

procedure LRSObjectToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);
begin
  InternalLRSBinaryToText(Input, Output, OriginalFormat,
    @LRSObjectBinaryToText, Integer(FilerSignature), sizeof(Integer));
end;

procedure LRSObjectResToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);
begin
  InternalLRSBinaryToText(Input, Output, OriginalFormat,
    @LRSObjectResourceToText, $FF, 1);
end;

procedure LRSObjectResourceToText(Input, Output: TStream);
begin
  Input.ReadResHeader;
  LRSObjectBinaryToText(Input, Output);
end;

procedure FormDataToText(FormStream, TextStream: TStream);
begin
  case TestFormStreamFormat(FormStream) of
  sofBinary:
    LRSObjectResourceToText(FormStream, TextStream);

  sofText:
    TextStream.CopyFrom(FormStream,FormStream.Size);

  else
    raise Exception.Create(rsInvalidFormObjectStream);
  end;
end;

function InitLazResourceComponent(Instance: TComponent;
  RootAncestor: TClass): Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  var
    CompResource: TLResource;
    MemStream: TMemoryStream;
    Reader: TReader;
    DestroyDriver: Boolean;
    Driver: TAbstractObjectReader;
  begin
    //DebugLn('[InitComponent] ',ClassType.Classname,' ',Instance<>nil);
    Result:=false;
    if (ClassType=TComponent) or (ClassType=RootAncestor) then exit;
    if Assigned(ClassType.ClassParent) then
      Result:=InitComponent(ClassType.ClassParent);
    CompResource:=LazarusResources.Find(ClassType.ClassName);
    if (CompResource=nil) or (CompResource.Value='') then exit;
    //DebugLn('[InitComponent] CompResource found for ',ClassType.Classname);
    MemStream:=TMemoryStream.Create;
    try
      MemStream.Write(CompResource.Value[1],length(CompResource.Value));
      MemStream.Position:=0;
      //DebugLn('Form Stream "',ClassType.ClassName,'" Signature=',copy(CompResource.Value,1,4));
      //try
      DestroyDriver:=false;
      Reader := CreateLRSReader(MemStream,DestroyDriver);
      try
        Reader.ReadRootComponent(Instance);
      finally
        Driver:=Reader.Driver;
        Reader.Free;
        if DestroyDriver then Driver.Free;
      end;
      //except
      //  on E: Exception do begin
      //    DebugLn(Format(rsFormStreamingError,[ClassType.ClassName,E.Message]));
      //    exit;
      //  end;
      //end;
    finally
      MemStream.Free;
    end;
    Result:=true;
  end;

begin
  Result:=InitComponent(Instance.ClassType);
end;

function CreateLRSReader(s: TStream; var DestroyDriver: boolean): TReader;
var
  p: Pointer;
  Driver: TAbstractObjectReader;
begin
  Result:=TReader.Create(s,4096);
  {$IFDEF TRANSLATESTRING}
  if Assigned(LRSTranslator) then
   Result.OnReadStringProperty:=@(LRSTranslator.TranslateStringProperty);
  {$ENDIF}
  DestroyDriver:=false;
  if Result.Driver.ClassType=LRSObjectReaderClass then exit;
  // hack to set a write protected variable.
  // DestroyDriver:=true; TReader will free it
  Driver:=LRSObjectReaderClass.Create(s,4096);
  p:=@Result.Driver;
  Result.Driver.Free;
  TAbstractObjectReader(p^):=Driver;
end;

function CreateLRSWriter(s: TStream; var DestroyDriver: boolean): TWriter;
var
  Driver: TAbstractObjectWriter;
begin
  Driver:=LRSObjectWriterClass.Create(s,4096);
  DestroyDriver:=true;
  Result:=TWriter.Create(Driver);
end;

{ LRS format converter functions }

procedure ReverseBytes(p: Pointer; Count: integer);
var
  p1: PChar;
  p2: PChar;
  c: Char;
begin
  p1:=PChar(p);
  p2:=PChar(p)+Count-1;
  while p1<p2 do begin
    c:=p1^;
    p1^:=p2^;
    p2^:=c;
    inc(p1);
    dec(p2);
  end;
end;

procedure ReverseByteOrderInWords(p: PWord; Count: integer);
var
  i: Integer;
  w: Word;
begin
  for i:=0 to Count-1 do begin
    w:=p[i];
    w:=(w shr 8) or ((w and $ff) shl 8);
    p[i]:=w;
  end;
end;

function ConvertLRSExtendedToDouble(p: Pointer): Double;
type
  Ti386ExtendedReversed = packed record
    {$IFDEF Endian_BIG}
    ExponentAndSign: word;
    Mantissa: qword;
    {$ELSE}
    Mantissa: qword;
    ExponentAndSign: word;
    {$ENDIF}
  end;
var
  e: Ti386ExtendedReversed;
  Exponent: word;
  ExponentAndSign: word;
  Mantissa: qword;
begin
  System.Move(p^,e,10);
  {$IFDEF Endian_BIG}
  ReverseBytes(@e,10);
  {$ENDIF}
  // i386 extended
  Exponent:=(e.ExponentAndSign and $7fff);
  if (Exponent>$4000+$3ff) or (Exponent<$4000-$400) then begin
    // exponent out of bounds
    Result:=0;
    exit;
  end;
  dec(Exponent,$4000-$400);
  ExponentAndSign:=Exponent or ((e.ExponentAndSign and $8000) shr 4);
  // i386 extended has leading 1, double has not (shl 1)
  // i386 has 64 bit, double has 52 bit (shr 12)
  Mantissa:=(e.Mantissa shl 1) shr 12;
  // put together
  QWord(Result):=Mantissa or (qword(ExponentAndSign) shl 52);
end;

procedure ConvertEndianBigDoubleToLRSExtended(BigEndianDouble,
  LRSExtended: Pointer);
// Floats consists of a sign bit, some exponent bits and the mantissa bits
// A 0 is all bits 0
// not 0 has always a leading 1, which exponent is stored
// Single/Double does not save the leading 1, Extended does.
//
// Double is 8 bytes long, leftmost bit is sign,
// then 11 bit exponent based $400, then 52 bit mantissa without leading 1
//
// Extended is 10 bytes long, leftmost bit is sign,
// then 15 bit exponent based $4000, then 64 bit mantissa with leading 1
// EndianLittle means reversed byte order
var
  e: array[0..9] of byte;
  i: Integer;
  Exponent: Word;
  d: PByte;
begin
  d:=PByte(BigEndianDouble);
  // convert ppc double to i386 extended
  if (PCardinal(d)[0] or PCardinal(d)[1])=0 then begin
    // 0
    FillChar(LRSExtended^,10,#0);
  end else begin
    Exponent:=((d[0] and $7f) shl 4)+(d[1] shr 4);
    inc(Exponent,$4000-$400);
    if (d[0] and $80)>0 then
      // signed
      inc(Exponent,$8000);
    e[9]:=Exponent shr 8;
    e[8]:=Exponent and $ff;
    e[7]:=($80 or (d[1] shl 3) or (d[2] shr 5)) and $ff;
    for i:=3 to 7 do begin
      e[9-i]:=((d[i-1] shl 3) or (d[i] shr 5)) and $ff;
    end;
    e[1]:=(d[7] shl 3) and $ff;
    e[0]:=0;
    System.Move(e[0],LRSExtended^,10);
  end;
end;

function ReadLRSWord(s: TStream): word;
begin
  s.Read(Result,2);
  {$IFDEF Endian_BIG}
  Result:=((Result and $ff) shl 8) or (Result shr 8);
  {$ENDIF}
end;

function ReadLRSInteger(s: TStream): integer;
begin
  s.Read(Result,4);
  {$IFDEF Endian_BIG}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSCardinal(s: TStream): cardinal;
begin
  s.Read(Result,4);
  {$IFDEF Endian_BIG}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSInt64(s: TStream): int64;
begin
  s.Read(Result,8);
  {$IFDEF Endian_BIG}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSSingle(s: TStream): Single;
begin
  s.Read(Result,4);
  {$IFDEF Endian_BIG}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSDouble(s: TStream): Double;
begin
  s.Read(Result,8);
  {$IFDEF Endian_BIG}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSExtended(s: TStream): Extended;
begin
  {$IFDEF CPUi386}
  s.Read(Result,10);
  {$ENDIF}
  {$IFDEF CPUPowerPC}
  Result:=ReadLRSEndianLittleExtendedAsDouble(s);
  {$ENDIF}
end;

{$ifdef HASCURRENCY}
function ReadLRSCurrency(s: TStream): Currency;
begin
  s.Read(Result,8);
  {$IFDEF Endian_BIG}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;
{$endif HASCURRENCY}

{$ifdef HASWIDESTRING}
function ReadLRSWideString(s: TStream): WideString;
var
  Len: LongInt;
begin
  Len:=ReadLRSInteger(s);
  SetLength(Result,Len);
  if Len>0 then begin
    s.Read(Result[1],Len*2);
    {$IFDEF Endian_BIG}
    ReverseByteOrderInWords(PWord(@Result[1]),Len);
    {$ENDIF}
  end;
end;
{$endif HASWIDESTRING}

function ReadLRSEndianLittleExtendedAsDouble(s: TStream): Double;
var
  e: array[1..10] of byte;
begin
  s.Read(e,10);
  Result:=ConvertLRSExtendedToDouble(@e);
end;

procedure WriteLRSReversedWord(s: TStream; w: word);
begin
  w:=(w shr 8) or ((w and $ff) shl 8);
  s.Write(w,2);
end;

procedure WriteLRS4BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..3] of char;
  i: Integer;
begin
  for i:=0 to 3 do
    a[i]:=PChar(p)[3-i];
  s.Write(a[0],4);
end;

procedure WriteLRS8BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..7] of char;
  i: Integer;
begin
  for i:=0 to 7 do
    a[i]:=PChar(p)[7-i];
  s.Write(a[0],8);
end;

procedure WriteLRS10BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..9] of char;
  i: Integer;
begin
  for i:=0 to 9 do
    a[i]:=PChar(p)[9-i];
  s.Write(a[0],10);
end;

procedure WriteLRSReversedWords(s: TStream; p: Pointer; Count: integer);
var
  w: Word;
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    w:=PWord(P)[i];
    w:=(w shr 8) or ((w and $ff) shl 8);
    s.Write(w,2);
  end;
end;

procedure WriteLRSNull(s: TStream; Count: integer);
var
  c: char;
  i: Integer;
begin
  c:=#0;
  for i:=0 to Count-1 do
    s.Write(c,1);
end;

procedure WriteLRSEndianBigDoubleAsEndianLittleExtended(s: TStream;
  EndBigDouble: PByte);
var
  e: array[0..9] of byte;
begin
  ConvertEndianBigDoubleToLRSExtended(EndBigDouble,@e);
  s.Write(e[0],10);
end;

procedure WriteLRSWord(s: TStream; const w: word);
begin
  {$IFDEF Endian_Little}
  s.Write(w,2);
  {$ELSE}
  WriteLRSReversedWord(s,w);
  {$ENDIF}
end;

procedure WriteLRSInteger(s: TStream; const i: integer);
begin
  {$IFDEF Endian_Little}
  s.Write(i,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@i);
  {$ENDIF}
end;

procedure WriteLRSCardinal(s: TStream; const c: cardinal);
begin
  {$IFDEF Endian_Little}
  s.Write(c,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@c);
  {$ENDIF}
end;

procedure WriteLRSSingle(s: TStream; const si: Single);
begin
  {$IFDEF Endian_Little}
  s.Write(si,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@si);
  {$ENDIF}
end;

procedure WriteLRSDouble(s: TStream; const d: Double);
begin
  {$IFDEF Endian_Little}
  s.Write(d,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@d);
  {$ENDIF}
end;

procedure WriteLRSExtended(s: TStream; const e: extended);
begin
  {$IFDEF CPUi386}
  s.Write(e,10);
  {$ENDIF}
  {$IFDEF CPUPowerPC}
  if SizeOf(e)=10 then
    WriteLRS10BytesReversed(s,@e)
  else if SizeOf(e)=8 then
    WriteLRSEndianBigDoubleAsEndianLittleExtended(s,@e)
  else begin
    debugln('WARNING: WriteLRSExtended not implemented yet for PowerPC');
    WriteLRSNull(s,10);
  end;
  {$ENDIF}
end;

procedure WriteLRSInt64(s: TStream; const i: int64);
begin
  {$IFDEF Endian_Little}
  s.Write(i,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@i);
  {$ENDIF}
end;

{$ifdef HASCURRENCY}
procedure WriteLRSCurrency(s: TStream; const c: Currency);
begin
  {$IFDEF Endian_Little}
  s.Write(c,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@c);
  {$ENDIF}
end;
{$endif HASCURRENCY}

{$ifdef HASWIDESTRING}
procedure WriteLRSWideStringContent(s: TStream; const w: WideString);
var
  Size: Integer;
begin
  Size:=length(w);
  if Size=0 then exit;
  {$IFDEF Endian_Little}
  s.Write(w[1], Size * 2);
  {$ELSE}
  WriteLRSReversedWords(s,@w[1],Size);
  {$ENDIF}
end;
{$endif HASWIDESTRING}

{ TLRSObjectReader }

procedure TLRSObjectReader.Read(var Buf; Count: LongInt);
var
  CopyNow: LongInt;
  Dest: Pointer;
begin
  Dest := @Buf;
  while Count > 0 do
  begin
    if FBufPos >= FBufEnd then
    begin
      FBufEnd := FStream.Read(FBuffer^, FBufSize);
      if FBufEnd = 0 then
        raise EReadError.Create('Read Error');
      FBufPos := 0;
    end;
    CopyNow := FBufEnd - FBufPos;
    if CopyNow > Count then
      CopyNow := Count;
    Move(PChar(FBuffer)[FBufPos], Dest^, CopyNow);
    Inc(FBufPos, CopyNow);
    Dest:=Dest+CopyNow;
    Dec(Count, CopyNow);
  end;
end;

procedure TLRSObjectReader.SkipProperty;
begin
  { Skip property name, then the property value }
  ReadStr;
  SkipValue;
end;

procedure TLRSObjectReader.SkipSetBody;
begin
  while Length(ReadStr) > 0 do;
end;

function TLRSObjectReader.ReadIntegerContent: integer;
begin
  Read(Result,4);
  {$ifdef Endian_BIG}
  ReverseBytes(@Result,4);
  {$endif}
end;

constructor TLRSObjectReader.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TLRSObjectReader.Destroy;
begin
  { Seek back the amount of bytes that we didn't process until now: }
  if Assigned(FStream) then
    FStream.Seek(Integer(FBufPos) - Integer(FBufEnd), soFromCurrent);

  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufSize);

  inherited Destroy;
end;

function TLRSObjectReader.ReadValue: TValueType;
var
  b: byte;
begin
  Result := vaNull; { Necessary in FPC as TValueType is larger than 1 byte! }
  Read(b,1);
  Result:=TValueType(b);
end;

function TLRSObjectReader.NextValue: TValueType;
begin
  Result := ReadValue;
  { We only 'peek' at the next value, so seek back to unget the read value: }
  Dec(FBufPos);
end;

procedure TLRSObjectReader.BeginRootComponent;
var
  Signature: LongInt;
begin
  { Read filer signature }
  Read(Signature,4);
  if Signature <> LongInt(FilerSignature) then
    raise EReadError.Create('Invalid Filer Signature');
end;

procedure TLRSObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
var
  Prefix: Byte;
  ValueType: TValueType;
begin
  { Every component can start with a special prefix: }
  Flags := [];
  if (Byte(NextValue) and $f0) = $f0 then
  begin
    Prefix := Byte(ReadValue);
    Flags := TFilerFlags(longint(Prefix and $0f));
    if ffChildPos in Flags then
    begin
      ValueType := NextValue;
      case ValueType of
        vaInt8:
          AChildPos := ReadInt8;
        vaInt16:
          AChildPos := ReadInt16;
        vaInt32:
          AChildPos := ReadInt32;
        else
          raise EReadError.Create('Invalid Property Value');
      end;
    end;
  end;

  CompClassName := ReadStr;
  CompName := ReadStr;
end;

function TLRSObjectReader.BeginProperty: String;
begin
  Result := ReadStr;
end;

procedure TLRSObjectReader.ReadBinary(const DestData: TMemoryStream);
var
  BinSize: LongInt;
begin
  BinSize:=ReadIntegerContent;
  DestData.Size := BinSize;
  Read(DestData.Memory^, BinSize);
end;

function TLRSObjectReader.ReadFloat: Extended;
{$ifdef Endian_BIG}
var
  e: array[1..10] of byte;
{$endif}
begin
  {$ifdef Endian_BIG}
  if SizeOf(extended)=10 then begin
    Read(Result, 10);
    ReverseBytes(@Result,10);
  end else begin
    Read(e,10);
    Result:=ConvertLRSExtendedToDouble(@e);
  end;
  {$else not Endian_BIG}
  Read(Result, 10);
  {$endif}
end;

function TLRSObjectReader.ReadSingle: Single;
begin
  Read(Result, 4);
  {$ifdef Endian_BIG}
  ReverseBytes(@Result,4);
  {$endif}
end;

{$ifdef HASCURRENCY}
function TLRSObjectReader.ReadCurrency: Currency;
begin
  Read(Result, 8);
  {$ifdef Endian_BIG}
  ReverseBytes(@Result,8);
  {$endif}
end;
{$endif  HASCURRENCY}

function TLRSObjectReader.ReadDate: TDateTime;
begin
  Read(Result, 8);
  {$ifdef Endian_BIG}
  ReverseBytes(@Result,8);
  {$endif}
end;

function TLRSObjectReader.ReadIdent(ValueType: TValueType): String;
var
  b: Byte;
begin
  case ValueType of
    vaIdent:
      begin
        Read(b, 1);
        SetLength(Result, b);
        Read(Result[1], b);
      end;
    vaNil:
      Result := 'nil';
    vaFalse:
      Result := 'False';
    vaTrue:
      Result := 'True';
    vaNull:
      Result := 'Null';
  end;
end;

function TLRSObjectReader.ReadInt8: ShortInt;
begin
  Read(Result, 1);
end;

function TLRSObjectReader.ReadInt16: SmallInt;
begin
  Read(Result, 2);
  {$ifdef Endian_BIG}
  ReverseBytes(@Result,2);
  {$endif}
end;

function TLRSObjectReader.ReadInt32: LongInt;
begin
  Read(Result, 4);
  {$ifdef Endian_BIG}
  ReverseBytes(@Result,4);
  {$endif}
end;

function TLRSObjectReader.ReadInt64: Int64;
begin
  Read(Result, 8);
  {$ifdef Endian_BIG}
  ReverseBytes(@Result,8);
  {$endif}
end;

function TLRSObjectReader.ReadSet(EnumType: Pointer): Integer;
var
  Name: String;
  Value: Integer;
begin
  try
    Result := 0;
    while True do
    begin
      Name := ReadStr;
      if Length(Name) = 0 then
        break;
      Value := GetEnumValue(PTypeInfo(EnumType), Name);
      if Value = -1 then
        raise EReadError.Create('Invalid Property Value');
      Result := Result or (1 shl Value);
    end;
  except
    SkipSetBody;
    raise;
  end;
end;

function TLRSObjectReader.ReadStr: String;
var
  b: Byte;
begin
  Read(b, 1);
  SetLength(Result, b);
  if b > 0 then
    Read(Result[1], b);
end;

function TLRSObjectReader.ReadString(StringType: TValueType): String;
var
  i: Integer;
  b: byte;
begin
  case StringType of
    vaString:
      begin
        Read(b, 1);
        i:=b;
      end;
    vaLString:
      i:=ReadIntegerContent;
  end;
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i);
end;


{$ifdef HASWIDESTRING}
function TLRSObjectReader.ReadWideString: WideString;
var
  i: Integer;
begin
  i:=ReadIntegerContent;
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i*2);
end;
{$endif HASWIDESTRING}


procedure TLRSObjectReader.SkipComponent(SkipComponentInfos: Boolean);
var
  Flags: TFilerFlags;
  Dummy: Integer;
  CompClassName, CompName: String;
begin
  if SkipComponentInfos then
    { Skip prefix, component class name and component object name }
    BeginComponent(Flags, Dummy, CompClassName, CompName);

  { Skip properties }
  while NextValue <> vaNull do
    SkipProperty;
  ReadValue;

  { Skip children }
  while NextValue <> vaNull do
    SkipComponent(True);
  ReadValue;
end;

procedure TLRSObjectReader.SkipValue;

  procedure SkipBytes(Count: LongInt);
  var
    Dummy: array[0..1023] of Byte;
    SkipNow: Integer;
  begin
    while Count > 0 do
    begin
      if Count > 1024 then
        SkipNow := 1024
      else
        SkipNow := Count;
      Read(Dummy, SkipNow);
      Dec(Count, SkipNow);
    end;
  end;

var
  Count: LongInt;
begin
  case ReadValue of
    vaNull, vaFalse, vaTrue, vaNil: ;
    vaList:
      begin
        while NextValue <> vaNull do
          SkipValue;
        ReadValue;
      end;
    vaInt8:
      SkipBytes(1);
    vaInt16:
      SkipBytes(2);
    vaInt32:
      SkipBytes(4);
    vaExtended:
      SkipBytes(10);
    vaString, vaIdent:
      ReadStr;
    vaBinary, vaLString, vaWString:
      begin
        Count:=ReadIntegerContent;
        SkipBytes(Count);
      end;
    vaSet:
      SkipSetBody;
    vaCollection:
      begin
        while NextValue <> vaNull do
        begin
          { Skip the order value if present }
          if NextValue in [vaInt8, vaInt16, vaInt32] then
            SkipValue;
          SkipBytes(1);
          while NextValue <> vaNull do
            SkipProperty;
          ReadValue;
        end;
        ReadValue;
      end;
    vaSingle:
      SkipBytes(4);
    {$ifdef HASCURRENCY}
    vaCurrency:
      SkipBytes(SizeOf(Currency));
    {$endif}
    vaDate:
      SkipBytes(8);
    vaInt64:
      SkipBytes(8);
  else
    RaiseGDBException('TLRSObjectReader.SkipValue unknown valuetype');
  end;
end;

{ TLRSObjectWriter }

procedure TLRSObjectWriter.FlushBuffer;
begin
  FStream.WriteBuffer(FBuffer^, FBufPos);
  FBufPos := 0;
end;

procedure TLRSObjectWriter.Write(const Buffer; Count: LongInt);
var
  CopyNow: LongInt;
  SourceBuf: PChar;
begin
  SourceBuf:=@Buffer;
  while Count > 0 do
  begin
    CopyNow := Count;
    if CopyNow > FBufSize - FBufPos then
      CopyNow := FBufSize - FBufPos;
    Move(SourceBuf^, PChar(FBuffer)[FBufPos], CopyNow);
    Dec(Count, CopyNow);
    Inc(FBufPos, CopyNow);
    SourceBuf:=SourceBuf+CopyNow;
    if FBufPos = FBufSize then
      FlushBuffer;
  end;
end;

procedure TLRSObjectWriter.WriteValue(Value: TValueType);
var
  b: byte;
begin
  b:=byte(Value);
  Write(b, 1);
end;

procedure TLRSObjectWriter.WriteStr(const Value: String);
var
  i: Integer;
  b: Byte;
begin
  i := Length(Value);
  if i > 255 then
    i := 255;
  b:=byte(i);
  Write(b,1);
  if i > 0 then
    Write(Value[1], i);
end;

procedure TLRSObjectWriter.WriteIntegerContent(i: integer);
begin
  {$IFDEF Endian_BIG}
  ReverseBytes(@i,4);
  {$ENDIF}
  Write(i,4);
end;

procedure TLRSObjectWriter.WriteWordContent(w: word);
begin
  {$IFDEF Endian_BIG}
  ReverseBytes(@w,2);
  {$ENDIF}
  Write(w,2);
end;

procedure TLRSObjectWriter.WriteInt64Content(i: int64);
begin
  {$IFDEF Endian_BIG}
  ReverseBytes(@i,8);
  {$ENDIF}
  Write(i,8);
end;

procedure TLRSObjectWriter.WriteSingleContent(s: single);
begin
  {$IFDEF Endian_BIG}
  ReverseBytes(@s,4);
  {$ENDIF}
  Write(s,4);
end;

procedure TLRSObjectWriter.WriteDoubleContent(d: Double);
begin
  {$IFDEF Endian_BIG}
  ReverseBytes(@d,8);
  {$ENDIF}
  Write(d,8);
end;

procedure TLRSObjectWriter.WriteExtendedContent(e: Extended);
{$IFDEF Endian_BIG}
var
  LRSExtended: array[1..10] of byte;
{$endif}
begin
  {$IFDEF Endian_BIG}
    if SizeOf(e)=10 then begin
      ReverseBytes(@e,10);
      Write(e,10);
    end else begin
      ConvertEndianBigDoubleToLRSExtended(@e,@LRSExtended);
      Write(LRSExtended,10);
    end;
  {$ENDIF}
  Write(e,10);
end;

{$ifdef HASCURRENCY}
procedure TLRSObjectWriter.WriteCurrencyContent(c: Currency);
begin
  {$IFDEF Endian_BIG}
  ReverseBytes(@c,8);
  {$ENDIF}
  Write(c,8);
end;
{$endif HASCURRENCY}

{$ifdef HASWIDESTRING}
procedure TLRSObjectWriter.WriteWideStringContent(ws: WideString);
begin
  {$IFDEF Endian_BIG}
  WriteWordsReversed(PWord(@ws[1]),length(ws));
  {$ELSE}
  Write(ws[1],length(ws)*2);
  {$ENDIF}
end;
{$endif HASWIDESTRING}

procedure TLRSObjectWriter.WriteWordsReversed(p: PWord; Count: integer);
var
  i: Integer;
  w: Word;
begin
  for i:=0 to Count-1 do begin
    w:=p[i];
    w:=((w and $ff) shl 8) or (w and $ff);
    Write(w,2);
  end;
end;

procedure TLRSObjectWriter.WriteNulls(Count: integer);
var
  c: Char;
  i: Integer;
begin
  c:=#0;
  for i:=0 to Count-1 do Write(c,1);
end;

constructor TLRSObjectWriter.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TLRSObjectWriter.Destroy;
begin
  // Flush all data which hasn't been written yet
  if Assigned(FStream) then
    FlushBuffer;

  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufSize);

  inherited Destroy;
end;

procedure TLRSObjectWriter.BeginCollection;
begin
  WriteValue(vaCollection);
end;

procedure TLRSObjectWriter.BeginComponent(Component: TComponent;
  Flags: TFilerFlags; ChildPos: Integer);
var
  Prefix: Byte;
begin
  if not FSignatureWritten then
  begin
    Write(FilerSignature, SizeOf(FilerSignature));
    FSignatureWritten := True;
  end;

  { Only write the flags if they are needed! }
  if Flags <> [] then
  begin
    Prefix := Integer(Flags) or $f0;
    Write(Prefix, 1);
    if ffChildPos in Flags then
      WriteInteger(ChildPos);
  end;

  WriteStr(Component.ClassName);
  WriteStr(Component.Name);
end;

procedure TLRSObjectWriter.BeginList;
begin
  WriteValue(vaList);
end;

procedure TLRSObjectWriter.EndList;
begin
  WriteValue(vaNull);
end;

procedure TLRSObjectWriter.BeginProperty(const PropName: String);
begin
  WriteStr(PropName);
end;

procedure TLRSObjectWriter.EndProperty;
begin
end;

procedure TLRSObjectWriter.WriteBinary(const Buffer; Count: LongInt);
begin
  WriteValue(vaBinary);
  WriteIntegerContent(Count);
  Write(Buffer, Count);
end;

procedure TLRSObjectWriter.WriteBoolean(Value: Boolean);
begin
  if Value then
    WriteValue(vaTrue)
  else
    WriteValue(vaFalse);
end;

procedure TLRSObjectWriter.WriteFloat(const Value: Extended);
begin
  WriteValue(vaExtended);
  WriteExtendedContent(Value);
end;

procedure TLRSObjectWriter.WriteSingle(const Value: Single);
begin
  WriteValue(vaSingle);
  WriteSingleContent(Value);
end;

{$ifdef HASCURRENCY}
procedure TLRSObjectWriter.WriteCurrency(const Value: Currency);
begin
  WriteValue(vaCurrency);
  WriteCurrencyContent(Value);
end;
{$endif HASCURRENCY}

procedure TLRSObjectWriter.WriteDate(const Value: TDateTime);
begin
  WriteValue(vaDate);
  WriteDoubleContent(Value);
end;

procedure TLRSObjectWriter.WriteIdent(const Ident: string);
begin
  { Check if Ident is a special identifier before trying to just write
    Ident directly }
  if UpperCase(Ident) = 'NIL' then
    WriteValue(vaNil)
  else if UpperCase(Ident) = 'FALSE' then
    WriteValue(vaFalse)
  else if UpperCase(Ident) = 'TRUE' then
    WriteValue(vaTrue)
  else if UpperCase(Ident) = 'NULL' then
    WriteValue(vaNull) else
  begin
    WriteValue(vaIdent);
    WriteStr(Ident);
  end;
end;

procedure TLRSObjectWriter.WriteInteger(Value: Int64);
var
  w: Word;
  i: Integer;
  b: Byte;
begin
  //writeln('TLRSObjectWriter.WriteInteger Value=',Value);
  // Use the smallest possible integer type for the given value:
  if (Value >= -128) and (Value <= 127) then
  begin
    WriteValue(vaInt8);
    b:=Byte(Value);
    Write(b, 1);
  end else if (Value >= -32768) and (Value <= 32767) then
  begin
    WriteValue(vaInt16);
    w:=Word(Value);
    WriteWordContent(w);
  end else if (Value >= -$80000000) and (Value <= $7fffffff) then
  begin
    WriteValue(vaInt32);
    i:=Integer(Value);
    WriteIntegerContent(i);
  end else
  begin
    WriteValue(vaInt64);
    WriteInt64Content(Value);
  end;
end;

procedure TLRSObjectWriter.WriteMethodName(const Name: String);
begin
  if Length(Name) > 0 then
  begin
    WriteValue(vaIdent);
    WriteStr(Name);
  end else
    WriteValue(vaNil);
end;

procedure TLRSObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
var
  i: Integer;
  Mask: LongInt;
begin
  WriteValue(vaSet);
  Mask := 1;
  for i := 0 to 31 do
  begin
    if (Value and Mask) <> 0 then
      WriteStr(GetEnumName(PTypeInfo(SetType), i));
    Mask := Mask shl 1;
  end;
  WriteStr('');
end;

procedure TLRSObjectWriter.WriteString(const Value: String);
var
  i: Integer;
  b: Byte;
begin
  i := Length(Value);
  if i <= 255 then
  begin
    WriteValue(vaString);
    b:=byte(i);
    Write(b, 1);
  end else
  begin
    WriteValue(vaLString);
    WriteIntegerContent(i);
  end;
  if i > 0 then
    Write(Value[1], i);
end;

{$ifdef HASWIDESTRING}
procedure TLRSObjectWriter.WriteWideString(const Value: WideString);
var
  i: Integer;
begin
  WriteValue(vaWString);
  i := Length(Value);
  WriteIntegerContent(i);
  WriteWideStringContent(Value);
end;
{$endif HASWIDESTRING}


//------------------------------------------------------------------------------
procedure InternalInit;
begin
  LazarusResources:=TLResourceList.Create;
  ByteToStrValid:=false;
  LRSObjectReaderClass:=TLRSObjectReader;
  LRSObjectWriterClass:=TLRSObjectWriter;
end;

initialization
  InternalInit;

finalization
  LazarusResources.Free;
  LazarusResources:=nil;

end.

