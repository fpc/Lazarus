{
 *****************************************************************************
  This file is part of LazUtils

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Convert streamed Lazarus resources from binary to text (TLRSObjBinToTextConvert)
   or vice versa (TLRSObjTextToBinConvert).
   These resources are used for .lfm and .lrs files.
}
unit ProjResConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  ProjResProc, LazTracer, LazLoggerBase;

{$if not declared(toWString)}
  const toWString = char(5);
{$endif}

type
  { TLRPositionLinks }

  TLRPositionLinks = class
  private
    FItems: TFPList;
    FCount: integer;
    function GetData(Index: integer): Pointer;
    function GetLFM(Index: integer): Int64;
    function GetLRS(Index: integer): Int64;
    procedure SetCount(const AValue: integer);
    procedure SetData(Index: integer; const AValue: Pointer);
    procedure SetLFM(Index: integer; const AValue: Int64);
    procedure SetLRS(Index: integer; const AValue: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Sort(LFMPositions: Boolean);
    function IndexOf(const Position: int64; LFMPositions: Boolean): integer;
    function IndexOfRange(const FromPos, ToPos: int64;
                          LFMPositions: Boolean): integer;
    procedure SetPosition(const FromPos, ToPos, MappedPos: int64;
                          LFMtoLRSPositions: Boolean);
    procedure Add(const LFMPos, LRSPos: Int64; AData: Pointer);
  public
    property LFM[Index: integer]: int64 read GetLFM write SetLFM;
    property LRS[Index: integer]: int64 read GetLRS write SetLRS;
    property Data[Index: integer]: Pointer read GetData write SetData;
    property Count: integer read FCount write SetCount;
  end;

  { TLRSObjBinToTextConvert }

  // Used when saving a .lfm form file to disk.
  TLRSObjBinToTextConvert = class
  private
    fInput: TStream;
    fOutput: TStream;
    procedure OutStr(const s: String);
    procedure OutLn(const s: String);
    procedure OutString(const s: String);
    procedure OutWideString(const s: WideString);
    function ReadInt(ValueType: TValueType): LongInt;
    function ReadInt: LongInt;
    function ReadShortString: String;
    function ReadLongString: String;
    function ValueTypeAsString(ValueType: TValueType): string;
    procedure UnknownValueType(ValueType: TValueType);
    procedure ProcessBinary(const Indent: String);
    procedure ProcessValue(ValueType: TValueType; const Indent: String);
    procedure ReadPropList(const indent: String);
    procedure ReadObject(const indent: String);
  public
    constructor Create(aInput, aOutput: TStream);
    destructor Destroy; override;
    procedure Run;
  end;

  { TLRSObjTextToBinConvert }

  // Used when reading a .lfm form file from disk.
  TLRSObjTextToBinConvert = class
  private
    fInput: TStream;
    fOutput: TStream;
    fLinks: TLRPositionLinks;
    fParser: TParser;
    fTokenStartPos: LongInt;
    procedure WriteShortString(const s: String);
    procedure WriteLongString(const s: String);
    procedure WriteWideString(const s: WideString);
    procedure WriteInteger(value: LongInt);
    procedure WriteInt64(const Value: Int64);
    procedure WriteIntegerStr(const s: string);
    function ParserNextToken: Char;
    procedure ProcessValue;
    procedure ProcessProperty;
    procedure ProcessObject;
  public
    constructor Create(aInput, aOutput: TStream; aLinks: TLRPositionLinks);
    destructor Destroy; override;
    procedure Run;
  end;

// A wrapper procedure for TLRSObjBinToTextConvert
procedure LRSObjectBinaryToText(aInput, aOutput: TStream); // binary to lfm
// A wrapper procedure for TLRSObjTextToBinConvert
procedure LRSObjectTextToBinary(aInput, aOutput: TStream;  // lfm to binary
                                aLinks: TLRPositionLinks = nil);

implementation

type
  TLRPositionLink = record
    LFMPosition: int64;
    LRSPosition: int64;
    Data: Pointer;
  end;
  PLRPositionLink = ^TLRPositionLink;


function CompareLRPositionLinkWithLFMPosition(Item1, Item2: Pointer): integer;
var
  p1: Int64;
  p2: Int64;
begin
  p1:=PLRPositionLink(Item1)^.LFMPosition;
  p2:=PLRPositionLink(Item2)^.LFMPosition;
  if p1<p2 then
    Result:=1
  else if p1>p2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareLRPositionLinkWithLRSPosition(Item1, Item2: Pointer): integer;
var
  p1: Int64;
  p2: Int64;
begin
  p1:=PLRPositionLink(Item1)^.LRSPosition;
  p2:=PLRPositionLink(Item2)^.LRSPosition;
  if p1<p2 then
    Result:=1
  else if p1>p2 then
    Result:=-1
  else
    Result:=0;
end;

procedure LRSObjectBinaryToText(aInput, aOutput: TStream);
var
  Conv: TLRSObjBinToTextConvert;
begin
  Conv := TLRSObjBinToTextConvert.Create(aInput, aOutput);
  try
    Conv.Run;
  finally
    Conv.Free;
  end;
end;

procedure LRSObjectTextToBinary(aInput, aOutput: TStream; aLinks: TLRPositionLinks);
var
  Conv: TLRSObjTextToBinConvert;
begin
  Conv := TLRSObjTextToBinConvert.Create(aInput, aOutput, aLinks);
  try
    Conv.Run;
  finally
    Conv.Free;
  end;
end;

{ TLRPositionLinks }

function TLRPositionLinks.GetLFM(Index: integer): Int64;
begin
  Result:=PLRPositionLink(FItems[Index])^.LFMPosition;
end;

function TLRPositionLinks.GetData(Index: integer): Pointer;
begin
  Result:=PLRPositionLink(FItems[Index])^.Data;
end;

function TLRPositionLinks.GetLRS(Index: integer): Int64;
begin
  Result:=PLRPositionLink(FItems[Index])^.LRSPosition;
end;

procedure TLRPositionLinks.SetCount(const AValue: integer);
var
  i: LongInt;
  Item: PLRPositionLink;
begin
  if FCount=AValue then exit;
  // free old items
  for i:=AValue to FCount-1 do begin
    Item:=PLRPositionLink(FItems[i]);
    Dispose(Item);
  end;
  // create new items
  FItems.Count:=AValue;
  for i:=FCount to AValue-1 do begin
    New(Item);
    Item^.LFMPosition:=-1;
    Item^.LRSPosition:=-1;
    Item^.Data:=nil;
    FItems[i]:=Item;
  end;
  FCount:=AValue;
end;

procedure TLRPositionLinks.SetData(Index: integer; const AValue: Pointer);
begin
  PLRPositionLink(FItems[Index])^.Data:=AValue;
end;

procedure TLRPositionLinks.SetLFM(Index: integer; const AValue: Int64);
begin
  PLRPositionLink(FItems[Index])^.LFMPosition:=AValue;
end;

procedure TLRPositionLinks.SetLRS(Index: integer; const AValue: Int64);
begin
  PLRPositionLink(FItems[Index])^.LRSPosition:=AValue;
end;

constructor TLRPositionLinks.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TLRPositionLinks.Destroy;
begin
  Count:=0;
  FItems.Free;
  inherited Destroy;
end;

procedure TLRPositionLinks.Clear;
begin
  Count:=0;
end;

procedure TLRPositionLinks.Sort(LFMPositions: Boolean);
begin
  if LFMPositions then
    FItems.Sort(@CompareLRPositionLinkWithLFMPosition)
  else
    FItems.Sort(@CompareLRPositionLinkWithLRSPosition)
end;

function TLRPositionLinks.IndexOf(const Position: int64; LFMPositions: Boolean): integer;
var
  l, r, m: integer;
  p: Int64;
begin
  // binary search for the line
  l:=0;
  r:=FCount-1;
  while r>=l do begin
    m:=(l+r) shr 1;
    if LFMPositions then
      p:=PLRPositionLink(FItems[m])^.LFMPosition
    else
      p:=PLRPositionLink(FItems[m])^.LRSPosition;
    if p>Position then
      r:=m-1    // too high, search lower
    else if p<Position then
      l:=m+1    // too low, search higher
    else
      exit(m);  // position found
  end;
  Result:=-1;
end;

function TLRPositionLinks.IndexOfRange(const FromPos, ToPos: int64;
  LFMPositions: Boolean): integer;
var
  l, r, m: integer;
  p: Int64;
  Item: PLRPositionLink;
begin
  // binary search for the line
  l:=0;
  r:=FCount-1;
  while r>=l do begin
    m:=(l+r) shr 1;
    Item:=PLRPositionLink(FItems[m]);
    if LFMPositions then
      p:=Item^.LFMPosition
    else
      p:=Item^.LRSPosition;
    if p>=ToPos then
      r:=m-1    // too high, search lower
    else if p<FromPos then
      l:=m+1    // too low, search higher
    else
      exit(m);  // position found
  end;
  Result:=-1;
end;

procedure TLRPositionLinks.SetPosition(const FromPos, ToPos, MappedPos: int64;
  LFMtoLRSPositions: Boolean);
var
  i: LongInt;
begin
  i:=IndexOfRange(FromPos,ToPos,LFMtoLRSPositions);
  if i>=0 then
    if LFMtoLRSPositions then
      PLRPositionLink(FItems[i])^.LRSPosition:=MappedPos
    else
      PLRPositionLink(FItems[i])^.LFMPosition:=MappedPos;
end;

procedure TLRPositionLinks.Add(const LFMPos, LRSPos: Int64; AData: Pointer);
var
  Item: PLRPositionLink;
begin
  Count:=Count+1;
  Item:=PLRPositionLink(FItems[Count-1]);
  Item^.LFMPosition:=LFMPos;
  Item^.LRSPosition:=LRSPos;
  Item^.Data:=AData;
end;

{ TLRSObjBinToTextConvert }

constructor TLRSObjBinToTextConvert.Create(aInput, aOutput: TStream);
begin
  fInput := aInput;
  fOutput := aOutput;
end;

destructor TLRSObjBinToTextConvert.Destroy;
begin
  inherited Destroy;
end;

procedure TLRSObjBinToTextConvert.OutStr(const s: String);
{$IFDEF VerboseLRSObjectBinaryToText}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF VerboseLRSObjectBinaryToText}
  for i:=1 to length(s) do begin
    if (s[i] in [#0..#8,#11..#12,#14..#31]) then begin
      DbgOut('#'+IntToStr(ord(s[i])));
      RaiseGDBException('OutStr: Invalid character');
    end else
      DbgOut(s[i]);
  end;
  {$ENDIF}
  if Length(s) > 0 then
    fOutput.Write(s[1], Length(s));
end;

procedure TLRSObjBinToTextConvert.OutLn(const s: String);
begin
  OutStr(s + LineEnding);
end;

procedure TLRSObjBinToTextConvert.OutString(const s: String);
var
  res, NewStr: String;
  i: Integer;
  InString, NewInString: Boolean;
begin
  if s<>'' then begin
    res := '';
    InString := False;
    for i := 1 to Length(s) do begin
      NewInString := InString;
      case s[i] of
        #0..#31: begin
            NewInString := False;
            NewStr := '#' + IntToStr(Ord(s[i]));
          end;
        '''': begin
            NewInString := True;
            NewStr:=''''''; // write two ticks, so the reader will read one
          end;
        else begin
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
  end else begin
    res:='''''';
  end;
  OutStr(res);
end;

procedure TLRSObjBinToTextConvert.OutWideString(const s: WideString);
// write as normal string
var
  res, NewStr: String;
  i: Integer;
  InString, NewInString: Boolean;
begin
  //debugln('OutWideString ',s);
  res := '';
  if s<>'' then begin
    InString := False;
    for i := 1 to Length(s) do begin
      NewInString := InString;
      if (ord(s[i])<ord(' ')) or (ord(s[i])>=127) then begin
        // special char
        NewInString := False;
        NewStr := '#' + IntToStr(Ord(s[i]));
      end
      else if s[i]='''' then begin
        // '
        if InString then
          NewStr := ''''''
        else
          NewStr := '''''''';
      end
      else begin
        // normal char
        NewInString := True;
        NewStr := AnsiString(s[i]);
      end;
      if NewInString <> InString then begin
        NewStr := '''' + NewStr;
        InString := NewInString;
      end;
      res := res + NewStr;
    end;
    if InString then res := res + '''';
  end else begin
    res:='''''';
  end;
  OutStr(res);
end;

function TLRSObjBinToTextConvert.ReadInt(ValueType: TValueType): LongInt;
var
  w: Word;
begin
  case ValueType of
    vaInt8: Result := ShortInt(fInput.ReadByte);
    vaInt16: begin
        w:=ReadLRSWord(fInput);
        //DebugLn('ReadInt vaInt16 w=',IntToStr(w));
        Result := SmallInt(w);
      end;
    vaInt32: Result := ReadLRSInteger(fInput);
    else Result := 0;
  end;
end;

function TLRSObjBinToTextConvert.ReadInt: LongInt;
begin
  Result := ReadInt(TValueType(fInput.ReadByte));
end;

function TLRSObjBinToTextConvert.ReadShortString: String;
var
  len: Byte;
begin
  Result:='';
  len := fInput.ReadByte;
  SetLength(Result, len);
  if (Len > 0) then
    fInput.Read(Result[1], len);
end;

function TLRSObjBinToTextConvert.ReadLongString: String;
var
  len: integer;
begin
  Result:='';
  len := ReadLRSInteger(fInput);
  SetLength(Result, len);
  if (Len > 0) then
    fInput.Read(Result[1], len);
end;

function TLRSObjBinToTextConvert.ValueTypeAsString(ValueType: TValueType): string;
begin
  case ValueType of
  vaNull: Result:='vaNull';
  vaList: Result:='vaList';
  vaInt8: Result:='vaInt8';
  vaInt16: Result:='vaInt16';
  vaInt32: Result:='vaInt32';
  vaExtended: Result:='vaExtended';
  vaString: Result:='vaString';
  vaIdent: Result:='vaIdent';
  vaFalse: Result:='vaFalse';
  vaTrue: Result:='vaTrue';
  vaBinary: Result:='vaBinary';
  vaSet: Result:='vaSet';
  vaLString: Result:='vaLString';
  vaNil: Result:='vaNil';
  vaCollection: Result:='vaCollection';
  vaSingle: Result:='vaSingle';
  vaCurrency: Result:='vaCurrency';
  vaDate: Result:='vaDate';
  vaWString: Result:='vaWString';
  vaInt64: Result:='vaInt64';
  vaUTF8String: Result:='vaUTF8String';
  vaUString: Result:='vaUString';
  vaQWord : Result:='vaQWord';
  else Result:='Unknown ValueType='+dbgs(Ord(ValueType));
  end;
end;

procedure TLRSObjBinToTextConvert.UnknownValueType(ValueType: TValueType);
{$IFNDEF DisableChecks}
var
  HintStr: string;
  HintLen: Int64;
{$ENDIF}
begin
  {$IFNDEF DisableChecks}
  HintLen:=fOutput.Position;
  if HintLen>50 then HintLen:=50;
  SetLength(HintStr,HintLen);
  if HintStr<>'' then begin
    try
      fOutput.Position:=fOutput.Position-length(HintStr);
      fOutput.Read(HintStr[1],length(HintStr));
      //debugln('UnknownValueType:');
      debugln(DbgStr(HintStr));
    except
    end;
  end;
  {$ENDIF}
  RaiseGDBException('UnknownValueType '+ValueTypeAsString(ValueType)+' ');
end;

procedure TLRSObjBinToTextConvert.ProcessBinary(const Indent: String);
var
  ToDo, DoNow, StartPos, i: LongInt;
  lbuf: array[0..31] of Byte;
  s: String;
  p: pchar;
const
  HexDigits: array[0..$F] of char = '0123456789ABCDEF';
begin
  ToDo := ReadLRSCardinal(fInput);
  OutLn('{');
  while ToDo > 0 do begin
    DoNow := ToDo;
    if DoNow > 32 then DoNow := 32;
    Dec(ToDo, DoNow);
    s := Indent + '  ';
    StartPos := length(s);
    fInput.Read(lbuf, DoNow);
    setlength(s, StartPos+DoNow*2);
    p := @s[StartPos];
    for i := 0 to DoNow - 1 do begin
      inc(p);
      p^ := HexDigits[(lbuf[i] shr 4) and $F];
      inc(p);
      p^ := HexDigits[lbuf[i] and $F];
    end;
    OutLn(s);
  end;
  OutStr(indent);
  OutLn('}');
end;

procedure TLRSObjBinToTextConvert.ProcessValue(ValueType: TValueType; const Indent: String);
var
  s: String;
  IsFirst: Boolean;
  ext: Extended;
  ASingle: single;
  ADate: TDateTime;
  ACurrency: Currency;
  AWideString: WideString;
begin
  //DebugLn(['ProcessValue ',Indent,' ValueType="',ValueTypeAsString(ValueType),'"']);
  case ValueType of
    vaList: begin
        OutStr('(');
        IsFirst := True;
        while True do begin
          ValueType := TValueType(fInput.ReadByte);
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
        OutLn(IntToStr(Integer(ShortInt(fInput.ReadByte))));
      end;
    vaInt16: OutLn(IntToStr(SmallInt(ReadLRSWord(fInput))));
    vaInt32: OutLn(IntToStr(ReadLRSInteger(fInput)));
    vaInt64: OutLn(IntToStr(ReadLRSInt64(fInput)));
    vaExtended: begin
        ext:=ReadLRSExtended(fInput);
        OutLn(FloatToStr(ext));
      end;
    vaString: begin
        OutString(ReadShortString);
        OutLn('');
      end;
    vaIdent: OutLn(ReadShortString);
    vaFalse: OutLn('False');
    vaTrue: OutLn('True');
    vaBinary: ProcessBinary(Indent);
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
        while fInput.ReadByte <> 0 do begin
          OutLn(Indent);
          fInput.Seek(-1, soFromCurrent);
          OutStr(indent + '  item');
          ValueType := TValueType(fInput.ReadByte);
          if ValueType <> vaList then
            OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
          OutLn('');
          ReadPropList(indent + '    ');
          OutStr(indent + '  end');
        end;
        OutLn('>');
      end;
    vaSingle: begin
        ASingle:=ReadLRSSingle(fInput);
        OutLn(FloatToStr(ASingle) + 's');
      end;
    vaDate: begin
        ADate:=TDateTime(ReadLRSDouble(fInput));
        OutLn(FloatToStr(ADate) + 'd');
      end;
    vaCurrency: begin
        ACurrency:=ReadLRSCurrency(fInput);
        OutLn(FloatToStr(ACurrency * 10000) + 'c');
      end;
    vaWString,vaUString: begin
        AWideString:=ReadLRSWideString(fInput);
        OutWideString(AWideString);
        OutLn('');
      end;
    else
      if ord(ValueType)=20 then begin
        // vaUTF8String
        // Delphi saves widestrings as UTF8 strings
        // The LCL does not use widestrings, but UTF8 directly
        // so, simply read and write the string
        OutString(ReadLongString);
        OutLn('');
      end else
        UnknownValueType(ValueType);
  end;
end;

procedure TLRSObjBinToTextConvert.ReadPropList(const indent: String);
var
  NextByte: Byte;
begin
  while fInput.ReadByte <> 0 do begin
    fInput.Seek(-1, soFromCurrent);
    OutStr(indent + ReadShortString + ' = ');
    NextByte:=fInput.ReadByte;
    if NextByte<>0 then
      ProcessValue(TValueType(NextByte), Indent)
    else
      OutLn('');
  end;
end;

procedure TLRSObjBinToTextConvert.ReadObject(const indent: String);
var
  b: Byte;
  ObjClassName, ObjName: String;
  ChildPos: LongInt;
begin
  ChildPos := 0;
  // Check for FilerFlags
  b := fInput.ReadByte;
  if (b and $f0) = $f0 then begin
    if (b and ObjStreamMaskChildPos) <> 0 then
      ChildPos := ReadInt;
  end else begin
    b := 0;
    fInput.Seek(-1, soFromCurrent);
  end;

  ObjClassName := ReadShortString;
  ObjName := ReadShortString;

  OutStr(Indent);
  if (b and ObjStreamMaskInherited) <> 0 then
    OutStr('inherited')
  else if (b and ObjStreamMaskInline) <> 0 then
    OutStr('inline')
  else
    OutStr('object');
  OutStr(' ');
  if ObjName <> '' then
    OutStr(ObjName + ': ');
  OutStr(ObjClassName);
  if (b and ObjStreamMaskChildPos) <> 0 then
    OutStr('[' + IntToStr(ChildPos) + ']');
  OutLn('');

  ReadPropList(indent + '  ');

  while fInput.ReadByte <> 0 do begin
    fInput.Seek(-1, soFromCurrent);
    ReadObject(indent + '  ');
  end;
  OutLn(indent + 'end');
end;

procedure TLRSObjBinToTextConvert.Run;
var
  OldDecimalSeparator, OldThousandSeparator: Char;
  Signature: TFilerSignature;
begin
  // Endian note: comparing 2 cardinals is endian independent
  Signature:='1234';
  fInput.Read(Signature[1], length(Signature));
  if Signature<>FilerSignature then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
  debugln(['TLRSObjBinToTextConvert.Run Signature=', string(Signature)]);
  OldDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  OldThousandSeparator:=DefaultFormatSettings.ThousandSeparator;
  DefaultFormatSettings.ThousandSeparator:=',';
  try
    ReadObject('');
  finally
    DefaultFormatSettings.DecimalSeparator:=OldDecimalSeparator;
    DefaultFormatSettings.ThousandSeparator:=OldThousandSeparator;
  end;
end;

{ TLRSObjTextToBinConvert }

constructor TLRSObjTextToBinConvert.Create(aInput, aOutput: TStream;
  aLinks: TLRPositionLinks);
begin
  fInput := aInput;
  fOutput := aOutput;
  fLinks := aLinks;
end;

destructor TLRSObjTextToBinConvert.Destroy;
begin
  inherited Destroy;
end;

procedure TLRSObjTextToBinConvert.WriteShortString(const s: String);
var
  Size: Integer;
begin
  Size:=length(s);
  if Size>255 then Size:=255;
  fOutput.WriteByte(byte(Size));
  if Size > 0 then
    fOutput.Write(s[1], Size);
end;

procedure TLRSObjTextToBinConvert.WriteLongString(const s: String);
begin
  WriteLRSInteger(fOutput,Length(s));
  if Length(s) > 0 then
    fOutput.Write(s[1], Length(s));
end;

procedure TLRSObjTextToBinConvert.WriteWideString(const s: WideString);
begin
  WriteLRSInteger(fOutput,Length(s));
  if Length(s) > 0 then
    fOutput.Write(s[1], Length(s)*2);
end;

procedure TLRSObjTextToBinConvert.WriteInteger(value: LongInt);
begin
  if (value >= -128) and (value <= 127) then begin
    fOutput.WriteByte(Ord(vaInt8));
    fOutput.WriteByte(Byte(value));
  end else if (value >= -32768) and (value <= 32767) then begin
    fOutput.WriteByte(Ord(vaInt16));
    WriteLRSWord(fOutput,Word(value));
  end else begin
    fOutput.WriteByte(ord(vaInt32));
    WriteLRSInteger(fOutput,value);
  end;
end;

procedure TLRSObjTextToBinConvert.WriteInt64(const Value: Int64);
begin
  if (Value >= -$80000000) and (Value <= $7fffffff) then
    WriteInteger(Integer(Value))
  else begin
    fOutput.WriteByte(ord(vaInt64));
    WriteLRSInt64(fOutput,Value);
  end;
end;

procedure TLRSObjTextToBinConvert.WriteIntegerStr(const s: string);
begin
  if length(s)>7 then
    WriteInt64(StrToInt64(s))
  else
    WriteInteger(StrToInt(s));
end;

function TLRSObjTextToBinConvert.ParserNextToken: Char;
begin
  fTokenStartPos:=fParser.SourcePos;
  Result:=fParser.NextToken;
  if fLinks<>nil then
    fLinks.SetPosition(fTokenStartPos,fParser.SourcePos,fOutput.Position,true);
end;

procedure TLRSObjTextToBinConvert.ProcessValue;
  procedure RaiseValueExpected;
  begin
    fParser.Error('Value expected, but '+fParser.TokenString+' found');
  end;
var
  flt: Extended;
  stream: TMemoryStream;
  BinDataSize: LongInt;
  toStringBuf: String;
begin
  if fParser.TokenSymbolIs('END') then exit;
  if fParser.TokenSymbolIs('OBJECT') then
    RaiseValueExpected;
  case fParser.Token of
    Classes.toInteger:
      begin
        WriteIntegerStr(fParser.TokenString);
        ParserNextToken;
      end;
    Classes.toFloat:
      begin
        flt := fParser.TokenFloat;
        case fParser.FloatType of
          's': begin
            fOutput.WriteByte(Ord(vaSingle));
            WriteLRSSingle(fOutput,flt);
          end;
          'd': begin
            fOutput.WriteByte(Ord(vaDate));
            WriteLRSDouble(fOutput,flt);
          end;
          'c': begin
            fOutput.WriteByte(Ord(vaCurrency));
            WriteLRSCurrency(fOutput,flt/10000);
          end;
          else
          begin
            fOutput.WriteByte(Ord(vaExtended));
            WriteLRSExtended(fOutput,flt);
          end;
        end;
        ParserNextToken;
      end;
    Classes.toString:
      begin
        toStringBuf := fParser.TokenString;
        //DebugLn(['ProcessValue toStringBuf="',toStringBuf,'" ',dbgstr(toStringBuf)]);
        while ParserNextToken = '+' do
        begin
          ParserNextToken;   // Get next string fragment
          if not (fParser.Token in [Classes.toString,toWString]) then
            fParser.CheckToken(Classes.toString);
          toStringBuf := toStringBuf + fParser.TokenString;
        end;
        if length(toStringBuf)<256 then begin
          //debugln('ProcessValue WriteShortString');
          fOutput.WriteByte(Ord(vaString));
          WriteShortString(toStringBuf);
        end else begin
          //debugln('ProcessValue WriteLongString');
          fOutput.WriteByte(Ord(vaLString));
          WriteLongString(toStringBuf);
        end;
      end;
    Classes.toWString:
      begin
        toStringBuf := fParser.TokenString;
        //DebugLn(['ProcessValue toStringBuf="',toStringBuf,'" ',dbgstr(toStringBuf)]);
        while ParserNextToken = '+' do
        begin
          ParserNextToken;   // Get next string fragment
          if not (fParser.Token in [Classes.toString,toWString]) then
            fParser.CheckToken(Classes.toString);
          toStringBuf := toStringBuf + fParser.TokenString;
        end;
        fOutput.WriteByte(Ord(vaWString));
        WriteWideString(UTF8Decode(toStringBuf));
      end;
    Classes.toSymbol:
      begin
        if CompareText(fParser.TokenString, 'True') = 0 then
          fOutput.WriteByte(Ord(vaTrue))
        else if CompareText(fParser.TokenString, 'False') = 0 then
          fOutput.WriteByte(Ord(vaFalse))
        else if CompareText(fParser.TokenString, 'nil') = 0 then
          fOutput.WriteByte(Ord(vaNil))
        else
        begin
          fOutput.WriteByte(Ord(vaIdent));
          WriteShortString(fParser.TokenComponentIdent);
        end;
        ParserNextToken;
      end;
    // Set
    '[':
      begin
        ParserNextToken;
        fOutput.WriteByte(Ord(vaSet));
        if fParser.Token <> ']' then
          while True do
          begin
            fParser.CheckToken(toSymbol);
            WriteShortString(fParser.TokenString);
            ParserNextToken;
            if fParser.Token = ']' then
              break;
            fParser.CheckToken(',');
            ParserNextToken;
          end;
        fOutput.WriteByte(0);
        ParserNextToken;
      end;
    // List
    '(':
      begin
        fOutput.WriteByte(Ord(vaList));
        ParserNextToken;
        while fParser.Token <> ')' do
          ProcessValue;
        fOutput.WriteByte(0);
        ParserNextToken;
      end;
    // Collection
    '<':
      begin
        ParserNextToken;
        fOutput.WriteByte(Ord(vaCollection));
        while fParser.Token <> '>' do
        begin
          fParser.CheckTokenSymbol('item');
          ParserNextToken;
          // ConvertOrder
          fOutput.WriteByte(Ord(vaList));
          while not fParser.TokenSymbolIs('end') do
            ProcessProperty;
          ParserNextToken;   // Skip 'end'
          fOutput.WriteByte(0);
        end;
        fOutput.WriteByte(0);
        ParserNextToken;
      end;
    // Binary data
    '{':
      begin
        fOutput.WriteByte(Ord(vaBinary));
        stream := TMemoryStream.Create;
        try
          fParser.HexToBinary(stream);
          BinDataSize:=integer(stream.Size);
          WriteLRSInteger(fOutput,BinDataSize);
          fOutput.Write(Stream.Memory^, BinDataSize);
          Stream.Position:=0;
          //debugln('ProcessValue binary data "',dbgMemStream(Stream,30),'"');
        finally
          stream.Free;
        end;
        ParserNextToken;
      end;
    else
      fParser.Error('Invalid Property');
  end;
end;

procedure TLRSObjTextToBinConvert.ProcessProperty;
var
  name: String;
begin
  // Get name of property
  fParser.CheckToken(toSymbol);
  name := fParser.TokenString;
  while True do begin
    ParserNextToken;
    if fParser.Token <> '.' then break;
    ParserNextToken;
    fParser.CheckToken(toSymbol);
    name := name + '.' + fParser.TokenString;
  end;
  WriteShortString(name);
  fParser.CheckToken('=');
  ParserNextToken;
  ProcessValue;
end;

procedure TLRSObjTextToBinConvert.ProcessObject;
var
  Flags: Byte;
  ChildPos: Integer;
  ObjectName, ObjectType: String;
begin
  if fParser.TokenSymbolIs('OBJECT') then
    Flags :=0  { IsInherited := False }
  else if fParser.TokenSymbolIs('INHERITED') then
    Flags := 1 { IsInherited := True; }
  else begin
    fParser.CheckTokenSymbol('INLINE');
    Flags := 4;
  end;
  ParserNextToken;
  fParser.CheckToken(toSymbol);
  if fParser.TokenSymbolIs('END') then begin
    // 'object end': no name, no content
    // this is normally invalid, but Delphi can create this, so ignore it
    exit;
  end;
  ObjectName := '';
  ObjectType := fParser.TokenString;
  ParserNextToken;
  ChildPos := 0;
  if fParser.Token = ':' then begin
    ParserNextToken;
    fParser.CheckToken(toSymbol);
    ObjectName := ObjectType;
    ObjectType := fParser.TokenString;
    ParserNextToken;
    if fParser.Token = '[' then begin
      ParserNextToken;
      ChildPos := fParser.TokenInt;
      ParserNextToken;
      fParser.CheckToken(']');
      ParserNextToken;
      Flags := Flags or ObjStreamMaskChildPos;
    end;
  end;
  if Flags <> 0 then begin
    fOutput.WriteByte($f0 or Flags);
    if (Flags and ObjStreamMaskChildPos) <> 0 then
      WriteInteger(ChildPos);
  end;
  WriteShortString(ObjectType);
  WriteShortString(ObjectName);

  // Convert property list
  while not (fParser.TokenSymbolIs('END') or
    fParser.TokenSymbolIs('OBJECT') or
    fParser.TokenSymbolIs('INHERITED') or
    fParser.TokenSymbolIs('INLINE'))
  do
    ProcessProperty;
  fOutput.WriteByte(0);        // Terminate property list

  // Convert child objects
  while not fParser.TokenSymbolIs('END') do
    ProcessObject;
  ParserNextToken;            // Skip end token
  fOutput.WriteByte(0);        // Terminate property list
end;

procedure TLRSObjTextToBinConvert.Run;
var
  OldDecimalSeparator, OldThousandSeparator: Char;
  Count: Integer;
begin
  if fLinks<>nil then
    fLinks.Sort(true);    // sort fLinks for LFM positions
  //debugln(['TLRSObjTextToBinConvert.Run Start']);
  fParser := TParser.Create(fInput);
  OldDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  OldThousandSeparator:=DefaultFormatSettings.ThousandSeparator;
  DefaultFormatSettings.ThousandSeparator:=',';
  try
    Count:=0;
    repeat
      fOutput.Write(FilerSignature[1], length(FilerSignature));
      ProcessObject;
      inc(Count);
    until fParser.TokenString='';
    if Count>1 then
      fOutput.WriteByte(0);        // Terminate object list
  finally
    fParser.Free;
    DefaultFormatSettings.DecimalSeparator:=OldDecimalSeparator;
    DefaultFormatSettings.ThousandSeparator:=OldThousandSeparator;
  end;
end;

end.

