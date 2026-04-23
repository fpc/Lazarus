{
 *****************************************************************************
  This file is part of LazUtils

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   A lazarus resource is an ansistring, with a name and a valuetype. Both, name
   and valuetype, are ansistrings as well.
   Lazarus resources are normally included via an include directive in the
   initialization part of a unit. To create such include files use the
   BinaryToLazarusResourceCode procedure.
}
unit ProjResProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazTracer, LazLoggerBase;

type
  TFilerSignature = array[1..4] of Char;

  TLRPositionLink = record
    LFMPosition: int64;
    LRSPosition: int64;
    Data: Pointer;
  end;
  PLRPositionLink = ^TLRPositionLink;

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

const
  ObjStreamMaskInherited = 1;
  ObjStreamMaskChildPos  = 2;
  ObjStreamMaskInline    = 4;


// This all was moved from LResources.
procedure BinaryToLazarusResourceCode(BinStream, ResStream: TStream;
  const ResourceName, ResourceType: String);
function ConvertLRSExtendedToDouble(p: Pointer): Double;
function FindLFMClassName(LFMStream: TStream): AnsiString;
procedure LRSObjectBinaryToText(Input, Output: TStream); // binary to lfm
procedure LRSObjectTextToBinary(Input, Output: TStream;  // lfm to binary
                                Links: TLRPositionLinks = nil);

function CompareLRPositionLinkWithLFMPosition(Item1, Item2: Pointer): integer;
function CompareLRPositionLinkWithLRSPosition(Item1, Item2: Pointer): integer;

function ReadLRSShortInt(s: TStream): shortint;
function ReadLRSByte(s: TStream): byte;
function ReadLRSSmallInt(s: TStream): smallint;
function ReadLRSWord(s: TStream): word;
function ReadLRSInteger(s: TStream): integer;
function ReadLRSCardinal(s: TStream): cardinal;
function ReadLRSInt64(s: TStream): int64;
function ReadLRSSingle(s: TStream): Single;
function ReadLRSDouble(s: TStream): Double;
function ReadLRSExtended(s: TStream): Extended;
function ReadLRSCurrency(s: TStream): Currency;
function ReadLRSWideString(s: TStream): WideString;
function ReadLRSEndianLittleExtendedAsDouble(s: TStream): Double;
function ReadLRSValueType(s: TStream): TValueType;
function ReadLRSInt64MB(s: TStream): int64;// multibyte

procedure WriteLRSSmallInt(s: TStream; const i: smallint);
procedure WriteLRSWord(s: TStream; const w: word);
procedure WriteLRSInteger(s: TStream; const i: integer);
procedure WriteLRSCardinal(s: TStream; const c: cardinal);
procedure WriteLRSSingle(s: TStream; const si: Single);
procedure WriteLRSDouble(s: TStream; const d: Double);
procedure WriteLRSExtended(s: TStream; const e: extended);
procedure WriteLRSInt64(s: TStream; const i: int64);
procedure WriteLRSCurrency(s: TStream; const c: Currency);
procedure WriteLRSWideStringContent(s: TStream; const w: WideString);
procedure WriteLRSInt64MB(s: TStream; const Value: int64);// multibyte


implementation

var
  ByteToStr: array[char] of shortstring;
  ByteToStrValid: boolean=false;

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
  s:=Indent+'LazarusResources.Add('''+ResourceName+''','''+ResourceType+''',['+LineEnding;
  WriteString(s);
  Indent:='  '+Indent;
  WriteString(Indent);
  x:=length(Indent);
  RangeString:=false;
  CurLine:=1;
  RightMargin:=80;
  if ReadBufLen>0 then begin
    while ReadChar(c{%H-}) do begin
      NewRangeString:=(ord(c)>=32) and (ord(c)<127);
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
        WriteShortString(LineEnding);
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
  end else begin
    WriteShortString('''''');
  end;
  Indent:=copy(Indent,3,length(Indent)-2);
  s:=LineEnding+Indent+']);'+LineEnding;
  WriteString(s);
  FlushWriteBuf;
  FreeMem(ReadBufStart);
  FreeMem(WriteBufStart);
end;

function ConvertLRSExtendedToDouble(p: Pointer): Double;
type
  Ti386ExtendedReversed = packed record
    {$IFDEF FPC_BIG_ENDIAN}
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
  {$IFDEF FPC_BIG_ENDIAN}
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
  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
    {$IFDEF FPC_BIG_ENDIAN}
    // accessing Mantissa will couse trouble, copy it first
    System.Move(e.Mantissa, Mantissa, SizeOf(Mantissa));
    Mantissa := (Mantissa shl 1) shr 12;
    {$ELSE FPC_BIG_ENDIAN}
    Mantissa := (e.Mantissa shl 1) shr 12;
    {$ENDIF FPC_BIG_ENDIAN}
  {$ELSE FPC_REQUIRES_PROPER_ALIGNMENT}
  Mantissa := (e.Mantissa shl 1) shr 12;
  {$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  // put together
  QWord(Result):=Mantissa or (qword(ExponentAndSign) shl 52);
end;

function FindLFMClassName(LFMStream:TStream): AnsiString;
{ examples:
  object Form1: TForm1
  inherited AboutBox2: TAboutBox2

  -> the classname is the last word of the first line
}
var
  c: Char;
  StartPos, EndPos: Int64;
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
  if Length(Result) > 0 then
    LFMStream.Read(Result[1],length(Result));
  LFMStream.Position:=0;
  if not IsValidIdent(Result) then
    Result:='';
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
    OutStr(s + LineEnding);
  end;

  procedure OutString(const s: String);
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

  procedure OutWideString(const s: WideString);
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

  function ReadInt(ValueType: TValueType): LongInt;
  var
    w: Word;
  begin
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: begin
          w:=ReadLRSWord(Input);
          //DebugLn('ReadInt vaInt16 w=',IntToStr(w));
          Result := SmallInt(w);
        end;
      vaInt32: Result := ReadLRSInteger(Input);
      else Result := 0;
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
    if (Len > 0) then
      Input.Read(Result[1], len);
  end;

  function ReadLongString: String;
  var
    len: integer;
  begin
    len := ReadLRSInteger(Input);
    SetLength(Result, len);
    if (Len > 0) then
      Input.Read(Result[1], len);
  end;

  procedure ReadPropList(const indent: String);

    procedure ProcessValue(ValueType: TValueType; const Indent: String);

      procedure Stop(const s: String);
      begin
        RaiseGDBException('ObjectLRSToText '+s);
      end;

      function ValueTypeAsString(ValueType: TValueType): string;
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

      procedure UnknownValueType;
      var
        s: String;
        {$IFNDEF DisableChecks}
        HintStr: string;
        HintLen: Int64;
        {$ENDIF}
      begin
        s:=ValueTypeAsString(ValueType);
        if s<>'' then
          s:='Unimplemented ValueType='+s;
        {$IFNDEF DisableChecks}
        HintLen:=Output.Position;
        if HintLen>50 then HintLen:=50;
        SetLength(HintStr,HintLen);
        if HintStr<>'' then begin
          try
            Output.Position:=Output.Position-length(HintStr);
            Output.Read(HintStr[1],length(HintStr));
            //debugln('ObjectLRSToText:');
            debugln(DbgStr(HintStr));
          except
          end;
        end;
        {$ENDIF}
        s:=s+' ';
        Stop(s);
      end;

      procedure ProcessBinary;
      var
        ToDo, DoNow, StartPos, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
        p: pchar;
      const
        HexDigits: array[0..$F] of char = '0123456789ABCDEF';
      begin
        ToDo := ReadLRSCardinal(Input);
        OutLn('{');
        while ToDo > 0 do begin
          DoNow := ToDo;
          if DoNow > 32 then DoNow := 32;
          Dec(ToDo, DoNow);
          s := Indent + '  ';
          StartPos := length(s);
          Input.Read(lbuf, DoNow);
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
            OutLn(FloatToStr(ASingle) + 's');
          end;
        vaDate: begin
            ADate:=TDateTime(ReadLRSDouble(Input));
            OutLn(FloatToStr(ADate) + 'd');
          end;
        vaCurrency: begin
            ACurrency:=ReadLRSCurrency(Input);
            OutLn(FloatToStr(ACurrency * 10000) + 'c');
          end;
        vaWString,vaUString: begin
            AWideString:=ReadLRSWideString(Input);
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
            UnknownValueType;
      end;
    end;

  var
    NextByte: Byte;
  begin
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      OutStr(indent + ReadShortString + ' = ');
      NextByte:=Input.ReadByte;
      if NextByte<>0 then
        ProcessValue(TValueType(NextByte), Indent)
      else
        OutLn('');
    end;
  end;

  procedure ReadObject(const indent: String);
  var
    b: Byte;
    ObjClassName, ObjName: String;
    ChildPos: LongInt;
  begin
    ChildPos := 0;
    // Check for FilerFlags
    b := Input.ReadByte;
    if (b and $f0) = $f0 then begin
      if (b and ObjStreamMaskChildPos) <> 0 then
        ChildPos := ReadInt;
    end else begin
      b := 0;
      Input.Seek(-1, soFromCurrent);
    end;

    ObjClassName := ReadShortString;
    ObjName := ReadShortString;

    OutStr(Indent);
    if (b and ObjStreamMaskInherited) <> 0 then OutStr('inherited')
    else if (b and ObjStreamMaskInline) <> 0 then OutStr('inline')
    else OutStr('object');
    OutStr(' ');
    if ObjName <> '' then
      OutStr(ObjName + ': ');
    OutStr(ObjClassName);
    if (b and ObjStreamMaskChildPos) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
    OutLn('');

    ReadPropList(indent + '  ');

    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      ReadObject(indent + '  ');
    end;
    OutLn(indent + 'end');
  end;

var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
  Signature: TFilerSignature;
begin
  // Endian note: comparing 2 cardinals is endian independent
  Signature:='1234';
  Input.Read(Signature[1], length(Signature));
  if Signature<>FilerSignature then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
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

procedure LRSObjectTextToBinary(Input, Output: TStream; Links: TLRPositionLinks);
var
  parser: TParser;
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
  TokenStartPos: LongInt;

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

  procedure WriteWideString(const s: WideString);
  begin
    WriteLRSInteger(Output,Length(s));
    if Length(s) > 0 then
      Output.Write(s[1], Length(s)*2);
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

  function ParserNextToken: Char;
  begin
    TokenStartPos:=Parser.SourcePos;
    Result:=Parser.NextToken;
    if Links<>nil then
      Links.SetPosition(TokenStartPos,Parser.SourcePos,Output.Position,true);
  end;

  procedure ProcessProperty; forward;

  {$if not declared(toWString)}
    const toWString = char(5);
  {$endif}

  procedure ProcessValue;

    procedure RaiseValueExpected;
    begin
      parser.Error('Value expected, but '+parser.TokenString+' found');
    end;

  var
    flt: Extended;
    stream: TMemoryStream;
    BinDataSize: LongInt;
    toStringBuf: String;
  begin
    if parser.TokenSymbolIs('END') then exit;
    if parser.TokenSymbolIs('OBJECT') then
      RaiseValueExpected;
    case parser.Token of
      toInteger:
        begin
          WriteIntegerStr(parser.TokenString);
          ParserNextToken;
        end;
      toFloat:
        begin
          flt := Parser.TokenFloat;
          case parser.FloatType of
            's': begin
              Output.WriteByte(Ord(vaSingle));
              WriteLRSSingle(Output,flt);
            end;
            'd': begin
              Output.WriteByte(Ord(vaDate));
              WriteLRSDouble(Output,flt);
            end;
            'c': begin
              Output.WriteByte(Ord(vaCurrency));
              WriteLRSCurrency(Output,flt/10000);
            end;
            else
            begin
              Output.WriteByte(Ord(vaExtended));
              WriteLRSExtended(Output,flt);
            end;
          end;
          ParserNextToken;
        end;
      toString:
        begin
          toStringBuf := parser.TokenString;
          //DebugLn(['ProcessValue toStringBuf="',toStringBuf,'" ',dbgstr(toStringBuf)]);
          while ParserNextToken = '+' do
          begin
            ParserNextToken;   // Get next string fragment
            if not (parser.Token in [toString,toWString]) then
              parser.CheckToken(toString);
            toStringBuf := toStringBuf + parser.TokenString;
          end;
          if length(toStringBuf)<256 then begin
            //debugln('LRSObjectTextToBinary.ProcessValue WriteShortString');
            Output.WriteByte(Ord(vaString));
            WriteShortString(toStringBuf);
          end else begin
            //debugln('LRSObjectTextToBinary.ProcessValue WriteLongString');
            Output.WriteByte(Ord(vaLString));
            WriteLongString(toStringBuf);
          end;
        end;
      toWString:
        begin
          toStringBuf := parser.TokenString;
          //DebugLn(['ProcessValue toStringBuf="',toStringBuf,'" ',dbgstr(toStringBuf)]);
          while ParserNextToken = '+' do
          begin
            ParserNextToken;   // Get next string fragment
            if not (parser.Token in [toString,toWString]) then
              parser.CheckToken(toString);
            toStringBuf := toStringBuf + parser.TokenString;
          end;
          Output.WriteByte(Ord(vaWString));
          WriteWideString(UTF8Decode(toStringBuf));
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
          ParserNextToken;
        end;
      // Set
      '[':
        begin
          ParserNextToken;
          Output.WriteByte(Ord(vaSet));
          if parser.Token <> ']' then
            while True do
            begin
              parser.CheckToken(toSymbol);
              WriteShortString(parser.TokenString);
              ParserNextToken;
              if parser.Token = ']' then
                break;
              parser.CheckToken(',');
              ParserNextToken;
            end;
          Output.WriteByte(0);
          ParserNextToken;
        end;
      // List
      '(':
        begin
          Output.WriteByte(Ord(vaList));
          ParserNextToken;
          while parser.Token <> ')' do
            ProcessValue;
          Output.WriteByte(0);
          ParserNextToken;
        end;
      // Collection
      '<':
        begin
          ParserNextToken;
          Output.WriteByte(Ord(vaCollection));
          while parser.Token <> '>' do
          begin
            parser.CheckTokenSymbol('item');
            ParserNextToken;
            // ConvertOrder
            Output.WriteByte(Ord(vaList));
            while not parser.TokenSymbolIs('end') do
              ProcessProperty;
            ParserNextToken;   // Skip 'end'
            Output.WriteByte(0);
          end;
          Output.WriteByte(0);
          ParserNextToken;
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
            Stream.Position:=0;
            //debugln('LRSObjectTextToBinary binary data "',dbgMemStream(Stream,30),'"');
          finally
            stream.Free;
          end;
          ParserNextToken;
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
      ParserNextToken;
      if parser.Token <> '.' then break;
      ParserNextToken;
      parser.CheckToken(toSymbol);
      name := name + '.' + parser.TokenString;
    end;
    WriteShortString(name);
    parser.CheckToken('=');
    ParserNextToken;
    ProcessValue;
  end;

  procedure ProcessObject;
  var
    Flags: Byte;
    ChildPos: Integer;
    ObjectName, ObjectType: String;
  begin
    if parser.TokenSymbolIs('OBJECT') then
      Flags :=0  { IsInherited := False }
    else if parser.TokenSymbolIs('INHERITED') then
      Flags := 1 { IsInherited := True; }
    else begin
      parser.CheckTokenSymbol('INLINE');
      Flags := 4;
    end;
    ParserNextToken;
    parser.CheckToken(toSymbol);
    if parser.TokenSymbolIs('END') then begin
      // 'object end': no name, no content
      // this is normally invalid, but Delphi can create this, so ignore it
      exit;
    end;
    ObjectName := '';
    ObjectType := parser.TokenString;
    ParserNextToken;
    ChildPos := 0;
    if parser.Token = ':' then begin
      ParserNextToken;
      parser.CheckToken(toSymbol);
      ObjectName := ObjectType;
      ObjectType := parser.TokenString;
      ParserNextToken;
      if parser.Token = '[' then begin
        ParserNextToken;
        ChildPos := parser.TokenInt;
        ParserNextToken;
        parser.CheckToken(']');
        ParserNextToken;
        Flags := Flags or 2;
      end;
    end;
    if Flags <> 0 then begin
      Output.WriteByte($f0 or Flags);
      if (Flags and ObjStreamMaskChildPos) <> 0 then
        WriteInteger(ChildPos);
    end;
    WriteShortString(ObjectType);
    WriteShortString(ObjectName);

    // Convert property list
    while not (parser.TokenSymbolIs('END') or
      parser.TokenSymbolIs('OBJECT') or
      parser.TokenSymbolIs('INHERITED') or
      parser.TokenSymbolIs('INLINE'))
    do
      ProcessProperty;
    Output.WriteByte(0);        // Terminate property list

    // Convert child objects
    while not parser.TokenSymbolIs('END') do ProcessObject;
    ParserNextToken;            // Skip end token
    Output.WriteByte(0);        // Terminate property list
  end;

var
  Count: Integer;
begin
  if Links<>nil then begin
    // sort links for LFM positions
    Links.Sort(true);
  end;
  parser := TParser.Create(Input);
  OldDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  OldThousandSeparator:=DefaultFormatSettings.ThousandSeparator;
  DefaultFormatSettings.ThousandSeparator:=',';
  try
    Count:=0;
    repeat
      Output.Write(FilerSignature[1], length(FilerSignature));
      ProcessObject;
      inc(Count);
    until parser.TokenString='';
    if Count>1 then
      Output.WriteByte(0);        // Terminate object list
  finally
    parser.Free;
    DefaultFormatSettings.DecimalSeparator:=OldDecimalSeparator;
    DefaultFormatSettings.ThousandSeparator:=OldThousandSeparator;
  end;
end;

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

function ReadLRSShortInt(s: TStream): shortint;
begin
  Result:=0;
  s.Read(Result,1);
end;

function ReadLRSByte(s: TStream): byte;
begin
  Result:=0;
  s.Read(Result,1);
end;

function ReadLRSWord(s: TStream): word;
begin
  Result:=0;
  s.Read(Result,2);
  {$IFDEF FPC_BIG_ENDIAN}
  Result:=((Result and $ff) shl 8) or (Result shr 8);
  {$ENDIF}
end;

function ReadLRSSmallInt(s: TStream): smallint;
begin
  Result:=0;
  {$IFDEF FPC_BIG_ENDIAN}
  Result:=smallint(ReadLRSWord(s));
  {$ELSE}
  s.Read(Result,2);
  {$ENDIF}
end;

function ReadLRSInteger(s: TStream): integer;
begin
  Result:=0;
  s.Read(Result,4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSCardinal(s: TStream): cardinal;
begin
  Result:=0;
  s.Read(Result,4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSInt64(s: TStream): int64;
begin
  Result:=0;
  s.Read(Result,8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSSingle(s: TStream): Single;
begin
  Result:=0;
  s.Read(Result,4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSDouble(s: TStream): Double;
begin
  Result:=0;
  s.Read(Result,8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSExtended(s: TStream): Extended;
begin
  Result:=0;
  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    s.Read(Result,10);
    {$IFDEF FPC_BIG_ENDIAN}
    ReverseBytes(@Result,10);
    {$ENDIF}
  {$ELSE}
    // possible endian conversion is handled in ConvertLRSExtendedToDouble
    Result:=ReadLRSEndianLittleExtendedAsDouble(s);
  {$ENDIF}
end;

function ReadLRSCurrency(s: TStream): Currency;
begin
  Result:=0;
  s.Read(Result,8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSWideString(s: TStream): WideString;
var
  Len: LongInt;
begin
  Len:=ReadLRSInteger(s);
  SetLength(Result,Len);
  if Len>0 then begin
    s.Read(Result[1],Len*2);
    {$IFDEF FPC_BIG_ENDIAN}
    ReverseByteOrderInWords(PWord(@Result[1]),Len);
    {$ENDIF}
  end;
end;

function ReadLRSEndianLittleExtendedAsDouble(s: TStream): Double;
var
  e: array[1..10] of byte;
begin
  s.Read(e,10);
  Result:=ConvertLRSExtendedToDouble(@e);
end;

function ReadLRSValueType(s: TStream): TValueType;
var
  b: byte;
begin
  s.Read(b,1);
  Result:=TValueType(b);
end;

function ReadLRSInt64MB(s: TStream): int64;
var
  v: TValueType;
begin
  v:=ReadLRSValueType(s);
  case v of
  vaInt8: Result:=ReadLRSShortInt(s);
  vaInt16: Result:=ReadLRSSmallInt(s);
  vaInt32: Result:=ReadLRSInteger(s);
  vaInt64: Result:=ReadLRSInt64(s);
  else
    raise EInOutError.Create('ordinal valuetype missing');
  end;
end;

procedure WriteLRSSmallInt(s: TStream; const i: SmallInt);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i,2);
  {$ELSE}
  WriteLRSReversedWord(s,Word(i));
  {$ENDIF}
end;

procedure WriteLRSWord(s: TStream; const w: word);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(w,2);
  {$ELSE}
  WriteLRSReversedWord(s,w);
  {$ENDIF}
end;

procedure WriteLRSInteger(s: TStream; const i: integer);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@i);
  {$ENDIF}
end;

procedure WriteLRSCardinal(s: TStream; const c: cardinal);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(c,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@c);
  {$ENDIF}
end;

procedure WriteLRSSingle(s: TStream; const si: Single);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(si,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@si);
  {$ENDIF}
end;

procedure WriteLRSDouble(s: TStream; const d: Double);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(d,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@d);
  {$ENDIF}
end;

procedure WriteLRSExtended(s: TStream; const e: extended);
begin
  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    {$IFDEF FPC_BIG_ENDIAN}
      WriteLRS10BytesReversed(s, @e);
    {$ELSE}
      s.Write(e,10);
    {$ENDIF}
  {$ELSE}
    WriteLRSDoubleAsExtended(s,pbyte(@e))
  {$ENDIF}
end;

procedure WriteLRSInt64(s: TStream; const i: int64);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@i);
  {$ENDIF}
end;

procedure WriteLRSCurrency(s: TStream; const c: Currency);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(c,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@c);
  {$ENDIF}
end;

procedure WriteLRSWideStringContent(s: TStream; const w: WideString);
var
  Size: Integer;
begin
  Size:=length(w);
  if Size=0 then exit;
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(w[1], Size * 2);
  {$ELSE}
  WriteLRSReversedWords(s,@w[1],Size);
  {$ENDIF}
end;

procedure WriteLRSInt64MB(s: TStream; const Value: int64);
var
  w: Word;
  i: Integer;
  b: Byte;
begin
  // Use the smallest possible integer type for the given value:
  if (Value >= -128) and (Value <= 127) then
  begin
    b:=byte(vaInt8);
    s.Write(b, 1);
    b:=byte(Value);
    s.Write(b, 1);
  end else if (Value >= -32768) and (Value <= 32767) then
  begin
    b:=byte(vaInt16);
    s.Write(b, 1);
    w:=Word(Value);
    WriteLRSWord(s,w);
  end else if (Value >= -$80000000) and (Value <= $7fffffff) then
  begin
    b:=byte(vaInt32);
    s.Write(b, 1);
    i:=Integer(Value);
    WriteLRSInteger(s,i);
  end else
  begin
    b:=byte(vaInt64);
    s.Write(b, 1);
    WriteLRSInt64(s,Value);
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

function TLRPositionLinks.IndexOf(const Position: int64; LFMPositions: Boolean
  ): integer;
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
    if p>Position then begin
      // too high, search lower
      r:=m-1;
    end else if p<Position then begin
      // too low, search higher
      l:=m+1;
    end else begin
      // position found
      Result:=m;
      exit;
    end;
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
    if p>=ToPos then begin
      // too high, search lower
      r:=m-1;
    end else if p<FromPos then begin
      // too low, search higher
      l:=m+1;
    end else begin
      // position found
      Result:=m;
      exit;
    end;
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

end.

