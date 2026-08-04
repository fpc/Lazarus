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
  Classes, SysUtils;

type
  TFilerSignature = array[1..4] of Char;

const
  ObjStreamMaskInherited = 1;
  ObjStreamMaskChildPos  = 2;
  ObjStreamMaskInline    = 4;

procedure BinaryToLazarusResourceCode(BinStream, ResStream: TStream;
  const ResourceName, ResourceType: String);
function ConvertLRSExtendedToDouble(p: Pointer): Double;
function FindLFMClassName(LFMStream: TStream): AnsiString;

procedure ReverseBytes(p: Pointer; Count: integer);
procedure ReverseByteOrderInWords(p: PWord; Count: integer);
procedure ConvertEndianBigDoubleToLRSExtended(BigEndianDouble,LRSExtended: Pointer);
procedure ConvertLEDoubleToLRSExtended(LEDouble, LRSExtended: Pointer);

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
procedure WriteLRSDoubleAsExtended(s: TStream; ADouble: PByte);

procedure WriteLRSReversedWord(s: TStream; w: word);
procedure WriteLRS4BytesReversed(s: TStream; p: Pointer);
procedure WriteLRS8BytesReversed(s: TStream; p: Pointer);
procedure WriteLRS10BytesReversed(s: TStream; p: Pointer);
procedure WriteLRSNull(s: TStream; Count: integer);
procedure WriteLRSEndianBigDoubleAsEndianLittleExtended(s: TStream; EndBigDouble: PByte);
procedure WriteLRSReversedWords(s: TStream; p: Pointer; Count: integer);


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

procedure ConvertEndianBigDoubleToLRSExtended(BigEndianDouble, LRSExtended: Pointer);
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

procedure ConvertLEDoubleToLRSExtended(LEDouble, LRSExtended: Pointer);
type
  TMantissaWrap = record
    case boolean of
      True: (Q: QWord);
      False: (B: array[0..7] of Byte);
  end;

  TExpWrap = packed record
    Mantissa: TMantissaWrap;
    Exp: Word;
  end;

var
  Q: PQWord absolute LEDouble;
  W: PWord absolute LEDouble;
  E: ^TExpWrap absolute LRSExtended;
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Mantissa: TMantissaWrap;
  {$endif}
begin
  if W[3] and $7FF0 = $7FF0 // infinite or NaN
  then E^.Exp := $7FFF
  else E^.Exp := (W[3] and $7FFF) shr 4 - $3FF + $3FFF;
  E^.Exp := E^.Exp or (W[3] and $8000); // sign
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Mantissa.Q := (Q^ shl 11);
  Mantissa.B[7] := Mantissa.B[7] or $80; // add ignored 1
  System.Move(Mantissa, E^.Mantissa, 8);
  {$else}
  E^.Mantissa.Q := (Q^ shl 11);
  E^.Mantissa.B[7] := E^.Mantissa.B[7] or $80; // add ignored 1
  {$endif}
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
  Result:='';
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

procedure WriteLRSDoubleAsExtended(s: TStream; ADouble: PByte);
var
  e: array[0..9] of byte;
begin
  {$ifdef FPC_LITTLE_ENDIAN}
  ConvertLEDoubleToLRSExtended(ADouble,@e);
  {$else}
  ConvertEndianBigDoubleToLRSExtended(ADouble,@e);
  {$endif}
  s.Write(e[0],10);
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

procedure WriteLRSNull(s: TStream; Count: integer);
var
  c: char;
  i: Integer;
begin
  c:=#0;
  for i:=0 to Count-1 do
    s.Write(c,1);
end;

procedure WriteLRSEndianBigDoubleAsEndianLittleExtended(s: TStream; EndBigDouble: PByte);
var
  e: array[0..9] of byte;
begin
  ProjResProc.ConvertEndianBigDoubleToLRSExtended(EndBigDouble,@e);
  s.Write(e[0],10);
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

end.

