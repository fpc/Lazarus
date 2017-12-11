{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  A widestring manager written in Pascal
  and optimized for DefaultSystemCodePage CP_UTF8.
}
unit PasWString;

{$mode objfpc}
{$inline on}
{$i lazutils_defines.inc}

//{$define PASWSTRING_VERBOSE}
//{.$define PASWSTRING_SUPPORT_NONUTF8_ANSISTRING} disabled by default because
// non utf-8 ansistring is rare in UNIXes and lconvencoding makes the executable big

// sanity checks for defines
//{$IF FPC_FULLVERSION >= 30000}
{$IFnDEF NO_CP_RTL}
  {$IFDEF UTF8_RTL}
    {$IFDEF PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}
      {$error UTF8 or not UTF8}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE DisablePasWString}
{$ENDIF}

interface

uses
  SysUtils, LazUTF8
  {$ifdef PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}, lconvencoding{$endif}
  ;

{$IFnDEF DisablePasWString}
procedure SetPasWidestringManager;
{$ENDIF}

implementation

{$IFnDEF DisablePasWString}
procedure fpc_rangeerror; [external name 'FPC_RANGEERROR'];

function IsASCII(const s: string): boolean; inline;
var
  i: Integer;
begin
  for i:=1 to length(s) do if ord(s[i])>127 then exit(false);
  Result:=true;
end;

// len comes in widechars, not bytes
procedure Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Wide2AnsiMove START');{$endif}
  dest := UTF16ToUTF8(Source,len);
  {$ifdef PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}
  // And correct to the real Ansi encoding
  dest := ConvertEncoding(dest, EncodingUTF8, GetDefaultTextEncoding());
  {$endif}
end;

procedure Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
{$ifdef PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}
var
  ansistr: ansistring;
{$endif}
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Ansi2WideMove START');{$endif}

  {$ifdef PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}
  // Copy the originating string taking into account the specified length
  SetLength(ansistr, len);
  System.Move(source^, ansistr[1], len);
  // Convert to UTF-8
  ansistr := ConvertEncoding(ansistr, GetDefaultTextEncoding(), EncodingUTF8);
  // Now convert it, using UTF-8 -> UTF-16
  dest := UTF8ToUTF16(ansistr);
  {$else}
  dest := UTF8ToUTF16(source,len);
  {$endif}
end;

function LowerWideString(const s : WideString) : WideString;
var
  str: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('LowerWideString START');{$endif}
  str := UTF16ToUTF8(PWideChar(s),length(s));
  str := UTF8LowerCase(str);
  Result := UTF8ToUTF16(str);
end;

function UpperWideString(const s : WideString) : WideString;
var
  str: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('UpperWideString START');{$endif}
  str := UTF16ToUTF8(PWideChar(s),length(s));
  str := UTF8UpperCase(str);
  Result := UTF8ToUTF16(str);
end;

procedure EnsureAnsiLen(var S: AnsiString; const len: SizeInt); inline;
var
  l: SizeUInt;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('EnsureAnsiLen START');{$endif}
  l:=length(s);
  if (len>l) then
    if (l < 128) then
      setlength(s,l+8)
    else
      setlength(s,l+l shr 8);
end;


procedure ConcatCharToAnsiStr(const c: char; var S: AnsiString; var index: SizeInt);
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('ConcatCharToAnsiStr START');{$endif}
  EnsureAnsiLen(s,index);
  pchar(@s[index])^:=c;
  inc(index);
end;

function LowerAnsiString(const s : AnsiString) : AnsiString;
var
  Str: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('LowerAnsiString START');{$endif}
  Str := SysToUTF8(s);
  Str := UTF8LowerCase(Str);
  Result := UTF8ToSys(Str);
end;

function UpperAnsiString(const s : AnsiString) : AnsiString;
var
  Str: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('UpperAnsiString START');{$endif}
  Str := SysToUTF8(s);
  Str := UTF8UpperCase(Str);
  Result := UTF8ToSys(Str);
end;

// Just do a simple byte comparison
// A more complex analysis would require normalization
function WideCompareStr(const s1, s2 : WideString) : PtrInt;
var
  count, count1, count2: integer;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('WideCompareStr START');{$endif}
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := SysUtils.CompareMemRange(Pointer(S1),Pointer(S2), Count*2);
  if result=0 then
    result:=Count1-Count2;
end;

function WideCompareText(const s1, s2 : WideString): PtrInt;
var
  a, b: String;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('WideCompareText START');{$endif}
  a := UTF16ToUTF8(PWideChar(s1),length(s1));
  a := UTF8LowerCase(a);
  b := UTF16ToUTF8(PWideChar(s2),length(s2));
  b := UTF8LowerCase(b);
  result := UTF8CompareStr(a,b);
end;

function CharLengthPChar(const Str: PChar): PtrInt;
// return the number of codepoints (including invalid codepoints)
var
  p: PChar;
  l: Integer;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('CharLengthPChar START');{$endif}
  p:=Str;
  if p=nil then exit(0);
  while p^<>#0 do begin
    l:=UTF8CodepointSize(p);
    inc(Result);
    inc(p,l);
  end;
end;

function CodePointLengthPChar(const p: PChar; MaxLookAhead: PtrInt): Ptrint;
{ return value:
  -1 if incomplete or invalid code point
  0 if NULL character,
  > 0 if that's the length in bytes of the code point }
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('CodePointLengthPChar START');{$endif}
  if (p=nil) then exit(0);
  if (MaxLookAhead<0) then exit(-1);
  if ord(p^)<%10000000 then begin
    // regular single byte character
    if p^=#0 then
      exit(0)
    else
      exit(1);
  end;
  if ord(p^)<%11000000 then begin
    // invalid single byte character
    exit(-1);
  end;
  if (MaxLookAhead=0) then exit(-1);
  if ((ord(p^) and %11100000) = %11000000) then begin
    // should be 2 byte character
    if (ord(p[1]) and %11000000) = %10000000 then
      exit(2)
    else
      exit(-1);
  end;
  if (MaxLookAhead=1) then exit(-1);
  if ((ord(p^) and %11110000) = %11100000) then begin
    // should be 3 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then
      exit(3)
    else
      exit(-1);
  end;
  if (MaxLookAhead=2) then exit(-1);
  if ((ord(p^) and %11111000) = %11110000) then begin
    // should be 4 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then
      exit(4)
    else
      exit(-1);
  end;
  exit(-1);
end;

function AnsiCompareStr(const s1, s2: ansistring): PtrInt;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiCompareStr START');{$endif}
  Result := SysUtils.CompareStr(s1, s2);
end;

// Similar to AnsiCompareStr, but with PChar
function StrCompAnsi(s1,s2 : PChar): PtrInt;
var
  Count1: SizeInt;
  Count2: SizeInt;
  Count: SizeInt;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('StrCompAnsi START');{$endif}
  result := 0;
  Count1:=StrLen(s1);
  Count2:=StrLen(s2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := CompareMemRange(s1, s2, Count);
  if result=0 then
    result:=Count1-Count2;
end;


function AnsiCompareText(const S1, S2: ansistring): PtrInt;
var
  str1, str2: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiCompareText START');{$endif}
  str1 := SysToUTF8(S1);
  str2 := SysToUTF8(S2);
  Result := UTF8CompareText(str1, str2);
end;


function AnsiStrIComp(S1, S2: PChar): PtrInt;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiStrIComp START');{$endif}
  Result := AnsiCompareText(StrPas(s1),StrPas(s2));
end;


function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    a, b: pchar;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiStrLComp START');{$endif}
  Result := 0;
  if (maxlen=0) then
    exit(0);
  if (s1[maxlen]<>#0) then
    begin
      getmem(a,maxlen+1);
      move(s1^,a^,maxlen);
      a[maxlen]:=#0;
    end
  else
    a:=s1;
  if (s2[maxlen]<>#0) then
    begin
      getmem(b,maxlen+1);
      move(s2^,b^,maxlen);
      b[maxlen]:=#0;
    end
  else
    b:=s2;
  result:=StrCompAnsi(a,b);
  if (a<>s1) then
    freemem(a);
  if (b<>s2) then
    freemem(b);
end;


function AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    a, b: ansistring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiStrLIComp START');{$endif}
  if (maxlen=0) then
    exit(0);
  setlength(a,maxlen);
  move(s1^,a[1],maxlen);
  setlength(b,maxlen);
  move(s2^,b[1],maxlen);
  result:=AnsiCompareText(a,b);
end;


procedure ansi2pchar(const s: ansistring; const orgp: pchar; out p: pchar);
var
  newlen: sizeint;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('ansi2pchar START');{$endif}
  newlen:=length(s);
  if newlen>strlen(orgp) then
    fpc_rangeerror;
  p:=orgp;
  if (newlen>0) then
    move(s[1],p[0],newlen);
  p[newlen]:=#0;
end;


function AnsiStrLower(Str: PChar): PChar;
var
  temp: ansistring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiStrLower START');{$endif}
  temp:=loweransistring(str);
  ansi2pchar(temp,str,result);
end;


function AnsiStrUpper(Str: PChar): PChar;
var
  temp: ansistring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiStrUpper START');{$endif}
  temp:=upperansistring(str);
  ansi2pchar(temp,str,result);
end;


procedure InitThread;
begin
end;


procedure FiniThread;
begin
end;

{ Unicode }

procedure Unicode2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Unicode2AnsiMove START');{$endif}
  dest := UTF16ToUTF8(source,len);
  {$ifdef PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}
  // And correct to the real Ansi encoding
  dest := ConvertEncoding(dest, EncodingUTF8, GetDefaultTextEncoding());
  {$endif}
end;

procedure Ansi2UnicodeMove(source:pchar;var dest:UnicodeString;len:SizeInt);
{$IFDEF PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}
var
  ansistr: ansistring;
{$ENDIF}
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Ansi2UnicodeMove START');{$endif}
  {$IFDEF PASWSTRING_SUPPORT_NONUTF8_ANSISTRING}
  if NeedRTLAnsi then begin
    // Copy the originating string taking into account the specified length
    SetLength(ansistr, len);
    System.Move(source^, ansistr[1], len);
    // Convert to UTF-8
    ansistr := ConvertEncoding(ansistr, GetDefaultTextEncoding(), EncodingUTF8);
    // Now convert it, using UTF-8 -> UTF-16
    dest := UTF8ToUTF16(ansistr);
  end else begin
    dest := UTF8ToUTF16(source,len);
  end;
  {$ELSE}
  dest := UTF8ToUTF16(source,len);
  {$ENDIF}
end;

function UpperUnicodeString(const s : UnicodeString) : UnicodeString;
var
  str: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('UpperUnicodeString START');{$endif}
  str := UTF16ToUTF8(s);
  str := UTF8UpperCase(str);
  Result := UTF8ToUTF16(str);
end;

function LowerUnicodeString(const s : UnicodeString) : UnicodeString;
var
  str: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('LowerUnicodeString START');{$endif}
  str := UTF16ToUTF8(s);
  str := UTF8LowerCase(str);
  Result := UTF8ToUTF16(str);
end;

// Just do a simple byte comparison
// A more complex analysis would require normalization
function PasUnicodeCompareStr(const s1, s2 : unicodestring) : PtrInt;
var
  count, count1, count2: integer;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('PasUnicodeCompareStr START');{$endif}
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := SysUtils.CompareMemRange(Pointer(S1),Pointer(S2), Count*2);
  if result=0 then
    result:=Count1-Count2;
end;

function PasUnicodeCompareText(const s1, s2 : unicodestring): PtrInt;
var
  a, b: string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('PasUnicodeCompareText START');{$endif}
  a := UTF16ToUTF8(s1);
  a := UTF8LowerCase(a);
  b := UTF16ToUTF8(s2);
  b := UTF8LowerCase(b);
  result := UTF8CompareText(a,b);
end;

Procedure SetPasWideStringManager;
Var
  PasWideStringManager : TUnicodeStringManager;
begin
  PasWideStringManager:=widestringmanager;
  PasWideStringManager.Wide2AnsiMoveProc:=@Wide2AnsiMove;
  PasWideStringManager.Ansi2WideMoveProc:=@Ansi2WideMove;

  //    UpperUTF8 : procedure(p:PUTF8String);
  PasWideStringManager.UpperWideStringProc:=@UpperWideString;
  //    UpperUCS4 : procedure(p:PUCS4Char);
  //    LowerUTF8 : procedure(p:PUTF8String);
  PasWideStringManager.LowerWideStringProc:=@LowerWideString;
  //    LowerUCS4 : procedure(p:PUCS4Char);

  {
    CompUTF8 : function(p1,p2:PUTF8String) : shortint;
    CompUCS2 : function(p1,p2:PUCS2Char) : shortint;
    CompUCS4 : function(p1,p2:PUC42Char) : shortint;
  }
  PasWideStringManager.CompareWideStringProc:=@WideCompareStr;
  PasWideStringManager.CompareTextWideStringProc:=@WideCompareText;

  { return value: number of code points in the string. Whenever an invalid
    code point is encountered, all characters part of this invalid code point
    are considered to form one "character" and the next character is
    considered to be the start of a new (possibly also invalid) code point }
  PasWideStringManager.CharLengthPCharProc:=@CharLengthPChar;
  PasWideStringManager.CodePointLengthProc:=@CodePointLengthPChar;

  { Ansi }
  PasWideStringManager.UpperAnsiStringProc:=@UpperAnsiString;
  PasWideStringManager.LowerAnsiStringProc:=@LowerAnsiString;
  PasWideStringManager.CompareStrAnsiStringProc:=@AnsiCompareStr;
  PasWideStringManager.CompareTextAnsiStringProc:=@AnsiCompareText;
  PasWideStringManager.StrCompAnsiStringProc:=@StrCompAnsi;
  PasWideStringManager.StrICompAnsiStringProc:=@AnsiStrIComp;
  PasWideStringManager.StrLCompAnsiStringProc:=@AnsiStrLComp;
  PasWideStringManager.StrLICompAnsiStringProc:=@AnsiStrLIComp;
  PasWideStringManager.StrLowerAnsiStringProc:=@AnsiStrLower;
  PasWideStringManager.StrUpperAnsiStringProc:=@AnsiStrUpper;
  PasWideStringManager.ThreadInitProc:=@InitThread;
  PasWideStringManager.ThreadFiniProc:=@FiniThread;

  { Unicode }
  PasWideStringManager.Unicode2AnsiMoveProc:=@Unicode2AnsiMove;
  PasWideStringManager.Ansi2UnicodeMoveProc:=@Ansi2UnicodeMove;
  PasWideStringManager.UpperUnicodeStringProc:=@UpperUnicodeString;
  PasWideStringManager.LowerUnicodeStringProc:=@LowerUnicodeString;
  PasWideStringManager.CompareUnicodeStringProc:=@PasUnicodeCompareStr;
  PasWideStringManager.CompareTextUnicodeStringProc:=@PasUnicodeCompareText;

  SetUnicodeStringManager(PasWideStringManager);
end;


initialization
  SetPasWideStringManager;
{$ENDIF}
end.
