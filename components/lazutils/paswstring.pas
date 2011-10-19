{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the LazUtils package                                *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit paswstring;

{$mode objfpc}
{$inline on}
//{$define PASWSTRING_VERBOSE}

interface

uses SysUtils, lazutf8, lconvencoding;

{$IFNDEF VER2_7}
procedure SetPasWidestringManager;
{$ENDIF}

implementation

{$IFNDEF VER2_7}
procedure fpc_rangeerror; [external name 'FPC_RANGEERROR'];

procedure Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
var
  widestr: widestring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Wide2AnsiMove START');{$endif}
  // Copy the originating string taking into account the specified length
  SetLength(widestr, len);
  System.Move(source^, widestr[1], len);

  // Now convert it, using UTF-16 -> UTF-8
  dest := UTF16ToUTF8(widestr);
  // And correct to the real Ansi encoding
  dest := ConvertEncoding(dest, EncodingUTF8, GetDefaultTextEncoding());
end;

procedure Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
var
  ansistr: ansistring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Ansi2WideMove START');{$endif}
  // Copy the originating string taking into account the specified length
  SetLength(ansistr, len);
  System.Move(source^, ansistr[1], len);

  // Convert to UTF-8
  ansistr := ConvertEncoding(ansistr, GetDefaultTextEncoding(), EncodingUTF8);
  // Now convert it, using UTF-8 -> UTF-16
  dest := UTF8ToUTF16(ansistr);
end;

function LowerWideString(const s : WideString) : WideString;
var
  str: utf8string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('LowerWideString START');{$endif}
  str := UTF16ToUTF8(s);
  str := UTF8LowerCase(str);
  Result := UTF8ToUTF16(str);
end;

function UpperWideString(const s : WideString) : WideString;
var
  str: utf8string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('UpperWideString START');{$endif}
  str := UTF16ToUTF8(s);
  str := UTF8UpperCase(str);
  Result := UTF8ToUTF16(str);
end;

procedure EnsureAnsiLen(var S: AnsiString; const len: SizeInt); inline;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('EnsureAnsiLen START');{$endif}
  if (len>length(s)) then
    if (length(s) < 10*256) then
      setlength(s,length(s)+10)
    else
      setlength(s,length(s)+length(s) shr 8);
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
  Str: utf8string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('LowerAnsiString START');{$endif}
  Str := SysToUTF8(s);
  Str := UTF8LowerCase(Str);
  Result := UTF8ToSys(Str);
end;

function UpperAnsiString(const s : AnsiString) : AnsiString;
var
  Str: utf8string;
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
  a, b: WideString;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('WideCompareText START');{$endif}
  a:=LowerWidestring(s1);
  b:=LowerWidestring(s2);
  result := WideCompareStr(a,b);
end;

function CharLengthPChar(const Str: PChar): PtrInt;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('CharLengthPChar START');{$endif}
  Result := UTF8CharacterLength(Str);
end;

function AnsiCompareStr(const s1, s2: ansistring): PtrInt;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('AnsiCompareStr START');{$endif}
  Result := SysUtils.CompareStr(s1, s2);
end;

// Similar to AnsiCompareStr, but with PChar
function StrCompAnsi(s1,s2 : PChar): PtrInt;
var
  ansi1, ansi2: ansistring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('StrCompAnsi START');{$endif}
  ansi1 := StrPas(S1);
  ansi2 := StrPas(S2);
  Result := SysUtils.CompareStr(ansi1, ansi2);
end;


function AnsiCompareText(const S1, S2: ansistring): PtrInt;
var
  str1, str2: utf8string;
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
var
  widestr: unicodestring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Unicode2AnsiMove START');{$endif}
  // Copy the originating string taking into account the specified length
  SetLength(widestr, len);
  System.Move(source^, widestr[1], len*2);

  // Now convert it, using UTF-16 -> UTF-8
  dest := UTF16ToUTF8(widestr);
  // And correct to the real Ansi encoding
  dest := ConvertEncoding(dest, EncodingUTF8, GetDefaultTextEncoding());
end;

procedure Ansi2UnicodeMove(source:pchar;var dest:UnicodeString;len:SizeInt);
var
  ansistr: ansistring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('Ansi2UnicodeMove START');{$endif}
  // Copy the originating string taking into account the specified length
  SetLength(ansistr, len);
  System.Move(source^, ansistr[1], len);

  // Convert to UTF-8
  ansistr := ConvertEncoding(ansistr, GetDefaultTextEncoding(), EncodingUTF8);
  // Now convert it, using UTF-8 -> UTF-16
  dest := UTF8ToUTF16(ansistr);
end;

function UpperUnicodeString(const s : UnicodeString) : UnicodeString;
var
  str: utf8string;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('UpperUnicodeString START');{$endif}
  str := UTF16ToUTF8(s);
  str := UTF8UpperCase(str);
  Result := UTF8ToUTF16(str);
end;

function LowerUnicodeString(const s : UnicodeString) : UnicodeString;
var
  str: utf8string;
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
  a, b: unicodestring;
begin
  {$ifdef PASWSTRING_VERBOSE}WriteLn('PasUnicodeCompareText START');{$endif}
  a:=LowerWidestring(s1);
  b:=LowerWidestring(s2);
  result := WideCompareStr(a,b);
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
