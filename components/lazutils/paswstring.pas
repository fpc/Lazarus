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

interface

uses SysUtils, lazutf8;

procedure SetPasWidestringManager;

implementation

procedure fpc_rangeerror; [external name 'FPC_RANGEERROR'];

procedure Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
var
  widestr: widestring;
begin
  // Copy the originating string taking into account the specified length
  SetLength(widestr, len+1);
  System.Move(source^, widestr, len);
  widestr[len+1] := #0;

  // Now convert it, using UTF-8 -> UTF-16
  dest := UTF16ToUTF8(widestr);
end;

procedure Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
var
  ansistr: ansistring;
begin
  // Copy the originating string taking into account the specified length
  SetLength(ansistr, len+1);
  System.Move(source^, ansistr, len);
  ansistr[len+1] := #0;

  // Now convert it, using UTF-16 -> UTF-8
  dest := UTF8ToUTF16(ansistr);
end;

function LowerWideString(const s : WideString) : WideString;
var
  str: utf8string;
begin
  str := UTF16ToUTF8(s);
  str := UTF8LowerCase(str);
  Result := UTF8ToUTF16(str);
end;

function UpperWideString(const s : WideString) : WideString;
var
  str: utf8string;
begin
  str := UTF16ToUTF8(s);
  str := UTF8UpperCase(str);
  Result := UTF8ToUTF16(str);
end;


procedure EnsureAnsiLen(var S: AnsiString; const len: SizeInt); inline;
begin
  if (len>length(s)) then
    if (length(s) < 10*256) then
      setlength(s,length(s)+10)
    else
      setlength(s,length(s)+length(s) shr 8);
end;


procedure ConcatCharToAnsiStr(const c: char; var S: AnsiString; var index: SizeInt);
begin
  EnsureAnsiLen(s,index);
  pchar(@s[index])^:=c;
  inc(index);
end;

function LowerAnsiString(const s : AnsiString) : AnsiString;
var
  Str: utf8string;
begin
  Str := SysToUTF8(s);
  Str := UTF8LowerCase(Str);
  Result := UTF8ToSys(Str);
end;

function UpperAnsiString(const s : AnsiString) : AnsiString;
var
  Str: utf8string;
begin
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
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := System.CompareMemRange(Pointer(S1),Pointer(S2), Count*2);
  if result=0 then
    result:=Count1-Count2;
end;

function WideCompareText(const s1, s2 : WideString): PtrInt;
var
  a, b: AnsiString;
begin
  a:=LowerWidestring(s1);
  b:=LowerWidestring(s2);
  result := WideCompareStr(a,b);
end;

function CharLengthPChar(const Str: PChar): PtrInt;
begin
  Result := UTF8CharacterLength(Str);
end;

function AnsiCompareStr(const s1, s2: ansistring): PtrInt;
begin
  Result := System.CompareStr(s1, s2);
end;

// Similar to AnsiCompareStr, but with PChar
function StrCompAnsi(s1,s2 : PChar): PtrInt;
var
  ansi1, ansi2: ansistring;
begin
  ansi1 := StrPas(S1);
  ansi2 := StrPas(S2);
  Result := System.CompareStr(ansi1, ansi2);
end;


function AnsiCompareText(const S1, S2: ansistring): PtrInt;
var
  str1, str2: utf8string;
begin
  str1 := SysToUTF8(S1);
  str2 := SysToUTF8(S2);
  Result := UTF8CompareText(str1, str2);
end;


function AnsiStrIComp(S1, S2: PChar): PtrInt;
begin
  Result := AnsiCompareText(StrPas(s1),StrPas(s2));
end;


function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
  var
    a, b: pchar;
begin
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
  temp:=loweransistring(str);
  ansi2pchar(temp,str,result);
end;


function AnsiStrUpper(Str: PChar): PChar;
var
  temp: ansistring;
begin
  temp:=upperansistring(str);
  ansi2pchar(temp,str,result);
end;


procedure InitThread;
begin
end;


procedure FiniThread;
begin
end;


Procedure SetPasWideStringManager;
Var
  PasWideStringManager : TUnicodeStringManager;
begin
  PasWideStringManager:=widestringmanager;
  PasWideStringManager.Wide2AnsiMoveProc:=@Wide2AnsiMove;
  PasWideStringManager.Ansi2WideMoveProc:=@Ansi2WideMove;

  PasWideStringManager.UpperWideStringProc:=@UpperWideString;
  PasWideStringManager.LowerWideStringProc:=@LowerWideString;

  PasWideStringManager.CompareWideStringProc:=@WideCompareStr;
  PasWideStringManager.CompareTextWideStringProc:=@WideCompareText;

  PasWideStringManager.CharLengthPCharProc:=@CharLengthPChar;

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
  PasWideStringManager.Unicode2AnsiMoveProc:=@Wide2AnsiMove;
  PasWideStringManager.Ansi2UnicodeMoveProc:=@Ansi2WideMove;
  PasWideStringManager.UpperUnicodeStringProc:=@UpperWideString;
  PasWideStringManager.LowerUnicodeStringProc:=@LowerWideString;

  SetUnicodeStringManager(PasWideStringManager);
end;


initialization
  SetPasWideStringManager;
end.
