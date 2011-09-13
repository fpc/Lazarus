{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Felipe Monteiro de Carvalho

    Pascal wide string support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{$mode objfpc}
{$inline on}

unit paswstring;

interface

uses lconvencoding, lclproc;

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

// Not implemented yet
function LowerWideString(const s : WideString) : WideString;
begin
  Result := s;
end;

// Not implemented yet
function UpperWideString(const s : WideString) : WideString;
begin
  Result := s;
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

// Not implemented yet
function LowerAnsiString(const s : AnsiString) : AnsiString;
begin
end;

// Not implemented yet
function UpperAnsiString(const s : AnsiString) : AnsiString;
begin
end;

// Not implemented yet
function CompareWideString(const s1, s2 : WideString) : PtrInt;
begin
  Result := PtrInt(Boolean(s1 = s2));
end;

// Not implemented yet
function CompareTextWideString(const s1, s2 : WideString): PtrInt;
var
  a, b: AnsiString;
begin
  a:=UpperWidestring(s1);
  b:=UpperWidestring(s2);
  result:=CompareWideString(a,b);
end;

function CharLengthPChar(const Str: PChar): PtrInt;
begin
  Result := UTF8CharacterLength(Str);
end;

function CompareStrAnsiString(const s1, s2: ansistring): PtrInt;
begin
  Result := PtrInt(Boolean(s1 = s2));
end;


function StrCompAnsi(s1,s2 : PChar): PtrInt;
begin
  Result := PtrInt(Boolean(s1 = s2));
end;


function AnsiCompareText(const S1, S2: ansistring): PtrInt;
var
  a, b: AnsiString;
begin
  a:=UpperAnsistring(s1);
  b:=UpperAnsistring(s2);
  result:=StrCompAnsi(pchar(a),pchar(b));
end;


function AnsiStrIComp(S1, S2: PChar): PtrInt;
  begin
    result:=AnsiCompareText(ansistring(s1),ansistring(s2));
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

  PasWideStringManager.CompareWideStringProc:=@CompareWideString;
  PasWideStringManager.CompareTextWideStringProc:=@CompareTextWideString;

  PasWideStringManager.CharLengthPCharProc:=@CharLengthPChar;

  PasWideStringManager.UpperAnsiStringProc:=@UpperAnsiString;
  PasWideStringManager.LowerAnsiStringProc:=@LowerAnsiString;
  PasWideStringManager.CompareStrAnsiStringProc:=@CompareStrAnsiString;
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
