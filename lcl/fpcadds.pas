{  $Id$  }
{
 /***************************************************************************
                                FPCAdds.pas
                                -----------

 ***************************************************************************/

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
}
unit FPCAdds;

{$mode objfpc}{$H+}
{$if defined(VER2_0_0) or defined(VER_2_0_1) or defined(VER_2_0_2)}
{$DEFINE FPC_HAS_NO_STRTOQWORD}
{$ENDIF}

interface

uses
  Classes, SysUtils
{$IFDEF FPC_HAS_NO_STRTOQWORD}
  ,sysconst
{$ENDIF}
  ;

// current TStream calculates in int64, old in longint
type
  TStreamSeekType = int64;
  TMemStreamSeekType = integer;
  TCompareMemSize = integer;
  PHandle = ^THandle;

function StrToWord(const s: string): word;

{$IFDEF VER2_0_0}
// These functions were introduced after fpc 2.0.0
function ExceptFrameCount: Longint;
function ExceptFrames: PPointer;
{$ENDIF}

{$IFDEF FPC_HAS_NO_STRTOQWORD}
function StrToQWord(const s: string): QWord;
{$ENDIF}

implementation

function StrToWord(const s: string): word;
var
  p: Integer;
begin
  Result:=0;
  p:=1;
  while (p<=length(s)) do begin
    Result:=Result*10+ord(s[p])-ord('0');
    inc(p);
  end;
end;

{$IFDEF VER2_0_0}
function ExceptFrameCount: Longint;
begin
  If RaiseList=Nil then
    Result:=0
  else
    Result:=RaiseList^.Framecount;
end;

function ExceptFrames: PPointer;
begin
  If RaiseList=Nil then
    Result:=Nil
  else
    Result:=RaiseList^.Frames;
end;
{$ENDIF}

// fpc 2.0.0 widestringmanager is incomplete for win32
{$IFDEF VER2_0_0}{$IFDEF win32}
//copied from rtl/win32/system.pp
type
  UINT=cardinal;
const
  { MultiByteToWideChar  }
     MB_PRECOMPOSED = 1;
     CP_ACP = 0;

function MultiByteToWideChar(CodePage:UINT; dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;
    stdcall; external 'kernel32' name 'MultiByteToWideChar';
function WideCharToMultiByte(CodePage:UINT; dwFlags:DWORD; lpWideCharStr:PWideChar; cchWideChar:longint; lpMultiByteStr:PChar;cchMultiByte:longint; lpDefaultChar:PChar; lpUsedDefaultChar:pointer):longint;
    stdcall; external 'kernel32' name 'WideCharToMultiByte';

procedure Win32Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
var
  destlen: SizeInt;
begin
  // retrieve length including trailing #0
  destlen:=WideCharToMultiByte(CP_ACP, 0, source, len+1, nil, 0, nil, nil);
  setlength(dest, destlen-1);
  WideCharToMultiByte(CP_ACP, 0, source, len+1, @dest[1], destlen, nil, nil);
end;
   
procedure Win32Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
var
  destlen: SizeInt;
begin
  // retrieve length including trailing #0
  destlen:=MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, source, len+1, nil, 0);
  setlength(dest, destlen-1);
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, source, len+1, @dest[1], destlen);
end;

procedure InitWin32Widestrings;
begin
  widestringmanager.Wide2AnsiMoveProc:=@Win32Wide2AnsiMove;
  widestringmanager.Ansi2WideMoveProc:=@Win32Ansi2WideMove;
end;
{$ENDIF}{$ENDIF}

{$IFDEF FPC_HAS_NO_STRTOQWORD}
function StrToQWord(const s: string): QWord;
var Error: word;
begin
  Val(S, result, Error);
  if Error <> 0 then raise EConvertError.createfmt(SInvalidInteger,[S]);
end;
{$ENDIF}

{$IFDEF VER2_0_0}{$IFDEF win32}
initialization
  InitWin32Widestrings;
{$ENDIF}{$ENDIF}

end.

