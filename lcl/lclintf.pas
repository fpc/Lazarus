
{  $Id$  }
{
 /***************************************************************************
                                LCLIntf.pas
                                -----------
                             Component Library Windows Controls
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{
@author(Curtis White <cwhite@aracnet.com>)
@created(17-Oct-1999)
@lastmod(17-Oct-1999)

This unit is being created specifically for compatibility with Delphi. It
should only be used for constants and type definitions that are included in
the Delphi Windows unit. This is only done for compatibility.
}

unit LCLIntf;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Types, Math, Classes, SysUtils, LCLType, LCLProc, GraphType, InterfaceBase,
  LResources;

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}
{$DEFINE ClientRectBugFix}

// All winapi related stuff (Delphi compatible)
{$I winapih.inc}
// All interface communication (Our additions)
{$I lclintfh.inc}


function MakeLong(A,B : Word) : LongInt; inline;
function MakeWord(A,B : Byte) : Word; inline;

function PredefinedClipboardFormat(
  AFormat: TPredefinedClipboardFormat): TClipboardFormat;

function CharLower(c: char): char; inline;
function CharUpper(c: char): char; inline;

function MsgKeyDataToShiftState(KeyData: Longint): TShiftState;


{$IFDEF WINDOWS}

{$IFDEF MSWindows}
function GetTickCount:DWORD; stdcall; external 'kernel32.dll' name 'GetTickCount';
{$ELSE}
function GetTickCount:DWORD; stdcall; external KernelDLL name 'GetTickCount';
{$ENDIF}

{$ELSE}
function GetTickCount: DWord;
{$ENDIF}

{$IFDEF DebugLCL}
function GetTickStep: DWord;
{$ENDIF}


implementation

var
  FPredefinedClipboardFormats:
    array[TPredefinedClipboardFormat] of TClipboardFormat;
  LowerCaseChars: array[char] of char;
  UpperCaseChars: array[char] of char;

{$IFNDEF WINDOWS}
function GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;
{$ENDIF}

{$IFDEF DebugLCL}
var
  LastTickValid: boolean;
  LastTick: DWord;

function GetTickStep: DWord;
var
  CurTick: DWord;
begin
  CurTick:=GetTickCount;
  if LastTickValid then begin
    if LastTick<=CurTick then
      Result:=CurTick-LastTick
    else begin
      // tick counter has restarted
      Result:=CurTick+(DWord($FFFFFFFF)-LastTick+1);
    end;
  end else begin
    Result:=0;
  end;
  LastTickValid:=true;
  LastTick:=CurTick;
end;
{$ENDIF}

function MakeLong(A,B : Word) : LongInt; inline;
begin
  Result := A or B shl 16;
end;

function MakeWord(A,B : Byte) : Word; inline;
Begin
  Result := A or B shl 8;
end;

function PredefinedClipboardFormat(AFormat: TPredefinedClipboardFormat
  ): TClipboardFormat;
begin
  if FPredefinedClipboardFormats[AFormat]=0 then
    FPredefinedClipboardFormats[AFormat]:=
      ClipboardRegisterFormat(PredefinedClipboardMimeTypes[AFormat]);
  Result:=FPredefinedClipboardFormats[AFormat];
end;

function CharLower(c: char): char; inline;
begin
  Result:=LowerCaseChars[c];
end;

function CharUpper(c: char): char; inline;
begin
  Result:=UpperCaseChars[c];
end;

function MsgKeyDataToShiftState(KeyData: Longint): TShiftState;
begin
  Result := [];

  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyData and $20000000 <> 0 then Include(Result, ssAlt);
end;

{$I winapi.inc}
{$I lclintf.inc}

procedure InternalInit;
var
  AClipboardFormat: TPredefinedClipboardFormat;
  c: char;
  s: string;
begin
  for AClipboardFormat:=Low(TPredefinedClipboardFormat) to
    High(TPredefinedClipboardFormat) do
      FPredefinedClipboardFormats[AClipboardFormat]:=0;
  for c:=Low(char) to High(char) do begin
    s:=lowercase(c);
    LowerCaseChars[c]:=s[1];
    UpperCaseChars[c]:=upcase(c);
  end;
  {$IFDEF DebugLCL}
  LastTickValid:=false;
  {$ENDIF}
end;

initialization
  InternalInit;

end.
