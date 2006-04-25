{ $Id$ }
{
 ---------------------------------------------------------------------------
 windextra.pp  -  Native windows debugger - Extra utilities
 ---------------------------------------------------------------------------

 This unit contains utility functions and missing win32/64 API

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit WindExtra;

{$mode objfpc}{$H+}

interface

uses
  Windows;
  
type
  TDbgPtr = PtrUInt;

function GetLastErrorText(AErrorCode: Cardinal): String; {$IFNDEF FPC} overload; {$ENDIF}
function GetLastErrorText: String; {$IFNDEF FPC} overload; {$ENDIF}

function FormatAddress(const P): String;
function AlignPtr(Src: Pointer; Alignment: Byte): Pointer;
procedure Log(const AText: String; const AParams: array of const); overload;
procedure Log(const AText: String); overload;


//function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall;
//function Wow64GetThreadContext(hThread: THandle; var lpContext: TContext): BOOL; stdcall;


implementation

uses
  SysUtils, FPWDGLobal;

//function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall; external 'kernel32';
//function Wow64GetThreadContext(hThread: THandle; var lpContext: TContext): BOOL; stdcall; external 'kernel32';


function FormatAddress(const P): String;
begin
  case GMode of
    dm32: Result := '$' + IntToHex(DWord(p), 8);
    dm64: Result := '$' + IntToHex(int64(p), 16);
  else
    Result := 'Unknown mode';
  end;

end;

function GetLastErrorText: String;
begin
  Result := GetLastErrorText(GetLastError);
end;

function GetLastErrorText(AErrorCode: Cardinal): String;
var
  R: cardinal;
  Temp: PChar;
begin
  Temp := nil;
  R := FormatMessage(
         FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
         nil,
         AErrorCode,
         LANG_NEUTRAL,
         @Temp,
         0,
         nil);
  if R = 0
  then begin
    Result := '';
  end
  else begin
    Result := Temp;
    SetLength(Result, Length(Result)-2);
  end;
  if Temp <> nil
  then LocalFree(HLOCAL(Temp));
end;

function AlignPtr(Src: Pointer; Alignment: Byte): Pointer;
begin
  Result := Pointer(((PtrUInt(Src) + Alignment - 1) and not PtrUInt(Alignment - 1)));
end;

procedure Log(const AText: String; const AParams: array of const); overload;
begin
  WriteLN(Format(AText, AParams));
end;

procedure Log(const AText: String); overload;
begin
  WriteLN(AText);
end;


end.

