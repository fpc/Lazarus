{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgwinextra.pp  -  Native Freepascal debugger - Extra windows utilities
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgWinExtra;

{$mode objfpc}{$H+}

interface

{$ifdef windows}
uses
  Windows;
{$endif}

const
  FACILITY_DEBUGGER                                  = $001;
  FACILITY_RPC_RUNTIME                               = $002;
  FACILITY_RPC_STUBS                                 = $003;
  FACILITY_IO_ERROR_CODE                             = $004;
  FACILITY_TERMINAL_SERVER                           = $00A;
  FACILITY_USB_ERROR_CODE                            = $010;
  FACILITY_HID_ERROR_CODE                            = $011;
  FACILITY_FIREWIRE_ERROR_CODE                       = $012;
  FACILITY_CLUSTER_ERROR_CODE                        = $013;
  FACILITY_ACPI_ERROR_CODE                           = $014;
  FACILITY_SXS_ERROR_CODE                            = $015;

  STATUS_SEVERITY_SUCCESS                            = $0;
  STATUS_SEVERITY_INFORMATIONAL                      = $1;
  STATUS_SEVERITY_WARNING                            = $2;
  STATUS_SEVERITY_ERROR                              = $3;

function GetLastErrorText(AErrorCode: Cardinal): String; {$IFNDEF FPC} overload; {$ENDIF}
function GetLastErrorText: String; {$IFNDEF FPC} overload; {$ENDIF}

{$ifdef windows}
var
  MDebugEvent: TDebugEvent;
{$endif}

//function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall;
//function Wow64GetThreadContext(hThread: THandle; var lpContext: TContext): BOOL; stdcall;


implementation

uses
  SysUtils;

//function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall; external 'kernel32';
//function Wow64GetThreadContext(hThread: THandle; var lpContext: TContext): BOOL; stdcall; external 'kernel32';




function GetLastErrorText: String;
begin
{$ifdef windows}
  Result := GetLastErrorText(GetLastError);
{$endif}
end;

function GetLastErrorText(AErrorCode: Cardinal): String;
{$ifdef windows}
var
  R: cardinal;
  Temp: PChar;
{$endif}
begin
{$ifdef windows}
  Temp := nil;
  R := FormatMessage(
         FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
         nil,
         AErrorCode,
         LANG_NEUTRAL,
         Pointer(@Temp), // the fuction needs either a PChar or a PPChar, depending on FORMAT_MESSAGE_ALLOCATE_BUFFER
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
{$endif}
end;

end.

