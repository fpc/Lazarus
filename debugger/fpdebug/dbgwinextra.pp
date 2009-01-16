{ $Id$ }
{
 ---------------------------------------------------------------------------
 dbgwinextra.pp  -  Native Freepascal debugger - Extra windows utilities
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
unit DbgWinExtra;

{$mode objfpc}{$H+}

interface

uses
  Windows;
  


function GetLastErrorText(AErrorCode: Cardinal): String; {$IFNDEF FPC} overload; {$ENDIF}
function GetLastErrorText: String; {$IFNDEF FPC} overload; {$ENDIF}


//function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall;
//function Wow64GetThreadContext(hThread: THandle; var lpContext: TContext): BOOL; stdcall;


implementation

uses
  SysUtils;

//function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall; external 'kernel32';
//function Wow64GetThreadContext(hThread: THandle; var lpContext: TContext): BOOL; stdcall; external 'kernel32';




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
end;



end.

