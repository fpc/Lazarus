unit JcfSystemUtils;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfSystemUtils, released Jan 2009
The Initial Developer of the Original Code is Paul Ishenin
Portions created by Paul Ishenin are Copyright (C) 1999-2008 Paul Ishenin
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL")
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

{
This unit contains OS and File utility code
For use when the JCL functions are not available
}
interface

uses
  SysUtils, Classes;

function GetWindowsTempFolder: string;

function FileIsReadOnly(const ps: string): boolean;
function FileGetSize(const FileName: string): Int64;

procedure ShellExecEx(const FileName: string; const Parameters: string = '');
function IsMultiByte(const {%H-}pcChar: Char): Boolean;

function IsWinServer2008R2: Boolean;
function IsWin7: Boolean;
function IsWinServer2008: Boolean;
function IsWinVista: Boolean;
function IsWinXP: Boolean;
function IsWin2k: Boolean;
function IsWin2003: Boolean;

implementation

// We know that this unit contains platform-specific code guarded by ifdefs
{$WARN SYMBOL_PLATFORM OFF}

uses
  {$ifdef MSWINDOWS}
    Windows, ShellApi, {$PUSH} {$WARNINGS OFF} FileCtrl {$POP}
  {$endif}
  {$ifdef Unix}
    Unix
  {$endif}
  ,LCLIntf, fileutil;

function GetWindowsTempFolder: string;
begin
  Result := GetTempDir;
end;

function FileIsReadOnly(const ps: string): boolean;
var
  liAttr: integer;
begin
  Assert(FileExists(ps));
  liAttr := FileGetAttr(ps);
  Result := (liAttr and faReadOnly) <> 0;
end;

function FileGetSize(const FileName: string): Int64;
begin
  Result := FileUtil.FileSize(FileName);
end;

procedure ShellExecEx(const FileName: string; const Parameters: string = '');
begin
  {$ifdef MSWINDOWS}
    ShellApi.ShellExecute(0, 'open', PChar(FileName), PChar(Parameters), nil, SW_SHOW);
  {$endif}
  {$ifdef unix}
    fpsystem(format('%s %s',[FileName, Parameters]));
  {$endif}
end;

function IsMultiByte(const pcChar: Char): Boolean;
begin
  //Result := IsDBCSLeadByte(Byte(pcChar));
  Result := False;
end;

function IsWinServer2008R2: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (Win32MajorVersion = 6) and (Win32MinorVersion = 1);
  // Should also make sure it's a server (see JclSysInfo)
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsWin7: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (Win32MajorVersion = 6) and (Win32MinorVersion = 1);
  // Should also make sure it's a workstation (see JclSysInfo)
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsWinServer2008: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (Win32MajorVersion = 6) and (Win32MinorVersion = 0);
  // Should also make sure it's a server (see JclSysInfo)
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsWinVista: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (Win32MajorVersion = 6) and (Win32MinorVersion = 0);
  // Should also make sure it's a workstation (see JclSysInfo)
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsWinXP: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (Win32MajorVersion = 5) and (Win32MinorVersion = 1);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsWin2k: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (Win32MajorVersion = 5) and (Win32MinorVersion = 0);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IsWin2003: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (Win32MajorVersion = 5) and (Win32MinorVersion = 2);
  // can be also window xp 64 bit
{$ELSE}
  Result := False;
{$ENDIF}
end;

end.
