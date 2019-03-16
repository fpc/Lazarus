unit JcfFileUtils;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfFileUtils.pas, released October 2001.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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

interface

{ this unit is a wrapper for platform-specific file fns
  IE a way to get rid of those portability warnings
  and a place to put the equivalent linux fns }

{$IFDEF FPC}
uses Dialogs;
{$ELSE}
{$IFDEF WIN32}
 uses {$PUSH} {$WARNINGS OFF} FileCtrl {$POP};
 {$ENDIF}
{$ENDIF}


function FileIsReadOnly(const ps: string): boolean;

implementation

uses SysUtils;

{$IFDEF FPC}

  // FPC version
  function FileIsReadOnly(const ps: string): boolean;
  var
    liAttr: integer;
  begin
    Assert(FileExists(ps));
  {$PUSH}
  {$WARNINGS OFF}
    liAttr := FileGetAttr(ps);
    Result := ((liAttr and faReadOnly) <> 0);
  {$POP}
  end;

{$ELSE}
  {$IFDEF WIN32}

  // delphi-windows version
  function FileIsReadOnly(const ps: string): boolean;
  var
    liAttr: integer;
  begin
    Assert(FileExists(ps));
  {$PUSH}
  {$WARNINGS OFF}
    liAttr := FileGetAttr(ps);
    Result := ((liAttr and faReadOnly) <> 0);
  {$POP}
  end;

  {$ENDIF}
  {$IFDEF LINUX}
    // delphi-linux version
    This bit will not compile under linux yet
    as the above win32 fns will not work there.
  {$ENDIF}
{$ENDIF}

end.
