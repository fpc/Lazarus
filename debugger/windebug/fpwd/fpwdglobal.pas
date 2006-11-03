{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpwdglobal.pas  -  FP standalone windows debugger - Globals
 ---------------------------------------------------------------------------

 This unit contains global types / vars

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
unit FPWDGlobal;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Windows, FPWDType, Maps, WinDebugger, WinDExtra;

type
  TMWDState = (dsStop, dsRun, dsPause, dsQuit, dsEvent);
  TMWDMode = (dm32, dm64);
  TMWDImageInfo = (iiNone, iiName, iiDetail);
  
const
  DBGPTRSIZE: array[TMWDMode] of Integer = (4, 8);

var
  GState: TMWDState;
  GFileName: String;
  {$ifdef cpui386}
  GMode: TMWDMode = dm32;
  {$else}
  GMode: TMWDMode = dm64;
  {$endif}
  GBreakOnLibraryLoad: Boolean = False;
  GImageInfo: TMWDImageInfo = iiNone;
  
  GCurrentContext: PContext;

  GMainProcess: TDbgProcess = nil;
  GCurrentProcess: TDbgProcess = nil;
  GCurrentThread: TDbgThread = nil;
  GProcessMap: TMap;
  

function GetProcess(const AID: Integer; out AProcess: TDbgProcess): Boolean;
function FormatAddress(const AAddress): String;

implementation

function GetProcess(const AID: Integer; out AProcess: TDbgProcess): Boolean;
begin
  Result := GProcessMap.GetData(AID, AProcess) and (AProcess <> nil);
//  if not Result
//  then Log('Unknown Process ID %u', [AID]);
end;

function FormatAddress(const AAddress): String;
begin
  Result := HexValue(AAddress, DBGPTRSIZE[GMode], [hvfIncludeHexchar]);
end;


var
  _UnAligendContext: record
    C: TContext;
    dummy: array[1..16] of byte;
  end;


initialization
  GState := dsStop;
  GProcessMap := TMap.Create(itu4, SizeOf(TDbgProcess));
  
  GCurrentContext := Pointer((PtrUInt(@_UnAligendContext) + 15) and not PtrUInt($F));
  
finalization
  FreeAndNil(GProcessMap)

end.
