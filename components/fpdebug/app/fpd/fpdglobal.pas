{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdglobal.pas  -  FP standalone debugger - Globals
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
unit FPDGlobal;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Maps, FpDbgUtil, FpDbgClasses;

type
  TFPDImageInfo = (iiNone, iiName, iiDetail);

var
  GState: TFPDState;
  GFileName: String;
  GBreakOnLibraryLoad: Boolean = False;
  GImageInfo: TFPDImageInfo = iiNone;

  GMainProcess: TDbgProcess = nil;
  GCurrentProcess: TDbgProcess = nil;
  GCurrentThread: TDbgThread = nil;
  GProcessMap: TMap;
  

function GetProcess(const AProcessIdentifier: THandle; out AProcess: TDbgProcess): Boolean;

implementation

function GetProcess(const AProcessIdentifier: THandle; out AProcess: TDbgProcess): Boolean;
begin
  Result := GProcessMap.GetData(AProcessIdentifier, AProcess) and (AProcess <> nil);
//  if not Result
//  then Log('Unknown Process ID %u', [AID]);
end;

initialization
  GState := dsStop;
  GProcessMap := TMap.Create(itu4, SizeOf(TDbgProcess));
  
finalization
  FreeAndNil(GProcessMap)

end.
