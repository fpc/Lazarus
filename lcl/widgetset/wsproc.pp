{
 /***************************************************************************
                                   wsproc.pp
                                   ---------
                             Widgetset Utility Code


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Useful lower level helper functions and classes for implementing widgetsets.
}
unit WSProc;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Controls;


function WSCheckHandleAllocated(const AWincontrol: TWinControl;
                                const AProcName: String): Boolean;
  
implementation                                                  
                                                              
function WSCheckHandleAllocated(const AWincontrol: TWinControl;
  const AProcName: String): Boolean;
begin
  Result := AWinControl.HandleAllocated; 
  if not Result
  then Assert(False, Format('trace: [WARNING] %s called without handle for %s(%s)', [AProcName, AWinControl.Name, AWinControl.ClassName]));
end;

end.