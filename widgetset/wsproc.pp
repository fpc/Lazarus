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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  LCLClasses, LCLProc, Controls, Menus;


function WSCheckReferenceAllocated(const AComponent: TLCLReferenceComponent;
                                   const AProcName: String): Boolean;

function WSCheckHandleAllocated(const AWincontrol: TWinControl;
                                const AProcName: String): Boolean;

function WSCheckHandleAllocated(const AMenu: TMenu;
                                const AProcName: String): Boolean;

implementation

function WSCheckReferenceAllocated(const AComponent: TLCLReferenceComponent;
  const AProcName: String): Boolean;

  procedure Warn;
  begin
    DebugLn('[WARNING] %s called without reference for %s(%s)', [AProcName, AComponent.Name, AComponent.ClassName]);
  end;
begin
  Result := AComponent.ReferenceAllocated;
  if Result then Exit;
  Warn;
end;

function WSCheckHandleAllocated(const AWincontrol: TWinControl;
  const AProcName: String): Boolean;

  procedure Warn;
  begin
    DebugLn('[WARNING] %s called without handle for %s(%s)', [AProcName, AWincontrol.Name, AWincontrol.ClassName]);
  end;
begin
  Result := AWinControl.HandleAllocated;
  if Result then Exit;
  Warn;
end;

function WSCheckHandleAllocated(const AMenu: TMenu;
                                const AProcName: String): Boolean;
  procedure Warn;
  begin
    DebugLn('[WARNING] %s called without handle for %s(%s)', [AProcName, AMenu.Name, AMenu.ClassName]);
  end;
begin
  Result := AMenu.HandleAllocated;
  if Result then Exit;
  Warn;
end;



end.
