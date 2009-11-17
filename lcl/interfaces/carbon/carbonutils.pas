{ $Id$
                  ------------------------------------------
                  carbonutils.pp  -  Common carbon utilities
                  ------------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains common carbon untilities (usable by other projects)
 Procs needed for the Carbon <-> LCL interface go to CarbonProc

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
}

unit CarbonUtils;

{$mode objfpc}{$H+}
{$inline on}

interface

// defines
{$I carbondefines.inc}

uses
  MacOSAll;

type
  TFourCC = packed array[0..3] of Char;

// in eventhandlers UInt32 and FourCCs are mixed, so we provide here some fixup wrappers
function MakeEventSpec(AClass: TFourCC; AKind: UInt32): EventTypeSpec; //inline;
function MakeEventSpec(AClass, AKind: TFourCC): EventTypeSpec; //inline;
function MakeEventSpec(AClass, AKind: UInt32): EventTypeSpec; //inline;

function MakeFourCC(AFourCC: TFourCC): FourCharCode; //inline;

// Some missing macros (params differ)
function InstallMenuEventHandler(inMenu: MenuRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): Boolean;
function InstallControlEventHandler(inControl: ControlRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): Boolean;
function InstallWindowEventHandler(inWindow: WindowRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): Boolean;
function InstallApplicationEventHandler(inHandler: EventHandlerUPP;
  inNumTypes: UInt32; inList: EventTypeSpecPtr; inUserData: Pointer;
  outRef: EventHandlerRefPtr): Boolean;

function CGRectMake(x, y, width, height: Float32): CGRect;

implementation

uses
  CarbonProc, CarbonDbgConsts;

{------------------------------------------------------------------------------
  Name:    MakeEventSpec
  Params:  AClass - Event class
           AKind  - Event kind
  Returns: Event type specification
 ------------------------------------------------------------------------------}
function MakeEventSpec(AClass, AKind: TFourCC): EventTypeSpec;
begin
  Result.eventClass := FourCharCode(AClass);
  Result.eventKind := FourCharCode(AKind);
end;

{------------------------------------------------------------------------------
  Name:    MakeEventSpec
  Params:  AClass - Event class
           AKind  - Event kind
  Returns: Event type specification
 ------------------------------------------------------------------------------}
function MakeEventSpec(AClass, AKind: UInt32): EventTypeSpec;
begin
  Result.eventClass := AClass;
  Result.eventKind := AKind;
end;

{------------------------------------------------------------------------------
  Name:    MakeEventSpec
  Params:  AClass - Event class
           AKind  - Event kind
  Returns: Event type specification
 ------------------------------------------------------------------------------}
function MakeEventSpec(AClass: TFourCC; AKind: UInt32): EventTypeSpec;
begin
  Result.eventClass := FourCharCode(AClass);
  Result.eventKind := AKind;
end;

{------------------------------------------------------------------------------
  Name:    MakeFourCC
  Params:  AFourCC - Four char code
  Returns: Four char code
 ------------------------------------------------------------------------------}
function MakeFourCC(AFourCC: TFourCC): FourCharCode;
begin
  Result := FourCharCode(AFourCC);
end;

{------------------------------------------------------------------------------
  Name:    InstallMenuEventHandler
  Params:  inMenu     - Event target menu
           inHandler  - Event handler
           inNumTypes - Count of event types in list
           inList     - The list of event types
           inUserData - User data passed to handler
           outRef     - Reference to handler for disposing
  Returns: If the function succeeds

  Installs the handler for the specified event types on the menu
 ------------------------------------------------------------------------------}
function InstallMenuEventHandler(inMenu: MenuRef; inHandler: EventHandlerUPP;
  inNumTypes: UInt32; inList: EventTypeSpecPtr; inUserData: Pointer;
  outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetMenuEventTarget(inMenu), inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallMenuEventHandler', SInstallEvent);
end;

{------------------------------------------------------------------------------
  Name:    InstallControlEventHandler
  Params:  inControl  - Event target control
           inHandler  - Event handler
           inNumTypes - Count of event types in list
           inList     - The list of event types
           inUserData - User data passed to handler
           outRef     - Reference to handler for disposing
  Returns: If the function succeeds
  
  Installs the handler for the specified event types on the control
 ------------------------------------------------------------------------------}
function InstallControlEventHandler(inControl: ControlRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetControlEventTarget(inControl), inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallControlEventHandler', SInstallEvent);
end;

{------------------------------------------------------------------------------
  Name:    InstallWindowEventHandler
  Params:  inWindow   - Event target window
           inHandler  - Event handler
           inNumTypes - Count of event types in list
           inList     - The list of event types
           inUserData - User data passed to handler
           outRef     - Reference to handler for disposing
  Returns: If the function succeeds

  Installs the handler for the specified event types on the window
 ------------------------------------------------------------------------------}
function InstallWindowEventHandler(inWindow: WindowRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetWindowEventTarget(inWindow), inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallWindowEventHandler', SInstallEvent);
end;

{------------------------------------------------------------------------------
  Name:    InstallApplicationEventHandler
  Params:  inHandler  - Event handler
           inNumTypes - Count of event types in list
           inList     - The list of event types
           inUserData - User data passed to handler
           outRef     - Reference to handler for disposing
  Returns: If the function succeeds

  Installs the handler for the specified event types on the application
 ------------------------------------------------------------------------------}
function InstallApplicationEventHandler(inHandler: EventHandlerUPP;
  inNumTypes: UInt32; inList: EventTypeSpecPtr; inUserData: Pointer;
  outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetApplicationEventTarget, inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallApplicationEventHandler',
    SInstallEvent);
end;


function CGRectMake(x, y, width, height: Float32): CGRect;
begin
  Result.origin.x := x;
  Result.origin.y := y;
  Result.size.width := width;
  Result.size.height := height;
end;

end.
