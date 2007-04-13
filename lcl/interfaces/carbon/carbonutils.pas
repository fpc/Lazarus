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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

// debugging defines
{$I carbondebug.inc}

uses
  FPCMacOSAll;

type
  TFourCC = packed array[0..3] of Char;

// in eventhandlers UInt32 and FourCCs are mixed, so we provide here some fixup wrappers
function MakeEventSpec(AClass: TFourCC; AKind: UInt32): EventTypeSpec; inline;
function MakeEventSpec(AClass, AKind: TFourCC): EventTypeSpec; inline;
function MakeEventSpec(AClass, AKind: UInt32): EventTypeSpec; inline;

function MakeFourCC(AFourCC: TFourCC): FourCharCode; inline;

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


implementation

uses
  CarbonProc, CarbonConsts;

function MakeEventSpec(AClass, AKind: TFourCC): EventTypeSpec; inline;
begin
  Result.eventClass := FourCharCode(AClass);
  Result.eventKind := FourCharCode(AKind);
end;

function MakeEventSpec(AClass, AKind: UInt32): EventTypeSpec; inline;
begin
  Result.eventClass := AClass;
  Result.eventKind := AKind;
end;

function MakeEventSpec(AClass: TFourCC; AKind: UInt32): EventTypeSpec; inline;
begin
  Result.eventClass := FourCharCode(AClass);
  Result.eventKind := AKind;
end;

function MakeFourCC(AFourCC: TFourCC): FourCharCode; inline;
begin
  Result := FourCharCode(AFourCC);
end;

function InstallMenuEventHandler(inMenu: MenuRef; inHandler: EventHandlerUPP;
  inNumTypes: UInt32; inList: EventTypeSpecPtr; inUserData: Pointer;
  outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetMenuEventTarget(inMenu), inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallMenuEventHandler', SInstallEvent);
end;

function InstallControlEventHandler(inControl: ControlRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetControlEventTarget(inControl), inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallControlEventHandler', SInstallEvent);
end;

function InstallWindowEventHandler(inWindow: WindowRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetWindowEventTarget(inWindow), inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallWindowEventHandler', SInstallEvent);
end;

function InstallApplicationEventHandler(inHandler: EventHandlerUPP;
  inNumTypes: UInt32; inList: EventTypeSpecPtr; inUserData: Pointer;
  outRef: EventHandlerRefPtr): Boolean;
begin
  Result := not OSError(
    InstallEventHandler(GetApplicationEventTarget, inHandler, inNumTypes,
      inList, inUserData, outRef), 'InstallApplicationEventHandler',
    SInstallEvent);
end;



end.
