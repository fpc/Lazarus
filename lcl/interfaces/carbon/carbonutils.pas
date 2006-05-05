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

interface

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
function InstallControlEventHandler(inControl: ControlRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): OSStatus; inline;
function InstallWindowEventHandler(inWindow: WindowRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): OSStatus; inline;
function InstallApplicationEventHandler(inHandler: EventHandlerUPP;
  inNumTypes: UInt32; inList: EventTypeSpecPtr; inUserData: Pointer;
  outRef: EventHandlerRefPtr): OSStatus; inline;


implementation

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

function InstallControlEventHandler(inControl: ControlRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): OSStatus; inline;
begin
  Result := InstallEventHandler(GetControlEventTarget(inControl), inHandler,
                                inNumTypes, inList, inUserData, outRef);
end;

function InstallWindowEventHandler(inWindow: WindowRef;
  inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpecPtr;
  inUserData: Pointer; outRef: EventHandlerRefPtr): OSStatus; inline;
begin
  Result := InstallEventHandler(GetWindowEventTarget(inWindow), inHandler,
                                inNumTypes, inList, inUserData, outRef);
end;

function InstallApplicationEventHandler(inHandler: EventHandlerUPP;
  inNumTypes: UInt32; inList: EventTypeSpecPtr; inUserData: Pointer;
  outRef: EventHandlerRefPtr): OSStatus; inline;
begin
  Result := InstallEventHandler(GetApplicationEventTarget, inHandler,
                                inNumTypes, inList, inUserData, outRef);
end;



end.