{ $Id$
                  ------------------------------------------
                  carbonextra.pp  -  Common carbon utilities
                  ------------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains missing/corrected carbon definitions

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

unit CarbonExtra;
{$mode MacPas}

interface
{.$ALIGN MAC68K}
{.$LibExport+}


uses
  FPCMacOSAll;

(*
 *  Summary:
 *    Indicates the parent window edge on which a drawer will be shown.
 *)
(*
const
  kWindowEdgeDefault            = 0;  { This constant is typically used with the OpenDrawer API; it
                                        indicates that the drawer should be opened on whatever edge of the
                                        parent window has previously been set as the drawer's preferred
                                        edge.}
  kWindowEdgeTop                = 1;  { The drawer should open on the top edge of the parent window. }
  kWindowEdgeLeft               = 2;  { The drawer should open on the left edge of the parent window. }
  kWindowEdgeBottom             = 4;  { The drawer should open on the bottom edge of the parent window. }
  kWindowEdgeRight              = 8;  { The drawer should open on the right edge of the parent window. }
*)
(*
 *  Summary:
 *    Indicates the current state of a drawer window.
 *)
(*
const
  kWindowDrawerOpening          = 1;  { The drawer is opening, but is not yet fully open. }
  kWindowDrawerOpen             = 2;  { The drawer is fully open. }
  kWindowDrawerClosing          = 3;  { The drawer is closing, but is not yet fully closed. }
  kWindowDrawerClosed           = 4;   { The drawer is fully closed. }
*)
(*
type
  WindowDrawerState = UInt32;

function SetDrawerParent(inDrawerWindow: WindowRef; inParent: WindowRef): OSStatus;
function ToggleDrawer(inDrawerWindow: WindowRef): OSStatus;
function OpenDrawer(inDrawerWindow: WindowRef; inEdge: OptionBits; inAsync: Boolean): OSStatus; //AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER;
*)

(*
// missing macros
function InstallControlEventHandler(inControl: ControlRef; inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpec; inUserData: UNIV Ptr; outRef: EventHandlerRefPtr): OSStatus; inline;
function InstallWindowEventHandler(inWindow: WindowRef; inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpec; inUserData: UNIV Ptr; outRef: EventHandlerRefPtr): OSStatus; inline;
function InstallApplicationEventHandler(inHandler: EventHandlerUPP; inNumTypes: UInt32; inList: EventTypeSpec; inUserData: UNIV Ptr; outRef: EventHandlerRefPtr): OSStatus; inline;
*)
// wrong translated ???
//type
//  PEventTypeSpec = ^EventTypeSpec;

//function ReceiveNextEvent(inNumTypes: UInt32; inList: EventTypeSpecPtr; inTimeout: EventTimeout; inPullEvent: BOOLEAN; VAR outEvent: EventRef): OSStatus;

implementation

(*
function InstallControlEventHandler(inControl: ControlRef; inHandler: EventHandlerUPP; inNumTypes: UInt32; {CONST}VAR inList: EventTypeSpec; inUserData: UNIV Ptr; outRef: EventHandlerRefPtr): OSStatus; inline;
begin
  Result := InstallEventHandler(GetControlEventTarget(inControl), inHandler, inNumTypes, inList, inUserData, outRef);
end;

function InstallWindowEventHandler(inWindow: WindowRef; inHandler: EventHandlerUPP; inNumTypes: UInt32; {CONST}VAR inList: EventTypeSpec; inUserData: UNIV Ptr; outRef: EventHandlerRefPtr): OSStatus; inline;
begin
  Result := InstallEventHandler(GetWindowEventTarget(inWindow), inHandler, inNumTypes, inList, inUserData, outRef);
end;

function InstallApplicationEventHandler(inHandler: EventHandlerUPP; inNumTypes: UInt32; {CONST}VAR inList: EventTypeSpec; inUserData: UNIV Ptr; outRef: EventHandlerRefPtr): OSStatus; inline;
begin
  Result := InstallEventHandler(GetApplicationEventTarget, inHandler, inNumTypes, inList, inUserData, outRef);
end;
*)
end.