{%mainunit appkit.pas}
(*
	NSPanel.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSPANEL_PAS_H}
{$define NSPANEL_PAS_H}

{$include NSWindow.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}

//APPKIT_EXTERN int NSRunAlertPanel(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, ...);
//APPKIT_EXTERN int NSRunInformationalAlertPanel(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, ...);
//APPKIT_EXTERN int NSRunCriticalAlertPanel(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, ...);
//APPKIT_EXTERN int NSRunAlertPanelRelativeToWindow(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, NSWindow *docWindow, ...);
//APPKIT_EXTERN int NSRunInformationalAlertPanelRelativeToWindow(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, NSWindow *docWindow, ...);
//APPKIT_EXTERN int NSRunCriticalAlertPanelRelativeToWindow(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, NSWindow *docWindow, ...);
//APPKIT_EXTERN void NSBeginAlertSheet(NSString *title, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, NSWindow *docWindow, id modalDelegate, SEL didEndSelector, SEL didDismissSelector, void *contextInfo, NSString *msg, ...);
//APPKIT_EXTERN void NSBeginInformationalAlertSheet(NSString *title, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, NSWindow *docWindow, id modalDelegate, SEL didEndSelector, SEL didDismissSelector, void *contextInfo, NSString *msg, ...);
//APPKIT_EXTERN void NSBeginCriticalAlertSheet(NSString *title, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, NSWindow *docWindow, id modalDelegate, SEL didEndSelector, SEL didDismissSelector, void *contextInfo, NSString *msg, ...);
//APPKIT_EXTERN id NSGetAlertPanel(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, ...);
//APPKIT_EXTERN id NSGetInformationalAlertPanel(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, ...);
//APPKIT_EXTERN id NSGetCriticalAlertPanel(NSString *title, NSString *msg, NSString *defaultButton, NSString *alternateButton, NSString *otherButton, ...);
//APPKIT_EXTERN void NSReleaseAlertPanel(id panel);
  //  * NSRunAlertPanel() return values (also returned by runModalSession: when
  //  * the modal session is run with a panel returned by NSGetAlertPanel()).
  // 
const
  NSAlertDefaultReturn = 1;
  NSAlertAlternateReturn = 0;
  NSAlertOtherReturn = 1;
  NSAlertErrorReturn = 2;

const
  NSOKButton = 1;
  NSCancelButton = 0;

  //  Panel specific styleMask
const
  NSUtilityWindowMask = 1 shl 4;
  NSDocModalWindowMask = 1 shl 6;

const
  NSNonactivatingPanelMask = 1 shl 7;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPanel = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPANEL_PAS_C}
{$define NSPANEL_PAS_C}

{$include NSWindow.inc}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    {.$endif}

  { NSPanel }

  NSPanel = class(NSWindow)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    function isFloatingPanel: LongBool;
    procedure setFloatingPanel(_flag: LongBool);
    function becomesKeyOnlyIfNeeded: LongBool;
    procedure setBecomesKeyOnlyIfNeeded(_flag: LongBool);
    function worksWhenModal: LongBool;
    procedure setWorksWhenModal(_flag: LongBool);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSPanel_NSPanel = 'NSPanel';
  StrNSPanel_isFloatingPanel = 'isFloatingPanel';
  StrNSPanel_setFloatingPanel = 'setFloatingPanel:';
  StrNSPanel_becomesKeyOnlyIfNeeded = 'becomesKeyOnlyIfNeeded';
  StrNSPanel_setBecomesKeyOnlyIfNeeded = 'setBecomesKeyOnlyIfNeeded:';
  StrNSPanel_worksWhenModal = 'worksWhenModal';
  StrNSPanel_setWorksWhenModal = 'setWorksWhenModal:';

{ NSPanel }

class function NSPanel.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSPanel_NSPanel);
end;

function NSPanel.isFloatingPanel: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSPanel_isFloatingPanel)), []));
end;

procedure NSPanel.setFloatingPanel(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSPanel_setFloatingPanel)), _flag);
end;

function NSPanel.becomesKeyOnlyIfNeeded: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSPanel_becomesKeyOnlyIfNeeded)), []));
end;

procedure NSPanel.setBecomesKeyOnlyIfNeeded(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSPanel_setBecomesKeyOnlyIfNeeded)), _flag);
end;

function NSPanel.worksWhenModal: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSPanel_worksWhenModal)), []));
end;

procedure NSPanel.setWorksWhenModal(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSPanel_setWorksWhenModal)), _flag);
end;

{$endif}
