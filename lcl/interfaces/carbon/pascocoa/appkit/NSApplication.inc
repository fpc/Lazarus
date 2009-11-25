{%mainunit appkit.pas}
(*
	NSApplication.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSAPPLICATION_PAS_H}
{$define NSAPPLICATION_PAS_H}

{$include NSResponder.inc}
{$include AppKitDefines.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
const
  NSAppKitVersionNumber10_0 = 577;
  NSAppKitVersionNumber10_1 = 620;
  NSAppKitVersionNumber10_2 = 663;
  NSAppKitVersionNumber10_2_3 = 663.6;
  NSAppKitVersionNumber10_3 = 743;
  NSAppKitVersionNumber10_3_2 = 743.14;
  NSAppKitVersionNumber10_3_3 = 743.2;
  NSAppKitVersionNumber10_3_5 = 743.24;
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}

//APPKIT_EXTERN const double NSAppKitVersionNumber;
//APPKIT_EXTERN NSString *NSModalPanelRunLoopMode;
//APPKIT_EXTERN NSString *NSEventTrackingRunLoopMode;
  //  Pre-defined return values for runModalFor: and runModalSession:. The system also
  // reserves all values below these.
const
  NSRunStoppedResponse = 1000;
  NSRunAbortedResponse = 1001;
  NSRunContinuesResponse = 1002;

  //  used with NSRunLoop's performSelector:target:argument:order:modes:
const
  NSUpdateWindowsRunLoopOrdering = 500000;

//APPKIT_EXTERN id NSApp;
type
  _NSModalSession = Pointer;
  NSModalSession = _NSModalSession;

type
  NSThreadPrivate = Pointer;
  _NSThreadPrivate = NSThreadPrivate;

type
  NSApplicationTerminateReply = (
    NSTerminateCancel = 0, 
    NSTerminateNow = 1, 
    NSTerminateLater = 2
  );


type
  NSApplicationPrintReply = (
    NSPrintingCancelled = 0, 
    NSPrintingSuccess = 1, 
    NSPrintingFailure = 3, 
    NSPrintingReplyLater = 2
  );


{ An Application's startup function }

//APPKIT_EXTERN int NSApplicationMain(int argc, const char *argv[]);

//#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2
{ The startup function to call for a Cocoa bundle }

function NSApplicationLoad(): CBOOL; cdecl; external;
//#endif

{ NSShowsServicesMenuItem() always returns YES. }
//APPKIT_EXTERN BOOL NSShowsServicesMenuItem(NSString * itemName);

{ NSSetShowsServicesMenuItem() has no effect, and always returns 0. }
//APPKIT_EXTERN int NSSetShowsServicesMenuItem(NSString * itemName, BOOL enabled);

{ NSUpdateDynamicServices() causes the services information for the system to be updated.  This will only be necessary if your program adds dynamic services to the system (i.e. services not found in macho segments of executables).
}
{APPKIT_EXTERN void NSUpdateDynamicServices(void);
APPKIT_EXTERN BOOL NSPerformService(NSString *itemName, NSPasteboard *pboard);

APPKIT_EXTERN void NSRegisterServicesProvider(id provider, NSString *name); // apps should use -setServicesProvider
APPKIT_EXTERN void NSUnregisterServicesProvider(NSString *name);   }

{ Notifications }
{APPKIT_EXTERN NSString *NSApplicationDidBecomeActiveNotification;
APPKIT_EXTERN NSString *NSApplicationDidHideNotification;
APPKIT_EXTERN NSString *NSApplicationDidFinishLaunchingNotification;
APPKIT_EXTERN NSString *NSApplicationDidResignActiveNotification;
APPKIT_EXTERN NSString *NSApplicationDidUnhideNotification;
APPKIT_EXTERN NSString *NSApplicationDidUpdateNotification;
APPKIT_EXTERN NSString *NSApplicationWillBecomeActiveNotification;
APPKIT_EXTERN NSString *NSApplicationWillHideNotification;
APPKIT_EXTERN NSString *NSApplicationWillFinishLaunchingNotification;
APPKIT_EXTERN NSString *NSApplicationWillResignActiveNotification;
APPKIT_EXTERN NSString *NSApplicationWillUnhideNotification;
APPKIT_EXTERN NSString *NSApplicationWillUpdateNotification;
APPKIT_EXTERN NSString *NSApplicationWillTerminateNotification;
APPKIT_EXTERN NSString *NSApplicationDidChangeScreenParametersNotification;   }

type
  NSRequestUserAttentionType = (
    NSCriticalRequest = 0, 
    NSInformationalRequest = 10
  );


type
  NSApplicationDelegateReply = (
    NSApplicationDelegateReplySuccess = 0, 
    NSApplicationDelegateReplyCancel = 1, 
    NSApplicationDelegateReplyFailure = 2
  );


{$endif}
{$endif}

{$ifdef FORWARD}
  NSApplication = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAPPLICATION_PAS_C}
{$define NSAPPLICATION_PAS_C}

{$include NSResponder.inc}
{$include AppKitDefines.inc}

  { NSApplication }

  NSApplication = class(NSResponder)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    //  unused
    constructor sharedApplication;
    procedure setDelegate(_anObject: lobjc.id);
    function delegate: lobjc.id;
    function context: lobjc.id;{NSGraphicsContext}
    procedure hide(_sender: lobjc.id);
    procedure unhide(_sender: lobjc.id);
    procedure unhideWithoutActivation;
    function windowWithWindowNumber(_windowNum: Integer): lobjc.id;{NSWindow}
    function mainWindow: lobjc.id;{NSWindow}
    function keyWindow: lobjc.id;{NSWindow}
    function isActive: LongBool;
    function isHidden: LongBool;
    function isRunning: LongBool;
    procedure deactivate;
    procedure activateIgnoringOtherApps(_flag: LongBool);
    procedure hideOtherApplications(_sender: lobjc.id);
    procedure unhideAllApplications(_sender: lobjc.id);
    procedure finishLaunching;
    procedure run;
    function runModalForWindow(_theWindow: lobjc.id {NSWindow}): Integer;
    procedure stop(_sender: lobjc.id);
    procedure stopModal;
    procedure stopModalWithCode(_returnCode: Integer);
    procedure abortModal;
    function modalWindow: lobjc.id;{NSWindow}
    function beginModalSessionForWindow(_theWindow: lobjc.id {NSWindow}): NSModalSession;
    function runModalSession(_session: NSModalSession): Integer;
    procedure endModalSession(_session: NSModalSession);
    procedure terminate(_sender: lobjc.id);
    //  inform the user that this application needs attention - call this method only if
    // your application is not already active
    function requestUserAttention(_requestType: NSRequestUserAttentionType): Integer;
    procedure cancelUserAttentionRequest(_request: Integer);
    // **  Present a sheet on the given window.  When the modal session is ended,
    // ** the didEndSelector will be invoked in the modalDelegate.  The didEndSelector
    // ** should have the following signature, and will be invoked when the modal session
    // ends.
    // ** This method should dimiss the sheet using orderOut:
    // ** - (void)sheetDidEnd:(NSWindow *)sheet returnCode:(int)returnCode contextInfo:(void
    // *)contextInfo;
    // **
    procedure beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo(_sheet: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}; _modalDelegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
    procedure endSheet(_sheet: lobjc.id {NSWindow});
    procedure endSheet_returnCode(_sheet: lobjc.id {NSWindow}; _returnCode: Integer);
    // ** runModalForWindow:relativeToWindow: is deprecated. 
    // ** Please use beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:
    function runModalForWindow_relativeToWindow(_theWindow: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}): Integer;
    // 
    // ** beginModalSessionForWindow:relativeToWindow: is deprecated.
    // ** Please use beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:
    function beginModalSessionForWindow_relativeToWindow(_theWindow: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}): NSModalSession;
    function nextEventMatchingMask_untilDate_inMode_dequeue(_mask: LongWord; _expiration: CFDateRef; _mode: CFStringRef; _deqFlag: LongBool): lobjc.id;{NSEvent}
    procedure discardEventsMatchingMask_beforeEvent(_mask: LongWord; _lastEvent: lobjc.id {NSEvent});
    procedure postEvent_atStart(_event: lobjc.id {NSEvent}; _flag: LongBool);
    function currentEvent: lobjc.id;{NSEvent}
    procedure sendEvent(_theEvent: lobjc.id {NSEvent});
    procedure preventWindowOrdering;
    function makeWindowsPerform_inOrder(_aSelector: SEL; _flag: LongBool): lobjc.id;{NSWindow}
    function windows: CFArrayRef;
    procedure setWindowsNeedUpdate(_needUpdate: LongBool);
    procedure updateWindows;
    procedure setMainMenu(_aMenu: lobjc.id {NSMenu});
    function mainMenu: lobjc.id;{NSMenu}
    procedure setApplicationIconImage(_image: lobjc.id {NSImage});
    function applicationIconImage: lobjc.id;{NSImage}
    function sendAction_to_from(_theAction: SEL; _theTarget: lobjc.id; _sender: lobjc.id): LongBool;
    function targetForAction(_theAction: SEL): lobjc.id;
    function targetForAction_to_from(_theAction: SEL; _theTarget: lobjc.id; _sender: lobjc.id): lobjc.id;
    function tryToPerform_with(_anAction: SEL; _anObject: lobjc.id): LongBool;
    function validRequestorForSendType_returnType(_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id;
    procedure reportException(_theException: lobjc.id {NSException});
    class procedure detachDrawingThread_toTarget_withObject(_selector: SEL; _target: lobjc.id; _argument: lobjc.id);
    //   If an application delegate returns NSTerminateLater from -applicationShouldTerminate:,
    // -replyToApplicationShouldTerminate: must be called with YES or NO once the application
    // decides if it can terminate
    procedure replyToApplicationShouldTerminate(_shouldTerminate: LongBool);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    //  If an application delegate encounters an error while handling -application:openFiles:
    // or -application:printFiles:, -replyToOpenOrPrint: should be called with NSApplicationDelegateReplyFailure.
    //  If the user cancels the operation, NSApplicationDelegateReplyCancel should be used,
    // and if the operation succeeds, NSApplicationDelegateReplySuccess should be used
    procedure replyToOpenOrPrint(_reply: NSApplicationDelegateReply);
    //  Opens the character palette
    procedure orderFrontCharacterPalette(_sender: lobjc.id);
    {.$endif}
    procedure setWindowsMenu(_aMenu: lobjc.id {NSMenu});
    function windowsMenu: lobjc.id;{NSMenu}
    procedure arrangeInFront(_sender: lobjc.id);
    procedure removeWindowsItem(_win: lobjc.id {NSWindow});
    procedure addWindowsItem_title_filename(_win: lobjc.id {NSWindow}; _aString: CFStringRef; _isFilename: LongBool);
    procedure changeWindowsItem_title_filename(_win: lobjc.id {NSWindow}; _aString: CFStringRef; _isFilename: LongBool);
    procedure updateWindowsItem(_win: lobjc.id {NSWindow});
    procedure miniaturizeAll(_sender: lobjc.id);
    procedure setServicesMenu(_aMenu: lobjc.id {NSMenu});
    function servicesMenu: lobjc.id;{NSMenu}
    procedure registerServicesMenuSendTypes_returnTypes(_sendTypes: CFArrayRef; _returnTypes: CFArrayRef);
    procedure setServicesProvider(_provider: lobjc.id);
    function servicesProvider: lobjc.id;
    procedure orderFrontStandardAboutPanel(_sender: lobjc.id);
    procedure orderFrontStandardAboutPanelWithOptions(_optionsDictionary: CFDictionaryRef);
    //  Optional keys in optionsDictionary:
    // @"Credits": NSAttributedString displayed in the info area of the panel. If
    // not specified, contents obtained from "Credits.rtf" in [NSBundle mainBundle];
    // if not available, blank.
    // @"ApplicationName": NSString displayed in place of the default app name. If
    // not specified, uses the value of CFBundleName (localizable). Fallback is [[NSProcessInfo
    // processInfo] processName].
    // @"ApplicationIcon": NSImage displayed in place of NSApplicationIcon. If not
    // specified, use [NSImage imageNamed:@"NSApplicationIcon"]; if not available, generic
    // icon.
    // @"Copyright": NSString containing the copyright string. If not specified,
    // obtain from the value of NSHumanReadableCopyright (localizable) in infoDictionary;
    // if not available, leave blank.
    // @"Version": NSString containing the build version number of the application
    // ("58.4", "1.2d3"); displayed as "Version 58.4" or "Version 1.0 (58.4) depending on
    // the presence of ApplicationVersion.
    // If not specified, obtain from the CFBundleVersion key in infoDictionary; if not specified
    // or empty string, leave blank.
    // @"ApplicationVersion": NSString displayed as the marketing version  ("1.0", "Mac
    // OS X", "3", "WebObjects 3.5", ...), before the build version.
    // If not specified, obtain from CFBundleShortVersionString key in infoDictionary. Prefixed
    // with word "Version" if it looks like a number.
    class procedure setApplicationHandle_previousHandle_commandLine_show(_hInstance: Pointer; _PrevInstance: Pointer; _cmdLine: CFStringRef; _cmdShow: Integer);
    class procedure useRunningCopyOfApplication;
    function applicationHandle: Pointer;
    function windowWithWindowHandle(_hWnd: Pointer): lobjc.id;{NSWindow}
    procedure setAppleMenu(aMenu: lobjc.id {NSMenu});
  end;

    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    {.$endif}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    {.$endif}
{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSApplication_NSApplication = 'NSApplication';
  StrNSApplication_sharedApplication = 'sharedApplication';
  StrNSApplication_setDelegate = 'setDelegate:';
  StrNSApplication_delegate = 'delegate';
  StrNSApplication_context = 'context';
  StrNSApplication_hide = 'hide:';
  StrNSApplication_unhide = 'unhide:';
  StrNSApplication_unhideWithoutActivation = 'unhideWithoutActivation';
  StrNSApplication_windowWithWindowNumber = 'windowWithWindowNumber:';
  StrNSApplication_mainWindow = 'mainWindow';
  StrNSApplication_keyWindow = 'keyWindow';
  StrNSApplication_isActive = 'isActive';
  StrNSApplication_isHidden = 'isHidden';
  StrNSApplication_isRunning = 'isRunning';
  StrNSApplication_deactivate = 'deactivate';
  StrNSApplication_activateIgnoringOtherApps = 'activateIgnoringOtherApps:';
  StrNSApplication_hideOtherApplications = 'hideOtherApplications:';
  StrNSApplication_unhideAllApplications = 'unhideAllApplications:';
  StrNSApplication_finishLaunching = 'finishLaunching';
  StrNSApplication_run = 'run';
  StrNSApplication_runModalForWindow = 'runModalForWindow:';
  StrNSApplication_stop = 'stop:';
  StrNSApplication_stopModal = 'stopModal';
  StrNSApplication_stopModalWithCode = 'stopModalWithCode:';
  StrNSApplication_abortModal = 'abortModal';
  StrNSApplication_modalWindow = 'modalWindow';
  StrNSApplication_beginModalSessionForWindow = 'beginModalSessionForWindow:';
  StrNSApplication_runModalSession = 'runModalSession:';
  StrNSApplication_endModalSession = 'endModalSession:';
  StrNSApplication_terminate = 'terminate:';
  StrNSApplication_requestUserAttention = 'requestUserAttention:';
  StrNSApplication_cancelUserAttentionRequest = 'cancelUserAttentionRequest:';
  StrNSApplication_beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo = 'beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:';
  StrNSApplication_endSheet = 'endSheet:';
  StrNSApplication_endSheet_returnCode = 'endSheet:returnCode:';
  StrNSApplication_runModalForWindow_relativeToWindow = 'runModalForWindow:relativeToWindow:';
  StrNSApplication_beginModalSessionForWindow_relativeToWindow = 'beginModalSessionForWindow:relativeToWindow:';
  StrNSApplication_nextEventMatchingMask_untilDate_inMode_dequeue = 'nextEventMatchingMask:untilDate:inMode:dequeue:';
  StrNSApplication_discardEventsMatchingMask_beforeEvent = 'discardEventsMatchingMask:beforeEvent:';
  StrNSApplication_postEvent_atStart = 'postEvent:atStart:';
  StrNSApplication_currentEvent = 'currentEvent';
  StrNSApplication_sendEvent = 'sendEvent:';
  StrNSApplication_preventWindowOrdering = 'preventWindowOrdering';
  StrNSApplication_makeWindowsPerform_inOrder = 'makeWindowsPerform:inOrder:';
  StrNSApplication_windows = 'windows';
  StrNSApplication_setWindowsNeedUpdate = 'setWindowsNeedUpdate:';
  StrNSApplication_updateWindows = 'updateWindows';
  StrNSApplication_setMainMenu = 'setMainMenu:';
  StrNSApplication_mainMenu = 'mainMenu';
  StrNSApplication_setApplicationIconImage = 'setApplicationIconImage:';
  StrNSApplication_applicationIconImage = 'applicationIconImage';
  StrNSApplication_sendAction_to_from = 'sendAction:to:from:';
  StrNSApplication_targetForAction = 'targetForAction:';
  StrNSApplication_targetForAction_to_from = 'targetForAction:to:from:';
  StrNSApplication_tryToPerform_with = 'tryToPerform:with:';
  StrNSApplication_validRequestorForSendType_returnType = 'validRequestorForSendType:returnType:';
  StrNSApplication_reportException = 'reportException:';
  StrNSApplication_detachDrawingThread_toTarget_withObject = 'detachDrawingThread:toTarget:withObject:';
  StrNSApplication_replyToApplicationShouldTerminate = 'replyToApplicationShouldTerminate:';
  StrNSApplication_replyToOpenOrPrint = 'replyToOpenOrPrint:';
  StrNSApplication_orderFrontCharacterPalette = 'orderFrontCharacterPalette:';

  StrNSApplication_setWindowsMenu = 'setWindowsMenu:';
  StrNSApplication_windowsMenu = 'windowsMenu';
  StrNSApplication_arrangeInFront = 'arrangeInFront:';
  StrNSApplication_removeWindowsItem = 'removeWindowsItem:';
  StrNSApplication_addWindowsItem_title_filename = 'addWindowsItem:title:filename:';
  StrNSApplication_changeWindowsItem_title_filename = 'changeWindowsItem:title:filename:';
  StrNSApplication_updateWindowsItem = 'updateWindowsItem:';
  StrNSApplication_miniaturizeAll = 'miniaturizeAll:';

  StrNSApplication_setServicesMenu = 'setServicesMenu:';
  StrNSApplication_servicesMenu = 'servicesMenu';
  StrNSApplication_registerServicesMenuSendTypes_returnTypes = 'registerServicesMenuSendTypes:returnTypes:';

  StrNSApplication_setServicesProvider = 'setServicesProvider:';
  StrNSApplication_servicesProvider = 'servicesProvider';

  StrNSApplication_orderFrontStandardAboutPanel = 'orderFrontStandardAboutPanel:';
  StrNSApplication_orderFrontStandardAboutPanelWithOptions = 'orderFrontStandardAboutPanelWithOptions:';

  StrNSApplication_setApplicationHandle_previousHandle_commandLine_show = 'setApplicationHandle:previousHandle:commandLine:show:';
  StrNSApplication_useRunningCopyOfApplication = 'useRunningCopyOfApplication';
  StrNSApplication_applicationHandle = 'applicationHandle';
  StrNSApplication_windowWithWindowHandle = 'windowWithWindowHandle:';

  StrNSApplication_setAppleMenu = 'setAppleMenu:';

{ NSApplication }

class function NSApplication.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSApplication_NSApplication);
end;

constructor NSApplication.sharedApplication;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSApplication_sharedApplication)));
end;

procedure NSApplication.setDelegate(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setDelegate)), _anObject);
end;

function NSApplication.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_delegate)), []));
end;

function NSApplication.context: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_context)), []));
end;

procedure NSApplication.hide(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_hide)), _sender);
end;

procedure NSApplication.unhide(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_unhide)), _sender);
end;

procedure NSApplication.unhideWithoutActivation;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_unhideWithoutActivation)), []);
end;

function NSApplication.windowWithWindowNumber(_windowNum: Integer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_windowNum: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSApplication_windowWithWindowNumber)), _windowNum));
end;

function NSApplication.mainWindow: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_mainWindow)), []));
end;

function NSApplication.keyWindow: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_keyWindow)), []));
end;

function NSApplication.isActive: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_isActive)), []));
end;

function NSApplication.isHidden: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_isHidden)), []));
end;

function NSApplication.isRunning: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_isRunning)), []));
end;

procedure NSApplication.deactivate;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_deactivate)), []);
end;

procedure NSApplication.activateIgnoringOtherApps(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_activateIgnoringOtherApps)), _flag);
end;

procedure NSApplication.hideOtherApplications(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_hideOtherApplications)), _sender);
end;

procedure NSApplication.unhideAllApplications(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_unhideAllApplications)), _sender);
end;

procedure NSApplication.finishLaunching;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_finishLaunching)), []);
end;

procedure NSApplication.run;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_run)), []);
end;

function NSApplication.runModalForWindow(_theWindow: lobjc.id {NSWindow}): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theWindow: lobjc.id {NSWindow}): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSApplication_runModalForWindow)), _theWindow));
end;

procedure NSApplication.stop(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_stop)), _sender);
end;

procedure NSApplication.stopModal;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_stopModal)), []);
end;

procedure NSApplication.stopModalWithCode(_returnCode: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_returnCode: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_stopModalWithCode)), _returnCode);
end;

procedure NSApplication.abortModal;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_abortModal)), []);
end;

function NSApplication.modalWindow: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_modalWindow)), []));
end;

function NSApplication.beginModalSessionForWindow(_theWindow: lobjc.id {NSWindow}): NSModalSession;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theWindow: lobjc.id {NSWindow}): NSModalSession; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSModalSession(vmethod(Handle, sel_registerName(PChar(StrNSApplication_beginModalSessionForWindow)), _theWindow));
end;

function NSApplication.runModalSession(_session: NSModalSession): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_session: NSModalSession): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSApplication_runModalSession)), _session));
end;

procedure NSApplication.endModalSession(_session: NSModalSession);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_session: NSModalSession); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_endModalSession)), _session);
end;

procedure NSApplication.terminate(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_terminate)), _sender);
end;

function NSApplication.requestUserAttention(_requestType: NSRequestUserAttentionType): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_requestType: NSRequestUserAttentionType): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSApplication_requestUserAttention)), _requestType));
end;

procedure NSApplication.cancelUserAttentionRequest(_request: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_request: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_cancelUserAttentionRequest)), _request);
end;

procedure NSApplication.beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo(_sheet: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}; _modalDelegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sheet: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}; _modalDelegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_beginSheet_modalForWindow_modalDelegate_didEndSelector_contextInfo)), _sheet, _docWindow, _modalDelegate, _didEndSelector, _contextInfo);
end;

procedure NSApplication.endSheet(_sheet: lobjc.id {NSWindow});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sheet: lobjc.id {NSWindow}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_endSheet)), _sheet);
end;

procedure NSApplication.endSheet_returnCode(_sheet: lobjc.id {NSWindow}; _returnCode: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sheet: lobjc.id {NSWindow}; _returnCode: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_endSheet_returnCode)), _sheet, _returnCode);
end;

function NSApplication.runModalForWindow_relativeToWindow(_theWindow: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theWindow: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSApplication_runModalForWindow_relativeToWindow)), _theWindow, _docWindow));
end;

function NSApplication.beginModalSessionForWindow_relativeToWindow(_theWindow: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}): NSModalSession;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theWindow: lobjc.id {NSWindow}; _docWindow: lobjc.id {NSWindow}): NSModalSession; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSModalSession(vmethod(Handle, sel_registerName(PChar(StrNSApplication_beginModalSessionForWindow_relativeToWindow)), _theWindow, _docWindow));
end;

function NSApplication.nextEventMatchingMask_untilDate_inMode_dequeue(_mask: LongWord; _expiration: CFDateRef; _mode: CFStringRef; _deqFlag: LongBool): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_mask: LongWord; _expiration: CFDateRef; _mode: CFStringRef; _deqFlag: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSApplication_nextEventMatchingMask_untilDate_inMode_dequeue)), _mask, _expiration, _mode, _deqFlag));
end;

procedure NSApplication.discardEventsMatchingMask_beforeEvent(_mask: LongWord; _lastEvent: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mask: LongWord; _lastEvent: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_discardEventsMatchingMask_beforeEvent)), _mask, _lastEvent);
end;

procedure NSApplication.postEvent_atStart(_event: lobjc.id {NSEvent}; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_event: lobjc.id {NSEvent}; _flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_postEvent_atStart)), _event, _flag);
end;

function NSApplication.currentEvent: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_currentEvent)), []));
end;

procedure NSApplication.sendEvent(_theEvent: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_sendEvent)), _theEvent);
end;

procedure NSApplication.preventWindowOrdering;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_preventWindowOrdering)), []);
end;

function NSApplication.makeWindowsPerform_inOrder(_aSelector: SEL; _flag: LongBool): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aSelector: SEL; _flag: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSApplication_makeWindowsPerform_inOrder)), _aSelector, _flag));
end;

function NSApplication.windows: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_windows)), []));
end;

procedure NSApplication.setWindowsNeedUpdate(_needUpdate: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_needUpdate: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setWindowsNeedUpdate)), _needUpdate);
end;

procedure NSApplication.updateWindows;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_updateWindows)), []);
end;

procedure NSApplication.setMainMenu(_aMenu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aMenu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setMainMenu)), _aMenu);
end;

function NSApplication.mainMenu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_mainMenu)), []));
end;

procedure NSApplication.setApplicationIconImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setApplicationIconImage)), _image);
end;

function NSApplication.applicationIconImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_applicationIconImage)), []));
end;

function NSApplication.sendAction_to_from(_theAction: SEL; _theTarget: lobjc.id; _sender: lobjc.id): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theAction: SEL; _theTarget: lobjc.id; _sender: lobjc.id): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSApplication_sendAction_to_from)), _theAction, _theTarget, _sender));
end;

function NSApplication.targetForAction(_theAction: SEL): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theAction: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSApplication_targetForAction)), _theAction));
end;

function NSApplication.targetForAction_to_from(_theAction: SEL; _theTarget: lobjc.id; _sender: lobjc.id): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theAction: SEL; _theTarget: lobjc.id; _sender: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSApplication_targetForAction_to_from)), _theAction, _theTarget, _sender));
end;

function NSApplication.tryToPerform_with(_anAction: SEL; _anObject: lobjc.id): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anAction: SEL; _anObject: lobjc.id): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSApplication_tryToPerform_with)), _anAction, _anObject));
end;

function NSApplication.validRequestorForSendType_returnType(_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSApplication_validRequestorForSendType_returnType)), _sendType, _returnType));
end;

procedure NSApplication.reportException(_theException: lobjc.id {NSException});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_theException: lobjc.id {NSException}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_reportException)), _theException);
end;

class procedure NSApplication.detachDrawingThread_toTarget_withObject(_selector: SEL; _target: lobjc.id; _argument: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_selector: SEL; _target: lobjc.id; _argument: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSApplication_detachDrawingThread_toTarget_withObject)), _selector, _target, _argument);
end;

procedure NSApplication.replyToApplicationShouldTerminate(_shouldTerminate: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_shouldTerminate: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_replyToApplicationShouldTerminate)), _shouldTerminate);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSApplication.replyToOpenOrPrint(_reply: NSApplicationDelegateReply);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_reply: NSApplicationDelegateReply); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_replyToOpenOrPrint)), _reply);
end;

procedure NSApplication.orderFrontCharacterPalette(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_orderFrontCharacterPalette)), _sender);
end;

{.$endif}
procedure NSApplication.setWindowsMenu(_aMenu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aMenu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setWindowsMenu)), _aMenu);
end;

function NSApplication.windowsMenu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_windowsMenu)), []));
end;

procedure NSApplication.arrangeInFront(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_arrangeInFront)), _sender);
end;

procedure NSApplication.removeWindowsItem(_win: lobjc.id {NSWindow});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_win: lobjc.id {NSWindow}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_removeWindowsItem)), _win);
end;

procedure NSApplication.addWindowsItem_title_filename(_win: lobjc.id {NSWindow}; _aString: CFStringRef; _isFilename: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_win: lobjc.id {NSWindow}; _aString: CFStringRef; _isFilename: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_addWindowsItem_title_filename)), _win, _aString, _isFilename);
end;

procedure NSApplication.changeWindowsItem_title_filename(_win: lobjc.id {NSWindow}; _aString: CFStringRef; _isFilename: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_win: lobjc.id {NSWindow}; _aString: CFStringRef; _isFilename: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_changeWindowsItem_title_filename)), _win, _aString, _isFilename);
end;

procedure NSApplication.updateWindowsItem(_win: lobjc.id {NSWindow});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_win: lobjc.id {NSWindow}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_updateWindowsItem)), _win);
end;

procedure NSApplication.miniaturizeAll(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_miniaturizeAll)), _sender);
end;

procedure NSApplication.setServicesMenu(_aMenu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aMenu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setServicesMenu)), _aMenu);
end;

function NSApplication.servicesMenu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_servicesMenu)), []));
end;

procedure NSApplication.registerServicesMenuSendTypes_returnTypes(_sendTypes: CFArrayRef; _returnTypes: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sendTypes: CFArrayRef; _returnTypes: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_registerServicesMenuSendTypes_returnTypes)), _sendTypes, _returnTypes);
end;

procedure NSApplication.setServicesProvider(_provider: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_provider: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setServicesProvider)), _provider);
end;

function NSApplication.servicesProvider: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_servicesProvider)), []));
end;

procedure NSApplication.orderFrontStandardAboutPanel(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_orderFrontStandardAboutPanel)), _sender);
end;

procedure NSApplication.orderFrontStandardAboutPanelWithOptions(_optionsDictionary: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_optionsDictionary: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_orderFrontStandardAboutPanelWithOptions)), _optionsDictionary);
end;

class procedure NSApplication.setApplicationHandle_previousHandle_commandLine_show(_hInstance: Pointer; _PrevInstance: Pointer; _cmdLine: CFStringRef; _cmdShow: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_hInstance: Pointer; _PrevInstance: Pointer; _cmdLine: CFStringRef; _cmdShow: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSApplication_setApplicationHandle_previousHandle_commandLine_show)), _hInstance, _PrevInstance, _cmdLine, _cmdShow);
end;

class procedure NSApplication.useRunningCopyOfApplication;
begin
  objc_msgSend(getClass, sel_registerName(PChar(StrNSApplication_useRunningCopyOfApplication)), []);
end;

function NSApplication.applicationHandle: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSApplication_applicationHandle)), []));
end;

function NSApplication.windowWithWindowHandle(_hWnd: Pointer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_hWnd: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSApplication_windowWithWindowHandle)), _hWnd));
end;

procedure NSApplication.setAppleMenu(aMenu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;aMenu: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSApplication_setAppleMenu)), aMenu);
end;

{$endif}
