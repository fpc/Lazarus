unit CocoaApplication;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  Controls, Forms,
  InterfaceBase,
  MacOSAll, CocoaAll,
  CocoaPrivate, CocoaWSService, CocoaEvent, CocoaConst, CocoaConfig,
  CocoaWindows, CocoaMenus, CocoaThemes, CocoaCursor, CocoaUtils, Cocoa_Extra;

type

  TCocoaAppOnOpenURLNotify = procedure (const url: NSURL) of object;

  { TCocoaApplication }

  TCocoaApplication = objcclass(NSApplication)
  private
    _isInSandbox: Boolean;
    _lclMainLoop: TApplicationMainLoop;
    _onOpenURLObserver: TCocoaAppOnOpenURLNotify;
    // Sandboxing
    {$ifdef COCOAPPRUNNING_OVERRIDEPROPERTY}
    _stopped : Boolean;
    {$endif}
  private
    procedure onOpenURL( const url: NSURL );
      message 'lclOnOpenURL:';
    procedure lclSyncCheck(arg: id);
      message 'lclSyncCheck:';
  public
    procedure sendEvent(theEvent: NSEvent); override;
    function nextEventMatchingMask_untilDate_inMode_dequeue(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: LCLObjCBoolean): NSEvent; override;
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString;
      object_: id; change: NSDictionary; context_: pointer); override;

    {$ifdef COCOALOOPOVERRIDE}
    procedure run; override;
    {$endif}

    {$ifdef COCOAPPRUNNING_OVERRIDEPROPERTY}
    function isRunning: objc.ObjCBOOL; override;
    procedure stop(sender: id); override;
    {$endif}
  public
    function isInSandbox: Boolean; message 'lclIsInSandbox';
    function hasLCLMainLoop: Boolean; message 'lclHasLCLMainLoop';
    procedure setLCLMainLoop( const mainLoop: TApplicationMainLoop );
      message 'lclSetLCLMainLoop:';
    procedure setOpenURLObserver( const onOpenURLObserver: TCocoaAppOnOpenURLNotify );
      message 'lclSetOpenURLObserver:';
  public
    class function createApplication: TCocoaApplication;
      message 'lclInitApplication';
  end;

implementation

type

  TCocoaSharedApplicationExt = objccategory external (NSObject)
    function sharedApplication: NSApplication; message 'sharedApplication';
  end;

  { TAppDelegate }

  TAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocolFix)
  public
    procedure application_openURLs(sender: NSApplication; urls: NSArray);
    procedure applicationDidHide(notification: NSNotification);
    procedure applicationDidUnhide(notification: NSNotification);
    procedure applicationDidBecomeActive(notification: NSNotification);
    procedure applicationDidResignActive(notification: NSNotification);
    procedure applicationDidChangeScreenParameters(notification: NSNotification);
    procedure applicationWillFinishLaunching(notification: NSNotification);
    function applicationDockMenu(sender: NSApplication): NSMenu;
    procedure handleQuitAppEvent_withReplyEvent(event: NSAppleEventDescriptor; replyEvent: NSAppleEventDescriptor); message 'handleQuitAppEvent:withReplyEvent:';
  end;

{ TAppDelegate }

// when the delegate implements application_openURLs(), macOS no longer calls
// application_openFiles(). therefore, the action in application_openURLs is
// determined based on the type of URL.
// if they are local files, call WidgetSet.DropFiles(), otherwise call NSAPP.onOpenURL().
procedure TAppDelegate.application_openURLs(sender: NSApplication; urls: NSArray);
var
  url: NSURL;
  filenames: NSMutableArray;
begin
  filenames:= NSMutableArray.new;
  for url in urls do begin
    if url.isFileURL then begin
      filenames.addObject( url.path );
    end else begin
      TCocoaApplication(NSAPP).onOpenURL( url );
    end;
  end;
  if filenames.count > 0 then
    CocoaWidgetSetBaseService.tryDropFiles(filenames);
  filenames.release;
end;

procedure TAppDelegate.applicationDidHide(notification: NSNotification);
begin
  Application.IntfAppMinimize;
end;

procedure TAppDelegate.applicationDidUnhide(notification: NSNotification);
begin
  Application.IntfAppRestore;
end;

procedure TAppDelegate.applicationDidBecomeActive(notification: NSNotification);
var
  windows: NSArray;
  window: NSWindow;
  form: TObject;
  style: TFormStyle;
  i: Integer;
begin
  TCocoaEventTapUtil.enableTap;

  windows := NSApp.orderedWindows;
  for i:= windows.count-1 downto 0 do begin
    window:= NSWindow( windows.objectAtIndex(i) );
    if not window.isVisible then
      continue;
    if window.isKindOfClass(TCocoaWindow) then
      if TCocoaWindow(window).lclIsFullScreen then
        continue;
    form:= window.lclGetTarget;
    if not (form is TCustomForm) then
      continue;
    if csDesigning in TCustomForm(form).ComponentState then
      continue;
    style:= TCustomForm(form).FormStyle;
    if style in fsAllNonSystemStayOnTop then
      window.setLevel( NSFloatingWindowLevel );
  end;

  Application.IntfAppActivate;
end;

procedure TAppDelegate.applicationDidResignActive(notification: NSNotification);
var
  lastWindowNumber: NSInteger;
  topWindowNumber: NSInteger;
  windows: NSArray;
  window: NSWindow;
  form: TObject;
  style: TFormStyle;
  state: TFormState;
  i: Integer;
begin
  TCocoaEventTapUtil.disableTap;

  // no window in this space
  if NSWindow.windowNumbersWithOptions(0).count = 0 then
    Exit;

  windows:= NSApp.orderedWindows;

  // reset fsStayOnTop form
  lastWindowNumber:= 0;
  for i:=windows.count-1 downto 0 do begin
    window:= NSWindow( windows.objectAtIndex(i) );
    if not window.isVisible then
      continue;
    form:= window.lclGetTarget;
    if not (form is TCustomForm) then
      continue;
    if csDesigning in TCustomForm(form).ComponentState then
      continue;
    style:= TCustomForm(form).FormStyle;
    if style in fsAllNonSystemStayOnTop then begin
      window.setLevel( NSNormalWindowLevel );
      if lastWindowNumber<>0 then
        window.orderWindow_relativeTo( NSWindowAbove, lastWindowNumber );
    end;
    lastWindowNumber:= window.windowNumber;
  end;

  // find top window of NSNormalWindowLevel
  topWindowNumber:= 0;
  for window in windows do begin
    if not window.isVisible then
      continue;
    if window.level <> NSNormalWindowLevel then
      continue;
    topWindowNumber:= window.windowNumber;
    break;
  end;

  // bring up modal form
  for i:=windows.count-1 downto 0 do begin
    window:= NSWindow( windows.objectAtIndex(i) );
    if not window.isVisible then
      continue;
    form:= window.lclGetTarget;
    if not (form is TCustomForm) then
      continue;
    state:= TCustomForm(form).FormState;
    if fsModal in state then begin
      window.orderWindow_relativeTo( NSWindowAbove, topWindowNumber );
      topWindowNumber:= window.windowNumber;
    end;
  end;

  Application.IntfAppDeactivate;
  Application.DoBeforeMouseMessage(nil);
end;

procedure TAppDelegate.applicationDidChangeScreenParameters(notification: NSNotification);
begin
  Screen.UpdateMonitors;
  Screen.UpdateScreen;
end;

procedure TAppDelegate.applicationWillFinishLaunching(notification: NSNotification);
begin
  NSAppleEventManager.sharedAppleEventManager.setEventHandler_andSelector_forEventClass_andEventID(
    Self, ObjCSelector('handleQuitAppEvent:withReplyEvent:'), kCoreEventClass,
    kAEQuitApplication);
end;

function TAppDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
begin
  Result:= NSMenu.alloc.init;
  if Assigned(CocoaConfigMenu.dockMenu.customMenus) then
    TCocoaMenuUtil.attachLCLMenu(Result, CocoaConfigMenu.dockMenu.customMenus);
end;

procedure TAppDelegate.handleQuitAppEvent_withReplyEvent(event: NSAppleEventDescriptor; replyEvent: NSAppleEventDescriptor);
{ Capture "Quit Application" Apple Events, either from system shutdown/logout
  or sent by another application.  Don't use [applicationShouldTerminate:]
  because that terminates the app immediately after [applicationWillTerminate:]
  returns, so there's no chance to run finalization blocks }
var
  Cancel: Boolean;
  Reason: NSAppleEventDescriptor;
begin
  Cancel:= False;
  // Check if it's a system-wide event
  Reason := event.paramDescriptorForKeyword(kEventParamReason);
  if (Reason = nil) or
     (((Reason.typeCodeValue = kAEQuitAll) or
      (reason.typeCodeValue = kAEReallyLogOut) or
      (reason.typeCodeValue = kAERestart) or
      (reason.typeCodeValue = kAEShutDown))) then
  begin
    Application.IntfQueryEndSession(Cancel);
    if not Cancel then
      Application.IntfEndSession;
  end;

  if NOT Cancel then begin
    if NOT Application.Terminated then
      Application.Terminate;
    if Assigned(WakeMainThread) then
      WakeMainThread(nil);
  end;

  // Let caller know if the shutdown was cancelled
  if (not Application.Terminated) and (replyEvent.descriptorType <> typeNull) then
    replyEvent.setParamDescriptor_forKeyword(NSAppleEventDescriptor.descriptorWithInt32(userCanceledErr), keyErrorNumber);
end;

{ TCocoaApplication }

{$ifdef COCOALOOPOVERRIDE}
procedure TCocoaApplication.run;
begin
  CocoaWidgetSetBaseService.finalAutoreleaseMainPool;   // MainPool Stage 1 final
  CocoaWidgetSetBaseService.initAutoreleaseMainPool;    // MainPool Stage 2 init
  {$ifdef COCOAPPRUNNING_SETINTPROPERTY}
  setValue_forKey(NSNumber.numberWithBool(true), NSSTR('_running'));
  {$endif}
  _lclMainLoop();
end;
{$endif}

procedure TCocoaApplication.sendEvent(theEvent: NSEvent);
var
  cb : ICommonCallback;
  allowcocoa : Boolean;
  idx: integer;
  win : NSWindow;
  responder : NSResponder;

  procedure init;
  begin
    idx := CocoaWidgetSetBaseService.countWaitingReleasedLCLObjects;

    responder := nil;
    cb := nil;

    win:= theEvent.window;
    if NOT Assigned(win) then
      win := self.keyWindow;

    if NOT Assigned(win) then
      Exit;

    responder:= win.firstResponder;
    cb:= responder.lclGetCallback;
  end;

  procedure deinit;
  begin
    CocoaWidgetSetBaseService.releaseWaitingLCLObjects(idx);
  end;

  procedure setCocoaOnlyStateBeforeCocoa;
  begin
    case theEvent.type_ of
      NSKeyDown:
        // when NSKeyDown, always reset CocoaOnlyState
        if responder.conformsToProtocol(objcprotocol(NSTextInputClientProtocol)) then
          CocoaWidgetSetState.CocoaOnlyState := NSTextInputClientProtocol(responder).hasMarkedText
        else
          CocoaWidgetSetState.CocoaOnlyState := false;
      NSKeyUp:
        // when NSKeyUp, reset CocoaOnlyState only if it's false (last KeyDown set)
        // keep true if CocoaOnlyState=true
        if NOT CocoaWidgetSetState.CocoaOnlyState then begin
          if responder.conformsToProtocol(objcprotocol(NSTextInputClientProtocol)) then
            CocoaWidgetSetState.CocoaOnlyState := NSTextInputClientProtocol(responder).hasMarkedText;
        end;
    end;
  end;

  procedure setCocoaOnlyStateAfterCocoa;
  begin
    case theEvent.type_ of
      NSKeyDown,
      NSKeyUp,
      NSFlagsChanged: ;
      else
        Exit;
    end;

    if CocoaWidgetSetState.CocoaOnlyState then
      Exit;

    // retest IME state
    if responder.conformsToProtocol(objcprotocol(NSTextInputClientProtocol)) then
      CocoaWidgetSetState.CocoaOnlyState:= NSTextInputClientProtocol(responder).hasMarkedText;
  end;

begin
  {$ifdef COCOALOOPNATIVE}
  try
  {$endif}
  init;
  try
    if Assigned(cb) then begin
      setCocoaOnlyStateBeforeCocoa;
      allowCocoa:= cb.handleEventBeforeCocoa( theEvent );
      // may be triggered into IME state
      if allowCocoa then
        inherited sendEvent(theEvent);
      setCocoaOnlyStateAfterCocoa;
      cb.handleEventAfterCocoa( theEvent );
    end else begin
      inherited sendEvent(theEvent);
    end;
  finally
    deinit;
  end;
  {$ifdef COCOALOOPNATIVE}
    if CocoaWidgetSet.FTerminating then stop(nil);
  except
    if CocoaWidgetSet.FTerminating then stop(nil);
    if Assigned(Application) and Application.CaptureExceptions then
      Application.HandleException(Application)
    else
      raise;
  end;
  {$endif}
end;

function isMouseMoveEvent(tp: NSEventType): Boolean; inline;
begin
  Result := (tp = NSMouseMoved)
    or (tp = NSLeftMouseDragged)
    or (tp = NSRightMouseDragged)
    or (tp = NSOtherMouseDragged);
end;

type
  TCrackerApplication = class(TApplication);

function TCocoaApplication.nextEventMatchingMask_untilDate_inMode_dequeue(
  mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: LCLObjCBoolean
  ): NSEvent;
var
  cb : ICommonCallback;
begin
  CocoaWidgetSetState.prevKeyModifiers  := CocoaWidgetSetState.savedKeyModifiers;

  {$ifdef COCOALOOPHIJACK}
  if not isrun and Assigned(aloop) then begin
    isrun := True;
    Result := nil;
    aloop();
    stop(nil); // this should stop the main loop
    CocoaWidgetSetState.LoopHiJackEnded := true;
    exit;
  end;
  {$endif}

  {$ifdef BOOLFIX}
  Result:=inherited nextEventMatchingMask_untilDate_inMode_dequeue_(
    mask,
    expiration, mode, Ord(deqFlag));
  {$else}
  Result:=inherited nextEventMatchingMask_untilDate_inMode_dequeue(mask,
    expiration, mode, deqFlag);
  {$endif}

  {
    Monitors from NSEvent.addLocalMonitorForEvents aren't called until
    NSApplication.sendEvent, so it's not early enough to capture the tracking
    run loop behavior below or to prevent key events going into KeyEvBefore, and
    Application.OnUserInput doesn't allow filtering messages.
    see also !351
  }
  if Assigned(Result) and Assigned(CocoaConfigApplication.events.highestHandler) then begin
    if CocoaConfigApplication.events.highestHandler(Result) then
      Exit;
  end;

  if (Result.type_=NSApplicationDefined) and (Result.subtype=LazarusApplicationDefinedSubtypeWakeup) then
    Result:= nil;

  if not Assigned(Result) then
  begin
    {$ifdef COCOALOOPNATIVE}
    if Assigned(Application) then Application.Idle(true);
    {$endif}
    Exit;
  end;

  CocoaWidgetSetState.savedKeyModifiers := Result.modifierFlags;

  if ((mode = NSEventTrackingRunLoopMode) or mode.isEqualToString(NSEventTrackingRunLoopMode))
    and Assigned(CocoaWidgetSetState.trackedControl)
  then
  begin
    if Result.type_ = NSLeftMouseUp then
    begin
      //todo: send callback!
      CocoaWidgetSetState.trackedControl := nil;
    end
    else
    if isMouseMoveEvent(Result.type_) then
    begin
      cb := CocoaWidgetSetState.trackedControl.lclGetCallback;
      if Assigned(cb) then begin
        // if the mouse event was handled by LCL - just don't return it back to Cocoa
        // i.e. needed for LCL to handle MouseMove events during DragAndDrop
        // Cocoa should not process the mouseMove at this time!
        if cb.MouseMove(Result) then Result := nil;
      end;
    end;
  end;
end;

procedure TCocoaApplication.lclSyncCheck(arg: id);
begin
  {$ifdef COCOALOOPNATIVE}
  try
    CheckSynchronize;
    if Assigned(Application) then
      TCrackerApplication(Application).ProcessAsyncCallQueue;
  except
    if Assigned(Application) and Application.CaptureExceptions then
      Application.HandleException(Application)
    else
      raise;
  end;
  {$else}
  CheckSynchronize;
  if Assigned(Application) then
    TCrackerApplication(Application).ProcessAsyncCallQueue;
  {$endif}
end;

{$ifdef COCOAPPRUNNING_OVERRIDEPROPERTY}
function TCocoaApplication.isRunning: objc.ObjCBOOL;
begin
  Result:=not _stopped;
end;

procedure TCocoaApplication.stop(sender: id);
begin
  _stopped := true;
  inherited stop(sender);
end;
{$endif}

procedure TCocoaApplication.observeValueForKeyPath_ofObject_change_context(
  keyPath: NSString; object_: id; change: NSDictionary; context_: pointer);
var
  prior: Integer;
begin
  Inherited observeValueForKeyPath_ofObject_change_context( keyPath, object_, change, context_ );
  if keyPath.isEqualToString(NSSTR('effectiveAppearance')) then begin
    prior:= TCocoaNumberUtil.toInt( change.valueForKey(NSSTR('notificationIsPrior')) );
    if prior > 0 then
      Exit;
    NSAppearance.setCurrentAppearance( self.effectiveAppearance );
    TCocoaThemeServices.darwinThemeChangedNotify;
  end;
end;

procedure TCocoaApplication.onOpenURL(const url: NSURL);
begin
  if Assigned(_onOpenURLObserver) then
    _onOpenURLObserver( url );
end;

function TCocoaApplication.isInSandbox: Boolean;
begin
  Result:= _isInSandbox;
end;

function TCocoaApplication.hasLCLMainLoop: Boolean;
begin
  Result:= Assigned( _lclMainLoop );
end;

procedure TCocoaApplication.setLCLMainLoop( const mainLoop: TApplicationMainLoop );
begin
  _lclMainLoop:= mainLoop;
end;

procedure TCocoaApplication.setOpenURLObserver( const onOpenURLObserver: TCocoaAppOnOpenURLNotify);
begin
  _onOpenURLObserver:= onOpenURLObserver;
end;

// The function tries to initialize the proper application class.
// The desired application class can be specified in info.plit
// by specifying NSPrincipalClass property.
// If then principal class has been found (in the bundle binaries)
// InitApplication function will try to call its "sharedApplication" method.
// If principle class is not specified, then TCocoaApplication is used.
// You should always specify either TCocoaApplication or
// a class derived from TCocoaApplication, in order for LCL to fucntion properly
class function TCocoaApplication.createApplication: TCocoaApplication;
var
  bundle : NSBundle;
  dict: NSDictionary;
begin
  bundle := NSBundle.mainBundle;
  if Assigned(bundle) and Assigned(bundle.principalClass) then
    Result := TCocoaApplication(NSObject(bundle.principalClass).sharedApplication)
  else
    Result := TCocoaApplication(TCocoaApplication.sharedApplication);

  Result.setDelegate( NSApplicationDelegateProtocol(TAppDelegate.new) );

  dict := NSProcessInfo.processInfo.environment;
  Result._isInSandbox := dict.valueForKey(NSStr('APP_SANDBOX_CONTAINER_ID')) <> nil;

  WakeMainThread:= @CocoaWidgetSetBaseService.OnWakeMainThread;
end;

end.

