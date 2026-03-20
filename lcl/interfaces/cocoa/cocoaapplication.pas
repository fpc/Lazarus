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
  CocoaWSService, CocoaConst, CocoaConfig, CocoaPrivate, CocoaCallback,
  CocoaThemes, CocoaCursor, CocoaMenus, CocoaWindows, CocoaUtils, Cocoa_Extra;

type

  TCocoaAppOnOpenURLNotify = procedure (const url: NSURL) of object;

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

  { TCocoaApplication }

  TCocoaApplication = objcclass(NSApplication)
    aloop : TApplicationMainLoop;
    {$ifdef COCOAPPRUNNING_OVERRIDEPROPERTY}
    isrun : Boolean;
    Stopped : Boolean;
    {$endif}

    // Store state of key modifiers so that we can emulate keyup/keydown
    // of keys like control, option, command, caps lock, shift
    PrevKeyModifiers : NSUInteger;
    SavedKeyModifiers : NSUInteger;

    {$ifdef COCOALOOPOVERRIDE}
    procedure run; override;
    {$endif}
    procedure sendEvent(theEvent: NSEvent); override;
    function nextEventMatchingMask_untilDate_inMode_dequeue(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: LCLObjCBoolean): NSEvent; override;

    procedure lclSyncCheck(arg: id); message 'lclSyncCheck:';
    {$ifdef COCOAPPRUNNING_OVERRIDEPROPERTY}
    function isRunning: objc.ObjCBOOL; override;
    procedure stop(sender: id); override;
    {$endif}

    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString;
      object_: id; change: NSDictionary; context_: pointer); override;

    procedure onOpenURL( const url: NSURL ); message 'lclOnOpenURL:';
  private
    _onOpenURLObserver: TCocoaAppOnOpenURLNotify;
  public
    procedure setOpenURLObserver( const onOpenURLObserver: TCocoaAppOnOpenURLNotify );
      message 'lclSetOpenURLObserver:';
  public
    class function InitApplication: TCocoaApplication;
      message 'lclInitApplication';
  end;

type
  AppClassMethod = objccategory external (NSObject)
    function sharedApplication: NSApplication; message 'sharedApplication';
  end;

implementation

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
    CocoaWidgetSetService.tryDropFiles(filenames);
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
  Cancel := False;
  // Check if it's a system-wide event
  Reason := event.attributeDescriptorForKeyword(kEventParamReason);
  if (Reason <> nil) and
     ((Reason.typeCodeValue = kAEQuitAll) or
      (reason.typeCodeValue = kAEReallyLogOut) or
      (reason.typeCodeValue = kAERestart) or
      (reason.typeCodeValue = kAEShutDown)) then
  begin
    Application.IntfQueryEndSession(Cancel);
    if not Cancel then
      Application.IntfEndSession;
  end;
  // Try to quit
  if not Cancel then
  begin
    if Assigned(CocoaConfigApplication.events.onQuitApp) then
      CocoaConfigApplication.events.onQuitApp(PtrInt(nil))
    else if Assigned(Application.MainForm) then
      Application.MainForm.Close
    else
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
  CocoaWidgetSetService.finalAutoreleaseMainPool;   // MainPool Stage 1 final
  CocoaWidgetSetService.initAutoreleaseMainPool;    // MainPool Stage 2 init
  {$ifdef COCOAPPRUNNING_SETINTPROPERTY}
  setValue_forKey(NSNumber.numberWithBool(true), NSSTR('_running'));
  {$endif}
  aloop();
end;
{$endif}

procedure ForwardMouseMove(app: NSApplication; theEvent: NSEvent);
var
  w   : NSWindow;
  ev  : NSEvent;
  p   : NSPoint;
  wfr : NSRect;
begin
  if not app.isActive then
    exit;

  p := theEvent.mouseLocation;
  w := TCocoaWindowUtil.getWindowAtPos(p);;

  if Assigned(w) then
  begin
    wfr := w.contentRectForFrameRect(w.frame);
    // if mouse outside of ClientFrame of Window,
    // Cursor should be forced to default.
    // see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40515
    if not NSPointInRect(p, wfr) then
    begin
      if Screen.Cursor=crDefault then
        CursorHelper.ForceSetDefaultCursor
      else
        CursorHelper.SetScreenCursor;
      Application.DoBeforeMouseMessage( nil );
    end;
  end
  else
  begin
    Application.DoBeforeMouseMessage( nil );
    Exit;
  end;

  if (not Assigned(theEvent.window)) or (w=theEvent.window) then
    Exit;

  p.x := p.x - w.frame.origin.x;
  p.y := p.y - w.frame.origin.y;
  ev := NSEvent.mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure(
    theEvent.type_,
    p,
    theEvent.modifierFlags,
    theEvent.timestamp,
    w.windowNumber,
    theEvent.context,
    theEvent.eventNumber,
    theEvent.clickCount,
    theEvent.pressure
  );
  w.sendEvent(ev);
end;

procedure TCocoaApplication.sendEvent(theEvent: NSEvent);
var
  cb : ICommonCallback;
  wnd: TCocoaWindow;
  allowcocoa : Boolean;
  idx: integer;
  win : NSWindow;
  responder : NSResponder;
begin
  {$ifdef COCOALOOPNATIVE}
  try
  {$endif}
  idx := CocoaWidgetSetService.countWaitingReleasedLCLObjects;
  win := theEvent.window;
  if not Assigned(win) then win := self.keyWindow;

  responder := nil;
  cb := nil;

  if Assigned(win) then
  begin
    responder := win.firstResponder;
    cb := responder.lclGetCallback;
    if Assigned(cb) then
    begin
      case theEvent.type_ of
        NSKeyDown:
          // when NSKeyDown, always reset CocoaOnlyState
          if responder.conformsToProtocol(objcprotocol(NSTextInputClientProtocol)) then
            cb.CocoaOnlyState := NSTextInputClientProtocol(responder).hasMarkedText
          else
            cb.CocoaOnlyState := false;
        NSKeyUp:
          // when NSKeyUp, reset CocoaOnlyState only if it's false (last KeyDown set)
          // keep true if CocoaOnlyState=true
          if not cb.CocoaOnlyState then
          begin
            if responder.conformsToProtocol(objcprotocol(NSTextInputClientProtocol)) then
              cb.CocoaOnlyState := NSTextInputClientProtocol(responder).hasMarkedText;
          end;
      end;
    end;
  end;

  try
    if (theEvent.type_ = NSKeyDown) or (theEvent.type_ = NSKeyUp) or
       (theEvent.type_ = NSFlagsChanged) then begin
      if Assigned(cb) then
      begin
        try
          if win.isKindOfClass_(TCocoaWindow) then begin
            wnd := TCocoaWindow(win);
            wnd._keyEvCallback := cb;
          end
          else
            wnd := nil;

          if cb.IsCocoaOnlyState then
          begin
            // in IME state
            inherited sendEvent(theEvent);
          end
          else
          begin
            // not in IME state
            cb.KeyEvBefore(theEvent, allowcocoa);
            // may be triggered into IME state
            if allowcocoa then
              inherited sendEvent(theEvent);
            // retest IME state
            if responder.conformsToProtocol(objcprotocol(NSTextInputClientProtocol)) then
              cb.CocoaOnlyState := NSTextInputClientProtocol(responder).hasMarkedText;
            // if in IME state, pass KeyEvAfter
            if not cb.CocoaOnlyState then
              cb.KeyEvAfter;
          end;
        finally
          if Assigned(wnd) then
            wnd._keyEvCallback := nil;
        end;
        Exit;
      end;
    end;

    inherited sendEvent(theEvent);

    if (theEvent.type_ = NSMouseMoved) then ForwardMouseMove(Self, theEvent);

     // todo: this should be called for "Default" or "Modal" loops
     NSApp.updateWindows;

  finally

    CocoaWidgetSetService.releaseWaitingLCLObjects(idx);

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
  PrevKeyModifiers  := SavedKeyModifiers;

  {$ifdef COCOALOOPHIJACK}
  if not isrun and Assigned(aloop) then begin
    isrun := True;
    Result := nil;
    aloop();
    stop(nil); // this should stop the main loop
    LoopHiJackEnded := true;
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

  SavedKeyModifiers := Result.modifierFlags;

  if ((mode = NSEventTrackingRunLoopMode) or mode.isEqualToString(NSEventTrackingRunLoopMode))
    and Assigned(TrackedControl)
  then
  begin
    if Result.type_ = NSLeftMouseUp then
    begin
      //todo: send callback!
      TrackedControl := nil;
    end
    else
    if isMouseMoveEvent(Result.type_) then
    begin
      cb := TrackedControl.lclGetCallback;
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
  Result:=not Stopped;
end;

procedure TCocoaApplication.stop(sender: id);
begin
  Stopped := true;
  inherited stop(sender);
end;

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
class function TCocoaApplication.InitApplication: TCocoaApplication;
var
  bun : NSBundle;
begin
  bun := NSBundle.mainBundle;
  if Assigned(bun) and Assigned(bun.principalClass) then
    Result := TCocoaApplication(NSObject(bun.principalClass).sharedApplication)
  else
    Result := TCocoaApplication(TCocoaApplication.sharedApplication);
end;

{$endif}

end.

