{
 /***************************************************************************
                    CocoaInt.pas  -  CocoaInterface Object
                    ----------------------------------------

                 Initial Revision  : Mon August 6th CST 2004


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
 }

unit CocoaInt;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Types, Classes, SysUtils, Math, GraphMath,
  LCLPlatformDef, InterfaceBase, GraphType,
  MacOSAll, CocoaAll,
  CocoaWSService, CocoaConst, CocoaConfig, CocoaPrivate, CocoaCallback,
  CocoaUtils, Cocoa_Extra, CocoaGDIObjects, CocoaCursor, CocoaMenus, CocoaWindows,
  CocoaScrollers, CocoaWSScrollers,
  CocoaWSClipboard, CocoaTextEdits,
  LMessages, LCLProc, LCLIntf, LCLType,
  Controls, Forms, Themes, Menus, ExtCtrls,
  IntfGraphics, Graphics;

type

  { TCocoaTimerObject }

  TCocoaTimerObject = objcclass(NSObject)
    func: TWSTimerProc;
    timer: NSTimer;
    function initWithInterval_func(interval: integer; timerFunc: TWSTimerProc): id; message 'initWithInterval:func:';
    procedure invalidate; message 'invalidate';
    procedure timerFireMethod(atimer: NSTimer); message 'timerFireMethod:';
  end;

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
    isrun : Boolean;
    modals : NSMutableDictionary;
    {$ifdef COCOAPPRUNNING_OVERRIDEPROPERTY}
    Stopped : Boolean;
    {$endif}

    // Store state of key modifiers so that we can emulate keyup/keydown
    // of keys like control, option, command, caps lock, shift
    PrevKeyModifiers : NSUInteger;
    SavedKeyModifiers : NSUInteger;

    procedure dealloc; override;
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
  end;

  { TModalSession }

  TModalSession = class(TObject)
    window : NSWindow;
    sess   : NSModalSession;
    // recording menu state for the modality stack
    // there's no limitation for a modal window to have its own menu
    // if it override the mainMenu, we still need the information
    // to restore the previous state of the mainmenu
    prevMenuEnabled: Boolean;
    cocoaMenu : NSMenu;
    lclMenu   : TMenu;
    constructor Create(awin: NSWindow; asess: NSModalSession;
      APrevMenuEnabled: Boolean;
      amainmenu: NSMenu; ALCL: TMenu);
  end;

  { TCocoaWidgetSet }

  TCocoaWidgetSet = class(TWidgetSet)
  private
    FTerminating: Boolean;
    FNSApp: TCocoaApplication;
    FNSApp_Delegate: TAppDelegate;
    FCaptureControl: HWND;
    FSendingScrollWheelCount: Integer;

  protected
    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;

    FStockNullPen: HPEN;
    FStockBlackPen: HPEN;
    FStockWhitePen: HPEN;
    FStockSystemFont: HFONT;
    FStockFixedFont: HFONT;

    FSysColorBrushes: array[0..MAX_SYS_COLORS] of HBrush;

    // Sandboxing
    SandboxingOn: Boolean;
    fClipboard: TCocoaWSClipboard;

    function nextEvent(const eventExpDate: NSDate): NSEvent;
    function nextEventBeforeRunLoop(const eventExpDate: NSDate): NSEvent;

    function isSendingScrollWheelFromInterface(): Boolean;

    procedure SyncClipboard();

    function PromptUser(const DialogCaption, DialogMessage: String;
      DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
      EscapeResult: Longint): Longint; override;
    function MessageBox(HWnd: HWND; lpText, lpCaption: PChar;
      uType: Cardinal): Integer; override;
    function GetAppHandle: TLCLHandle; override;
    function CreateThemeServices: TThemeServices; override;

    procedure SendCheckSynchronizeMessage;
    procedure OnWakeMainThread(Sender: TObject);
  public
    KeyWindow: NSWindow;
    KillingFocus: Boolean;

    // modal session
    Modals : TList;
    ModalCounter: Integer; // the cheapest way to determine if modal window was called
                           // used in mouse handling (in callbackobject)
                           // Might not be needed, if native Modality used

    constructor Create; override;
    destructor Destroy; override;

    function LCLPlatform: TLCLPlatform; override;

    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure AppRunMessages(onlyOne: Boolean; eventExpDate: NSDate);
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetIcon(const Small, Big: HICON); override;
    procedure AppSetTitle(const ATitle: string); override;
    function AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean=False
      ): Boolean; override;
    function AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean=False
      ): Boolean; override;

    function BeginMessageProcess: TLCLHandle; override;
    procedure EndMessageProcess(context: TLCLHandle); override;

    function  GetLCLCapability(ACapability: TLCLCapability): PtrUInt; override;

    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): TLCLHandle; override;
    function DestroyTimer(TimerHandle: TLCLHandle): boolean; override;

    procedure InitStockItems;
    procedure FreeStockItems;
    procedure FreeSysColorBrushes;

    function StartModal(awin: NSWindow; hasMenu: Boolean): Boolean;
    procedure EndModal(awin: NSWindow);
    function CurModalForm: NSWindow;
    function isTopModalWin(awin: NSWindow): Boolean;
    function isModalSession: Boolean;

    {todo:}
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean); override;
    procedure SetDesigning(AComponent: TComponent); override;

    function RawImage_DescriptionFromCocoaBitmap(out ADesc: TRawImageDescription; ABitmap: TCocoaBitmap): Boolean;
    function RawImage_FromCocoaBitmap(out ARawImage: TRawImage; ABitmap, AMask: TCocoaBitmap; ARect: PRect = nil): Boolean;
    function RawImage_DescriptionToBitmapType(ADesc: TRawImageDescription; out bmpType: TCocoaBitmapType): Boolean;
    function GetImagePixelData(AImage: CGImageRef; out bitmapByteCount: PtrUInt): Pointer;
    class function Create32BitAlphaBitmap(ABitmap, AMask: TCocoaBitmap): TCocoaBitmap;
    property NSApp: TCocoaApplication read FNSApp;
    property CaptureControl: HWND read FCaptureControl;
    // the winapi compatibility methods
    {$I cocoawinapih.inc}
    // the extra LCL interface methods
    {$I cocoalclintfh.inc}
  end;
  
var
  CocoaWidgetSet: TCocoaWidgetSet;

implementation

// NSCursor doesn't support any wait cursor, so we need to use a non-native one
// Not supporting it at all would result in crashes in Screen.Cursor := crHourGlass;
{$R ../../cursor_hourglass.res}

uses
  dl,dynlibs,
  CocoaCaret, CocoaThemes;

const
  // Lack of documentation, provisional definition
  LazarusApplicationDefinedSubtypeWakeup = 13579;

type

  TCocoaDialogUtil = class
  public
    class function promptUser(
      const DialogCaption : string;
      const DialogMessage : string;
      const DialogType    : LongInt;
      const Buttons       : PLongInt;
      const ButtonCount   : LongInt;
      const DefaultIndex  : LongInt;
      const EscapeResult  : LongInt;
      const sheetOfWindow : NSWindow = nil;
      const modalSheet    : Boolean = False ): LongInt;
  end;

function HWNDToTargetObject(AFormHandle: HWND): TObject;
var
  cb : ICommonCallback;
begin
  Result := nil;
  if AFormHandle = 0 then Exit;
  cb := NSObject(AFormHandle).lclGetCallback;
  if not Assigned(cb) then Exit;
  Result := cb.GetTarget;
end;

function HWNDToForm(AFormHandle: HWND): TCustomForm;
var
  obj : TObject;
begin
  obj := HWNDToTargetObject(AFormHandle);
  if Assigned(obj) and (obj is TCustomForm)
    then Result := TCustomForm(obj)
    else Result := nil;
end;

procedure wakeupEventLoop;
var
  ev: NSevent;
begin
  ev := NSEvent.otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2(
          NSApplicationDefined,
          NSZeroPoint,
          0, 0, 0, nil,
          LazarusApplicationDefinedSubtypeWakeup,
          0, 0);
  NSApp.postEvent_atStart(ev, false);
end;

{ TModalSession }

constructor TModalSession.Create(awin: NSWindow; asess: NSModalSession;
  APrevMenuEnabled: Boolean; amainmenu: NSMenu; ALCL: TMenu);
begin
  inherited Create;
  window := awin;
  sess := asess;
  prevMenuEnabled := APrevMenuEnabled;
  cocoaMenu := amainmenu;
  lclMenu   := alcl;
end;

{ TCocoaApplication }

procedure TCocoaApplication.dealloc;
begin
  if Assigned(modals) then modals.release;
  inherited dealloc;
end;

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

{$endif}

type
  AppClassMethod = objccategory external (NSObject)
    function sharedApplication: NSApplication; message 'sharedApplication';
  end;

// The function tries to initialize the proper application class.
// The desired application class can be specified in info.plit
// by specifying NSPrincipalClass property.
// If then principal class has been found (in the bundle binaries)
// InitApplication function will try to call its "sharedApplication" method.
// If principle class is not specified, then TCocoaApplication is used.
// You should always specify either TCocoaApplication or
// a class derived from TCocoaApplication, in order for LCL to fucntion properly
function InitApplication: TCocoaApplication;
var
  bun : NSBundle;
begin
  bun := NSBundle.mainBundle;
  if Assigned(bun) and Assigned(bun.principalClass) then
    Result := TCocoaApplication(NSObject(bun.principalClass).sharedApplication)
  else
    Result := TCocoaApplication(TCocoaApplication.sharedApplication);
end;

// the implementation of the utility methods
{$I cocoaobject.inc}
// the implementation of the winapi compatibility methods
{$I cocoawinapi.inc}
// the implementation of the extra LCL interface methods
{$I cocoalclintf.inc}

function TCocoaWidgetSet.StartModal(awin: NSWindow; hasMenu: Boolean): Boolean;
var
  sess : NSModalSession;
begin
  Result := false;
  if not Assigned(awin) then Exit;

  sess := NSApplication(NSApp).beginModalSessionForWindow(awin);
  if not Assigned(sess) then Exit;

  if not Assigned(Modals) then Modals := TList.Create;

  TCocoaMenuUtil.trackCancelAll();

  // If a modal menu has it's menu, then SetMainMenu has already been called
  // (Show is called for modal windows prior to ShowModal. Show triggers Activate and Active is doing MainMenu)
  if not hasMenu then begin
    Modals.Add( TModalSession.Create(awin, sess, CocoaWidgetSetMenuService.mainMenuEnabled, NSApplication(NSApp).mainMenu, CocoaWidgetSetMenuService.currentLCLMenu));
    CocoaWidgetSetMenuService.mainMenuEnabled := false;
    TCocoaMenuUtil.toggleAppMenu(false); // modal menu doesn't have a window, disabling it
  end else
    // if modal window has its own menu, then the prior window is rescord in "Prev" fields
    Modals.Add( TModalSession.Create(awin, sess, CocoaWidgetSetMenuService.prevMenuEnabled, CocoaWidgetSetMenuService.prevMenu, CocoaWidgetSetMenuService.prevLCLMenu));

  Result := true;
  inc(ModalCounter);
end;

procedure TCocoaWidgetSet.EndModal(awin: NSWindow);
var
  ms : TModalSession;
begin
  if not Assigned(Modals) or (Modals.Count = 0) then Exit;
  ms := TModalSession(Modals[Modals.Count-1]);
  if (ms.window <> awin) then Exit;
  NSApplication(NSApp).endModalSession(ms.sess);

  // restoring the menu status that was before the modality
  CocoaWidgetSetMenuService.DoSetMainMenu(ms.cocoaMenu, ms.lclMenu);
  CocoaWidgetSetMenuService.prevMenuEnabled := CocoaWidgetSetMenuService.mainMenuEnabled;
  CocoaWidgetSetMenuService.mainMenuEnabled := ms.prevMenuEnabled;
  TCocoaMenuUtil.toggleAppMenu(ms.prevMenuEnabled); // modal menu doesn't have a window, disabling it

  ms.Free;
  Modals.Delete(Modals.Count-1);

  wakeupEventLoop;
end;

function TCocoaWidgetSet.CurModalForm: NSWindow;
begin
  if isModalSession then begin
    Result := TModalSession(Modals[Modals.Count-1]).window;
  end else begin
    Result:= nil;
  end;
end;

function TCocoaWidgetSet.isTopModalWin(awin: NSWindow): Boolean;
begin
  if Assigned(awin) then begin
    Result:= CurModalForm=awin;
  end else begin
    Result:= false;
  end;
end;

function TCocoaWidgetSet.isModalSession: Boolean;
begin
  Result := Assigned(Modals) and (Modals.Count > 0);
end;

initialization
  CocoaWidgetSetService.initAutoreleaseMainPool;   // MainPool Stage 1 init
//  {$I Cocoaimages.lrs}

finalization
  CocoaWidgetSetService.finalAutoreleaseMainPool;  // MainPool Stage 2 Final

end.
