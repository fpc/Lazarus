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
  MacOSAll, CocoaAll, CocoaConst, CocoaConfig, CocoaPrivate, CocoaCallback,
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

  { TAppDelegate }

  TAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
  public
    procedure application_openFiles(sender: NSApplication; filenames: NSArray);
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

    function runModalForWindow(theWindow: NSWindow): NSInteger; override;
    procedure lclSyncCheck(arg: id); message 'lclSyncCheck:';
    {$ifdef COCOAPPRUNNING_OVERRIDEPROPERTY}
    function isRunning: objc.ObjCBOOL; override;
    procedure stop(sender: id); override;
    {$endif}
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
    FWaitingDropFiles: NSMutableArray;
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

    // Clipboard

    // collecting objects that needs to be released AFTER an event
    // has been processed
    ToCollect: TList;
    function RetainToCollect: Integer;
    procedure ReleaseToCollect(fromIdx: integer);

    function nextEvent(const eventExpDate: NSDate): NSEvent;
    function nextEventBeforeRunLoop(const eventExpDate: NSDate): NSEvent;

    function isSendingScrollWheelFromInterface(): Boolean;

    procedure SyncClipboard();
    procedure DropWaitingFiles;
    procedure DropFiles(filenames: NSArray);

    function PromptUser(const DialogCaption, DialogMessage: String;
      DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
      EscapeResult: Longint): Longint; override;
    function MessageBox(HWnd: HWND; lpText, lpCaption: PChar;
      uType: Cardinal): Integer; override;
    function GetAppHandle: TLCLHandle; override;
    function CreateThemeServices: TThemeServices; override;

    procedure SendCheckSynchronizeMessage;
    procedure OnWakeMainThread(Sender: TObject);

    procedure DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
  public
    KeyWindow: NSWindow;
    KillingFocus: Boolean;

    // modal session
    Modals : TList;
    ModalCounter: Integer; // the cheapest way to determine if modal window was called
                           // used in mouse handling (in callbackobject)
                           // Might not be needed, if native Modality used
    MainMenuEnabled: Boolean; // the latest main menu status
    PrevMenu : NSMenu;
    PrevLCLMenu : TMenu;
    CurLCLMenu: TMenu;
    PrevMenuEnabled: Boolean; // previous mainmenu status

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

    procedure SetMainMenu(const AMenu: HMENU; const ALCLMenu: TMenu);
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
    procedure AddToCollect(obj: TObject);
  end;
  
var
  CocoaWidgetSet: TCocoaWidgetSet;

function CocoaScrollBarSetScrollInfo(bar: TCocoaScrollBar; const ScrollInfo: TScrollInfo): Integer;
function CocoaScrollBarGetScrollInfo(bar: TCocoaScrollBar; var ScrollInfo: TScrollInfo): Boolean;

function CocoaPromptUser(const DialogCaption, DialogMessage: String;
    DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
    EscapeResult: Longint;
    sheetOfWindow: NSWindow = nil; modalSheet: Boolean = false): Longint;

function GetCocoaWindowAtPos(p: NSPoint): NSWindow;

// The function tries to initialize the proper application class.
// The desired application class can be specified in info.plit
// by specifying NSPrincipalClass property.
// If then principal class has been found (in the bundle binaries)
// InitApplication function will try to call its "sharedApplication" method.
// If principle class is not specified, then TCocoaApplication is used.
// You should always specify either TCocoaApplication or
// a class derived from TCocoaApplication, in order for LCL to fucntion properly
function InitApplication: TCocoaApplication;

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

var
  MainPool : NSAutoreleasePool = nil;

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

procedure InternalInit;
begin
  // MacOSX 10.6 reports a lot of warnings during initialization process
  // adding the autorelease pool for the whole Cocoa widgetset
  MainPool := NSAutoreleasePool.alloc.init;
end;

procedure InternalFinal;
begin
  if Assigned(MainPool) then
  begin
    MainPool.release;
    MainPool := nil;
  end;
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

function CocoaScrollBarSetScrollInfo(bar: TCocoaScrollBar; const ScrollInfo: TScrollInfo): Integer;
var
  pg  : Integer;
  mn  : Integer;
  mx  : Integer;
  dl  : Integer;
begin
  if not Assigned(bar) then
  begin
    Result := 0;
    Exit;
  end;

  if ScrollInfo.fMask and SIF_PAGE>0 then
  begin
    pg:=ScrollInfo.nPage;
  end
  else pg:=bar.pageInt;

  if ScrollInfo.fMask and SIF_RANGE>0 then
  begin
    mn:=ScrollInfo.nMin;
    mx:=ScrollInfo.nMax;
  end
  else
  begin
    mn:=bar.minInt;
    mx:=bar.maxInt;
  end;

  dl:=mx-mn;
  {$ifdef BOOLFIX}
  bar.setEnabled_(Ord(dl<>0));
  {$else}
  bar.SetEnabled(dl<>0);
  {$endif}

  // if changed page or range, the knob changes
  if ScrollInfo.fMask and (SIF_RANGE or SIF_PAGE)>0 then
  begin
    if dl<>0 then
      bar.setKnobProportion(pg/dl)
    else
      bar.setKnobProportion(1);
    bar.pageInt:=pg;
    bar.minInt:=mn;
    bar.maxInt:=mx;
  end;

  if ScrollInfo.fMask and SIF_POS > 0 then
    bar.lclSetPos( ScrollInfo.nPos );

  Result:=bar.lclPos;
end;

function CocoaScrollBarGetScrollInfo(bar: TCocoaScrollBar; var ScrollInfo: TScrollInfo): Boolean;
var
  l : integer;
begin
  Result:=Assigned(bar);
  if not Result then Exit;

  FillChar(ScrollInfo, sizeof(ScrollInfo), 0);
  ScrollInfo.cbSize:=sizeof(ScrollInfo);
  ScrollInfo.fMask:=SIF_ALL;
  ScrollInfo.nMin:=bar.minInt;
  ScrollInfo.nMax:=bar.maxInt;
  ScrollInfo.nPage:=bar.pageInt;
  ScrollInfo.nPos:=bar.lclPos;
  ScrollInfo.nTrackPos:=ScrollInfo.nPos;
  Result:=true;
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
  InternalFinal;   // MainPool Stage 1 final
  InternalInit;    // MainPool Stage 2 init
  {$ifdef COCOAPPRUNNING_SETINTPROPERTY}
  setValue_forKey(NSNumber.numberWithBool(true), NSSTR('_running'));
  {$endif}
  aloop();
end;
{$endif}

// ensure that gets the correct window at mouse pos
// 1. in Z-Order
// 2. on the active Space
// 3. in current App
// 4. is visible window
// 5. is not the misc window like Menu Bar
function GetCocoaWindowAtPos(p: NSPoint): NSWindow;
var
  windowNumber: NSInteger;
  windowNumbers: NSArray;
  window: NSWindow;
begin
  Result := nil;

  // ensure 1
  windowNumber := NSWindow.windowNumberAtPoint_belowWindowWithWindowNumber(p,0);
  windowNumbers := NSWindow.windowNumbersWithOptions(0);

  // ensure 2, 3, 4
  if not windowNumbers.containsObject(NSNumber.numberWithInt(windowNumber)) then
    exit;

  // ensure 5
  window := NSApp.windowWithWindowNumber(windowNumber);
  if Assigned(window) and (window.isKindOfClass(TCocoaWindow) or window.isKindOfClass(TCocoaPanel)) then
    Result := window;
end;

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
  w := GetCocoaWindowAtPos(p);;

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
    end;
  end;

  if (not Assigned(w)) or (not Assigned(theEvent.window)) or (w=theEvent.window) then
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
  idx := CocoaWidgetSet.RetainToCollect;
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

    CocoaWidgetSet.ReleaseToCollect(idx);

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
  if Assigned(Result) and Assigned(CocoaConfigApplication.event.highestHandler) then begin
    if CocoaConfigApplication.event.highestHandler(Result) then
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

function TCocoaApplication.runModalForWindow(theWindow: NSWindow): NSInteger;
begin
  ApplicationWillShowModal;

  Result:=inherited runModalForWindow(theWindow);
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
{$endif}

type
  AppClassMethod = objccategory external (NSObject)
    function sharedApplication: NSApplication; message 'sharedApplication';
  end;

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

procedure TCocoaWidgetSet.DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
var
  i: Integer;
  lCurItem: TMenuItem;
  lMenuObj: NSObject;
  lNSMenu: NSMenu absolute AMenu;
  appleMenuFound: Boolean = false;
begin
  if Assigned(PrevMenu) then PrevMenu.release;
  PrevMenu := NSApplication(NSApp).mainMenu;
  PrevMenu.retain;

  PrevLCLMenu := CurLCLMenu;
  CurLCLMenu := ALCLMenu;

  if (ALCLMenu = nil) or not ALCLMenu.HandleAllocated then begin
    NSApp.setMainMenu(lNSMenu);
    Exit;
  end;

  // Find the Apple menu, if the user provided any by setting the Caption to 
  // Some older docs say we should use setAppleMenu to obtain the Services/Hide/Quit items,
  // but its now private and in 10.10 it doesn't seam to do anything
  // NSApp.setAppleMenu(NSMenu(lMenuObj));
  for i := 0 to ALCLMenu.Items.Count-1 do
  begin
    lCurItem := ALCLMenu.Items.Items[i];
    if not lNSMenu.isKindOfClass_(TCocoaMenu) then Break;
    if not lCurItem.HandleAllocated then Continue;

    lMenuObj := NSObject(lCurItem.Handle);
    if not lMenuObj.isKindOfClass_(TCocoaMenuItem) then Continue;
    if TCocoaMenuItem(lMenuObj).isValidAppleMenu() then
    begin
      TCocoaMenu(lNSMenu).overrideAppleMenu(TCocoaMenuItem(lMenuObj));
      appleMenuFound:= true;
      Break;
    end;
  end;

  if lNSMenu.isKindOfClass(TCocoaMenu) then begin
    if NOT appleMenuFound then
      TCocoaMenu(lNSMenu).createAppleMenu();
    TCocoaMenu(lNSMenu).attachAppleMenu();
  end;

  NSApp.setMainMenu(lNSMenu);
end;

procedure TCocoaWidgetSet.SetMainMenu(const AMenu: HMENU; const ALCLMenu: TMenu);
begin
  if AMenu<>0 then
  begin
    DoSetMainMenu(NSMenu(AMenu), ALCLMenu);

    PrevMenuEnabled := MainMenuEnabled;
    MainMenuEnabled := true;
    ToggleAppMenu(true);
    //if not Assigned(ACustomForm.Menu) then ToggleAppMenu(false);

    // for modal windows work around bug, but doesn't work :(
    {$ifdef COCOA_USE_NATIVE_MODAL}
    {if CurModalForm <> nil then
    for i := 0 to lNSMenu.numberOfItems()-1 do
    begin
      lNSMenu.itemAtIndex(i).setTarget(TCocoaWSCustomForm.GetWindowFromHandle(CurModalForm));
    end;}
    {$endif}
  end;
end;

function TCocoaWidgetSet.StartModal(awin: NSWindow; hasMenu: Boolean): Boolean;
var
  sess : NSModalSession;
begin
  Result := false;
  if not Assigned(awin) then Exit;

  sess := NSApplication(NSApp).beginModalSessionForWindow(awin);
  if not Assigned(sess) then Exit;

  if not Assigned(Modals) then Modals := TList.Create;

  MenuTrackCancelAll();

  // If a modal menu has it's menu, then SetMainMenu has already been called
  // (Show is called for modal windows prior to ShowModal. Show triggers Activate and Active is doing MainMenu)
  if not hasMenu then begin
    Modals.Add( TModalSession.Create(awin, sess, MainMenuEnabled, NSApplication(NSApp).mainMenu, CurLCLMenu));
    MainMenuEnabled := false;
    ToggleAppMenu(false); // modal menu doesn't have a window, disabling it
  end else
    // if modal window has its own menu, then the prior window is rescord in "Prev" fields
    Modals.Add( TModalSession.Create(awin, sess, PrevMenuEnabled, PrevMenu, PrevLCLMenu));

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
  DoSetMainMenu(ms.cocoaMenu, ms.lclMenu);
  PrevMenuEnabled := MainMenuEnabled;
  MainMenuEnabled := ms.prevMenuEnabled;
  ToggleAppMenu(ms.prevMenuEnabled); // modal menu doesn't have a window, disabling it

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

procedure TCocoaWidgetSet.AddToCollect(obj: TObject);
begin
  // let's try to find an object. Do not add a duplicate
  if (ToCollect.IndexOf(Obj)>=0) then Exit;
  ToCollect.Add(obj);
end;

function TCocoaWidgetSet.RetainToCollect: Integer;
begin
  Result := ToCollect.Count;
end;

procedure TCocoaWidgetSet.ReleaseToCollect(fromIdx: integer);
var
  i  : integer;
begin
  for i := fromIdx to ToCollect.Count - 1 do
  begin
    TObject(ToCollect[i]).Free;
    ToCollect[i]:=nil;
  end;
  ToCollect.Pack;
end;

initialization
  InternalInit;   // MainPool Stage 1 init
//  {$I Cocoaimages.lrs}

finalization
  InternalFinal;  // MainPool Stage 2 Final

end.
