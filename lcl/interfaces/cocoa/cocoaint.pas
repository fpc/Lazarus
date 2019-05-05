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
  // rtl+ftl
  Types, Classes, SysUtils, Math, contnrs,
  // carbon bindings
  MacOSAll,
  // interfacebase
  LCLPlatformDef, InterfaceBase, GraphType,
  // private
  CocoaAll, CocoaPrivate, CocoaUtils, CocoaGDIObjects,
  cocoa_extra, CocoaWSMenus, CocoaWSForms, CocoaWindows, CocoaScrollers,
  CocoaWSClipboard, CocoaTextEdits, CocoaWSCommon,
  // LCL
  LCLStrConsts, LMessages, LCLMessageGlue, LCLProc, LCLIntf, LCLType,
  Controls, Forms, Themes, Menus,
  IntfGraphics, Graphics, CocoaWSFactory;

type

  { TCocoaTimerObject }

  TCocoaTimerObject = objcclass(NSObject)
    func: TWSTimerProc;
    procedure timerEvent; message 'timerEvent';
    class function newWithFunc(afunc: TWSTimerProc): TCocoaTimerObject; message 'newWithFunc:';
  end;

  TAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
    procedure application_openFiles(sender: NSApplication; filenames: NSArray);
    procedure applicationDidHide(notification: NSNotification);
    procedure applicationDidUnhide(notification: NSNotification);
    procedure applicationDidBecomeActive(notification: NSNotification);
    procedure applicationDidResignActive(notification: NSNotification);
    procedure applicationDidChangeScreenParameters(notification: NSNotification);
  end;

  { TCocoaApplication }

  TCocoaApplication = objcclass(NSApplication)
    aloop : TApplicationMainLoop;
    isrun : Boolean;
    modals : NSMutableDictionary;

    procedure dealloc; override;
    function isRunning: LCLObjCBoolean; override;
    procedure run; override;
    procedure sendEvent(theEvent: NSEvent); override;
    function nextEventMatchingMask_untilDate_inMode_dequeue(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: LCLObjCBoolean): NSEvent; override;

    function runModalForWindow(theWindow: NSWindow): NSInteger; override;
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
    FNSApp: NSApplication;
    FNSApp_Delegate: TAppDelegate;
    FCurrentCursor: HCursor;
    FCaptureControl: HWND;

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

    procedure SyncClipboard();

    function PromptUser(const DialogCaption, DialogMessage: String;
      DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
      EscapeResult: Longint): Longint; override;
    function MessageBox(HWnd: HWND; lpText, lpCaption: PChar;
      uType: Cardinal): Integer; override;
    function GetAppHandle: THandle; override;
    function CreateThemeServices: TThemeServices; override;

    procedure SendCheckSynchronizeMessage;
    procedure OnWakeMainThread(Sender: TObject);

    procedure DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
  public
    // modal session
    CurModalForm: NSWindow;
    Modals : TList;
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
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetIcon(const Small, Big: HICON); override;
    procedure AppSetTitle(const ATitle: string); override;

    function  GetLCLCapability(ACapability: TLCLCapability): PtrUInt; override;

    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle; override;
    function DestroyTimer(TimerHandle: THandle): boolean; override;
    function NewUserEventInfo(Handle: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): NSMutableDictionary;
    function PrepareUserEvent(Handle: HWND; Info: NSDictionary; NeedsResult: Boolean): NSEvent;

    procedure InitStockItems;
    procedure FreeStockItems;
    procedure FreeSysColorBrushes;

    procedure SetMainMenu(const AMenu: HMENU; const ALCLMenu: TMenu);
    function StartModal(awin: NSWindow; hasMenu: Boolean): Boolean;
    procedure EndModal(awin: NSWindow);
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
    property NSApp: NSApplication read FNSApp;
    property CurrentCursor: HCursor read FCurrentCursor write FCurrentCursor;
    property CaptureControl: HWND read FCaptureControl;
    // the winapi compatibility methods
    {$I cocoawinapih.inc}
    // the extra LCL interface methods
    {$I cocoalclintfh.inc}
  end;
  
var
  CocoaWidgetSet: TCocoaWidgetSet;
  CocoaBasePPI : Integer = 96; // for compatiblity with LCL 1.8 release. The macOS base is 72ppi
  MainPool : NSAutoreleasePool = nil;

function CocoaScrollBarSetScrollInfo(bar: TCocoaScrollBar; const ScrollInfo: TScrollInfo): Integer;
function CocoaScrollBarGetScrollInfo(bar: TCocoaScrollBar; var ScrollInfo: TScrollInfo): Boolean;
procedure NSScrollerGetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; Var ScrollInfo: TScrollInfo);
procedure NSScrollViewGetScrollInfo(sc: NSScrollView; BarFlag: Integer; Var ScrollInfo: TScrollInfo);
procedure NSScrollerSetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; const ScrollInfo: TScrollInfo);
procedure NSScrollViewSetScrollPos(sc: NSScrollView; BarFlag: Integer; const ScrollInfo: TScrollInfo);

function CocoaPromptUser(const DialogCaption, DialogMessage: String;
    DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
    EscapeResult: Longint;
    sheetOfWindow: NSWindow = nil; modalSheet: Boolean = false): Longint;

implementation


// NSCursor doesn't support any wait cursor, so we need to use a non-native one
// Not supporting it at all would result in crashes in Screen.Cursor := crHourGlass;
{$R ../../cursor_hourglass.res}

uses
  dl,dynlibs,
  CocoaCaret,
  CocoaThemes;

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

procedure NSScrollerGetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; Var ScrollInfo: TScrollInfo);
begin
  ScrollInfo.cbSize:=sizeof(ScrollInfo);
  ScrollInfo.fMask:=SIF_ALL;
  ScrollInfo.nPos:=round(rl.floatValue*(docSz-pageSz));
  ScrollInfo.nTrackPos:=ScrollInfo.nPos;
  ScrollInfo.nMin:=0;
  ScrollInfo.nMax:=round(docSz);
  ScrollInfo.nPage:=round(rl.knobProportion*docSz);
end;

procedure NSScrollViewGetScrollInfo(sc: NSScrollView; BarFlag: Integer; Var ScrollInfo: TScrollInfo);
var
  ns : NSView;
  vr : NSRect;
begin
  ns:=sc.documentView;
  if not Assigned(ns) then begin
    FillChar(ScrollInfo, sizeof(ScrollInfo),0);
    ScrollInfo.cbSize:=sizeof(ScrollInfo);
    Exit;
  end;
  vr:=sc.documentVisibleRect;
  if BarFlag = SB_Vert then
    NSScrollerGetScrollInfo(ns.frame.size.height, vr.size.height, sc.verticalScroller, ScrollInfo)
  else
    NSScrollerGetScrollInfo(ns.frame.size.width, vr.size.width, sc.horizontalScroller, ScrollInfo);
end;

procedure NSScrollerSetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; const ScrollInfo: TScrollInfo);
var
  sz : CGFloat;
begin
  if ScrollInfo.fMask and SIF_POS>0 then begin
    sz:=docSz-pageSz;
    if sz=0 then rl.setFloatValue(0)
    else rl.setFloatValue(ScrollInfo.nPos/sz);
  end;
  if ScrollInfo.fMask and SIF_PAGE>0 then begin
    sz:=docSz-pageSz;
    if sz=0 then rl.setKnobProportion(1)
    else rl.setKnobProportion(1/sz);
  end;
end;

procedure NSScrollViewSetScrollPos(sc: NSScrollView; BarFlag: Integer; const ScrollInfo: TScrollInfo);
var
  ns : NSView;
  vr : NSRect;
begin
  ns:=sc.documentView;
  if not Assigned(ns) then Exit;

  vr:=sc.documentVisibleRect;
  if BarFlag = SB_Vert then
  begin
    //NSScrollerSetScrollInfo(ns.frame.size.height, sc.verticalScroller, ScrollInfo)
    if not sc.documentView.isFlipped then
      vr.origin.y := sc.documentView.frame.size.height - ScrollInfo.nPos - vr.size.Height
    else
      vr.origin.y := ScrollInfo.nPos;
  end
  else
  begin
    //NSScrollerSetScrollInfo(ns.frame.size.width, sc.horizontalScroller, ScrollInfo);
    vr.origin.x:=ScrollInfo.nPos;
  end;
  ns.scrollRectToVisible(vr);
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

function TCocoaApplication.isRunning: LCLObjCBoolean;
begin
  Result:=isrun;
end;

procedure TCocoaApplication.run;
begin
  isrun:=true;
  aloop();
end;

procedure ForwardMouseMove(app: NSApplication; theEvent: NSEvent);
var
  w   : NSWindow;
  kw  : NSWindow;
  ev  : NSEvent;
  p   : NSPoint;
  wfr : NSRect;
begin
  kw := app.keyWindow;

  // mouse move was consumed by the focused window
  if Assigned(kw) and NSPointInRect( theEvent.mouseLocation, kw.frame) then
    Exit;

  for w in app.windows do
  begin
    if w = kw then Continue;
    if not w.isVisible then Continue;
    // todo: check for enabled windows? modal windows?

    wfr := w.frame;
    if not NSPointInRect( theEvent.mouseLocation, wfr) then Continue;

    p := theEvent.mouseLocation;
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
end;

procedure TCocoaApplication.sendEvent(theEvent: NSEvent);
var
  cb : ICommonCallback;
  wnd: TCocoaWindow;
  allowcocoa : Boolean;
begin
  if (theEvent.type_ = NSKeyDown) or (theEvent.type_ = NSKeyUp) or
     (theEvent.type_ = NSFlagsChanged) then begin
    cb := self.keyWindow.firstResponder.lclGetCallback;
    if Assigned(cb) then
    begin
      try
        if self.keyWindow.isKindOfClass_(TCocoaWindow) then begin
          wnd := TCocoaWindow(self.keyWindow);
          wnd._keyEvCallback := cb;
          wnd._calledKeyEvAfter := False;
        end
        else
          wnd := nil;
        cb.KeyEvBefore(theEvent, allowcocoa);
        if allowcocoa then
          inherited sendEvent(theEvent);
        if (not Assigned(wnd)) or (not wnd._calledKeyEvAfter) then
          cb.KeyEvAfter;
      finally
        if Assigned(wnd) then
          wnd._keyEvCallback := nil;
      end;
      Exit;
    end;
  end;

  inherited sendEvent(theEvent);

  if (theEvent.type_ = NSMouseMoved) then ForwardMouseMove(Self, theEvent);
end;

function isMouseMoveEvent(tp: NSEventType): Boolean; inline;
begin
  Result := (tp = NSMouseMoved)
    or (tp = NSLeftMouseDragged)
    or (tp = NSRightMouseDragged)
    or (tp = NSOtherMouseDragged);
end;

function TCocoaApplication.nextEventMatchingMask_untilDate_inMode_dequeue(
  mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: LCLObjCBoolean
  ): NSEvent;
var
  cb : ICommonCallback;
begin
  {$ifdef BOOLFIX}
  Result:=inherited nextEventMatchingMask_untilDate_inMode_dequeue_(mask,
    expiration, mode, Ord(deqFlag));
  {$else}
  Result:=inherited nextEventMatchingMask_untilDate_inMode_dequeue(mask,
    expiration, mode, deqFlag);
  {$endif}
  if Assigned(Result)
    and ((mode = NSEventTrackingRunLoopMode) or mode.isEqualToString(NSEventTrackingRunLoopMode))
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
      if Assigned(cb) then cb.MouseMove(Result);
    end;
  end;

end;

function TCocoaApplication.runModalForWindow(theWindow: NSWindow): NSInteger;
begin
  ApplicationWillShowModal;

  Result:=inherited runModalForWindow(theWindow);
end;


procedure InternalFinal;
begin
  if Assigned(MainPool) then
  begin
    MainPool.release;
    MainPool := nil;
  end;
end;

// the implementation of the utility methods
{$I cocoaobject.inc}
// the implementation of the winapi compatibility methods
{$I cocoawinapi.inc}
// the implementation of the extra LCL interface methods
{$I cocoalclintf.inc}

procedure InternalInit;
begin
  // MacOSX 10.6 reports a lot of warnings during initialization process
  // adding the autorelease pool for the whole Cocoa widgetset
  MainPool := NSAutoreleasePool.alloc.init;
end;

procedure TCocoaWidgetSet.DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
var
  i: Integer;
  lCurItem: TMenuItem;
  lMenuObj: NSObject;
  lNSMenu: NSMenu absolute AMenu;
begin
  if Assigned(PrevMenu) then PrevMenu.release;
  PrevMenu := NSApplication(NSApp).mainMenu;
  PrevMenu.retain;

  PrevLCLMenu := CurLCLMenu;
  NSApp.setMainMenu(lNSMenu);
  CurLCLMenu := ALCLMenu;

  if (ALCLMenu = nil) or not ALCLMenu.HandleAllocated then Exit;

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
      Break;
    end;
  end;
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
  lvl : NSInteger;
begin
  Result := false;
  if not Assigned(awin) then Exit;

  lvl := awin.level;

  sess := NSApplication(NSApp).beginModalSessionForWindow(awin);
  if not Assigned(sess) then Exit;

  // beginModalSession "configures" the modality and potentially is changing window level
  awin.setLevel(lvl);

  if not Assigned(Modals) then Modals := TList.Create;

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
end;

function TCocoaWidgetSet.isModalSession: Boolean;
begin
  Result := Assigned(Modals) and (Modals.Count > 0);
end;

initialization
//  {$I Cocoaimages.lrs}
  InternalInit;

finalization
  InternalFinal;

end.
