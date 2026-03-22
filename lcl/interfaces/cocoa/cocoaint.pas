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
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Types, Classes, SysUtils, Math, GraphMath,
  LCLPlatformDef, InterfaceBase, GraphType,
  MacOSAll, CocoaAll,
  CocoaWSService, CocoaApplication, CocoaConst, CocoaConfig, CocoaPrivate, CocoaCallback,
  CocoaUtils, Cocoa_Extra, CocoaGDIObjects, CocoaCursor, CocoaMenus, CocoaWindows,
  CocoaScrollers, CocoaWSScrollers,
  CocoaWSClipboard, CocoaTextEdits, CocoaWSModalService,
  LMessages, LCLProc, LCLIntf, LCLType,
  Controls, Forms, Themes, Menus, ExtCtrls,
  IntfGraphics, Graphics;

type

  { TCocoaWidgetSet }

  TCocoaWidgetSet = class(TWidgetSet)
  private
    FTerminating: Boolean;

    procedure AppRunMessages(onlyOne: Boolean; eventExpDate: NSDate);
    function nextEvent(const eventExpDate: NSDate): NSEvent;
    function nextEventBeforeRunLoop(const eventExpDate: NSDate): NSEvent;
    procedure SyncClipboard();
  protected
    _gdiObject: TCocoaWidgetSetGDIObject;
    _clipboard: TCocoaWSClipboard;

    function GetAppHandle: TLCLHandle; override;
    function CreateThemeServices: TThemeServices; override;
  public
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
    function AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean=False
      ): Boolean; override;
    function AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean=False
      ): Boolean; override;

    function BeginMessageProcess: TLCLHandle; override;
    procedure EndMessageProcess(context: TLCLHandle); override;

    function  GetLCLCapability(ACapability: TLCLCapability): PtrUInt; override;

    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): TLCLHandle; override;
    function DestroyTimer(TimerHandle: TLCLHandle): boolean; override;

    function PromptUser(const DialogCaption, DialogMessage: String;
      DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
      EscapeResult: Longint): Longint; override;
    function MessageBox(HWnd: HWND; lpText, lpCaption: PChar;
      uType: Cardinal): Integer; override;

    {todo:}
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean); override;
    procedure SetDesigning(AComponent: TComponent); override;

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

type

  { TCocoaTimerObject }

  TCocoaTimerObject = objcclass(NSObject)
    func: TWSTimerProc;
    timer: NSTimer;
    function initWithInterval_func(interval: integer; timerFunc: TWSTimerProc): id; message 'initWithInterval:func:';
    procedure invalidate; message 'invalidate';
    procedure timerFireMethod(atimer: NSTimer); message 'timerFireMethod:';
  end;

  { TCocoaDialogUtil }

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

{ TCocoaWidgetSet }

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppInit
  Params:  ScreenInfo

  Initialize Cocoa Widget Set
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppInit');
  {$ENDIF}
  ScreenInfo.PixelsPerInchX := CocoaConfigGlobal.basePPI;
  ScreenInfo.PixelsPerInchY := CocoaConfigGlobal.basePPI;

  { Creates the application NSApp object }
  TCocoaApplication.initApplication;
  {$ifdef COCOALOOPOVERRIDE}
  NSApp.finishLaunching;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRun
  Params:  ALoop
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  if Assigned(ALoop) then
  begin
    TCocoaApplication(NSApp).aloop:=ALoop;
    CocoaWidgetSetService.setReadyDropFiles;
    CocoaWidgetSetService.dropWaitingFiles;
    NSApp.run();
  end;
end;

function TCocoaWidgetSet.nextEvent(const eventExpDate: NSDate): NSEvent;
begin
  {$ifdef BOOLFIX}
  Result := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue_(NSAnyEventMask, eventExpDate, NSDefaultRunLoopMode, Ord(true));
  {$else}
  Result := NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, eventExpDate, NSDefaultRunLoopMode, true);
  {$endif}
end;

// before entering RunLoop (TCocoaWidgetSet.AppRun), APP may call
// TApplication.ProcessMessages (for example, to display Splash Form in IDE).
// because it has not yet entered the message processing loop, nextEvent should
// be called multiple times to ensure various asynchronous callback situations.
// otherwise, it will cause some strange problems, and it is difficult to find
// the reason, such as the display problem of Splash Form.
// see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40484
function TCocoaWidgetSet.nextEventBeforeRunLoop(const eventExpDate: NSDate): NSEvent;
var
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    Result := nextEvent(eventExpDate);
    if Assigned(Result) then
      break;
  end;
end;

procedure TCocoaWidgetSet.AppRunMessages(onlyOne: Boolean; eventExpDate: NSDate);
var
  event: NSEvent;
  pool:NSAutoReleasePool;
begin
  repeat
    pool := NSAutoreleasePool.alloc.init;
    if Assigned(TCocoaApplication(NSApp).aloop) or Assigned(eventExpDate) then
      event := nextEvent(eventExpDate)
    else
      event := nextEventBeforeRunLoop(eventExpDate);
    if event <> nil then
    begin
      NSApp.sendEvent(event);
      NSApp.updateWindows;
    end;

    SyncClipboard(); // NSPasteboard doesn't provide any notifications regarding the change
                     // Thus we have to check the clipboard on every loop

    pool.release;
  until onlyOne or (event = nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppProcessMessages

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppProcessMessages;
begin
  AppRunMessages(false, nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppWaitMessage

  Passes execution control to Cocoa
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppWaitMessage;
begin
  AppRunMessages(true, NSDate.distantFuture);
end;

function TCocoaWidgetSet.BeginMessageProcess: TLCLHandle;
begin
  Result := TLCLHandle(NSAutoreleasePool.alloc.init);
end;

procedure TCocoaWidgetSet.EndMessageProcess(context: TLCLHandle);
begin
  NSAutoreleasePool(context).release;
end;


{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Create

  Constructor for the class
 ------------------------------------------------------------------------------}
constructor TCocoaWidgetSet.Create;
begin
  CocoaWidgetSet := Self;
  inherited Create;
  FTerminating := False;

  DefaultBrush := TCocoaBrush.CreateDefault(True);
  DefaultPen := TCocoaPen.CreateDefault(True);
  DefaultFont := TCocoaFont.CreateDefault(True);
  DefaultBitmap := TCocoaBitmap.CreateDefault;
  DefaultContext := TCocoaBitmapContext.Create;
  DefaultContext.Bitmap := DefaultBitmap;

  ScreenContext := TCocoaContext.Create(DefaultContext.ctx);

  _gdiObject:= TCocoaWidgetSetGDIObject.Create;
  _clipboard := TCocoaWSClipboard.Create; // must be here otherwise clipboard calls before Application.Initialize crash
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Destroy

  Destructor for the class
 ------------------------------------------------------------------------------}
destructor TCocoaWidgetSet.Destroy;
begin
  CocoaWidgetSetService.releaseWaitingLCLObjects(0);
  inherited Destroy;

  ScreenContext.Free;
  DefaultContext.Free;
  DefaultBitmap.Free;
  DefaultFont.Free;
  DefaultPen.Free;
  DefaultBrush.Free;

  _gdiObject.Free;
  _clipboard.Free;

  // The CocoaCaret is based WidgetSet timer.
  // The GlobalCaret is freed in finalization section, which is called
  // after the destruction of the widgetset and will cause a failure.
  // Need to destroy the caret here.. or CustomTimer must be verified.
  // or CocoaCaret should not use TTimer at all (use raw cocoa timer)
  DestroyGlobalCaret;

  // NSApp.terminate(nil);   // causes app to quit immediately, which is undesirable

  // Must release the Main autorelease pool here.
  // Some objects still in the pool my depend on releasing Widgetset objects
  // (i.e. images). If autorelease pool is released After the widgetset object
  // then it finalization of WS dependent objects would fail (suppressed AVs)
  // and would cause leaks. (see #35400)
  CocoaWidgetSetService.finalAutoreleaseMainPool;

  CocoaWidgetSet := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppTerminate

  Tells Cocoa to halt the application
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppTerminate;
begin
  if FTerminating then Exit;
  // TODO: Check if there is more cleanup to do here
  // NSApp.terminate(nil);   // causes app to quit immediately, which is undesirable

  {$ifdef COCOALOOPNATIVE}
  NSApp.stop(nil);
  {$else}
  TCocoaApplicationUtil.wakeupEventLoop;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppMinimize

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppMinimize;
begin
  NSApp.hide(NSApp);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRestore

  Restores the whole minimized application from the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRestore;
begin
  NSApp.unhide(NSApp);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppBringToFront

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppBringToFront;
begin
  {$ifdef BOOLFIX}
  NSApp.activateIgnoringOtherApps_(Ord(True));
  {$else}
  NSApp.activateIgnoringOtherApps(True);
  {$endif}
end;

procedure TCocoaWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
  if Big <> 0 then
    NSApp.setApplicationIconImage(TCocoaBitmap(Big).image)
  else
    NSApp.setApplicationIconImage(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppSetTitle
  Params:  ATitle - New application title

  Changes the application title
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppSetTitle(const ATitle: string);
begin
  // There is no way to change the dock title
end;

// NSModalPanelWindowLevel has higher priority than NSFloatingWindowLevel
// on Cocoa, so nothing needs to be done
function TCocoaWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean
  ): Boolean;
begin
  Result:= true;
end;

function TCocoaWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean
  ): Boolean;
begin
  Result:= true;
end;

function TCocoaWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint,
    lcNeedMininimizeAppWithMainForm,
    {$ifndef COCOA_USE_NATIVE_MODAL}
    lcModalWindow,
    {$endif}
    lcApplicationTitle:
      Result := LCL_CAPABILITY_NO;
    {$ifdef COCOA_USE_NATIVE_MODAL}
    lcModalWindow,
    {$endif}
    lcFormIcon,
    lcAntialiasingEnabledByDefault,
    lcTransparentWindow,
    lcCanDrawHidden:
      Result := LCL_CAPABILITY_YES;
    lcAccelleratorKeys:
      Result := LCL_CAPABILITY_NO;
    lcTextHint:
      if NSAppKitVersionNumber >= NSAppKitVersionNumber10_10 then
        Result := LCL_CAPABILITY_YES
      else
        Result := LCL_CAPABILITY_NO;
  else
    Result := inherited;
  end;
end;

function TCocoaWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): TLCLHandle;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.CreateTimer');
  {$ENDIF}
  Result:=TLCLHandle(TCocoaTimerObject.alloc.initWithInterval_func(Interval, TimerFunc));
end;

function TCocoaWidgetSet.DestroyTimer(TimerHandle: TLCLHandle): boolean;
var
  obj : NSObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.DestroyTimer');
  {$ENDIF}
  obj:=NSObject(TimerHandle);
  try
    Result:= Assigned(obj) and obj.isKindOfClass_(TCocoaTimerObject);
  except
    Result:=false;
  end;
  if not Result then Exit;
  TCocoaTimerObject(obj).invalidate;
  obj.release;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.GetAppHandle
  Returns: Returns NSApp object, created via NSApplication.sharedApplication
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetAppHandle: TLCLHandle;
begin
  Result:=TLCLHandle(NSApp);
end;

function TCocoaWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result:=TCocoaThemeServices.Create;
end;

function TCocoaWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result := 0;
  if CanvasHandle <> 0 then
    Result := TCocoaContext(CanvasHandle).GetPixel(X,Y);
end;

procedure TCocoaWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin
  if CanvasHandle <> 0 then
    TCocoaContext(CanvasHandle).SetPixel(X,Y,AColor);
end;

procedure TCocoaWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  if CanvasHandle <> 0 then
    TCocoaContext(CanvasHandle).ctx.flushGraphics;
end;

procedure TCocoaWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
  if CanvasHandle <> 0 then
    TCocoaContext(CanvasHandle).SetAntialiasing(AEnabled);
end;

procedure TCocoaWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.LCLPlatform
  Returns: lpCocoa - enum value for Cocoa widgetset
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCocoa;
end;

{ TCocoaTimerObject }

function TCocoaTimerObject.initWithInterval_func(interval: integer;
  timerFunc: TWSTimerProc): id;
begin
  Self:=TCocoaTimerObject(inherited init);
  Result:=Self;
  if not Assigned(Result) then Exit;
  func:=timerFunc;
  // timer maintains a strong reference to Self until it's invalidate is called
  timer:=NSTimer.timerWithTimeInterval_target_selector_userInfo_repeats(
    interval/1000, Self, objcselector(timerFireMethod), nil, True);
  if timer = nil then Exit;
  timer.retain;
  // adding timer to all "common" loop mode.
  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSDefaultRunLoopMode);
  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSModalPanelRunLoopMode);
  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSEventTrackingRunLoopMode);
end;

procedure TCocoaTimerObject.invalidate;
begin
  if timer=nil then Exit;
  func:=nil;
  timer.invalidate;
  timer.release;
  timer:=nil;
end;

procedure TCocoaTimerObject.timerFireMethod(atimer: NSTimer);
begin
  if Assigned(func) then func;
end;

// the implementation of the winapi compatibility methods
{$I cocoawinapi.inc}
// the implementation of the extra LCL interface methods
{$I cocoalclintf.inc}

initialization
  CocoaWidgetSetService.initAutoreleaseMainPool;   // MainPool Stage 1 init
//  {$I Cocoaimages.lrs}

finalization
  CocoaWidgetSetService.finalAutoreleaseMainPool;  // MainPool Stage 2 Final

end.
