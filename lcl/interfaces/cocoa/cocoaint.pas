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
  CocoaWSClipboard,
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
    function isRunning: Boolean; override;
    procedure run; override;
    procedure sendEvent(theEvent: NSEvent); override;
    function nextEventMatchingMask_untilDate_inMode_dequeue(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: Boolean): NSEvent; override;
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
    function GetAppHandle: THandle; override;
    function CreateThemeServices: TThemeServices; override;

    procedure SendCheckSynchronizeMessage;
    procedure OnWakeMainThread(Sender: TObject);
  public
    // modal session
    CurModalForm: TCustomForm;

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
    function IsControlDisabledDueToModal(AControl: NSView): Boolean;

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

function CocoaScrollBarSetScrollInfo(bar: TCocoaScrollBar; const ScrollInfo: TScrollInfo): Integer;
function CocoaScrollBarGetScrollInfo(bar: TCocoaScrollBar; var ScrollInfo: TScrollInfo): Boolean;
procedure NSScrollerGetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; Var ScrollInfo: TScrollInfo);
procedure NSScrollViewGetScrollInfo(sc: NSScrollView; BarFlag: Integer; Var ScrollInfo: TScrollInfo);
procedure NSScrollerSetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; const ScrollInfo: TScrollInfo);
procedure NSScrollViewSetScrollInfo(sc: NSScrollView; BarFlag: Integer; const ScrollInfo: TScrollInfo);
function HandleToNSObject(AHWnd: HWND): id;


function CocoaPromptUser(const DialogCaption, DialogMessage: String;
    DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
    EscapeResult: Longint;
    sheetOfWindow: NSWindow = nil): Longint;

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
  pos : CGFloat;
begin
  if not Assigned(bar) then
  begin
    Result := 0;
    Exit;
  end;

  if ScrollInfo.fMask and SIF_PAGE>0 then
  begin
    pg:=ScrollInfo.nPage;
    if pg=0 then pg:=1; // zero page is not allowed?
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
  bar.setEnabled(dl<>0);

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

procedure NSScrollViewSetScrollInfo(sc: NSScrollView; BarFlag: Integer; const ScrollInfo: TScrollInfo);
var
  ns : NSView;
  vr : NSRect;
  p  : NSPoint;
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
  p:=sc.documentVisibleRect.origin;
end;

function HandleToNSObject(AHWnd: HWND): id;
begin
  if (AHwnd=0) or not NSObject(AHWnd).lclisHandle then Result:=nil
  else Result:=NSObject(AHwnd);
end;

{ TCocoaApplication }

function TCocoaApplication.isRunning: Boolean;
begin
  Result:=isrun;
end;

procedure TCocoaApplication.run;
begin
  isrun:=true;
  aloop();
end;

procedure TCocoaApplication.sendEvent(theEvent: NSEvent);
begin
  // https://stackoverflow.com/questions/4001565/missing-keyup-events-on-meaningful-key-combinations-e-g-select-till-beginning
  if (theEvent.type_ = NSKeyUp) and
     ((theEvent.modifierFlags and NSCommandKeyMask) = NSCommandKeyMask)
  then
    self.keyWindow.sendEvent(theEvent);
  inherited sendEvent(theEvent);
end;

function TCocoaApplication.nextEventMatchingMask_untilDate_inMode_dequeue(
  mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: Boolean
  ): NSEvent;
var
  cb : ICommonCallback;
begin
  Result:=inherited nextEventMatchingMask_untilDate_inMode_dequeue(mask,
    expiration, mode, deqFlag);
  if Assigned(Result)
    and ((mode = NSEventTrackingRunLoopMode) or mode.isEqualToString(NSEventTrackingRunLoopMode))
    and Assigned(TrackedControl)
  then begin
    cb := TrackedControl.lclGetCallback;
    if Assigned(cb) then cb.MouseMove(Result);
  end;
end;

// the implementation of the utility methods
{$I cocoaobject.inc}
// the implementation of the winapi compatibility methods
{$I cocoawinapi.inc}
// the implementation of the extra LCL interface methods
{$I cocoalclintf.inc}

initialization
//  {$I Cocoaimages.lrs}

  InternalInit;

finalization
  InternalFinal;

end.
