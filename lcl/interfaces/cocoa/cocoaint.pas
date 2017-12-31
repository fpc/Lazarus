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
  // fcl-image
  fpreadpng, fpwritepng, fpimage, fpreadbmp, fpwritebmp,
  // carbon bindings
  MacOSAll,
  // interfacebase
  LCLPlatformDef, InterfaceBase, GraphType,
  // private
  CocoaAll, CocoaPrivate, CocoaUtils, CocoaGDIObjects,
  cocoa_extra, CocoaWSMenus, CocoaWSForms,
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

  TCocoaClipboardDataType = (ccdtText,
    ccdtCocoaStandard, // Formats supported natively by Mac OS X
    ccdtBitmap,     // BMPs need conversion to PNG to work with other Mac OS X apps
    ccdtNonStandard { Formats that will only work in LCL apps } );

  TCocoaClipboardData = class(TObject) // TClipboardFormat is a reference to a TClipboardData
  public
    MimeType: string;
    CocoaFormat: NSString;  // utilized for ccdtCocoaStandard and ccdtNonStandard
    DataType: TCocoaClipboardDataType;
    constructor Create(AMimeType: string; ACocoaFormat: NSString; ADataType: TCocoaClipboardDataType);
    destructor Destroy; override;
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

    // Clipboard
    PrimarySelection: NSPasteboard;
    SecondarySelection: NSPasteboard;
    ClipboardFormats: TFPObjectList; // of TCocoaClipboardData

    procedure InitClipboard();
    procedure FreeClipboard();
    function GetClipboardDataForFormat(AFormat: TClipboardFormat): TCocoaClipboardData;

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

procedure NSScrollerGetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; Var ScrollInfo: TScrollInfo);
procedure NSScrollViewGetScrollInfo(sc: NSScrollView; BarFlag: Integer; Var ScrollInfo: TScrollInfo);
procedure NSScrollerSetScrollInfo(docSz, pageSz: CGFloat; rl: NSSCroller; const ScrollInfo: TScrollInfo);
procedure NSScrollViewSetScrollInfo(sc: NSScrollView; BarFlag: Integer; const ScrollInfo: TScrollInfo);
function HandleToNSObject(AHWnd: HWND): id;

implementation

// NSCursor doesn't support any wait cursor, so we need to use a non-native one
// Not supporting it at all would result in crashes in Screen.Cursor := crHourGlass;
{$R ../../cursor_hourglass.res}

uses
  CocoaCaret,
  CocoaThemes;


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
  p  : NSPoint;
begin
  ns:=sc.documentView;
  if not Assigned(ns) then Exit;

  p:=sc.documentVisibleRect.origin;
  if BarFlag = SB_Vert then
  begin
    //NSScrollerSetScrollInfo(ns.frame.size.height, sc.verticalScroller, ScrollInfo)
    p:=NSMakePoint(p.x, ScrollInfo.nPos);
  end
  else
  begin
    //NSScrollerSetScrollInfo(ns.frame.size.width, sc.horizontalScroller, ScrollInfo);
    p:=NSMakePoint(ScrollInfo.nPos, p.y);
  end;
  ns.scrollPoint(p);
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
