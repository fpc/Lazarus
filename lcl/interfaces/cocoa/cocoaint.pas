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
  CocoaProc, cocoa_extra, CocoaWSMenus, CocoaWSForms,
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

implementation

// NSCursor doesn't support any wait cursor, so we need to use a non-native one
// Not supporting it at all would result in crashes in Screen.Cursor := crHourGlass;
{$R ../../cursor_hourglass.res}

uses
  CocoaCaret,
  CocoaThemes;

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
