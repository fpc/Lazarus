{ 
 /*************************************************************************** 
                         androidint.pp  -  Android Interface Object
                             ------------------- 
 ***************************************************************************/ 
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
 
unit androidint;
 
{$mode objfpc}{$H+}

interface

{.$I androiddefines.inc}

uses
  // Android Bindings
  android_all, androidapp, androidpipescomm,
  // FPC
  Classes, SysUtils, Math, Types, maps,
  // LCL
  InterfaceBase, LCLProc, LCLType, LMessages, LCLMessageGlue, LCLStrConsts,
  GraphType, Forms,
  // LCL-Android
  androidprivate
  ;
{  Controls, ExtCtrls,
  Dialogs, StdCtrls, Comctrls, LCLIntf, GraphUtil, Themes,
  Arrow, CheckLst;}

type
  { TAndroidWidgetSet }

  TAndroidWidgetSet = Class(TWidgetSet)
  private
    // For DebugOut
    FPartialDebugLine: string;

{    App: QApplicationH;

    // cache for WindowFromPoint
    FLastWFPMousePos: TPoint;
    FLastWFPResult: HWND;

    FEatNextDeactivate: Boolean;
    FOverrideCursor: TObject;
    SavedDCList: TFPList;
    CriticalSection: TRTLCriticalSection;
    SavedHandlesList: TMap;
    FSocketEventMap: TMap;
    StayOnTopList: TMap;
    // global hooks
    FAppEvenFilterHook: QObject_hookH;
    FAppFocusChangedHook: QApplication_hookH;

    // default application font name (FamilyName for "default" font)
    FDefaultAppFontName: WideString;

    FDockImage: QRubberBandH;
    FDragImageList: QWidgetH;
    FDragHotSpot: TPoint;
    FDragImageLock: Boolean;
    FCachedColors: array[0..MAX_SYS_COLORS] of PLongWord;
    FSysColorBrushes: array[0..MAX_SYS_COLORS] of HBrush;
    procedure ClearCachedColors;
    procedure SetOverrideCursor(const AValue: TObject);
    procedure QtRemoveStayOnTop(const ASystemTopAlso: Boolean = False);
    procedure QtRestoreStayOnTop(const ASystemTopAlso: Boolean = False);
    procedure SetDefaultAppFontName;
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
    FStockDefaultDC: HDC;
    
    function CreateThemeServices: TThemeServices; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
    procedure FocusChanged(old: QWidgetH; now: QWidgetH); cdecl;
    procedure OnWakeMainThread(Sender: TObject);}
  public
    function LCLPlatform: TLCLPlatform; override;
    function  GetLCLCapability(ACapability: TLCLCapability): PtrUInt; override;
    // Application
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
    function AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; override;
    function AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure DebugLogLn(const s: string); override;
    procedure DebugLog(const s: string); override;

    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean); override;
    procedure SetDesigning(AComponent: TComponent); override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle; override;
    function DestroyTimer(TimerHandle: THandle): boolean; override;

    // device contexts
    function IsValidDC(const DC: HDC): Boolean; virtual;
    function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean; virtual;

{    // qt object handles map
    procedure AddHandle(AHandle: TObject);
    procedure RemoveHandle(AHandle: TObject);
    function IsValidHandle(AHandle: HWND): Boolean;

    // cache for WindowFromPoint to reduce very expensive calls
    // of QApplication_widgetAt() inside WindowFromPoint().
    function IsWidgetAtCache(AHandle: HWND): Boolean;
    procedure InvalidateWidgetAtCache;
    function IsValidWidgetAtCachePointer: Boolean;
    function GetWidgetAtCachePoint: TPoint;

    // drag image list
    function DragImageList_BeginDrag(AImage: QImageH; AHotSpot: TPoint): Boolean;
    procedure DragImageList_EndDrag;
    function DragImageList_DragMove(X, Y: Integer): Boolean;
    function DragImageList_SetVisible(NewVisible: Boolean): Boolean;
  public
    function CreateDefaultFont: HFONT; virtual;
    function GetDefaultAppFontName: WideString;
    function GetQtDefaultDC: HDC; virtual;
    procedure DeleteDefaultDC; virtual;
    procedure SetQtDefaultDC(Handle: HDC); virtual;
    procedure InitStockItems;
    procedure FreeStockItems;
    procedure FreeSysColorBrushes(const AInvalidateHandlesOnly: Boolean = False);

    property DragImageLock: Boolean read FDragImageLock write FDragImageLock;
    property OverrideCursor: TObject read FOverrideCursor write SetOverrideCursor;}

  public
    // Android Callbacks
    AndroidDialogResult: Integer;
    procedure HandleAlertButtonPositive(); virtual;
    procedure HandleAlertButtonNegative(); virtual;

    {$I androidwinapih.inc}
    {$I androidlclintfh.inc}
  end;


var
  AndroidWidgetSet: TAndroidWidgetSet;

implementation

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
 AndroidWSFactory;
{ AndroidCaret,
 AndroidThemes,
////////////////////////////////////////////////////
  Graphics, buttons, Menus,
  // Bindings
  qtprivate, qtwidgets, qtobjects;}


{$I androidobject.inc}
{$I androidwinapi.inc}
{$I androidlclintf.inc}

end.
