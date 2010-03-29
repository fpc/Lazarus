{ $Id$ }
{ 
 /*************************************************************************** 
                         QTINT.pp  -  QTInterface Object
                             ------------------- 
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
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
 
unit qtint;
 
{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses 
  {$IFDEF MSWINDOWS}
  Windows, // used to retrieve correct caption color values
  {$ENDIF}
  // Bindings - qt4 must come first to avoid type redefinition problems
  qt4,
  // FPC
  Classes, SysUtils, Math, Types, maps,
  // LCL
  InterfaceBase, LCLProc, LCLType, LMessages, LCLMessageGlue, LCLStrConsts,
  Controls, ExtCtrls, Forms,
  Dialogs, StdCtrls, Comctrls, LCLIntf, GraphType, GraphUtil, Themes,
  Arrow, CheckLst,
  // WS
  qtproc;

type
  { TQtWidgetSet }

  TQtWidgetSet = Class(TWidgetSet)
  private
    App: QApplicationH;
    FOverrideCursor: TObject;
    SavedDCList: TList;
    CriticalSection: TRTLCriticalSection;
    SavedHandlesList: TMap;
    // global hooks
    FAppEvenFilterHook: QObject_hookH;
    FAppFocusChangedHook: QApplication_hookH;
    
    FOldFocusWidget: QWidgetH;
    FDockImage: QRubberBandH;
    FDragImageList: QWidgetH;
    FDragHotSpot: TPoint;
    FDragImageLock: Boolean;
    FCachedColors: Array[0..MAX_SYS_COLORS + 1] of PLongWord;
    procedure ClearCachedColors;
    procedure SetOverrideCursor(const AValue: TObject);
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
    procedure OnWakeMainThread(Sender: TObject);
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
  public
    constructor Create; override;
    destructor Destroy; override;

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

    // qt object handles map
    procedure AddHandle(AHandle: TObject);
    procedure RemoveHandle(AHandle: TObject);
    function IsValidHandle(AHandle: HWND): Boolean;

    // drag image list
    function DragImageList_BeginDrag(AImage: QImageH; AHotSpot: TPoint): Boolean;
    procedure DragImageList_EndDrag;
    function DragImageList_DragMove(X, Y: Integer): Boolean;
    function DragImageList_SetVisible(NewVisible: Boolean): Boolean;
  public
    function CreateDefaultFont: HFONT; virtual;
    function GetQtDefaultDC: HDC; virtual;
    procedure DeleteDefaultDC; virtual;
    procedure SetQtDefaultDC(Handle: HDC); virtual;
    procedure InitStockItems; virtual;
    procedure FreeStockItems; virtual;

    property DragImageLock: Boolean read FDragImageLock write FDragImageLock;
    property OverrideCursor: TObject read FOverrideCursor write SetOverrideCursor;

    {$I qtwinapih.inc}
    {$I qtlclintfh.inc}
  end;


type
  TEventProc = record
    Name : String[25];
    CallBack : procedure(Data : TObject);
    Data : Pointer;
  end;

  CallbackProcedure = procedure (Data : Pointer);

  pTRect = ^TRect;

  procedure EventTrace(message : string; data : pointer);
  function HwndFromWidgetH(const WidgetH: QWidgetH): HWND;


const
   TargetEntrys = 3;
   QEventLCLMessage = QEventUser;
   LCLQt_CheckSynchronize = QEventType(Ord(QEventUser) + $1001);
   LCLQt_PopupMenuClose = QEventType(Ord(QEventUser) + $1002);
   LCLQt_PopupMenuTriggered = QEventType(Ord(QEventUser) + $1003);

var
  QtWidgetSet: TQtWidgetSet;

implementation

uses 
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
 QtWSFactory,
 QtCaret,
 QtThemes,
////////////////////////////////////////////////////
  Graphics, buttons, Menus,
  // Bindings
  qtprivate, qtwidgets, qtobjects;


const
  KEYMAP_VKUNKNOWN = $10000;
  KEYMAP_TOGGLE    = $20000;
  KEYMAP_EXTENDED  = $40000;
  
procedure EventTrace(message: string; data: pointer);
begin

end;

function QtObjectFromWidgetH(const WidgetH: QWidgetH): TQtWidget;
var
  V: QVariantH;
  Ok: Boolean;
  QtWg: TQtWidget;
begin
  Result := nil;
  
  if WidgetH = nil then
    exit;
    
  V := QVariant_Create();
  try
    QObject_property(QObjectH(WidgetH), V, 'lclwidget');
    if not QVariant_IsNull(v) and QVariant_isValid(V) then
    begin
      //Write('Got a valid variant .. ');
{$IFDEF CPU32}
      QtWg := TQtWidget(QVariant_toUint(V, @Ok));
{$ENDIF}
{$IFDEF CPU64}
      QtWg := TQtWidget(QVariant_toULongLong(V, @Ok));
{$ENDIF}
      if OK then
      begin
        //Write('Converted successfully, Control=');
        if QtWg<>nil then
        begin
          Result := QtWg;
          //WriteLn(Result.LCLObject.Name);
        end else
          ;//WriteLn('nil');
      end else
        ;//WriteLn('Can''t convert to UINT');
    end else
      ;//Writeln('GetFocus: Variant is NULL or INVALID');
  finally
    QVariant_Destroy(V);
  end;
end;

function HwndFromWidgetH(const WidgetH: QWidgetH): HWND;
begin
  Result := 0;
  if WidgetH = nil then
    exit;
  Result := HWND(QtObjectFromWidgetH(WidgetH));
end;

function GetFirstQtObjectFromWidgetH(WidgetH: QWidgetH): TQtWidget;
begin
  Result := nil;
  if WidgetH = nil then
    Exit;
  repeat
    Result := QtObjectFromWidgetH(WidgetH);
    if Result = nil then
    begin
      WidgetH := QWidget_parentWidget(WidgetH);
      if WidgetH = nil then
        break;
    end;
  until Result <> nil;
end;

{$I qtobject.inc}
{$I qtwinapi.inc}
{$I qtlclintf.inc}

end.
