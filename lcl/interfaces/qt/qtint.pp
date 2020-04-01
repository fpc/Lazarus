{ $Id$ }
{ 
 /*************************************************************************** 
                         QTINT.pp  -  QTInterface Object
                             ------------------- 
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
 ***************************************************************************/ 
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  Classes, SysUtils, Math, Types,
  // LazUtils
  LazUTF8, Maps, LazStringUtils,
  // LCL
  LCLPlatformDef, InterfaceBase, LCLProc, LCLType, LCLIntf,
  LMessages, LCLMessageGlue, LCLStrConsts,
  Controls, ExtCtrls, Forms, StdCtrls, GraphType, GraphUtil, Themes,
  // WS
  qtproc;

type
  TSysColorBrushes = array[0..MAX_SYS_COLORS] of HBrush;

  { TQtWidgetSet }

  TQtWidgetSet = Class(TWidgetSet)
  private
    App: QApplicationH;
    {$IFDEF QtUseNativeEventLoop}
    FMainTimerID: integer;
    {$ENDIF}
    {$IFDEF QtUseAccurateFrame}
    FWSFrameMargins: TRect;
    {$ENDIF}
    FIsLibraryInstance: Boolean;

    // cache for WindowFromPoint
    FLastWFPMousePos: TPoint;
    FLastWFPResult: HWND;

    // global actions
    FGlobalActions: TFPList;
    FAppActive: Boolean;
    FOverrideCursor: TObject;
    SavedDCList: TFPList;
    CriticalSection: TRTLCriticalSection;
    SavedHandlesList: TMap;
    FSocketEventMap: TMap;
    StayOnTopList: TMap;
    SysTrayIconsList: TFPList;
    // global hooks
    FAppEvenFilterHook: QObject_hookH;
    FAppFocusChangedHook: QApplication_hookH;
    FAppSessionQuit: QApplication_hookH;
    FAppSaveSessionRequest: QApplication_hookH;

    // default application font name (FamilyName for "default" font)
    FDefaultAppFontName: WideString;

    FDockImage: QRubberBandH;
    FDragImageList: QWidgetH;
    FDragHotSpot: TPoint;
    FDragImageLock: Boolean;
    FCachedColors: array[0..MAX_SYS_COLORS] of PLongWord;
    FSysColorBrushes: TSysColorBrushes;

    {$IFDEF HASX11}
    SavedHintHandlesList: TFPList;
    FWindowManagerName: String; // Track various incompatibilities between WM. Initialized at WS start.
    {$ENDIF}


    // qt style does not have pixel metric for themed menubar (menu) height
    // so we must calculate it somehow.
    FCachedMenuBarHeight: Integer;
    {$IFDEF QtUseAccurateFrame}
    function GetFrameMargins: TRect;
    {$ENDIF}
    function GetMenuHeight: Integer;

    procedure ClearCachedColors;
    function GetStyleName: String;
    procedure SetOverrideCursor(const AValue: TObject);
    procedure QtRemoveStayOnTop(const ASystemTopAlso: Boolean = False);
    procedure QtRestoreStayOnTop(const ASystemTopAlso: Boolean = False);
    procedure SetDefaultAppFontName;
  protected
    FPenForSetPixel: QPenH;
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
    procedure FocusChanged(aold: QWidgetH; anew: QWidgetH); cdecl;
    procedure OnWakeMainThread(Sender: TObject);
    procedure SlotCommitDataRequest(sessionManager: QSessionManagerH); cdecl;
    procedure SlotSaveDataRequest(sessionManager: QSessionManagerH); cdecl;
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

    // qt systray icons map
    procedure RegisterSysTrayIcon(AHandle: TObject);
    procedure UnRegisterSysTrayIcon(AHandle: TObject);
    function IsValidSysTrayIcon(AHandle: HWND): Boolean;

    {$IFDEF HASX11}
    // qt hints handles map (needed on X11 only)
    procedure AddHintHandle(AHandle: TObject);
    procedure RemoveHintHandle(AHandle: TObject);
    function IsValidHintHandle(AHandle: TObject): Boolean;
    procedure HideAllHints;
    procedure RestoreAllHints;
    {$ENDIF}

    // application global actions (mainform mainmenu mnemonics Alt+XX)
    procedure ClearGlobalActions;
    procedure AddGlobalAction(AnAction: QActionH);
    function ShortcutInGlobalActions(const AMnemonicText: WideString;
      out AGlobalActionIndex: Integer): Boolean;
    procedure TriggerGlobalAction(const ActionIndex: Integer);

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
    {$IFDEF HASX11}
    FLastMinimizeEvent: DWord; // track mainform minimize events -> TQtMainWindow.EventFilter
    FMinimizedByPager: Boolean; // track if app is minimized via desktop pager or by us.
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    function GetWinKeyState(AKeyState: LongInt): SHORT;
    {$ENDIF}
    function CreateDefaultFont: HFONT; virtual;
    function GetDefaultAppFontName: WideString;
    function GetQtDefaultDC: HDC; virtual;
    procedure DeleteDefaultDC; virtual;
    procedure SetQtDefaultDC(Handle: HDC); virtual;
    procedure InitStockItems;
    procedure FreeStockItems;
    procedure FreeSysColorBrushes(const AInvalidateHandlesOnly: Boolean = False);

    property AppActive: Boolean read FAppActive;
    property DragImageLock: Boolean read FDragImageLock write FDragImageLock;

    {$IFDEF QtUseAccurateFrame}
    property WSFrameMargins: TRect read GetFrameMargins write FWSFrameMargins;
    {$ENDIF}

    {do not create new QApplication object if we are called from library }
    property IsLibraryInstance: Boolean read FIsLibraryInstance;

    property OverrideCursor: TObject read FOverrideCursor write SetOverrideCursor;
    property StyleName: String read GetStyleName;
    {$IFDEF HASX11}
    property WindowManagerName: String read FWindowManagerName;
    {$ENDIF}
    {$I qtwinapih.inc}
    {$I qtlclintfh.inc}

    {$IFDEF EnableQTAppH}
    property AppH: QApplicationH read App;
    {$ENDIF}
  end;


type
  TEventProc = record
    Name : String[25];
    CallBack : procedure(Data : TObject);
    Data : Pointer;
  end;

  CallbackProcedure = procedure (Data : Pointer);

  pTRect = ^TRect;

  function HwndFromWidgetH(const WidgetH: QWidgetH): HWND;
  function DTFlagsToQtFlags(const Flags: Cardinal): Integer;
  function GetPixelMetric(AMetric: QStylePixelMetric; AOption: QStyleOptionH;
    AWidget: QWidgetH): Integer;
  function GetQtVersion: String;
  function QtVersionCheck(const AMajor, AMinor, AMicro: Integer): Boolean;
  {$IFDEF HASX11}
  function IsWayland: Boolean; {this is not X11 but wayland !}
  function IsCurrentDesktop(AWidget: QWidgetH): Boolean;
  function X11Raise(AHandle: HWND): boolean;
  function X11GetActiveWindow: QWidgetH;
  function GetWindowManager: String;
  function SetTransientForHint(Widget: QWidgetH; ATransientWin: QWidgetH): boolean;
  procedure SetSkipX11Taskbar(Widget: QWidgetH; const ASkipTaskBar: Boolean);
  {check if XWindow have _NET_WM_STATE_ABOVE and our form doesn''t know anything about it}
  function GetAlwaysOnTopX11(Widget: QWidgetH): boolean;
  {check if we are running under kde3 installation}
  function IsOldKDEInstallation: Boolean;
  {check KDE session version. Possible results are > 2, -1 means not running under KDE}
  function GetKdeSessionVersion: integer;
  {force mapping}
  procedure MapX11Window(AWinID: LongWord);
  {$IFDEF QtUseAccurateFrame}
  function GetX11WindowRealized(AWinID: LongWord): boolean;
  function GetX11WindowAttributes(AWinID: LongWord; out ALeft, ATop, AWidth, AHeight, ABorder: integer): boolean;
  function GetX11SupportedAtoms(AWinID: LongWord; AList: TStrings): boolean;
  {Ask for _NET_FRAME_EXTENTS,_KDE_NET_WM_SHADOW,_GTK_NET_FRAME_EXTENTS}
  function GetX11RectForAtom(AWinID: LongWord; const AAtomName: string; out ARect: TRect): boolean;
  function GetX11WindowPos(AWinID: LongWord; out ALeft, ATop: integer): boolean;
  function SetX11WindowPos(AWinID: LongWord; const ALeft, ATop: integer): boolean;
  function GetX11WindowGeometry(AWinID: LongWord; out ARect: TRect): boolean;
  {check if wm supports request for frame extents}
  function AskX11_NET_REQUEST_FRAME_EXTENTS(AWinID: LongWord; out AMargins: TRect): boolean;
  {$ENDIF}
  {$ENDIF}

const
   QtVersionMajor: Integer = 0;
   QtVersionMinor: Integer = 0;
   QtVersionMicro: Integer = 0;
   QtMinimumWidgetSize = 0;
   QtMaximumWidgetSize = 16777215;

   TargetEntrys = 3;
   QEventLCLMessage = QEventUser;
   // QEventType(Ord(QEventUser) + $1000) is reserved by
   // LCLQt_Destroy (qtobjects) to reduce includes !
   LCLQt_CheckSynchronize = QEventType(Ord(QEventUser) + $1001);
   LCLQt_PopupMenuClose = QEventType(Ord(QEventUser) + $1002);
   LCLQt_PopupMenuTriggered = QEventType(Ord(QEventUser) + $1003);
   // QEventType(Ord(QEventUser) + $1004 is reserved by
   // LCLQt_ClipboardPrimarySelection (qtobjects) to reduce includes !
   LCLQt_ApplicationActivate = QEventType(Ord(QEventUser) + $1005);
   // deactivate sent from qt
   LCLQt_ApplicationDeactivate = QEventType(Ord(QEventUser) + $1006);
   // deactivate sent from LCLQt_ApplicationDeactivate to check it twice
   // instead of using timer.
   LCLQt_ApplicationDeactivate_Check = QEventType(Ord(QEventUser) + $1007);

   // needed by itemviews (TQtListWidget, TQtTreeWidget)
   LCLQt_ItemViewAfterMouseRelease = QEventType(Ord(QEventUser) + $1008);
   // used by TQtTabWidget
   LCLQt_DelayLayoutRequest = QEventType(Ord(QEventUser) + $1009);
   // delayed resize event if wincontrol is computing bounds
   LCLQt_DelayResizeEvent = QEventType(Ord(QEventUser) + $1010);
   // systemtrayicon event, used to find and register private QWidget of QSystemTrayIcon
   LCLQt_RegisterSystemTrayIcon = QEventType(Ord(QEventUser) + $1011);


   QtTextSingleLine            = $0100;
   QtTextDontClip              = $0200;
   QtTextExpandTabs            = $0400;
   QtTextShowMnemonic          = $0800;
   QtTextWordWrap              = $1000;
   QtTextWrapAnywhere          = $2000;
   QtTextHideMnemonic          = $8000;
   QtTextDontPrint             = $4000;
   QtTextIncludeTrailingSpaces =	$08000000;
   QtTextJustificationForced   = $10000;


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
 {$IFDEF HASX11}
 XAtom, X, XLib, XKB, xkblib,
 {$ENDIF}
 QtCaret,
 QtThemes,
////////////////////////////////////////////////////
  Graphics, buttons,
  // Bindings
  QtWSFactory, qtwidgets, qtobjects, qtsystemtrayicon;

function DTFlagsToQtFlags(const Flags: Cardinal): Integer;
begin
  Result := 0;
  // horizontal alignment
  if Flags and DT_CENTER <> 0 then
    Result := Result or  QtAlignHCenter
  else
  if Flags and DT_RIGHT <> 0 then
    Result := Result or QtAlignRight
  else
    Result := Result or QtAlignLeft;
  // vertical alignment
  if Flags and DT_VCENTER <> 0 then
    Result := Result or QtAlignVCenter
  else
  if Flags and DT_BOTTOM <> 0 then
    Result := Result or QtAlignBottom
  else
    Result := Result or QtAlignTop;

  // mutually exclusive wordbreak and singleline
  if Flags and DT_WORDBREAK <> 0 then
    Result := Result or QtTextWordWrap
  else
  if Flags and DT_SINGLELINE <> 0 then
    Result := Result or QtTextSingleLine;

  if Flags and DT_NOPREFIX = 0 then
    Result := Result or QtTextShowMnemonic;

  if Flags and DT_NOCLIP <> 0 then
    Result := Result or QtTextDontClip;

  if Flags and DT_EXPANDTABS <> 0 then
    Result := Result or QtTextExpandTabs;
end;

function GetPixelMetric(AMetric: QStylePixelMetric; AOption: QStyleOptionH;
  AWidget: QWidgetH): Integer;
begin
  Result := QStyle_pixelMetric(QApplication_style(),
    AMetric, AOption, AWidget);
end;

function QtObjectFromWidgetH(const WidgetH: QWidgetH): TQtWidget;
var
  V: QVariantH;
  Ok: Boolean;
  Obj: TObject;
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
      Obj := TObject(QVariant_toUint(V, @Ok));
      {$ENDIF}
      {$IFDEF CPU64}
      Obj := TObject(QVariant_toULongLong(V, @Ok));
      {$ENDIF}
      if OK and QtWidgetset.IsValidHandle(HWND(Obj)) then
      begin
        if not (Obj is TQtWidget) then
          raise Exception.Create('QtObjectFromWidgetH: QObject_property returned '
                    + 'a variant which is not TQtWidget ' + dbgHex(PtrUInt(Obj)));
        QtWg := TQtWidget(Obj);
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

{------------------------------------------------------------------------------
  Method: GetQtVersion
  Params:  none
  Returns: String

  Returns current Qt lib version used by application.
 ------------------------------------------------------------------------------}
function GetQtVersion: String;
begin
  Result := QtVersion;
end;

procedure QtVersionInt(out AMajor, AMinor, AMicro: integer);
var
  S: String;
  i: Integer;
  sLen: integer;
begin
  AMajor := 0;
  AMinor := 0;
  AMicro := 0;
  S := GetQtVersion;
  sLen := length(S);

  // 5 is usual length of qt version eg. 4.6.3
  if sLen < 5 then
    exit;
  if sLen = 5 then
  begin
    TryStrToInt(S[1], AMajor);
    TryStrToInt(S[3], AMinor);
    TryStrToInt(S[5], AMicro);
  end else
  begin
    i := Pos('.', S);
    // major
    if i > 0 then
    begin
      TryStrToInt(Copy(S, 1, i -1), AMajor);
      Delete(S, 1, i - 1);
    end;
    // minor
    i := Pos('.', S);
    if i > 0 then
    begin
      TryStrToInt(Copy(S, 1, i -1), AMinor);
      Delete(S, 1, i - 1);
    end;
    // micro
    i := Pos('.', S);
    if i > 0 then
      TryStrToInt(Copy(S, 1, i -1), AMinor);
  end;
end;

{------------------------------------------------------------------------------
  Method: QtVersionCheck
  Params:  AMajor, AMinor, AMicro: Integer
  Returns: Boolean

  Function checks if qt lib version satisfies our function params values.
  Returns TRUE if successfull.
  It is possible to check Major and/or Minor version only (or any of those
  3 params) by setting it's param to -1.
  eg. QtVersionCheck(4, 5, -1) checks only major and minor version and will
  not process micro version check.
  NOTE: It checks qt lib version used by application.
 ------------------------------------------------------------------------------}
function QtVersionCheck(const AMajor, AMinor, AMicro: Integer): Boolean;
begin
  Result := False;
  if AMajor > 0 then
    Result := AMajor = QtVersionMajor;
  if (AMajor > 0) and not Result then
    exit;
  if AMinor >= 0 then
    Result := AMinor = QtVersionMinor;
  if (AMinor >= 0) and not Result then
    exit;
  if AMicro >= 0 then
    Result := AMicro = QtVersionMicro;
end;

{$IFDEF HASX11}
{$I qtx11.inc}
{$ENDIF}
{$I qtobject.inc}
{$I qtwinapi.inc}
{$I qtlclintf.inc}

end.
