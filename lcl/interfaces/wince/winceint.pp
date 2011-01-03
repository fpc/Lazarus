{ $Id: winceint.pp 8004 2005-10-30 15:33:20Z micha $ }
{
 /***************************************************************************
                         WINCEINT.pp  -  WinCEInterface Object
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

unit WinCEInt;

{$mode objfpc}{$H+}

interface

{ At least FPC 2.2.1 is required if the architecture is ARM
  FPC 2.0 or inferior isn't checked because it can't compile for wince }
{$ifdef CPUARM}
  {$if defined(ver2_2) and (fpc_patch<1)}
//    {$fatal The Lazarus WinCE Interface in Lazarus 0.9.25+ requires at least FPC 2.2.1}
  {$endif}
{$endif}

{$IFDEF Trace}
{$ASSERTIONS ON}
{$ENDIF}

// defining the following will print all messages as they are being handled
// valuable for investigation of message trees / interrelations
{ $define MSG_DEBUG}

{
  When editing this unit list, be sure to keep Windows listed first to ensure
  successful compilation.
}
uses
  // Compatibility
  {$ifdef Win32}
    win32compat,
  {$else}
    {$ifndef ver2_2_0}aygshell,{$endif}
  {$endif}
  // Libs
  Windows,
  // RTL, LCL
  Classes, ComCtrls, Controls, Buttons, Dialogs, DynHashArray,
  ExtCtrls, Forms, GraphMath, GraphType, InterfaceBase, LCLIntf, LCLType,
  LMessages, StdCtrls, SysUtils, Graphics, Menus,
  // Widgetset
  WinCEProc, WinCEExtra, WinExt, WinCEDef, Themes;

const
  {$ifdef Win32}
  // standard windows cursors
  // they are already defined in the rtl, however the
  // const = const defines after this fail with an illegal expression
  IDC_ARROW     = System.MakeIntResource(32512);
  IDC_IBEAM     = System.MakeIntResource(32513);
  IDC_WAIT      = System.MakeIntResource(32514);
  IDC_CROSS     = System.MakeIntResource(32515);
  IDC_UPARROW   = System.MakeIntResource(32516);
  IDC_SIZE      = System.MakeIntResource(32640);
  IDC_ICON      = System.MakeIntResource(32641);
  IDC_SIZENWSE  = System.MakeIntResource(32642);
  IDC_SIZENESW  = System.MakeIntResource(32643);
  IDC_SIZEWE    = System.MakeIntResource(32644);
  IDC_SIZENS    = System.MakeIntResource(32645);
  IDC_SIZEALL   = System.MakeIntResource(32646);
  IDC_NO        = System.MakeIntResource(32648);
  IDC_HAND      = System.MakeIntResource(32649);
  IDC_APPSTARTING = System.MakeIntResource(32650);
  IDC_HELP      = System.MakeIntResource(32651);
  {$endif}

  IDC_NODROP    = IDC_NO;
  IDC_DRAG      = IDC_ARROW;
  IDC_HSPLIT    = IDC_SIZEWE;
  IDC_VSPLIT    = IDC_SIZENS;
  IDC_MULTIDRAG = IDC_ARROW;
  IDC_SQLWAIT   = IDC_WAIT;
  IDC_HANDPT    = IDC_HAND;

  LclCursorToWin32CursorMap: array[crLow..crHigh] of PWideChar = (
  // uni-direction cursors are mapped to bidirection win32 cursors
     IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZEWE, IDC_SIZEWE,
     IDC_SIZENESW, IDC_SIZENS, IDC_SIZENWSE, IDC_SIZEALL, IDC_HANDPT, IDC_HELP,
     IDC_APPSTARTING, IDC_NO, IDC_SQLWAIT, IDC_MULTIDRAG, IDC_VSPLIT,
     IDC_HSPLIT, IDC_NODROP, IDC_DRAG, IDC_WAIT, IDC_UPARROW, IDC_SIZEWE,
     IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZE, IDC_IBEAM, IDC_CROSS,
     IDC_ARROW, IDC_ARROW, IDC_ARROW);
     
  WM_LCL_SOCK_ASYNC = WM_USER;

  //flag used to avoid propagating LM_CHANGE for TCustomCheckBox
  SKIP_LMCHANGE = 1000;

type

  { Policy for using the "OK" close button in the title instead of
    the default "X" minimize button }
  TWinCETitlePolicy = (tpAlwaysUseOKButton, tpOKButtonOnlyOnDialogs, tpControlWithBorderIcons);

  PPPipeEventInfo = ^PPipeEventInfo;
  PPipeEventInfo = ^TPipeEventInfo;
  TPipeEventInfo = record
    Handle: THandle;
    UserData: PtrInt;
    OnEvent: TPipeEvent;
    Prev: PPipeEventInfo;
    Next: PPipeEventInfo;
  end;

  TWaitHandler = record
    ListIndex: pdword;
    UserData: PtrInt;
    OnEvent: TWaitHandleEvent;
  end;

  TSocketEvent = function(ASocket: THandle; Flags: dword): Integer of object;

  { WinCE interface-object class }

  { TWinCEWidgetSet }

  TWinCEWidgetSet = class(TWidgetSet)
  private
    // In win32 it was: The parent of all windows, represents the button of the taskbar
    // In wince it is just an invisible window, but retains the following functions:
    // * This window is also the owner of the clipboard.
    // * Assoc. windowproc also acts as handler for popup menus
    // * It is indispensable for popupmenus and thread synchronization
    FAppHandle: HWND;

    FMetrics: TNonClientMetrics;
    FMetricsFailed: Boolean;

    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;

    FStatusFont: HFONT;
    FMessageFont: HFONT;

    FWaitHandleCount: dword;
    FWaitHandles: array of HANDLE;
    FWaitHandlers: array of TWaitHandler;
    FWaitPipeHandlers: PPipeEventInfo;
    
    FOnAsyncSocketMsg: TSocketEvent;

    { event handler helper functions }
    procedure HandleProcessEvent(AData: PtrInt; AFlags: dword);
    procedure CheckPipeEvents;


    function WinRegister: Boolean;

  public
    { Creates a callback of Lazarus message Msg for Sender }
    procedure SetCallback(Msg: LongInt; Sender: TObject); virtual;
    { Removes all callbacks for Sender }
    procedure RemoveCallbacks(Sender: TObject); virtual;

    { Constructor of the class }
    constructor Create; override;
    { Destructor of the class }
    destructor Destroy; override;

    function LCLPlatform: TLCLPlatform; override;
    function GetLCLCapability(ACapability: TLCLCapability): PtrUInt; override;

    { Initialize the API }
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    procedure AppTerminate; override;
    procedure AppSetTitle(const ATitle: string); override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;

    procedure ShowHide(Sender: TObject);

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;

    // thread synchronize support
    procedure HandleWakeMainThread(Sender: TObject);


    {$I wincewinapih.inc}
    {$I wincelclintfh.inc}

    property AppHandle: HWND read FAppHandle;
    property MessageFont: HFONT read FMessageFont;
    property OnAsyncSocketMsg: TSocketEvent read FOnAsyncSocketMsg write FOnAsyncSocketMsg;

  public
    { Variables to be set by the user }

    WinCETitlePolicy: TWinCETitlePolicy;
  end;

{$I wincelistslh.inc}


const
  BOOL_RESULT: Array[Boolean] of String = ('False', 'True');
  ClsName: array[0..6] of WideChar = ('W','i','n','d','o','w',#0);
  EditClsName: array[0..4] of WideChar = ('E','D','I','T',#0);
  ButtonClsName: array[0..6] of WideChar = ('B','U','T','T','O','N',#0);
  LabelClsName: array[0..6] of WideChar = ('S','T','A','T','I','C',#0);
  ComboboxClsName: array[0..8] of WideChar = ('C','O','M','B','O','B','O','X',#0);
  TabControlClsName: array[0..15] of WideChar = ('S','y','s','T','a','b','C','o','n','t','r','o','l','3','2',#0);
  ScrollBarClsName: array[0..9] of WideChar = ('S','C','R','O','L','L','B','A','R',#0);
  ListBoxClsName: array[0..7] of WideChar = ('L','I','S','T','B','O','X',#0);


{ export for widgetset implementation }

function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; {$ifdef win32}stdcall{$else}cdecl{$endif};
function ComboBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; cdecl;
function CallDefaultWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult;

var
  WinCEWidgetSet: TWinCEWidgetSet;

Implementation

uses
  WinCEWSFactory,
  WinCEWSButtons,
  WinCEWSExtCtrls,
  WinCEWSMenus,
  WinCEWSStdCtrls,
  WinCEWSSpin,
////////////////////////////////////////////////////
  Spin, CheckLst, LCLProc, LCLMessageGlue;

type
  TMouseDownFocusStatus = (mfNone, mfFocusSense, mfFocusChanged);

  PProcessEvent = ^TProcessEvent;
  TProcessEvent = record
    Handle: THandle;
    Handler: PEventHandler;
    UserData: PtrInt;
    OnEvent: TChildExitEvent;
  end;

var
  MouseDownTime: dword;
  MouseDownPos: TPoint;
  MouseDownWindow: HWND = 0;
  MouseDownFocusWindow: HWND;
  MouseDownFocusStatus: TMouseDownFocusStatus = mfNone;
  ComboBoxHandleSizeWindow: HWND = 0;//just do not know the use yet
  IgnoreNextCharWindow: HWND = 0;  // ignore next WM_(SYS)CHAR message
  OnClipBoardRequest: TClipboardRequestEvent = nil;

{$I wincelistsl.inc}
{$I wincecallback.inc}
{$I winceobject.inc}
{$I wincewinapi.inc}
{$I wincelclintf.inc}

initialization

  Assert(False, 'Trace:WinCEint.pp - Initialization');

finalization
  Assert(False, 'Trace:WinCEint.pp - Finalization');

end.
