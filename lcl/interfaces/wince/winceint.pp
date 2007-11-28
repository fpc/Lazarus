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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

Interface

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
Uses
  Windows, Classes, ComCtrls, Controls, Buttons, Dialogs, DynHashArray,
  ExtCtrls, Forms, GraphMath, GraphType, InterfaceBase, LCLIntf, LCLType,
  LMessages, StdCtrls, SysUtils, Graphics, Menus, WinCEProc, WinCEExtra,
  WinExt, WinCEDef;

const
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

type

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
    AppTerminated: Boolean;

    // The parent of all windows, represents the button of the taskbar
    // This window is also the owner of the clipboard.
    // Assoc. windowproc also acts as handler for popup menus
    FAppHandle: HWND;//roozbeh:in win32 it was parrent of all..a window on taskbar

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
    
    FThemesActive: boolean;

    { event handler helper functions }
    procedure HandleProcessEvent(AData: PtrInt; AFlags: dword);
    procedure CheckPipeEvents;


    Function WinRegister: Boolean;

  Public
    { Creates a callback of Lazarus message Msg for Sender }
    Procedure SetCallback(Msg: LongInt; Sender: TObject); virtual;
    { Removes all callbacks for Sender }
    Procedure RemoveCallbacks(Sender: TObject); virtual;

    { Constructor of the class }
    constructor Create; override;
    { Destructor of the class }
    destructor Destroy; override;

    function LCLPlatform: TLCLPlatform; override;

    { Initialize the API }
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    Procedure AppTerminate; Override;
    procedure AppSetTitle(const ATitle: string); override;
    //Function  InitHintFont(HintFont: TObject): Boolean; override;
    Procedure AttachMenuToWindow(AMenuObject: TComponent); override;
//    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;

    procedure ShowHide(Sender: TObject);

    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;

    // thread synchronize support
    procedure HandleWakeMainThread(Sender: TObject);


    {$I wincewinapih.inc}
    {$I wincelclintfh.inc}

    property AppHandle: HWND read FAppHandle;
    property MessageFont: HFONT read FMessageFont;
    property ThemesActive: boolean read FThemesActive;//just for not removing all those refrences
    property OnAsyncSocketMsg: TSocketEvent read FOnAsyncSocketMsg write FOnAsyncSocketMsg;
  end;

 {$I wincelistslh.inc}


const
  BOOL_RESULT: Array[Boolean] Of String = ('False', 'True');
  ClsName: array[0..6] of WideChar = ('W','i','n','d','o','w',#0);
  EditClsName: array[0..4] of WideChar = ('E','D','I','T',#0);
  ButtonClsName: array[0..6] of WideChar = ('B','U','T','T','O','N',#0);
  LabelClsName: array[0..6] of WideChar = ('S','T','A','T','I','C',#0);
  ComboboxClsName: array[0..8] of WideChar = ('C','O','M','B','O','B','O','X',#0);
  TabControlClsName: array[0..15] of WideChar = ('S','y','s','T','a','b','C','o','n','t','r','o','l','3','2',#0);
  ScrollBarClsName: array[0..9] of WideChar = ('S','C','R','O','L','L','B','A','R',#0);
  ListBoxClsName: array[0..7] of WideChar = ('L','I','S','T','B','O','X',#0);

  CP_UTF7                  = 65000;         { UTF-7 translation }
  CP_UTF8                  = 65001;         { UTF-8 translation }


{ export for widgetset implementation }

function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; cdecl;
function ComboBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; cdecl;
function CallDefaultWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult;

var
  WinCEWidgetSet: TWinCEWidgetSet;


Implementation

Uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// WinCEWSActnList,
 WinCEWSArrow,
 WinCEWSButtons,
// WinCEWSCalendar,
 WinCEWSCheckLst,
// WinCEWSCListBox,
 WinCEWSComCtrls,
 WinCEWSControls,
// WinCEWSDbCtrls,
// WinCEWSDBGrids,
// WinCEWSDialogs,
// WinCEWSDirSel,
// WinCEWSEditBtn,
 WinCEWSExtCtrls,
// WinCEWSExtDlgs,
// WinCEWSFileCtrl,
 WinCEWSForms,
// WinCEWSGrids,
// WinCEWSImgList,
// WinCEWSMaskEdit,
 WinCEWSMenus,
// WinCEWSPairSplitter,
 WinCEWSSpin,
 WinCEWSStdCtrls,
// WinCEWSToolwin,
////////////////////////////////////////////////////
  Arrow, Spin, CheckLst, LclProc;
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
  ComboBoxHandleSizeWindow: HWND = 0;//just dont know the use yet
  IgnoreNextCharWindow: HWND = 0;  // ignore next WM_(SYS)CHAR message


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
