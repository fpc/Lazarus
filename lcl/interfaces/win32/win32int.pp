{ $Id$ }
{
 /***************************************************************************
                         WIN32INT.pp  -  Win32Interface Object
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

Unit Win32Int;

{$mode objfpc}{$H+}{$T-}{$message warning Fix implicit pointer conversions}

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
  LMessages, StdCtrls, SysUtils, Win32Def, Graphics, Menus, CommCtrl;

const

  IDC_ARROW     = MakeIntResource(32512);
  IDC_IBEAM     = MakeIntResource(32513);
  IDC_WAIT      = MakeIntResource(32514);
  IDC_CROSS     = MakeIntResource(32515);
  IDC_UPARROW   = MakeIntResource(32516);
  IDC_SIZE      = MakeIntResource(32640);
  IDC_ICON      = MakeIntResource(32641);
  IDC_SIZENWSE  = MakeIntResource(32642);
  IDC_SIZENESW  = MakeIntResource(32643);
  IDC_SIZEWE    = MakeIntResource(32644);
  IDC_SIZENS    = MakeIntResource(32645);
  IDC_SIZEALL   = MakeIntResource(32646);
  IDC_NO        = MakeIntResource(32648);
  IDC_HAND      = MakeIntResource(32649);
  IDC_APPSTARTING = MakeIntResource(32650);
  IDC_HELP      = MakeIntResource(32651);

{
  These are add-ons, don't exist in windows itself!
  IDC_NODROP    = MakeIntResource(32767);
  IDC_DRAG      = MakeIntResource(32766);
  IDC_HSPLIT    = MakeIntResource(32765);
  IDC_VSPLIT    = MakeIntResource(32764);
  IDC_MULTIDRAG = MakeIntResource(32763);
  IDC_SQLWAIT   = MakeIntResource(32762);
  IDC_HANDPT    = MakeIntResource(32761);
}
  IDC_NODROP    = IDC_NO;
  IDC_DRAG      = IDC_ARROW;
  IDC_HSPLIT    = IDC_SIZEWE;
  IDC_VSPLIT    = IDC_SIZENS;
  IDC_MULTIDRAG = IDC_ARROW;
  IDC_SQLWAIT   = IDC_WAIT;
  IDC_HANDPT    = IDC_HAND;

  LclCursorToWin32CursorMap: array[crLow..crHigh] of PChar = (
  // uni-direction cursors are mapped to bidirection win32 cursors
     IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZEWE, IDC_SIZEWE,
     IDC_SIZENESW, IDC_SIZENS, IDC_SIZENWSE, IDC_SIZEALL, IDC_HANDPT, IDC_HELP,
     IDC_APPSTARTING, IDC_NO, IDC_SQLWAIT, IDC_MULTIDRAG, IDC_VSPLIT,
     IDC_HSPLIT, IDC_NODROP, IDC_DRAG, IDC_WAIT, IDC_UPARROW, IDC_SIZEWE,
     IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZE, IDC_IBEAM, IDC_CROSS,
     IDC_ARROW, IDC_ARROW, IDC_ARROW);

  { month picker, date picker, time picker, updown }
  ICC_DATE_CLASSES       = $00000100;

Type
  PInitCommonControlsEx = ^TInitCommonControlsEx;
  TInitCommonControlsEx = record
    dwSize: dword;
    dwICC: dword;
  end;

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

  { Win32 interface-object class }

  { TWin32WidgetSet }

  TWin32WidgetSet = Class(TWidgetSet)
  Private
    // The parent of all windows, represents the button of the taskbar
    // This window is also the owner of the clipboard.
    // Assoc. windowproc also acts as handler for popup menus
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

    FThemesActive: boolean;
    FThemeLibrary: HMODULE;
    IsThemeActive: function: LongBool; stdcall;
    IsAppThemed: function: LongBool; stdcall;
    InitCommonControlsEx: function(ICC: PInitCommonControlsEx): LongBool; stdcall;

    Procedure AssignSelf(Window: HWnd; Data: Pointer);

    procedure AllocAndCopy(const BitmapInfo: Windows.TBitmap; const BitmapHandle: HBITMAP;
      const SrcRect: TRect; var Data: PByte; var Size: PtrUInt);
    procedure FillRawImageDescriptionColors(Desc: PRawImageDescription);
    procedure FillRawImageDescription(const BitmapInfo: Windows.TBitmap;
        Desc: PRawImageDescription);

    { event handler helper functions }
    procedure HandleProcessEvent(AData: PtrInt; AFlags: dword);
    procedure CheckPipeEvents;

    Function WinRegister: Boolean;
    Procedure NormalizeIconName(Var IconName: String);
    Procedure NormalizeIconName(Var IconName: PChar);

  Public
    { Creates a callback of Lazarus message Msg for Sender }
    Procedure SetCallback(Msg: LongInt; Sender: TObject); virtual;
    { Removes all callbacks for Sender }
    Procedure RemoveCallbacks(Sender: TObject); virtual;

    { Constructor of the class }
    Constructor Create;
    { Destructor of the class }
    Destructor Destroy; Override;
    { Initialize the API }
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppMinimize; override;
    procedure AppBringToFront; override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    Procedure AppTerminate; Override;
    procedure AppSetTitle(const ATitle: string); override;

    Function  InitHintFont(HintFont: TObject): Boolean; Override;
    Procedure AttachMenuToWindow(AMenuObject: TComponent); Override;

    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;

    procedure UpdateThemesActive;
    procedure ShowHide(Sender: TObject);

    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;

    // thread synchronize support
    procedure HandleWakeMainThread(Sender: TObject);

    {$I win32winapih.inc}
    {$I win32lclintfh.inc}

    property AppHandle: HWND read FAppHandle;
    property MessageFont: HFONT read FMessageFont;
    property ThemesActive: boolean read FThemesActive;
  End;

  {$I win32listslh.inc}

const
  BOOL_RESULT: Array[Boolean] Of String = ('False', 'True');
  ClsName: array[0..6] of char = 'Window'#0;
  EditClsName: array[0..4] of char = 'Edit'#0;
  ButtonClsName: array[0..6] of char = 'Button'#0;
  ComboboxClsName: array[0..8] of char = 'ComboBox'#0;
  TabControlClsName: array[0..15] of char = 'SysTabControl32'#0;

{$ifdef DEBUG_DOUBLEBUFFER}
var
  CopyBitmapToClipboard: boolean = true;
{$endif}

{ export for widgetset implementation }

function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
function ComboBoxWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
function CallDefaultWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult;

Implementation

Uses
  Win32Proc,
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// Win32WSActnList,
 Win32WSArrow,
 Win32WSButtons,
 Win32WSCalendar,
 Win32WSCheckLst,
// Win32WSCListBox,
 Win32WSComCtrls,
 Win32WSControls,
// Win32WSDbCtrls,
// Win32WSDBGrids,
 Win32WSDialogs,
// Win32WSDirSel,
// Win32WSEditBtn,
 Win32WSExtCtrls,
// Win32WSExtDlgs,
// Win32WSFileCtrl,
 Win32WSForms,
// Win32WSGrids,
// Win32WSImgList,
// Win32WSMaskEdit,
 Win32WSMenus,
// Win32WSPairSplitter,
 Win32WSSpin,
 Win32WSStdCtrls,
// Win32WSToolwin,
////////////////////////////////////////////////////
 Arrow, Calendar, Spin, CheckLst, WinExt, LclProc;

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
  ComboBoxHandleSizeWindow: HWND = 0;
  IgnoreNextCharWindow: HWND = 0;  // ignore next WM_(SYS)CHAR message
  OnClipBoardRequest: TClipboardRequestEvent;
{$ifdef MSG_DEBUG}
  MessageStackDepth: string = '';
{$endif}

{$I win32listsl.inc}
{$I win32callback.inc}
{$I win32object.inc}
{$I win32winapi.inc}
{$I win32lclintf.inc}

Initialization

  Assert(False, 'Trace:win32int.pp - Initialization');
  { initialize mousedownclick to far before double click time }
  MouseDownTime := GetTickCount - 5000;

finalization

  Assert(False, 'Trace:win32int.pp - Finalization');

end.
