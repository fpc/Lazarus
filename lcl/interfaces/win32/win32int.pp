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
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

Unit Win32Int;

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
  LMessages, StdCtrls, SysUtils, Win32Def, Graphics, Menus;

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
  TInitCommonControlsEx = packed record
    dwSize: dword;
    dwICC: dword;
  end;
  
  { Win32 interface-object class }
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

    FThemesActive: boolean;
    FThemeLibrary: HMODULE;
    IsThemeActive: function: LongBool; stdcall;
    IsAppThemed: function: LongBool; stdcall;
    InitCommonControlsEx: function(ICC: PInitCommonControlsEx): LongBool; stdcall;

    Procedure AssignSelf(Window: HWnd; Data: Pointer);

    Procedure AllocAndCopy(const BitmapInfo: Windows.TBitmap; const SrcRect: TRect; var Data: PByte; var Size: Cardinal);
    procedure FillRawImageDescriptionColors(Desc: PRawImageDescription);
    procedure FillRawImageDescription(const BitmapInfo: Windows.TBitmap;
        Desc: PRawImageDescription);

    Function WinRegister: Boolean;
    Procedure PaintPixmap(Surface: TObject; PixmapData: Pointer);
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
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;
    Procedure HandleEvents; Override;
    Procedure WaitMessage; Override;
    Procedure AppTerminate; Override;
    procedure ShowHide(Sender: TObject);
    Function  InitHintFont(HintFont: TObject): Boolean; Override;
    Function  RecreateWnd(Sender: TWinControl): Integer; virtual;
    Procedure AttachMenuToWindow(AMenuObject: TComponent); Override;
    procedure UpdateThemesActive;

    // procedures needed by interface methods
    procedure ResizeChild(Sender: TWinControl; Left, Top, Width, Height: Integer);
    
    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer; override;
    function DestroyTimer(TimerHandle: Integer) : boolean; override;

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
  ButtonClsName: array[0..6] of char = 'Button'#0;
  ComboboxClsName: array[0..8] of char = 'ComboBox'#0;
  TabControlClsName: array[0..15] of char = 'SysTabControl32'#0;

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
// To get as litle as posible circles,
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
  Calendar, CListBox, Spin, CheckLst, WinExt, LclProc;

type
  { Linked list of objects for events }
  PLazObject = ^TLazObject;
  TLazObject = Record
    Parent: TObject;
    Messages: TList;
    Next: PLazObject;
  End;

const
  // needs to move
  MCM_FIRST             = $1000;
  MCM_GETMINREQRECT     = MCM_FIRST + 9;
      

var
  OnClipBoardRequest: TClipboardRequestEvent;

{$I win32listsl.inc}
{$I win32callback.inc}
{$I win32object.inc}
{$I win32winapi.inc}
{$I win32lclintf.inc}

Initialization

  Assert(False, 'Trace:win32int.pp - Initialization');
{$ifdef MSG_DEBUG}
  MessageStackDepth := '';
{$endif}
  EraseBkgndStack := 0;

{$ifndef VER1_0}
  { TThread.Synchronize support }
  SynchronizeMethodProc := @PrepareSynchronize;
{$endif}

Finalization

  Assert(False, 'Trace:win32int.pp - Finalization');

End.

{ =============================================================================

  $Log$
  Revision 1.134  2005/02/23 01:12:47  marc
  + Added RemoveProp winapi call
  * Some maintenace on winapi/lclintf files

  Revision 1.133  2005/02/10 21:07:29  micha
  let general WindowProc also handle childedit of combobox, reduces code duplication, implements doubleclick for combobox

  Revision 1.132  2005/02/07 16:19:14  micha
  force default width and height for calendar control

  Revision 1.131  2005/01/15 10:09:23  micha
  fix bug 558: title bar redraw (win32)
  USE_SYNCHRONIZE enabled per default for 1.9.x

  Revision 1.130  2005/01/14 12:16:39  vincents
  added elements to LclCursorToWin32CursorMap for new crXXX constants

  Revision 1.129  2004/12/27 10:18:21  micha
  implement support for tthread.synchronize

  Revision 1.128  2004/11/04 16:57:31  micha
  remove obsolete and unused method twin32widgetset.settext

  Revision 1.127  2004/10/29 09:52:08  micha
  fix crash on showing tabpage
  fix painting of radiobutton in groupbox (non-tabpage-parent)

  Revision 1.126  2004/10/28 21:00:56  micha
  convert GetProp and SetProp usage to one Atom pointing to a record of fields

  Revision 1.125  2004/10/28 14:17:18  micha
  remove obsolete message event array

  Revision 1.124  2004/10/28 07:43:29  micha
  experiment: use CS_SAVEBITS class style on tabpages to reduce flickering

  Revision 1.123  2004/10/27 20:58:58  micha
  fix winxp theming for tabcontrols (shaded background)

  Revision 1.122  2004/10/16 10:17:21  micha
  remove statusbar helper methods from general widgetset object

  Revision 1.121  2004/10/15 09:51:09  micha
  splitup of CreateComponent to widgetset CreateHandle methods

  Revision 1.120  2004/10/06 10:52:46  micha
  split up common dialogs code

  Revision 1.119  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.118  2004/09/24 14:50:57  micha
  convert LM_SETDESIGNING message to TWidgetSet method

  Revision 1.117  2004/09/24 07:52:35  micha
  convert LM_SETPROPERTIES message to interface method for TCustomTrackBar
  remove message LM_SETPROPERTIES, conversion done

  Revision 1.116  2004/09/22 14:50:18  micha
  convert LM_SETPROPERTIES message for tcustomlabel to interface methods

  Revision 1.115  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.114  2004/09/18 17:07:58  micha
  convert LM_GETVALUE message to interface method

  Revision 1.113  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.112  2004/09/15 17:21:22  micha
  convert LM_GETITEMINDEX and LM_SETITEMINDEX messages to interface methods

  Revision 1.111  2004/09/14 10:06:26  micha
  convert LM_REDRAW message to interface method (in twidgetset)

  Revision 1.110  2004/09/13 19:57:30  micha
  convert LM_SHOWTABS message to interface method

  Revision 1.109  2004/09/13 19:06:04  micha
  convert LM_ADDPAGE and LM_REMOVEPAGE messages to new interface methods

  Revision 1.108  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.107  2004/09/12 19:50:36  micha
  convert LM_SETSIZE message to new interface method

  Revision 1.106  2004/09/12 13:11:50  micha
  convert LM_GETPIXEL and LM_SETPIXEL to interface methods (of twidgetset, DCGetPixel and DCSetPixel)

  Revision 1.105  2004/09/11 13:38:37  micha
  convert LM_BRINGTOFRONT message to interface method
  NOTE: was only used for tapplication, not from other controls

  Revision 1.104  2004/09/11 13:06:49  micha
  convert LM_ADDCHILD message to interface method

  Revision 1.103  2004/09/10 20:19:13  micha
  convert LM_CLB_G/SETCHECKED to interface methods

  Revision 1.102  2004/09/10 18:58:22  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.101  2004/09/10 14:38:29  micha
  convert lm_gettext to new interface methods
  remove lm_settext replacement settext methods in twidgetsets

  Revision 1.100  2004/09/10 09:43:13  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.99  2004/09/08 20:47:17  micha
  convert LM_SHOWHIDE message to new intf method TWSWinControl.ShowHide

  Revision 1.98  2004/09/08 19:09:34  micha
  convert LM_SETCOLOR message to new intf method TWSWinControl.SetColor

  Revision 1.97  2004/09/07 10:26:17  micha
  fix logs to get rid of comment level 2 warning

  Revision 1.96  2004/09/07 10:18:10  micha
  fix win32 interface, remove lm_setlimittext (obsolete)

  Revision 1.95  2004/09/07 09:44:46  micha
  convert lcl messages to new interface using methods: LM_G/SETSELSTART, LM_G/SETSELLEN, LM_G/SETLIMITTEXT

  Revision 1.94  2004/08/27 08:55:23  micha
  implement tapplication.minimize for win32, stub for gtk

  Revision 1.93  2004/08/25 15:04:44  micha
  use new lcl interface methods instead of messages (for win32; twsbitbtn)

  Revision 1.92  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.91  2004/07/15 10:43:39  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.90  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.89  2004/06/30 20:59:11  micha
  initialize common controls: date picker

  Revision 1.88  2004/06/29 08:03:08  micha
  fix showtabs for win32 interface

  Revision 1.87  2004/06/20 20:36:55  micha
  remove old obsolete/commented toolbutton code
  rename lazarusform classname to window, because we use it for panels, notebookpages, etc too

  Revision 1.86  2004/06/18 20:47:34  vincents
  fixed pasting from clipboard

  Revision 1.85  2004/06/18 19:55:43  micha
  fix xp themes drawing image on bitbtn

  Revision 1.84  2004/06/13 14:32:15  micha
  fix cursors to use what's available

  Revision 1.83  2004/06/10 22:07:58  vincents
  listbox style changes are notified to the widgetset

  Revision 1.82  2004/06/10 18:14:09  vincents
  converted win32proc.inc to unit

  Revision 1.81  2004/06/09 20:51:45  vincents
  implemented basic clipboard support for win32

  Revision 1.80  2004/05/21 09:03:55  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.79  2004/05/14 17:48:39  micha
  fix itemheight of listbox, handle measureitem message

  Revision 1.78  2004/05/12 09:46:25  micha
  fix toolbar buttons by handling them as customcontrols
  remove handledialogmessage, now handled in lcl

  Revision 1.77  2004/04/11 10:19:28  micha
  cursor management updated:
  - lcl notifies interface via WSControl.SetCursor of changes
  - fix win32 interface to respond to wm_setcursor callback and set correct cursor

  Revision 1.76  2004/04/10 17:54:52  micha
  - added: [win32] mousewheel default handler sends scrollbar messages
  - fixed: lmsetcursor; partial todo

  Revision 1.75  2004/03/26 21:20:54  vincents
  Fixed line endings

  Revision 1.74  2004/03/19 00:53:34  marc
  * Removed all ComponentCreateHandle routines

  Revision 1.73  2004/03/17 19:59:56  marc
  * Fixes some typos and changes uses clause cases

  Revision 1.72  2004/03/17 00:34:37  marc
  * Interface reconstruction. Created skeleton units, classes and wscontrols

  Revision 1.71  2004/03/05 01:04:21  marc
  * Renamed TWin32Object to TWin32WidgetSet

  Revision 1.70  2004/03/05 00:14:02  marc
  * Renamed TInterfaceBase to TWidgetSet

  Revision 1.69  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.68  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.66  2004/02/21 13:35:15  micha
  fixed: name clash SetCursor (message LM_SETCURSOR), and inherited SetCursor (winapi)

  Revision 1.65  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.64  2003/12/27 16:47:18  micha
  fix dialogs owner handle, fixes focusing issue

  Revision 1.63  2003/12/19 18:18:17  micha
  fix window activation z-order

  Revision 1.62  2003/12/18 10:17:00  micha
  remove non-useful variable wndlist (thx vincent)

  Revision 1.61  2003/12/18 08:51:01  micha
  fix accelerators: now registered per window

  Revision 1.60  2003/12/15 21:57:16  micha
  checklistbox, implement object+checked; from vincent

  Revision 1.59  2003/12/14 19:18:04  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.58  2003/12/13 19:44:42  micha
  hintwindow, color, rectangle size fixes

  Revision 1.57  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.56  2003/11/26 21:55:15  mattias
  fixed win32 TBaseMenuitem

  Revision 1.55  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.54  2003/11/25 21:20:38  micha
  implement tchecklistbox

  Revision 1.53  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.52  2003/11/21 20:32:01  micha
  cleanups; wm_hscroll/wm_vscroll fix

  Revision 1.51  2003/11/14 20:23:31  micha
  fpimage fixes

  Revision 1.50  2003/11/10 16:15:32  micha
  cleanups; win32 fpimage support

  Revision 1.49  2003/11/08 17:41:03  micha
  compiler warning cleanups

  Revision 1.48  2003/10/28 14:25:37  mattias
  fixed unit circle

  Revision 1.47  2003/10/26 17:34:41  micha
  new interface method to attach a menu to window

  Revision 1.46  2003/10/23 07:45:49  micha
  cleanups; single parent window (single taskbar button)

  Revision 1.45  2003/10/21 15:06:27  micha
  spinedit fix; variables cleanup

  Revision 1.44  2003/09/30 13:05:59  mattias
  removed FMainForm by Micha

  Revision 1.43  2003/09/27 09:52:44  mattias
  TScrollBox for win32 intf from Karl

  Revision 1.42  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.41  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.40  2003/09/14 09:43:45  mattias
  fixed common dialogs from Karl

  Revision 1.39  2003/09/08 13:24:17  mattias
  removed class function

  Revision 1.38  2003/09/08 12:21:48  mattias
  added fpImage reader/writer hooks to TBitmap

  Revision 1.37  2003/08/28 09:10:01  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.36  2003/08/27 09:33:26  mattias
  implements SET_LABEL from Micha

  Revision 1.35  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.34  2003/08/26 07:04:04  mattias
  fixed win32int

  Revision 1.33  2003/08/21 06:52:47  mattias
  size fixes from Karl

  Revision 1.32  2003/08/17 12:51:35  mattias
  added directory selection dialog from Vincent

  Revision 1.31  2003/08/17 12:47:53  mattias
  fixed mem leak

  Revision 1.30  2003/08/17 12:26:00  mattias
  fixed parts of the win32 intf size system

  Revision 1.29  2003/03/13 19:57:38  mattias
  added identcompletion context information and fixed win32 intf

  Revision 1.28  2003/03/11 07:46:44  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.27  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.26  2002/12/28 09:42:12  mattias
  toolbutton patch from Martin Smat

  Revision 1.25  2002/12/16 09:02:27  mattias
  applied win32 notebook patch from Vincent

  Revision 1.24  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.23  2002/12/04 20:39:16  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.22  2002/12/03 09:15:15  mattias
  cleaned up

  Revision 1.21  2002/11/26 20:51:05  mattias
  applied clipbrd patch from Vincent

  Revision 1.20  2002/11/23 13:48:48  mattias
  added Timer patch from Vincent Snijders

  Revision 1.19  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.18  2002/10/27 19:59:03  lazarus
  AJ: fixed compiling

  Revision 1.17  2002/10/26 15:15:55  lazarus
  MG: broke LCL<->interface circles

  Revision 1.16  2002/08/08 18:05:48  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.15  2002/05/31 13:10:49  lazarus
  Keith: Code cleanup.

  Revision 1.14  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.13  2002/04/03 03:41:29  lazarus
  Keith:
    * Removed more obsolete code
    * Compiles again!

  Revision 1.12  2002/04/03 01:52:42  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.11  2002/02/07 08:35:12  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.10  2002/02/03 06:06:25  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.9  2002/02/01 10:13:09  lazarus
  Keith: Fixes for Win32

  Revision 1.8  2002/01/31 09:32:07  lazarus
  Keith:
    * Open and save dialogs can now coexist in apps (however, only one of each type of common dialog can be used per app :( )
    * Fixed make all
    * Fixed crash in Windows 98/ME

  Revision 1.7  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.6  2002/01/17 03:17:44  lazarus
  Keith: Fixed TCustomPage creation

  Revision 1.5  2002/01/05 13:16:09  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.4  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.3  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.2  2000/12/12 14:16:43  lazarus
  Updated OI from Mattias
  Shane

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

}
