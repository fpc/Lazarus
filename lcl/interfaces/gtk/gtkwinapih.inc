(******************************************************************************
  All GTK Winapi related stuff goes here.

  !! Keep this alphabetical !!
 ******************************************************************************)
//##apiwiz##sps##   // Do not remove

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; override;

function CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer; override;
Function CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; wParam ,lParam : LongInt) : Integer; override;
Function ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean;override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; override;
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; override;
function CreateCaret(Handle : HWND; Bitmap : hBitmap; width, Height : Integer) : Boolean; override;
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; override;
function CreateCompatibleDC(DC: HDC): HDC; override;
function CreateFontIndirect(const LogFont: TLogFont): HFONT; override;
function CreatePenIndirect(const LogPen: TLogPen): HPEN; override;
function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP; override;
function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;

function DeleteDC(hDC: HDC): Boolean; override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean;  override;
function DestroyCaret: Boolean; override;
Function DrawFrameControl(DC: HDC; var Rect : TRect; uType, uState : Cardinal) : Boolean; override;
function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean; override;

Function EmptyClipBoard : Boolean; override;
function EnableMenuItem(hMenu: HMENU; uIDEnableItem: Integer; bEnable: Boolean): Boolean; override;
function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean; override;
function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean; override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  override;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; override;

Function GetActiveWindow : HWND; override;
function GetCapture: HWND; override;
function GetCaretPos(var lpPoint: TPoint): Boolean; override;
function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean; override;
Function GetClientRect(handle : HWND; var Rect : TRect) : Boolean; override;
function GetDC(hWnd: HWND): HDC;  override;
function GetFocus: HWND; override;
function GetKeyState(nVirtKey: Integer): Smallint; override;
function GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; override;
Function GetParent(Handle : HWND): HWND; override;
Function GetProp(Handle : hwnd; Str : PChar): Pointer; override;
function GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean; override;
function GetStockObject(Value: Integer): LongInt; override;
function GetSysColor(nIndex: Integer): DWORD; override;
function GetSystemMetrics(nIndex: Integer): Integer;  override;
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;  override;
Function GetWindowLong(Handle : hwnd; int : Integer): Longint; override;
Function GetWindowOrgEx(dc : hdc; var P : TPoint): Integer;  override;
Function GetWindowRect(Handle : hwnd; var Rect : TRect): Integer; override;

function HideCaret(hWnd: HWND): Boolean; override;

function InvalidateRect(aHandle : HWND; Rect : pRect; bErase : Boolean) : Boolean; override;

function KillTimer (hWnd : HWND; uIDEvent : cardinal) : boolean; override;

function LineTo(DC: HDC; X, Y: Integer): Boolean; override;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; override;
function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal): integer; override;
function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; override;

function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
function PostMessage(hWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Boolean; override;

function RealizePalette(DC: HDC): Cardinal; override;
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
Function ReleaseCapture : Boolean; override;
function ReleaseDC(hWnd: HWND; DC: HDC): Integer; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; override;

function SaveDC(DC: HDC): Integer; override;
Function ScreenToClient(Handle : HWND; var P : TPoint) : Integer; override;
function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; override;
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;  override;
function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; override;
function SendMessage(HandleWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Integer; override;
function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; override;
Function SetBkMode(DC: HDC; bkMode : Integer) : Integer; override;
Function SetCapture(Value : Longint): Longint; override;
function SetCaretPos(X, Y: Integer): Boolean; override;
function SetFocus(hWnd: HWND): HWND; override;
Function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean; override;
function SetScrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer; override;
function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; override;
Function SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer; override;
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetTimer(hWnd: HWND; nIDEvent, uElapse: integer; lpTimerFunc: TFNTimerProc) : integer; override;
function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : Longint): LongInt;
function SetWindowOrgEx(dc : hdc; NewX, NewY : Integer; var Point: TPoint) : Boolean; override;
function SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
      X, Y, cx, cy: Integer; uFlags: UINT): Boolean;  override;
function ShowCaret(hWnd: HWND): Boolean; override;
function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean; override;
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; override;
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; override;

Function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;



Function WindowFromPoint(Point : TPoint) : HWND; override;
//##apiwiz##eps##   // Do not remove
{ =============================================================================

  $Log$
  Revision 1.16  2001/06/26 21:44:33  lazarus
  MG: reduced paint messages

  Revision 1.15  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.14  2001/04/06 22:25:15  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.13  2001/03/26 14:58:32  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.12  2001/03/13 15:02:14  lazarus
  MG: activated GetWindowOrgEx

  Revision 1.11  2001/02/16 19:13:31  lazarus
  Added some functions
  Shane

  Revision 1.10  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.9  2001/01/23 19:13:57  lazarus
  Fixxed the errors I commited with Unionrect
  Shane

  Revision 1.8  2001/01/23 19:01:10  lazarus
  Fixxed bug in RestoreDC
  Shane

  Revision 1.7  2001/01/23 18:42:10  lazarus
  Added InvalidateRect to gtkwinapi.inc
  Shane

  Revision 1.6  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.5  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.4  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.3  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.2  2000/07/30 21:48:34  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import

  Revision 1.7  2000/05/11 22:04:16  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.6  2000/05/08 12:54:20  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.5  2000/05/03 00:27:05  lazarus
  MWE:
    + First rollout of the API wizzard.

  Revision 1.4  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.3  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.2  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.1  2000/03/30 22:51:43  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.29  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.28  2000/03/28 22:47:50  lazarus
  MWE:
    Started with the blt function family

  Revision 1.27  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.26  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.25  2000/03/14 19:49:05  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.24  2000/03/10 18:31:10  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.23  2000/03/09 23:48:00  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.22  2000/03/08 23:57:39  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.21  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.20  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.19  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.18  2000/01/31 20:00:22  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.17  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.16  2000/01/16 23:23:07  lazarus
  MWE:
    Added/completed scrollbar API funcs

  Revision 1.15  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.14  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.13  1999/12/20 21:01:14  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.12  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.11  1999/12/14 21:07:12  lazarus
  Added more stuff for TToolbar
  Shane

  Revision 1.10  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.9  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.8  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.7  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.6  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.5  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.4  1999/11/16 01:32:22  lazarus
  MWE:
    Added some more DC functionality

}

