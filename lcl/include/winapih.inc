(******************************************************************************
  All Winapi related stuff goes here.
  This file is used by both LCLLinux and InterfaceBase
  If a procedure is platform dependent then it should have the following tail:
    {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
  If a procedure isn't platform dependent, it is no part of InterfaseBase and
  should be surrounded by:
    {$IFNDEF IF_BASE_MEMBER}
      procedure MyProc;
    {$ENDIF}

  !! Keep this alphabetical and place a pointer to the independent part 
  !! if needed 
 ******************************************************************************)


{******************************************************************************
  Platform specific stuff
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove
 
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCaret(Handle: HWND; Bitmap: hBitmap; width, Height: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCompatibleDC(DC: HDC): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreateFont --> independent
function CreateFontIndirect(const LogFont: TLogFont): HFONT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreatePen --> independent
function CreatePenIndirect(const LogPen: TLogPen): HPEN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function DeleteDC(hDC: HDC): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DeleteObject(GDIObject: HGDIOBJ): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DestroyCaret: Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function DrawFrameControl(DC: HDC; var Rect : TRect; uType, uState : Cardinal) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function EnableMenuItem(hMenu: HMENU; uIDEnableItem: Integer; bEnable: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function EqualRect --> independent
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function GetCapture : HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCaretPos(var lpPoint: TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCursorPos(var lpPoint: TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} //pbd
function GetDC(hWnd: HWND): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetFocus: HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetKeyState(nVirtKey: Integer): Smallint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetParent(Handle : HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetProp(Handle : hwnd; Str : PChar): Pointer;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetScrollPos --> independent
//function GetScrollRange --> independent
function GetStockObject(Value: Integer): LongInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSysColor(nIndex: Integer): DWORD; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSystemMetrics(nIndex: Integer): Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowLong(Handle : hwnd; int : Integer): Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowRect(Handle : hwnd; Rect : PRect): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowOrgEx(dc : hdc; var P : TPoint): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function HideCaret(hWnd: HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function InflateRect --> independent
//function IntersectRect --> independent
//function IsRectEmpty --> independent

function LineTo(DC: HDC; X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function OffsetRect --> independent

function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PostMessage(hWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function PtInRect --> independent

function RealizePalette(DC: HDC): Cardinal; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseCapture : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseDC(hWnd: HWND; hDC: HDC): Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function SaveDC(DC: HDC): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function ScrollWindow --> independent deprecated calls ScrollWindowEx
function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SendMessage(hWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} //pbd
Function SetCapture (Value : LongInt): LongInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretPos(X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetFocus(hWnd: HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetRect --> independent
//function SetRectEmpty --> independent
function SetScrollInfo(Handle: HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; Redraw : Boolean): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetScrollPos --> independent
//function SetScrollRange --> independent
function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : Longint): Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowOrgEx(dc : hdc; NewX, NewY : Integer; Var Point : TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowCaret(hWnd: HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function UnionRect --> independent


Function WindowFromPoint(Point : TPoint) : HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//##apiwiz##eps##     // Do not remove
{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

{$IFNDEF IF_BASE_MEMBER}

const
  { Ternary raster operations }
  SRCCOPY     = $00CC0020;     { dest = source                    }
  SRCPAINT    = $00EE0086;     { dest = source OR dest            }
  SRCAND      = $008800C6;     { dest = source AND dest           }
  SRCINVERT   = $00660046;     { dest = source XOR dest           }
  SRCERASE    = $00440328;     { dest = source AND (NOT dest )    }
  NOTSRCCOPY  = $00330008;     { dest = (NOT source)              }
  NOTSRCERASE = $001100A6;     { dest = (NOT src) AND (NOT dest)  }
  MERGECOPY   = $00C000CA;     { dest = (source AND pattern)      }
  MERGEPAINT  = $00BB0226;     { dest = (NOT source) OR dest      }
  PATCOPY     = $00F00021;     { dest = pattern                   }
  PATPAINT    = $00FB0A09;     { dest = DPSnoo                    }
  PATINVERT   = $005A0049;     { dest = pattern XOR dest          }
  DSTINVERT   = $00550009;     { dest = (NOT dest)                }
  BLACKNESS   = $00000042;     { dest = BLACK                     }
  WHITENESS   = $00FF0062;     { dest = WHITE                     }

type
  WPARAM = longint;
  LPARAM = LongInt;
  LRESULT = LongInt;

//##apiwiz##spi##    // Do not remove

function AdjustWindowRectEx( Var Rect: TRect; Style1: Word; MenuExist : Boolean; Style2 : Word) : Boolean;

Function BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc;

function CopyRect(var lprcDst: TRect; const lprcSrc: TRect): Boolean;
function CreateFont(Height, Width, Escapement, Orientation, Weight: Integer;
  Italic, Underline, StrikeOut, CharSet, OutputPrecision, ClipPrecision,
  Quality, PitchAndFamily: Cardinal; FaceName: PChar): HFONT;
function CreatePen(Style, Width: Integer; Color: TColorRef): HPEN;

Function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
Function ExcludeClipRect(dc: hdc; LEft,Top, Right, Bottom : Integer) : Integer;
function EqualRect(const lprc1, lprc2: TRect): Boolean;

function GetScrollPos(Handle: HWND; nBar: Integer): Integer;
function GetScrollRange(Handle: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): Boolean;

function InflateRect(var Rect: TRect; dx, dy: Integer): Boolean;
function IntersectClipRect(dc: hdc; Leftrect, Toprect, RightREct,Bottomrect : Integer): Integer;
function IntersectRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): Boolean;
function IsRectEmpty(const lprc: TRect): Boolean; 

function OffSetRect(var Rect: TRect; dx,dy: Integer): Boolean;

function MakeWParam(l, h: Word): WPARAM;
function MakeLParam(l, h: Word): LPARAM;
function MakeLResult(l, h: Word): LRESULT;

Function PtInRect(Rect : TRect; Point : TPoint) : Boolean;
Function PointtoSmallPoint(const P : TPoint) : TSmallPoint;

function RectVisible(dc : hdc; Rect: TRect) : Boolean;

function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer; Rect, ClipRect: PRect): Boolean;
function SetRect(Var Rect : TRect; xLeft,yTop,xRight,yBottom : Integer) : Boolean;
function SetRectEmpty(var lprc: TRect): Boolean;
function SetScrollPos(Handle: HWND; nBar, nPos: Integer; bRedraw: Boolean): Integer;
function SetScrollRange(Handle: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: Boolean): Boolean;
Function SmallPointtoPoint(const P : TSmallPoint) : Tpoint;

function UnionRect(var lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): Boolean; //pbd


//##apiwiz##epi##    // Do not remove


{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.7  2000/05/11 22:04:15  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.6  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane

  Revision 1.5  2000/05/08 12:54:20  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.4  2000/04/10 22:22:18  lazarus
  MWE:
    + Added fpcdir to makefiles

  Revision 1.3  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.34  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.33  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.32  2000/03/28 22:47:50  lazarus
  MWE:
    Started with the blt function family

  Revision 1.31  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.30  2000/03/16 23:58:47  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.29  2000/03/14 19:49:05  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.28  2000/03/13 23:17:34  lazarus
  MWE:
    + finished hide caret
    + added blinking caret

  Revision 1.27  2000/03/10 18:31:11  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.26  2000/03/09 23:48:06  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.25  2000/03/08 23:57:39  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.24  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.23  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.22  2000/02/24 21:15:31  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.21  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.20  2000/01/31 20:00:22  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.19  2000/01/25 23:51:14  lazarus
  MWE:
    Added more Caret functionality.
    Removed old ifdef stuff from the editor

  Revision 1.18  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.17  2000/01/18 21:47:00  lazarus
  Added OffSetRec

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

