{%MainUnit win32int.pp}
{ $Id$ }

{******************************************************************************
  All Windows API related stuff goes here.

  !! Keep this alphabetical !!
 ******************************************************************************

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
//##apiwiz##sps##   // Do not remove

function Arc(DC: HDC; Left,Top, Width, Height, Angle1, Angle2: Integer): Boolean; Override;
function AngleChord(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean; override;

function BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc; Override;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; Override;
function BringWindowToTop(HWnd: HWND): Boolean; Override;

procedure CallDefaultWndHandler(Sender: TObject; var Message); Override;
function CallNextHookEx(HHk: HHOOK; NCode: Integer; WParam: WParam; LParam: LParam): Integer; Override;
function CallWindowProc(LPPrevWndFunc: TFarProc; Handle: HWND; Msg: UINT; WParam: WParam; LParam: LParam): Integer; Override;
function ClientToScreen(Handle: HWND; Var P: TPoint): Boolean; Override;

// clipboard
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): String; Override;
{ Gets data from the clipboard }
function ClipboardGetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat; Stream: TStream): Boolean; Override;
// ! List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType; Var Count: Integer; Var List: PClipboardFormat): Boolean; Override;
{ Sets the supported formats and requests ownership for the clipboard }
function ClipboardGetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent; FormatCount: Integer; Formats: PClipboardFormat): Boolean; Override;
{ Registers a clip board format }
function ClipboardRegisterFormat(Const AMimeType: String): TClipboardFormat; Override;

function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;  override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: LongInt; BitmapBits: Pointer): HBITMAP; Override;
function CreateBrushIndirect(Const LogBrush: TLogBrush): HBRUSH; Override;
function CreateCaret(Handle: HWND; Bitmap: HBITMAP; Width, Height: Integer): Boolean; Override;
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; Override;
function CreateCompatibleDC(DC: HDC): HDC; Override;
function CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP; Override;
function CreateFontIndirect(Const LogFont: TLogFont): HFONT; Override;
function CreatePenIndirect(Const LogPen: TLogPen): HPEN; Override;
{ Creates a bitmap from raw pixmap data }
function CreatePixmapIndirect(Const Data: Pointer; Const TransColor: LongInt): HBITMAP; Override;
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN; Override;
function CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN; Override;

function DeleteDC(HDC: HDC): Boolean; Override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean; Override;
function DestroyCaret(Handle: HWND): Boolean; Override;
function DrawFrameControl(DC: HDC; Var Rect: TRect; UType, UState: Cardinal): Boolean; Override;
function DrawEdge(DC: HDC; Var Rect: TRect; Edge: Cardinal; GrfFlags: Cardinal): Boolean; Override;
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; Override;

function Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; Override;
function EmptyClipBoard: Boolean;
function EnableScrollBar(Wnd: HWND; WSBFlags, WArrows: Cardinal): Boolean; Override;
function EnableWindow(HWnd: HWND; BEnable: Boolean): Boolean; Override;
function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; Override;
function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect; Str: PChar; Count: LongInt; Dx: PInteger): Boolean; Override;
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer; override;

function FillRect(DC: HDC; Const Rect: TRect; Brush: HBRUSH): Boolean; Override;
{ Draws a 3D border in GTK native style. }
function Frame3D(DC: HDC; var Rect: TRect; Const FrameWidth: Integer; Const Style: TBevelCut): Boolean; Override;

function GetBitmapRawImageDescription(Bitmap: HBITMAP; Desc: PRawImageDescription): Boolean; Override;
function GetDeviceRawImageDescription(DC: HDC; Desc: PRawImageDescription): boolean; Override;
function GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; Override;
function GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; Override;
function CreateBitmapFromRawImage(const RawImage: TRawImage; var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean; Override;

function GetActiveWindow: HWND; Override;
function GetCapture: HWND; Override;
function GetCaretPos(Var LPPoint: TPoint): Boolean; Override;
function GetCharABCWidths(DC: HDC; P2, P3: UINT; Const ABCStructs): Boolean; Override;
function GetClientBounds(Handle: HWND; Var Rect: TRect): Boolean; Override;
function GetClientRect(Handle: HWND; Var Rect: TRect): Boolean; Override;
function GetClipBox(DC : hDC; lpRect : PRect) : Longint; Override;
function GetClipRGN(DC : hDC; RGN : hRGN) : Longint; override;
function GetCursorPos(var LPPoint: TPoint): Boolean; override;
function GetDC(HWnd: HWND): HDC; Override;
function GetDeviceCaps(DC: HDC; Index: Integer): Integer; Override;
function GetDeviceSize(DC: HDC; var P: TPoint): Boolean; Override;
function GetDesignerDC(WindowHandle: HWND): HDC; Override;
function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean; override;
function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer; Override;
function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint; Override;
function GetFocus: HWND; Override;
function GetKeyState(NVirtKey: Integer): SmallInt; Override;
function GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; Override;
function GetParent(Handle: HWND): HWND; Override;
function GetProp(Handle: HWND; Str: PChar): Pointer; Override;
function GetROP2(DC: HDC): Integer; override;
function GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean; Override;
function GetStockObject(Value: Integer): LongInt; Override;
function GetSysColor(NIndex: Integer): DWORD; Override;
function GetSystemMetrics(NIndex: Integer): Integer; Override;
function GetTextColor(DC: HDC): TColorRef; Override;
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; Var Size: TSize): Boolean; Override;
function GetTextMetrics(DC: HDC; Var TM: TTextMetric): Boolean; Override;
function GetWindowLong(Handle: HWND; Int: Integer): PtrInt; Override;
function GetWindowOrgEx(DC: HDC; P: PPoint): Integer; Override;
function GetWindowRect(Handle: HWND; Var Rect: TRect): Integer; Override;
function GetWindowRelativePosition(Handle : HWND; var Left, Top:integer): boolean; Override;
function GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean; override;

function HideCaret(HWnd: HWND): Boolean; Override;

function IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer; override;
function InvalidateRect(AHandle: HWND; Rect: PRect; BErase: Boolean): Boolean; Override;
function IsWindowEnabled(handle: HWND): boolean; override;
function IsWindowVisible(handle: HWND): boolean; override;

function LineTo(DC: HDC; X, Y: Integer): Boolean; Override;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; Override;
function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean; Override;
function MessageBox(HWnd: HWND; LPText, LPCaption: PChar; UType: Cardinal): Integer; Override;
function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; Override;

function PeekMessage(Var LPMsg: TMsg; Handle: HWND; WMsgFilterMin, WMsgFilterMax, WRemoveMsg: UINT): Boolean; Override;
function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean): Boolean; Override;
function Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean): Boolean; Override;
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): Boolean; Override;
function PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean; Override;

function RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; override;
function RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; override;
function RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; override;
function RealizePalette(DC: HDC): Cardinal; Override;
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; Override;
function RectVisible(dc : hdc; const ARect: TRect) : Boolean; Override;
function ReleaseCapture: Boolean; Override;
function ReleaseDC(Window: HWND; DC: HDC): Integer; Override;
function ReleaseDesignerDC(Window: HWND; DC: HDC): Integer; Override;
function RemoveProp(Handle: hwnd; Str: PChar): THandle; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; Override;

function SaveDC(DC: HDC): Integer; Override;
function ScreenToClient(Handle: HWND; Var P: TPoint): Integer; Override;
function ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll, PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean; Override;
function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; override;
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; Override;
function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; Override;
function SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; Override;
function SetActiveWindow(Window: HWND): HWND; override;
function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; Override;
function SetBkMode(DC: HDC; BkMode: Integer): Integer; Override;
function SetCapture(Value: HWND): HWND; Override;
function SetCaretPos(X, Y: Integer): Boolean; Override;
function SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean; Override;
function SetCaretRespondToFocus(Handle: HWND; ShowHideOnFocus: Boolean): Boolean; Override;
function SetCursorPos(X, Y: Integer): Boolean; override;
function SetFocus(HWnd: HWND): HWND; Override;
function SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean; Override;
function SetROP2(DC: HDC; Mode: Integer): Integer; override;
function SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; BRedraw: Boolean): Integer; Override;
function SetSysColors(CElements: Integer; Const LPAElements; Const LPARgbValues): Boolean; Override;
function SetTextCharacterExtra(_HDC: HDC; NCharExtra: Integer): Integer; Override;
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; Override;
function SetWindowLong(Handle: HWND; Idx: Integer; NewLong: PtrInt): PtrInt; Override;
function SetWindowOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean; Override;
function SetWindowPos(HWnd: HWND; HWndInsertAfter: HWND; X, Y, CX, CY: Integer; UFlags: UINT): Boolean; Override;
function ShowCaret(HWnd: HWND): Boolean; Override;
function ShowScrollBar(Handle: HWND; WBar: Integer; BShow: Boolean): Boolean; Override;
function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean; Override;
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; Override;
{ Copies a bitmap from a source rectangle into a destination rectangle using the specified mask and raster operations }
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; Override;

function TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): Boolean; Override;

function WindowFromPoint(Point: TPoint): HWND; Override;

Procedure InitializeCriticalSection(var CritSection: TCriticalSection); Override;
Procedure EnterCriticalSection(var CritSection: TCriticalSection); Override;
Procedure LeaveCriticalSection(var CritSection: TCriticalSection); Override;
Procedure DeleteCriticalSection(var CritSection: TCriticalSection); Override;

//##apiwiz##eps##   // Do not remove
{ =============================================================================

  $Log$
  Revision 1.57  2005/06/22 17:37:06  mattias
  implemented TMouse.SetCursorPos  from Andrew

  Revision 1.56  2005/03/25 16:41:35  micha
  implement TWin32WidgetSet.GetDCOriginRelativeToWindow

  Revision 1.55  2005/03/04 13:50:09  mattias
  fixed Arc and changed x,y to Left,Top to make meaning more clear

  Revision 1.54  2005/02/23 01:12:47  marc
  + Added RemoveProp winapi call
  * Some maintenace on winapi/lclintf files

  Revision 1.53  2005/02/05 20:40:17  vincents
  SetCapture uses HWND as parameter and result type

  Revision 1.52  2005/02/05 09:05:51  micha
  add platform independent winapi function IsWindowEnabled

  Revision 1.51  2005/01/08 11:03:18  mattias
  implemented TPen.Mode=pmXor  from Jesus

  Revision 1.50  2004/11/19 21:59:23  micha
  fix focus issue (for example using MessageDlg)

  Revision 1.49  2004/03/05 12:16:09  micha
  fix designer (overlay) window transparency issue
  fix releasedesignerdc to use correct window

  Revision 1.48  2004/02/23 08:19:05  micha
  revert intf split

  Revision 1.46  2004/02/19 05:07:17  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.45  2004/02/03 08:54:09  mattias
  Frame3D rect now var again

  Revision 1.44  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.43  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.42  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.41  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.40  2003/11/18 07:20:40  micha
  added "included by" notice at top of file

  Revision 1.39  2003/11/14 21:17:59  micha
  new maskblt method for transparency blitting

  Revision 1.38  2003/11/10 16:15:32  micha
  cleanups; win32 fpimage support

  Revision 1.37  2003/11/07 18:48:52  micha
  symmetry getdesignerdc, releasedesignerdc

  Revision 1.36  2003/11/04 13:18:22  micha
  Implement GetDesignerDC

  Revision 1.35  2003/10/31 11:49:30  micha
  fix win32 designer: grid, non-windowed components, selectiongrabbers

  Revision 1.34  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.33  2003/08/31 17:30:49  mattias
  fixed TControl painting for win32

  Revision 1.32  2003/08/18 19:24:18  mattias
  fixed TCanvas.Pie

  Revision 1.31  2003/08/13 21:23:10  mattias
  fixed log

  Revision 1.30  2003/08/13 16:26:07  mattias
  fixed combobox height from Karl

  Revision 1.29  2003/07/29 07:38:09  marc
  + Added GetCursorPos

  Revision 1.28  2003/07/26 10:30:44  mattias
  rewritten WM_COMMAND by Micha

  Revision 1.27  2003/07/07 07:59:34  mattias
  made Size_SourceIsInterface a flag

  Revision 1.26  2003/07/04 10:12:16  mattias
  added default message handler to win32 interface

  Revision 1.25  2003/07/03 17:19:20  mattias
  added RectVisible from Micha

  Revision 1.24  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.23  2003/03/25 08:12:39  mattias
  patch from Martin Smat for menu items and default messages

  Revision 1.22  2003/03/06 17:15:49  mattias
  applied patch from Martin Smat fixing LM_SETSIZE, LM_SETDESIGNING, LM_SCREENINIT

  Revision 1.21  2003/03/01 17:54:53  mattias
  added ShowWindow function

  Revision 1.20  2003/01/12 19:09:19  mattias
  patch from Martin Smat for dis/enabling menuitems

  Revision 1.19  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.18  2002/12/26 11:00:15  mattias
  added included by to unitinfo and a few win32 functions

  Revision 1.17  2002/12/25 13:30:37  mattias
  added more windows funcs and fixed jump to compiler error end of file

  Revision 1.16  2002/11/23 13:48:49  mattias
  added Timer patch from Vincent Snijders

  Revision 1.15  2002/10/01 10:16:34  lazarus
  MG: removed last clientrectbugfix switches

  Revision 1.14  2002/09/10 06:49:25  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.13  2002/08/30 12:32:25  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.12  2002/08/28 09:40:52  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.11  2002/08/19 20:34:49  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.10  2002/08/15 15:46:50  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.9  2002/08/13 07:08:25  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.8  2002/08/08 18:05:48  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.7  2002/05/13 22:00:25  lazarus
  Keith: Implemented GetWindowSize

  Revision 1.6  2002/05/10 07:43:49  lazarus
  MG: updated licenses

  Revision 1.5  2002/02/07 08:35:13  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.4  2002/02/03 06:06:26  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.3  2002/01/18 09:07:44  lazarus
  Keith: Fixed menu creation

  Revision 1.2  2002/01/17 03:17:44  lazarus
  Keith: Fixed TCustomPage creation

  Revision 1.1  2002/01/06 23:09:53  lazarus
  MG: added missing files


}
