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

Function Arc(DC: HDC; X, Y, Width, Height, Angle1, Angle2: Integer): Boolean; Override;
function AngleChord(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean; override;

Function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; Override;
Function BringWindowToTop(HWnd: HWND): Boolean; Override;

Function CallNextHookEx(HHk: HHOOK; NCode: Integer; WParam, LParam: LongInt): Integer; Override;
Function CallWindowProc(LPPrevWndFunc: TFarProc; Handle: HWND; Msg: UINT; WParam, LParam: LongInt): Integer; Override;
Function ClientToScreen(Handle: HWND; Var P: TPoint): Boolean; Override;

// clipboard
Function ClipboardFormatToMimeType(FormatID: TClipboardFormat): String; Override;
{ Gets data from the clipboard }
Function ClipboardGetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat; Stream: TStream): Boolean; Override;
// ! List will be created. You must free it yourself with FreeMem(List) !
Function ClipboardGetFormats(ClipboardType: TClipboardType; Var Count: Integer; Var List: PClipboardFormat): Boolean; Override;
{ Sets the supported formats and requests ownership for the clipboard }
Function ClipboardGetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent; FormatCount: Integer; Formats: PClipboardFormat): Boolean; Override;
{ Registers a clip board format }
Function ClipboardRegisterFormat(Const AMimeType: String): TClipboardFormat; Override;

Function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;  override;
Function CreateBitmap(Width, Height: Integer; Planes, BitCount: LongInt; BitmapBits: Pointer): HBITMAP; Override;
Function CreateBrushIndirect(Const LogBrush: TLogBrush): HBRUSH; Override;
Function CreateCaret(Handle: HWND; Bitmap: HBITMAP; Width, Height: Integer): Boolean; Override;
Function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; Override;
Function CreateCompatibleDC(DC: HDC): HDC; Override;
Function CreateFontIndirect(Const LogFont: TLogFont): HFONT; Override;
Function CreatePenIndirect(Const LogPen: TLogPen): HPEN; Override;
{ Creates a bitmap from raw pixmap data }
Function CreatePixmapIndirect(Const Data: Pointer; Const TransColor: LongInt): HBITMAP; Override;
Function CreatePolygonRgn(Points: PPoint; NumPts: Integer; Winding : Boolean): HRGN; Override;
Function CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN; Override;

Function DeleteDC(HDC: HDC): Boolean; Override;
Function DeleteObject(GDIObject: HGDIOBJ): Boolean; Override;
Function DestroyCaret(Handle: HWND): Boolean; Override;
Function DrawFrameControl(DC: HDC; Var Rect: TRect; UType, UState: Cardinal): Boolean; Override;
Function DrawEdge(DC: HDC; Var Rect: TRect; Edge: Cardinal; GrfFlags: Cardinal): Boolean; Override;
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; Override;

Function Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; Override;
Function EmptyClipBoard: Boolean;
Function EnableMenuItem(HMenu: HMENU; UIDEnableItem: Integer; BEnable: Boolean): Boolean; Override;
Function EnableScrollBar(Wnd: HWND; WSBFlags, WArrows: Cardinal): Boolean; Override;
Function EnableWindow(HWnd: HWND; BEnable: Boolean): Boolean; Override;
Function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; override;
Function ExtTextOut(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect; Str: PChar; Count: LongInt; Dx: PInteger): Boolean; Override;
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer; override;

Function FillRect(DC: HDC; Const Rect: TRect; Brush: HBRUSH): Boolean; Override;
{ Draws a 3D border in GTK native style. }
Function Frame3D(DC: HDC; Var Rect: TRect; Const FrameWidth: Integer; Const Style: TBevelCut): Boolean; Override;

Function GetActiveWindow: HWND; Override;
Function GetCapture: HWND; Override;
Function GetCaretPos(Var LPPoint: TPoint): Boolean; Override;
Function GetCharABCWidths(DC: HDC; P2, P3: UINT; Const ABCStructs): Boolean; Override;
Function GetClientRect(Handle: HWND; Var Rect: TRect): Boolean; Override;
Function GetClipBox(DC : hDC; lpRect : PRect) : Longint; Override;
Function GetClipRGN(DC : hDC; RGN : hRGN) : Longint; override;
Function GetDC(HWnd: HWND): HDC; Override;
Function GetFocus: HWND; Override;
Function GetKeyState(NVirtKey: Integer): SmallInt; Override;
Function GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; Override;
Function GetParent(Handle: HWND): HWND; Override;
Function GetProp(Handle: HWND; Str: PChar): Pointer; Override;
Function GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean; Override;
Function GetStockObject(Value: Integer): LongInt; Override;
Function GetSysColor(NIndex: Integer): DWORD; Override;
Function GetSystemMetrics(NIndex: Integer): Integer; Override;
Function GetTextColor(DC: HDC): TColorRef; Override;
Function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; Var Size: TSize): Boolean; Override;
Function GetTextMetrics(DC: HDC; Var TM: TTextMetric): Boolean; Override;
Function GetWindowLong(Handle: HWND; Int: Integer): LongInt; Override;
Function GetWindowOrgEx(DC: HDC; Var P: TPoint): Integer; Override;
Function GetWindowRect(Handle: HWND; Var Rect: TRect): Integer; Override;

{$IFDEF ClientRectBugFix}
Function GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean;
{$ENDIF}

Function HideCaret(HWnd: HWND): Boolean; Override;

function IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer; override;
Function InvalidateRect(AHandle: HWND; Rect: PRect; BErase: Boolean): Boolean; Override;

Function KillTimer (HWnd: HWND; UIDEvent: Cardinal): Boolean; Override;

Function LineTo(DC: HDC; X, Y: Integer): Boolean; Override;

Function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; Override;
Function MessageBox(HWnd: HWND; LPText, LPCaption: PChar; UType: Cardinal): Integer; Override;
Function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; Override;

Function PeekMessage(Var LPMsg: TMsg; Handle: HWND; WMsgFilterMin, WMsgFilterMax, WRemoveMsg: UINT): Boolean; Override;
Function Pie(DC: HDC; X, Y, Width, Height, Angle1, Angle2: Integer): Boolean; Override;
Function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean): Boolean; Override;
Function Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean): Boolean; Override;
Function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): Boolean; Override;
Function PostMessage(Handle: HWND; Msg: Cardinal; WParam: LongInt; LParam: LongInt): Boolean; Override;

function RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; override;
function RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; override;
function RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; override;
Function RealizePalette(DC: HDC): Cardinal; Override;
Function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; Override;
Function ReleaseCapture: Boolean; Override;
Function ReleaseDC(HWnd: HWND; DC: HDC): Integer; Override;
Function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; Override;

Function SaveDC(DC: HDC): Integer; Override;
Function ScreenToClient(Handle: HWND; Var P: TPoint): Integer; Override;
Function ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll, PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean; Override;
Function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; override;
Function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; Override;
Function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; Override;
Function SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: LongInt; LParam: LongInt): Integer; Override;
Function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; Override;
Function SetBkMode(DC: HDC; BkMode: Integer): Integer; Override;
Function SetCapture(Value: LongInt): LongInt; Override;
Function SetCaretPos(X, Y: Integer): Boolean; Override;
Function SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean; Override;
Function SetCaretRespondToFocus(Handle: HWND; ShowHideOnFocus: Boolean): Boolean; Override;
Function SetFocus(HWnd: HWND): HWND; Override;
Function SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean; Override;
Function SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; BRedraw: Boolean): Integer; Override;
Function SetSysColors(CElements: Integer; Const LPAElements; Const LPARgbValues): Boolean; Override;
Function SetTextCharacterExtra(_HDC: HDC; NCharExtra: Integer): Integer; Override;
Function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; Override;
Function SetTimer(HWnd: HWND; NIDEvent, uElapse: Integer; LPTimerFunc: TFNTimerProc): Integer; Override;
Function SetWindowLong(Handle: HWND; Idx: Integer; NewLong: LongInt): LongInt; Override;
Function SetWindowOrgEx(DC: HDC; NewX, NewY: Integer; Var OldPoint: TPoint): Boolean; Override;
Function SetWindowPos(HWnd: HWND; HWndInsertAfter: HWND; X, Y, CX, CY: Integer; UFlags: UINT): Boolean; Override;
Function ShowCaret(HWnd: HWND): Boolean; Override;
Function ShowScrollBar(Handle: HWND; WBar: Integer; BShow: Boolean): Boolean; Override;
Function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; Override;
{ Copies a bitmap from a source rectangle into a destination rectangle using the specified mask and raster operations }
Function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; Override;

Function TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): Boolean; Override;

Function WindowFromPoint(Point: TPoint): HWND; Override;

Procedure InitializeCriticalSection(var CritSection: TCriticalSection); Override;
Procedure EnterCriticalSection(var CritSection: TCriticalSection); Override;
Procedure LeaveCriticalSection(var CritSection: TCriticalSection); Override;
Procedure DeleteCriticalSection(var CritSection: TCriticalSection); Override;

//##apiwiz##eps##   // Do not remove
{ =============================================================================

  $Log$
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
  Keith: Fixed TPage creation

  Revision 1.1  2002/01/06 23:09:53  lazarus
  MG: added missing files


}
