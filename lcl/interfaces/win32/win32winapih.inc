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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove

function Arc(DC: HDC; Left,Top, Right, Bottom, Angle1, Angle2: Integer): Boolean; Override;
function AngleChord(DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer): Boolean; override;

function BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc; Override;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; Override;

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
function ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean; override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: LongInt; BitmapBits: Pointer): HBITMAP; Override;
function CreateBrushIndirect(Const LogBrush: TLogBrush): HBRUSH; Override;
function CreateCaret(Handle: HWND; Bitmap: HBITMAP; Width, Height: Integer): Boolean; Override;
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; Override;
function CreateCompatibleDC(DC: HDC): HDC; Override;
function CreateCursor(ACursorInfo: PIconInfo): hCursor; Override;
function CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP; Override;
function CreateFontIndirect(Const LogFont: TLogFont): HFONT; Override;
function CreatePatternBrush(ABitmap: HBITMAP): HBRUSH; override;
function CreatePenIndirect(Const LogPen: TLogPen): HPEN; Override;
{ Creates a bitmap from raw pixmap data }
function CreatePixmapIndirect(Const Data: Pointer; Const TransColor: LongInt): HBITMAP; Override;
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN; Override;
function CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN; Override;

function DeleteDC(HDC: HDC): Boolean; Override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean; Override;
function DestroyCaret(Handle: HWND): Boolean; Override;
function DestroyCursor(Handle: hCursor): Boolean; Override;
function DrawFrameControl(DC: HDC; const Rect: TRect; UType, UState: Cardinal): Boolean; Override;
function DrawFocusRect(DC: HDC; const Rect: TRect): boolean; override;
function DrawEdge(DC: HDC; Var Rect: TRect; Edge: Cardinal; GrfFlags: Cardinal): Boolean; Override;
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; Override;

function Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; Override;
function EmptyClipBoard: Boolean;
function EnableScrollBar(Wnd: HWND; WSBFlags, WArrows: Cardinal): Boolean; Override;
function EnableWindow(HWnd: HWND; BEnable: Boolean): Boolean; Override;
function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; Override;
function EnumFontFamilies(DC: HDC; Family:Pchar; EnumFontFamProc: FontEnumProc; LParam:Lparam):longint; override;
function EnumFontFamiliesEx(DC:HDC; lpLogFont:PLogFont; Callback: FontEnumExProc; LParam:Lparam; flags:dword):longint; override;
function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect; Str: PChar; Count: LongInt; Dx: PInteger): Boolean; Override;
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer; override;

function FillRect(DC: HDC; Const Rect: TRect; Brush: HBRUSH): Boolean; Override;
function FloodFill(DC: HDC; X, Y: Integer; Color: TGraphicsColor; FillStyle: TGraphicsFillStyle;   Brush: HBRUSH): Boolean; override;
{ Draws a 3D border in GTK native style. }
function Frame3D(DC: HDC; var Rect: TRect; Const FrameWidth: Integer; Const Style: TBevelCut): Boolean; Override;
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH) : integer; override;

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
function GetForegroundWindow: HWND; override;
function GetKeyState(NVirtKey: Integer): SmallInt; Override;
function GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; Override;
function GetParent(Handle: HWND): HWND; Override;
function GetProp(Handle: HWND; Str: PChar): Pointer; Override;
function GetROP2(DC: HDC): Integer; override;
function GetRGNBox(Rgn: HRGN; lpRect : PRect) : Longint; override;
function GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean; Override;
function GetStockObject(Value: Integer): THandle; Override;
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
function InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean; override;
function IsWindowEnabled(handle: HWND): boolean; override;
function IsWindowVisible(handle: HWND): boolean; override;

function LineTo(DC: HDC; X, Y: Integer): Boolean; Override;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; Override;
function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean; Override;
function MessageBox(HWnd: HWND; LPText, LPCaption: PChar; UType: Cardinal): Integer; Override;
function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; Override;

function PeekMessage(Var LPMsg: TMsg; Handle: HWND; WMsgFilterMin, WMsgFilterMax, WRemoveMsg: UINT): Boolean; Override;
function Pie(DC: HDC; EllipseX1,EllipseY1,EllipseX2,EllipseY2,
             StartX,StartY,EndX,EndY: Integer): Boolean; override;
function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean): Boolean; Override;
function Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean): Boolean; Override;
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): Boolean; Override;
function PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean; Override;
function PtInRegion(Rgn: HRGN; X, Y: Integer): Boolean; override;

function RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey: Integer): Boolean; override;
function RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean; override;
function RealizePalette(DC: HDC): Cardinal; Override;
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; Override;
function RectVisible(dc : hdc; const ARect: TRect) : Boolean; Override;
function ReleaseCapture: Boolean; Override;
function ReleaseDC(Window: HWND; DC: HDC): Integer; Override;
function ReleaseDesignerDC(Window: HWND; DC: HDC): Integer; Override;
function RemoveProp(Handle: hwnd; Str: PChar): THandle; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; Override;
function RoundRect(DC: HDC; X1, Y1, X2, Y2: Integer; RX, RY : Integer): Boolean; override;

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
// SetCaretRespondToFocus is a hack for GTK where sometimes focus seems to be lost, but isn't
//function SetCaretRespondToFocus(Handle: HWND; ShowHideOnFocus: Boolean): Boolean; Override;
function SetCursor(hCursor: HICON): HCURSOR; override;
function SetCursorPos(X, Y: Integer): Boolean; override;
function SetFocus(HWnd: HWND): HWND; Override;
function SetForegroundWindow(HWnd: HWND): boolean; override;
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
function SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool; override;

function TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): Boolean; Override;

function WindowFromPoint(Point: TPoint): HWND; Override;

procedure InitializeCriticalSection(var CritSection: TCriticalSection); Override;
procedure EnterCriticalSection(var CritSection: TCriticalSection); Override;
procedure LeaveCriticalSection(var CritSection: TCriticalSection); Override;
procedure DeleteCriticalSection(var CritSection: TCriticalSection); Override;

//##apiwiz##eps##   // Do not remove
