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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove

function Arc(DC: HDC; Left,Top, Right, Bottom, Angle1, Angle2: Integer): Boolean; override;
function AngleChord(DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer): Boolean; override;

function BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc; override;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; override;

procedure CallDefaultWndHandler(Sender: TObject; var Message); override;
function CallNextHookEx(HHk: HHOOK; NCode: Integer; WParam: WParam; LParam: LParam): Integer; override;
function CallWindowProc(LPPrevWndFunc: TFarProc; Handle: HWND; Msg: UINT; WParam: WParam; LParam: LParam): Integer; override;
function ClientToScreen(Handle: HWND; Var P: TPoint): Boolean; override;

// clipboard
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): String; override;
{ Gets data from the clipboard }
function ClipboardGetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat; Stream: TStream): Boolean; override;
// ! List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType; Var Count: Integer; Var List: PClipboardFormat): Boolean; override;
{ Sets the supported formats and requests ownership for the clipboard }
function ClipboardGetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent; FormatCount: Integer; Formats: PClipboardFormat): Boolean; override;
{ Registers a clip board format }
function ClipboardRegisterFormat(const AMimeType: String): TClipboardFormat; override;

function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;  override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: LongInt; BitmapBits: Pointer): HBITMAP; override;
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; override;
function CreateCaret(Handle: HWND; Bitmap: HBITMAP; Width, Height: Integer): Boolean; override;
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; override;
function CreateCompatibleDC(DC: HDC): HDC; override;
function CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP; override;
function CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN; override;
function CreateFontIndirect(const LogFont: TLogFont): HFONT; override;
function CreateIconIndirect(IconInfo: PIconInfo): HICON; override;
function CreatePatternBrush(ABitmap: HBITMAP): HBRUSH; override;
function CreatePenIndirect(const LogPen: TLogPen): HPEN; override;
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN; override;
function CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN; override;

function DeleteDC(HDC: HDC): Boolean; override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean; override;
function DestroyCaret(Handle: HWND): Boolean; override;
function DestroyCursor(Handle: HCURSOR): Boolean; override;
function DestroyIcon(Handle: HICON): Boolean; override;
function DPtoLP(DC: HDC; var Points; Count: Integer): BOOL; override;
function DrawEdge(DC: HDC; var Rect: TRect; Edge: Cardinal; GrfFlags: Cardinal): Boolean; override;
function DrawFrameControl(DC: HDC; const Rect: TRect; UType, UState: Cardinal): Boolean; override;
function DrawFocusRect(DC: HDC; const Rect: TRect): boolean; override;
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; override;

function Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
function EmptyClipBoard: Boolean;
function EnableScrollBar(Wnd: HWND; WSBFlags, WArrows: Cardinal): Boolean; override;
function EnableWindow(HWnd: HWND; BEnable: Boolean): Boolean; override;
function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; override;
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool; override;
function EnumFontFamilies(DC: HDC; Family:Pchar; EnumFontFamProc: FontEnumProc; LParam:Lparam):longint; override;
function EnumFontFamiliesEx(DC:HDC; lpLogFont:PLogFont; Callback: FontEnumExProc; LParam:Lparam; flags:dword):longint; override;
function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; override;
function ExtCreatePen(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN; override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect; Str: PChar; Count: LongInt; Dx: PInteger): Boolean; override;
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer; override;

function FillRect(DC: HDC; Const Rect: TRect; Brush: HBRUSH): Boolean; override;
function FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL; override;
function FloodFill(DC: HDC; X, Y: Integer; Color: TGraphicsColor; FillStyle: TGraphicsFillStyle;   Brush: HBRUSH): Boolean; override;
{ Draws a 3D border in GTK native style. }
function Frame3D(DC: HDC; var ARect: TRect; Const FrameWidth: Integer; Const Style: TBevelCut): Boolean; override;
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH) : integer; override;

function GetActiveWindow: HWND; override;
function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint; override;
function GetBkColor(DC: HDC): TColorRef; override;
function GetCapture: HWND; override;
function GetCaretPos(Var LPPoint: TPoint): Boolean; override;
function GetCharABCWidths(DC: HDC; P2, P3: UINT; Const ABCStructs): Boolean; override;
function GetClientBounds(Handle: HWND; Var Rect: TRect): Boolean; override;
function GetClientRect(Handle: HWND; Var Rect: TRect): Boolean; override;
function GetClipBox(DC : hDC; lpRect : PRect) : Longint; override;
function GetClipRGN(DC : hDC; RGN : hRGN) : Longint; override;
function GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ; override;
function GetCursorPos(var LPPoint: TPoint): Boolean; override;
function GetDC(HWnd: HWND): HDC; override;
function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean; override;
function GetDeviceCaps(DC: HDC; Index: Integer): Integer; override;
function GetDeviceSize(DC: HDC; var P: TPoint): Boolean; override;
function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer; override;
function GetDoubleClickTime: UINT; override;
function GetFocus: HWND; override;
function GetForegroundWindow: HWND; override;
function GetIconInfo(AIcon: HICON; AIconInfo: PIconInfo): Boolean; override;
function GetKeyState(NVirtKey: Integer): SmallInt; override;
function GetMapMode(DC: HDC): Integer; override;
function GetMonitorInfo(hMonitor: HMONITOR; lpmi: LCLType.PMonitorInfo): Boolean; override;
function GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; override;
function GetParent(Handle: HWND): HWND; override;
function GetProp(Handle: HWND; Str: PChar): Pointer; override;
function GetROP2(DC: HDC): Integer; override;
function GetRGNBox(Rgn: HRGN; lpRect : PRect) : Longint; override;
function GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean; override;
function GetStockObject(Value: Integer): THandle; override;
function GetSysColor(NIndex: Integer): DWORD; override;
function GetSysColorBrush(nIndex: Integer): HBrush; override;
function GetSystemMetrics(NIndex: Integer): Integer; override;
function GetTextColor(DC: HDC): TColorRef; override;
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; Var Size: TSize): Boolean; override;
function GetTextMetrics(DC: HDC; Var TM: TTextMetric): Boolean; override;
function GetViewPortExtEx(DC: HDC; Size: PSize): Integer; override;
function GetViewPortOrgEx(DC: HDC; P: PPoint): Integer; override;
function GetWindowExtEx(DC: HDC; Size: PSize): Integer; override;
function GetWindowLong(Handle: HWND; Int: Integer): PtrInt; override;
function GetWindowOrgEx(DC: HDC; P: PPoint): Integer; override;
function GetWindowRect(Handle: HWND; Var Rect: TRect): Integer; override;
function GetWindowRelativePosition(Handle : HWND; var Left, Top:integer): boolean; override;
function GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean; override;
function GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean; override;

function HideCaret(HWnd: HWND): Boolean; override;

function IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer; override;
function InvalidateRect(AHandle: HWND; Rect: PRect; BErase: Boolean): Boolean; override;
function InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean; override;
function IsIconic(handle: HWND): boolean; override;
function IsWindow(handle: HWND): boolean; override;
function IsWindowEnabled(handle: HWND): boolean; override;
function IsWindowVisible(handle: HWND): boolean; override;
function IsZoomed(handle: HWND): boolean; override;

function LineTo(DC: HDC; X, Y: Integer): Boolean; override;
function LPtoDP(DC: HDC; var Points; Count: Integer): BOOL; override;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; override;
function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean; override;
function MessageBox(HWnd: HWND; LPText, LPCaption: PChar; UType: Cardinal): Integer; override;
function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; override;
function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; override;
function MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR; override;
function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; override;

function PeekMessage(Var LPMsg: TMsg; Handle: HWND; WMsgFilterMin, WMsgFilterMax, WRemoveMsg: UINT): Boolean; override;
function Pie(DC: HDC; EllipseX1,EllipseY1,EllipseX2,EllipseY2,
             StartX,StartY,EndX,EndY: Integer): Boolean; override;
function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean): Boolean; override;
function Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean): Boolean; override;
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): Boolean; override;
function PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean; override;
function PtInRegion(Rgn: HRGN; X, Y: Integer): Boolean; override;

function RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey: Integer): Boolean; override;
function RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean; override;
function RealizePalette(DC: HDC): Cardinal; override;
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
function RectVisible(dc : hdc; const ARect: TRect) : Boolean; override;
function RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean; override;
function ReleaseCapture: Boolean; override;
function ReleaseDC(Window: HWND; DC: HDC): Integer; override;
function RemoveProp(Handle: hwnd; Str: PChar): THandle; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; override;
function RoundRect(DC: HDC; X1, Y1, X2, Y2: Integer; RX, RY : Integer): Boolean; override;

function SaveDC(DC: HDC): Integer; override;
function ScreenToClient(Handle: HWND; Var P: TPoint): Integer; override;
function ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll, PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean; override;
function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; override;
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; override;
function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; override;
function SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; override;
function SetActiveWindow(Window: HWND): HWND; override;
function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetBkMode(DC: HDC; BkMode: Integer): Integer; override;
function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; override;
function SetCapture(Value: HWND): HWND; override;
function SetCaretPos(X, Y: Integer): Boolean; override;
function SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean; override;
// SetCaretRespondToFocus is a hack for GTK where sometimes focus seems to be lost, but isn't
//function SetCaretRespondToFocus(Handle: HWND; ShowHideOnFocus: Boolean): Boolean; override;
function SetCursor(hCursor: HICON): HCURSOR; override;
function SetCursorPos(X, Y: Integer): Boolean; override;
function SetFocus(HWnd: HWND): HWND; override;
function SetForegroundWindow(HWnd: HWND): boolean; override;
function SetMapMode(DC: HDC; fnMapMode : Integer): Integer; override;
function SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean; override;
function SetParent(hWndChild: HWND; hWndParent: HWND): HWND; override;
function SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean; override;
function SetROP2(DC: HDC; Mode: Integer): Integer; override;
function SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; BRedraw: Boolean): Integer; override;
function SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer; override;
function SetSysColors(CElements: Integer; Const LPAElements; Const LPARgbValues): Boolean; override;
function SetTextCharacterExtra(_HDC: HDC; NCharExtra: Integer): Integer; override;
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean; override;
function SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean; override;
function SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean; override;
function SetWindowLong(Handle: HWND; Idx: Integer; NewLong: PtrInt): PtrInt; override;
function SetWindowOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean; override;
function SetWindowPos(HWnd: HWND; HWndInsertAfter: HWND; X, Y, CX, CY: Integer; UFlags: UINT): Boolean; override;
function SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: Boolean):longint; override;
function ShowCaret(HWnd: HWND): Boolean; override;
function ShowScrollBar(Handle: HWND; WBar: Integer; BShow: Boolean): Boolean; override;
function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean; override;
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; override;
{ Copies a bitmap from a source rectangle into a destination rectangle using the specified mask and raster operations }
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; override;
function SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool; override;

function TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): Boolean; override;

function UpdateWindow(Handle: HWND): Boolean; override;

function WindowFromPoint(Point: TPoint): HWND; override;

procedure InitializeCriticalSection(var CritSection: TCriticalSection); override;
procedure EnterCriticalSection(var CritSection: TCriticalSection); override;
procedure LeaveCriticalSection(var CritSection: TCriticalSection); override;
procedure DeleteCriticalSection(var CritSection: TCriticalSection); override;

//##apiwiz##eps##   // Do not remove
