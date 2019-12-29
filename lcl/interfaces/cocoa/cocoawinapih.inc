{%MainUnit cocoaint.pas}

{******************************************************************************
  All Cocoa Winapi related stuff goes here.
  This are the overrides of the Cocoa Interface for the methods defined in the
  lcl/include/winapih.inc

  !! Keep this alphabetical !!

 ******************************************************************************
 Interface
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

function Arc(DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer): Boolean; override;
function AngleChord(DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer): Boolean; override;

function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc; override;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; override;

{function CallNextHookEx(hHk: HHOOK; ncode : Integer; wParam: WParam; lParam : LParam) : Integer; override;
function CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; wParam: WParam; lParam : lParam) : Integer; override;}
procedure CallDefaultWndHandler(Sender: TObject; var Message); override;
function ClientToScreen(Handle: HWND; var P: TPoint) : Boolean; override;

function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; override;
function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean; override;
// ! List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean; override;
function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean; override;
function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; override;
function ClipboardFormatNeedsNullByte(const AFormat: TPredefinedClipboardFormat): Boolean; override;

function CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;  override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; override;
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; override;
function CreateCaret(Handle : HWND; Bitmap : hBitmap; Width, Height : Integer) : Boolean; override;
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; override;
function CreateCompatibleDC(DC: HDC): HDC; override;
{function CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN; override;}
function CreateFontIndirect(const LogFont: TLogFont): HFONT; override;
function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;
function CreateIconIndirect(IconInfo: PIconInfo): HICON; override;
function CreatePenIndirect(const LogPen: TLogPen): HPEN; override;
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN; override;
function CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN; override;
procedure DeleteCriticalSection(var CritSection: TCriticalSection); override;
function DeleteDC(hDC: HDC): Boolean; override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean; override;
function DestroyCaret(Handle : HWND): Boolean; override;
function DestroyIcon(Handle: HICON): Boolean; override;
function DPtoLP(DC: HDC; var Points; Count: Integer): BOOL; override;
function DrawFocusRect(DC: HDC; const Rect: TRect): boolean; override;
function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean; override;

function Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean; override;
{function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean; override;}
function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean; override;
function EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer; override;
procedure EnterCriticalSection(var CritSection: TCriticalSection); override;
function EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont; Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint; override;
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool; override;
function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; override;
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint): Integer;  override;
function ExtCreatePen(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN; override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; override;
function FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): Bool; override;
function Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TBevelCut): Boolean; override;
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer; override;

function GetActiveWindow: HWND; override;
{function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint; override;}
function GetBkColor(DC: HDC): TColorRef; override;
function GetCapture: HWND; override;
function GetCaretPos(var lpPoint: TPoint): Boolean; override;
function GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean; override;
function GetClientBounds(handle : HWND; var ARect : TRect) : Boolean; override;
function GetClientRect(handle : HWND; var ARect : TRect) : Boolean; override;
function GetClipBox(DC : hDC; lpRect : PRect) : Longint; override;
function GetClipRGN(DC: hDC; RGN: hRGN): Longint; override;
function GetCursorPos(var lpPoint: TPoint ): Boolean; override;
function GetDC(hWnd: HWND): HDC; override;
function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean; override;
function GetDeviceCaps(DC: HDC; Index: Integer): Integer; override;
function GetDeviceSize(DC: HDC; var P: TPoint): Boolean; override;
{function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer; override;}
function GetFocus: HWND; override;
function GetForegroundWindow: HWND; override;
function GetKeyState(nVirtKey: Integer): Smallint; override;
function GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean; override;
function GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; override;
function GetParent(Handle: HWND): HWND; override;
function GetProp(Handle: hwnd; Str: PChar): Pointer; override;
function GetRgnBox(RGN: HRGN; lpRect: PRect) : Longint; override;
function GetROP2(DC: HDC): Integer; override;
function GetScrollBarSize(Handle: HWND; BarKind: Integer): integer; override;
function GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean; override;
function GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean; override;
function GetStockObject(Value: Integer): THandle; override;
function GetSysColor(nIndex: Integer): DWORD; override;
function GetSysColorBrush(nIndex: Integer): HBRUSH; override;
function GetSystemMetrics(nIndex: Integer): Integer; override;
function GetTextColor(DC: HDC) : TColorRef; Override;
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;  override;
function GetViewPortOrgEx(DC: HDC; P: PPoint): Integer; override;
{function GetWindowLong(Handle : hwnd; int: Integer): PtrInt; override;}
function GetWindowOrgEx(dc : hdc; P : PPoint): Integer; override;
function GetWindowRect(Handle: hwnd; var ARect: TRect): Integer; override;
function GetWindowRelativePosition(Handle: hwnd; var Left, Top: Integer): boolean; override;
function GetWindowSize(Handle: hwnd; var Width, Height: Integer): boolean; override;

function HideCaret(Handle: HWND): Boolean; override;

function InvalidateRect(aHandle : HWND; Rect : pRect; bErase : Boolean) : Boolean; override;
procedure InitializeCriticalSection(var CritSection: TCriticalSection); override;
function InitStockFont(AFont: TObject; AStockFont: TStockFont): Boolean; override;
function IsWindowEnabled(Handle: HWND): boolean; override;
function IsWindowVisible(Handle: HWND): boolean; override;
function IsWindow(handle: HWND): boolean; override;

procedure LeaveCriticalSection(var CritSection: TCriticalSection); override;
function LineTo(DC: HDC; X, Y: Integer): Boolean; override;
function LPtoDP(DC: HDC; var Points; Count: Integer): BOOL; override;

function OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer; override;

{function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal): integer; override;}
function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; override;

{function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean; override;
function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer; Filled, Continuous: boolean): boolean; override;}
function Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: boolean): boolean; override;
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean; override;
function PostMessage(Handle: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): Boolean; override;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
function RectVisible(dc : hdc; const ARect: TRect) : Boolean; override;
function ReleaseCapture : Boolean; override;
function ReleaseDC(hWnd: HWND; DC: HDC): Integer; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; override;
function RoundRect(DC: HDC; X1, Y1, X2, Y2: Integer; RX, RY : Integer): Boolean; override;

function SaveDC(DC: HDC): Integer; override;
function ScreenToClient(Handle: HWND; var P: TPoint): Integer; override;
function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; override;
function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; override;
function SelectObject(ADC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; override;
function SendMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; override;
function SetActiveWindow(Handle: HWND): HWND; override;
function SetBKColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetBkMode(DC: HDC; bkMode : Integer) : Integer; override;
function SetCapture(AHandle: HWND): HWND; override;
function SetCaretPos(X, Y: Integer): Boolean; override;
function SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean; override;
function SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean; override;
function SetCursor(ACursor: HCURSOR): HCURSOR; override;
function SetCursorPos(X, Y: Integer): Boolean; override;
function SetFocus(Handle: HWND): HWND; override;
function SetForegroundWindow(HWnd: HWND): boolean; override;
function SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean; override;
function SetProp(Handle: hwnd; Str: PChar; Data: Pointer) : Boolean; override;
function SetROP2(DC: HDC; Mode: Integer): Integer; override;
function SetScrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer; override;
function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; override;
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean; override;
//function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : PtrInt): PtrInt; override;
function SetWindowOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint) : Boolean; override;
function ShowCaret(Handle: HWND): Boolean; override;
function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean; override;
function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean; override;
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean; override;
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean; override;
function SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool; override;

function TextOut(DC: HDC; X,Y: Integer; Str: Pchar; Count: Integer) : Boolean; override;
function UpdateWindow(Handle: HWND): Boolean; override;
function WindowFromPoint(Point: TPoint): HWND; override;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line


