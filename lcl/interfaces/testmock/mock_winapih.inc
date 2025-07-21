{%MainUnit testmockint.pp}

(******************************************************************************
  All Winapi related MOCKING goes here.

  This file is based on winapih.inc

  !! Keep this alphabetical and place a pointer to the independent part
  !! if needed

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 ******************************************************************************)

{******************************************************************************
  Platform specific stuff

  Note:
    the IFNDEF IF_BASE_MEMBER section is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function Arc(DC: HDC; Left,Top,Right,Bottom,Angle16Deg, Angle16DegLength: Integer): Boolean; override;
function AngleChord(DC: HDC; x1,y1,x2,y2,angle1,angle2 : Integer): Boolean; override;

function BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc; override;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; override;

function CallNextHookEx(hhk : HHOOK; ncode : Integer; WParam: WParam; LParam: LParam) : Integer; override;
function CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; WParam: WParam; LParam: LParam): Integer; override;
//function CharLower --> independent
//function CharLowerBuff --> independent
//function CharUpper --> independent
function ClientToScreen(Handle : HWND; var P : TPoint) : Boolean; override;
function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint; override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; override;
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; override;
function CreateBrushWithRadialGradient(const LogBrush: TLogRadialGradient): HBRUSH; override;
function CreateCaret(Handle: HWND; Bitmap: hBitmap; width, Height: Integer): Boolean; override;
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; override;
function CreateCompatibleDC(DC: HDC): HDC; override;
function CreateDIBitmap(DC: HDC; var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo;
  wUsage: UINT): HBITMAP; override;
function CreateDIBSection(DC: HDC; const BitmapInfo: tagBitmapInfo; Usage: UINT;
  var Bits: Pointer; SectionHandle: TLCLHandle; Offset: DWORD): HBITMAP; override;
function CreateEllipticRgn(X1, Y1, X2, Y2: Integer): HRGN; override;
//function CreateEllipticRgnIndirect --> independent
//function CreateFont --> independent
function CreateFontIndirect(const LogFont: TLogFont): HFONT; override;
function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;
function CreateIconIndirect(IconInfo: PIconInfo): HICON; override;
function CreatePalette(const LogPalette: TLogPalette): HPalette; override;
function CreatePatternBrush(ABitmap: HBITMAP): HBRUSH; override;
//function CreatePen --> independent
function CreatePenIndirect(const LogPen: TLogPen): HPEN; override;
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN; override;
function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN; override;
//function CreateRectRgnIndirect --> independent
function CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse, nHeightEllipse: Integer): HRGN; override;

procedure DeleteCriticalSection(var CritSection: TCriticalSection); override;
function DeleteDC(hDC: HDC): Boolean; override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean; override;
function DestroyCaret(Handle : HWND): Boolean; override;
function DestroyCursor(Handle: HCURSOR): Boolean; override;
function DestroyIcon(Handle: HICON): Boolean; override;

function DPtoLP(DC: HDC; var Points; Count: Integer): BOOL; override;
function DrawFrameControl(DC: HDC; const Rect : TRect; uType, uState : Cardinal) : Boolean; override;
function DrawFocusRect(DC: HDC; const Rect: TRect): Boolean; override;
function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean; override;
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; override;

function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean; override;
function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean; override;
function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; override;
procedure EnterCriticalSection(var CritSection: TCriticalSection); override;
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool; override;
function EnumFontFamilies(DC: HDC; Family: Pchar; EnumFontFamProc: FontEnumProc; LParam:Lparam):longint; override;
function EnumFontFamiliesEx(DC: HDC; lpLogFont:PLogFont; Callback: FontEnumExProc; Lparam:LParam; Flags: dword): longint; override;

function Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean; override;
//function EqualRect --> independent
function EqualRgn(Rgn1: HRGN; Rgn2: HRGN): Boolean; override;
function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; override;
function ExtCreatePen(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN; override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  override;
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;  override;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; override;
function FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL; override;
function FloodFill(DC: HDC; X, Y: Integer;
                   Color: TGraphicsColor;
                   FillStyle: TGraphicsFillStyle;
                   Brush: HBRUSH): Boolean; override;
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer; override;

function GetActiveWindow : HWND; override;
function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint; override;
function GetBkColor(DC: HDC): TColorRef; override;
function GetCapture : HWND; override;
function GetCaretPos(var lpPoint: TPoint): Boolean; override;
function GetClientRect(handle : HWND; var Rect: TRect) : Boolean; override;
function GetClipBox(DC : hDC; lpRect : PRect) : Longint; override;
function GetClipRGN(DC : hDC; RGN : hRGN) : Longint; override;
function GetCompositorExtendedBorder(handle : HWND; var Borders: TRect) : Boolean; override;
function GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ; override;
function GetCursorPos(var lpPoint: TPoint): Boolean; override;
function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean; override; //pbd
function GetDC(hWnd: HWND): HDC; override;
function GetDeviceCaps(DC: HDC; Index: Integer): Integer; override;
function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;override;
function GetDoubleClickTime: UINT; override;
function GetFocus: HWND; override;
function GetFontLanguageInfo(DC: HDC): DWord; override;
function GetForegroundWindow: HWND; override;
function GetIconInfo(AIcon: HICON; AIconInfo: PIconInfo): Boolean; override;
function GetKeyState(nVirtKey: Integer): Smallint; override;
function GetMapMode(DC: HDC): Integer; override;
function GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean; override;
function GetDpiForMonitor(hmonitor: HMONITOR; dpiType: TMonitorDpiType; out dpiX: UINT; out dpiY: UINT): HRESULT; override;
function GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; override;
function GetPaletteEntries(Palette: HPALETTE; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT; override;
function GetParent(Handle : HWND): HWND; override;
function GetProp(Handle : hwnd; Str : PChar): Pointer;override;
function GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint; override;
function GetROP2(DC: HDC): integer; override;
function GetScrollInfo(Handle: HWND; SBStyle: Integer; var ScrollInfo: TScrollInfo): Boolean; override;
//function GetScrollPos --> independent
//function GetScrollRange --> independent
function GetStockObject(Value: Integer): TLCLHandle; override;
function GetSysColor(nIndex: Integer): DWORD; override;
function GetSysColorBrush(nIndex: Integer): HBrush; override;
function GetSystemPaletteEntries(DC: HDC; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT; override;
function GetSystemMetrics(nIndex: Integer): Integer;  override;
function GetTextColor(DC: HDC) : TColorRef; override;
function GetTextExtentExPoint(DC: HDC; Str: PChar; Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger; var Size: TSize): Boolean; override;
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
function GetTextExtentPoint32(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;  override;
function GetViewPortExtEx(DC: HDC; Size: PSize): Integer; override;
function GetViewPortOrgEx(DC: HDC; P: PPoint): Integer; override;
function GetWindowExtEx(DC: HDC; Size: PSize): Integer; override;
function GetWindowLong(Handle : hwnd; int : Integer): PtrInt; override;
function GetWindowRect(Handle : hwnd; var Rect : TRect): Integer; override;
function GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean; override;
function GetWindowOrgEx(dc : hdc; var P: TPoint): Integer; override; // because of delphi compatibility
function GetWindowOrgEx(dc : hdc; P : PPoint): Integer; override; // an old declaration
function GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean; override;

function HideCaret(hWnd: HWND): Boolean; override;

//function IndexToOverlayMask --> independent
//function InflateRect --> independent
procedure InitializeCriticalSection(var CritSection: TCriticalSection); override;
function IntersectClipRect(dc: hdc; Left, Top, Right,Bottom: Integer): Integer; override;
//function IntersectRect --> independent
function InvalidateRect(aHandle : HWND; ARect : pRect; bErase : Boolean) : Boolean; override;
function InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean; override;
//function IsCharAlphaNumeric --> independent
function IsDBCSLeadByte(TestChar: Byte): boolean; override;
//function IsRectEmpty --> independent
function IsIconic(handle: HWND): boolean; override;
function IsWindow(handle: HWND): boolean; override;
function IsWindowEnabled(handle: HWND): boolean; override;
function IsWindowVisible(handle: HWND): boolean; override;
function IsZoomed(handle: HWND): boolean; override;

//function KillTimer --> independent

procedure LeaveCriticalSection(var CritSection: TCriticalSection); override;
function LineTo(DC: HDC; X, Y: Integer): Boolean; override;
function LoadBitmap(hInstance: TLCLHandle; lpBitmapName: PChar): HBitmap; override;
function LoadCursor(hInstance: TLCLHandle; lpCursorName: PChar): HCursor; override;
function LoadIcon(hInstance: TLCLHandle; lpIconName: PChar): HIcon; override;
function LPtoDP(DC: HDC; var Points; Count: Integer): BOOL; override;

//function MakeLong --> independent
//function MakeROP4 --> independent
//function MakeWord --> independent
function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; override;
function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean; override;
function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal = MB_OK): integer; override;
function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; override;
function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; override;
function MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR; override;

function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; override;

//function OffsetRect --> independent
function OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer; override;

function PaintRgn(DC: HDC; RGN: HRGN): Boolean; override;
function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean; override;
function Pie(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean; override;
function PolyBezier(DC: HDC; Points: PPoint;  NumPts: Integer; Filled, Continuous: boolean): boolean; override;
function Polygon(DC: HDC; Points: PPoint;  NumPts: Integer; Winding: boolean): boolean; override;
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean; override;
function PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean; override;
function PtInRegion(RGN: HRGN; X, Y: Integer): Boolean; override;
//function PtInRect --> independent

function RealizePalette(DC: HDC): Cardinal; override;
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
function RectInRegion(RGN: HRGN; ARect: TRect): Boolean; override;
function RectVisible(DC: HDC; const ARect: TRect): Boolean; override;
function RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean; override;
function ReleaseCapture : Boolean; override;
function ReleaseDC(hWnd: HWND; DC: HDC): Integer; override;
function RemoveProp(Handle: HWND; Str: PChar): TLCLHandle; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; override;
function RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean; override;

function SaveDC(DC: HDC): Integer; override;
function ScreenToClient(Handle : HWND; var P : TPoint) : Integer; override;
//function ScrollWindow --> independent deprecated calls ScrollWindowEx
function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; override;
function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; override;
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; override;
function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; override;
function SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; override;
function SetActiveWindow(Handle: HWND): HWND;  override;
function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; override; //pbd
function SetBkMode(DC: HDC; bkMode : Integer): Integer; override;
function SetCapture (AHandle: HWND): HWND; override;
function SetCaretPos(X, Y: Integer): Boolean; override;
function SetCaretPosEx(handle: HWnd; X, Y: Integer): Boolean; override;
function SetCursor(hCursor: HICON): HCURSOR; override;
function SetCursorPos(X, Y: Integer): Boolean; override;
function SetFocus(hWnd: HWND): HWND; override;
function SetForegroundWindow(hWnd : HWND): Boolean; override;
function SetMapMode(DC: HDC; fnMapMode : Integer): Integer; override;
function SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean; override;
function SetParent(hWndChild: HWND; hWndParent: HWND): HWND; override;
function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean; override;
//function SetRect --> independent
function SetRectRgn(aRGN: HRGN; X1, Y1, X2, Y2 : Integer): Boolean; override;
//function SetRectEmpty --> independent
function SetROP2(DC: HDC; Mode: Integer): Integer; override;
function SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; Redraw : Boolean): Integer; override;
//function SetScrollPos --> independent
//function SetScrollRange --> independent
function SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer; override;
function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; override;
function SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer; override;
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; override;
//function SetTimer --> independent
function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : PtrInt): PtrInt;override;
function SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean; override;
function SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean; override;
function SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean; override;
function SetWindowOrgEx(dc : hdc; NewX, NewY: Integer; OldPoint: PPoint): Boolean; override;
function SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
       X, Y, cx, cy: Integer; uFlags: UINT): Boolean;  override;
function SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: Boolean): longint; override;
function ShowCaret(hWnd: HWND): Boolean; override;
function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean; override;
function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean; override;
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; override;
function StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHeight, SrcX,
  SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; var BitsInfo: TBitmapInfo;
  Usage: UINT; Rop: DWORD): Integer; override;
//function SubtractRect --> independent
function SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool; override;

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;

function UpdateWindow(Handle: HWND): Boolean; override;

//function UnionRect --> independent

function WindowFromPoint(Point : TPoint) : HWND; override;
//##apiwiz##eps##     // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

//{$IFNDEF IF_BASE_MEMBER}
//
////##apiwiz##spi##    // Do not remove
//
//function AdjustWindowRectEx(Var Rect: TRect; Style1: Word; MenuExist : Boolean;
//  Style2 : Word) : Boolean;
//
//function CharLower(c: char): char; inline;
//function CharLowerBuff(pStr : PChar; Len : Integer): Integer;
//function CharUpper(c: char): char; inline;
//function CharUpperBuff(pStr : PChar; Len : Integer): Integer;
//function CopyRect(var DestRect: TRect; const SrcRect: TRect): Boolean;
//function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
//function CreateFont(Height, Width, Escapement, Orientation, Weight: Integer;
//  Italic, Underline, StrikeOut, CharSet, OutputPrecision, ClipPrecision,
//  Quality, PitchAndFamily: Byte; FaceName: PChar): HFONT;
//function CreatePen(Style, Width: Integer; Color: TColorRef): HPEN;
//function CreateRectRgnIndirect(const ARect: TRect): HRGN;
//function CreateSolidBrush(Color: TColorRef): HBRUSH;
//
//function EqualRect(const lprc1, lprc2: TRect): Boolean;
//
//function GetScrollPos(Handle: HWND; nBar: Integer): Integer;
//function GetScrollRange(Handle: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): Boolean;
//function GetRValue(RGB : DWORD) : BYTE; inline;
//function GetGValue(RGB : DWORD) : BYTE; inline;
//function GetBValue(RGB : DWORD) : BYTE; inline;
//procedure GetRGBValues(RGB : DWORD; out R, G, B: Byte); inline;
//procedure GetRGBIntValues(RGB : DWORD; out R, G, B: integer); inline;
//
//function IndexToOverlayMask(iOverlay: UINT): UINT; inline;
//function InflateRect(var ARect: TRect; dx, dy: Integer): Boolean;
//function IntersectRect(var DestRect: TRect; const SrcRect1, SrcRect2: TRect): Boolean; inline;
//function IsCharAlphaNumeric(c : Char) : Boolean;
//function IsRectEmpty(const ARect: TRect): Boolean;
//
//function KillTimer(Wnd: HWND; uIDEvent: UINT_PTR): BOOL;
//
//function MakeLong(A,B: Word): DWORD; inline;
//function MakeROP4(fore, back: DWORD): DWORD;inline;
//function MakeWord(A,B: Byte): Word; inline;
//function MakeWParam(l, h: Word): WPARAM; inline;
//function MakeLParam(l, h: Word): LPARAM; inline;
//function MakeLResult(l, h: Word): LRESULT; inline;
//
//procedure LParamExtractLoHiWord(const aParam: LPARAM; out LoWord, HiWord: Word); inline;
//function LParamHiWord(const aParam: LPARAM): Word; inline;
//function LParamLoWord(const aParam: LPARAM): Word; inline;
//
//function PtInRect(const Rect : TRect; const Point : TPoint) : Boolean; inline;
//function PointToSmallPoint(const P : TPoint) : TSmallPoint; inline;
//function PointToSmallPointNoChecks(const P : TPoint) : TSmallPoint; inline;
//
//function RGB(R, G, B : Byte) : TColorRef; inline;
//
//function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer; Rect, ClipRect: PRect): Boolean; inline;
//function SetRect(var ARect : TRect; aLeft,aTop,aRight,aBottom : Integer) : Boolean;
//function SetRectEmpty(var ARect: TRect): Boolean;
//function SetScrollPos(Handle: HWND; nBar, nPos: Integer; bRedraw: Boolean): Integer;
//function SetScrollRange(Handle: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: Boolean): Boolean;
//function SetTimer(Wnd: HWND; nIDEvent: UINT_PTR; uElapse: UINT;  lpTimerFunc: TTimerProc): UINT_PTR;
//function SmallPointToPoint(const P : TSmallPoint) : Tpoint;
//function SubtractRect(out lprcDst: TRect; const lprcSrc1, lprcSrc2: TRect): Boolean;
//
//function UnionRect(var DestRect: TRect; const SrcRect1, SrcRect2: TRect): Boolean; //pbd
//
//
////##apiwiz##epi##    // Do not remove
//
//
//{$ENDIF}
//
//

