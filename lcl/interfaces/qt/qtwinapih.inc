{%MainUnit qtint.pas}

{******************************************************************************
  All QT Winapi related stuff goes here.
  This are the overrides of the QT Interface for the methods defined in the
  lcl/include/winapih.inc

  !! Keep this alphabetical !!

 ******************************************************************************
 Interface
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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc; override;

function ClientToScreen(Handle: HWND; var P: TPoint) : Boolean; override;
function CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;  override;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; override;
function CreateBitmapFromRawImage(const RawImage: TRawImage; var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean; override;
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; override;
function CreateCompatibleDC(DC: HDC): HDC; override;
function CreateCursor(ACursorInfo: PIconInfo): hCursor; override;
function CreateFontIndirect(const LogFont: TLogFont): HFONT; override;
function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;
function CreatePenIndirect(const LogPen: TLogPen): HBRUSH; override;
function CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN; override;

procedure DeleteCriticalSection(var CritSection: TCriticalSection); override;
function DeleteObject(GDIObject: HGDIOBJ): Boolean; override;
function DestroyCursor(Handle: hCursor): Boolean; override;
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; override;

function Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean; override;
function EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer; override;
procedure EnterCriticalSection(var CritSection: TCriticalSection); override;
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; override;
function Frame(DC: HDC; const ARect: TRect): Integer; override;
function Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TBevelCut): Boolean; override;
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer; override;

function GetBitmapRawImageDescription(Bitmap: HBITMAP; Desc: PRawImageDescription): Boolean; override;
function GetClientBounds(handle : HWND; var ARect : TRect) : Boolean; override;
function GetClientRect(handle : HWND; var ARect : TRect) : Boolean; override;
function GetClipBox(DC : hDC; lpRect : PRect) : Longint; override;
function GetClipRGN(DC: hDC; RGN: hRGN): Longint; override;
function GetCursorPos(var lpPoint: TPoint ): Boolean; override;
function GetDeviceRawImageDescription(DC: HDC; Desc: PRawImageDescription): boolean; override;
function GetDeviceSize(DC: HDC; var P: TPoint): Boolean; Override;
function GetDC(hWnd: HWND): HDC; override;
function GetFocus: HWND; override;
function GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; override;
function GetProp(Handle : hwnd; Str : PChar): Pointer; override;
function GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; override;
function GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; Override;
function GetSysColor(nIndex: Integer): DWORD; override;
function GetSystemMetrics(nIndex: Integer): Integer; override;
function GetTextColor(DC: HDC) : TColorRef; Override;
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;  override;
function GetWindowOrgEx(dc : hdc; P : PPoint): Integer; override;
function GetWindowRect(Handle: hwnd; var ARect: TRect): Integer; override;
function GetWindowRelativePosition(Handle: hwnd; var Left, Top: Integer): boolean; override;
function GetWindowSize(Handle: hwnd; var Width, Height: Integer): boolean; override;

function InvalidateRect(aHandle : HWND; Rect : pRect; bErase : Boolean) : Boolean; override;
procedure InitializeCriticalSection(var CritSection: TCriticalSection); override;

procedure LeaveCriticalSection(var CritSection: TCriticalSection); override;
function LineTo(DC: HDC; X, Y: Integer): Boolean; override;

function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; override;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; override;
function ReleaseDC(hWnd: HWND; DC: HDC): Integer; override;
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; override;

function SaveDC(DC: HDC): Integer; override;
function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; override;
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; override;
function SetBKColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetBkMode(DC: HDC; bkMode : Integer) : Integer; override;
function SetCursor(ACursor: HCURSOR): HCURSOR; override;
function SetCursorPos(X, Y: Integer): Boolean; override;
function SetFocus(hWnd: HWND): HWND; override;
function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean; override;
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; override;
function SetWindowOrgEx(DC : HDC; NewX, NewY : Integer; OldPoint: PPoint) : Boolean; override;
function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean; override;
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean; override;
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean; override;
function SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool; override;

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;
function WindowFromPoint(Point: TPoint): HWND; override;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line


