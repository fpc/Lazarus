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

 ******************************************************************************)

{******************************************************************************
  Platform specific stuff
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove
 
function Arc(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function AngleChord(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function BringWindowToTop(hWnd : HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; wParam ,lParam : LongInt) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CharLowerBuff --> independent
function CheckMenuItem(hndMenu: HMENU; uIDEnableItem: Integer;
  bChecked: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

// the clipboard functions are internally used by TClipboard
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
// ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

Function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCaret(Handle: HWND; Bitmap: hBitmap; width, Height: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateCompatibleDC(DC: HDC): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreateFont --> independent
function CreateFontIndirect(const LogFont: TLogFont): HFONT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreatePen --> independent
function CreatePenIndirect(const LogPen: TLogPen): HPEN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePolygonRgn(Points: PPoint; NumPts: Integer; Winding : Boolean): HRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function DeleteDC(hDC: HDC): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DeleteObject(GDIObject: HGDIOBJ): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DestroyCaret(Handle : HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function DrawFrameControl(DC: HDC; var Rect : TRect; uType, uState : Cardinal) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function EnableMenuItem(hndMenu: HMENU; uIDEnableItem: Integer; bEnable: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function EqualRect --> independent
Function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
{$IFNDEF IF_BASE_MEMBER} function ExcludeClipRgn(dc: hdc; rgn : hrgn) : Integer; {$ENDIF}
function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function FloodFill(DC: HDC; X, Y: Integer; Color: TColor; FillStyle: TFillStyle; Brush: HBRUSH): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Frame3d(DC: HDC; var Rect: TRect; const FrameWidth : integer; const Style : TBevelCut): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

Function GetActiveWindow : HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCapture : HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCaretPos(var lpPoint: TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetClientBounds(handle : HWND; var Rect: TRect) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetClientRect(handle : HWND; var Rect: TRect) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetClipBox(DC : hDC; lpRect : PRect) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetClipRGN(DC : hDC; RGN : hRGN) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetCmdLineParamDescForInterface: string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCursorPos(var lpPoint: TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} //pbd
function GetDC(hWnd: HWND): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDeviceCaps(DC: HDC; Index: Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDeviceSize(DC: HDC; var p: TPoint): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetFocus: HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetKeyState(nVirtKey: Integer): Smallint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetParent(Handle : HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetProp(Handle : hwnd; Str : PChar): Pointer;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetScrollPos --> independent
//function GetScrollRange --> independent
function GetStockObject(Value: Integer): LongInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSysColor(nIndex: Integer): DWORD; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetSystemMetrics(nIndex: Integer): Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextColor(DC: HDC) : TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowLong(Handle : hwnd; int : Integer): Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowRect(Handle : hwnd; var Rect : TRect): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowOrgEx(dc : hdc; var P : TPoint): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function HideCaret(hWnd: HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function InflateRect --> independent
function IntersectClipRect(dc: hdc; Left, Top, Right,Bottom: Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function IntersectRect --> independent
Function InvalidateRect(aHandle : HWND; Rect : pRect; bErase : Boolean) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function IsCharAlphaNumeric --> independent
//function IsRectEmpty --> independent

function KillTimer (hWnd : HWND; uIDEvent : cardinal) : boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function LineTo(DC: HDC; X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function LoadStockPixmap(StockID: longint) : HBitmap; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function OffsetRect --> independent

function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Pie(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PolyBezier(DC: HDC; Points: PPoint;  NumPts: Integer; Filled, Continuous: boolean): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Polygon(DC: HDC; Points: PPoint;  NumPts: Integer; Winding: boolean): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PostMessage(Handle: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function PtInRect --> independent

function RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RealizePalette(DC: HDC): Cardinal; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RectVisible(dc : hdc; ARect: TRect) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseCapture : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseDC(hWnd: HWND; DC: HDC): Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RestoreDC(DC: HDC; SavedDC: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RightJustifyMenuItem(HndMenu: HMenu; bRightJustify: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function SaveDC(DC: HDC): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function ScreenToClient(Handle : HWND; var P : TPoint) : Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function ScrollWindow --> independent deprecated calls ScrollWindowEx
function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SendMessage(HandleWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetActiveWindow(Handle: HWND): HWND;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetBkColor(DC: HDC; Color: TColorRef): TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF} //pbd
Function SetBkMode(DC: HDC; bkMode : Integer): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function SetCapture (Value : LongInt): LongInt; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretPos(X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretPosEx(handle: HWnd; X, Y: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetFocus(hWnd: HWND): HWND; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetRect --> independent
//function SetRectEmpty --> independent
function SetScrollInfo(Handle: HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; Redraw : Boolean): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetScrollPos --> independent
//function SetScrollRange --> independent
function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetTextColor(DC: HDC; Color: TColorRef): TColorRef; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetTimer(hWnd: HWND; nIDEvent, uElapse: integer; lpTimerFunc: TFNTimerProc) : integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : Longint): Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowOrgEx(dc : hdc; NewX, NewY : Integer; var OldPoint: TPoint): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
       X, Y, cx, cy: Integer; uFlags: UINT): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowCaret(hWnd: HWND): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

{$IFNDEF IF_BASE_MEMBER}
Procedure NotifyUser(const DialogMessage : String; DialogType : longint);
Procedure NotifyUserAtXY(const DialogMessage : String; DialogType : longint; X, Y : Longint);{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

Procedure NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint);
Procedure NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint);{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

Function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
{$EndIf}

Function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

Function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

Function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

Procedure InitializeCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Procedure EnterCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Procedure LeaveCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
Procedure DeleteCriticalSection(var CritSection: TCriticalSection); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

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

function AdjustWindowRectEx(Var Rect: TRect; Style1: Word; MenuExist : Boolean;
  Style2 : Word) : Boolean;

Function BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc;

Function CharLowerBuff(pStr : PChar; Len : Integer): Integer;
function CopyRect(var DestRect: TRect; const SrcRect: TRect): Boolean;
function CreateFont(Height, Width, Escapement, Orientation, Weight: Integer;
  Italic, Underline, StrikeOut, CharSet, OutputPrecision, ClipPrecision,
  Quality, PitchAndFamily: Cardinal; FaceName: PChar): HFONT;
function CreatePen(Style, Width: Integer; Color: TColorRef): HPEN;

Function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
function EqualRect(const lprc1, lprc2: TRect): Boolean;

function GetScrollPos(Handle: HWND; nBar: Integer): Integer;
function GetScrollRange(Handle: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): Boolean;

function InflateRect(var ARect: TRect; dx, dy: Integer): Boolean;
function IntersectRect(var DestRect: TRect; const SrcRect1, SrcRect2: TRect): Boolean;
Function IsCharAlphaNumeric(c : Char) : Boolean;
function IsRectEmpty(const ARect: TRect): Boolean;

function OffSetRect(var Rect: TRect; dx,dy: Integer): Boolean;

function MakeWParam(l, h: Word): WPARAM;
function MakeLParam(l, h: Word): LPARAM;
function MakeLResult(l, h: Word): LRESULT;

Function PtInRect(Rect : TRect; Point : TPoint) : Boolean;
Function PointtoSmallPoint(const P : TPoint) : TSmallPoint;

function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer; Rect, ClipRect: PRect): Boolean;
function SetRect(Var ARect : TRect; xLeft,yTop,xRight,yBottom : Integer) : Boolean;
function SetRectEmpty(var ARect: TRect): Boolean;
function SetScrollPos(Handle: HWND; nBar, nPos: Integer; bRedraw: Boolean): Integer;
function SetScrollRange(Handle: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: Boolean): Boolean;
Function SmallPointToPoint(const P : TSmallPoint) : Tpoint;

function UnionRect(var DestRect: TRect; const SrcRect1, SrcRect2: TRect): Boolean; //pbd

Function GetRValue(RGB : DWORD) : BYTE;
Function GetGValue(RGB : DWORD) : BYTE;
Function GetBValue(RGB : DWORD) : BYTE;

Function RGB(R, G, B : Byte) : TColorRef;

//##apiwiz##epi##    // Do not remove


{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.50  2002/10/14 18:36:57  lazarus
  AJ: Improvements/Fixes to new PromptUser API

  Revision 1.49  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.48  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.47  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  Revision 1.46  2002/10/03 14:47:31  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.45  2002/09/27 20:52:23  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.44  2002/09/19 19:56:14  lazarus
  MG: accelerated designer drawings

  Revision 1.43  2002/09/18 17:07:25  lazarus
  MG: added patch from Andrew

  Revision 1.42  2002/09/12 05:56:15  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.41  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.40  2002/09/09 14:01:06  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.39  2002/08/30 12:32:21  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.38  2002/08/28 09:40:49  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.37  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.36  2002/08/17 15:45:33  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.35  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.34  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.33  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.32  2002/08/08 17:26:38  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.31  2002/08/08 09:07:06  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.30  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.29  2002/08/05 10:45:03  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.28  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.27  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.26  2002/05/27 17:58:42  lazarus
  MG: added command line help

  Revision 1.25  2002/05/24 07:16:32  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.24  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.23  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.22  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.21  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.20  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.19  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.18  2001/12/28 11:41:51  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.17  2001/12/27 16:31:28  lazarus
  MG: implemented TCanvas.Arc

  Revision 1.16  2001/12/12 14:23:18  lazarus
  MG: implemented DestroyCaret

  Revision 1.15  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.14  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.13  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.12  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.11  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.10  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.9  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.8  2001/02/16 19:13:31  lazarus
  Added some functions
  Shane

  Revision 1.7  2001/01/23 18:42:10  lazarus
  Added InvalidateRect to gtkwinapi.inc
  Shane

  Revision 1.6  2000/09/10 23:08:30  lazarus
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

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

}

