// included by lcllinux.pp

(******************************************************************************
  All Winapi related stuff goes here.
  This file is used by LCLLinux
  If a procedure is platform dependent then it should call:
    InterfaceObject.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!
  
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
  These functions redirect to the platform specific interface object.

 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function Arc(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean;
begin
  Result := InterfaceObject.Arc(DC,x,y,width,height,angle1,angle2);
end;

function AngleChord(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean;
begin
  Result := InterfaceObject.AngleChord(DC,x,y,width,height,angle1,angle2);
end;

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; 
begin
  Result := InterfaceObject.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
end;

Function BringWindowToTop(hWnd : HWND): Boolean;
begin
  Result := InterfaceObject.BringWindowToTop(hWnd);
end;

function CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer; 
begin
  Result := InterfaceObject.CallNExtHookEx(hhk,ncode,wparam,lParam);
end;

Function CallWindowProc(lpPrevWndFunc :TFarProc; Handle : HWND; Msg : UINT; wParam ,lParam : LongInt) : Integer;
begin
  Result := InterfaceObject.CallWindowProc(lpPrevWndFunc, Handle, Msg, wParam, lParam);
end;

function CheckMenuItem(hndMenu: HMENU; uIDEnableItem: Integer;
  bChecked: Boolean): Boolean;
begin
  Result := InterfaceObject.CheckMenuItem(hndMenu,uIDEnableItem,bChecked);
end;

Function ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean;
Begin
  Result := InterfaceObject.ClientToScreen(Handle, P);
end;

// the clipboard functions are internally used by TClipboard
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  Result := InterfaceObject.ClipboardFormatToMimeType(FormatID);
end;

function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  Result := InterfaceObject.ClipboardGetData(ClipboardType, FormatID, Stream);
end;

// ! List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result := InterfaceObject.ClipboardGetFormats(ClipboardType,Count,List);
end;

function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := InterfaceObject.ClipboardGetOwnerShip(ClipboardType, OnRequestProc,
                   FormatCount, Formats);
end;

function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := InterfaceObject.ClipboardRegisterFormat(AMimeType);
end;


Function CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;
begin
  Result := InterfaceObject.CombineRgn(Dest, Src1, Src2, fnCombineMode);
end;

function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  Result := InterfaceObject.CreateBitmap(Width, Height, Planes, BitCount, BitmapBits);
end;

function CreateDIBitmap(DC: HDC; var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo;
  wUsage: UINT): HBITMAP;
begin
  Result := InterfaceObject.CreateDIBitmap(DC,InfoHeader,dwUsage,InitBits,
                                           InitInfo,wUsage);
end;

function CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  Result := InterfaceObject.CreateBrushIndirect(LogBrush);
end;

Function CreateCaret(Handle : HWND; Bitmap : hBitmap; width, Height : Integer) : Boolean;
Begin
  Result := InterfaceObject.CreateCaret(Handle, Bitmap, width, Height);
end;

function CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := InterfaceObject.CreateCompatibleBitmap(DC, Width, Height);
end;

function CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := InterfaceObject.CreateCompatibleDC(DC);
end;

function CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := InterfaceObject.CreateFontIndirect(LogFont);
end;

function CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result := InterfaceObject.CreateFontIndirectEx(LogFont,LongFontName);
end;

function CreatePalette(const LogPalette: TLogPalette): HPalette;
begin
  Result := InterfaceObject.CreatePalette(LogPalette);
end;

function CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := InterfaceObject.CreatePenIndirect(LogPen);
end;

function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  Result := InterfaceObject.CreatePixmapIndirect(Data, TransColor);
end;

function CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN;
Begin
  Result := InterfaceObject.CreatePolygonRgn(Points, NumPts, FillMode);
end;

function CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
Begin
  Result := InterfaceObject.CreateRectRgn(X1,Y1,X2,Y2);
end;

function DeleteDC(hDC: HDC): Boolean;
begin
  Result := InterfaceObject.DeleteDC(hDC);
end;

function DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result := InterfaceObject.DeleteObject(GDIObject);
end;

function DestroyCaret(Handle : HWND): Boolean;
Begin
  Result := InterfaceObject.DestroyCaret(Handle);
end;

Function DrawFrameControl(DC: HDC; var Rect : TRect; uType, uState : Cardinal) : Boolean;
Begin
  Result := InterfaceObject.DrawFrameControl(DC, Rect, uType, uState);
end;

function DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
Begin
  Result := InterfaceObject.DrawEdge(DC, Rect, edge, grfFlags);
end;

function DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
begin
  Result := InterfaceObject.DrawText(DC, Str, Count, Rect, Flags);
end;

function DPtoLP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := InterfaceObject.DPtoLP(DC,Points,Count);
end;

function EnableMenuItem(hndMenu: HMENU; uIDEnableItem: Integer; bEnable: Boolean): Boolean;
begin
  Result := InterfaceObject.EnableMenuItem(hndMenu, uIDEnableItem, bEnable);
end;

function EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Result := InterfaceObject.EnableScrollBar(Wnd, wSBflags, wArrows);
end;

function EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := InterfaceObject.EnableWindow(hWnd, bEnable);
end;

function Ellipse(DC: HDC; x1,y1,x2,y2: Integer): Boolean;
begin
  Result := InterfaceObject.Ellipse(DC,x1,y1,x2,y2);
end;

function ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer;
begin
  Result := InterfaceObject.ExcludeClipRect(DC,Left,Top,Right,Bottom);
end;

{------------------------------------------------------------------------------
  Function: ExcludeClipRgn
  Params:  dc, RGN
  Returns: integer

  Subtracts all intersecting points of the passed Region from the Current
  clipping region in the device context (dc).

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function ExcludeClipRgn(dc: hdc; rgn : hrgn) : Integer;
begin
  Result := ExtSelectClipRGN(DC, RGN, RGN_DIFF);
end;

function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := InterfaceObject.ExtTextOut(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
begin
  Result := InterfaceObject.ExtSelectClipRGN(DC, RGN, Mode);
end;

function FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  Result := InterfaceObject.FillRect(DC, Rect, Brush);
end;

function FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL;
begin
  Result := InterfaceObject.FillRgn(DC,RegionHnd,hbr);
end;

function FloodFill(DC: HDC; X, Y: Integer; Color: TColor;
  FillStyle: TFillStyle; Brush: HBRUSH): Boolean;
begin
  Result := InterfaceObject.FloodFill(DC,X,Y,Color,FillStyle,Brush);
end;

function Frame(DC: HDC; const ARect: TRect): Integer;
begin
  Result:= InterfaceObject.Frame(DC, ARect);
end;

function Frame3d(DC : HDC; var ARect : TRect; const FrameWidth : integer;
  const Style : TBevelCut) : boolean;
begin
  Result:= InterfaceObject.Frame3d(DC, ARect, FrameWidth, Style);
end;

function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
begin
  Result:= InterfaceObject.FrameRect(DC, ARect, hBr);
end;

Function GetActiveWindow : HWND;
begin
  Result := InterfaceObject.GetActiveWindow;
end;

Function GetCapture: HWND;
Begin
  Result := InterfaceObject.GetCapture;
end;

function GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := InterfaceObject.GetCaretPos(lpPoint);
end;

function GetCaretRespondToFocus(handle: HWND;
  var ShowHideOnFocus: boolean): Boolean;
begin
  Result := InterfaceObject.GetCaretRespondToFocus(handle,ShowHideOnFocus);
end;

{------------------------------------------------------------------------------
  Function: GetClientBounds
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
Function GetClientBounds(handle : HWND; var Rect : TRect) : Boolean;
begin
  Result := InterfaceObject.GetClientBounds(handle, Rect);
end;

{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns: true on success

  Returns the client rectangle of a control. Left and Top are always 0.
  The client rectangle is the size of the inner area of a control, where the
  child controls are visible.
 ------------------------------------------------------------------------------}
Function GetClientRect(handle : HWND; var Rect : TRect) : Boolean;
begin
  Result := InterfaceObject.GetClientRect(handle, Rect);
end;

Function GetClipBox(DC : hDC; lpRect : PRect) : Longint;
begin
  Result := InterfaceObject.GetClipBox(DC, lpRect);
end;

Function GetClipRGN(DC : hDC; RGN : hRGN) : Longint;
begin
  Result := InterfaceObject.GetClipRGN(DC, RGN);
end;

{------------------------------------------------------------------------------
  Function: GetCmdLineParamDescForInterface
  Params: none
  Returns: ansistring

  Returns a description of the command line parameters, that are understood by
  the interface.
 ------------------------------------------------------------------------------}
Function GetCmdLineParamDescForInterface: string;
begin
  Result := InterfaceObject.GetCmdLineParamDescForInterface;
end;

Function GetCursorPos(var lpPoint:TPoint): Boolean;
Begin
  Result := InterfaceObject.GetCaretPos(lpPoint);
end;

function GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean;
begin
  Result := InterfaceObject.GetCharABCWidths(DC, p2, p3, ABCStructs);
end;

function GetDC(hWnd: HWND): HDC;
begin
  Result := InterfaceObject.GetDC(hWnd);
end;

function GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := InterfaceObject.GetDIBits(DC, Bitmap, StartScan, NumScans, Bits, BitInfo, Usage);
end;

function GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result := InterfaceObject.GetDeviceCaps(DC, Index);
end;

function GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  Result := InterfaceObject.GetDeviceSize(DC,p);
end;

function GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  Result := InterfaceObject.GetBitmapBits(Bitmap, Count, Bits);
end;

function CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP;
begin
  Result := InterfaceObject.CreateDIBSection(DC, p2,  p3, p4, p5, p6)
end;

function GetFocus: HWND;
begin
  Result := InterfaceObject.GetFocus;
end;

function GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result := InterfaceObject.GetFontLanguageInfo(DC);
end;

function GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := InterfaceObject.GetKeyState(nVirtKey);
end;

function GetMapMode(DC: HDC): Integer;
begin
  Result := InterfaceObject.GetMapMode(DC);
end;

function GetNotebookTabIndexAtPos(Handle: HWND;
  const ClientPos: TPoint): integer;
begin
  Result := InterfaceObject.GetNotebookTabIndexAtPos(Handle,ClientPos);
end;

function GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
begin
  Result := InterfaceObject.GetObject(GDIObject, BufSize, Buf);
end;

function GetPaletteEntries(Palette: HPALETTE; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT;
begin
  Result := InterfaceObject.GetPaletteEntries(Palette,StartIndex,NumEntries,
                                              PaletteEntries);
end;

Function GetParent(Handle : HWND): HWND;
begin
  Result := InterfaceObject.GetParent(Handle);
end;

Function GetProp(Handle : hwnd; Str : PChar): Pointer;
Begin
  Result := InterfaceObject.GetProp(Handle,Str);
end;

Function GetRGNBox(RGN : HRGN; lpRect : PRect) : Longint;
begin
  Result := InterfaceObject.GetRGNBox(RGN, lpRect);
end;

function GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;
begin
  Result := InterfaceObject.GetScrollBarSize(Handle, BarKind);
end;

function GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean;
begin
  Result := InterfaceObject.GetScrollInfo(Handle, BarFlag, ScrollInfo);
end;

function GetStockObject(Value : Integer): Longint;
begin
  Result := InterfaceObject.GetStockObject(Value);
end;

function GetSysColor(nIndex: Integer): DWORD;
begin
  Result := InterfaceObject.GetSysColor(nIndex);
end;

function GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result := InterfaceObject.GetSystemMetrics(nIndex);
end;

function GetSystemPaletteEntries(DC: HDC; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT;
begin
  Result := InterfaceObject.GetSystemPaletteEntries(DC,StartIndex,NumEntries,
                                                    PaletteEntries);
end;

function GetTextColor(DC: HDC) : TColorRef;
begin
  Result := InterfaceObject.GetTextColor(DC);
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar; Count, p4: Integer;
  p5, p6: PInteger; var Size: TSize): BOOL;
begin
  Result := InterfaceObject.GetTextExtentExPoint(DC,Str,Count,p4,p5,p6,Size);
end;

function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
begin
  Result := InterfaceObject.GetTextExtentPoint(DC, Str, Count, Size);
end;

function GetTextExtentPoint32(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
begin
  Result := InterfaceObject.GetTextExtentPoint32(DC, Str, Count, Size);
end;

function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Result := InterfaceObject.GetTextMetrics(DC, TM);
end;

function GetWindowLong(Handle : hwnd; int : Integer): Longint;
begin
  Result := InterfaceObject.GetWindowLong(Handle, int);
end;

Function GetWindowRect(Handle : hwnd; var Rect : TRect): Integer;
begin
  Result := InterfaceObject.GetWindowRect(Handle, Rect);
end;

Function GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean;
begin
  Result := InterfaceObject.GetWindowSize(Handle, Width, Height);
end;

Function GetWindowOrgEx(dc : hdc; P : PPoint): Integer;
begin
 Result := InterfaceObject.GetWindowOrgEx(dc,P);
end;

function GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean;
begin
  Result := InterfaceObject.GradientFill(DC, Vertices, NumVertices, Meshes, NumMeshes, Mode);
end;

function HideCaret(hWnd: HWND): Boolean;
begin
  Result := InterfaceObject.HideCaret(hWnd);
end;

{------------------------------------------------------------------------------
  Function: IntersectClipRect
  Params:  dc: hdc; Left, Top, Right, Bottom: Integer
  Returns: Integer

  Shrinks the current clipping region in the device context dc to the boundary
  defined by Left, Top, Right, Bottom.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
      Region_Error

 ------------------------------------------------------------------------------}
function IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer;
Begin
  Result := InterfaceObject.IntersectClipRect(dc,Left,Top,Right,Bottom);
end;

{------------------------------------------------------------------------------
  Function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean;
    BorderWidth: integer) : Boolean;
    
  Calls InvalidateRect for the borderframe.
------------------------------------------------------------------------------}
Function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean; BorderWidth: integer) : Boolean;
begin
  Result := InterfaceObject.InvalidateFrame(aHandle,ARect,bErase,BorderWidth);
end;

Function InvalidateRect(aHandle : HWND; ARect : pRect; bErase : Boolean) : Boolean;
begin
  Result := InterfaceObject.InvalidateRect(aHandle, ARect, bErase);
end;

function IsDBCSLeadByte(TestChar: Byte): boolean;
begin
  Result := InterfaceObject.IsDBCSLeadByte(TestChar);
end;

function IsWindowVisible(handle: HWND): boolean;
begin
  Result := InterfaceObject.IsWindowVisible(Handle);
end;

function LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result := InterfaceObject.LineTo(DC, X, Y);
end;

Function LoadStockPixmap(StockID: longint) : HBitmap;
begin
  Result := InterfaceObject.LoadStockPixmap(StockID);
end;

function MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; 
begin
  Result := InterfaceObject.MaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Mask, XMask, YMask, Rop);
end;

function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType : Cardinal): integer;
begin
  Result:= InterfaceObject.MessageBox(hWnd, lpText, lpCaption, uType);
end;

function MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Result := InterfaceObject.MoveToEx(DC, X, Y, OldPoint);
end;

function MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean;
begin
  Result := InterfaceObject.MoveWindowOrgEx(DC, dX, dY);
end;

function PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  Result := InterfaceObject.PeekMessage(lpMsg,Handle,wMsgFilterMin,wMsgFilterMax,wRemoveMsg);
end;

function Pie(DC: HDC; x,y,width,height,angle1,angle2 : Integer): Boolean;
begin
  Result := InterfaceObject.Pie(DC,x,y,width,height,angle1,angle2);
end;

function PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer; 
  Filled, Continuous: boolean): boolean; 
begin
  Result := InterfaceObject.PolyBezier(DC,Points,NumPts,Filled,Continuous);
end;

function Polygon(DC: HDC; Points: PPoint; NumPts: Integer; 
  Winding: boolean): boolean; 
begin
  Result := InterfaceObject.Polygon(DC,Points,NumPts,Winding);
end;

function Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean; 
begin
  Result := InterfaceObject.Polyline(DC,Points,NumPts);
end;

function PostMessage(Handle: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Boolean;
begin
  Result := InterfaceObject.PostMessage(Handle, Msg, wParam, lParam);
end;

function PtInRegion(RGN: HRGN; X, Y: Integer) : Boolean;
begin
  Result := InterfaceObject.PtInRegion(RGN,X,Y);
end;

function RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := InterfaceObject.RadialChord(DC, x,y,width,height,sx,sy,ex,ey);
end;

function RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := InterfaceObject.RadialArc(DC, x,y,width,height,sx,sy,ex,ey);
end;

function RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
begin
  Result := InterfaceObject.RadialPie(DC, x,y,width,height,sx,sy,ex,ey);
end;

function RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean;
begin
  Result := InterfaceObject.RadioMenuItemGroup(hndMenu,bRadio);
end;

function RealizePalette(DC: HDC): Cardinal;
begin
  Result := InterfaceObject.RealizePalette(DC);
end;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := InterfaceObject.Rectangle(DC, X1, Y1, X2, Y2);
end;

function RectVisible(dc : hdc; ARect: TRect) : Boolean;
begin
  Result := InterfaceObject.RectVisible(dc,ARect);
end;

function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer) : Boolean;
begin
  Result := InterfaceObject.RegroupMenuItem(hndMenu,GroupIndex);
end;

function ReleaseCapture:  Boolean;
begin
  Result := InterfaceObject.ReleaseCapture;
end;

function ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := InterfaceObject.ReleaseDC(hWnd, DC);
end;

Function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean;
begin
  Result := InterfaceObject.RequestInput(InputCaption, InputPrompt, MaskInput, Value);
end;

Procedure NotifyUser(const DialogMessage : String; DialogType : longint);
begin
  PromptUser(DialogMessage, DialogType, [idButtonOK], -1, -1);
end;

Procedure NotifyUserAtXY(const DialogMessage : String; DialogType : longint; X, Y : Longint);
begin
  PromptUserAtXY(DialogMessage, DialogType, [idButtonOK], -1, -1, X, Y);
end;

Procedure NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint);
begin
  PromptUser(DialogCaption, DialogMessage, DialogType, [idButtonOK], -1, -1);
end;

Procedure NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint);
begin
  PromptUserAtXY(DialogCaption, DialogMessage, DialogType, [idButtonOK], -1, -1, X, Y);
end;

Function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := PromptUser(DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult);
end;

Function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUser('', DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult);
end;

Function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := PromptUser(DialogCaption, DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult);
end;

Function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUser(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult);
end;

Function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := PromptUserAtXY(DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult, X, Y);
end;

Function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUserAtXY('', DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult, X, Y);
end;

Function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint;
  DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := PromptUserAtXY(DialogCaption, DialogMessage, DialogType, @Buttons[Low(Buttons)],
    High(Buttons) - Low(Buttons) + 1, DefaultIndex, EscapeResult, X, Y);
end;

Function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint;
  ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  Result := InterfaceObject.PromptUserAtXY(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount, DefaultIndex, EscapeResult, X, Y);
end;

function RightJustifyMenuItem(HndMenu: HMenu;
  bRightJustify: boolean): boolean;
begin
  Result := InterfaceObject.RightJustifyMenuItem(hndMenu, bRightJustify);
end;

Function RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Result := InterfaceObject.RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

function RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result := InterfaceObject.RestoreDC(DC, SavedDC)  
end;

function SaveDC(DC: HDC) : Integer; 
begin
  Result := InterfaceObject.SaveDC(DC)  
end;

Function ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  Result := InterfaceObject.ScreenToClient(Handle, P);
end;

function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result := InterfaceObject.ScrollWindowEx(hWnd, dx, dy, prcScroll, prcClip, hrgnUpdate, prcUpdate, flags);
end;

procedure SendCachedLCLMessages;
begin

end;

function SendMessage(HandleWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Integer;
begin
  Result := InterfaceObject.SendMessage(HandleWnd, Msg, wParam, lParam);
end;

function SetActiveWindow(Handle: HWND): HWND;
begin
  Result := InterfaceObject.SetActiveWindow(Handle);
end;

function SetBkColor(DC: HDC; Color: TColorRef): TColorRef;  //pbd
begin
  Result := InterfaceObject.SetBkColor(DC, Color);
end;

Function SetBkMode(DC: HDC; bkMode : Integer) : Integer;
begin
  Result := InterfaceObject.SetBkMode(DC, bkMode);
end;

Function SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result := InterfaceObject.SetComboMinDropDownSize(Handle,
                                   MinItemsWidth, MinItemsHeight, MinItemCount);
end;

Function SetCapture(value : Longint): Longint;
Begin
  Result := InterfaceObject.SetCapture(Value);
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := InterfaceObject.SetCaretPos(X, Y);
end;

function SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result := InterfaceObject.SetCaretPosEx(Handle, X, Y);
end;

function SetCaretRespondToFocus(Handle: hWnd; ShowHideOnFocus: boolean):Boolean;
begin
  Result := InterfaceObject.SetCaretRespondToFocus(Handle,ShowHideOnFocus);
end;
 
function SetCursor(hCursor: HICON): HCURSOR;
begin
  Result := InterfaceObject.SetCursor(hCursor);
end;
 
Function SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
Begin
  Result := InterfaceObject.SetProp(Handle,Str,Data);
end;

Function SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  Result := InterfaceObject.SelectClipRGN(DC, RGN);
end;

function SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result := InterfaceObject.SelectObject(DC, GDIObj);
end;

function SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Result := InterfaceObject.SelectPalette(DC, Palette, ForceBackground);
end;

function SetFocus(hWnd: HWND): HWND;
begin
//writeln('[winapi.inc SetFocus] A');
  Result := InterfaceObject.SetFocus(hWnd);
//writeln('[winapi.inc SetFocus] END');
end;

function SetMapMode(DC: HDC; MapMode: Integer): Integer;
begin
  Result := InterfaceObject.SetMapMode(DC,MapMode);
end;

function SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; Redraw : Boolean): Integer;
begin
  Result := InterfaceObject.SetSCrollInfo(Handle, SBStyle, ScrollInfo, Redraw)
end;

function SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer;
begin
  Result := InterfaceObject.SetStretchBltMode(DC,StretchMode);
end;

function SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; 
begin
  Result := InterfaceObject.SetSysColors(cElements, lpaElements, lpaRgbValues);
end;

function SetTextAlign(DC: HDC; Flags: UINT): UINT;
begin
  Result := InterfaceObject.SetTextAlign(DC,Flags);
end;

Function SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
begin
  Result := InterfaceObject.SetTextCharacterExtra(_hdc, nCharExtra);
end;

function SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := InterfaceObject.SetTextColor(DC, Color);
end;

function SetWindowLong(Handle: HWND; Idx: Integer; NewLong : Longint): LongInt;
begin
  Result := InterfaceObject.SetWindowLong(handle, Idx, NewLong);
end;

function SetWindowOrgEx(dc : hdc; NewX, NewY : Integer;
  OldPoint: PPoint) : Boolean;
begin
  Result := InterfaceObject.SetWindowOrgEx(dc, NewX, NewY, OldPoint);
end;

function SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx, cy: Integer; uFlags: UINT): Boolean;
begin
  Result:=false;
end;

function ShowCaret(hWnd: HWND): Boolean;
begin
  Result := InterfaceObject.ShowCaret(hWnd)
end;

function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
begin
  Result := InterfaceObject.ShowScrollBar(Handle, wBar, bShow);
end;

function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result := InterfaceObject.ShowWindow(hWnd,nCmdShow);
end;

procedure Sleep(dwMilliseconds: DWORD);
begin
  InterfaceObject.Sleep(dwMilliseconds);
end;

function StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  Result := InterfaceObject.StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop)
end;

function StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHeight, SrcX,
  SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; var BitsInfo: TBitmapInfo;
  Usage: UINT; Rop: DWORD): Integer;
begin
  Result := InterfaceObject.StretchDIBits(DC,DestX,DestY,DestWidth,DestHeight,
                          SrcX,SrcY,SrcWidth,SrcHeight,Bits,BitsInfo,Usage,Rop);
end;

function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; 
begin
  Result := InterfaceObject.StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Mask, XMask, YMask, Rop);
end;

function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result := InterfaceObject.TextOut(DC, X, Y, Str, Count);
end;

function WindowFromPoint(Point : TPoint) : HWND;
begin
  Result := InterfaceObject.WindowFromPoint(Point);
end;

Procedure InitializeCriticalSection(var CritSection: TCriticalSection);
begin
  InterfaceObject.InitializeCriticalSection(CritSection);
end;

Procedure EnterCriticalSection(var CritSection: TCriticalSection);
begin
  InterfaceObject.EnterCriticalSection(CritSection);
end;

Procedure LeaveCriticalSection(var CritSection: TCriticalSection);
begin
  InterfaceObject.LeaveCriticalSection(CritSection);
end;

Procedure DeleteCriticalSection(var CritSection: TCriticalSection);
begin
  InterfaceObject.DeleteCriticalSection(CritSection);
end;

//##apiwiz##eps##   // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

//##apiwiz##spi##   // Do not remove
{------------------------------------------------------------------------------
  Function: AdjustWindowRectEx
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function AdjustWindowRectEx( Var Rect: TRect; Style1: Word; MenuExist : Boolean;
  Style2 : Word) : Boolean;
begin
  // ToDo:
  Result := true;
  if MenuExist
  then Rect.Top := Rect.Top + 25;
end;

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function BeginPaint(Handle : hwnd; Var PS : TPaintStruct) : hdc;
begin
  Assert(False, Format('Trace:> [BeginPaint] HWND: 0x%x', [Handle]));
  
  //TODO: Finish this.  BEGINPAINT
  // Move to platform dependent ??
  Result := Getdc(Handle);

  Assert(False, Format('Trace:< [BeginPaint] HWND: 0x%x --> 0x%x', [Handle, Result]));
end;

{------------------------------------------------------------------------------
  Function: CharLowerBuff
  Params: pStr:
          Len:
  Returns:

 ------------------------------------------------------------------------------}
function CharLowerBuff(pStr : PChar; Len : Integer): Integer;
begin
  // your code here
  //TODO:WINAPI call CHARLOWERBUFF
  Writeln('TODO: WINAPI call CHARLOWERBUFF');
  Result := -1;
end;


{------------------------------------------------------------------------------
  Function: CopyRect  pbd
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CopyRect(var DestRect: TRect; const SrcRect: TRect): Boolean;
begin
  Move(SrcRect, DestRect, SizeOf(TRect));
  Result := True;
end;

{------------------------------------------------------------------------------
  function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
 ------------------------------------------------------------------------------}
function CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
begin
  Result := InterfaceObject.CreateEllipticRgn(p1,p2,p3,p4);
end;

{------------------------------------------------------------------------------
  function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
 ------------------------------------------------------------------------------}
function CreateEllipticRgnIndirect(const ARect: TRect): HRGN;
begin
  with ARect do
    Result:=CreateEllipticRgn(Left,Top,Right,Bottom);
end;

{------------------------------------------------------------------------------
  Function: CreateFont
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CreateFont(Height, Width, Escapement, Orientation, Weight: Integer;
  Italic, Underline, StrikeOut, CharSet, OutputPrecision, ClipPrecision,
  Quality, PitchAndFamily: Cardinal; FaceName: PChar): HFONT;
var
  LogFont: TLogFont;
begin
writeln('CreateFont Name="',FaceName,'"');
  with LogFont do
  begin
    lfHeight := Height;
    lfWidth := Width;
    lfEscapement := Escapement;
    lfOrientation := Orientation;
    lfWeight := Weight;
    lfItalic := Italic;
    lfUnderline := Underline;
    lfStrikeOut := StrikeOut;
    lfCharSet := CharSet;
    lfOutPrecision := OutputPrecision;
    lfClipPrecision := ClipPrecision;
    lfQuality := Quality;
    lfPitchAndFamily := PitchAndFamily;
    StrLCopy(@lfFaceName, FaceName, SizeOf(lfFaceName)-1);
  end;

  Result := CreateFontIndirect(LogFont);
end;

{------------------------------------------------------------------------------
  Function: CreatePen
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function CreatePen(Style, Width: Integer; Color: TColorRef): HPEN;
var
  LogPen: TLogPen;
begin
  with LogPen do
  begin
    lopnStyle := Style;
    lopnWidth.X := Width;
    lopnColor := Color;
  end;

  Result := CreatePenIndirect(LogPen);
end;

{------------------------------------------------------------------------------
  function CreateRectRgnIndirect(const ARect: TRect): HRGN;
-------------------------------------------------------------------------------}
function CreateRectRgnIndirect(const ARect: TRect): HRGN;
begin
  with ARect do
    Result:=CreateRectRgn(Left,Top,Right,Bottom);
end;

{------------------------------------------------------------------------------
  Function: EndPaint
  Params:
-------------------------------------------------------------------------------}
Function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
Begin
  Assert(False, Format('Trace:> [EndPaint] HWND: 0x%x', [Handle]));

  //TODO: Finish EndPaint in winapi.inc
  Result := 1;
  Assert(False, Format('Trace:< [EndPaint] HWND: 0x%x --> 0x%x', [Handle, Result]));
end;


{------------------------------------------------------------------------------
  Function: EqualRect
  Params:   Rect to Compare
  Returns:
  Quicker with a comparemem? whats the fpc equiv?
 ------------------------------------------------------------------------------}
function EqualRect(const lprc1, lprc2: TRect): Boolean;
begin
  //Result := CompareMem(@lprc1, @lprc2, SizeOf(TRect);
  Result := (lprc1.Left = lprc2.Left) And (lprc1.Right = lprc2.Right) And
            (lprc1.Top = lprc2.Top) And (lprc1.Bottom = lprc2.Bottom);
end;


{------------------------------------------------------------------------------
  Function: GetScrollPos
  Params:   Handle, nBar
  Returns:

 ------------------------------------------------------------------------------}
function GetScrollPos(Handle: HWND; nBar: Integer): Integer;
var
  Info: TScrollInfo;
begin
  GetScrollInfo(Handle, nBar, Info);
  Result := Info.nPos;
end;

{------------------------------------------------------------------------------
  Function: GetScrollRange
  Params:   Handle, nBar, lpMinPos, lpMaxPos
  Returns:

 ------------------------------------------------------------------------------}
function GetScrollRange(Handle: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): Boolean;
var
  Info: TScrollInfo;
begin
  Result := GetScrollInfo(Handle, nBar, Info);
  lpMinPos := Info.nMin;
  lpMaxPos := Info.nMax;
end;

{------------------------------------------------------------------------------
  Function: InflateRect
  Params:   ARect: points to structure that increases or decreases in size.
            dx   : amount to increase the rectangle width.
            dy   : amount to increase the rectangle height.
  Returns:  True if succesful

  Increases or decreases the width and height of the specified rectangle.
 ------------------------------------------------------------------------------}
function InflateRect(var ARect: TRect; dx, dy: Integer): Boolean;
begin
  // make sure, that after deflating holds: Left<=Right
  if (dx<0) and (ARect.Right-ARect.Left+2*dx<0) then begin
    ARect.Left:=(ARect.Left+ARect.Right) shr 1;
    ARect.Right:=ARect.Left;
  end else begin
    dec(ARect.Left,dx);
    inc(ARect.Right,dx);
  end;

  // make sure, that after deflating holds: Top<=Bottom
  if (dy<0) and (ARect.Bottom-ARect.Top+2*dy<0) then begin
    ARect.Top:=(ARect.Top+ARect.Bottom) shr 1;
    ARect.Bottom:=ARect.Top;
  end else begin
    dec(ARect.Top,dy);
    inc(ARect.Bottom,dy);
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: IntersectRect
  Params:  var DestRect: TRect; const SrcRect1, SrcRect2: TRect
  Returns: Boolean
  
  Intersects SrcRect1 and SrcRect2 into DestRect.
  Intersecting means that DestRect will be the overlapping area of lprcSrc1 and
  lprcSrc2. If SrcRect1 and SrcRect2 does not overlapp the Result is false, else
  true.
 ------------------------------------------------------------------------------}
function IntersectRect(var DestRect: TRect;
  const SrcRect1, SrcRect2: TRect): Boolean;
begin
  Result := False;

   // test if rectangles intersects
  Result:=(SrcRect2.Left >= SrcRect1.Right)
       or (SrcRect2.Right <= SrcRect1.Left)
       or (SrcRect2.Top >= SrcRect1.Bottom)
       or (SrcRect2.Bottom <= SrcRect1.Top);
       
  if Result then begin
    DestRect.Left:=Max(SrcRect1.Left,SrcRect2.Left);
    DestRect.Top:=Max(SrcRect1.Top,SrcRect2.Top);
    DestRect.Right:=Min(SrcRect1.Right,SrcRect2.Right);
    DestRect.Bottom:=Min(SrcRect1.Bottom,SrcRect2.Bottom);
  end else begin
    SetRectEmpty(DestRect);
  end;
end;

{------------------------------------------------------------------------------
  Function: IsCharAlphaNumeric
  Params: c:
  Returns:

 ------------------------------------------------------------------------------}
Function IsCharAlphaNumeric(c : Char) : Boolean;
begin
  // your code here
  Result := False;
  Result :=  ((ord(c) >= 65) and (ord(c) <=90) ) or ((ord(c) >= 97) and (ord(c) <=122) );
end;

{------------------------------------------------------------------------------
  Function: IsRectEmpty
  Params: const lprc: TRect
  Returns: Boolean

  Returns true if ARect is (0,0,0,0)
 ------------------------------------------------------------------------------}
function IsRectEmpty(const ARect: TRect): Boolean;
begin
  with ARect do
    Result := (Left = 0) and (Top = 0) and (Right = 0) and (Bottom = 0);
end;

{------------------------------------------------------------------------------
  Function: MakeLParam
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeLParam(l, h: Word): LPARAM;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: MakeLResult
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeLResult(l, h: Word): LRESULT;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: MakeWParam
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function MakeWParam(l, h: Word): WPARAM;
begin
  Result := MakeLong(l, h);
end;

{------------------------------------------------------------------------------
  Function: OffSetRect
  Params:   Rect: points to structure that moves.
            dx  : amount to move the rect to left or right.  Must be negative to move to left.
            dy  : amount to move the rect up or down.  Mmust be negative to move up.
  Returns:  True if succesful

  Moves the rectangle up or down, left or right.
 ------------------------------------------------------------------------------}
function OffSetRect(var Rect: TRect; dx,dy: Integer): Boolean;
Begin
  Result:=LCLProc.OffsetRect(Rect,dx,dy);
end;

{------------------------------------------------------------------------------
  Function: PointtoSmallPoint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function PointtoSmallPoint(const P : TPoint) : TSmallPoint;
Begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

{------------------------------------------------------------------------------
  Function: PtInRect
  Params:  Rect
           Point
  Returns: True if point is in rect

  Determines if the POINT is within the rect
  It is considered inside if it lies on the left top, or within the rectangle.
  It is outside ther rect if it's on the bottom or right.
 ------------------------------------------------------------------------------}
Function PtInRect(Rect : TRect; Point : TPoint) : Boolean;
Begin
  Result := ((Point.X >= Rect.Left) and
             (Point.X < Rect.Right) and
             (Point.Y >= Rect.Top)  and
             (Point.Y < Rect.Bottom)
          );
end;

{------------------------------------------------------------------------------
  Function: ScrollWindow   In progress pbd
  Params:   Handle
            XAmount +scroll down -scroll up 
            Rect: Rect to move
            ClipRect: Boundaries at which the pixels go to /dev/nul
  Returns:  More than a simple boolean but for compatibilty bool will do

  scrolls a window or portion of a window
 ------------------------------------------------------------------------------}
function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer;
  Rect, ClipRect: PRect): Boolean;
begin
  Result := ScrollWindowEx(hWnd, XAmount, YAmount, Rect, ClipRect, 0, 0, 0);
end;

{------------------------------------------------------------------------------
  Function: SetRect
  Params:
  Returns: 
 ------------------------------------------------------------------------------}
Function SetRect(var ARect : TRect; xLeft,yTop,xRight,yBottom : Integer) : Boolean;
Begin
  Result := True;
  with ARect do begin
    Left := xLeft;
    Top := yTop;
    Right := xRight;
    Bottom := yBottom;
  end;
End;

{------------------------------------------------------------------------------
  Function: SetRectEmpty
  Params: Rect to clear 
  Returns: essentially nothing
  
  
 ------------------------------------------------------------------------------}
function SetRectEmpty(var ARect: TRect): Boolean;
begin
  FillChar(ARect, SizeOf(TRect), 0);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SetScrollPos
  Params:   Handle, nBar, nPos, bRedraw
  Returns:  The old position

  The SetScrollPos function sets the position of the scroll box (thumb) in the
  specified scroll bar and, if requested, redraws the scroll bar to reflect
  the new position of the scroll box.
 ------------------------------------------------------------------------------}
function SetScrollPos(Handle: HWND; nBar, nPos: Integer; bRedraw: Boolean): Integer;
var
  Info: TScrollInfo;
begin
  Info.fMask := SIF_POS;
  Info.nPos := nPos;
  Result := SetScrollInfo(Handle, nBar, Info, bRedraw);
end;

{------------------------------------------------------------------------------
  Function: SetScrollRange
  Params:   Handle, nBar, nMinPos, nMaxPos, bRedraw
  Returns:  True is succesful

  The SetScrollRange function sets the minimum and maximum position values
  for the specified scroll bar.
 ------------------------------------------------------------------------------}
function SetScrollRange(Handle: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: Boolean): Boolean;
var
  Info: TScrollInfo;
begin
  Info.fMask := SIF_RANGE;
  Info.nMin := nMinPos;
  Info.nMAx := nMaxPos;
  SetScrollInfo(Handle, nBar, Info, bRedraw);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function:
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function SmallPointtoPoint(const P : TSmallPoint) : Tpoint;
Begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

{------------------------------------------------------------------------------
  Function: UnionRect          pbd
  Params: var DestRect: TRect; const SrcRect1, SrcRect2: TRect
  Returns: Boolean 0 on failure

  Creates the union rectangle of SrcRect1 and SrcRect2 into DestRect.
  The union rectangle encloses SrcRect1 and SrcRect2.
 ------------------------------------------------------------------------------}
function UnionRect(var DestRect: TRect;
  const SrcRect1, SrcRect2: TRect): Boolean;
begin
  Result := True;
  DestRect.Left := Min(SrcRect1.Left, SrcRect2.Left);
  DestRect.Top := Min(SrcRect1.Top, SrcRect2.Top);
  DestRect.Right := Max(SrcRect1.Right, SrcRect2.Right);
  DestRect.Bottom := Max(SrcRect1.Bottom, SrcRect2.Bottom);
end;

Function GetRValue(RGB : DWORD) : BYTE;
begin
  Result := RGB and $ff;
end;

Function GetGValue(RGB : DWORD) : BYTE;
begin
  Result := (RGB shr 8) and $ff;
end;

Function GetBValue(RGB : DWORD) : BYTE;
begin
  Result := (RGB shr 16) and $ff;
end;

Function RGB(R, G, B : Byte) : TColorRef;
begin
  Result := R or (G shl 8) or (B Shl 16);
end;

Procedure InitializeCriticalSection(var CritSection: TRTLCriticalSection);
begin
  writeln('InitializeCriticalSection(TRTLCriticalSection) Not implemented yet');
end;

Procedure EnterCriticalSection(var CritSection: TRTLCriticalSection);
begin
  writeln('EnterCriticalSection(TRTLCriticalSection) Not implemented yet');
end;

Procedure LeaveCriticalSection(var CritSection: TRTLCriticalSection);
begin
  writeln('LeaveCriticalSection(TRTLCriticalSection) Not implemented yet');
end;

Procedure DeleteCriticalSection(var CritSection: TRTLCriticalSection);
begin
  writeln('DeleteCriticalSection(TRTLCriticalSection) Not implemented yet');
end;

procedure RaiseLastOSError;
begin
  writeln('ToDo: RaiseLastOSError');
  RaiseGDBException('ToDo: RaiseLastOSError');
end;

//##apiwiz##epi##   // Do not remove

// included by lcllinux.pp

{ =============================================================================

  $Log$
  Revision 1.78  2003/03/17 20:53:16  mattias
  removed SetRadioButtonGroupMode

  Revision 1.77  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.76  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.75  2003/03/16 13:47:45  mattias
  improved rpm building and added support for 1.0.7

  Revision 1.74  2003/03/15 13:26:07  mattias
  fixes for fpc 1.1

  Revision 1.73  2003/02/28 19:54:05  mattias
  added ShowWindow

  Revision 1.72  2003/02/28 10:14:29  mattias
  started package system (packager)

  Revision 1.71  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.70  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.69  2002/12/30 17:24:08  mattias
  added history to identifier completion

  Revision 1.68  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.67  2002/12/26 11:00:14  mattias
  added included by to unitinfo and a few win32 functions

  Revision 1.66  2002/12/25 13:30:37  mattias
  added more windows funcs and fixed jump to compiler error end of file

  Revision 1.65  2002/12/25 10:21:05  mattias
  made Form.Close more Delphish, added some windows compatibility functions

  Revision 1.64  2002/12/05 22:16:29  mattias
  double byte char font started

  Revision 1.63  2002/11/30 11:22:53  mattias
  statusbar now uses invalidaterect

  Revision 1.62  2002/11/23 13:48:44  mattias
  added Timer patch from Vincent Snijders

  Revision 1.61  2002/11/09 15:02:07  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.60  2002/10/31 21:29:47  lazarus
  MG: implemented TControlScrollBar.Size

  Revision 1.59  2002/10/26 15:15:48  lazarus
  MG: broke LCL<->interface circles

  Revision 1.58  2002/10/23 14:36:52  lazarus
  AJ:Fixes to PromptUser;Switched ShowMessage* to use NotifyUser*;
     fixed TGraphicPropertyEditor for when Property is nil.

  Revision 1.57  2002/10/16 16:58:22  lazarus
  MG: moved SendCachedLCLMessages

  Revision 1.56  2002/10/14 18:36:57  lazarus
  AJ: Improvements/Fixes to new PromptUser API

  Revision 1.55  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.54  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.53  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  Revision 1.52  2002/10/03 14:47:31  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.51  2002/09/27 20:52:23  lazarus
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

  Revision 1.50  2002/09/19 19:56:14  lazarus
  MG: accelerated designer drawings

  Revision 1.49  2002/09/18 17:07:25  lazarus
  MG: added patch from Andrew

  Revision 1.48  2002/09/12 05:56:15  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.47  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.46  2002/09/09 14:01:06  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.45  2002/08/30 12:32:21  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.44  2002/08/28 09:40:49  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.43  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.42  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.41  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.40  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.39  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.38  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.37  2002/08/08 17:26:38  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.36  2002/08/08 09:07:06  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.35  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.34  2002/08/05 10:45:03  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.33  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.32  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.31  2002/05/27 17:58:42  lazarus
  MG: added command line help

  Revision 1.30  2002/05/24 07:16:32  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.29  2002/05/12 04:56:20  lazarus
  MG: client rect bugs nearly completed

  Revision 1.28  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.27  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.26  2002/03/11 23:22:46  lazarus
  MG: added TPicture clipboard support

  Revision 1.25  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.24  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.23  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.22  2001/12/28 11:41:51  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.21  2001/12/27 16:31:28  lazarus
  MG: implemented TCanvas.Arc

  Revision 1.20  2001/12/12 14:23:18  lazarus
  MG: implemented DestroyCaret

  Revision 1.19  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.18  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.17  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.16  2001/09/30 08:34:50  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.15  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.14  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.13  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.12  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.11  2001/02/16 19:13:30  lazarus
  Added some functions
  Shane

  Revision 1.10  2001/02/04 18:24:42  lazarus
  Code cleanup
  Shane

  Revision 1.9  2001/01/23 19:13:57  lazarus
  Fixxed the errors I commited with Unionrect
  Shane

  Revision 1.8  2001/01/23 19:01:10  lazarus
  Fixxed bug in RestoreDC
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
