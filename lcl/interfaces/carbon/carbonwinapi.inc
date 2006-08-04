{%MainUnit carbonint.pas}

{******************************************************************************
  All Carbon Winapi implementations.
  This are the implementations of the overrides of the Carbon Interface for the
  methods defined in the
  lcl/include/winapi.inc

  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
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

function TCarbonWidgetSet.Arc(DC: HDC; Left, Top, Right, Bottom, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited Arc(DC, Left, Top, Right, Bottom, angle1, angle2);
end;

function TCarbonWidgetSet.AngleChord(DC: HDC; x1, y1, x2, y2, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited AngleChord(DC, x1, y1, x2, y2, angle1, angle2);
end;

function TCarbonWidgetSet.BeginPaint(Handle: hWnd; var PS: TPaintStruct): hdc;
begin
  Result:=inherited BeginPaint(Handle, PS);
end;

function TCarbonWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result:=inherited BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop
    );
end;

function TCarbonWidgetSet.CallNextHookEx(hHk: HHOOK; ncode: Integer;
  wParam: WParam; lParam: LParam): Integer;
begin
  Result:=inherited CallNextHookEx(hHk, ncode, wParam, lParam);
end;

function TCarbonWidgetSet.CallWindowProc(lpPrevWndFunc: TFarProc; Handle: HWND;
  Msg: UINT; wParam: WParam; lParam: lParam): Integer;
begin
  Result:=inherited CallWindowProc(lpPrevWndFunc, Handle, Msg, wParam, lParam);
end;

function TCarbonWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
begin
  Result:=inherited ClientToScreen(Handle, P);
end;

function TCarbonWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat
  ): string;
begin
  Result:=inherited ClipboardFormatToMimeType(FormatID);
end;

function TCarbonWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  Result:=inherited ClipboardGetData(ClipboardType, FormatID, Stream);
end;

function TCarbonWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result:=inherited ClipboardGetFormats(ClipboardType, Count, List);
end;

function TCarbonWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result:=inherited ClipboardGetOwnerShip(ClipboardType, OnRequestProc,
    FormatCount, Formats);
end;

function TCarbonWidgetSet.ClipboardRegisterFormat(const AMimeType: string
  ): TClipboardFormat;
begin
  Result:=inherited ClipboardRegisterFormat(AMimeType);
end;

function TCarbonWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
begin
  Result:=inherited CombineRgn(Dest, Src1, Src2, fnCombineMode);
end;

function TCarbonWidgetSet.ComboBoxDropDown(Handle: HWND; DropDown: boolean
  ): boolean;
begin
  Result:=inherited ComboBoxDropDown(Handle, DropDown);
end;

function TCarbonWidgetSet.CreateBitmap(Width, Height: Integer; Planes,
  BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  Result:=inherited CreateBitmap(Width, Height, Planes, BitCount, BitmapBits);
end;

function TCarbonWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
begin
  Result:=inherited CreateBitmapFromRawImage(RawImage, Bitmap, MaskBitmap,
    AlwaysCreateMask);
end;

function TCarbonWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush
  ): HBRUSH;
begin
  Result:=inherited CreateBrushIndirect(LogBrush);
end;

function TCarbonWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap; width,
  Height: Integer): Boolean;
begin
  Result:=inherited CreateCaret(Handle, Bitmap, width, Height);
end;

function TCarbonWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer
  ): HBITMAP;
begin
  Result:=inherited CreateCompatibleBitmap(DC, Width, Height);
end;

function TCarbonWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  Result:=inherited CreateCompatibleDC(DC);
end;

function TCarbonWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result:=inherited CreateFontIndirect(LogFont);
end;

function TCarbonWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result:=inherited CreateFontIndirectEx(LogFont, LongFontName);
end;

function TCarbonWidgetSet.CreatePalette(const LogPalette: TLogPalette
  ): HPALETTE;
begin
  Result:=inherited CreatePalette(LogPalette);
end;

function TCarbonWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result:=inherited CreatePenIndirect(LogPen);
end;

function TCarbonWidgetSet.CreatePixmapIndirect(const Data: Pointer;
  const TransColor: Longint): HBITMAP;
begin
  Result:=inherited CreatePixmapIndirect(Data, TransColor);
end;

function TCarbonWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  Result:=inherited CreatePolygonRgn(Points, NumPts, FillMode);
end;

function TCarbonWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  Result:=inherited CreateRectRgn(X1, Y1, X2, Y2);
end;

procedure TCarbonWidgetSet.DeleteCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.DoneCriticalsection(ACritSec^);
  Dispose(ACritSec);
  CritSection:=0;
end;

function TCarbonWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  Result:=inherited DeleteDC(hDC);
end;

function TCarbonWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result:=inherited DeleteObject(GDIObject);
end;

function TCarbonWidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
  Result:=inherited DestroyCaret(Handle);
end;

function TCarbonWidgetSet.DrawFrameControl(DC: HDC; var Rect: TRect; uType,
  uState: Cardinal): Boolean;
begin
  Result:=inherited DrawFrameControl(DC, Rect, uType, uState);
end;

function TCarbonWidgetSet.DrawEdge(DC: HDC; var ARect: TRect; Edge: Cardinal;
  grfFlags: Cardinal): Boolean;
begin
  Result:=inherited DrawEdge(DC, ARect, Edge, grfFlags);
end;

function TCarbonWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var Rect: TRect; Flags: Cardinal): Integer;
begin
  Result:=inherited DrawText(DC, Str, Count, Rect, Flags);
end;

function TCarbonWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
begin
  Result:=inherited Ellipse(DC, x1, y1, x2, y2);
end;

function TCarbonWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal
  ): Boolean;
begin
  Result:=inherited EnableScrollBar(Wnd, wSBflags, wArrows);
end;

function TCarbonWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result:=inherited EnableWindow(hWnd, bEnable);
end;

function TCarbonWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct
  ): Integer;
begin
  Result:=inherited EndPaint(Handle, PS);
end;

procedure TCarbonWidgetSet.EnterCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.EnterCriticalsection(ACritSec^);
end;

function TCarbonWidgetSet.ExcludeClipRect(dc: hdc; Left, Top, Right,
  Bottom: Integer): Integer;
begin
  Result:=inherited ExcludeClipRect(dc, Left, Top, Right, Bottom);
end;

function TCarbonWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=inherited ExtTextOut(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function TCarbonWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint
  ): Integer;
begin
  Result:=inherited ExtSelectClipRGN(dc, rgn, Mode);
end;

function TCarbonWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH
  ): Boolean;
begin
  Result:=inherited FillRect(DC, Rect, Brush);
end;

function TCarbonWidgetSet.Frame(DC: HDC; const ARect: TRect): Integer;
begin
  Result:=inherited Frame(DC, ARect);
end;

function TCarbonWidgetSet.Frame3d(DC: HDC; var ARect: TRect;
  const FrameWidth: integer; const Style: TBevelCut): Boolean;
begin
  Result:=inherited Frame3d(DC, ARect, FrameWidth, Style);
end;

function TCarbonWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH
  ): Integer;
begin
  Result:=inherited FrameRect(DC, ARect, hBr);
end;

function TCarbonWidgetSet.GetActiveWindow: HWND;
var
  Window: WindowRef;
begin
  Result := 0;//Inherited GetFocus;
  Window := GetWindowList;
  while (Window <> nil) and not IsWindowActive(Window) do begin
    Window := GetNextWindow(Window);
  end;
  Result := HWND(Window);
end;

function TCarbonWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;
  Bits: Pointer): Longint;
begin
  Result:=inherited GetBitmapBits(Bitmap, Count, Bits);
end;

function TCarbonWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): boolean;
begin
  Result:=inherited GetBitmapRawImageDescription(Bitmap, Desc);
end;

function TCarbonWidgetSet.GetCapture: HWND;
begin
  Result:=inherited GetCapture;
end;

function TCarbonWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result:=inherited GetCaretPos(lpPoint);
end;

function TCarbonWidgetSet.GetCaretRespondToFocus(handle: HWND;
  var ShowHideOnFocus: boolean): Boolean;
begin
  Result:=inherited GetCaretRespondToFocus(handle, ShowHideOnFocus);
end;

function TCarbonWidgetSet.GetCharABCWidths(DC: HDC; p2, p3: UINT;
  const ABCStructs): Boolean;
begin
  Result:=inherited GetCharABCWidths(DC, p2, p3, ABCStructs);
end;

function TCarbonWidgetSet.GetClientBounds(handle: HWND; var ARect: TRect): Boolean;
var
 Info: PWidgetInfo;
 WinControl: TWinControl;
begin
  Result := False;
  Info := GetwidgetInfo(Pointer(Handle));
  if Info = nil then Exit;
  
  WinControl := TWinControl(Info^.LCLObject);
  Result := TCarbonWSWinControlClass(WinControl.WidgetSetClass).GetClientBounds(WinControl, ARect);
end;

function TCarbonWidgetSet.GetClientRect(handle: HWND; var ARect: TRect): Boolean;
var
 Info: PWidgetInfo;
 WinControl: TWinControl;
begin
  Result := False;
  Info := GetwidgetInfo(Pointer(Handle));
  if Info = nil then Exit;
  
  WinControl := TWinControl(Info^.LCLObject);
  Result := TCarbonWSWinControlClass(WinControl.WidgetSetClass).GetClientRect(WinControl, ARect);
end;

function TCarbonWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
begin
  Result:=inherited GetClipBox(DC, lpRect);
end;

function TCarbonWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
begin
  Result:=inherited GetClipRGN(DC, RGN);
end;

function TCarbonWidgetSet.GetCmdLineParamDescForInterface: string;
begin
  Result:=inherited GetCmdLineParamDescForInterface;
end;

function TCarbonWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
var
  Pt: Point;
begin
  GetGlobalMouse(Pt);
  lpPoint.X := Pt.h;
  lpPoint.Y := Pt.v;
  Result := True;
end;

function TCarbonWidgetSet.GetDC(hWnd: HWND): HDC;
var
  Info: PWidgetInfo;
begin
  if hWnd=0 then
    Result:=HDC(CreateDefaultDevContext)
  else begin
    Info:=GetWidgetInfo(Pointer(HWnd));
    Result:=HDC(CreateDevContextForInfo(Info));
  end;
end;

function TCarbonWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
begin
  Result:=inherited GetDCOriginRelativeToWindow(PaintDC, WindowHandle,
    OriginDiff);
end;

function TCarbonWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result:=inherited GetDesignerDC(WindowHandle);
end;

function TCarbonWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result:=inherited GetDeviceCaps(DC, Index);
end;

function TCarbonWidgetSet.GetDeviceRawImageDescription(DC: HDC;
  Desc: PRawImageDescription): boolean;
begin
  Result:=inherited GetDeviceRawImageDescription(DC, Desc);
end;

function TCarbonWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  Result:=inherited GetDeviceSize(DC, p);
end;

function TCarbonWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan,
  NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT
  ): Integer;
begin
  Result:=inherited GetDIBits(DC, Bitmap, StartScan, NumScans, Bits, BitInfo,
    Usage);
end;

function TCarbonWidgetSet.GetFocus: HWND;
var
  Control: ControlRef;
  Window: WindowRef;
begin
  Result := 0;//Inherited GetFocus;
  Window := GetWindowList;
  while (Window <> nil) and not IsWindowActive(Window) do begin
    Window := GetNextWindow(Window);
  end;
  if Window = nil then Exit;
  Control := nil;
  GetKeyboardFocus(Window, Control);
  Result := HWND(Control);
end;

function TCarbonWidgetSet.GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result:=inherited GetFontLanguageInfo(DC);
end;

function TCarbonWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result:=0;
  
  case nVirtKey of
  VK_MENU:
    if (GetCurrentKeyModifiers and optionKey)>0 then
      // the ssAlt/VK_MENU is mapped to optionKey under MacOS
      Result:=-1;
  VK_SHIFT:
    if (GetCurrentKeyModifiers and shiftKey)>0 then
      Result:=-1;
  VK_CONTROL:
    if (GetCurrentKeyModifiers and cmdKey)>0 then
      // the ssCtrl/VK_CONTROL is mapped to optionKey under MacOS
      Result:=-1;
  else
    debugln('TCarbonWidgetSet.GetKeyState TODO ',dbgs(nVirtkey));
  end;
end;

function TCarbonWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer;
  Buf: Pointer): Integer;
begin
  Result:=inherited GetObject(GDIObj, BufSize, Buf);
end;

function TCarbonWidgetSet.GetParent(Handle: HWND): HWND;
begin
  Result:=inherited GetParent(Handle);
end;

function TCarbonWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
begin
  Result:=inherited GetProp(Handle, Str);
end;

function TCarbonWidgetSet.GetRawImageFromDevice(SrcDC: HDC;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
begin
  Result:=inherited GetRawImageFromDevice(SrcDC, SrcRect, NewRawImage);
end;

function TCarbonWidgetSet.GetRawImageFromBitmap(SrcBitmap,
  SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage
  ): boolean;
begin
  Result:=inherited GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap, SrcRect,
    NewRawImage);
end;

function TCarbonWidgetSet.GetRgnBox(RGN: HRGN; lpRect: PRect): Longint;
begin
  Result:=inherited GetRgnBox(RGN, lpRect);
end;

function TCarbonWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer
  ): integer;
begin
  Result:=inherited GetScrollBarSize(Handle, BarKind);
end;

function TCarbonWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer
  ): boolean;
begin
  Result:=inherited GetScrollbarVisible(Handle, SBStyle);
end;

function TCarbonWidgetSet.GetScrollInfo(Handle: HWND; SBStyle: Integer;
  var ScrollInfo: TScrollInfo): Boolean;
begin
  Result:=inherited GetScrollInfo(Handle, SBStyle, ScrollInfo);
end;

function TCarbonWidgetSet.GetStockObject(Value: Integer): LongInt;
begin
  Result:=inherited GetStockObject(Value);
end;

function TCarbonWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  Result:=inherited GetSysColor(nIndex);
end;

function TCarbonWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result:=inherited GetSystemMetrics(nIndex);
end;

function TCarbonWidgetSet.GetTextColor(DC: HDC): TColorRef;
begin
  Result:=inherited GetTextColor(DC);
end;

function TCarbonWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
begin
  Result:=inherited GetTextExtentPoint(DC, Str, Count, Size);
end;

function TCarbonWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric
  ): Boolean;
begin
  Result:=inherited GetTextMetrics(DC, TM);
end;

function TCarbonWidgetSet.GetWindowLong(Handle: hwnd; int: Integer): PtrInt;
begin
  Result:=inherited GetWindowLong(Handle, int);
end;

function TCarbonWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
begin
  Result:=inherited GetWindowOrgEx(dc, P);
end;

function TCarbonWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect
  ): Integer;
begin
  Result:=inherited GetWindowRect(Handle, ARect);
end;

function TCarbonWidgetSet.GetWindowRelativePosition(Handle: hwnd; var Left,
  Top: integer): boolean;
begin
  Result:=inherited GetWindowRelativePosition(Handle, Left, Top);
end;

function TCarbonWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: integer
  ): boolean;
var
  AWndRect: FPCMacOSAll.Rect;
  Info: PWidgetInfo;
begin
  Info:=GetWidgetInfo(Pointer(Handle));
  if Info^.widgetType=cwtWindowRef then begin
    Result := GetWindowBounds(WindowRef(Handle),kWindowStructureRgn, AWndRect) = 0;
  end else begin
    Result :=  GetControlBounds(ControlRef(Handle), AWndRect) <>nil;
  end;
  if not Result then Exit;
  Width := AWndRect.right-AWndRect.left;
  Height := AWndRect.bottom-AWndRect.top;
end;

function TCarbonWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex;
  NumVertices: Longint; Meshes: Pointer; NumMeshes: Longint; Mode: Longint
  ): Boolean;
begin
  Result:=inherited GradientFill(DC, Vertices, NumVertices, Meshes, NumMeshes,
    Mode);
end;

function TCarbonWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  Result:=inherited HideCaret(hWnd);
end;

procedure TCarbonWidgetSet.InitializeCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

function TCarbonWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right,
  Bottom: Integer): Integer;
begin
  Result:=inherited IntersectClipRect(dc, Left, Top, Right, Bottom);
end;

function TCarbonWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect;
  bErase: Boolean): Boolean;
begin
  Result:=inherited InvalidateRect(aHandle, Rect, bErase);
end;

function TCarbonWidgetSet.IsWindowVisible(handle: HWND): boolean;
begin
  Result:=inherited IsWindowVisible(handle);
  DebugLn('TODO:  TCarbonWidgetSet.IsWindowVisible');
end;

procedure TCarbonWidgetSet.LeaveCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

function TCarbonWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result:=inherited LineTo(DC, X, Y);
end;

function TCarbonWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): integer;
begin
  Result:=inherited MessageBox(hWnd, lpText, lpCaption, uType);
end;

function TCarbonWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint
  ): Boolean;
begin
  Result:=inherited MoveToEx(DC, X, Y, OldPoint);
end;

function TCarbonWidgetSet.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
begin
  Result:=inherited MoveWindowOrgEx(DC, dX, dY);
end;

function TCarbonWidgetSet.PairSplitterAddSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=inherited PairSplitterAddSide(SplitterHandle, SideHandle, Side);
end;

function TCarbonWidgetSet.PairSplitterGetInterfaceInfo: Boolean;
begin
  Result:=inherited PairSplitterGetInterfaceInfo;
end;

function TCarbonWidgetSet.PairSplitterRemoveSide(SplitterHandle,
  SideHandle: hWnd; Side: integer): Boolean;
begin
  Result:=inherited PairSplitterRemoveSide(SplitterHandle, SideHandle, Side);
end;

function TCarbonWidgetSet.PairSplitterSetPosition(SplitterHandle: hWnd;
  var NewPosition: integer): Boolean;
begin
  Result:=inherited PairSplitterSetPosition(SplitterHandle, NewPosition);
end;

function TCarbonWidgetSet.PeekMessage(var lpMsg: TMsg; Handle: HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): Boolean;
begin
  Result:=inherited PeekMessage(lpMsg, Handle, wMsgFilterMin, wMsgFilterMax,
    wRemoveMsg);
end;

function TCarbonWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: boolean): boolean;
begin
  Result:=inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
end;

function TCarbonWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: boolean): boolean;
begin
  Result:=inherited Polygon(DC, Points, NumPts, Winding);
end;

function TCarbonWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer
  ): boolean;
begin
  Result:=inherited Polyline(DC, Points, NumPts);
end;

function TCarbonWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): Boolean;
begin
  Result:=inherited PostMessage(Handle, Msg, wParam, lParam);
end;

function TCarbonWidgetSet.RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialArc(DC, left, top, right, bottom, sx, sy, ex, ey);
end;

function TCarbonWidgetSet.RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialChord(DC, x1, y1, x2, y2, sx, sy, ex, ey);
end;

function TCarbonWidgetSet.RadialPie(DC: HDC; x1, y1, x2, y2, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialPie(DC, x1, y1, x2, y2, sx, sy, ex, ey);
end;

function TCarbonWidgetSet.RealizePalette(DC: HDC): Cardinal;
begin
  Result:=inherited RealizePalette(DC);
end;

function TCarbonWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result:=inherited Rectangle(DC, X1, Y1, X2, Y2);
end;

function TCarbonWidgetSet.RectVisible(dc: hdc; const ARect: TRect): Boolean;
begin
  Result:=inherited RectVisible(dc, ARect);
end;

function TCarbonWidgetSet.RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer
  ): Boolean;
begin
  Result:=inherited RegroupMenuItem(hndMenu, GroupIndex);
end;

function TCarbonWidgetSet.ReleaseCapture: Boolean;
begin
  Result:=inherited ReleaseCapture;
end;

function TCarbonWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  FreeDevContext(TDeviceContext(DC));
  Result:=0;
end;

function TCarbonWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result:=inherited RestoreDC(DC, SavedDC);
end;

function TCarbonWidgetSet.RoundRect(DC: hDC; X1, Y1, X2, Y2: Integer; RX,
  RY: Integer): Boolean;
begin
  Result:=inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

function TCarbonWidgetSet.SaveDC(DC: HDC): Integer;
begin
  Result:=inherited SaveDC(DC);
end;

function TCarbonWidgetSet.ScreenToClient(Handle: HWND; var P: TPoint): Integer;
begin
  Result:=inherited ScreenToClient(Handle, P);
end;

function TCarbonWidgetSet.ScrollWindowEx(hWnd: HWND; dx, dy: Integer;
  prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT
  ): Boolean;
begin
  Result:=inherited ScrollWindowEx(hWnd, dx, dy, prcScroll, prcClip,
    hrgnUpdate, prcUpdate, flags);
end;

function TCarbonWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
begin
  Result:=inherited SelectClipRGN(DC, RGN);
end;

function TCarbonWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result:=inherited SelectObject(DC, GDIObj);
end;

function TCarbonWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE;
  ForceBackground: Boolean): HPALETTE;
begin
  Result:=inherited SelectPalette(DC, Palette, ForceBackground);
end;

function TCarbonWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): LResult;
begin
  Result:=inherited SendMessage(HandleWnd, Msg, wParam, lParam);
end;

function TCarbonWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  Result := 0;
  if Handle = 0 then exit; 
  Result := GetActiveWindow;
  if ActivateWindow(WindowRef(Handle), True) <> NoErr then Result := 0;
end;

function TCarbonWidgetSet.SetBkColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result:=inherited SetBkColor(DC, Color);
end;

function TCarbonWidgetSet.SetBkMode(DC: HDC; bkMode: Integer): Integer;
begin
  Result:=inherited SetBkMode(DC, bkMode);
end;

function TCarbonWidgetSet.SetCapture(AHandle: HWND): HWND;
begin
  Result:=inherited SetCapture(AHandle);
end;

function TCarbonWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result:=inherited SetCaretPos(X, Y);
end;

function TCarbonWidgetSet.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result:=inherited SetCaretPosEx(Handle, X, Y);
end;

function TCarbonWidgetSet.SetCaretRespondToFocus(handle: HWND;
  ShowHideOnFocus: boolean): Boolean;
begin
  Result:=inherited SetCaretRespondToFocus(handle, ShowHideOnFocus);
end;

function TCarbonWidgetSet.SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result:=inherited SetComboMinDropDownSize(Handle, MinItemsWidth,
    MinItemsHeight, MinItemCount);
end;

function TCarbonWidgetSet.SetFocus(hWnd: HWND): HWND;
begin
  Result:=inherited SetFocus(hWnd);
  DebugLn('TODO:  TCarbonWidgetSet.SetFocus');
end;

function TCarbonWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer
  ): Boolean;
begin
  Result:=inherited SetProp(Handle, Str, Data);
end;

function TCarbonWidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer;
  ScrollInfo: TScrollInfo; bRedraw: Boolean): Integer;
begin
  Result:=inherited SetScrollInfo(Handle, SBStyle, ScrollInfo, bRedraw);
end;

function TCarbonWidgetSet.SetSysColors(cElements: Integer; const lpaElements;
  const lpaRgbValues): Boolean;
begin
  Result:=inherited SetSysColors(cElements, lpaElements, lpaRgbValues);
end;

function TCarbonWidgetSet.SetTextCharacterExtra(_hdc: hdc; nCharExtra: Integer
  ): Integer;
begin
  Result:=inherited SetTextCharacterExtra(_hdc, nCharExtra);
end;

function TCarbonWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result:=inherited SetTextColor(DC, Color);
end;

function TCarbonWidgetSet.SetWindowLong(Handle: HWND; Idx: Integer;
  NewLong: PtrInt): PtrInt;
begin
  Result:=inherited SetWindowLong(Handle, Idx, NewLong);
end;

function TCarbonWidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
begin
  Result:=inherited SetWindowOrgEx(DC, NewX, NewY, OldPoint);
end;

function TCarbonWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y,
  cx, cy: Integer; uFlags: UINT): Boolean;
begin
  Result:=inherited SetWindowPos(hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
end;

function TCarbonWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result:=inherited ShowCaret(hWnd);
end;

function TCarbonWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer;
  bShow: Boolean): Boolean;
begin
  Result:=inherited ShowScrollBar(Handle, wBar, bShow);
end;

function TCarbonWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result:=inherited ShowWindow(hWnd, nCmdShow);
end;

function TCarbonWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal
  ): Boolean;
begin
  Result:=inherited StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc,
    SrcWidth, SrcHeight, ROp);
end;

function TCarbonWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result:=inherited StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc,
    YSrc, SrcWidth, SrcHeight, Mask, XMask, YMask, Rop);
end;

function TCarbonWidgetSet.TextOut(DC: HDC; X, Y: Integer; Str: Pchar;
  Count: Integer): Boolean;
begin
  Result:=inherited TextOut(DC, X, Y, Str, Count);
end;

function TCarbonWidgetSet.VkKeyScan(AChar: Char): Short;
begin
  Result:=inherited VkKeyScan(AChar);
end;

function TCarbonWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  Result:=inherited WindowFromPoint(Point);
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

