{%MainUnit ../lclintf.pp}

(******************************************************************************
  All Winapi related stuff goes here.
  This file is used by LCLIntf.pas
  If a procedure is platform dependent then it should call:
    WidgetSet.MyDependentProc

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

  Note:
    the section for not referring WidgetSet is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

{function TWinCEWidgetSet.Arc(DC: HDC; Left, Top, width, height, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited Arc(DC, Left, Top, width, height, angle1, angle2);
end;

function TWinCEWidgetSet.AngleChord(DC: HDC; x, y, width, height, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited AngleChord(DC, x, y, width, height, angle1, angle2);
end;

function TWinCEWidgetSet.BeginPaint(Handle: hWnd; var PS: TPaintStruct): hdc;
begin
  Result:=inherited BeginPaint(Handle, PS);
end;

function TWinCEWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result:=inherited BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop
    );
end;

function TWinCEWidgetSet.CallNextHookEx(hHk: HHOOK; ncode: Integer;
  wParam: WParam; lParam: LParam): Integer;
begin
  Result:=inherited CallNextHookEx(hHk, ncode, wParam, lParam);
end;

function TWinCEWidgetSet.CallWindowProc(lpPrevWndFunc: TFarProc; Handle: HWND;
  Msg: UINT; wParam: WParam; lParam: lParam): Integer;
begin
  Result:=inherited CallWindowProc(lpPrevWndFunc, Handle, Msg, wParam, lParam);
end;

function TWinCEWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
begin
  Result:=inherited ClientToScreen(Handle, P);
end;

function TWinCEWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat
  ): string;
begin
  Result:=inherited ClipboardFormatToMimeType(FormatID);
end;

function TWinCEWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  Result:=inherited ClipboardGetData(ClipboardType, FormatID, Stream);
end;

function TWinCEWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result:=inherited ClipboardGetFormats(ClipboardType, Count, List);
end;

function TWinCEWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result:=inherited ClipboardGetOwnerShip(ClipboardType, OnRequestProc,
    FormatCount, Formats);
end;

function TWinCEWidgetSet.ClipboardRegisterFormat(const AMimeType: string
  ): TClipboardFormat;
begin
  Result:=inherited ClipboardRegisterFormat(AMimeType);
end;

function TWinCEWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
begin
  Result:=inherited CombineRgn(Dest, Src1, Src2, fnCombineMode);
end;

function TWinCEWidgetSet.ComboBoxDropDown(Handle: HWND; DropDown: boolean
  ): boolean;
begin
  Result:=inherited ComboBoxDropDown(Handle, DropDown);
end;

function TWinCEWidgetSet.CreateBitmap(Width, Height: Integer; Planes,
  BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  Result:=inherited CreateBitmap(Width, Height, Planes, BitCount, BitmapBits);
end;

function TWinCEWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
begin
  Result:=inherited CreateBitmapFromRawImage(RawImage, Bitmap, MaskBitmap,
    AlwaysCreateMask);
end;

function TWinCEWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush
  ): HBRUSH;
begin
  Result:=inherited CreateBrushIndirect(LogBrush);
end;

function TWinCEWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap; width,
  Height: Integer): Boolean;
begin
  Result:=inherited CreateCaret(Handle, Bitmap, width, Height);
end;

function TWinCEWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer
  ): HBITMAP;
begin
  Result:=inherited CreateCompatibleBitmap(DC, Width, Height);
end;

function TWinCEWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  Result:=inherited CreateCompatibleDC(DC);
end;

function TWinCEWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result:=inherited CreateFontIndirect(LogFont);
end;

function TWinCEWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result:=inherited CreateFontIndirectEx(LogFont, LongFontName);
end;

function TWinCEWidgetSet.CreatePalette(const LogPalette: TLogPalette
  ): HPALETTE;
begin
  Result:=inherited CreatePalette(LogPalette);
end;

function TWinCEWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result:=inherited CreatePenIndirect(LogPen);
end;

function TWinCEWidgetSet.CreatePixmapIndirect(const Data: Pointer;
  const TransColor: Longint): HBITMAP;
begin
  Result:=inherited CreatePixmapIndirect(Data, TransColor);
end;

function TWinCEWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  Result:=inherited CreatePolygonRgn(Points, NumPts, FillMode);
end;

function TWinCEWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  Result:=inherited CreateRectRgn(X1, Y1, X2, Y2);
end;

procedure TWinCEWidgetSet.DeleteCriticalSection(
  var CritSection: TCriticalSection);
begin
  inherited DeleteCriticalSection(CritSection);
end;

function TWinCEWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  Result:=inherited DeleteDC(hDC);
end;

function TWinCEWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result:=inherited DeleteObject(GDIObject);
end;

function TWinCEWidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
  Result:=inherited DestroyCaret(Handle);
end;

function TWinCEWidgetSet.DrawFrameControl(DC: HDC; var Rect: TRect; uType,
  uState: Cardinal): Boolean;
begin
  Result:=inherited DrawFrameControl(DC, Rect, uType, uState);
end;

function TWinCEWidgetSet.DrawEdge(DC: HDC; var ARect: TRect; Edge: Cardinal;
  grfFlags: Cardinal): Boolean;
begin
  Result:=inherited DrawEdge(DC, ARect, Edge, grfFlags);
end;

function TWinCEWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var Rect: TRect; Flags: Cardinal): Integer;
begin
  Result:=inherited DrawText(DC, Str, Count, Rect, Flags);
end;

function TWinCEWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
begin
  Result:=inherited Ellipse(DC, x1, y1, x2, y2);
end;

function TWinCEWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal
  ): Boolean;
begin
  Result:=inherited EnableScrollBar(Wnd, wSBflags, wArrows);
end;

function TWinCEWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result:=inherited EnableWindow(hWnd, bEnable);
end;

function TWinCEWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
begin
  Result:=inherited EndPaint(Handle, PS);
end;

procedure TWinCEWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection
  );
begin
  inherited EnterCriticalSection(CritSection);
end;

function TWinCEWidgetSet.EnumFontFamilies(DC: HDC; Family: Pchar;
  EnumFontFamProc: FontEnumProc; LParam: Lparam): longint;
begin
  Result:=inherited EnumFontFamilies(DC, Family, EnumFontFamProc, LParam);
end;

function TWinCEWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin
  Result:=inherited EnumFontFamiliesEx(DC, lpLogFont, Callback, Lparam, Flags);
end;

function TWinCEWidgetSet.ExcludeClipRect(dc: hdc; Left, Top, Right,
  Bottom: Integer): Integer;
begin
  Result:=inherited ExcludeClipRect(dc, Left, Top, Right, Bottom);
end;

function TWinCEWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=inherited ExtTextOut(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function TWinCEWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint
  ): Integer;
begin
  Result:=inherited ExtSelectClipRGN(dc, rgn, Mode);
end;

function TWinCEWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH
  ): Boolean;
begin
  Result:=inherited FillRect(DC, Rect, Brush);
end;

function TWinCEWidgetSet.Frame(DC: HDC; const ARect: TRect): Integer;
begin
  Result:=inherited Frame(DC, ARect);
end;

function TWinCEWidgetSet.Frame3d(DC: HDC; var ARect: TRect;
  const FrameWidth: integer; const Style: TBevelCut): Boolean;
begin
  Result:=inherited Frame3d(DC, ARect, FrameWidth, Style);
end;

function TWinCEWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH
  ): Integer;
begin
  Result:=inherited FrameRect(DC, ARect, hBr);
end;

function TWinCEWidgetSet.GetActiveWindow: HWND;
begin
  Result:=inherited GetActiveWindow;
end;

function TWinCEWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;
  Bits: Pointer): Longint;
begin
  Result:=inherited GetBitmapBits(Bitmap, Count, Bits);
end;

function TWinCEWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): boolean;
begin
  Result:=inherited GetBitmapRawImageDescription(Bitmap, Desc);
end;

function TWinCEWidgetSet.GetCapture: HWND;
begin
  Result:=inherited GetCapture;
end;

function TWinCEWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result:=inherited GetCaretPos(lpPoint);
end;

function TWinCEWidgetSet.GetCaretRespondToFocus(handle: HWND;
  var ShowHideOnFocus: boolean): Boolean;
begin
  Result:=inherited GetCaretRespondToFocus(handle, ShowHideOnFocus);
end;

function TWinCEWidgetSet.GetCharABCWidths(DC: HDC; p2, p3: UINT;
  const ABCStructs): Boolean;
begin
  Result:=inherited GetCharABCWidths(DC, p2, p3, ABCStructs);
end;

function TWinCEWidgetSet.GetClientBounds(handle: HWND; var ARect: TRect
  ): Boolean;
begin
  Result:=inherited GetClientBounds(handle, ARect);
end;

function TWinCEWidgetSet.GetClientRect(handle: HWND; var ARect: TRect
  ): Boolean;
begin
  Result:=inherited GetClientRect(handle, ARect);
end;

function TWinCEWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
begin
  Result:=inherited GetClipBox(DC, lpRect);
end;

function TWinCEWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
begin
  Result:=inherited GetClipRGN(DC, RGN);
end;

function TWinCEWidgetSet.GetCmdLineParamDescForInterface: string;
begin
  Result:=inherited GetCmdLineParamDescForInterface;
end;

function TWinCEWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  Result:=inherited GetCursorPos(lpPoint);
end;

function TWinCEWidgetSet.GetDC(hWnd: HWND): HDC;
begin
  Result:=inherited GetDC(hWnd);
end;

function TWinCEWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
begin
  Result:=inherited GetDCOriginRelativeToWindow(PaintDC, WindowHandle,
    OriginDiff);
end;

function TWinCEWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result:=inherited GetDesignerDC(WindowHandle);
end;

function TWinCEWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result:=inherited GetDeviceCaps(DC, Index);
end;

function TWinCEWidgetSet.GetDeviceRawImageDescription(DC: HDC;
  Desc: PRawImageDescription): boolean;
begin
  Result:=inherited GetDeviceRawImageDescription(DC, Desc);
end;

function TWinCEWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  Result:=inherited GetDeviceSize(DC, p);
end;

function TWinCEWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan,
  NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT
  ): Integer;
begin
  Result:=inherited GetDIBits(DC, Bitmap, StartScan, NumScans, Bits, BitInfo,
    Usage);
end;

function TWinCEWidgetSet.GetFocus: HWND;
begin
  Result:=inherited GetFocus;
end;

function TWinCEWidgetSet.GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result:=inherited GetFontLanguageInfo(DC);
end;

function TWinCEWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result:=inherited GetKeyState(nVirtKey);
end;

function TWinCEWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer;
  Buf: Pointer): Integer;
begin
  Result:=inherited GetObject(GDIObj, BufSize, Buf);
end;

function TWinCEWidgetSet.GetParent(Handle: HWND): HWND;
begin
  Result:=inherited GetParent(Handle);
end;

function TWinCEWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
begin
  Result:=inherited GetProp(Handle, Str);
end;

function TWinCEWidgetSet.GetRawImageFromDevice(SrcDC: HDC;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
begin
  Result:=inherited GetRawImageFromDevice(SrcDC, SrcRect, NewRawImage);
end;

function TWinCEWidgetSet.GetRawImageFromBitmap(SrcBitmap,
  SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage
  ): boolean;
begin
  Result:=inherited GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap, SrcRect,
    NewRawImage);
end;

function TWinCEWidgetSet.GetRgnBox(RGN: HRGN; lpRect: PRect): Longint;
begin
  Result:=inherited GetRgnBox(RGN, lpRect);
end;

function TWinCEWidgetSet.GetROP2(DC: HDC): Integer;
begin
  Result:=inherited GetROP2(DC);
end;

function TWinCEWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer
  ): integer;
begin
  Result:=inherited GetScrollBarSize(Handle, BarKind);
end;

function TWinCEWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer
  ): boolean;
begin
  Result:=inherited GetScrollbarVisible(Handle, SBStyle);
end;

function TWinCEWidgetSet.GetScrollInfo(Handle: HWND; SBStyle: Integer;
  var ScrollInfo: TScrollInfo): Boolean;
begin
  Result:=inherited GetScrollInfo(Handle, SBStyle, ScrollInfo);
end;

function TWinCEWidgetSet.GetStockObject(Value: Integer): LongInt;
begin
  Result:=inherited GetStockObject(Value);
end;

function TWinCEWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  Result:=inherited GetSysColor(nIndex);
end;

function TWinCEWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result:=inherited GetSystemMetrics(nIndex);
end;

function TWinCEWidgetSet.GetTextColor(DC: HDC): TColorRef;
begin
  Result:=inherited GetTextColor(DC);
end;

function TWinCEWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
begin
  Result:=inherited GetTextExtentPoint(DC, Str, Count, Size);
end;

function TWinCEWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Result:=inherited GetTextMetrics(DC, TM);
end;

function TWinCEWidgetSet.GetWindowLong(Handle: hwnd; int: Integer): PtrInt;
begin
  Result:=inherited GetWindowLong(Handle, int);
end;

function TWinCEWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
begin
  Result:=inherited GetWindowOrgEx(dc, P);
end;

function TWinCEWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect
  ): Integer;
begin
  Result:=inherited GetWindowRect(Handle, ARect);
end;

function TWinCEWidgetSet.GetWindowRelativePosition(Handle: hwnd; var Left,
  Top: integer): boolean;
begin
  Result:=inherited GetWindowRelativePosition(Handle, Left, Top);
end;

function TWinCEWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: integer
  ): boolean;
begin
  Result:=inherited GetWindowSize(Handle, Width, Height);
end;

function TWinCEWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex;
  NumVertices: Longint; Meshes: Pointer; NumMeshes: Longint; Mode: Longint
  ): Boolean;
begin
  Result:=inherited GradientFill(DC, Vertices, NumVertices, Meshes, NumMeshes,
    Mode);
end;

function TWinCEWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  Result:=inherited HideCaret(hWnd);
end;

procedure TWinCEWidgetSet.InitializeCriticalSection(
  var CritSection: TCriticalSection);
begin
  inherited InitializeCriticalSection(CritSection);
end;

function TWinCEWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right,
  Bottom: Integer): Integer;
begin
  Result:=inherited IntersectClipRect(dc, Left, Top, Right, Bottom);
end;

function TWinCEWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect;
  bErase: Boolean): Boolean;
begin
  Result:=inherited InvalidateRect(aHandle, Rect, bErase);
end;

function TWinCEWidgetSet.IsWindowEnabled(handle: HWND): boolean;
begin
  Result:=inherited IsWindowEnabled(handle);
end;

function TWinCEWidgetSet.IsWindowVisible(handle: HWND): boolean;
begin
  Result:=inherited IsWindowVisible(handle);
end;

procedure TWinCEWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection
  );
begin
  inherited LeaveCriticalSection(CritSection);
end;

function TWinCEWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result:=inherited LineTo(DC, X, Y);
end;

function TWinCEWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): integer;
begin
  Result:=inherited MessageBox(hWnd, lpText, lpCaption, uType);
end;

function TWinCEWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint
  ): Boolean;
begin
  Result:=inherited MoveToEx(DC, X, Y, OldPoint);
end;

function TWinCEWidgetSet.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
begin
  Result:=inherited MoveWindowOrgEx(DC, dX, dY);
end;

function TWinCEWidgetSet.PairSplitterAddSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=inherited PairSplitterAddSide(SplitterHandle, SideHandle, Side);
end;

function TWinCEWidgetSet.PairSplitterGetInterfaceInfo: Boolean;
begin
  Result:=inherited PairSplitterGetInterfaceInfo;
end;

function TWinCEWidgetSet.PairSplitterRemoveSide(SplitterHandle,
  SideHandle: hWnd; Side: integer): Boolean;
begin
  Result:=inherited PairSplitterRemoveSide(SplitterHandle, SideHandle, Side);
end;

function TWinCEWidgetSet.PairSplitterSetPosition(SplitterHandle: hWnd;
  var NewPosition: integer): Boolean;
begin
  Result:=inherited PairSplitterSetPosition(SplitterHandle, NewPosition);
end;

function TWinCEWidgetSet.PeekMessage(var lpMsg: TMsg; Handle: HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): Boolean;
begin
  Result:=inherited PeekMessage(lpMsg, Handle, wMsgFilterMin, wMsgFilterMax,
    wRemoveMsg);
end;

function TWinCEWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: boolean): boolean;
begin
  Result:=inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
end;

function TWinCEWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: boolean): boolean;
begin
  Result:=inherited Polygon(DC, Points, NumPts, Winding);
end;

function TWinCEWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer
  ): boolean;
begin
  Result:=inherited Polyline(DC, Points, NumPts);
end;

function TWinCEWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): Boolean;
begin
  Result:=inherited PostMessage(Handle, Msg, wParam, lParam);
end;

function TWinCEWidgetSet.RadialArc(DC: HDC; x, y, width, height, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialArc(DC, x, y, width, height, sx, sy, ex, ey);
end;

function TWinCEWidgetSet.RadialChord(DC: HDC; x, y, width, height, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialChord(DC, x, y, width, height, sx, sy, ex, ey);
end;

function TWinCEWidgetSet.RadialPie(DC: HDC; x, y, width, height, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialPie(DC, x, y, width, height, sx, sy, ex, ey);
end;

function TWinCEWidgetSet.RealizePalette(DC: HDC): Cardinal;
begin
  Result:=inherited RealizePalette(DC);
end;

function TWinCEWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result:=inherited Rectangle(DC, X1, Y1, X2, Y2);
end;

function TWinCEWidgetSet.RectVisible(dc: hdc; const ARect: TRect): Boolean;
begin
  Result:=inherited RectVisible(dc, ARect);
end;

function TWinCEWidgetSet.RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer
  ): Boolean;
begin
  Result:=inherited RegroupMenuItem(hndMenu, GroupIndex);
end;

function TWinCEWidgetSet.ReleaseCapture: Boolean;
begin
  Result:=inherited ReleaseCapture;
end;

function TWinCEWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result:=inherited ReleaseDC(hWnd, DC);
end;

function TWinCEWidgetSet.RemoveProp(Handle: hwnd; Str: PChar): THandle;
begin
  Result:=inherited RemoveProp(Handle, Str);
end;

function TWinCEWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result:=inherited RestoreDC(DC, SavedDC);
end;

function TWinCEWidgetSet.RoundRect(DC: hDC; X1, Y1, X2, Y2: Integer; RX,
  RY: Integer): Boolean;
begin
  Result:=inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

function TWinCEWidgetSet.SaveDC(DC: HDC): Integer;
begin
  Result:=inherited SaveDC(DC);
end;

function TWinCEWidgetSet.ScreenToClient(Handle: HWND; var P: TPoint): Integer;
begin
  Result:=inherited ScreenToClient(Handle, P);
end;

function TWinCEWidgetSet.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll,
  prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result:=inherited ScrollWindowEx(hWnd, dx, dy, prcScroll, prcClip,
    hrgnUpdate, prcUpdate, flags);
end;

function TWinCEWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
begin
  Result:=inherited SelectClipRGN(DC, RGN);
end;

function TWinCEWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result:=inherited SelectObject(DC, GDIObj);
end;

function TWinCEWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE;
  ForceBackground: Boolean): HPALETTE;
begin
  Result:=inherited SelectPalette(DC, Palette, ForceBackground);
end;

function TWinCEWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): LResult;
begin
  Result:=inherited SendMessage(HandleWnd, Msg, wParam, lParam);
end;

function TWinCEWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  Result:=inherited SetActiveWindow(Handle);
end;

function TWinCEWidgetSet.SetBkColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result:=inherited SetBkColor(DC, Color);
end;

function TWinCEWidgetSet.SetBkMode(DC: HDC; bkMode: Integer): Integer;
begin
  Result:=inherited SetBkMode(DC, bkMode);
end;

function TWinCEWidgetSet.SetComboMinDropDownSize(Handle: HWND; MinItemsWidth,
  MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result:=inherited SetComboMinDropDownSize(Handle, MinItemsWidth,
    MinItemsHeight, MinItemCount);
end;

function TWinCEWidgetSet.SetCapture(AHandle: HWND): HWND;
begin
  Result:=inherited SetCapture(AHandle);
end;

function TWinCEWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result:=inherited SetCaretPos(X, Y);
end;

function TWinCEWidgetSet.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result:=inherited SetCaretPosEx(Handle, X, Y);
end;

function TWinCEWidgetSet.SetCaretRespondToFocus(handle: HWND;
  ShowHideOnFocus: boolean): Boolean;
begin
  Result:=inherited SetCaretRespondToFocus(handle, ShowHideOnFocus);
end;

function TWinCEWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  Result:=inherited SetCursorPos(X, Y);
end;

function TWinCEWidgetSet.SetFocus(hWnd: HWND): HWND;
begin
  Result:=inherited SetFocus(hWnd);
end;

function TWinCEWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer
  ): Boolean;
begin
  Result:=inherited SetProp(Handle, Str, Data);
end;

function TWinCEWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
begin
  Result:=inherited SetROP2(DC, Mode);
end;

function TWinCEWidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer;
  ScrollInfo: TScrollInfo; bRedraw: Boolean): Integer;
begin
  Result:=inherited SetScrollInfo(Handle, SBStyle, ScrollInfo, bRedraw);
end;

function TWinCEWidgetSet.SetSysColors(cElements: Integer; const lpaElements;
  const lpaRgbValues): Boolean;
begin
  Result:=inherited SetSysColors(cElements, lpaElements, lpaRgbValues);
end;

function TWinCEWidgetSet.SetTextCharacterExtra(_hdc: hdc; nCharExtra: Integer
  ): Integer;
begin
  Result:=inherited SetTextCharacterExtra(_hdc, nCharExtra);
end;

function TWinCEWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result:=inherited SetTextColor(DC, Color);
end;

function TWinCEWidgetSet.SetWindowLong(Handle: HWND; Idx: Integer;
  NewLong: PtrInt): PtrInt;
begin
  Result:=inherited SetWindowLong(Handle, Idx, NewLong);
end;

function TWinCEWidgetSet.SetWindowOrgEx(dc: hdc; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
begin
  Result:=inherited SetWindowOrgEx(dc, NewX, NewY, OldPoint);
end;

function TWinCEWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y,
  cx, cy: Integer; uFlags: UINT): Boolean;
begin
  Result:=inherited SetWindowPos(hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
end;

function TWinCEWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result:=inherited ShowCaret(hWnd);
end;

function TWinCEWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer;
  bShow: Boolean): Boolean;
begin
  Result:=inherited ShowScrollBar(Handle, wBar, bShow);
end;

function TWinCEWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result:=inherited ShowWindow(hWnd, nCmdShow);
end;

function TWinCEWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal
  ): Boolean;
begin
  Result:=inherited StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc,
    SrcWidth, SrcHeight, ROp);
end;

function TWinCEWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result:=inherited StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc,
    YSrc, SrcWidth, SrcHeight, Mask, XMask, YMask, Rop);
end;

function TWinCEWidgetSet.TextOut(DC: HDC; X, Y: Integer; Str: Pchar;
  Count: Integer): Boolean;
begin
  Result:=inherited TextOut(DC, X, Y, Str, Count);
end;

function TWinCEWidgetSet.VkKeyScan(AChar: Char): Short;
begin
  Result:=inherited VkKeyScan(AChar);
end;

function TWinCEWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  Result:=inherited WindowFromPoint(Point);
end;}

//##apiwiz##epi##   // Do not remove
