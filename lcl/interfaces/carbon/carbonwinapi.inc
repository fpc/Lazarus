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

{------------------------------------------------------------------------------
  Method:  ClientToScreen
  Params:  Handle - Handle of window
           P      - Record for coordinates
  Returns: If the function succeeds

  Converts the specified client coordinates to the screen coordinates
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
var
  R: TRect;
begin
  Result := GetWindowRect(Handle, R) <> 0;
  
  if Result then
  begin
    Inc(P.X, R.Left);
    Inc(P.Y, R.Top);
    
    Result := GetCarbonClientRect(Handle, R);
    
    if Result then
    begin
      Inc(P.X, R.Left);
      Inc(P.Y, R.Top);
    end;
  end;
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

{------------------------------------------------------------------------------
  Method:  CreateFontIndirect
  Params:  LogFont - Font characteristic
  Returns: Handle to the font

  Creates new font with specified characteristic
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := HFONT(TCarbonFont.Create(LogFont, LogFont.lfFaceName));
end;

{------------------------------------------------------------------------------
  Method:  CreateFontIndirectEx
  Params:  LogFont      - Font characteristic
           LongFontName - Font name
  Returns: Handle to the font

  Creates new font with specified characteristic and name
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result := HFONT(TCarbonFont.Create(LogFont, LongFontName));
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

{------------------------------------------------------------------------------
  Method:  DeleteObject
  Params:  GDIObject - Handle to the graphic object
  Returns: If the function succeeds

  Deletes a graphic object, freeing all system resources associated with the
  object
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  AObject: TObject;
begin
  Result := False;
  
  if not IsValidGDIObject(GDIObject) then Exit;
  
  AObject := TObject(GDIObject);
  
  if AObject is TCarbonFont then
  begin
    (AObject as TCarbonFont).Free;
    Result := True;
  end;
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

{------------------------------------------------------------------------------
  Method:  EnableWindow
  Params:  hWnd    - Handle to window
           bEnable - Whether to enable the window
  Returns: If the window was previously disabled

  Enables or disables mouse and keyboard input to the specified window or
  control
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
var
  Root: ControlRef;
begin
  case GetWidgetType(Pointer(hWnd)) of
  cwtWindowRef:
    begin
      GetRootControl(WindowRef(hWnd), Root);
      
      // enable/disable root
      // add/remove standard handler
      if bEnable then
      begin
        FPCMacOSAll.EnableControl(Root);
        ChangeWindowAttributes(WindowRef(hWnd), kWindowStandardHandlerAttribute,
          kWindowNoAttributes);
      end
      else
      begin
        FPCMacOSAll.DisableControl(Root);
        ChangeWindowAttributes(WindowRef(hWnd), kWindowNoAttributes,
          kWindowStandardHandlerAttribute);
      end;
    end;
  cwtControlRef:
    begin
      Result := not FPCMacOSAll.IsControlEnabled(ControlRef(hWnd));
      
      if bEnable then
        FPCMacOSAll.EnableControl(ControlRef(hWnd))
      else
        FPCMacOSAll.DisableControl(ControlRef(hWnd));
    end;
  end;
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

function TCarbonWidgetSet.EnumFontFamilies(DC: HDC; Family: Pchar;
  EnumFontFamProc: FontEnumProc; LParam: Lparam): Longint;
begin
  Result:=inherited EnumFontFamilies(DC, Family, EnumFontFamProc, LParam);
end;

{------------------------------------------------------------------------------
  Method:  EnumFontFamiliesEx
  Params:  DC        - Handle to the device context
           lpLogFont - Font characteristic to match
           Callback  - Callback function
           LParam    - Parameter to pass to the callback function.
           flags     - Not used
  Returns: The last value returned by callback function

  Enumerates all the font families in the system that match specified
  characteristic
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; LParam: Lparam; flags: dword): Longint;
var
  FamilyCount, NameLength: LongWord;
  FamilyListPtr, PFamily: ^ATSUFontID;
  FontName: PChar;
  EnumLogFont: TEnumLogFontEx;
  Metric: TNewTextMetricEx;
  FontType, I: Integer;
begin
  Result := 0;
  if (lpLogFont = nil) or not Assigned(Callback) then Exit;
  
  // enumarate ATSUI font families:
  
  if (lpLogFont^.lfCharSet = DEFAULT_CHARSET) and (lpLogFont^.lfFaceName = '') then
  begin
    // all system fonts
    if ATSUFontCount(FamilyCount) <> noErr then Exit;
    
    GetMem(FamilyListPtr, SizeOf(ATSUFontID) * FamilyCount);
    try
      if ATSUGetFontIDs(FamilyListPtr, FamilyCount, nil) <> noErr then Exit;

      PFamily := FamilyListPtr;
      for I := 0 to Pred(FamilyCount) do
      begin
        // retrieve font name length
        if ATSUFindFontName(PFamily^, kFontFamilyName, kFontNoPlatform,
          kFontNoScript, kFontNoLanguage, NameLength, nil,
          @NameLength, nil) <> noErr then Continue;
        
        GetMem(FontName, NameLength);
        FillChar(FontName^, NameLength, #0);
        try
          // retrieve font name
          if ATSUFindFontName(PFamily^, kFontFamilyName, kFontNoPlatform,
            kFontNoScript, kFontNoLanguage, NameLength,
            Pointer(FontName), @NameLength, nil) <> noErr then Continue;
            
          if FontName <> '' then // execute callback
          begin
            FillChar(EnumLogFont, SizeOf(EnumLogFont), #0);
            FillChar(Metric, SizeOf(Metric), #0);
            FontType := 0;
            EnumLogFont.elfLogFont.lfFaceName := FontName;
            // TODO: get all attributes
            
            Result := Callback(EnumLogFont, Metric, FontType, LParam);
          end;
        finally
          System.FreeMem(FontName);
        end;
        Inc(PFamily);
      end;
    finally
      System.FreeMem(FamilyListPtr);
    end;
  end
  else
  begin
    DebugLn('EnumFontFamiliesEx with specific face or specific char set is not implemented!');
  end;
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

{------------------------------------------------------------------------------
  Method:  GetActiveWindow
  Params:  None
  Returns: The handle to the active window

  Retrieves the window handle to the active window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetActiveWindow: HWND;
var
  Window: WindowRef;
begin
  Result := 0;
  Window := GetWindowList;
  while (Window <> nil) and not IsWindowActive(Window) do
    Window := GetNextWindow(Window);

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

{------------------------------------------------------------------------------
  Method:  GetClientBounds
  Params:  Handle - Handle of window
           Rect   - Record for client coordinates
  Returns: If the function succeeds

  Retrieves the local coordinates of a window's client area
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetClientBounds(Handle: HWND; var ARect: TRect): Boolean;
begin
  Result := GetCarbonClientRect(Handle, ARect);
end;

{------------------------------------------------------------------------------
  Method:  GetClientRect
  Params:  Handle - Handle of window
           Rect   - Record for client coordinates
  Returns: If the function succeeds

  Retrieves the dimension of a window's client area.
  Left and Top are always 0, 0.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetClientRect(Handle: HWND; var ARect: TRect): Boolean;
begin
  Result := GetCarbonClientRect(Handle, ARect);
  if Result then OffsetRect(ARect, -ARect.Left, -ARect.Top);
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

{------------------------------------------------------------------------------
  Method:  GetCursorPos
  Params:  lpPoint - Record for coordinates
  Returns: If the function succeeds

  Retrieves the global screen coordinates of the mouse cursor
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
var
  Pt: FPCMacOSAll.Point;
begin
  GetGlobalMouse(Pt);
  lpPoint.X := Pt.h;
  lpPoint.Y := Pt.v;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  GetDC
  Params:  HWnd - Handle of window
  Returns: Value identifying the device context for the given window's client
           area

  Retrieves a handle of a display device context (DC) for the client area of
  the specified window.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDC(HWnd: HWND): HDC;
var
  AInfo: PWidgetInfo;
begin
  if HWnd = 0 then Result := HDC(TCarbonDeviceContext.Create(nil))
  else
  begin
    AInfo := GetWidgetInfo(Pointer(HWnd));
    Result := HDC(TCarbonDeviceContext.Create(AInfo));
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

{------------------------------------------------------------------------------
  Method: GetDeviceCaps
  Params: DC    - Display device context
          Index - Index of needed capability

  Returns device specific information
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
var
  DeviceContext: TCarbonDeviceContext;
  ResX, ResY: FPCMacOSAll.SInt16;
begin
  Result := 0;
  if not IsValidDC(DC) then Exit;

  DeviceContext := TCarbonDeviceContext(DC);

  case Index of
  LOGPIXELSX,
  LOGPIXELSY:
    begin
      FPCMacOSAll.ScreenRes(ResX, ResY);
      if Index = LOGPIXELSX then Result := ResX
                            else Result := ResY;
    end;
  BITSPIXEL:  Result := CGDisplayBitsPerPixel(CGMainDisplayID);
  end;
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

{------------------------------------------------------------------------------
  Method:  GetFocus
  Params:  None
  Returns: The handle of the window with focus

  Retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetFocus: HWND;
var
  Control: ControlRef;
  Window: WindowRef;
begin
  Result := 0;
  Window := WindowRef(GetActiveWindow);
  if Window = nil then Exit;
  
  Control := nil;
  GetKeyboardFocus(Window, Control);
  if Control <> nil then
    Result := HWND(Control)
  else Result := HWND(Window);
end;

function TCarbonWidgetSet.GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result:=inherited GetFontLanguageInfo(DC);
end;

{------------------------------------------------------------------------------
  Method:  GetKeyState
  Params:  nVirtKey - The requested key
  Returns: If the function succeeds, the return value specifies the status of
           the given virtual key. If the high-order bit is 1, the key is down;
           otherwise, it is up. If the low-order bit is 1, the key is toggled.

  Retrieves the status of the specified virtual key
 ------------------------------------------------------------------------------}
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
      // the ssCtrl/VK_CONTROL is mapped to cmdKey under MacOS
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

{------------------------------------------------------------------------------
  Method:  GetSysColor
  Params:  NIndex - Display element whose color is to be retrieved
  Returns: RGB color value

  Retrieves the current color of the specified display element.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetSysColor(NIndex: Integer): DWORD;
var
  C: FPCMacOSAll.RGBColor;
  Depth: SInt16;
  R: OSStatus;
begin
  Result := 0;
  R := not noErr;
  
  Depth := CGDisplayBitsPerPixel(CGMainDisplayID);
  case NIndex of
  //COLOR_BACKGROUND:
  //COLOR_GRADIENTACTIVECAPTION, COLOR_ACTIVECAPTION:
  //COLOR_GRADIENTINACTIVECAPTION, COLOR_INACTIVECAPTION:
  COLOR_MENU:
    R := GetThemeBrushAsColor(kThemeBrushMenuBackground, Depth, True, C);
  //COLOR_WINDOWFRAME:
  COLOR_MENUTEXT:
    R := GetThemeTextColor(kThemeTextColorMenuItemActive, Depth, True, C);
  COLOR_WINDOWTEXT:
    R := GetThemeTextColor(kThemeTextColorMenuItemActive, Depth, True, C);
  COLOR_CAPTIONTEXT:
    R := GetThemeTextColor(kThemeTextColorDocumentWindowTitleActive, Depth, True, C);
  //COLOR_ACTIVEBORDER:
  //COLOR_INACTIVEBORDER:
  COLOR_APPWORKSPACE:
    R := GetThemeBrushAsColor(kThemeBrushUtilityWindowBackgroundActive, Depth, True, C);
  COLOR_HIGHLIGHT:
    R := GetThemeBrushAsColor(kThemeBrushPrimaryHighlightColor, Depth, True, C);
  COLOR_HIGHLIGHTTEXT:
    R := GetThemeTextColor(kThemeTextColorPushButtonPressed, Depth, True, C);
  COLOR_SCROLLBAR, COLOR_BTNFACE:
    R := GetThemeBrushAsColor(kThemeBrushButtonFaceActive, Depth, True, C);
  COLOR_BTNSHADOW:
    R := GetThemeBrushAsColor(kThemeBrushButtonActiveDarkShadow, Depth, True, C);
  COLOR_GRAYTEXT:
    R := GetThemeTextColor(kThemeTextColorBevelButtonInactive , Depth, True, C);
  COLOR_BTNTEXT:
    R := GetThemeTextColor(kThemeTextColorPushButtonActive, Depth, True, C);
  COLOR_INACTIVECAPTIONTEXT:
    R := GetThemeTextColor(kThemeTextColorDocumentWindowTitleInactive, Depth, True, C);
  COLOR_BTNHIGHLIGHT:
    R := GetThemeBrushAsColor(kThemeBrushButtonFacePressed, Depth, True, C);
  COLOR_3DDKSHADOW:
    R := GetThemeBrushAsColor(kThemeBrushButtonActiveDarkShadow, Depth, True, C);
  COLOR_3DLIGHT:
    R := GetThemeBrushAsColor(kThemeBrushButtonActiveLightShadow, Depth, True, C);
  //COLOR_INFOTEXT:
  //COLOR_INFOBK:
  //COLOR_HOTLIGHT:
  COLOR_WINDOW, COLOR_FORM:
    R := GetThemeBrushAsColor(kThemeBrushDocumentWindowBackground, Depth, True, C);
  end;
  
  if R = noErr then
    Result := CarbonColorToColor(C);
end;

{------------------------------------------------------------------------------
  Method:  GetSystemMetrics
  Params:  NIndex - System metric to retrieve
  Returns: The requested system metric value

  Retrieves various system metrics.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetSystemMetrics(NIndex: Integer): Integer;
begin
  Result := 0;
  case NIndex of
  //SM_CYHSCROLL
  //SM_CXVSCROLL
  SM_CXSCREEN: Result := CGDisplayPixelsWide(CGMainDisplayID);
  SM_CYSCREEN: Result := CGDisplayPixelsHigh(CGMainDisplayID);
  else
    Result := inherited GetSystemMetrics(NIndex);
  end;
end;

function TCarbonWidgetSet.GetTextColor(DC: HDC): TColorRef;
begin
  Result:=inherited GetTextColor(DC);
end;

{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  DC    - Handle of device context
           Str   - Text string
           Count - Number of characters in string
           Size  - The record for the dimensions of the string
  Returns: If the function succeeds

  Computes the width and height of the specified string of text.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
var
  W: WideString;
  S: String;
  TextLength: LongWord;
  TextLayout: ATSUTextLayout;
  TextStyle: ATSUStyle;
  ADC: TCarbonDeviceContext;
  AFont: TCarbonFont;
  TextBefore, TextAfter, Ascent, Descent: ATSUTextMeasurement;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;

  // convert str to wide string
  if Count < 0 then S := Str
  else
  begin
    SetLength(S, Count);
    System.Move(Str^, S[1], Count);
  end;
  W := Utf8Decode(S);

  ADC := TCarbonDeviceContext(DC);
  if not IsValidGDIObject(ADC.CurrentFont) then
    TextStyle := DafultTextStyle
  else
  begin
    AFont := TCarbonFont(ADC.CurrentFont);
    TextStyle := AFont.Style;
  end;

  // create text layout
  TextLength := kATSUToTextEnd;
  if ATSUCreateTextLayoutWithTextPtr(ConstUniCharArrayPtr(@W[1]),
      kATSUFromTextBeginning, kATSUToTextEnd, Count, 1, @TextLength, @TextStyle,
      TextLayout) <> noErr then Exit;
  try

    // finally compute the text dimensions
    Result := ATSUGetUnjustifiedBounds(TextLayout, kATSUFromTextBeginning,
      kATSUToTextEnd, TextBefore, TextAfter, Ascent, Descent) = noErr;
      
    if Result then
    begin
      Size.cx := (TextAfter - TextBefore) shr 16;
      Size.cy := (Descent + Ascent) shr 16;
    end;
  finally
    ATSUDisposeTextLayout(TextLayout);
  end;
end;

{------------------------------------------------------------------------------
  Method:  GetTextMetrics
  Params:  DC - Handle of device context
           TM - The Record for the text metrics
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  TextStyle: ATSUStyle;
  ADC: TCarbonDeviceContext;
  AFont: TCarbonFont;
  M: ATSUTextMeasurement;
  B: Boolean;
  S: TSize;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;

  ADC := TCarbonDeviceContext(DC);
  if not IsValidGDIObject(ADC.CurrentFont) then
    TextStyle := DafultTextStyle
  else
  begin
    AFont := TCarbonFont(ADC.CurrentFont);
    TextStyle := AFont.Style;
  end;
  
  FillChar(TM, SizeOf(TM), 0);
  
  //if ATSUGetAttribute(TextStyle, kATSULineAscentTag, SizeOf(M), @M, nil) <> noErr then Exit;
  //TM.tmAscent := (-M) shr 16;
  
  //if ATSUGetAttribute(TextStyle, kATSULineDescentTag, SizeOf(M), @M, nil) <> noErr then Exit;
  //TM.tmDescent := M shr 16;
  
  //TM.tmHeight := TM.tmAscent + TM.tmDescent;
  
  if ATSUGetAttribute(TextStyle, kATSULeadingTag, SizeOf(M), @M, nil) <> noErr then Exit;
  TM.tmInternalLeading := M shr 16;

  // According to the MSDN library, TEXTMETRIC:
  // the average char width is generally defined as the width of the letter x
  if not GetTextExtentPoint(DC, 'x', 1, S) then Exit;
  TM.tmAveCharWidth := S.cx;
  
  TM.tmMaxCharWidth := TM.tmAscent; //TODO: don't know how to determine this right
  
  if ATSUGetAttribute(TextStyle, kATSUQDBoldfaceTag, SizeOf(B), @B, nil) <> noErr then Exit;
  if B then TM.tmWeight := FW_NORMAL
       else TM.tmWeight := FW_BOLD;
  
  if ATSUGetAttribute(TextStyle, kATSUQDItalicTag, SizeOf(B), @B, nil) <> noErr then Exit;
  TM.tmItalic := Byte(B);
  
  if ATSUGetAttribute(TextStyle, kATSUQDUnderlineTag, SizeOf(B), @B, nil) <> noErr then Exit;
  TM.tmUnderlined := Byte(B);
  
  if ATSUGetAttribute(TextStyle, kATSUStyleStrikeThroughTag, SizeOf(B), @B, nil) <> noErr then Exit;
  TM.tmStruckOut := Byte(B);

  //TODO: get these from font
  TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;
  TM.tmCharSet := DEFAULT_CHARSET;
  
  Result := True;
end;

function TCarbonWidgetSet.GetWindowLong(Handle: hwnd; int: Integer): PtrInt;
begin
  Result:=inherited GetWindowLong(Handle, int);
end;

function TCarbonWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
begin
  Result:=inherited GetWindowOrgEx(dc, P);
end;

{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - Handle of window
           Rect   - Record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the screen bounding rectangle of the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect): Integer;
var
  WinControl: TWinControl;
  AInfo: PWidgetInfo;
  R: TRect;
begin
  Result := 0;
  
  AInfo := nil;
  case GetWidgetType(Pointer(Handle), AInfo) of
  cwtWindowRef:
    if GetCarbonLocalWindowRect(Handle, ARect, AInfo) then Result := 1;
  cwtControlRef:
  begin
    if not GetCarbonLocalWindowRect(Handle, ARect, AInfo) then Exit;
    
    WinControl := (AInfo^.LCLObject as TWinControl).Parent;

    while WinControl <> nil do
    begin
      if not GetCarbonLocalWindowRect(WinControl.Handle, R) then Exit;
      OffsetRect(ARect, R.Left, R.Top);
      if not GetCarbonClientRect(Handle, R, AInfo) then Exit;
      OffsetRect(ARect, R.Left, R.Top);
      
      WinControl := WinControl.Parent;
    end;
    
    Result := 1;
  end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  GetWindowRelativePosition
  Params:  Handle - Handle of window
  Returns: If function succeeds

  Returns the window left and top relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetWindowRelativePosition(Handle: hwnd; var Left,
  Top: integer): boolean;
var
  ARect: TRect;
begin
  Result := GetCarbonLocalWindowRect(Handle, ARect);
  
  if not Result then Exit;
  Left := ARect.Left;
  Top := ARect.Top;
end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:   Handle - Handle of window
            Width
            Height
  Returns:  If function succeeds

  Returns the width and height of the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: integer
  ): boolean;
var
  ARect: TRect;
begin
  Result := GetCarbonLocalWindowRect(Handle, ARect);
    
  if not Result then Exit;
  Width := ARect.Right - ARect.Left;
  Height := ARect.Bottom - ARect.Top;
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

{------------------------------------------------------------------------------
  Method:  IsWindowEnabled
  Params:  Handle - Handle of window
  Returns: True if window is enabled, false otherwise
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
var
  Root: ControlRef;
begin
  Result := False;

  case GetWidgetType(Pointer(Handle)) of
  cwtWindowRef:
    begin
      GetRootControl(WindowRef(Handle), Root);
      Result := IsControlEnabled(Root);
    end;
  cwtControlRef:
    Result := IsControlEnabled(ControlRef(Handle));
  end;
end;

{------------------------------------------------------------------------------
  Method:  IsWindowVisible
  Params:  Handle - Handle of window
  Returns: True if window is visible, false otherwise
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsWindowVisible(Handle: HWND): boolean;
begin
  Result := False;
  
  case GetWidgetType(Pointer(Handle)) of
  cwtWindowRef:
    Result := FPCMacOSAll.IsWindowVisible(WindowRef(Handle));
  cwtControlRef:
    Result := FPCMacOSAll.IsControlVisible(ControlRef(Handle));
  end;
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

{------------------------------------------------------------------------------
  Method:  ReleaseDC
  Params:  HWnd - Handle of window
           DC   - Handle of device context
  Returns: 1 if the device context was released or 0 if it wasn't

  Releases a device context (DC), freeing it for use by other applications
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ReleaseDC(HWnd: HWND; DC: HDC): Integer;
begin
  Result := 0;
  if not IsValidDC(DC) then Exit;
  TCarbonDeviceContext(DC).Free;
  Result := 1;
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

{------------------------------------------------------------------------------
  Method:  SelectObject
  Params:  DC     - Handle of the device context
           GDIObj - Handle of the object
  Returns: The handle of the object being replaced or 0 if error occurs

  Selects an object into the specified device context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  AObject: TObject;
  ADC: TCarbonDeviceContext;
begin
  Result := 0;
  if not IsValidDC(DC) or not IsValidGDIObject(GDIObj) then
  begin
    DebugLn('SelectObject error - invalid device context or object!');
    Exit;
  end;
  
  ADC := TCarbonDeviceContext(DC);
  AObject := TObject(GDIObj);
  
  if AObject is TCarbonFont then
  begin
    Result := ADC.CurrentFont;
    ADC.CurrentFont := GDIObj;
  end;
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

{------------------------------------------------------------------------------
  Method:  SetActiveWindow
  Params:  Handle - Window to activate
  Returns: Previous active window

  Sets focus to the specified window.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  Result := 0;
  if Handle = 0 then Exit;
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

{------------------------------------------------------------------------------
  Method:  SetFocus
  Params:  HWnd - Handle of new focus window
  Returns: Previous focused window

  Sets the keyboard focus to the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetFocus(HWnd: HWND): HWND;
var
  Window: WindowRef;
  Control: ControlRef;
  Info: PWidgetInfo;
begin
  Result := GetFocus;
  if HWnd = 0 then Exit;
  if HWnd = Result then Exit; // if window is already focused exit
  
  Info := GetWidgetInfo(Pointer(HWnd));
  Window := GetTopParentWindow(Info^.LCLObject as TWinControl);
  
  ActivateWindow(Window, True); // can change focus!
  if HWnd <> THandle(Window) then
  begin
    GetKeyboardFocus(Window, Control);
    if Control <> ControlRef(HWnd) then
      SetKeyboardFocus(Window, ControlRef(HWnd), kControlFocusNextPart);
  end;
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

{------------------------------------------------------------------------------
  Method:  ShowWindow
  Params:  hWnd     - Handle of window
           nCmdShow - (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED)
  Returns: If the function succeeds
  
  Shows the window normal, minimized or maximized
------------------------------------------------------------------------------}
function TCarbonWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
 P: FPCMacOSAll.Point;
begin
  Result := False;

  case nCmdShow of
  SW_SHOWNORMAL, SW_SHOWMAXIMIZED:
  begin
    if IsWindowCollapsed(WindowRef(hWnd)) then
      Result := CollapseWindow(WindowRef(hWnd), False) = noErr;

    if Result then
    begin
      if nCmdShow = SW_SHOWNORMAL then
        Result := ZoomWindowIdeal(WindowRef(hWnd), inZoomIn, P) = noErr
      else
      begin
        P.v := $3FFF;
        P.h := $3FFF;
        Result := ZoomWindowIdeal(WindowRef(hWnd), inZoomOut, P) = noErr;
      end;
    end;
  end;
  SW_MINIMIZE: Result := CollapseWindow(WindowRef(hWnd), True) = noErr;
  end;
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

{------------------------------------------------------------------------------
  Method:  TextOut
  Params:  DC    - Handle of the device context
           X     - X-coordinate of starting position
           Y     - Y-coordinate of starting position
           Str   - String
           Count - Number of characters in string
  Returns: If the function succeeds

  Draws a character string at the specified location, using the currently
  selected font.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.TextOut(DC: HDC; X, Y: Integer; Str: Pchar;
  Count: Integer): Boolean;
var
  W: WideString;
  S: String;
  TextLength: LongWord;
  TextLayout: ATSUTextLayout;
  TextStyle: ATSUStyle;
  ADC: TCarbonDeviceContext;
  AFont: TCarbonFont;
  TextBefore, TextAfter, Ascent, Descent: ATSUTextMeasurement;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  
  // convert str to wide string
  if Count < 0 then S := Str
  else
  begin
    SetLength(S, Count);
    System.Move(Str^, S[1], Count);
  end;
  W := Utf8Decode(S);

  ADC := TCarbonDeviceContext(DC);
  ADC.Activate;
  
  if not IsValidGDIObject(ADC.CurrentFont) then
    TextStyle := DafultTextStyle
  else
  begin
    AFont := TCarbonFont(ADC.CurrentFont);
    TextStyle := AFont.Style;
  end;

  // create text layout
  TextLength := kATSUToTextEnd;
  if ATSUCreateTextLayoutWithTextPtr(ConstUniCharArrayPtr(@W[1]),
      kATSUFromTextBeginning, kATSUToTextEnd, Count, 1, @TextLength, @TextStyle,
      TextLayout) <> noErr then Exit;
  try
    // get text ascent
    if ATSUGetUnjustifiedBounds(TextLayout, kATSUFromTextBeginning,
      kATSUToTextEnd, TextBefore, TextAfter, Ascent, Descent) <> noErr then Exit;
      
    // finally draw the text
    Result := ATSUDrawText(TextLayout, kATSUFromTextBeginning, kATSUToTextEnd,
      X shl 16, Y shl 16 + Ascent) = noErr;
  finally
    ATSUDisposeTextLayout(TextLayout);
  end;
end;

function TCarbonWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  Result:=inherited WindowFromPoint(Point);
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

