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

{------------------------------------------------------------------------------
  Method:  CreateBitmap
  Params:  Width      - Bitmap width, in pixels
           Height     - Bitmap height, in pixels
           Planes     - Number of color planes
           BitCount   - Number of bits required to identify a color (TODO)
           BitmapBits - Pointer to array containing color data (TODO)
  Returns: A handle to a bitmap

  Creates a bitmap with the specified width, height and color format
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateBitmap(Width, Height: Integer; Planes,
  BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  Result := HBitmap(TCarbonBitmap.Create(Width, Height, BitCount, BitmapBits));
end;

function TCarbonWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
begin
  Result:=inherited CreateBitmapFromRawImage(RawImage, Bitmap, MaskBitmap,
    AlwaysCreateMask);
end;

{------------------------------------------------------------------------------
  Method:   CreateBrushIndirect
  Params:   LogBrush - Record with brush characteristic
  Returns:  Handle to a logical brush

  Creates new logical brush that has the specified style, color, and pattern
  TODO: patterns
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush
  ): HBRUSH;
begin
  Result := HBRUSH(TCarbonBrush.Create(LogBrush));
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

{------------------------------------------------------------------------------
  Method:  CreateCompatibleDC
  Params:  DC - Handle to memory device context
  Returns: Handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := HDC(TCarbonBitmapContext.Create);
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

{------------------------------------------------------------------------------
  Method:  CreatePenIndirect
  Params:  LogPen - Record with pen characteristic
  Returns: Handle to a logical cosmetic pen

  Creates new logical cosmetic pen that has the specified style, width and color
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := HPEN(TCarbonPen.Create(LogPen));
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

{------------------------------------------------------------------------------
  Method:  DeleteCriticalSection
  Params:  CritSection - Critical section to be deleted
  Returns: Nothing

  Deletes the specified critical section
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.DeleteCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec := System.PRTLCriticalSection(CritSection);

  System.DoneCriticalsection(ACritSec^);
  Dispose(ACritSec);
  
  CritSection := 0;
end;

{------------------------------------------------------------------------------
  Method:  DeleteDC
  Params:  HDC - Handle to device context
  Returns: If the function succeeds

  Deletes the specified device context (DC)
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  Result := False;
  if not IsValidDC(hDC) then Exit;
  
  TCarbonDeviceContext(hDC).Free;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  DeleteObject
  Params:  GDIObject - Handle to graphic object
  Returns: If the function succeeds

  Deletes the specified graphic object, freeing all system resources associated
  with the object
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result := False;
  
  if not IsValidGDIObject(GDIObject) then Exit;
  
  TCarbonGDIObject(GDIObject).Free;
  Result := True;
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

{------------------------------------------------------------------------------
  Method:  Ellipse
  Params:
           DC - Handle to device context
           X1 - X-coord. of bounding rectangle's upper-left corner
           Y1 - Y-coord. of bounding rectangle's upper-left corner
           X2 - X-coord. of bounding rectangle's lower-right corner
           Y2 - Y-coord. of bounding rectangle's lower-right corner
  Returns: If the function succeeds

  Draws a ellipse. The ellipse is outlined by using the current pen and filled
  by using the current brush.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  ADC: TCarbonDeviceContext;
  R: CGRect;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;

  ADC := TCarbonDeviceContext(DC);
  
  R := GetCGRect(X1, -Y1, X2, -Y2);

  CGContextFillEllipseInRect(ADC.CGContext, R);
  CGContextStrokeEllipseInRect(ADC.CGContext, R);
  
  Result := True;
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
  ContentView: HIviewRef;
begin
  case GetWidgetType(Pointer(hWnd)) of
    cwtWindowRef:
      begin
        ContentView := GetCarbonWindowContent(hWnd);

        // enable/disable window content
        // add/remove standard handler
        if bEnable then
        begin
          FPCMacOSAll.EnableControl(ContentView);
          ChangeWindowAttributes(WindowRef(hWnd), kWindowStandardHandlerAttribute,
            kWindowNoAttributes);
        end
        else
        begin
          FPCMacOSAll.DisableControl(ContentView);
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

{------------------------------------------------------------------------------
  Method:  EnterCriticalSection
  Params:  CritSection - Critical section to be entered
  Returns: Nothing

  Enters the specified critical section
 ------------------------------------------------------------------------------}
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
  TODO: specific face or specific char set enumeration
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; LParam: Lparam; flags: dword): Longint;
var
  FamilyCount, NameLength: LongWord;
  FamilyListPtr, PFamily: ^ATSUFontID;
  FontName: String;
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
        if ATSUFindFontName(PFamily^, kFontFamilyName, kFontMacintoshPlatform,
          kFontRomanScript, kFontEnglishLanguage, NameLength, nil,
          @NameLength, nil) <> noErr then Continue;
        
        SetLength(FontName, NameLength);

        // retrieve font name
        if ATSUFindFontName(PFamily^, kFontFamilyName, kFontMacintoshPlatform,
          kFontRomanScript, kFontEnglishLanguage, NameLength,
          @FontName[1], @NameLength, nil) <> noErr then Continue;
          
        if FontName <> '' then // execute callback
        begin
          FillChar(EnumLogFont, SizeOf(EnumLogFont), #0);
          FillChar(Metric, SizeOf(Metric), #0);
          FontType := 0;
          EnumLogFont.elfLogFont.lfFaceName := FontName;
          // TODO: get all attributes
          
          Result := Callback(EnumLogFont, Metric, FontType, LParam);
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

{------------------------------------------------------------------------------
  Method:  ExtTextOut
  Params:  DC      - Handle to device context
           X       - X-coordinate of reference point
           Y       - Y-coordinate of reference point
           Options - Text-output options
           Rect    - Optional clipping and/or opaquing rectangle (TODO)
           Str     - Character string to be drawn
           Count   - Number of characters in string
           Dx      - Pointer to array of intercharacter spacing values (IGNORED)
  Returns: If the string was drawn

  Draws a character string by using the currently selected font
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  TextLayout: ATSUTextLayout;
  ADC: TCarbonDeviceContext;
  TextBefore, TextAfter, Ascent, Descent: ATSUTextMeasurement;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);

  if not BeginTextRender(ADC, Str, Count, TextLayout) then Exit;
  try
    // get text ascent
    if ATSUGetUnjustifiedBounds(TextLayout, kATSUFromTextBeginning,
      kATSUToTextEnd, TextBefore, TextAfter, Ascent, Descent) <> noErr then Exit;

    // fill drawed text background
    if (Options and ETO_OPAQUE) > 0 then
      CGContextFillRect(ADC.CGContext, GetCGRect(X - TextBefore shr 16,
        -Y shr 16, X + TextAfter shr 16, -Y - (Ascent + Descent) shr 16));

    // finally draw the text
    Result := ATSUDrawText(TextLayout, kATSUFromTextBeginning, kATSUToTextEnd,
      X shl 16 - TextBefore, -(Y shl 16) - Descent) = noErr;
     if Result then
       DebugLn('ExtTextOut text drawed');
  finally
    EndTextRender(ADC, TextLayout);
  end;
end;

function TCarbonWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint
  ): Integer;
begin
  Result:=inherited ExtSelectClipRGN(dc, rgn, Mode);
end;

{------------------------------------------------------------------------------
  Method:  FillRect
  Params:  DC    - Handle to device context
           Rect  - Record with rectangle coordinates
           Brush - Handle to brush
  Returns: If the function succeeds

  Fills the rectangle by using the specified brush
  It includes the left and top borders, but excludes the right and
  bottom borders of the rectangle!
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH
  ): Boolean;
var
  ADC: TCarbonDeviceContext;
  TempBrush: HBRUSH;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  if not (TObject(Brush) is TCarbonBrush) then Exit;

  ADC := TCarbonDeviceContext(DC);
  
  TempBrush := HBRUSH(ADC.CurrentBrush);
  TCarbonBrush(Brush).Apply(ADC, False); // do not use ROP2
  try
    CGContextFillRect(ADC.CGContext, RectToCGRect(Rect));
    Result := True;
  finally
    SelectObject(DC, TempBrush);
  end;
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

{------------------------------------------------------------------------------
  Method:  GetClipBox
  Params:  DC   - Handle to device context
           Rect - Record for client coordinates of clipping box
  Returns: See bellow

  Retrieves the smallest rectangle which includes the entire current clipping
  region. The result can be one of the following constants: ERROR, NULLREGION,
  SIMPLEREGION, COMPLEXREGION.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
var
  ADC: TCarbonDeviceContext;
begin
  if not IsValidDC(DC) then
  begin
    Result := ERROR;
    Exit;
  end
  else Result := COMPLEXREGION;
  ADC := TCarbonDeviceContext(DC);
  
  if lpRect <> nil then
  begin
    lpRect^ := CGRectToRect(CGContextGetClipBoundingBox(ADC.CGContext));
    lpRect^.Top := -lpRect^.Top;
    lpRect^.Bottom := -lpRect^.Bottom;
  end;
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
  the specified window
  TODO: implement screen context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDC(HWnd: HWND): HDC;
var
  AInfo: TCarbonWidgetInfo;
begin
  Result := 0;
  
  if HWnd = 0 then Result := HDC(TCarbonScreenContext.Create)
  else
  begin
    AInfo := GetWidgetInfo(Pointer(HWnd));
    if AInfo = nil then Exit;
    
    // TODO: save DC
    Result := HDC(AInfo.Context);
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
  ADC: TCarbonDeviceContext;
  ResX, ResY: FPCMacOSAll.SInt16;
begin
  Result := 0;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);

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

{------------------------------------------------------------------------------
  Method:  GetDeviceSize
  Params:  DC - Handle to device context
           P  - Record point for result
  Returns: If the function succeeds

  Retrieves the size of the specified device context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
var
  ADC: TCarbonDeviceContext;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);
  
  P := ADC.Size;
  Result := True;
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
    DebugLn('TCarbonWidgetSet.GetKeyState TODO ', DbgS(nVirtkey));
  end;
end;

function TCarbonWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer;
  Buf: Pointer): Integer;
begin
  Result:=inherited GetObject(GDIObj, BufSize, Buf);
end;

{------------------------------------------------------------------------------
  Method:  GetParent
  Params:  Handle - Handle of child window
  Returns: The handle of the parent window

  Retrieves the handle of the specified child window's parent window.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetParent(Handle: HWND): HWND;
begin
  Result := 0;
  if Handle = 0 then Exit;
  
  case GetWidgetType(Pointer(Handle)) of
  cwtControlRef: Result := HWND(HIViewGetSuperview(HIViewRef(Handle)));
  end;
  // Carbon windows has no parent
end;

{------------------------------------------------------------------------------
  Method:  GetProp
  Params:  Handle - Handle of window
           Str    - Property name
  Returns: The property data or nil if the property is not listed

  Retrieves a pointer to data from the property list of the specified window or
  nil if the property is not listed
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
var
  Info: TCarbonWidgetInfo;
begin
  Result := nil;
  Info := GetWidgetInfo(Pointer(Handle));
  if Info = nil then Exit;

  Result := Info.Properties[Str];
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

{------------------------------------------------------------------------------
  Method:  GetStockObject
  Params:  Value - Type of stock object
  Returns: A value identifying the logical object requested

  Retrieves a handle to one of the predefined stock objects
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetStockObject(Value: Integer): LongInt;
begin
  case Value of
    NULL_BRUSH: // null brush (equivalent to HOLLOW_BRUSH).
      Result := HBRUSH(StockNullBrush);
    DEFAULT_GUI_FONT, SYSTEM_FONT:
      Result := HFONT(StockSystemFont);
  else
    begin
      DebugLn('TCarbonWidgetSet.GetStockObject TODO ', DbgS(Value));
      Result := 0;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  GetSysColor
  Params:  NIndex - Display element whose color is to be retrieved
  Returns: RGB color value

  Retrieves the current color of the specified display element
  TODO: all system colors
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
  else
    DebugLn('TCarbonWidgetSet.GetSysColor TODO ', DbgS(NIndex));
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
    SM_CYHSCROLL, SM_CXVSCROLL:
      GetThemeMetric(kThemeMetricScrollBarWidth, Result);
    SM_CXSCREEN: Result := CGDisplayPixelsWide(CGMainDisplayID);
    SM_CYSCREEN: Result := CGDisplayPixelsHigh(CGMainDisplayID);
  else
    DebugLn('TCarbonWidgetSet.GetSystemMetrics TODO ', DbgS(NIndex));;
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
  TextLayout: ATSUTextLayout;
  ADC: TCarbonDeviceContext;
  TextBefore, TextAfter, Ascent, Descent: ATSUTextMeasurement;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);

  if not BeginTextRender(ADC, Str, Count, TextLayout) then Exit;
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
    EndTextRender(ADC, TextLayout);
  end;
end;

{------------------------------------------------------------------------------
  Method:  GetTextMetrics
  Params:  DC - Handle of device context
           TM - The Record for the text metrics
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font
  TODO: get exact max. and av. char width, pitch and charset
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  TextStyle: ATSUStyle;
  ADC: TCarbonDeviceContext;
  M: ATSUTextMeasurement;
  B: Boolean;
  R: OSStatus;
  TextLayout: ATSUTextLayout;
  TextBefore, TextAfter, Ascent, Descent: ATSUTextMeasurement;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;

  ADC := TCarbonDeviceContext(DC);
  if not IsValidGDIObject(HFONT(ADC.CurrentFont)) then
    TextStyle := DefaultTextStyle
  else
    TextStyle := ADC.CurrentFont.Style;
  
  FillChar(TM, SizeOf(TM), 0);

  // According to the MSDN library, TEXTMETRIC:
  // the average char width is generally defined as the width of the letter x
  if not BeginTextRender(ADC, 'x', 1, TextLayout) then Exit;
  try
    if ATSUGetUnjustifiedBounds(TextLayout, kATSUFromTextBeginning,
      kATSUToTextEnd, TextBefore, TextAfter, Ascent, Descent) <> noErr then Exit
  finally
    EndTextRender(ADC, TextLayout);
  end;
  
  TM.tmAscent := Ascent shr 16;
  TM.tmDescent := Descent shr 16;
  TM.tmHeight := (Ascent + Descent) shr 16;
  
  R := ATSUGetAttribute(TextStyle, kATSULeadingTag, SizeOf(M), @M, nil);
  if (R <> noErr) and (R <> kATSUNotSetErr) then Exit;
  TM.tmInternalLeading := M shr 16;
  TM.tmExternalLeading := 0;
  
  TM.tmAveCharWidth := (TextAfter - TextBefore) shr 16;
  
  TM.tmMaxCharWidth := TM.tmAscent; // TODO: don't know how to determine this right
  TM.tmOverhang := 0;
  TM.tmDigitizedAspectX := 0;
  TM.tmDigitizedAspectY := 0;
  TM.tmFirstChar := 'a';
  TM.tmLastChar := 'z';
  TM.tmDefaultChar := 'x';
  TM.tmBreakChar := '?';
  
  R := ATSUGetAttribute(TextStyle, kATSUQDBoldfaceTag, SizeOf(B), @B, nil);
  if (R <> noErr) and (R <> kATSUNotSetErr) then Exit;
  if B then TM.tmWeight := FW_NORMAL
       else TM.tmWeight := FW_BOLD;
  
  R := ATSUGetAttribute(TextStyle, kATSUQDItalicTag, SizeOf(B), @B, nil);
  if (R <> noErr) and (R <> kATSUNotSetErr) then Exit;
  TM.tmItalic := Byte(B);
  
  R := ATSUGetAttribute(TextStyle, kATSUQDUnderlineTag, SizeOf(B), @B, nil);
  if (R <> noErr) and (R <> kATSUNotSetErr) then Exit;
  TM.tmUnderlined := Byte(B);
  
  R := ATSUGetAttribute(TextStyle, kATSUStyleStrikeThroughTag, SizeOf(B), @B, nil);
  if (R <> noErr) and (R <> kATSUNotSetErr) then Exit;
  TM.tmStruckOut := Byte(B);

  // TODO: get these from font
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
  AInfo: TCarbonWidgetInfo;
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

        WinControl := (AInfo.LCLObject as TWinControl).Parent;

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

{------------------------------------------------------------------------------
  Method:  InitializeCriticalSection
  Params:  CritSection - Record for initialized critical section
  Returns: Nothing

  Creates a new critical section
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.InitializeCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  
  System.InitCriticalSection(ACritSec^);
  CritSection := TCriticalSection(ACritSec);
end;

function TCarbonWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right,
  Bottom: Integer): Integer;
begin
  Result:=inherited IntersectClipRect(dc, Left, Top, Right, Bottom);
end;

{------------------------------------------------------------------------------
  Method:  InvalidateRect
  Params:  AHandle - Handle of window
           Rect    - Pointer to rectangle coordinates
           BErase  - Specifies whether the background is to be erased
  Returns: If the function succeeds

  Adds a rectangle to the specified window's update region
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.InvalidateRect(AHandle: HWND; Rect: pRect;
  bErase: Boolean): Boolean;
var
  AInfo: TCarbonWidgetInfo;
  R: TRect;
begin
  Result := False;
  
  AInfo := GetWidgetInfo(Pointer(AHandle));
  if AInfo = nil then Exit;
  
  if not GetCarbonClientRect(AHandle, R, AInfo) then Exit;
  
  if Rect <> nil then // offset to client origin
  begin
    OffsetRect(Rect^, R.Left, R.Top);
    R := Rect^;
  end;
  
  case AInfo.WidgetType of
    cwtWindowRef:
      Result := InvalWindowRect(WindowRef(AHandle), GetCarbonRect(R)) = noErr;
    cwtControlRef:
      Result := HIViewSetNeedsDisplayInRect(HIViewRef(AHandle), RectToCGRect(R), True) = noErr;
  end;
end;

{------------------------------------------------------------------------------
  Method:  IsWindowEnabled
  Params:  Handle - Handle of window
  Returns: True if window is enabled, false otherwise
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  Result := False;

  case GetWidgetType(Pointer(Handle)) of
    cwtWindowRef:
      begin
        Result := IsControlEnabled(GetCarbonWindowContent(Handle));
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

{------------------------------------------------------------------------------
  Method:  LeaveCriticalSection
  Params:  CritSection - Critical section to be left
  Returns: Nothing

  Leaves the specified critical section
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.LeaveCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec := System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

{------------------------------------------------------------------------------
  Method:  LineTo
  Params:  DC - Handle to device context
           X  - X-coordinate of line's ending point
           Y  - Y-coordinate of line's ending point
  Returns: If the function succeeds

  Draws a line from the current position up to the specified point except and
  updates the current position
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ADC: TCarbonDeviceContext;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;

  ADC := TCarbonDeviceContext(DC);

  CGContextBeginPath(ADC.CGContext);
  CGContextMoveToPoint(ADC.CGContext, ADC.PenPos.x, -ADC.PenPos.y);
  CGContextAddLineToPoint(ADC.CGContext, X, -Y);
  CGContextStrokePath(ADC.CGContext);
  
  Result := True;
  ADC.PenPos.x := X;
  ADC.PenPos.y := Y;
end;

function TCarbonWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): integer;
begin
  Result:=inherited MessageBox(hWnd, lpText, lpCaption, uType);
end;

{------------------------------------------------------------------------------
  Method:  MoveToEx
  Params:  DC       - Handle to device context
           X        - X-coordinate of new current position
           Y        - Y-coordinate of new current position
           OldPoint - Pointer to old current position
  Returns: If the function succeeds.

  Updates the current position to the specified point
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint
  ): Boolean;
var
  ADC: TCarbonDeviceContext;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;

  ADC := TCarbonDeviceContext(DC);

  if OldPoint <> nil then OldPoint^ := ADC.PenPos;
  ADC.PenPos.x := X;
  ADC.PenPos.y := Y;
  
  Result := True;
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

{------------------------------------------------------------------------------
  Method:  Polygon
  Params:  DC      - Handle to device context
           Points  - Pointer to polygon's vertices
           NumPts  - Number of polygon's vertices
           Winding - Use winding fill rule
  Returns: If the function succeeds

  Draws a closed, many-sided shape on the canvas, using the pen and brush.
  If Winding is set, Polygon fills the shape using the Winding fill algorithm.
  Otherwise, Polygon uses the even-odd (alternative) fill algorithm. The first
  point is always connected to the last point.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: boolean): boolean;
var
  ADC: TCarbonDeviceContext;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  if Points = nil then Exit;

  ADC := TCarbonDeviceContext(DC);

  CGContextBeginPath(ADC.CGContext);
  CGContextMoveToPoint(ADC.CGContext, Points^.x, -Points^.y);

  while NumPts > 1 do
  begin
    Inc(Points);
    CGContextAddLineToPoint(ADC.CGContext, Points^.x, -Points^.y);
    Dec(NumPts);
  end;

  CGContextClosePath(ADC.CGContext);
  
  if Winding then
    CGContextDrawPath(ADC.CGContext, kCGPathFillStroke)
  else
    CGContextDrawPath(ADC.CGContext, kCGPathEOFillStroke);
    
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  Polyline
  Params:  DC     - Handle to device context
           Points - Pointer to array containing points
           NumPts - Number of points in the array
  Returns: If the function succeeds

  Draws a series of line segments by connecting the points in the specified
  array
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
var
  ADC: TCarbonDeviceContext;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;
  if Points = nil then Exit;

  ADC := TCarbonDeviceContext(DC);
  
  CGContextBeginPath(ADC.CGContext);
  CGContextMoveToPoint(ADC.CGContext, Points^.x, -Points^.y);

  while NumPts > 1 do
  begin
    Inc(Points);
    CGContextAddLineToPoint(ADC.CGContext, Points^.x, -Points^.y);
    Dec(NumPts);
  end;
  
  CGContextStrokePath(ADC.CGContext);

  Result := True;
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

{------------------------------------------------------------------------------
  Method:  Rectangle
  Params:  DC - Handle to device context
           X1 - X-coordinate of bounding rectangle's upper-left corner
           Y1 - Y-coordinate of bounding rectangle's upper-left corner
           X2 - X-coordinate of bounding rectangle's lower-right corner
           Y2 - Y-coordinate of bounding rectangle's lower-right corner
  Returns: If the function succeeds

  Draws a rectangle. The rectangle is outlined by using the current pen and
  filled by using the current brush.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ADC: TCarbonDeviceContext;
  R: CGRect;
begin
  Result := False;
  if not IsValidDC(DC) then Exit;

  ADC := TCarbonDeviceContext(DC);

  R := GetCGRect(X1, -Y1, X2, -Y2);

  CGContextFillRect(ADC.CGContext, R);
  CGContextStrokeRect(ADC.CGContext, R);

  Result := True;
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
  // TODO: restore DC
  Result := 1;
end;

{------------------------------------------------------------------------------
  Method:  RemoveProp
  Params:  Handle - Handle of window
           Str    - Property name
  Returns: Property data or nil if the property is not listed

  Removes the an existing entry from the property list of the
  specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RemoveProp(Handle: hwnd; Str: PChar): THandle;
var
  Info: TCarbonWidgetInfo;
begin
  Result := 0;
  Info := GetWidgetInfo(Pointer(Handle));
  if Info = nil then Exit;

  Result := THandle(Info.Properties[Str]);
  Info.Properties[Str] := nil;
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
  
  if AObject is TCarbonFont then // select font
  begin
    Result := HFONT(ADC.CurrentFont);
    ADC.CurrentFont := TCarbonFont(GDIObj);
  end;
  
  if AObject is TCarbonBrush then // select brush
  begin
    Result := HBRUSH(ADC.CurrentBrush);
    ADC.CurrentBrush := TCarbonBrush(GDIObj);
  end;
  
  if AObject is TCarbonPen then // select pen
  begin
    Result := HPEN(ADC.CurrentPen);
    ADC.CurrentPen := TCarbonPen(GDIObj);
  end;
  
  if AObject is TCarbonBitmap then // select bitmap
  begin
    if not (ADC is TCarbonBitmapContext) then
    begin
      DebugLn('SelectObject error - The specified device is not bitmap context!');
      Exit;
    end;
    
    Result := HBITMAP((ADC as TCarbonBitmapContext).Bitmap);
    (ADC as TCarbonBitmapContext).Bitmap := TCarbonBitmap(GDIObj);
  end;
end;

function TCarbonWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE;
  ForceBackground: Boolean): HPALETTE;
begin
  Result:=inherited SelectPalette(DC, Palette, ForceBackground);
end;

{------------------------------------------------------------------------------
  Method:  SendMessage
  Params:  HandleWnd - Handle of destination window
           Msg       - LCL message to send
           WParam    - First message parameter
           LParam    - Second message parameter
  Returns: The result of the message processing

  Sends the specified LCL message to the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): LResult;
var
  Info: TCarbonWidgetInfo;
  AMsg: TLMessage;
begin
  Result := 0;
  Info := GetWidgetInfo(Pointer(HandleWnd));
  if Info = nil then Exit;
  
  AMsg.Msg := Msg;
  AMsg.WParam := wParam;
  AMsg.LParam := lParam;
  
  Result := DeliverMessage(Info.LCLObject, AMsg);
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

{------------------------------------------------------------------------------
  Method:  SetBkColor
  Params:  DC    - Handle to device context
           Color - Background color value
  Returns: The previous background color if succeeds, otherwise CLR_INVALID

  Sets the current background color to the specified color value
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetBkColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ADC: TCarbonDeviceContext;
begin
  Result := CLR_INVALID;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);

  Result := ADC.BkColor;
  ADC.BkColor := Color;
end;

{------------------------------------------------------------------------------
  Method:  SetBkMode
  Params:  DC     - Handle to device context
           BkMode - Flag specifying background mode
  Returns: The previous background mode if suceeds, otherwise 0

  Sets the background mix mode of the specified device context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetBkMode(DC: HDC; BkMode: Integer): Integer;
var
  ADC: TCarbonDeviceContext;
begin
  Result := 0;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);

  Result := ADC.BkMode;
  ADC.BkMode := BkMode;
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
  Info: TCarbonWidgetInfo;
begin
  Result := GetFocus;
  if HWnd = 0 then Exit;
  if HWnd = Result then Exit; // if window is already focused exit
  
  Info := GetWidgetInfo(Pointer(HWnd));
  Window := GetTopParentWindow(Info.LCLObject as TWinControl);
  
  ActivateWindow(Window, True); // can change focus!
  if HWnd <> THandle(Window) then
  begin
    GetKeyboardFocus(Window, Control);
    if Control <> ControlRef(HWnd) then
      SetKeyboardFocus(Window, ControlRef(HWnd), kControlFocusNextPart);
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetForegroundWindow
  Params:  HWnd - Handle of window
  Returns: If the function suceeds

  Brings the specified window to the top
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
begin
  Result := ActivateWindow(WindowRef(HWnd), True) = noErr;
end;

{------------------------------------------------------------------------------
  Method:  SetProp
  Params:  Handle - Handle of window
           Str    - Property name
           Data   - Property data
  Returns: If the function suceeds

  Adds a new entry or changes an existing entry in the property list of the
  specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean;
var
  Info: TCarbonWidgetInfo;
begin
  Result := False;
  Info := GetWidgetInfo(Pointer(Handle));
  if Info = nil then Exit;
  
  Info.Properties[Str] := Data;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  SetROP2
  Params:  DC		- Handle to device context
           Mode	- Foreground mixing mode
  Returns: The previous mode if succeeds, otherwise 0
  
  Sets the specified foreground mixing mode to the device context
  TODO: implement all modes
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
var
  ADC: TCarbonDeviceContext;
begin
  Result := 0;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);

  Result := ADC.ROP2;
  ADC.ROP2 := Mode;
end;

{------------------------------------------------------------------------------
  Method:  SetScrollInfo
  Params:  Handle     - Handle of window
           SBStyle    - Scroll bar flag
           ScrollInfo - Scrolling info
           bRedraw    - Redraw the scroll bar?
  Returns: The old position value

  Sets the parameters of a scroll bar
  TODO: set info of window scrollbars
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer;
  ScrollInfo: TScrollInfo; bRedraw: Boolean): Integer;
begin
  Result := 0;
  if Handle = 0 then Exit;
  
  if SBStyle = SB_CTL then
  begin
    Result := GetControl32BitValue(ControlRef(Handle));
    if (SIF_RANGE and ScrollInfo.fMask) > 0 then
    begin
      SetControl32BitMinimum(ControlRef(Handle), ScrollInfo.nMin);
      SetControl32BitMaximum(ControlRef(Handle), ScrollInfo.nMax);
    end;
    if (SIF_POS and ScrollInfo.fMask) > 0 then
      SetControl32BitValue(ControlRef(Handle), ScrollInfo.nPos);
    if (SIF_PAGE and ScrollInfo.fMask) > 0 then
      SetControlviewSize(ControlRef(Handle), ScrollInfo.nPage);
  end
  else
    // TODO: SB_VERT, SB_HORZ
    DebugLn('TCarbonWidgetSet.SetScrollInfo TODO SB_VERT, SB_HORZ');
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

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  DC    - Handle to device context.
           Color - Specifies the color of the text
  Returns: The previous color if succeeds, CLR_INVALID otherwise

  Sets the text color for the specified device context to the specified color
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ADC: TCarbonDeviceContext;
begin
  Result := CLR_INVALID;
  if not IsValidDC(DC) then Exit;
  ADC := TCarbonDeviceContext(DC);
  
  Result := ADC.TextColor;
  ADC.TextColor := Color;
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
  TODO: solve maximized <-> normal bug
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

{------------------------------------------------------------------------------
  Method:  StretchMaskBlt
  Params:  DestDC              - Handle to destination device context
           X, Y                - Left/top corner of the destination rectangle
           Width, Height       - Size of the destination rectangle
           SrcDC               - Handle to source device context
           XSrc, YSrc          - Left/top corner of the source rectangle
           SrcWidth, SrcHeight - Size of the source rectangle
           Mask                - Handle of a monochrome bitmap (IGNORED)
           XMask, YMask        - Left/top corner of the mask rectangle
           Rop                 - Raster operation to be performed (TODO)
  Returns: If the function succeeds

  Copies a bitmap from a source rectangle into a destination rectangle using
  the specified mask and raster operations. If needed it resizes the bitmap to
  fit the dimensions of the destination rectangle. Sizing is done according to
  the stretching mode currently set in the destination device context.
  TODO: copy from any canvas
        ROP
        stretch mode
        SrcX, SrcY, SrcWidth, SrcHeight
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  Result := False;
  if not IsValidDC(DestDC) then
  begin
    DebugLn('StretchMaskBlt error - invalid destination device context!');
    Exit;
  end;
  
  if (not IsValidDC(SrcDC)) or not (TCarbonDeviceContext(SrcDC) is TCarbonBitmapContext) or
   (TCarbonBitmapContext(SrcDC).Bitmap = nil) then
  begin
    DebugLn('StretchMaskBlt error - invalid source device context!');
    Exit;
  end;
  
  CGContextDrawImage(TCarbonDeviceContext(SrcDC).CGContext,
    GetCGRect(X, -Y, Width, -Y - Height),
    TCarbonBitmapContext(SrcDC).Bitmap.CGImage);
  Result := True;
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
  selected font
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.TextOut(DC: HDC; X, Y: Integer; Str: Pchar;
  Count: Integer): Boolean;
begin
  Result := ExtTextOut(DC, X, Y, 0, nil, Str, Count, nil);
end;

{------------------------------------------------------------------------------
  Method:  UpdateWindow
  Params:  Handle - Handle to window
  Returns: If the function succeeds

  Updates the dirty areas of the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
  Result := False;
  
  case GetWidgetType(Pointer(Handle)) of
    cwtControlRef:
      Result := HIViewRender(HIViewRef(Handle)) = noErr;
    cwtWindowRef:
      begin
        Result := HIViewRender(GetCarbonWindowContent(Handle)) = noErr;
      end;
  end;
end;

function TCarbonWidgetSet.WindowFromPoint(Point: TPoint): HWND;
begin
  Result:=inherited WindowFromPoint(Point);
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

