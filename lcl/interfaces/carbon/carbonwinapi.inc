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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ClientToScreen P: ' + DbgS(P));
  {$ENDIF}
  
  if not CheckWidget(Handle, 'ClientToScreen') then Exit;
  Result := TCarbonWidget(Handle).GetScreenBounds(R);
  
  if Result then
  begin
    Inc(P.X, R.Left);
    Inc(P.Y, R.Top);
    
    Result := TCarbonWidget(Handle).GetClientRect(R);
    
    if Result then
    begin
      Inc(P.X, R.Left);
      Inc(P.Y, R.Top);
    end;
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ClientToScreen Result: ' + DbgS(Result) + ' P: ' + DbgS(P));
  {$ENDIF}
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

{------------------------------------------------------------------------------
  Method:  ComboBoxDropDown
  Params:  Handle   - Handle to combo box
           DropDown - Show list
  Returns: If hte function succeeds

  Shows or hides the combo box list
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ComboBoxDropDown(Handle: HWND; DropDown: boolean
  ): boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ComboBoxDropDown Handle: ' + DbgS(Handle) + ' Drop: ' + DbgS(DropDown));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(Handle, 'ComboBoxDropDown') then Exit;

  if OSError(HIComboBoxSetListVisible(AsControlRef(Handle), DropDown), Self,
    'ComboBoxDropDown', 'HIComboBoxSetListVisible') then Exit;
    
  Result := True;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateBitmap');
  {$ENDIF}
  
  Result := HBITMAP(TCarbonBitmap.Create(Width, Height, BitCount, BitmapBits));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.CreateBitmapFromRawImage
  Params:  RawImage         - Source raw image
           Bitmap           - Destination bitmap object
           MaskBitmap       - Destination mask object
           AlwaysCreateMask
  Returns: If the function suceeds

  Creates a bitmap from the specified raw image
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateBitmapFromRawImage');
  {$ENDIF}
  
  Result := False;
  Bitmap := 0;
  MaskBitmap := 0;

  Bitmap := HBITMAP(TCarbonBitmap.Create(RawImage.Description.Width,
   RawImage.Description.Height, 32, RawImage.Data));
  Result := True;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateBrushIndirect');
  {$ENDIF}
  
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateCompatibleDC');
  {$ENDIF}
  
  Result := HDC(TCarbonBitmapContext.Create);
end;

{------------------------------------------------------------------------------
  Method:  CreateCursor
  Params:  ACursorInfo - Cursor info as in win32
  Returns: Handle to a cursor

  Creates a cursor from bitmap and mask
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateCursor(ACursorInfo: PIconInfo): HCURSOR;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateCursor');
  {$ENDIF}
  
  Result := HCURSOR(TCarbonCursor.CreateFromInfo(ACursorInfo));
end;

{------------------------------------------------------------------------------
  Method:  CreateFontIndirect
  Params:  LogFont - Font characteristic
  Returns: Handle to the font

  Creates new font with specified characteristic
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateFontIndirect');
  {$ENDIF}
  
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateFontIndirectEx');
  {$ENDIF}
  
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreatePenIndirect');
  {$ENDIF}
  
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

  Deletes the specified critical section
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.DeleteCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DeleteCriticalSection Section: ' + DbgS(CritSection));
  {$ENDIF}
  
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DeleteDC DC: ' + DbgS(hDC));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(hDC, 'DeleteDC') then Exit;
  
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DeleteObject GDIObject: ' + DbgS(GDIObject));
  {$ENDIF}
  
  Result := False;
  if not CheckGDIObject(GDIObject, 'DeleteObject') then Exit;
  
  TCarbonGDIObject(GDIObject).Free;
  Result := True;
end;

function TCarbonWidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
  Result:=inherited DestroyCaret(Handle);
end;

{------------------------------------------------------------------------------
  Method:  DestroyCursor
  Params:  Handle - Handle to cursor
  Returns: If the function succeeds

  Destroy previously created cursor
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DestroyCursor(Handle: HCURSOR): Boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DestroyCursor Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := False;
  if not CheckCursor(Handle, 'DestroyCursor') then Exit;
  TCarbonCursor(Handle).Free;
end;

function TCarbonWidgetSet.DrawFrameControl(DC: HDC; const Rect: TRect; uType,
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Ellipse DC: ' + DbgS(DC) + ' R: ' + DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}
  
  if (X1 = X2) or (Y1 = Y2) then
  begin
    Result := True;
    Exit;
  end;
  
  Result := False;
  if not CheckDC(DC, 'Ellipse') then Exit;

  ADC := TCarbonDeviceContext(DC);
  
  R := GetCGRectSorted(X1, Y1, X2, Y2);
  R.origin.x := R.origin.x + 0.5;
  R.origin.y := R.origin.y + 0.5;
  R.size.width := R.size.width - 1;
  R.size.height := R.size.height - 1;
  
  CGContextBeginPath(ADC.CGContext);
  CGContextAddEllipseInRect(ADC.CGContext, R);
  CGContextDrawPath(ADC.CGContext, kCGPathFillStroke);

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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.EnableWindow Handle: ' + DbgS(hWnd) + 'Enable: ' + DbgS(bEnable));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(HWnd, 'EnableWindow') then Exit;
  Result := TCarbonWidget(HWnd).Enable(bEnable);
end;

function TCarbonWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct
  ): Integer;
begin
  Result:=inherited EndPaint(Handle, PS);
end;

{------------------------------------------------------------------------------
  Method:  EnterCriticalSection
  Params:  CritSection - Critical section to be entered

  Enters the specified critical section
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.EnterCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.EnterCriticalSection Section: ' + DbgS(CritSection));
  {$ENDIF}
  
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
  Params:  DC        - Handle to the device context (ignored)
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
  FontName: UTF8String;
  EnumLogFont: TEnumLogFontEx;
  Metric: TNewTextMetricEx;
  FontType, I: Integer;
const AName = 'TCarbonWidgetSet.EnumFontFamiliesEx';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.EnumFontFamiliesEx');
  {$ENDIF}
  
  Result := 0;
  if (lpLogFont = nil) or not Assigned(Callback) then Exit;
  
  // enumarate ATSUI font families:
  
  if (lpLogFont^.lfCharSet = DEFAULT_CHARSET) and (lpLogFont^.lfFaceName = '') then
  begin
    // all system fonts
    if OSError(ATSUFontCount(FamilyCount), AName, 'ATSUFontCount') then Exit;
    
    GetMem(FamilyListPtr, SizeOf(ATSUFontID) * FamilyCount);
    try
      if OSError(ATSUGetFontIDs(FamilyListPtr, FamilyCount, nil), AName, 'ATSUGetFontIDs') then Exit;

      {$IFDEF VerboseWinAPI}
        DebugLn(AName + ' Found: ' + DbgS(FamilyCount));
      {$ENDIF}

      PFamily := FamilyListPtr;
      for I := 0 to Pred(FamilyCount) do
      begin
        // retrieve font name length
        if OSError(ATSUFindFontName(PFamily^, kFontFamilyName, kFontMacintoshPlatform,
          kFontRomanScript, kFontEnglishLanguage, NameLength, nil,
          @NameLength, nil), AName, 'ATSUFindFontName', 'Length') then Continue;
        
        SetLength(FontName, NameLength);

        // retrieve font name, UTF-16 encoded
        if OSError(ATSUFindFontName(PFamily^, kFontFamilyName, kFontMacintoshPlatform,
          kFontRomanScript, kFontEnglishLanguage, NameLength,
          @FontName[1], @NameLength, nil), AName, 'ATSUFindFontName', 'Name') then Continue;
          
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
    DebugLn(AName + ' with specific face or specific char set is not implemented!');
  end;
end;

{------------------------------------------------------------------------------
  Method:  ExcludeClipRect
  Params:  DC - Handle to device context
           Left, Top, Right, Bottom - Rectangle coordinates
  Returns: See bellow

  Subtracts all intersecting points of the passed bounding rectangle from the
  current clipping region of the device context. The result can be one of the
  following constants: ERROR, NULLREGION, SIMPLEREGION, COMPLEXREGION.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ExcludeClipRect(DC: HDC; Left, Top, Right,
  Bottom: Integer): Integer;
var
  ClipBox: TRect;
  Rects: CGRectArray;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ExcludeClipRect DC: ' + DbgS(DC) + ' R: ' +
      DbgS(Classes.Rect(Left, Top, Right, Bottom)));
  {$ENDIF}
  
  Result := ERROR;
  if not CheckDC(DC, 'ExcludeClipRect') then Exit;
  
  if (Left < Right) and (Top < Bottom) then
  begin
    ClipBox := CGRectToRect(
      CGContextGetClipBoundingBox(TCarbonDeviceContext(DC).CGContext));
      
    Rects := ExcludeRect(ClipBox, Classes.Rect(Left, Top, Right, Bottom));

    if Length(Rects) > 0 then
      CGContextClipToRects(TCarbonDeviceContext(DC).CGContext, @Rects[0],
        Length(Rects))
    else
      CGContextClipToRect(TCarbonDeviceContext(DC).CGContext, CGRectZero);
  end;
  
  Result := COMPLEXREGION;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ExtTextOut DC: ' + DbgS(DC) + ' ' + DbgS(X) +
      ', ' + DbgS(Y) + ' Str: ' + Str);
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'ExtTextOut') then Exit;
  ADC := TCarbonDeviceContext(DC);

  if not ADC.BeginTextRender(Str, Count, TextLayout) then Exit;
  try
    // get text ascent
    if ATSUGetUnjustifiedBounds(TextLayout, kATSUFromTextBeginning,
      kATSUToTextEnd, TextBefore, TextAfter, Ascent, Descent) <> noErr then Exit;

    // fill drawed text background
    if (Options and ETO_OPAQUE) > 0 then
    begin
      ADC.BkBrush.Apply(ADC, False); // do not use ROP2
      CGContextFillRect(ADC.CGContext, GetCGRectSorted(X - TextBefore shr 16,
        -Y, X + TextAfter shr 16, -Y - (Ascent + Descent) shr 16));
    end;
        
    // apply text color
    ADC.TextBrush.Apply(ADC, False); // do not use ROP2


    // finally draw the text
    Result := ATSUDrawText(TextLayout, kATSUFromTextBeginning, kATSUToTextEnd,
      X shl 16 - TextBefore, -(Y shl 16) - Ascent) = noErr;
    //if Result then DebugLn('ExtTextOut text drawed');
  finally
    ADC.EndTextRender(TextLayout);
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ExtTextOut Result: ' + DbgS(Result));
  {$ENDIF}
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
  SavedBrush: HBRUSH;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.FillRect DC: ' + DbgS(DC) + ' R: ' + DbgS(Rect) +
      ' Brush: ' + DbgS(Brush));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'FillRect') then Exit;
  if not (TObject(Brush) is TCarbonBrush) then
  begin
    DebugLn('TCarbonWidgetSet.FillRect Error - invalid Brush!');
    Exit;
  end;

  ADC := TCarbonDeviceContext(DC);
  
  SavedBrush := HBRUSH(ADC.CurrentBrush);
  TCarbonBrush(Brush).Apply(ADC, False); // do not use ROP2
  try
    CGContextFillRect(ADC.CGContext, RectToCGRect(Rect));
  finally
    SelectObject(DC, SavedBrush);
    if SavedBrush = Brush then
      TCarbonBrush(SavedBrush).Apply(ADC); // ensure that saved brush is applied
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  Frame
  Params:  DC    - Handle to device context
           ARect - Bounding box of frame
  Returns: > 0 if the function succeeds

  Draws a border in Carbon native style
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.Frame(DC: HDC; const ARect: TRect): Integer;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Frame DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, 'Frame') then Exit;
  
  Result := Integer(Rectangle(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom));
end;

{------------------------------------------------------------------------------
  Method:  Frame3D
  Params:  DC         - Handle to device context
           ARect      - Bounding box of frame
           FrameWidth - Frame width
           Style      - Frame style
  Returns: If the function succeeds

  Draws a 3D border in Carbon native style
  TODO: lowered style
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.Frame3d(DC: HDC; var ARect: TRect;
  const FrameWidth: integer; const Style: TBevelCut): Boolean;
var
  I, D: Integer;
  DrawInfo: HIThemeGroupBoxDrawInfo;
  ADC: TCarbonDeviceContext;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Frame3D DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect) +
      ' Width: ' + DbgS(FrameWidth) + ' Style: ' + DbgS(Integer(Style)));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'Frame3d') then Exit;
  if FrameWidth <= 0 then Exit;
  
  ADC := TCarbonDeviceContext(DC);
  
  if Style in [bvLowered, bvRaised] then
  begin
    GetThemeMetric(kThemeMetricPrimaryGroupBoxContentInset, D);

    // draw frame as group box
    DrawInfo.version := 0;
    DrawInfo.state := kThemeStateActive;
    DrawInfo.kind := kHIThemeGroupBoxKindPrimary;

    for I := 1 to FrameWidth do
    begin
      HIThemeDrawGroupBox(RectToCGRect(ARect), DrawInfo,
        ADC.CGContext, kHIThemeOrientationNormal);
      InflateRect(ARect, -D, -D);
    end;
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  FrameRect
  Params:  DC    - Handle to device context
           ARect - Bounding box of frame
           hBr   - Border brush (ignored)
  Returns: > 0 if the function succeeds

  Draws a border with the specified brush in Carbon native style
  TODO: use brush
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.FrameRect(DC: HDC; const ARect: TRect;
  hBr: HBRUSH): Integer;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.FrameRect DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect) +
      ' Brush: ' + DbgS(hBr));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, 'FrameRect') then Exit;

  Result := Integer(Rectangle(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom));
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

  Result := HWND(GetCarbonWindow(Window));
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetActiveWindow Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCarbonWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;
  Bits: Pointer): Longint;
begin
  Result:=inherited GetBitmapBits(Bitmap, Count, Bits);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.GetBitmapRawImageDescription
  Params:  Bitmap - Bitmap
           Desc   - Bitmap raw image description
  Returns: If the function succeeds

  Describes the inner format utilized by Carbon and specific information
  for the specified bitmap
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): Boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetBitmapRawImageDescription Bitmap: ' + DbgS(Bitmap));
  {$ENDIF}
  
  Result := False;
  if Desc = nil then
  begin
    DebugLn('TCarbonWidgetSet.GetBitmapRawImageDescription error - Desc = nil!');
    Exit;
  end;
  if not CheckBitmap(Bitmap, 'GetBitmapRawImageDescription') then Exit;

  FillStandardDescription(Desc^);

  Desc^.Width := TCarbonBitmap(Bitmap).Width;
  Desc^.Height := TCarbonBitmap(Bitmap).Height;
  
  Result := True;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetClientBounds Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  if not CheckWidget(Handle, 'GetClientBounds') then Exit;
  Result := TCarbonWidget(Handle).GetClientRect(ARect);
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetClientBounds Result: ' + DbgS(Result));
  {$ENDIF}
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetClientRect Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  if not CheckWidget(Handle, 'GetClientRect') then Exit;
  
  Result := TCarbonWidget(Handle).GetClientRect(ARect);
  if Result then OffsetRect(ARect, -ARect.Left, -ARect.Top);
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetClientRect Result: ' + DbgS(Result));
  {$ENDIF}
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetClipBox DC: ' + DbgS(DC));
  {$ENDIF}
  
  Result := ERROR;
  if not CheckDC(DC, 'GetClipBox') then Exit;
  
  if lpRect <> nil then
    lpRect^ := CGRectToRect(
      CGContextGetClipBoundingBox(TCarbonDeviceContext(DC).CGContext));

  Result := COMPLEXREGION;
  
  {$IFDEF VerboseWinAPI}
    if lpRect <> nil then
      DebugLn('TCarbonWidgetSet.GetClipBox Rect: ' + DbgS(lpRect^));
  {$ENDIF}
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
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetCursorPos Point: ' + DbgS(lpPoint));
  {$ENDIF}
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDC HWnd: ' + DbgS(HWnd));
  {$ENDIF}
  
  Result := 0;
  
  if HWnd = 0 then Result := HDC(TCarbonScreenContext.Create)
  else
  begin
    if not CheckWidget(HWnd, 'GetDC') then Exit;
    
    // TODO: save DC
    Result := HDC(TCarbonWidget(HWnd).Context);
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDC Result: ' + DbgS(Result));
  {$ENDIF}
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceCaps DC: ' + DbgS(DC) + ' Index: ' + DbgS(Index));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, 'GetDeviceCaps') then Exit;
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
  else
    DebugLn('TCarbonWidgetSet.GetDeviceCaps TODO Index: ' + DbgS(Index));
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceCaps Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.GetDeviceRawImageDescription
  Params:  DC   - Handle to device context
           Desc - Pointer to raw image description
  Returns: If the function succeeds

  Retrieves the standard image format utilized by Carbon
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDeviceRawImageDescription(DC: HDC;
  Desc: PRawImageDescription): boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceRawImageDescription DC: ' + DbgS(DC));
  {$ENDIF}
  
  if Desc = nil then
  begin
    Result := False;
    Exit;
  end;

  FillStandardDescription(Desc^);
  Result := True;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceSize DC: ' + DbgS(DC));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'GetDeviceSize') then Exit;
  ADC := TCarbonDeviceContext(DC);
  
  P := ADC.Size;
  Result := True;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceSize Size: ' + DbgS(P));
  {$ENDIF}
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetFocus');
  {$ENDIF}
  
  Result := 0;
  Window := WindowRef(GetUserFocusWindow);
  if Window = nil then Exit;
  
  Control := nil;
  GetKeyboardFocus(Window, Control);
  if Control <> nil then
    Result := HWND(GetCarbonControl(Control))
  else Result := HWND(GetCarbonWindow(Window));
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetFocus Result: ' + DbgS(Result));
  {$ENDIF}
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
  {$IFDEF VerboseKeyboard}
    DebugLn('TCarbonWidgetSet.GetKeyState ' + DbgSVKCode(nVirtKey));
  {$ENDIF}
  
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
    DebugLn('TCarbonWidgetSet.GetKeyState TODO ', DbgSVKCode(nVirtkey));
  end;
  
  {$IFDEF VerboseKeyboard}
    DebugLn('TCarbonWidgetSet.GetKeyState Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.GetObject
  Params:  GDIObj  - GDI object
           BufSize - Size of specified buffer
           Buf     - Pointer to the buffer
  Returns: The size written to the buffer

  Retrieves the GDI object information
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer;
  Buf: Pointer): Integer;
var
  AObject: TCarbonGDIObject;
  DIB: TDIBSection;
  Width, Height: Integer;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetObject GDIObj: ' + DbgS(GDIObj));
  {$ENDIF}
  
  Result := 0;
  if not CheckGDIObject(GDIObj, 'GetObject') then Exit;
  
  AObject := TCarbonGDIObject(GDIObj);
  if AObject is TCarbonBitmap then
  begin
    if Buf = nil then
    begin
      Result := SizeOf(TDIBSection);
      Exit;
    end;
     
    Width := TCarbonBitmap(AObject).Width;
    Height := TCarbonBitmap(AObject).Height;

    FillChar(DIB, SizeOf(TDIBSection), 0);

    {dsBM - BITMAP}
    DIB.dsBm.bmType := $4D42;
    DIB.dsBm.bmWidth := Width;
    DIB.dsBm.bmHeight := Height;
    DIB.dsBm.bmWidthBytes := 0;
    DIB.dsBm.bmPlanes := 1;
    DIB.dsBm.bmBitsPixel := 32;
    DIB.dsBm.bmBits := nil;

    {dsBmih - BITMAPINFOHEADER}
    DIB.dsBmih.biSize := 40;
    DIB.dsBmih.biWidth := Width;
    DIB.dsBmih.biHeight := Height;
    DIB.dsBmih.biPlanes := DIB.dsBm.bmPlanes;
    DIB.dsBmih.biCompression := 0;
    DIB.dsBmih.biSizeImage := 0;
    DIB.dsBmih.biXPelsPerMeter := 0;
    DIB.dsBmih.biYPelsPerMeter := 0;
    DIB.dsBmih.biClrUsed   := 0;
    DIB.dsBmih.biClrImportant := 0;
    DIB.dsBmih.biBitCount := 32;

    if BufSize >= SizeOf(TDIBSection) then
    begin
      PDIBSection(Buf)^ := DIB;
      Result := SizeOf(TDIBSection);
    end
    else
      if BufSize > 0 then
      begin
        System.Move(DIB, Buf^, BufSize);
        Result := BufSize;
      end;
  end
  else
    DebugLn('TCarbonWidgetSet.GetObject Font, Brush, Pen TODO');
end;

{------------------------------------------------------------------------------
  Method:  GetParent
  Params:  Handle - Handle of child window
  Returns: The handle of the parent window

  Retrieves the handle of the specified child window's parent window.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetParent(Handle: HWND): HWND;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetParent Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := 0;
  if not CheckWidget(Handle, 'GetParent') then Exit;
  
  if TCarbonWidget(Handle) is TCarbonControl then
     Result := HWnd(GetCarbonWidget(HIViewGetSuperview(AsControlRef(Handle))));
  // Carbon windows has no parent
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetParent Result: ' + DbgS(Result));
  {$ENDIF}
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetProp Handle: ' + DbgS(Handle) + ' Str: ' + Str);
  {$ENDIF}
  
  Result := nil;
  if not CheckWidget(Handle, 'GetProp') then Exit;

  Result := TCarbonWidget(Handle).Properties[Str];
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetProp Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCarbonWidgetSet.GetRawImageFromDevice(SrcDC: HDC;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
begin
  Result:=inherited GetRawImageFromDevice(SrcDC, SrcRect, NewRawImage);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.GetRawImageFromBitmap
  Params:  SrcBitmap     - Source bitmap
           SrcMaskBitmap - Source mask (ignored)
           SrcRect       - Source rect (TODO)
           NewRawImage   - New raw image
  Returns: If the function succeeds

  Creates a raw image from the specified bitmap
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetRawImageFromBitmap(SrcBitmap,
  SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage
  ): boolean;
var
  Bitmap: TCarbonBitmap;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetRawImageFromBitmap SrcBitmap: ' + DbgS(SrcBitmap) +
      ' SrcMaskBitmap: ' + DbgS(SrcMaskBitmap) + ' SrcRect: ' + DbgS(SrcRect));
  {$ENDIF}
  
  Result := False;
  if not CheckBitmap(SrcBitmap, 'GetRawImageFromBitmap') then Exit;
  
  Bitmap := TCarbonBitmap(SrcBitmap);

  FillChar(NewRawImage, SizeOf(NewRawImage), 0);
  FillStandardDescription(NewRawImage.Description);

  NewRawImage.Description.Width := Bitmap.Width;
  NewRawImage.Description.Height := Bitmap.Height;

  NewRawImage.DataSize := Bitmap.DataSize;
  ReAllocMem(NewRawImage.Data, NewRawImage.DataSize);
  if NewRawImage.DataSize > 0 then
    System.Move(Bitmap.Data^, NewRawImage.Data^, NewRawImage.DataSize);

  Result := True;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetStockObject Value: ' + DbgS(Value));
  {$ENDIF}
  
  Result := 0;
  
  case Value of
    NULL_BRUSH: // null brush (equivalent to HOLLOW_BRUSH).
      Result := HBRUSH(StockNullBrush);
    DEFAULT_GUI_FONT, SYSTEM_FONT:
      Result := HFONT(StockSystemFont);
  else
    DebugLn('TCarbonWidgetSet.GetStockObject TODO ', DbgS(Value));
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetSysColor NIndex: ' + DbgS(NIndex));
  {$ENDIF}
  
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
  COLOR_INFOTEXT, COLOR_WINDOWTEXT:
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
  //COLOR_HOTLIGHT:
  COLOR_INFOBK, COLOR_WINDOW, COLOR_FORM:
    R := GetThemeBrushAsColor(kThemeBrushDocumentWindowBackground, Depth, True, C);
  else
    DebugLn('TCarbonWidgetSet.GetSysColor TODO ', DbgS(NIndex));
  end;
  
  if OSError(R, Self, 'GetSysColor', 'NIndex = ' + DbgS(NIndex)) then Exit;

  Result := RGBColorToColor(C);
    
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetSysColor Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  GetSystemMetrics
  Params:  NIndex - System metric to retrieve
  Returns: The requested system metric value

  Retrieves various system metrics.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetSystemMetrics(NIndex: Integer): Integer;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetSystemMetrics NIndex: ' + DbgS(NIndex));
  {$ENDIF}
  
  Result := 0;
  
  case NIndex of
    SM_CYHSCROLL, SM_CXVSCROLL:
      GetThemeMetric(kThemeMetricScrollBarWidth, Result);
    SM_CXSCREEN: Result := CGDisplayPixelsWide(CGMainDisplayID);
    SM_CYSCREEN: Result := CGDisplayPixelsHigh(CGMainDisplayID);
    SM_CXCURSOR,
    SM_CYCURSOR:
      begin
        if TCarbonCursor.HardwareCursorsSupported then
          Result := 64 else
          Result := 16;
      end;
  else
    DebugLn('TCarbonWidgetSet.GetSystemMetrics TODO ', DbgS(NIndex));;
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetSystemMetrics Result: ' + DbgS(Result));
  {$ENDIF}
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
const AName = 'GetTextExtentPoint';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetTextExtentPoint DC: ' + DbgS(DC) + ' Str: ' + Str);
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, AName) then
    // workaround for calculating metrics of invisible controls
    ADC := DefaultContext
  else
    ADC := TCarbonDeviceContext(DC);

  if not ADC.BeginTextRender(Str, Count, TextLayout) then Exit;
  try
    // finally compute the text dimensions
    if OSError(ATSUGetUnjustifiedBounds(TextLayout, kATSUFromTextBeginning,
        kATSUToTextEnd, TextBefore, TextAfter, Ascent, Descent),
      Self, AName, 'ATSUGetUnjustifiedBounds') then Exit;
      
    Size.cx := (TextAfter - TextBefore) shr 16;
    Size.cy := (Descent + Ascent) shr 16;

    Result := True;
  finally
    ADC.EndTextRender(TextLayout);
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetTextExtentPoint Size: ' + DbgS(Size));
  {$ENDIF}
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
  TextLayout: ATSUTextLayout;
  TextBefore, TextAfter, Ascent, Descent: ATSUTextMeasurement;
const AName = 'GetTextMetrics';
      AGetAttrName = 'ATSUGetAttribute';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetTextMetrics DC: ' + DbgS(DC));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, AName) then
    // workaround for calculating metrics of invisible controls
    ADC := DefaultContext
  else
    ADC := TCarbonDeviceContext(DC);
    
  if not (ADC.CurrentFont is TCarbonFont) then
    TextStyle := DefaultTextStyle
  else
    TextStyle := ADC.CurrentFont.Style;
  
  FillChar(TM, SizeOf(TM), 0);

  // According to the MSDN library, TEXTMETRIC:
  // the average char width is generally defined as the width of the letter x
  if not ADC.BeginTextRender('x', 1, TextLayout) then Exit;
  try
    if OSError(ATSUGetUnjustifiedBounds(TextLayout, kATSUFromTextBeginning,
      kATSUToTextEnd, TextBefore, TextAfter, Ascent, Descent), AName,
      'ATSUGetUnjustifiedBounds') then Exit
  finally
    ADC.EndTextRender(TextLayout);
  end;
  
  TM.tmAscent := Ascent shr 16;
  TM.tmDescent := Descent shr 16;
  TM.tmHeight := (Ascent + Descent) shr 16;
  
  if OSError(ATSUGetAttribute(TextStyle, kATSULeadingTag, SizeOf(M), @M, nil),
    Self, AName, AGetAttrName, 'kATSULeadingTag', kATSUNotSetErr) then Exit;
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
  
  if OSError(ATSUGetAttribute(TextStyle, kATSUQDBoldfaceTag, SizeOf(B), @B, nil),
    Self, AName, AGetAttrName, 'kATSUQDBoldfaceTag', kATSUNotSetErr) then Exit;
  if B then TM.tmWeight := FW_NORMAL
       else TM.tmWeight := FW_BOLD;
  
  if OSError(ATSUGetAttribute(TextStyle, kATSUQDItalicTag, SizeOf(B), @B, nil),
    Self, AName, AGetAttrName, 'kATSUQDItalicTag', kATSUNotSetErr) then Exit;
  TM.tmItalic := Byte(B);
  
  if OSError(ATSUGetAttribute(TextStyle, kATSUQDUnderlineTag, SizeOf(B), @B, nil),
    Self, AName, AGetAttrName, 'kATSUQDUnderlineTag', kATSUNotSetErr) then Exit;
  TM.tmUnderlined := Byte(B);
  
  if OSError(ATSUGetAttribute(TextStyle, kATSUStyleStrikeThroughTag, SizeOf(B), @B, nil),
    Self, AName, AGetAttrName, 'kATSUStyleStrikeThroughTag', kATSUNotSetErr) then Exit;
  TM.tmStruckOut := Byte(B);

  // TODO: get these from font
  TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;
  TM.tmCharSet := DEFAULT_CHARSET;
  
  Result := True;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetTextMetrics Result: ' + DbgS(Result) +
      ' TextMetric: ' + DbgS(TM));
  {$ENDIF}
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowRect Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := 0;
  if not CheckWidget(Handle, 'GetWindowRect') then Exit;
  Result := Integer(TCarbonWidget(Handle).GetScreenBounds(ARect));
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowRect R: ' + DbgS(ARect));
  {$ENDIF}
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowRelativePosition Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(Handle, 'GetWindowRelativePosition') then Exit;
  Result := TCarbonWidget(Handle).GetBounds(ARect);
  
  if not Result then Exit;
  Left := ARect.Left;
  Top := ARect.Top;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowRelativePosition Left: ' + DbgS(Left) +
      ' Top: ' + DbgS(Top));
  {$ENDIF}
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowSize Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(Handle, 'GetWindowSize') then Exit;
  Result := TCarbonWidget(Handle).GetBounds(ARect);
    
  if not Result then Exit;
  Width := ARect.Right - ARect.Left;
  Height := ARect.Bottom - ARect.Top;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowSize Width: ' + DbgS(Width) + ' Height:'
      + DbgS(Height));
  {$ENDIF}
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
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.InitializeCriticalSection Section: ' + DbgS(CritSection));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  IntersectClipRect
  Params:  DC - Handle to device context
           Left, Top, Right, Bottom - Rectangle coordinates
  Returns: See bellow

  Changes the current clipping region of the device context to intersection with
  the specified rectangle. The result can be one of the following constants:
  ERROR, NULLREGION, SIMPLEREGION, COMPLEXREGION.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IntersectClipRect(DC: HDC; Left, Top, Right,
  Bottom: Integer): Integer;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IntersectClipRect DC: ' + DbgS(DC) + ' R: ' +
      DbgS(Classes.Rect(Left, Top, Right, Bottom)));
  {$ENDIF}
  
  Result := ERROR;
  if not CheckDC(DC, 'IntersectClipRect') then Exit;
  
  CGContextClipToRect(TCarbonContext(DC).CGContext,
    RectToCGRect(Classes.Rect(Left, Top, Right, Bottom)));
  Result := COMPLEXREGION;
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.InvalidateRect Handle: ' + DbgS(AHandle));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(AHandle, 'InvalidateRect') then Exit;
  TCarbonWidget(AHandle).Invalidate(Rect);
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  IsWindowEnabled
  Params:  Handle - Handle of window
  Returns: True if window is enabled, false otherwise
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IsWindowEnabled Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(Handle, 'IsWindowEnabled') then Exit;
  Result := TCarbonWidget(Handle).IsEnabled;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IsWindowEnabled Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  IsWindowVisible
  Params:  Handle - Handle of window
  Returns: True if window is visible, false otherwise
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsWindowVisible(Handle: HWND): boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IsWindowVisible Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(Handle, 'IsWindowVisible') then Exit;
  Result := TCarbonWidget(Handle).IsVisible;
  
   {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IsWindowVisible Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  LeaveCriticalSection
  Params:  CritSection - Critical section to be left

  Leaves the specified critical section
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.LeaveCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.LeaveCriticalSection Section: ' + DbgS(CritSection));
  {$ENDIF}
  
  ACritSec := System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

{------------------------------------------------------------------------------
  Method:  LineTo
  Params:  DC - Handle to device context
           X  - X-coordinate of line's ending point
           Y  - Y-coordinate of line's ending point
  Returns: If the function succeeds

  Draws a line from the current position up to the specified point and updates
  the current position
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ADC: TCarbonDeviceContext;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.LineTo DC: ' + DbgS(DC) + ' ' + DbgS(X) + ', ' +
      DbgS(Y));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'LineTo') then Exit;

  ADC := TCarbonDeviceContext(DC);

  CGContextBeginPath(ADC.CGContext);
  // add 0.5 to both coordinates because context for better rasterization
  CGContextMoveToPoint(ADC.CGContext, ADC.PenPos.x + 0.5, ADC.PenPos.y + 0.5);
  CGContextAddLineToPoint(ADC.CGContext, X + 0.5, Y + 0.5);
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.MoveToEx DC: ' + DbgS(DC) + ' ' + DbgS(X) + ', ' +
      DbgS(Y));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'MoveToEx') then Exit;

  ADC := TCarbonDeviceContext(DC);

  if OldPoint <> nil then OldPoint^ := ADC.PenPos;
  ADC.PenPos.x := X;
  ADC.PenPos.y := Y;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  MoveWindowOrgEx
  Params:  DC - Handle to device context
           DX - Horizontal shift
           DY - Vertical shift
  Returns: If the function succeeds

  Moves origin of the device context by the specified shift
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.MoveWindowOrgEx DC: ' + DbgS(DC) + ' ' + DbgS(DX)
      + ', ' + DbgS(DY));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'MoveWindowOrgEx') then Exit;
  
  CGContextTranslateCTM(TCarbonContext(DC).CGContext, DX, DY);
  Result := True;
end;

function TCarbonWidgetSet.PeekMessage(var lpMsg: TMsg; Handle: HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): Boolean;
begin
  Result:=inherited PeekMessage(lpMsg, Handle, wMsgFilterMin, wMsgFilterMax,
    wRemoveMsg);
end;

{------------------------------------------------------------------------------
  Method:  PolyBezier
  Params:  DC        - Handle to device context
           Points    - Points defining the cubic Bézier curve
           NumPts    - Number of points passed
           Filled    - Fill the drawed shape
           Continous - Connect Bézier curves
  Returns: If the function succeeds

  Draws a cubic Bézier curves. The first curve is drawn from the first point to
  the fourth point with the second and third points being the control points.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: boolean): boolean;
var
  ADC: TCarbonDeviceContext;
  C1, C2: TPoint;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.PolyBezier DC: ' + DbgS(DC) + ' NumPts: ' +
      DbgS(NumPts) + ' Filled: ' + DbgS(Filled) + ' Continuous: ' +
      DbgS(Continuous));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'PolyBezier') then Exit;
  if Points = nil then Exit;
  if NumPts < 4 then Exit;

  ADC := TCarbonDeviceContext(DC);

  CGContextBeginPath(ADC.CGContext);

  if Continuous then
  begin
    CGContextMoveToPoint(ADC.CGContext, Points^.x, Points^.y);
    Dec(NumPts);

    while NumPts >= 3 do
    begin
      Inc(Points);
      C1 := Points^;
      Inc(Points);
      C2 := Points^;
      Inc(Points);
      CGContextAddCurveToPoint(ADC.CGContext, C1.x, C1.y, C2.x, C2.y,
        Points^.x, Points^.y);
      Dec(NumPts, 3);
    end;
  end
  else
  begin
    while NumPts >= 4 do
    begin
      CGContextMoveToPoint(ADC.CGContext, Points^.x, Points^.y);
      Inc(Points);
      C1 := Points^;
      Inc(Points);
      C2 := Points^;
      Inc(Points);
      CGContextAddCurveToPoint(ADC.CGContext, C1.x, C1.y, C2.x, C2.y,
        Points^.x, Points^.y);
      Inc(Points);
      Dec(NumPts, 4);
    end;
  end;

  if Filled and Continuous then
    CGContextDrawPath(ADC.CGContext, kCGPathFillStroke)
  else
    CGContextDrawPath(ADC.CGContext, kCGPathStroke);

  Result := True;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Polygon DC: ' + DbgS(DC) + ' NumPts: ' +
      DbgS(NumPts) + ' Winding: ' + DbgS(Winding));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'Polygon') then Exit;
  if Points = nil then Exit;
  if NumPts < 2 then Exit;

  ADC := TCarbonDeviceContext(DC);

  CGContextBeginPath(ADC.CGContext);
  CGContextMoveToPoint(ADC.CGContext, Points^.x + 0.5, Points^.y + 0.5);
  Dec(NumPts);

  while NumPts > 0 do
  begin
    Inc(Points);
    CGContextAddLineToPoint(ADC.CGContext, Points^.x + 0.5, Points^.y + 0.5);
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Polyline DC: ' + DbgS(DC) + ' NumPts: ' + DbgS(NumPts));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'Polyline') then Exit;
  if Points = nil then Exit;

  ADC := TCarbonDeviceContext(DC);
  
  CGContextBeginPath(ADC.CGContext);
  CGContextMoveToPoint(ADC.CGContext, Points^.x + 0.5, Points^.y + 0.5);
  Dec(NumPts);

  while NumPts > 0 do
  begin
    Inc(Points);
    CGContextAddLineToPoint(ADC.CGContext, Points^.x + 0.5, Points^.y + 0.5);
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Rectangle DC: ' + DbgS(DC) + ' R: ' +
      DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}
  
  if (X1 = X2) or (Y1 = Y2) then
  begin
    Result := True;
    Exit;
  end;
  
  Result := False;
  if not CheckDC(DC, 'Rectangle') then Exit;

  ADC := TCarbonDeviceContext(DC);

  R := GetCGRectSorted(X1, Y1, X2, Y2);
  R.origin.x := R.origin.x + 0.5;
  R.origin.y := R.origin.y + 0.5;
  R.size.width := R.size.width - 1;
  R.size.height := R.size.height - 1;

  CGContextBeginPath(ADC.CGContext);
  CGContextAddRect(ADC.CGContext, R);
  CGContextDrawPath(ADC.CGContext, kCGPathFillStroke);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  RectVisible
  Params:  DC    - Handle to device context
           ARect - Rectangle to test
  Returns: If the rectangle is not completely clipped away
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RectVisible(DC: HDC; const ARect: TRect): Boolean;
var
  ClipBox: CGRect;
  R: TRect;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RectVisible DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'RectVisible') then Exit;
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then Exit;
  
  // In Quartz 2D there is no direct access to clipping path of CGContext,
  // therefore we can only test bounding box of the clipping path.
  
  ClipBox := CGContextGetClipBoundingBox(TCarbonContext(DC).CGContext);
  Result := IntersectRect(R, ARect, CGRectToRect(ClipBox));
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RectVisible Result: ' + DbgS(Result) + ' Clip: ' + DbgS(CGRectToRect(ClipBox)));
  {$ENDIF}
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ReleaseDC HWnd: ' + DbgS(HWnd) + ' DC: ' + DbgS(DC));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, 'ReleaseDC') then Exit;
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RemoveProp Handle: ' + DbgS(Handle) + ' Str: ' +
      DbgS(Str));
  {$ENDIF}
  
  Result := 0;
  if not CheckWidget(Handle, 'RemoveProp') then Exit;

  Result := THandle(TCarbonWidget(Handle).Properties[Str]);
  TCarbonWidget(Handle).Properties[Str] := nil;
end;

{------------------------------------------------------------------------------
  Method:  RestoreDC
  Params:  DC      - Handle to device context
           SavedDC - Index of saved DC
  Returns: If the function succeeds

  Resores state of the device context to the state with the specified index
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RestoreDC DC: ' + DbgS(DC) + ' SavedDC: ' +
      DbgS(SavedDC));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DC, 'RestoreDC') then Exit;

  Result := TCarbonDeviceContext(DC).RestoreDC(SavedDC);
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RestoreDC Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCarbonWidgetSet.RoundRect(DC: hDC; X1, Y1, X2, Y2: Integer; RX,
  RY: Integer): Boolean;
begin
  Result:=inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

{------------------------------------------------------------------------------
  Method:  SaveDC
  Params:  DC - Handle to device context
  Returns: Saved DC index or 0 if failed

  Saves current state of the device context and returns its index
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SaveDC(DC: HDC): Integer;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SaveDC DC: ' + DbgS(DC));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, 'SaveDC') then Exit;
  
  Result := TCarbonDeviceContext(DC).SaveDC;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SaveDC Result: ' + DbgS(Result));
  {$ENDIF}
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
const AName = 'TCarbonWidgetSet.SelectObject';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SelectObject DC: ' + DbgS(DC) + ' GDIObj: ' +
      DbgS(GDIObj));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, AName) then Exit;
  if not CheckGDIObject(GDIObj, AName) then Exit;
  
  ADC := TCarbonDeviceContext(DC);
  AObject := TObject(GDIObj);
  
  if AObject is TCarbonBrush then // select brush
  begin
    Result := HBRUSH(ADC.CurrentBrush);
    ADC.CurrentBrush := TCarbonBrush(GDIObj);
  end
  else
  if AObject is TCarbonPen then // select pen
  begin
    Result := HPEN(ADC.CurrentPen);
    ADC.CurrentPen := TCarbonPen(GDIObj);
  end
  else
  if AObject is TCarbonFont then // select font
  begin
    Result := HFONT(ADC.CurrentFont);
    ADC.CurrentFont := TCarbonFont(GDIObj);
  end
  else
  if AObject is TCarbonBitmap then // select bitmap
  begin
    if not (ADC is TCarbonBitmapContext) then
    begin
      DebugLn(AName + ' Error - The specified device context is not bitmap context!');
      Exit;
    end;
    
    Result := HBITMAP((ADC as TCarbonBitmapContext).GetBitmap);
    (ADC as TCarbonBitmapContext).SetBitmap(TCarbonBitmap(GDIObj));
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SelectObject Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCarbonWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE;
  ForceBackground: Boolean): HPALETTE;
begin
  Result:=inherited SelectPalette(DC, Palette, ForceBackground);
end;

{------------------------------------------------------------------------------
  Method:  SendMessage
  Params:  HandleWnd - Handle of destination window
           Msg       - Message to send
           WParam    - First message parameter
           LParam    - Second message parameter
  Returns: The result of the message processing

  Sends the specified message to the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): LResult;
begin
  Result := inherited SendMessage(HandleWnd, Msg, wParam, lParam);
end;

{------------------------------------------------------------------------------
  Method:  SetActiveWindow
  Params:  Handle - Window to activate
  Returns: Previous active window

  Sets focus to the specified window.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetActiveWindow Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := 0;
  if not CheckWidget(Handle, 'SetActiveWindow') then Exit;
  Result := GetActiveWindow;
  if ActivateWindow(AsWindowRef(Handle), True) <> NoErr then Result := 0;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetBkColor DC: ' + DbgS(DC) + ' Color: ' + DbgS(Color));
  {$ENDIF}
  
  Result := CLR_INVALID;
  if not CheckDC(DC, 'SetBkColor') then Exit;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetBkMode DC: ' + DbgS(DC) + ' BkMode: ' + DbgS(BkMode));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, 'SetBkMode') then Exit;
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
  Method:  SetCursor
  Params:  ACursor - Handle of cursor to set
  Returns: Previous cursor

  Sets the cursor to application
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetCursor(ACursor: HCURSOR): HCURSOR;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetCursor ACursor: ' + DbgS(ACursor));
  {$ENDIF}
  
  Result := FCurrentCursor;
  if not CheckCursor(ACursor, 'SetCursor') then Exit;

  // If we setted cursor before, them we should uninstall it.
  // This needs for animated cursors (because of threading) and wait cursor
  if (FCurrentCursor <> 0) then
    TCarbonCursor(FCurrentCursor).UnInstall;

  // install new cursor
  TCarbonCursor(ACursor).Install;
  FCurrentCursor := ACursor;
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetFocus HWnd: ' + DbgS(HWnd));
  {$ENDIF}
  
  Result := GetFocus;
  if HWnd = 0 then Exit;
  if HWnd = Result then Exit; // if window is already focused exit
  if not CheckWidget(HWnd, 'SetFocus') then Exit;
  
  Window := TCarbonWidget(HWnd).GetTopParentWindow;
  
  SetUserFocusWindow(Window); // can change focus!
  if HWnd <> THandle(GetCarbonWindow(Window)) then
  begin
    GetKeyboardFocus(Window, Control);
    if Control <> AsControlRef(HWnd) then
      SetKeyboardFocus(Window, AsControlRef(HWnd),
        kControlFocusNextPart);
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetForegroundWindow HWnd: ' + DbgS(HWnd));
  {$ENDIF}
  
  if not CheckWidget(HWnd, 'SetForegroundWindow') then Exit;
  Result := ActivateWindow(AsWindowRef(HWnd), True) = noErr;
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
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetProp Handle: ' + DbgS(Handle) + ' Str: ' + Str +
      ' Data: ' + DbgS(Data));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(Handle, 'SetProp') then Exit;
  
  TCarbonWidget(Handle).Properties[Str] := Data;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetROP2 DC: ' + DbgS(DC) + ' Mode: ' + DbgS(Mode));
  {$ENDIF}
  
  Result := 0;
  if not CheckDC(DC, 'SetROP2') then Exit;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetScrollInfo Handle: ' + DbgS(Handle) +
      ' SBStyle: ' + DbgS(SBStyle));
  {$ENDIF}
  
  Result := 0;
  if not CheckWidget(Handle, 'SetScrollInfo') then Exit;
  
  if SBStyle = SB_CTL then
  begin
    Result := GetControl32BitValue(AsControlRef(Handle));
    if (SIF_RANGE and ScrollInfo.fMask) > 0 then
    begin
      SetControl32BitMinimum(AsControlRef(Handle), ScrollInfo.nMin);
      SetControl32BitMaximum(AsControlRef(Handle), ScrollInfo.nMax);
    end;
    if (SIF_POS and ScrollInfo.fMask) > 0 then
      SetControl32BitValue(AsControlRef(Handle), ScrollInfo.nPos);
    if (SIF_PAGE and ScrollInfo.fMask) > 0 then
      SetControlviewSize(AsControlRef(Handle), ScrollInfo.nPage);
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetTextColor DC: ' + DbgS(DC) + ' Color: ' + DbgS(Color));
  {$ENDIF}
  
  Result := CLR_INVALID;
  if not CheckDC(DC, 'SetTextColor') then Exit;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ShowWindow hWnd: ' + DbgS(hWnd) + ' nCmdShow: ' +
      DbgS(nCmdShow));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(HWnd, 'ShowWindow') then Exit;

  case nCmdShow of
  SW_SHOWNORMAL, SW_SHOWMAXIMIZED:
  begin
    if IsWindowCollapsed(AsWindowRef(HWnd)) then
      Result := CollapseWindow(AsWindowRef(HWnd), False) = noErr;

    if Result then
    begin
      if nCmdShow = SW_SHOWNORMAL then
        Result := ZoomWindowIdeal(AsWindowRef(HWnd), inZoomIn, P) = noErr
      else
      begin
        P.v := $3FFF;
        P.h := $3FFF;
        Result := ZoomWindowIdeal(AsWindowRef(HWnd), inZoomOut, P) = noErr;
      end;
    end;
  end;
  SW_MINIMIZE: Result := CollapseWindow(AsWindowRef(HWnd), True) = noErr;
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
const
  AName = 'TCarbonWidgetSet.StretchMaskBlt';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.StretchMaskBlt DestDC: ' + DbgS(DestDC) + ' SrcDC: ',
      DbgS(SrcDC) + ' X: ' +  DbgS(X) + ' Y: ' + DbgS(Y),
     ' W: ' + DbgS(Width) + ' H: ', DbgS(Height),
     ' XSrc: ' + DbgS(XSrc) + ' YSrc: ' + DbgS(YSrc),
     ' SrcW: ' + DbgS(SrcWidth), ' SrcH: ' + DbgS(SrcHeight) + ' Rop: ' + DbgS(Rop));
  {$ENDIF}
  
  Result := False;
  if not CheckDC(DestDC, AName, 'Dest') then Exit;
  if not CheckDC(SrcDC, AName, 'Src') then Exit;
  if not (TCarbonDeviceContext(SrcDC) is TCarbonBitmapContext) then
  begin
    DebugLn(AName + ' Error - invalid source device context!');
    Exit;
  end;
  
  // save dest context
  CGContextSaveGState(TCarbonDeviceContext(DestDC).CGContext);

  CGContextSetBlendMode(TCarbonDeviceContext(DestDC).CGContext, kCGBlendModeNormal);

  try
    HIViewDrawCGImage(TCarbonDeviceContext(DestDC).CGContext,
      GetCGRectSorted(X, Y, X + Width, Y + Height),
      TCarbonBitmapContext(SrcDC).GetBitmap.CGImage);
    Result := True;
   {DebugLn('StretchMaskBlt succeeds: ', Format('Dest %d Src %d X %d Y %d',
      [Integer(TCarbonDeviceContext(SrcDC).CGContext),
      Integer(TCarbonBitmapContext(SrcDC).Bitmap.CGImage),
      X, Y]));}
  finally
    CGContextRestoreGState(TCarbonDeviceContext(DestDC).CGContext);
  end;
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
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.UpdateWindow Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := False;
  if not CheckWidget(Handle, 'UpdateWindow') then Exit;
  
  TCarbonWidget(Handle).Update;
end;

{------------------------------------------------------------------------------
  Method:  WindowFromPoint
  Params:  Point - Screen point
  Returns: Carbon control or window under the specified screen point
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.WindowFromPoint(Point: TPoint): HWND;
var
  Window: WindowRef;
  Control: ControlRef;
  WindowPart: WindowPartCode;
  P: FPCMacOSAll.Point;
  R: FPCMacOSAll.Rect;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.WindowFromPoint Point: ' + DbgS(Point));
  {$ENDIF}
  
  Result := 0;
  
  P.h := Point.X;
  P.v := Point.Y;
  if FindWindowOfClass(P, kAllWindowClasses, Window, @WindowPart) <> noErr then Exit;
  if Window = nil then Exit;
  if WindowPart <> inContent then Exit;
  
  if OSError(GetWindowBounds(Window, kWindowContentRgn, R), Self,
    'WindowFromPoint', 'GetWindowBounds') then Exit;
    
  Dec(P.h, R.left);
  Dec(P.v, R.top);
  
  Control := FindControlUnderMouse(P, Window, nil);
  if Control = nil then
    Result := HWND(GetCarbonWidget(Window))
  else
    Result := HWND(GetCarbonWidget(Control));
    
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.WindowFromPoint Result: ' + DbgS(Result));
  {$ENDIF}
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line





