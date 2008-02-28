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

{------------------------------------------------------------------------------
  Method:  BitBlt
  Params:  DestDC        - Destination device context
           X, Y          - Left/top corner of the destination rectangle
           Width, Height - Size of the destination rectangle
           SrcDC         - Source device context
           XSrc, YSrc    - Left/top corner of the source rectangle
           Rop           - Raster operation to be performed
  Returns: If the function succeeds

  Copies a bitmap from a source context into a destination context using the
  specified raster operation
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc,
    Width, Height, 0, 0, 0, Rop);
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
  Result := False;
  
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

{------------------------------------------------------------------------------
  Method:  ClipboardFormatToMimeType
  Params:  FormatID - A registered format identifier (0 is invalid)
  Returns: The corresponding mime type as string
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ClipboardFormatToMimeType FormatID: ' + DbgS(FormatID));
  {$ENDIF}
  
  Result := Clipboard.FormatToMimeType(FormatID);
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetData
  Params:  ClipboardType - Clipboard type
           FormatID      - A registered format identifier (0 is invalid)
           Stream        - If format is available, it will be appended to this
                           stream
  Returns: If the function succeeds
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ClipboardGetData ClipboardType' +
      ClipboardTypeName[ClipboardType] + ' FormatID: ' + DbgS(FormatID));
  {$ENDIF}
  
  Result := Clipboard.GetData(ClipboardType, FormatID, Stream);
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetFormats
  Params:  ClipboardType - The type of clipboard operation
           Count         - The number of clipboard formats
           List          - Pointer to an array of supported formats
                           (you must free it yourself)
  Returns: If the function succeeds
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): Boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ClipboardGetFormats ClipboardType' +
      ClipboardTypeName[ClipboardType]);
  {$ENDIF}
  
  Result := Clipboard.GetFormats(ClipboardType, Count, List);
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetOwnerShip
  Params:  ClipboardType - Type of clipboard
           OnRequestProc - TClipboardRequestEvent is defined in LCLType.pp
                           If OnRequestProc is nil the onwership will end.
           FormatCount   - Number of formats
           Formats       - Array of TClipboardFormat. The supported formats the
                           owner provides.

  Returns: If the function succeeds

  Sets the supported formats and requests ownership for the clipboard.
  The OnRequestProc is used to get the data from the LCL and to put it on the
  clipboard.
  If someone else requests the ownership, the OnRequestProc will be executed
  with the invalid FormatID 0 to notify the old owner of the lost of ownership.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ClipboardGetOwnerShip ClipboardType' +
      ClipboardTypeName[ClipboardType] + ' FormatCount: ' + DbgS(FormatCount));
  {$ENDIF}
  
  Result := Clipboard.GetOwnerShip(ClipboardType, OnRequestProc, FormatCount,
    Formats);
end;

{------------------------------------------------------------------------------
  Method:  ClipboardRegisterFormat
  Params:  AMimeType - A string (usually a MIME type) identifying a new format
                       type to register
  Returns: The registered Format identifier (TClipboardFormat)
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := Clipboard.RegisterFormat(AMimeType);
end;

function TCarbonWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN;
  fnCombineMode: Longint): Longint;
begin
  DebugLn('TCarbonWidgetSet.CombineRgn TODO');
  Result:=inherited CombineRgn(Dest, Src1, Src2, fnCombineMode);
end;

{------------------------------------------------------------------------------
  Method:  ComboBoxDropDown
  Params:  Handle   - Handle to combo box
           DropDown - Show list
  Returns: If the function succeeds

  Shows or hides the combo box list
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ComboBoxDropDown Handle: ' + DbgS(Handle) + ' Drop: ' + DbgS(DropDown));
  {$ENDIF}
  
  if not CheckWidget(Handle, 'ComboBoxDropDown', TCarbonComboBox) then Exit;
  
  Result := TCarbonComboBox(Handle).DropDown(DropDown);
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
var
  bmpType: TCarbonBitmapType;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateBitmap');
  {$ENDIF}
  
  // WORKAROUND: force context supported depths
  if BitmapBits = nil then
  begin
    if BitCount = 24 then BitCount := 32;
    if BitCount = 1 then BitCount := 8;
  end;
  
  case BitCount of
    1:  bmpType := cbtMono;
    8:  bmpType := cbtGray;
    32: bmpType := cbtARGB;
  else
    bmpType := cbtRGB;
  end;
  
  // winapi Bitmaps are on a word boundary
  Result := HBITMAP(TCarbonBitmap.Create(Width, Height, BitCount, BitCount, cbaWord, bmpType, BitmapBits));
end;


{------------------------------------------------------------------------------
  Method:   CreateBrushIndirect
  Params:   LogBrush - Record with brush characteristics
  Returns:  Handle to a logical brush

  Creates new logical brush that has the specified style, color, and pattern
  TODO: patterns
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateBrushIndirect');
  {$ENDIF}
  
  Result := HBRUSH(TCarbonBrush.Create(LogBrush));
end;

{------------------------------------------------------------------------------
  Method:  CreateCaret
  Params:  Handle - handle to owner window
           Bitmap - handle to bitmap for caret shape
           Width  - caret width
           Height - caret height
  Returns: If the function succeeded

  Creates a new shape for the system caret and assigns ownership of the caret
  to the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap; Width, Height: Integer): Boolean;
begin
  Result := True;
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateCaret Handle: ' + DbgS(Handle) + ' Bitmap: ' + DbgS(Bitmap),
      ' W: ' + DbgS(Width), ' H: ' + DbgS(Height));
  {$ENDIF}
  if not CheckWidget(Handle, 'CreateCaret') then Exit;
  if Bitmap > 1 then
    if not CheckBitmap(Bitmap, 'CreateCaret') then Exit;
  
  Result := CarbonCaret.CreateCaret(TCarbonWidget(Handle), Bitmap, Width, Height);
end;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleBitmap
  Params:  DC     - Handle to memory device context
           Width  - Bitmap width
           Height - Bitmap height

  Returns: Handle to a bitamp

  Creates a bitamp compatible with the specified device
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer
  ): HBITMAP;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateCompatibleBitmap');
  {$ENDIF}
  
  // TODO: consider DC depth
  Result := HBITMAP(TCarbonBitmap.Create(Width, Height, 32, 32, cbaDQWord, cbtARGB, nil));
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
  
  // TODO: consider DC depth
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
  Params:  LogFont - Font characteristics
  Returns: Handle to the font

  Creates new font with specified characteristics
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
  Params:  LogFont      - Font characteristics
           LongFontName - Font name
  Returns: Handle to the font

  Creates new font with specified characteristics and name
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
  Params:  LogPen - Record with pen characteristics
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

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points   - Pointer to array of polygon points
           NumPts   - Number of points passed
           FillMode - Filling mode
  Returns: The new polygonal region

  Creates a new polygonal region from the specified points
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreatePolygonRgn NumPts: ' + DbgS(NumPts) +
      ' FillMode: ' + DbgS(FillMode));
  {$ENDIF}
  
  Result := HRGN(TCarbonRegion.Create(Points, NumPts, FillMode));
end;

{------------------------------------------------------------------------------
  Method:  CreateRectRgn
  Params:  X1, Y1, X2, Y2 - Region bounding rectangle
  Returns: The new rectangular region

  Creates a new rectangular region
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.CreateRectRgn R: ' + DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}
  
  Result := HRGN(TCarbonRegion.Create(X1, Y1, X2, Y2));
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DeleteDC DC: ' + DbgS(hDC));
  {$ENDIF}
  
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
var
  CarbonGDIObject: TCarbonGDIObject;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DeleteObject GDIObject: ' + DbgS(GDIObject));
  {$ENDIF}
  
  if not CheckGDIObject(GDIObject, 'DeleteObject') then Exit;
  CarbonGDIObject := TCarbonGDIObject(GDIObject);
  
  if CarbonGDIObject.Global then
  begin
    DebugLn('TCarbonWidgetSet.DeleteObject Error - GDIObject: ' +
      DbgSName(CarbonGDIObject) + ' is global!');
    Exit;
  end;
  
  if CarbonGDIObject.SelCount = 0 then CarbonGDIObject.Free
  else
  begin
    DebugLn('TCarbonWidgetSet.DeleteObject Error - GDIObject: ' +
      DbgSName(CarbonGDIObject) + ' is still selected!');
    Exit;
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  DestroyCaret
  Params:  Handle - handle to the window with a caret (IGNORED)
  Returns: If the function succeeds

  Destroys the caret but doesn't free the bitmap.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DestroyCaret Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := CarbonCaret.DestroyCaret;
end;

{------------------------------------------------------------------------------
  Method:  DestroyCursor
  Params:  Handle - Handle to cursor
  Returns: If the function succeeds

  Destroy previously created cursor
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DestroyCursor(Handle: HCURSOR): Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DestroyCursor Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  if not CheckCursor(Handle, 'DestroyCursor') then Exit;
  TCarbonCursor(Handle).Free;
end;

{------------------------------------------------------------------------------
  Method:  DrawFocusRect
  Params:  DC   - Handle to device context
           Rect - Bounding rectangle
  Returns: If the function succeeds

  Draws a focus rectangle
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.DrawFocusRect DC: ' + DbgS(DC) + ' Rect: ' + DbgS(Rect));
  {$ENDIF}

  if not CheckDC(DC, 'DrawFocusRect') then Exit;

  TCarbonDeviceContext(DC).DrawFocusRect(Rect);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  DrawFrameControl
  Params:  DC     - Handle to device context
           Rect   - Bounding rectangle
           UType  - Frame-control type
           UState - Frame-control state
  Returns: If the function succeeds

  Draws a frame control of the specified type and style.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.DrawFrameControl(DC: HDC; const Rect: TRect; UType,
  UState: Cardinal): Boolean;
begin
  Result := inherited DrawFrameControl(DC, Rect, UType, UState);
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Ellipse DC: ' + DbgS(DC) + ' R: ' + DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}
  
  if not CheckDC(DC, 'Ellipse') then Exit;

  TCarbonDeviceContext(DC).Ellipse(X1, Y1, X2, Y2);

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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.EnableWindow Handle: ' + DbgS(hWnd) + 'Enable: ' + DbgS(bEnable));
  {$ENDIF}
  
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
           lpLogFont - Font characteristics to match
           Callback  - Callback function
           LParam    - Parameter to pass to the callback function.
           flags     - Not used
  Returns: The last value returned by callback function

  Enumerates all the font families in the system that match specified
  characteristics
  TODO: specific face or specific char set enumeration
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; LParam: Lparam; flags: dword): Longint;
var
  FamilyCount: LongWord;
  FamilyListPtr, PFamily: ^ATSUFontID;
  FontName: UTF8String;
  EnumLogFont: TEnumLogFontEx;
  Metric: TNewTextMetricEx;
  FontType, I: Integer;
const
  SName = 'TCarbonWidgetSet.EnumFontFamiliesEx';
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.EnumFontFamiliesEx');
  {$ENDIF}
  
  if (lpLogFont = nil) or not Assigned(Callback) then Exit;
  
  // enumarate ATSUI font families:
  
  if (lpLogFont^.lfCharSet = DEFAULT_CHARSET) and (lpLogFont^.lfFaceName = '') then
  begin
    // all system fonts
    if OSError(ATSUFontCount(FamilyCount), SName, 'ATSUFontCount') then Exit;
    
    GetMem(FamilyListPtr, SizeOf(ATSUFontID) * FamilyCount);
    try
      if OSError(ATSUGetFontIDs(FamilyListPtr, FamilyCount, nil), SName, 'ATSUGetFontIDs') then Exit;

      {$IFDEF VerboseWinAPI}
        DebugLn(SName + ' Found: ' + DbgS(FamilyCount));
      {$ENDIF}

      PFamily := FamilyListPtr;
      for I := 0 to Pred(FamilyCount) do
      begin
        FontName := CarbonFontIDToFontName(PFamily^);
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
    DebugLn(SName + ' with specific face or char set is not implemented!');
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
begin
  Result := ERROR;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ExcludeClipRect DC: ' + DbgS(DC) + ' R: ' +
      DbgS(Classes.Rect(Left, Top, Right, Bottom)));
  {$ENDIF}
  
  if not CheckDC(DC, 'ExcludeClipRect') then Exit;
  
  TCarbonDeviceContext(DC).ExcludeClipRect(Left, Top, Right, Bottom);
  
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ExtTextOut DC: ' + DbgS(DC) + ' ' + DbgS(X) +
      ', ' + DbgS(Y) + ' Str: ' + Str + ' Count: ' + DbgS(Count));
  {$ENDIF}
  
  if not CheckDC(DC, 'ExtTextOut') then Exit;
  
  Result :=
    TCarbonDeviceContext(DC).ExtTextOut(X, Y, Options, Rect, Str, Count, Dx);
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ExtTextOut Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCarbonWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint
  ): Integer;
begin
  DebugLn('TCarbonWidgetSet.ExtSelectClipRGN TODO');
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.FillRect DC: ' + DbgS(DC) + ' R: ' + DbgS(Rect) +
      ' Brush: ' + DbgS(Brush));
  {$ENDIF}
  
  if not CheckDC(DC, 'FillRect') then Exit;
  if not (TObject(Brush) is TCarbonBrush) then
  begin
    DebugLn('TCarbonWidgetSet.FillRect Error - invalid Brush!');
    Exit;
  end;

  TCarbonDeviceContext(DC).FillRect(Rect, TCarbonBrush(Brush));

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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Frame DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect));
  {$ENDIF}
  
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
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.Frame3D(DC: HDC; var ARect: TRect;
  const FrameWidth: integer; const Style: TBevelCut): Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Frame3D DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect) +
      ' Width: ' + DbgS(FrameWidth) + ' Style: ' + DbgS(Integer(Style)));
  {$ENDIF}
  
  if not CheckDC(DC, 'Frame3D') then Exit;
  
  if FrameWidth <= 0 then Exit;

  TCarbonDeviceContext(DC).Frame3D(ARect, FrameWidth, Style);
  
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
  Result := 0;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.FrameRect DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect) +
      ' Brush: ' + DbgS(hBr));
  {$ENDIF}
  
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
  Method:  GetCapture
  Returns: The handle of the capture window

  Retrieves the handle of the window (if any) that has captured the mouse
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetCapture: HWND;
begin
  Result := FCaptureWidget;
end;

{------------------------------------------------------------------------------
  Method:  GetCaretPos
  Params:  LPPoint - record to receive coordinates
  Returns: If the function succeeds

  Gets the caret's position, in client coordinates.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := CarbonCaret.GetCaretPos(lpPoint);
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetCaretPos Point: ' + DbgS(lpPoint), ' Result: ' + DbgS(Result));
  {$ENDIF}
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
  Result := False;
  
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
  Result := False;
  
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
  Result := ERROR;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetClipBox DC: ' + DbgS(DC));
  {$ENDIF}
  
  if not CheckDC(DC, 'GetClipBox') then Exit;
  
  if lpRect <> nil then
    lpRect^ := TCarbonDeviceContext(DC).GetClipRect;

  Result := COMPLEXREGION;
  
  {$IFDEF VerboseWinAPI}
    if lpRect <> nil then
      DebugLn('TCarbonWidgetSet.GetClipBox Rect: ' + DbgS(lpRect^));
  {$ENDIF}
end;

function TCarbonWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
begin
  DebugLn('TCarbonWidgetSet.GetClipRGN TODO');
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
  Result := False;
  
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDC HWnd: ' + DbgS(HWnd));
  {$ENDIF}

  if HWnd = 0 then Result := HDC(ScreenContext)
  else
  begin
    if not CheckWidget(HWnd, 'GetDC') then Exit;
    
    // use default context if we are outside paint event
    if TCarbonWidget(HWnd).Context <> nil then Result := HDC(TCarbonWidget(HWnd).Context)
    else
      Result := HDC(DefaultContext);
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

{------------------------------------------------------------------------------
  Method: GetDeviceCaps
  Params: DC    - Display device context
          Index - Index of needed capability

  Returns device specific information
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceCaps DC: ' + DbgS(DC) + ' Index: ' + DbgS(Index));
  {$ENDIF}
  
  if not CheckDC(DC, 'GetDeviceCaps') then Exit;

  case Index of
  LOGPIXELSX,
  LOGPIXELSY:
    // logical is allways 72 dpi, although physical can differ
    Result := 72; // TODO: test scaling and magnification
  BITSPIXEL:  Result := CGDisplayBitsPerPixel(CGMainDisplayID);
  else
    DebugLn('TCarbonWidgetSet.GetDeviceCaps TODO Index: ' + DbgS(Index));
  end;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceCaps Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  GetDeviceSize
  Params:  DC - Handle to device context
           P  - Record point for result
  Returns: If the function succeeds

  Retrieves the size of the specified device context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetDeviceSize DC: ' + DbgS(DC));
  {$ENDIF}
  
  if not CheckDC(DC, 'GetDeviceSize') then Exit;
  
  P := TCarbonDeviceContext(DC).Size;
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetFocus');
  {$ENDIF}
  
  Window := WindowRef(GetUserFocusWindow);
  if Window = nil then Exit;
  
  Control := nil;
  OSError(GetKeyboardFocus(Window, Control), Self, 'GetFocus', SGetKeyboardFocus);
  
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
const
  StateDown    = SmallInt($FF80);
  StateToggled = SmallInt($0001);
begin
  Result := 0;

  {$IFDEF VerboseKeyboard}
    DebugLn('TCarbonWidgetSet.GetKeyState ' + DbgSVKCode(nVirtKey));
  {$ENDIF}

  case nVirtKey of
  VK_MENU:
    if (GetCurrentEventKeyModifiers and optionKey) > 0 then
      // the ssAlt/VK_MENU is mapped to optionKey under MacOS
      Result := StateDown;
  VK_SHIFT:
    if (GetCurrentEventKeyModifiers and shiftKey) > 0 then
      Result := StateDown;
  VK_CONTROL:
    if (GetCurrentEventKeyModifiers and cmdKey) > 0 then
      // the ssCtrl/VK_CONTROL is mapped to cmdKey under MacOS
      Result := StateDown;
  VK_LBUTTON:
    if (GetCurrentEventButtonState and $01) > 0 then Result := StateDown;
  VK_RBUTTON:
    if (GetCurrentEventButtonState and $02) > 0 then Result := StateDown;
  VK_MBUTTON:
    if (GetCurrentEventButtonState and $04) > 0 then Result := StateDown;
  VK_XBUTTON1:
    if (GetCurrentEventButtonState and $08) > 0 then Result := StateDown;
  VK_XBUTTON2:
    if (GetCurrentEventButtonState and $10) > 0 then Result := StateDown;
  else
    DebugLn('TCarbonWidgetSet.GetKeyState TODO ', DbgSVKCode(Word(nVirtkey)));
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetObject GDIObj: ' + DbgS(GDIObj));
  {$ENDIF}
  
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetParent Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  if not CheckWidget(Handle, 'GetParent') then Exit;
  
  if TCarbonWidget(Handle) is TCarbonControl then
     Result :=
       HWnd(GetCarbonWidget(HIViewGetSuperview(TCarbonControl(Handle).Widget)));
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
  Result := nil;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetProp Handle: ' + DbgS(Handle) + ' Str: ' + Str);
  {$ENDIF}
  
  if not CheckWidget(Handle, 'GetProp') then Exit;

  Result := TCarbonWidget(Handle).Properties[Str];
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetProp Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  GetRgnBox
  Params:  RGN    - Handle to region
           lpRect - Pointer to rectangle
  Returns: See bellow

  Retrieves the specified region bounding rectangle. The result can be one of
  the following constants: ERROR, NULLREGION, SIMPLEREGION, COMPLEXREGION.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetRgnBox(RGN: HRGN; lpRect: PRect): Longint;
begin
  Result := ERROR;
  if lpRect <> nil then lpRect^ := Classes.Rect(0, 0, 0, 0);
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetRgnBox RGN: ' + DbgS(RGN));
  {$ENDIF}
  
  if not (TObject(RGN) is TCarbonRegion) then
  begin
    DebugLn('TCarbonWidgetSet.GetRgnBox Error - invalid region ', DbgS(RGN), '!');
    Exit;
  end;
  
  if lpRect <> nil then
  begin
    lpRect^ := TCarbonRegion(RGN).GetBounds;
    Result := TCarbonRegion(RGN).GetType;
  end;
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

{------------------------------------------------------------------------------
  Method:  GetScrollInfo
  Params:  Handle     - Handle of window
           SBStyle    - Scroll bar flag
           ScrollInfo - Record fo scrolling info
  Returns: If the function succeeds

  Gets the parameters of a scroll bar
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetScrollInfo(Handle: HWND; SBStyle: Integer;
  var ScrollInfo: TScrollInfo): Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetScrollInfo Handle: ' + DbgS(Handle) +
      ' SBStyle: ' + DbgS(SBStyle));
  {$ENDIF}

  if not CheckWidget(Handle, 'GetScrollInfo') then Exit;

  TCarbonWidget(Handle).GetScrollInfo(SBStyle, ScrollInfo);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  GetStockObject
  Params:  Value - Type of stock object
  Returns: A value identifying the logical object requested

  Retrieves a handle to one of the predefined stock objects
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetStockObject(Value: Integer): LongInt;
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetStockObject Value: ' + DbgS(Value));
  {$ENDIF}

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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetSysColor NIndex: ' + DbgS(NIndex));
  {$ENDIF}
  
  R := not noErr;
  
  Depth := CGDisplayBitsPerPixel(CGMainDisplayID);
  case NIndex of
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
  COLOR_BACKGROUND, COLOR_INFOBK,
  COLOR_WINDOW, COLOR_FORM:
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetSystemMetrics NIndex: ' + DbgS(NIndex));
  {$ENDIF}

  case NIndex of
    SM_CYHSCROLL, SM_CXVSCROLL:
      Result := GetCarbonThemeMetric(kThemeMetricScrollBarWidth);
    SM_CXSCREEN,
    SM_CXVIRTUALSCREEN : Result := CGDisplayPixelsWide(CGMainDisplayID);
    SM_CYSCREEN,
    SM_CYVIRTUALSCREEN : Result := CGDisplayPixelsHigh(CGMainDisplayID);
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

  Computes the width and height of the specified string of text
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetTextExtentPoint DC: ' + DbgS(DC) + ' Str: ' + Str);
  {$ENDIF}
  
  if not CheckDC(DC, 'GetTextExtentPoint') then Exit;

  Result := TCarbonDeviceContext(DC).GetTextExtentPoint(Str, Count, Size);
  
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetTextMetrics DC: ' + DbgS(DC));
  {$ENDIF}
  
  if not CheckDC(DC, 'GetTextMetrics') then Exit;

  Result := TCarbonDeviceContext(DC).GetTextMetrics(TM);
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetTextMetrics Result: ' + DbgS(Result) +
      ' TextMetric: ' + DbgS(TM));
  {$ENDIF}
end;

function TCarbonWidgetSet.GetWindowLong(Handle: hwnd; int: Integer): PtrInt;
begin
  Result:=inherited GetWindowLong(Handle, int);
end;

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC - Handle of device context
           P   - Record for context origin
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the origin of the specified context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  Transform: CGAffineTransform;
  SizeP: TPoint;
begin
  Result := 0;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowOrgEx DC: ' + DbgS(DC));
  {$ENDIF}
  
  if not CheckDC(DC, 'GetWindowOrgEx') then Exit;
  
  Transform := CGContextGetCTM(TCarbonContext(DC).CGContext);
  GetDeviceSize(DC, SizeP);
  P^.X := Round(Transform.tx);
  P^.Y := SizeP.Y - Round(Transform.ty);
  Result := 1;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowOrgEx ' + DbgS(P^.X) + ', ' + DbgS(P^.Y));
  {$ENDIF}
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowRect Handle: ' + DbgS(Handle));
  {$ENDIF}
  
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowRelativePosition Handle: ' + DbgS(Handle));
  {$ENDIF}
  
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.GetWindowSize Handle: ' + DbgS(Handle));
  {$ENDIF}
  
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

{------------------------------------------------------------------------------
  Method:  HideCaret
  Params:  HWnd - handle to the window with the caret
  Returns: Whether the window owns the caret

  Removes the caret from the screen.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  Result := False;
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.HideCaret Handle: ' + DbgS(hWnd));
  {$ENDIF}
  if not CheckWidget(hWnd, 'HideCaret') then Exit;

  Result := CarbonCaret.HideCaret(TCarbonWidget(hWnd));
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
  Result := ERROR;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IntersectClipRect DC: ' + DbgS(DC) + ' R: ' +
      DbgS(Classes.Rect(Left, Top, Right, Bottom)));
  {$ENDIF}
  
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.InvalidateRect Handle: ' + DbgS(AHandle));
  {$ENDIF}
  
  if not CheckWidget(AHandle, 'InvalidateRect') then Exit;
  
  TCarbonWidget(AHandle).Invalidate(Rect);
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  InvalidateRgn
  Params:  Handle - handle of window with changed update region
           Rgn    - handle to region to invalidate
           Erase  - specifies whether the background is to be erased
  Returns: if the function succeeds

  Adds a region to the specified window's update region.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.InvalidateRgn Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  if not CheckWidget(Handle, 'InvalidateRgn') then Exit;
  if (Rgn <> 0) and not (TObject(Rgn) is TCarbonRegion) then
  begin
    DebugLn('TCarbonWidgetSet.InvalidateRgn Error - invalid region: ', DbgS(Rgn), '!');
    Exit;
  end;

  if Rgn = 0 then
    TCarbonWidget(Handle).Invalidate(nil)
  else
    TCarbonWidget(Handle).InvalidateRgn(TCarbonRegion(Rgn).Shape);
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  IsWindowEnabled
  Params:  Handle - Handle of window
  Returns: True if window is enabled, false otherwise
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IsWindowEnabled Handle: ' + DbgS(Handle));
  {$ENDIF}
  
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.IsWindowVisible Handle: ' + DbgS(Handle));
  {$ENDIF}
  
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.LineTo DC: ' + DbgS(DC) + ' ' + DbgS(X) + ', ' +
      DbgS(Y));
  {$ENDIF}
  
  if not CheckDC(DC, 'LineTo') then Exit;

  TCarbonDeviceContext(DC).LineTo(X, Y);

  Result := True;
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
function TCarbonWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  ADC: TCarbonDeviceContext;
  TempPenPos: TPoint;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.MoveToEx DC: ' + DbgS(DC) + ' ' + DbgS(X) + ', ' +
      DbgS(Y));
  {$ENDIF}
  
  if not CheckDC(DC, 'MoveToEx') then Exit;

  ADC := TCarbonDeviceContext(DC);

  if OldPoint <> nil then OldPoint^ := ADC.PenPos;

  { We need a temporary variable or this wont compile with 2.3.x }
  TempPenPos.X := X;
  TempPenPos.Y := Y;
  ADC.PenPos := TempPenPos;
  
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.MoveWindowOrgEx DC: ' + DbgS(DC) + ' ' + DbgS(DX)
      + ', ' + DbgS(DY));
  {$ENDIF}
  
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.PolyBezier DC: ' + DbgS(DC) + ' NumPts: ' +
      DbgS(NumPts) + ' Filled: ' + DbgS(Filled) + ' Continuous: ' +
      DbgS(Continuous));
  {$ENDIF}
  
  if not CheckDC(DC, 'PolyBezier') then Exit;
  if Points = nil then Exit;
  if NumPts < 4 then Exit;

  TCarbonDeviceContext(DC).PolyBezier(Points, NumPts, Filled, Continuous);

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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Polygon DC: ' + DbgS(DC) + ' NumPts: ' +
      DbgS(NumPts) + ' Winding: ' + DbgS(Winding));
  {$ENDIF}
  
  if not CheckDC(DC, 'Polygon') then Exit;
  if Points = nil then Exit;
  if NumPts < 2 then Exit;

  TCarbonDeviceContext(DC).Polygon(Points, NumPts, Winding);
    
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Polyline DC: ' + DbgS(DC) + ' NumPts: ' + DbgS(NumPts));
  {$ENDIF}
  
  if not CheckDC(DC, 'Polyline') then Exit;
  if Points = nil then Exit;

  TCarbonDeviceContext(DC).Polyline(Points, NumPts);

  Result := True;
end;

function TCarbonWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): Boolean;
var
  UserEvent: EventRef;
  EventTarget: EventTargetRef;
begin
  Result := False;
  if not CheckWidget(Handle, 'PostMessage') then Exit;

  UserEvent := nil;
  try
    UserEvent := PrepareUserEvent(Handle, Msg, wParam, lParam, EventTarget);
    if UserEvent = nil then
      Exit;

    SetEventParameter(UserEvent, kEventParamPostTarget, typeEventTargetRef,
      SizeOf(EventTarget), @EventTarget);
      
    if PostEventToQueue(FMainEventQueue, UserEvent, kEventPriorityStandard) <> noErr then
      Exit;
  finally
    if UserEvent <> nil then 
      ReleaseEvent(UserEvent);
  end;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  PtInRegion
  Params:  RNG  - Handle to region
           X, Y - Point
  Returns: If the specified point lies in the region
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.PtInRegion(RGN: HRGN; X, Y: Integer): Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.PtInRegion RGN: ' + DbgS(RGN), ' X: ', DbgS(X),
      ' Y: ', DbgS(Y));
  {$ENDIF}

  if not (TObject(RGN) is TCarbonRegion) then
  begin
    DebugLn('TCarbonWidgetSet.PtInRegion Error - invalid region ', DbgS(RGN), '!');
    Exit;
  end;

  Result := TCarbonRegion(RGN).ContainsPoint(Classes.Point(X, Y));
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.PtInRegion Result: ' + DbgS(Result));
  {$ENDIF}
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
begin
  Result := False;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.Rectangle DC: ' + DbgS(DC) + ' R: ' +
      DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}
  
  if not CheckDC(DC, 'Rectangle') then Exit;

  TCarbonDeviceContext(DC).Rectangle(X1, Y1, X2, Y2);

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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RectVisible DC: ' + DbgS(DC) + ' R: ' + DbgS(ARect));
  {$ENDIF}
  
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

{------------------------------------------------------------------------------
  Method:  ReleaseCapture
  Returns: If the function succeeds

  Releases the mouse capture from a window and restores normal mouse input
  processing
  TODO: not only release capture indicator
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ReleaseCapture: Boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ReleaseCapture');
  {$ENDIF}
  
  SetCaptureWidget(0);
  
  Result := True;
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

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ReleaseDC HWnd: ' + DbgS(HWnd) + ' DC: ' + DbgS(DC));
  {$ENDIF}
  
  if not CheckDC(DC, 'ReleaseDC') then Exit;
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RemoveProp Handle: ' + DbgS(Handle) + ' Str: ' +
      DbgS(Str));
  {$ENDIF}
  
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.RestoreDC DC: ' + DbgS(DC) + ' SavedDC: ' +
      DbgS(SavedDC));
  {$ENDIF}
  
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SaveDC DC: ' + DbgS(DC));
  {$ENDIF}
  
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
const
  SName = 'TCarbonWidgetSet.SelectObject';
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SelectObject DC: ' + DbgS(DC) + ' GDIObj: ' +
      DbgS(GDIObj));
  {$ENDIF}
  
  if not CheckDC(DC, SName) then Exit;
  if not CheckGDIObject(GDIObj, SName) then Exit;
  
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
  if AObject is TCarbonRegion then // select region
  begin
    Result := HBRUSH(ADC.CurrentRegion);
    ADC.CurrentRegion := TCarbonRegion(GDIObj);
  end
  else
  if AObject is TCarbonBitmap then // select bitmap
  begin
    if not (ADC is TCarbonBitmapContext) then
    begin
      DebugLn(SName + ' Error - The specified device context is not bitmap context!');
      Exit;
    end;
    
    Result := HBITMAP(TCarbonBitmapContext(ADC).Bitmap);
    TCarbonBitmapContext(ADC).Bitmap := TCarbonBitmap(GDIObj);
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
var
  UserEvent: EventRef;
  EventTarget: EventTargetRef;
  AMessage: TLMessage;
begin
  Result := 0;
  if not CheckWidget(HandleWnd, 'SendMessage') then Exit;

  UserEvent := nil;
  try
    UserEvent := PrepareUserEvent(HandleWnd, Msg, wParam, lParam, EventTarget);
    if UserEvent = nil then
      Exit;

    SendEventToEventTarget(UserEvent, EventTarget);
    if GetEventParameter(UserEvent, MakeFourCC('wmsg'), MakeFourCC('wmsg'), nil,
       SizeOf(TLMessage), nil, @AMessage) = noErr then
      Result := AMessage.Result;
  finally
    if UserEvent <> nil then
      ReleaseEvent(UserEvent);
  end;
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

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetActiveWindow Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  if not CheckWidget(Handle, 'SetActiveWindow', TCarbonWindow) then Exit;
  
  Result := GetActiveWindow;
  if not TCarbonWindow(Handle).Activate then Result := 0;
end;

{------------------------------------------------------------------------------
  Method:  SetBkColor
  Params:  DC    - Handle to device context
           Color - Background color value
  Returns: The previous background color if succeeds, otherwise CLR_INVALID

  Sets the current background color to the specified color value
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetBkColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := CLR_INVALID;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetBkColor DC: ' + DbgS(DC) + ' Color: ' + DbgS(Color));
  {$ENDIF}
  
  if not CheckDC(DC, 'SetBkColor') then Exit;

  Result := TCarbonDeviceContext(DC).BkColor;
  TCarbonDeviceContext(DC).BkColor := Color;
end;

{------------------------------------------------------------------------------
  Method:  SetBkMode
  Params:  DC     - Handle to device context
           BkMode - Flag specifying background mode
  Returns: The previous background mode if suceeds, otherwise 0

  Sets the background mix mode of the specified device context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetBkMode(DC: HDC; BkMode: Integer): Integer;
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetBkMode DC: ' + DbgS(DC) + ' BkMode: ' + DbgS(BkMode));
  {$ENDIF}
  
  if not CheckDC(DC, 'SetBkMode') then Exit;

  Result := TCarbonDeviceContext(DC).BkMode;
  TCarbonDeviceContext(DC).BkMode := BkMode;
end;

function TCarbonWidgetSet.SetCapture(AHandle: HWND): HWND;
begin
  Result:=inherited SetCapture(AHandle);
end;

{------------------------------------------------------------------------------
  Method:  SetCaretPos
  Params:  X, Y - Caret pos
  Returns: If the function succeeds

  Moves the caret to the specified coordinates.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetCaretPos X: ' + DbgS(X) + ' Y: ' + DbgS(Y));
  {$ENDIF}
  
  Result := CarbonCaret.SetCaretPos(X, Y);
end;

{------------------------------------------------------------------------------
  Method:  SetCaretPosEx
  Params:  Handle - handle of window
           X      - Horizontal caret coordinate
           Y      - Vertical caret coordinate
  Returns: If the function succeeds

  Moves the caret to the specified coordinates in the specified window.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetCaretPosEx X: ' + DbgS(X) + ' Y: ' + DbgS(Y));
  {$ENDIF}
  
  Result := CarbonCaret.SetCaretPos(X, Y);
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
  if FCurrentCursor = ACursor then Exit;

  // If we setted cursor before, them we should uninstall it.
  // This needs for animated cursors (because of threading) and wait cursor
  if (FCurrentCursor <> 0) then
    TCarbonCursor(FCurrentCursor).UnInstall;

  // install new cursor
  TCarbonCursor(ACursor).Install;
  FCurrentCursor := ACursor;
end;

{------------------------------------------------------------------------------
  Method:  SetCursorPos
  Params:  X - global screen horizontal coordinate
           Y - global screen vertical coordinate
  Returns: If the function succeeds.

  Sets the position of the cursor on the screen.
  NOTE: does not generate events!
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
var
	CursorPos: CGPoint;
begin
  Result := False;
    
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetCursorPos X: ' + DbgS(X) + ' Y: ' + DbgS(Y));
  {$ENDIF}

  CursorPos.X := X;
	CursorPos.Y := Y;
  if OSError(CGWarpMouseCursorPosition(CursorPos), Self, 'SetCursorPos',
    'CGWarpMouseCursorPosition') then Exit;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  SetFocus
  Params:  HWnd - Handle of new focus window
  Returns: Previous focused window

  Sets the keyboard focus to the specified window
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetFocus(HWnd: HWND): HWND;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetFocus HWnd: ' + DbgS(HWnd));
  {$ENDIF}
  
  Result := GetFocus;
  if HWnd = 0 then Exit;
  if HWnd = Result then Exit; // if window is already focused exit
  if not CheckWidget(HWnd, SSetFocus) then Exit;
  
  TCarbonWidget(HWnd).SetFocus;
end;

{------------------------------------------------------------------------------
  Method:  SetForegroundWindow
  Params:  HWnd - Handle of window
  Returns: If the function suceeds

  Brings the specified window to the top
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetForegroundWindow HWnd: ' + DbgS(HWnd));
  {$ENDIF}

  if not CheckWidget(HWnd, 'SetForegroundWindow', TCarbonWindow) then Exit;
  
  Result := TCarbonWindow(HWnd).SetForeground;
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
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetProp Handle: ' + DbgS(Handle) + ' Str: ' + Str +
      ' Data: ' + DbgS(Data));
  {$ENDIF}
  
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
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetROP2 DC: ' + DbgS(DC) + ' Mode: ' + DbgS(Mode));
  {$ENDIF}
  
  if not CheckDC(DC, 'SetROP2') then Exit;

  Result := TCarbonDeviceContext(DC).ROP2;
  TCarbonDeviceContext(DC).ROP2 := Mode;
end;

{------------------------------------------------------------------------------
  Method:  SetScrollInfo
  Params:  Handle     - Handle of window
           SBStyle    - Scroll bar flag
           ScrollInfo - Scrolling info
           bRedraw    - Redraw the scroll bar?
  Returns: The old position value

  Sets the parameters of a scroll bar
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer;
  ScrollInfo: TScrollInfo; bRedraw: Boolean): Integer;
var
  CarbonControl: TCarbonControl;
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetScrollInfo Handle: ' + DbgS(Handle) +
      ' SBStyle: ' + DbgS(SBStyle));
  {$ENDIF}
  
  if SBStyle = SB_CTL then
  begin
    if not CheckWidget(Handle, 'SetScrollInfo', TCarbonControl) then Exit;

    CarbonControl := TCarbonControl(Handle);
    Result := CarbonControl.GetValue;
    
    if (SIF_RANGE and ScrollInfo.fMask) > 0 then
    begin
      CarbonControl.SetMinimum(ScrollInfo.nMin);
      CarbonControl.SetMaximum(ScrollInfo.nMax);
    end;
    if (SIF_POS and ScrollInfo.fMask) > 0 then
      CarbonControl.SetValue(ScrollInfo.nPos);
    if (SIF_PAGE and ScrollInfo.fMask) > 0 then
      CarbonControl.SetViewSize(ScrollInfo.nPage);
  end
  else
  begin
    if not CheckWidget(Handle, 'SetScrollInfo') then Exit;

    Result := TCarbonWidget(Handle).SetScrollInfo(SBStyle, ScrollInfo);
  end;
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
begin
  Result := CLR_INVALID;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetTextColor DC: ' + DbgS(DC) + ' Color: ' + DbgS(Color));
  {$ENDIF}
  
  if not CheckDC(DC, 'SetTextColor') then Exit;
  
  Result := TCarbonDeviceContext(DC).TextColor;
  TCarbonDeviceContext(DC).TextColor := Color;
end;

function TCarbonWidgetSet.SetWindowLong(Handle: HWND; Idx: Integer;
  NewLong: PtrInt): PtrInt;
begin
  Result:=inherited SetWindowLong(Handle, Idx, NewLong);
end;

{------------------------------------------------------------------------------
  Method:  SetWindowOrgEx
  Params:  DC    - Handle to device context.
           NewX, NewY - New context origin
  Returns: If the function succeeds

  Sets the origin of the specified device context
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
var
  OldP: TPoint;
begin
  Result  := False;
  
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.SetWindowOrgEx DC: ' + DbgS(DC) + ' X: ' + DbgS(NewX) +
	  ' Y: ' + DbgS(NewY));
  {$ENDIF}
  
  GetWindowOrgEx(DC, @OldP);
  
  Result := MoveWindowOrgEx(DC, NewX - OldP.X, NewY - OldP.Y);
  if OldPoint <> nil then
    OldPoint := @OldP;
end;

function TCarbonWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y,
  cx, cy: Integer; uFlags: UINT): Boolean;
begin
  Result:=inherited SetWindowPos(hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
end;

{------------------------------------------------------------------------------
  Method:  ShowCaret
  Params:  HWnd - Handle of window with caret
  Returns: if the function succeeds

  Makes the caret visible on the screen at the caret's current position.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result := False;
  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ShowCaret Handle: ' + DbgS(hWnd));
  {$ENDIF}
  if not CheckWidget(hWnd, 'ShowCaret') then Exit;

  Result := CarbonCaret.ShowCaret(TCarbonWidget(hWnd));
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
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.ShowWindow hWnd: ' + DbgS(hWnd) + ' nCmdShow: ' +
      DbgS(nCmdShow));
  {$ENDIF}
  
  if not CheckWidget(HWnd, 'ShowWindow', TCarbonWindow) then Exit;

  Result := TCarbonWindow(HWnd).Show(nCmdShow);
end;

{------------------------------------------------------------------------------
  Method:  StretchBlt
  Params:  DestDC              - Destination device context
           X, Y                - Left/top corner of the destination rectangle
           Width, Height       - Size of the destination rectangle
           SrcDC               - Source device context
           XSrc, YSrc          - Left/top corner of the source rectangle
           SrcWidth, SrcHeight - Size of the source rectangle
           Rop                 - Raster operation to be performed
  Returns: If the function succeeds

  Copies a bitmap from a source rectangle into a destination rectangle using the
  specified raster operation. If needed it resizes the bitmap to fit the
  dimensions of the destination rectangle. Sizing is done according to the
  stretching mode currently set in the destination device context.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean;
begin
  Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc,
    SrcWidth, SrcHeight, 0, 0, 0, Rop);
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
  SName = 'TCarbonWidgetSet.StretchMaskBlt';
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.StretchMaskBlt DestDC: ' + DbgS(DestDC) + ' SrcDC: ',
      DbgS(SrcDC) + ' X: ' +  DbgS(X) + ' Y: ' + DbgS(Y),
     ' W: ' + DbgS(Width) + ' H: ', DbgS(Height),
     ' XSrc: ' + DbgS(XSrc) + ' YSrc: ' + DbgS(YSrc),
     ' SrcW: ' + DbgS(SrcWidth), ' SrcH: ' + DbgS(SrcHeight) + ' Rop: ' + DbgS(Rop));
  {$ENDIF}
  
  if not CheckDC(DestDC, SName, 'Dest') then Exit;
  if not CheckDC(SrcDC, SName, 'Src') then Exit;
  if not (TCarbonDeviceContext(SrcDC) is TCarbonBitmapContext) then
  begin
    DebugLn(SName + ' Error - invalid source device context ', TCarbonDeviceContext(SrcDC).ClassName,
      ', expected TCarbonBitmapContext!');
    Exit;
  end;
  
  Result := TCarbonDeviceContext(DestDC).StretchDraw(X, Y, Width, Height,
    TCarbonBitmapContext(SrcDC), XSrc, YSrc, SrcWidth, SrcHeight,
    TCarbonBitmap(Mask), XMask, YMask, Rop);
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

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.UpdateWindow Handle: ' + DbgS(Handle));
  {$ENDIF}
  
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
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCarbonWidgetSet.WindowFromPoint Point: ' + DbgS(Point));
  {$ENDIF}

  P.h := Point.X;
  P.v := Point.Y;
  if FindWindowOfClass(P, kAllWindowClasses, Window, @WindowPart) <> noErr then Exit;
  if Window = nil then Exit;
  if WindowPart <> inContent then Exit;
  
  if OSError(GetWindowBounds(Window, kWindowContentRgn, R), Self,
    'WindowFromPoint', SGetWindowBounds) then Exit;
    
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






