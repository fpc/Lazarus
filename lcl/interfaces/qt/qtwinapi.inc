{%MainUnit qtint.pp}
{ $Id$ }
{******************************************************************************
  All QT Winapi implementations.
  This are the implementations of the overrides of the QT Interface for the
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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TQtWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI BeginPaint] Handle=', dbgs(Handle));
  {$endif}

{  if IsDoubleBuffered then
    Result :=GetDoubleBufferedDC(Handle)
  else}
  
  PS.hdc := HDC(TQtDeviceContext.Create(Handle));

  if Handle <> 0 then TQtMainWindow(Handle).Canvas := TQtDeviceContext(PS.hdc);

  Result := PS.hdc;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbgs(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: CombineRgn
  Params:  Dest, Src1, Src2, fnCombineMode
  Returns: longint

  Combine the 2 Source Regions into the Destination Region using the specified
  Combine Mode. The Destination must already be initialized. The Return value
  is the Destination's Region type, or ERROR.

  The Combine Mode can be one of the following:
      RGN_AND  : Gets a region of all points which are in both source regions

      RGN_COPY : Gets an exact copy of the first source region

      RGN_DIFF : Gets a region of all points which are in the first source
                 region but not in the second.(Source1 - Source2)

      RGN_OR   : Gets a region of all points which are in either the first
                 source region or in the second.(Source1 + Source2)

      RGN_XOR  : Gets all points which are in either the first Source Region
                 or in the second, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function TQtWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;
begin
  Result := SimpleRegion;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.CreateBitmapFromRawImage
  Params:
  Returns:

  This functions is for TBitmap support
  
  The memory allocation code was added because it is necessary for
 TBitmap.LoadFromDevice support. For other operations it isn´t needed
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
var
  NewData: PByte;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI CreateBitmapFromRawImage]',
     ' Width:', dbgs(RawImage.Description.Width),
     ' Height:', dbgs(RawImage.Description.Height),
     ' DataSize: ', dbgs(RawImage.DataSize),
     ' CreateMask: ', dbgs(AlwaysCreateMask));
  {$endif}

  Result := False;
  Bitmap := 0;
  MaskBitmap := 0;

  NewData := GetMem(RawImage.DataSize);

  Move(RawImage.Data^, NewData^, RawImage.DataSize);

  Bitmap := HBitmap(TQtImage.Create(NewData, RawImage.Description.Width,
   RawImage.Description.Height, QImageFormat_RGB32));

  Result := True;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI CreateBitmapFromRawImage] Bitmap:', dbgs(Integer(Bitmap)));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function:  CreateBrushIndirect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
var
  QtBrush: TQtBrush;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn(Format('Trace:> [WinAPI CreateBrushIndirect]  Style: %d, Color: %8x', [LogBrush.lbStyle, LogBrush.lbColor]));
  {$endif}

  result := 0;

  QtBrush := TQtBrush.Create(True);

  try
    case LogBrush.lbStyle of
//      BS_HOLLOW,         // Hollow brush.
      BS_NULL:           // Same as BS_HOLLOW.
      begin
        QtBrush.setStyle(QtNoBrush);
      end;

      BS_SOLID:          // Solid brush.
      begin
        QtBrush.setStyle(QtSolidPattern);
      end;

      BS_HATCHED:        // Hatched brush.
      begin
        case LogBrush.lbHatch of
          HS_BDIAGONAL: QtBrush.setStyle(QtBDiagPattern);
          HS_CROSS: QtBrush.setStyle(QtCrossPattern);
          HS_DIAGCROSS: QtBrush.setStyle(QtDiagCrossPattern);
          HS_FDIAGONAL: QtBrush.setStyle(QtFDiagPattern);
          HS_HORIZONTAL: QtBrush.setStyle(QtHorPattern);
          HS_VERTICAL: QtBrush.setStyle(QtVerPattern);
        else
          RaiseGDBException('invalid lbHatch');
        end;
      end;

      BS_DIBPATTERN,     // A pattern brush defined by a device-independent
             // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERN, the
             // lbHatch member contains a handle to a packed DIB.Windows 95:
             // Creating brushes from bitmaps or DIBs larger than 8x8 pixels
             // is not supported. If a larger bitmap is given, only a portion
             // of the bitmap is used.
      BS_DIBPATTERN8X8,  // Same as BS_DIBPATTERN.
      BS_DIBPATTERNPT,   // A pattern brush defined by a device-independent
             // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERNPT, the
             // lbHatch member contains a pointer to a packed DIB.
      BS_PATTERN,        // Pattern brush defined by a memory bitmap.
      BS_PATTERN8X8:     // Same as BS_PATTERN.
      begin
      end;
    else
      WriteLn(Format('Unsupported Style %d',[LogBrush.lbStyle]));
    end;

    {   Other non-utilized Qt brushes:

    QtDense1Pattern,
    QtDense2Pattern,
    QtDense3Pattern,
    QtDense4Pattern,
    QtDense5Pattern,
    QtDense6Pattern,
    QtDense7Pattern,
    QtLinearGradientPattern,
    QtRadialGradientPattern,
    QtConicalGradientPattern,
    QtTexturePattern = 24 );}
  except
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI CreateBrushIndirect] Failed');
    {$endif}
  end;

  Result := HBRUSH(QtBrush);

  {$ifdef VerboseQtWinAPI}
    WriteLn(Format('Trace:< [WinAPI CreateBrushIndirect] Got --> %x', [Result]));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI CreateCompatibleDC] DC: ', IntToStr(DC));
  {$endif}

  Result := GetDC(0);
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirect
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := CreateFontIndirectEx(LogFont, '');
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirectEx
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT;
var
  QtFont: TQtFont;
  FamilyName: string;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI CreateFontIndirectEx] FontName: ' + LongFontName);
  {$endif}

  Result := 0;

  QtFont := TQtFont.Create(True);
  try
    if LogFont.lfHeight > 0 then QtFont.setPointSize(LogFont.lfHeight)
    else if LogFont.lfHeight < 0 then QtFont.setPointSize(-1 * LogFont.lfHeight);

    // Some values at available on Qt documentation at a table
    // Others are guesses. The best would be to test different values for those
    // See: http://doc.trolltech.com/4.1/qfont.html#Weight-enum
    case LogFont.lfWeight of
      FW_THIN       : QtFont.setWeight(10);
      FW_EXTRALIGHT : QtFont.setWeight(15);
      FW_LIGHT      : QtFont.setWeight(25);
      FW_NORMAL     : QtFont.setWeight(50);
      FW_MEDIUM     : QtFont.setWeight(55);
      FW_SEMIBOLD   : QtFont.setWeight(63);
      FW_BOLD       : QtFont.setWeight(75);
      FW_EXTRABOLD  : QtFont.setWeight(80);
      FW_HEAVY      : QtFont.setWeight(87);
    end;

//    LogFont.lfOrientation: Longint;

    QtFont.setItalic(LogFont.lfItalic = High(Byte));
    QtFont.setUnderline(LogFont.lfUnderline = High(Byte));
    QtFont.setStrikeOut(LogFont.lfStrikeOut = High(Byte));

    FamilyName := StrPas(LogFont.lfFaceName);

    if (CompareText(FamilyName, 'default') <> 0) then
    begin
      QtFont.setFamily(FamilyName);
    end;

  finally
    Result := HFONT(QtFont);
  end;
end;

{------------------------------------------------------------------------------
  Function: CreateRectRgn
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: DeleteObject
  Params:  none
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TQtWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI DeleteObject] GDIObject: ', IntToStr(GDIObject));
    ObjType := 'Unidentifyed';
  {$endif}

  Result := False;

  if GDIObject = 0 then
  begin
    Result := true;

    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI DeleteObject]');
    {$endif}

    Exit;
  end;
  
  if not IsValidGDIObject(GDIObject) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI DeleteObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObject);

  {------------------------------------------------------------------------------
    Font
   ------------------------------------------------------------------------------}
  if aObject is TQtFont then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Font';
    {$endif}
    
//    TQtFont(aObject).Free;
  end
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  else if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}

//    TQtBrush(aObject).Free;
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

//    TQtImage(aObject).Free;
  end;

  // Find out if we want to release internal GDI object
{    case GDIType of
      gdiBrush:
      gdiBitmap:
      gdiPen:
      gdiRegion:
      gdiPalette:
      else begin
        Result:= false;
        DebugLn('[TGtkWidgetSet.DeleteObject] TODO : Unimplemented GDI type');
        Assert(False, 'Trace:TODO : Unimplemented GDI object in delete object');
  end;}

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI DeleteObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var Rect: TRect; Flags: Cardinal): Integer;
var
  WideStr: WideString;
  QtFontMetrics: TQtFontMetrics;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DrawText] DC: ', dbgs(DC), ' Str: ', string(Str),
     ' CalcRect: ', dbgs((Flags and DT_CALCRECT) = DT_CALCRECT),
     ' Rect.Left: ', Rect.Left, ' Rect.Top: ', Rect.Top);
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then Exit;

  WideStr := UTF8Decode(Str);

  if (Flags and DT_CALCRECT) = DT_CALCRECT then
  begin
    QtFontMetrics := TQtFontMetrics.Create(TQtDeviceContext(DC).font.Widget);
    try
      Result := QtFontMetrics.width(@WideStr);
      if Result < 30 then Result := 30;
    finally
      QtFontMetrics.Free;
    end;

    Exit;
  end;

  TQtDeviceContext(DC).drawText(Rect.Left, Rect.Top, @WideStr);

//  if Rect.Right = 40 then raise Exception.create('Error');
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
begin
  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).drawEllipse(x1, y1, X2 - X1, Y2 - Y1);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: EndPaint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI EndPaint] Handle: ', IntToStr(Handle), ' PS.HDC: ', IntToStr(PS.HDC));
  {$endif}

  Result := 1;

  if IsValidDC(PS.HDC) then
   if (TObject(PS.HDC) is TQtDeviceContext) then
   begin
    {$ifdef VerboseQtWinAPI}
       WriteLn('Freeing resources');
    {$endif}
    
     TQtDeviceContext(PS.HDC).Free;
   end;
end;

{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  WideStr: WideString;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ExtTextOut]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  WideStr := UTF8Decode(Str);

//  if TQtDeviceContext(DC).isDrawing then TQtDeviceContext(DC).drawText(X, Y, @WideStr)
//  else TQtDeviceContext(DC).AddObject(dcTextOut, @WideStr, X, Y);

  TQtDeviceContext(DC).drawText(X, Y, @WideStr);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetBitmapRawImageDescription
  Params:  none
  Returns: The handle of the window with focus

  Describes the inner format utilized by Qt + the specific information for this image
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): Boolean;
var
  BitmapInfo: TDIBSECTION;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetBitmapRawImageDescription] Bitmap=', dbgs(Bitmap));
  {$endif}

  Result := true;

  FillStandardDescription(Desc^);

  GetObject(Bitmap, SizeOf(BitmapInfo), @BitmapInfo);

  Desc^.Width := BitmapInfo.dsBm.bmWidth;
  Desc^.Height := BitmapInfo.dsBm.bmHeight;

//  Desc^.BitOrder := riboReversedBits;
//  Desc^.ByteOrder := riboLSBFirst;
//  Desc^.LineOrder := riloTopToBottom;
//  Desc^.ColorCount := 0;                // entries in color palette. Ignore when no palette.
//  Desc^.BitsPerPixel := BitmapInfo.bmBitsPixel;    // bits per pixel. can be greater than Depth.
//  Desc^.LineEnd := rileDWordBoundary;
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
Function TQtWidgetSet.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetClientBounds]');
  {$endif}

  QWidget_rect(TQtWidget(handle).Widget, @ARect);

  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
Function TQtWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetClientRect]');
  {$endif}

  QWidget_rect(TQtWidget(handle).Widget, @ARect);

  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer

  Returns a copy of the current Clipping Region.

  The result can be one of the following constants
     0 = no clipping set
     1 = ok
    -1 = error
 ------------------------------------------------------------------------------}
Function TQtWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN): Longint;
begin
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: GetCursorPos
  Params:  lpPoint: The cursorposition
  Returns: True if succesful

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetCursorPos(var lpPoint: TPoint ): Boolean;
begin
  QCursor_pos(@lpPoint);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetDC
  Params:  hWnd is any widget.
  Returns: Nothing

  This function is Called:
  - Once on app startup with hWnd = 0
  - Twice for every TLabel on the TCustomLabel.CalcSize function
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDC(hWnd: HWND): HDC;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetDC] hWnd: ', IntToStr(hWnd));
  {$endif}

  Result := HDC(TQtDeviceContext.Create(0));
  
//  if hWnd <> 0 then TQtCustomForm(hWnd).Canvas := TQtDeviceContext(Result);

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetDC] Result: ', dbgs(Result));
  {$endif}
end;


{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetDeviceRawImageDescription
  Params:  none
  Returns: True if successful

  Describes the standard format utilized by Qt
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceRawImageDescription(DC: HDC; Desc: PRawImageDescription): boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceRawImageDescription] DC: ' + IntToStr(DC));
  {$endif}
  Result := true;

  FillStandardDescription(Desc^);
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetDeviceSize
  Params:  none
  Returns: True if successful

  Return the size of a device
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
var
  Size: TSize;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceSize]');
  {$endif}

  Result := False;

  P.X := 0;
  P.Y := 0;

  if not IsValidDC(DC) then Exit;
  
  if (TObject(DC) is TQtDeviceContext) then
  begin
    if TQtDeviceContext(DC).Parent <> nil then
    begin
      QWidget_size(TQtDeviceContext(DC).Parent, @Size);
    
      P.X := Size.cx;
      P.Y := Size.cy;
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetObject
  Params:  none
  Returns: The size written to the buffer

  Necessary for TBitmap support
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
var
  aObject: TObject;
  NumColors, Width, Height: Longint;
  BitmapSection : TDIBSECTION;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetObject] GDIObj: ' + IntToStr(GDIObj));
    ObjType := '';
  {$endif}
  
  Result := 0;
  
  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI GetObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);

  {------------------------------------------------------------------------------
    Font
   ------------------------------------------------------------------------------}
  if aObject is TQtFont then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Font';
    {$endif}

  end
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  else if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

    if Buf = nil then Result := SizeOf(TDIBSECTION)
    else
    begin
      Width := TQtImage(aObject).width;
      Height := TQtImage(aObject).height;
    
      FillChar(BitmapSection, SizeOf(TDIBSECTION), 0);

      {dsBM - BITMAP}
      BitmapSection.dsBm.bmType := $4D42;
      BitmapSection.dsBm.bmWidth := Width;
      BitmapSection.dsBm.bmHeight := Height;
      BitmapSection.dsBm.bmWidthBytes := 0;
      BitmapSection.dsBm.bmPlanes := 1;//Does Bitmap Format support more?
      BitmapSection.dsBm.bmBitsPixel := 1;
      BitmapSection.dsBm.bmBits := nil;

      {dsBmih - BITMAPINFOHEADER}
      BitmapSection.dsBmih.biSize := 40;
      BitmapSection.dsBmih.biWidth := Width;
      BitmapSection.dsBmih.biHeight := Height;
      BitmapSection.dsBmih.biPlanes := BitmapSection.dsBm.bmPlanes;
      BitmapSection.dsBmih.biBitCount := 1;

      BitmapSection.dsBmih.biCompression := 0;
      BitmapSection.dsBmih.biSizeImage := 0;

      BitmapSection.dsBmih.biXPelsPerMeter := 0;
      BitmapSection.dsBmih.biYPelsPerMeter := 0;

      BitmapSection.dsBmih.biClrUsed   := 0;
      BitmapSection.dsBmih.biClrImportant := 0;

{             case GDIBitmapType of
                gbBitmap:
                  If GDIBitmapObject <> nil then begin
                    GDK_WINDOW_GET_SIZE(GDIBitmapObject, @biWidth, @biHeight);
                    NumColors := 2;
                    biBitCount := 1;
                  end;
                gbPixmap:
                  If GDIPixmapObject <> nil then begin
                     biBitCount := word(gdk_drawable_get_depth(GDIPixmapObject));
                     gdk_drawable_get_size(GDIPixmapObject,@biWidth, @biHeight);
                  end;
              end;}

      BitmapSection.dsBmih.biBitCount := 32;

//              biSizeImage := (((biBitCount*biWidth+31) shr 5) shl 2)*biHeight;

//      BitmapSection.dsBmih.biXPelsPerMeter := ;

//      BitmapSection.dsBmih.biYPelsPerMeter := ;

//        BitmapSection.dsBm.bmHeight :=      bmWidth := biWidth;
//              bmHeight := biHeight;
//              bmBitsPixel := biBitCount;

      {dsBitfields: array[0..2] of DWORD;
       dshSection: THandle;
       dsOffset: DWORD;}

      if BufSize >= SizeOf(BitmapSection) then
      begin
        PDIBSECTION(Buf)^ := BitmapSection;
        Result := SizeOf(TDIBSECTION);
      end
      else if BufSize > 0 then
      begin
        Move(BitmapSection, Buf^, BufSize);
        Result := BufSize;
      end;
    end;
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetRawImageFromDevice
  Params:  none
  Returns: True if successful

  Important note: LCL requires that a Data and a Mask be allocated, even if not necessary,
 because it will try to free them without checking if they are nil.
  Allocating dummy data won´t leak memory, because LCL takes care to free it.
  What is important here is to free all Qt handles utilized.
  
  This function is utilized when the function TBitmap.LoadFromDevice is called
  
  The main use for this function is to get a screenshot. It may have other uses,
 but this is the only one implemented here.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
var
  IsDesktopDC: Boolean;
  SrcWidth, SrcHeight: Integer;
  WinID: Cardinal;
  desktop: QDesktopWidgetH;
  ScreenSize: TSize;
  PScreenSize: PSize;
  Pixmap: TQtPixmap;
  Image: QImageH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetRawImageFromDevice] SrcDC: ', dbgs(SrcDC),
     ' SrcWidth: ', dbgs(SrcRect.Right - SrcRect.Left),
     ' SrcHeight: ', dbgs(SrcRect.Bottom - SrcRect.Top));
  {$endif}

  Result := True;

  FillStandardDescription(NewRawImage.Description);

  SrcWidth := SrcRect.Right - SrcRect.Left;
  SrcHeight := SrcRect.Bottom - SrcRect.Top;
  
  IsDesktopDC := True; // Hard-coded, but a real check should be made

  if IsDesktopDC then
  begin
    PScreenSize := @ScreenSize;

    desktop := QApplication_desktop;

    QWidget_size(QDesktopWidget_screen(desktop), PScreenSize);

    WinID := QWidget_winId(QDesktopWidget_screen(desktop));

    Pixmap := TQtPixmap.Create(PScreenSize);
    try
      NewRawImage.Mask := GetMem(1); // Creates a dummy mask

      Pixmap.grabWindow(WinID);
    
      Image := QImage_Create;

      Pixmap.toImage(Image);
    
      NewRawImage.DataSize := QImage_numBytes(Image);

      NewRawImage.Data := GetMem(QImage_numBytes(Image));

      Move(QImage_bits(Image)^, NewRawImage.Data^, QImage_numBytes(Image));
    finally
      Pixmap.Free;
    end;

    // In this case we use the size of the desktop
    
    NewRawImage.Description.Width := QWidget_width(QApplication_desktop);
    NewRawImage.Description.Height := QWidget_height(QApplication_desktop);
  end
  else
  begin
    NewRawImage.Mask := GetMem(1); // Creates a dummy mask

    NewRawImage.Data := GetMem(1); // Creates a dummy data

    NewRawImage.Description.Width := SrcWidth;
    NewRawImage.Description.Height := SrcHeight;
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetRawImageFromDevice]');
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetRawImageFromBitmap
  Params:  none
  Returns: True if successful

  Creates a raw image from a bitmap
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
var
  Image: TQtImage;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetRawImageFromBitmap] SrcBitmap: ', dbgs(SrcBitmap));
  {$endif}
  Result := false;
  
  Image := TQtImage(SrcBitmap);
  
  FillChar(NewRawImage, SizeOf(NewRawImage), 0);

  FillStandardDescription(NewRawImage.Description);
  
  {
    For now the image is simply entirely copyed and SrcRect is ignored.
    
    In the future this will need to change
  }
{  NewRawImage.Description.Width := SrcRect.Right - SrcRect.Left;
  NewRawImage.Description.Height := SrcRect.Bottom - SrcRect.Top;

  if (NewRawImage.Description.Width <= 0) or (NewRawImage.Description.Height <= 0)
  then begin
    DebugLn('WARNING: TQtWidgetSet.GetRawImageFromBitmap Intersection empty');
    exit;
  end;}

  NewRawImage.Description.Width := Image.width;
  NewRawImage.Description.Height := Image.height;

  try
    NewRawImage.DataSize := Image.numBytes;

    // copy data
    ReAllocMem(NewRawImage.Data, NewRawImage.DataSize);
    if NewRawImage.DataSize > 0 then
     Move(Image.bits()^, NewRawImage.Data^, NewRawImage.DataSize);
  except
    // Free partially copied data
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSysColor(nIndex: Integer): DWORD;

  {------------------------------------------------------------------------------
    Function: GetColor
    Params:   A Qt color group and a Qt color role
    Returns:  TColor

  ------------------------------------------------------------------------------}
  function GetColor(Group: QPaletteColorGroup; Role: QPaletteColorRole): TColor;
  var
    Handle : QPaletteH;
    QColor : PQColor;
  begin
    Handle := QPalette_create;
    QApplication_palette(Handle);

    QColor:=QPalette_color(Handle, Group,Role);
    Result:=(QColor^.r and $00FF) or ((QColor^.g and $00FF) shl 8) or ((QColor^.b and $00FF) shl 16);

    QPalette_destroy(Handle);
  end;

begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:Unknown lcl system color: [TQtWidgetSet.GetSysColor]');
    {$endif}
    exit;
  end;
    
  case nIndex of
    COLOR_SCROLLBAR               : Result:=GetColor(QPaletteActive,   QPaletteMid);
    COLOR_BACKGROUND              : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_ACTIVECAPTION           : Result:=GetColor(QPaletteActive,   QPaletteBase);
    COLOR_INACTIVECAPTION         : Result:=GetColor(QPaletteInActive, QPaletteBase);
    COLOR_MENU                    : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_WINDOW                  : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_WINDOWFRAME             : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_MENUTEXT                : Result:=GetColor(QPaletteActive,   QPaletteWindowText);
    COLOR_WINDOWTEXT              : Result:=GetColor(QPaletteActive,   QPaletteWindowText);
    COLOR_CAPTIONTEXT             : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_ACTIVEBORDER            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_INACTIVEBORDER          : Result:=GetColor(QPaletteInactive, QPaletteWindow);
    COLOR_APPWORKSPACE            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_HIGHLIGHT               : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
    COLOR_HIGHLIGHTTEXT           : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);
    COLOR_BTNFACE                 : Result:=GetColor(QPaletteActive,   QPaletteButton);
    COLOR_BTNSHADOW               : Result:=GetColor(QPaletteActive,   QPaletteShadow);
    COLOR_GRAYTEXT                : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_BTNTEXT                 : Result:=GetColor(QPaletteActive,   QPaletteButtonText);
    COLOR_INACTIVECAPTIONTEXT     : Result:=GetColor(QPaletteInactive, QPaletteText);
    COLOR_BTNHIGHLIGHT            : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);
    COLOR_3DDKSHADOW              : Result:=GetColor(QPaletteActive,   QPaletteMid);
    COLOR_3DLIGHT                 : Result:=GetColor(QPaletteActive,   QPaletteMidlight);
    COLOR_INFOTEXT                : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_INFOBK                  : Result:=GetColor(QPaletteActive,   QPaletteBase);
    // PBD: 25 is unassigned in all the docs I can find
    //      if someone finds what this is supposed to be then fill it in
    //      note defaults below, and cl[ColorConst] in graphics
    COLOR_HOTLIGHT                : Result:=GetColor(QPaletteActive,   QPaletteLight);
    COLOR_GRADIENTACTIVECAPTION   : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_GRADIENTINACTIVECAPTION : Result:=GetColor(QPaletteInactive, QPaletteText);
    COLOR_FORM                    : Result:=GetColor(QPaletteActive,   QPaletteWindow);

    COLOR_clForeground..COLOR_clHighlightedText
                                  : Result:=GetColor(QPaletteActive,   nIndex - COLOR_clForeground);
    COLOR_clNormalForeground..COLOR_clNormalHighlightedText
                                  : Result:=GetColor(QPaletteInactive, nIndex - COLOR_clNormalForeground);
    COLOR_clDisabledForeground..COLOR_clDisabledHighlightedText
                                  : Result:=GetColor(QPaletteDisabled, nIndex - COLOR_clDisabledForeground);
    COLOR_clActiveForeground..COLOR_clActiveHighlightedText
                                  : Result:=GetColor(QPaletteActive,   nIndex - COLOR_clActiveForeground);
  else
    Result:=0;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetSystemMetrics
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn(Format('Trace:> [TQtWidgetSet.GetSystemMetrics] %d', [nIndex]));
  {$endif}

  case nIndex of
    SM_ARRANGE:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_ARRANGE          ');
        {$endif}
      end;
    SM_CLEANBOOT:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CLEANBOOT          ');
        {$endif}
      end;
    SM_CMOUSEBUTTONS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXBORDER         ');
      end;
    SM_CYBORDER:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYBORDER         ');
      end;
    SM_CXCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXCURSOR         ');
      end;
    SM_CYCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYCURSOR         ');
      end;
    SM_CXDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
      end;
    SM_CXDRAG:
      begin
        Result := 2;
      end;
    SM_CYDRAG:
      begin
        Result := 2;
      end;
    SM_CXEDGE:
      begin
        Result := 2;
      end;
    SM_CYEDGE:
      begin
        Result := 2;
      end;
    SM_CXFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHSCROLL:
      begin
{        P:=GetStyleWidget(lgsVerticalScrollbar);
        Result := GTK_Widget(P)^.requisition.Width;}
      end;
    SM_CYHSCROLL:
      begin
{        P:=GetStyleWidget(lgsHorizontalScrollbar);
        Result := GTK_Widget(P)^.requisition.Height;}
      end;
    SM_CXHTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXICON           ');
      end;
    SM_CYICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYICON           ');
      end;
    SM_CXICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMENUSIZE       ');
      end;
    SM_CYMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENUSIZE       ');
      end;
    SM_CXMIN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINTRACK       ');
      end;
    SM_CXSCREEN:
      begin
        Result := QWidget_width(QApplication_desktop);
      end;
    SM_CYSCREEN:
      begin
        Result := QWidget_height(QApplication_desktop);
      end;
    SM_CXSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
    SM_CXSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSIZEFRAME      ');
      end;
    SM_CYSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSIZEFRAME      ');
      end;
    SM_CXSMICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSMICON         ');
      end;
    SM_CYSMICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMICON         ');
      end;
    SM_CXSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVSCROLL:
      begin
{        P:=GetStyleWidget(lgsVerticalScrollbar);
        Result := GTK_Widget(P)^.requisition.Width;}
      end;
    SM_CYVSCROLL:
      begin
{        P:=GetStyleWidget(lgsHorizontalScrollbar);
        Result := GTK_Widget(P)^.requisition.Height;}
      end;
    SM_CYCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYCAPTION        ');
      end;
    SM_CYKANJIWINDOW:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
    else Result := 0;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextColor(DC: HDC) : TColorRef;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextColor]');
  {$endif}

  Result := 0;
  
{  if IsValidDC(DC) then
    with TQtDeviceContext(DC) do
    begin
      Result := CurrentTextColor.ColorRef;
    end;}
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  QtFontMetrics: TQtFontMetrics;
  WideStr: WideString;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextExtentPoint]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  QtFontMetrics := TQtFontMetrics.Create(TQtDeviceContext(DC).font.Widget);
  try
    WideStr := Utf8Decode(Str);
    Size.cx := QtFontMetrics.width(@WideStr);
    Size.cy := QtFontMetrics.height;
  finally
    QtFontMetrics.Free;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextMetrics]');
  {$endif}

  Result := IsValidDC(DC);

  if Result then
  begin
  end;
end;


{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowRect(Handle: HWND; Var ARect: TRect): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRect]');
  {$endif}

  Result := 0;
  
  ARect.Top := QWidget_y(TQtWidget(Handle).Widget);
  ARect.Left := QWidget_x(TQtWidget(Handle).Widget);
  ARect.Bottom := QWidget_height(TQtWidget(Handle).Widget) + QWidget_y(TQtWidget(Handle).Widget);
  ARect.Right := QWidget_width(TQtWidget(Handle).Widget) + QWidget_x(TQtWidget(Handle).Widget);

  Result := -1;
end;

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : HWND;
  Returns: true on success

  returns the current widget Left, Top, relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowRelativePosition(Handle: HWND; var Left, Top: integer): boolean;
{var
  LeftTop:TPoint;
  R: TRect;
  ParentHandle: THandle;
  WindowInfo: PWindowInfo;}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRelativePosition]');
  {$endif}

  Result := False;
  
{  WindowInfo := GetWindowInfo(Handle);
  if (WindowInfo^.WinControl is TCustomFloatSpinEdit) then
    Handle := Windows.SendMessage(Handle, UDM_GETBUDDY, 0, 0);
  if not Windows.GetWindowRect(Handle,@R) then exit;
  LeftTop.X:=R.Left;
  LeftTop.Y:=R.Top;
  ParentHandle:=Windows.GetParent(Handle);
  if ParentHandle<>0 then
  begin
    if not Windows.ScreenToClient(ParentHandle,@LeftTop) then exit;
    if not GetLCLClientBoundsOffset(ParentHandle, R) then
      exit;
    dec(LeftTop.X, R.Left);
    dec(LeftTop.Y, R.Top);
  end;
  Left:=LeftTop.X;
  Top:=LeftTop.Y;
  Result := True;}
end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: integer): boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowSize]');
  {$endif}

  Result := False;

  Height := QWidget_height(TQtWidget(Handle).Widget);
  Width := QWidget_width(TQtWidget(Handle).Widget);

  Result := True;

  // Here we should convert top level lcl window coordinaties to qt coord
  // Due to borders and etc
{  Style := Windows.GetWindowLong(Handle, GWL_STYLE);
  ExStyle := Windows.GetWindowLong(Handle, GWL_EXSTYLE);
  if (Style and WS_THICKFRAME) <> 0 then
  begin
    // thick, sizing border
    // add twice, top+bottom border
    Dec(Width, 2*Windows.GetSystemMetrics(SM_CXSIZEFRAME));
    Dec(Height, 2*Windows.GetSystemMetrics(SM_CYSIZEFRAME));
  end else
  if (Style and WS_BORDER) <> 0 then
  begin
    // thin, non-sizing border
    Dec(Width, 2*Windows.GetSystemMetrics(SM_CXFIXEDFRAME));
    Dec(Height, 2*Windows.GetSystemMetrics(SM_CYFIXEDFRAME));
  end;
  if (Style and WS_CAPTION) <> 0 then
    if (ExStyle and WS_EX_TOOLWINDOW) <> 0 then
      Dec(Height, Windows.GetSystemMetrics(SM_CYSMCAPTION))
    else
      Dec(Height, Windows.GetSystemMetrics(SM_CYCAPTION));

  if (WindowInfo^.WinControl is TCustomFloatSpinEdit) then
    AdjustForBuddySize;}
end;

{------------------------------------------------------------------------------
  Function: InvalidateRect
  Params: aHandle:
          Rect:
          bErase:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect; bErase: Boolean): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Invalidate Rect]');
  {$endif}

  TQtWidget(aHandle).Update;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: LineTo
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  BrushPos: TPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI LineTo]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).brushOrigin(@BrushPos);

  TQtDeviceContext(DC).drawLine(
   TQtDeviceContext(DC).Origin.X + BrushPos.X,
   TQtDeviceContext(DC).Origin.Y + BrushPos.Y,
   X, Y);
   
  MoveToEx(DC, X, Y, nil);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI MoveToEx] DC:', dbgs(DC), ' X:', dbgs(X), ' Y:', dbgs(Y));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  if (OldPoint <> nil) then TQtDeviceContext(DC).brushOrigin(OldPoint);

  TQtDeviceContext(DC).setBrushOrigin(X, Y);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: Rectangle
  Params:  DC: HDC; X1, Y1, X2, Y2: Integer
  Returns: Nothing

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  //  Result := IsValidDC(DC);

  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Rectangle]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).drawRect(x1, y1, X2 - X1, Y2 - Y1);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: ReleaseDC
  Params:     hWnd:       Handle to the window whose DC is to be released.
              hDC:        Handle to the DC to be released.
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ReleaseDC] hWnd: ', dbgs(hWnd), ' DC: ', dbgs(DC));
  {$endif}

  Result := 0;

  if IsValidDC(DC) then TQtDeviceContext(DC).Free;
  
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: The GDI object of the same type previously associated with the DC

  Changes one of the GDI objects (Font, Brush, etc) of a Device Context;
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  aObject: TObject;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI SelectObject] DC=', dbgs(DC), ' GDIObj=', dbgs(GDIObj));
  {$endif}

  Result := 0;
  
  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SelectObject] Invalid DC');
    {$endif}

    Exit;
  end;
  
  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SelectObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);
  
  if aObject is TQtFont then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Font';
    {$endif}
    
    Result := HGDIOBJ(TQtDeviceContext(DC).font);
    
    TQtDeviceContext(DC).setFont(TQtFont(aObject));
  end
  else if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}
    
    Result := HGDIOBJ(TQtDeviceContext(DC).brush);
  end
  else if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

    Result := HGDIOBJ(TQtDeviceContext(DC).vImage);

    TQtDeviceContext(DC).vImage := TQtImage(aObject).Handle;
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI SelectObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: SetCursorPos
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetCursorPos]');
  {$endif}

  QCursor_setPos(X, Y);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  SetWindowOrgEx
  Params:  DC    - handle of device context
           NewX  - new x-coordinate of window origin
           NewY  - new y-coordinate of window origin
           Point - record receiving original origin
  Returns: Whether the call was successful

  Sets the window origin of the device context by using the specified coordinates.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer; OldPoint: PPoint) : Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowOrgEx] DC: ', dbgs(DC), ' NewX: ', dbgs(NewX), ' NewY: ', dbgs(NewY));
  {$endif}

  Result := False;

  if IsValidDC(DC) then
  begin
    TQtDeviceContext(DC).Origin.X := -NewX;
    TQtDeviceContext(DC).Origin.Y := -NewY;
    
    if OldPoint <> nil then OldPoint^ := TQtDeviceContext(DC).Origin;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TQtWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  Widget: QWidgetH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ShowWindow]');
  {$endif}

  Result := False;
  
  Widget := QWidgetH(hWnd);

//  if Widget = nil then RaiseException('TQtWidgetSet.ShowWindow  hWnd is nil');

  case nCmdShow of

    SW_SHOW: QWidget_setVisible(Widget, True);

    SW_SHOWNORMAL: QWidget_showNormal(Widget);

    SW_MINIMIZE: QWidget_setWindowState(Widget, QtWindowMinimized);

    SW_SHOWMINIMIZED: QWidget_showMinimized(Widget);

    SW_SHOWMAXIMIZED: QWidget_showMaximized(Widget);

    SW_HIDE: QWidget_setVisible(Widget, False);
    
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: StretchBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified raster operation. If needed it
  resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
  If SrcDC contains a mask the pixmap will be copied with this transparency.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean;
begin
  Result := StretchMaskBlt(DestDC,X,Y,Width,Height,
                          SrcDC,XSrc,YSrc,SrcWidth,SrcHeight,
                          0,0,0,
                          ROp);
end;

{------------------------------------------------------------------------------
  Function: StretchMaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified mask and raster operation. If needed
  it resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SrcRect, DstRect: TRect;
  Image: QImageH;
begin
  DstRect := Bounds(X, Y, Width, Height);

  SrcRect := Bounds(XSrc, YSrc, SrcWidth, SrcHeight);
  
  Image := TQtDeviceContext(SrcDC).vImage;

  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI StretchMaskBlt] DestDC:', dbgs(DestDC), ' SrcDC:', dbgs(SrcDC),
     ' Image:', dbgs(Integer(Image)),
     ' X:', dbgs(X), ' Y:', dbgs(Y),
     ' W:', dbgs(Width), ' H:', dbgs(Height),
     ' XSrc:', dbgs(XSrc), ' YSrc:', dbgs(YSrc),
     ' WSrc:', dbgs(SrcWidth), ' HSrc:', dbgs(SrcHeight));
  {$endif}

  TQtDeviceContext(DestDC).drawImage(@DstRect, Image, @SrcRect);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TextOut
  Params: DC:
          X:
          Y:
          Str:
          Count:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : PChar; Count: Integer) : Boolean;
var
  WideStr: WideString;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI TextOut]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  WideStr := UTF8Decode(Str);

//  if TQtDeviceContext(DC).isDrawing then TQtDeviceContext(DC).drawText(X, Y, @WideStr)
//  else TQtDeviceContext(DC).AddObject(dcTextOut, @WideStr, X, Y);
  
  TQtDeviceContext(DC).drawText(X, Y, @WideStr);
  
  Result := True;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

