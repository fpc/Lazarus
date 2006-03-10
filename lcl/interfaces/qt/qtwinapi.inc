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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
function TQtWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI BeginPaint');
  {$endif}

{  if IsDoubleBuffered then
    Result :=GetDoubleBufferedDC(Handle)
  else}
  
  PS.hdc := HDC(TQtDeviceContext.Create(Handle));

  if Handle <> 0 then TQtCustomForm(Handle).Canvas := TQtDeviceContext(Result);

  Result := PS.hdc;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.CreateBitmapFromRawImage
  Params:
  Returns:

  This functions is for TBitmap support
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
var
  Image, Mask: QImageH;
  Format: QImageFormat;
begin
  Result:=false;
  Bitmap:=0;
  MaskBitmap:=0;

  if (RawImage.Description.Width=0) or (RawImage.Description.Height=0) then
   Exit;

  if RawImage.Description.HasPalette and (RawImage.Description.BitsPerPixel <> 8) then
   Exit;

  {------------------------------------------------------------------------------
    Tryes to fit the image into Qt's classification. If not, it´s not supported by Qt
   ------------------------------------------------------------------------------}
  case RawImage.Description.BitsPerPixel of
   1: if RawImage.Description.BitOrder = riboBitsInOrder then
       Format := QImageFormat_MonoLSB
      else Format := QImageFormat_Mono;
   8: Format := QImageFormat_Indexed8;
   32:
   begin
     if RawImage.Description.AlphaPrec = 0 then
      Format := QImageFormat_RGB32
     else Format := QImageFormat_ARGB32;
   end;
  else
    Exit;
  end;

  Image := QImage_create(RawImage.Data, RawImage.Description.Width,
   RawImage.Description.Height, Format);

  if (AlwaysCreateMask or (not RawImageMaskIsEmpty(@RawImage,true))) then
  begin
    Mask := QImage_create(RawImage.Mask, RawImage.Description.Width,
     RawImage.Description.Height, QImageFormat_MonoLSB);
  end;
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.
 ------------------------------------------------------------------------------}
{function TQtWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
var
  pNewDC: TDeviceContext;
begin
  Result := 0;
  pNewDC := NewDC;

  pNewDC.CurrentFont := CreateDefaultFont;
  pNewDC.CurrentBrush := CreateDefaultBrush;
  pNewDC.CurrentPen := CreateDefaultPen;

  Result := HDC(pNewDC);

  Assert(False, Format('trace:  [TQtWidgetSet.CreateCompatibleDC] DC: 0x%x --> 0x%x', [Integer(DC), Integer(Result)]));
end;}

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var Rect: TRect; Flags: Cardinal): Integer;
var
  WideStr: WideString;
begin
  WideStr := WideString(Str);

  if DC = 0 then Exit;

  if (Flags and DT_CALCRECT) = DT_CALCRECT then
  begin
    Result := 30;
  end
  else
  begin
    Result := 40;
  end;

//  else
//    TQtDeviceContext(DC).drawText(Rect.Left, Rect.Top, @WideStr);
//    Result := 30;
//  end;
  
//  if Rect.Right = 40 then raise Exception.create('Error');
end;

{------------------------------------------------------------------------------
  Function: EndPaint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI EndPaint');
  {$endif}

  Result := 1;

  if PS.HDC <> 0 then TQtDeviceContext(PS.HDC).Free;
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
  Result := False;

  WideStr := WideString(Str);

  if DC = 0 then Exit;

//  if TQtDeviceContext(DC).isDrawing then TQtDeviceContext(DC).drawText(X, Y, @WideStr)
//  else TQtDeviceContext(DC).AddObject(dcTextOut, @WideStr, X, Y);

  TQtDeviceContext(DC).drawText(X, Y, @WideStr);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetBitmapRawImageDescription
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): Boolean;
begin
{  Result := Windows.GetObject(Bitmap, SizeOf(BitmapInfo), @BitmapInfo) > 0;
  if Result then
    FillRawImageDescription(BitmapInfo, Desc);}
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
    WriteLn('WinAPI GetClientBounds');
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
    WriteLn('WinAPI GetClientRect');
  {$endif}

  QWidget_rect(TQtWidget(handle).Widget, @ARect);

  Result:=true;
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
    WriteLn('WinAPI GetDC');
  {$endif}

  Result := HDC(TQtDeviceContext.Create(0));
  
//  if hWnd <> 0 then TQtCustomForm(hWnd).Canvas := TQtDeviceContext(Result);
end;


{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetDeviceRawImageDescription
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceRawImageDescription(DC: HDC; Desc: PRawImageDescription): boolean;
begin
{  Result := true;

  FillChar(Desc^, SizeOf(Desc^), 0);
  Desc^.Format := ricfRGBA;
  Desc^.HasPalette := (Windows.GetDeviceCaps(DC, RASTERCAPS) and RC_PALETTE) <> 0;
  Desc^.Depth := Windows.GetDeviceCaps(DC, BITSPIXEL) * Windows.GetDeviceCaps(DC, PLANES);
  // Width and Height not relevant
  Desc^.PaletteColorCount := Windows.GetDeviceCaps(DC, SIZEPALETTE);
  Desc^.BitOrder := riboReversedBits;
  Desc^.ByteOrder := riboLSBFirst;
  Desc^.LineOrder := riloTopToBottom;
  Desc^.ColorCount := Desc^.PaletteColorCount;
  if Desc^.HasPalette then
    Desc^.BitsPerPixel := Windows.GetDeviceCaps(DC, COLORRES)
  else
    Desc^.BitsPerPixel := Desc^.Depth;
  Desc^.LineEnd := rileDWordBoundary;
  FillRawImageDescriptionColors(Desc);
  Desc^.AlphaPrec := 1;
  Desc^.AlphaSeparate := true; // the alpha is stored as separate Mask
  // The next values are only valid, if there is a separate alpha mask
  Desc^.AlphaBitsPerPixel := 1; // bits per alpha mask pixel.
  Desc^.AlphaBitOrder := riboReversedBits;
  Desc^.AlphaByteOrder := riboLSBFirst;
  // CreateBitmap winapi call wants word-aligned data
  Desc^.AlphaLineEnd := rileWordBoundary;
  Desc^.AlphaShift := 0;}
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetRawImageFromDevice
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
{var
  SrcWidth, SrcHeight: Integer;
  hMemDC: HDC;
  hMemBitmap: HBITMAP;
  hOldObject: HGDIOBJ;}
begin
{  FillChar(NewRawImage, SizeOf(NewRawImage), 0);

  // make bitmap compatible to src device
  SrcWidth := SrcRect.Right - SrcRect.Left;
  SrcHeight := SrcRect.Bottom - SrcRect.Top;
  hMemBitmap := Windows.CreateCompatibleBitmap(SrcDC, SrcWidth, SrcHeight);
  Result := hMemBitmap <> 0;
  if not Result then exit;

  // make memory device context compatible to device, to select bitmap in for copying
  hMemDC := Windows.CreateCompatibleDC(SrcDC);
  Result := hMemDC <> 0;
  hOldObject := Windows.SelectObject(hMemDC, hMemBitmap);

  // copy srcdc -> membitmap
  Result := Result and Windows.BitBlt(hMemDC, 0, 0, SrcWidth, SrcHeight,
        SrcDC, SrcRect.Left, SrcRect.Top, SRCCOPY);

  // done copying, deselect bitmap from dc
  Windows.SelectObject(hMemDC, hOldObject);

  // copy membitmap -> rawimage
  Result := Result and GetRawImageFromBitmap(hMemBitmap, 0,
    Rect(0, 0, SrcWidth, SrcHeight), NewRawImage);

  // free temporary stuff
  Windows.DeleteDC(hMemDC);
  Windows.DeleteObject(hMemBitmap);}
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetRawImageFromBitmap
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
{var
  BitmapInfo: Windows.TBitmap;
  ARect: TRect;}
begin
{  FillChar(NewRawImage, SizeOf(NewRawImage), 0);
  Result := Windows.GetObject(SrcBitmap, SizeOf(BitmapInfo), @BitmapInfo) > 0;
  if not Result then exit;

  FillRawImageDescription(BitmapInfo, @NewRawImage.Description);
  ARect := SrcRect;
  if ARect.Top > BitmapInfo.bmHeight then
    ARect.Top := BitmapInfo.bmHeight;
  if ARect.Bottom > BitmapInfo.bmHeight then
    ARect.Bottom := BitmapInfo.bmHeight;
  if ARect.Left > BitmapInfo.bmWidth then
    ARect.Left := BitmapInfo.bmWidth;
  if ARect.Right > BitmapInfo.bmWidth then
    ARect.Right := BitmapInfo.bmWidth;

  // copy bitmap
  AllocAndCopy(BitmapInfo, SrcBitmap, ARect, NewRawImage.Data, NewRawImage.DataSize);

  // check mask
  if SrcMaskBitmap <> 0 then
  begin
    Result := Windows.GetObject(SrcMaskBitmap, SizeOf(BitmapInfo), @BitmapInfo) > 0;
    if not Result then exit;

    AllocAndCopy(BitmapInfo, SrcMaskBitmap, ARect, NewRawImage.Mask, NewRawImage.MaskSize);
    NewRawImage.Description.AlphaSeparate := true;
  end;}
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
    WriteLn('WinAPI Rectangle');
  {$endif}

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
    WriteLn('WinAPI ReleaseDC');
  {$endif}

  Result := 0;

  if DC <> 0 then TQtDeviceContext(DC).Free;
  
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: SetCursorPos
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  QCursor_setPos(X, Y);
  
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
  Result := False;
  
  Widget := QWidgetH(hWnd);

//  if QWidgetH = nil then RaiseException('TQtWidgetSet.ShowWindow  hWnd is nil');

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
  Result := False;

  WideStr := WideString(Str);
  
  if DC = 0 then Exit;

//  if TQtDeviceContext(DC).isDrawing then TQtDeviceContext(DC).drawText(X, Y, @WideStr)
//  else TQtDeviceContext(DC).AddObject(dcTextOut, @WideStr, X, Y);
  
  TQtDeviceContext(DC).drawText(X, Y, @WideStr);
  
  Result := True;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line


