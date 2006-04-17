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
function TQtWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI BeginPaint');
  {$endif}

{  if IsDoubleBuffered then
    Result :=GetDoubleBufferedDC(Handle)
  else}
  
  PS.hdc := HDC(TQtDeviceContext.Create(Handle));

  if Handle <> 0 then TQtMainWindow(Handle).Canvas := TQtDeviceContext(PS.hdc);

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
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI CreateBitmapFromRawImage');
  {$endif}

  Result := False;
  Bitmap := 0;
  MaskBitmap := 0;

  Bitmap := HBitmap(QImage_create(RawImage.Data, RawImage.Description.Width,
   RawImage.Description.Height, QImageFormat_ARGB32));

  Result := True;
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
    WriteLn('WinAPI CreateCompatibleDC ( DC: ', IntToStr(DC), ' )');
  {$endif}

  Result := GetDC(0);
end;

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var Rect: TRect; Flags: Cardinal): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI DrawText');
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then Exit;

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
    WriteLn('WinAPI EndPaint ( Handle=', IntToStr(Handle), ' PS.HDC=', IntToStr(PS.HDC), ' )');
  {$endif}

  Result := 1;

  if (PS.HDC <> 0) then
   if (TObject(PS.HDC) is TQtDeviceContext) then
   begin
     WriteLn('Freeing resources');
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
    WriteLn('WinAPI ExtTextOut');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  WideStr := WideString(Str);

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
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI GetBitmapRawImageDescription');
  {$endif}
  Result:=false;
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
    WriteLn('WinAPI GetDC ( hWnd: ', IntToStr(hWnd), ' )');
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
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI GetDeviceRawImageDescription');
  {$endif}
  Result := true;

  FillChar(Desc^, SizeOf(Desc^), 0);
  Desc^.Format := ricfRGBA;
  Desc^.HasPalette := False;
  Desc^.Depth := 32;
  // Width and Height not relevant
  Desc^.PaletteColorCount := 0;
  Desc^.BitOrder := riboReversedBits;
  Desc^.ByteOrder := riboLSBFirst;
  Desc^.LineOrder := riloTopToBottom;
  Desc^.ColorCount := Desc^.PaletteColorCount;
  Desc^.BitsPerPixel := 32;
  Desc^.LineEnd := rileDWordBoundary;
//  FillRawImageDescriptionColors(Desc);
  Desc^.AlphaPrec := 1;
  Desc^.AlphaSeparate := False;
  // CreateBitmap winapi call wants word-aligned data
  Desc^.AlphaShift := 0;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetRawImageFromDevice
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect;
  var NewRawImage: TRawImage): boolean;
var
  SrcWidth, SrcHeight: Integer;
{  hMemDC: HDC;
  hMemBitmap: HBITMAP;
  hOldObject: HGDIOBJ;}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI GetRawImageFromDevice');
  {$endif}
  Result:=false;
  FillChar(NewRawImage, SizeOf(NewRawImage), 0);

  // make bitmap compatible to src device
  SrcWidth := SrcRect.Right - SrcRect.Left;
  SrcHeight := SrcRect.Bottom - SrcRect.Top;
{  hMemBitmap := Windows.CreateCompatibleBitmap(SrcDC, SrcWidth, SrcHeight);
  Result := hMemBitmap <> 0;
  if not Result then exit;}

  // make memory device context compatible to device, to select bitmap in for copying
{  hMemDC := Windows.CreateCompatibleDC(SrcDC);
  Result := hMemDC <> 0;
  hOldObject := Windows.SelectObject(hMemDC, hMemBitmap);}

  // copy srcdc -> membitmap
{  Result := Result and Windows.BitBlt(hMemDC, 0, 0, SrcWidth, SrcHeight,
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
function TQtWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
{var
  BitmapInfo: Windows.TBitmap;
  ARect: TRect;}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI GetRawImageFromBitmap');
  {$endif}
  Result:=false;
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
  Function: GetSystemMetrics
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  P : Pointer;
begin
  Assert(False, Format('Trace:> [TGtkWidgetSet.GetSystemMetrics] %d', [nIndex]));
  case nIndex of
    SM_ARRANGE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_ARRANGE          ');
      end;
    SM_CLEANBOOT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CLEANBOOT        ');
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
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowRect(Handle: HWND; Var ARect: TRect): Integer;
begin
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
  TQtWidget(aHandle).Update;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: LineTo
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI LineTo');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).drawLine(
   TQtDeviceContext(DC).PenPos.X,
   TQtDeviceContext(DC).PenPos.Y,
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
    WriteLn('WinAPI MoveToEx');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  if (OldPoint <> nil) then OldPoint^ := TQtDeviceContext(DC).PenPos;

  TQtDeviceContext(DC).PenPos := Point(X, Y);
  
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
    WriteLn('WinAPI Rectangle');
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
    WriteLn('WinAPI ReleaseDC');
  {$endif}

  Result := 0;

  if DC <> 0 then TQtDeviceContext(DC).Free;
  
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI SelectObject ( DC=', IntToStr(DC), ' GDIObj=', IntToStr(GDIObj), ' )');
  {$endif}

  Result := 0;
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
    WriteLn('WinAPI SetCursorPos');
  {$endif}

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
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI ShowWindow');
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
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('WinAPI StretchMaskBlt ( DestDC=' + IntToStr(DestDC) + ' SrcDC=' +
     IntToStr(SrcDC) + ' )');
  {$endif}


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
    WriteLn('WinAPI TextOut');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  WideStr := WideString(Str);

//  if TQtDeviceContext(DC).isDrawing then TQtDeviceContext(DC).drawText(X, Y, @WideStr)
//  else TQtDeviceContext(DC).AddObject(dcTextOut, @WideStr, X, Y);
  
  TQtDeviceContext(DC).drawText(X, Y, @WideStr);
  
  Result := True;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line


