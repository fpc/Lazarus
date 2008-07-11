{%MainUnit gtk2int.pas}
{ $Id$ }
{******************************************************************************
                         All GTK2 Winapi implementations.
                   Initial Revision  : Mon Sep 22 15:50:00 2003


  !! Keep alphabetical !!

  Support routines go to gtk2proc.pp

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
{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$EndIf}

//##apiwiz##sps##   // Do not remove


function TGtk2WidgetSet.BeginPaint(Handle: hWnd; var PS : TPaintStruct) : hdc;
var
  paintrect : TGDKRectangle;
  Control: TWinControl;
begin
  result := inherited BeginPaint(Handle, PS);
  
  if Handle <> 0
  then Control := TWinControl(GetLCLObject(Pointer(Handle)))
  else Control := nil;

  if (Control <> nil)
  and Control.DoubleBuffered
  and not GTK_WIDGET_DOUBLE_BUFFERED(PGTKWidget(Handle))
  then begin
    //DebugLn(['TGtk2WidgetSet.BeginPaint ',DbgSName(Control)]);
    paintrect.x := PS.rcPaint.Left;
    paintrect.y := PS.rcPaint.Top;
    paintrect.width := PS.rcPaint.Right- PS.rcPaint.Left;
    paintrect.height := PS.rcPaint.Bottom - PS.rcPaint.Top;
    if (paintrect.width <= 0) or (paintrect.height <=0)
    then begin
      paintrect.x := 0;
      paintrect.y := 0;
      gdk_drawable_get_size(TGtkDeviceContext(Result).Drawable,
                            @paintrect.width, @paintrect.height);
    end;
    gdk_window_freeze_updates(TGtkDeviceContext(Result).Drawable);
    gdk_window_begin_paint_rect (TGtkDeviceContext(Result).Drawable, @paintrect);
  end;
end;

function TGtk2WidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
{off $DEFINE VerboseFonts}
var
  GdiObject: PGdiObject;
  FullString: String;
  aFamily,aStyle: String;
  aSize: Integer;
  PangoDesc: PPangoFontDescription;
  CachedFont: TGtkFontCacheDescriptor;
  AttrList: PPangoAttrList;
  AttrListTemporary: Boolean;
  Attr: PPangoAttribute;
  CurFont: PPangoLayout;
begin
  {$IFDEF VerboseFonts}
  DebugLn('TGtk2WidgetSet.CreateFontIndirectEx A Name=',LogFont.lfFaceName,' Height=',dbgs(LogFont.lfHeight),' LongName=',LongFontName);
  {$ENDIF}
  Result := 0;
  PangoDesc := nil;
  GdiObject := nil;
  try
    // first search in cache
    CachedFont:=FontCache.FindGTkFontDesc(LogFont,LongFontName);
    if CachedFont<>nil then begin
      CachedFont.Item.IncreaseRefCount;
      GdiObject := NewGdiObject(gdiFont);
      GdiObject^.GDIFontObject := TGtkFontCacheItem(CachedFont.Item).GtkFont;
      {$IFDEF VerboseFonts}
      WriteLn('Was already in cache');
      {$ENDIF}
      exit;
    end;

    with LogFont do
    begin
      if lfFaceName[0] = #0
      then begin
        Assert(false,'ERROR: [Tgt2kObject.CreateFontIndirectEx] No fontname');
        Exit;
      end;
      
      if (lfHeight = 0) and (CompareText(lfFacename, 'default') = 0) then
      begin
        // use default font
        {$IFDEF VerboseFonts}
        DebugLn(['TGtk2WidgetSet.CreateFontIndirectEx Creating default font']);
        {$ENDIF}
        GdiObject := CreateDefaultFont;
        exit;
      end;

      FontNameToPangoFontDescStr(LongFontname, aFamily, aStyle, aSize);

      // if font specified size, prefer this instead of 'possibly' inaccurate
      // lfHeight note that lfHeight may actually have a most accurate value
      // but there is no way to know this at this point.
      
      // setting the size, this could be done in two ways
      // method 1: fontdesc using fontname like "helvetica 12"
      // method 2: fontdesc using fontname like "helvetica" and later modify size
      
      // to obtain consistent font sizes method 2 should be used
      // for method 1 converting lfheight to fontsize can lead to rounding errors
      //   for example, font size=12, lfheight=-12 (75dpi), at 75 dpi aSize=11
      //   so we would get a font "helvetica 11" instead of "helvetica 12"
      // size information, and later modify font size
      
      // using method 2
      if (aSize=0) and (lfHeight=0) then
        FullString:='10' // use some default: TODO: find out the default size of the widget
      else if aSize>0 then
        FullString:=IntToStr(aSize)
      else
        FullString:='';
      FullString := AFamily + ' ' + aStyle + ' ' + FullString;

      PangoDesc := pango_font_description_from_string(PChar(FullString));

      if lfWeight <> FW_DONTCARE then
        pango_font_description_set_weight(PangoDesc, lfWeight);

      if lfItalic <> 0 then
        pango_font_description_set_style(PangoDesc, PANGO_STYLE_ITALIC);

      if (aSize=0) and (lfHeight<>0) then
      begin
        // a size is not specified, try to calculate one based on lfHeight
        // and use this value not in the font name but set this value appart
        // NOTE: in gtk2.8 is possible to use pango_font_description_set_absolute_size
        // which would be great with the given lfheight value, but older gtk2 version
        // doesn't have this function
        if lfHeight<0 then
          aSize := -MulDiv(lfheight, 72, ScreenInfo.PixelsPerInchY) * PANGO_SCALE
        else
          aSize := lfHeight * PANGO_SCALE;
        pango_font_description_set_size(PangoDesc, aSize);
      end;
      
      // create font
      // TODO: use context widget (CreateFontIndirectEx needs a parameter for this: Context: HWnd)
      GdiObject := NewGdiObject(gdiFont);
      GdiObject^.GDIFontObject:=gtk_widget_create_pango_layout(
                                               GetStyleWidget(lgsdefault), nil);
      CurFont:=GdiObject^.GDIFontObject;

      pango_layout_set_font_description(CurFont,PangoDesc);

      if (LogFont.lfUnderline<>0) or (LogFont.lfStrikeOut<>0) then
      begin
        AttrListTemporary := false;
        AttrList := pango_layout_get_attributes(CurFont);
        if (AttrList = nil) then
        begin
          AttrList := pango_attr_list_new();
          AttrListTemporary := True;
        end;
        if LogFont.lfUnderline <> 0 then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);
        pango_attr_list_change(AttrList, Attr);

        Attr := pango_attr_strikethrough_new(LogFont.lfStrikeOut<>0);
        pango_attr_list_change(AttrList, Attr);
        
        if AttrListTemporary then
          pango_attr_list_unref(AttrList);
      end;

      pango_layout_set_single_paragraph_mode(CurFont, True);
      pango_layout_set_width(CurFont, -1);
      pango_layout_set_alignment(CurFont, PANGO_ALIGN_LEFT);

      if (lfEscapement <> 0) then
      begin
        // the rotation is done via the pango matrix of the context
        // it must be set by the device context
      end;
    end;
  finally
    if (CachedFont = nil) and (GdiObject<>nil) and (GdiObject^.GDIFontObject <> nil) then
    begin
      // add to cache
      CachedFont := FontCache.Add(GdiObject^.GDIFontObject, LogFont, LongFontName);
      if CachedFont <> nil then
      begin
        CachedFont.PangoFontDescription := PangoDesc;
        PangoDesc := nil;
      end;
    end;
    {$IFDEF VerboseFonts}
    if (GdiObject<>nil) and (GdiObject^.GDIFontObject <> nil) then begin
      DebugLn(['TGtk2WidgetSet.CreateFontIndirectEx New pangolayout=',dbgs(GdiObject^.GDIFontObject),' Cached=',FontCache.FindGTKFont(GdiObject^.GDIFontObject)<>nil]);
    end;
    {$ENDIF}
    // clean up helper objects
    if PangoDesc<>nil then
      pango_font_description_free(PangoDesc);
      
    if (GdiObject<>nil) then begin
      if (GdiObject^.GDIFontObject = nil) then begin
        DebugLn(['TGtk2WidgetSet.CreateFontIndirectEx Unable to create font A']);
        DisposeGDIObject(GdiObject);
        Result := 0;
      end else begin
        // return the new font
        GdiObject^.LogFont:=LogFont;
        Result := HFONT(PtrUInt(GdiObject));
      end;
    end else begin
      {$IFDEF VerboseFonts}
      DebugLn(['TGtk2WidgetSet.CreateFontIndirectEx Unable to create font B']);
      {$ENDIF}
    end;
    {$IFDEF VerboseFonts}
    DebugLn(['TGtk2WidgetSet.CreateFontIndirectEx END Result=',dbgs(Pointer(PtrInt(Result)))]);
    {$ENDIF}
  end;
end;

function TGtk2WidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
var
  bitmap: PGdkBitmap;
  pixmap: PGdkPixmap;
  pixbuf: PGdkPixbuf;
  Width, Height: integer;
  MaxWidth, MaxHeight: guint;
begin
  Result := 0;
  if not IsValidGDIObject(IconInfo^.hbmColor) then Exit;

  pixmap := PGDIObject(IconInfo^.hbmColor)^.GDIPixmapObject.Image;
  //DbgDumpPixmap(pixmap, '');

  gdk_drawable_get_size(pixmap, @Width, @Height);

  if not IconInfo^.fIcon then
  begin
    gdk_display_get_maximal_cursor_size(gdk_display_get_default,
                                        @MaxWidth, @MaxHeight);

    if (Width > integer(MaxWidth))
    or (Height > integer(MaxHeight)) then Exit;
  end;
  
  bitmap := CreateGdkMaskBitmap(IconInfo^.hbmColor, IconInfo^.hbmMask);
  try
    pixbuf := CreatePixbufFromImageAndMask(pixmap, 0, 0, Width, Height, nil, Bitmap);
    
    if IconInfo^.fIcon
    then begin
      Result := HICON(PtrUInt(pixbuf));
      Exit;
    end;
    
    // create cursor from pixbuf
    Result := HCURSOR(PtrUInt(gdk_cursor_new_from_pixbuf(gdk_display_get_default, pixbuf,
      IconInfo^.xHotSpot, IconInfo^.yHotSpot)));

  finally
    if bitmap <> nil
    then gdk_bitmap_unref(bitmap);
  end;
end;

function TGtk2WidgetSet.DestroyIcon(Handle: HICON): Boolean;
begin
  Result := (Handle <> 0) and
            (
              GDK_IS_PIXBUF(Pointer(Handle)) or
              // todo: replace with GDK_IS_CURSOR when fpc will have it
              G_TYPE_CHECK_INSTANCE_TYPE(Pointer(Handle),GDK_TYPE_CURSOR)
            );
  if Result then
    if GDK_IS_PIXBUF(Pointer(Handle)) then
      gdk_pixbuf_unref(PGdkPixbuf(Handle))
    else
      gdk_cursor_unref(PGdkCursor(Handle));
end;


{------------------------------------------------------------------------------
  Function: EndPaint
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
 
function TGtk2WidgetSet.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
var
  Control: TWinControl;
begin

  if Handle <> 0 then Control := TWinControl(GetLCLObject(Pointer(Handle)))
  else Control := nil;

  If (Control <> nil) and (not GTK_WIDGET_DOUBLE_BUFFERED((PGTKWidget(Handle)))) and (Control.DoubleBuffered) then
  begin
    if PS.HDC <> 0 then begin
      gdk_window_thaw_updates(TGtkDeviceContext(PS.HDC).Drawable);
      gdk_window_end_paint (TGtkDeviceContext(PS.HDC).Drawable);
    end;
  end;

  result := Inherited EndPaint(Handle, PS);
end;

{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TGtk2WidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  DevCtx: TGtk2DeviceContext absolute DC;
  
  LineStart, LineEnd, StrEnd: PChar;
  Width, Height: Integer;
  TopY, LineLen, LineHeight: Integer;
  TxtPt: TPoint;
  DCOrigin: TPoint;
  Foreground: PGDKColor;
  CurDx: PInteger;
  CurStr: PChar;

  procedure DoTextOut(X,Y : Integer; Str: Pchar; CurCount: Integer);
  var
    CurScreenX: LongInt;
    CharLen: LongInt;
  begin
    if (Dx <> nil) then
    begin
      CurScreenX := X;
      while CurCount > 0 do
      begin
        CharLen := UTF8CharacterLength(CurStr);
        DevCtx.DrawTextWithColors(CurStr, CharLen, CurScreenX, Y, Foreground, nil);
        inc(CurScreenX, CurDx^);
        inc(CurDx);
        inc(CurStr, CharLen);
        dec(CurCount, CharLen);
      end;
    end
    else
      DevCtx.DrawTextWithColors(Str, Count, X, Y, Foreground, nil);
  end;

begin
  //DebugLn(['TGtk2WidgetSet.ExtTextOut X=',X,' Y=',Y,' Str="',copy(Str,1,Count),'" Count=',Count,' DX=',dbgs(DX)]);
  Assert(False, Format('trace:> [TGtk2WidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := IsValidDC(DC);
  if not Result then Exit;
  
  if DevCtx.GC <> nil then; // create GC

  if ((Options and (ETO_OPAQUE+ETO_CLIPPED)) <> 0)
  and (Rect=nil)
  then begin
    DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Rect=nil');
    Result := False;
    exit;
  end;

  // to reduce flickering calculate first and then paint
  
  DCOrigin := DevCtx.Offset;

  if (Options and ETO_CLIPPED) <> 0
  then begin
    X := Rect^.Left;
    Y := Rect^.Top;
    IntersectClipRect(DC, Rect^.Left, Rect^.Top,
                      Rect^.Right, Rect^.Bottom);
  end;

  LineLen := FindLineLen(Str,Count);
  TopY := Y;
  UpdateDCTextMetric(DevCtx);
  TxtPt.X := X + DCOrigin.X;
  LineHeight := DevCtx.DCTextMetric.TextMetric.tmHeight;
  TxtPt.Y := TopY + DCOrigin.Y;

  DevCtx.SelectedColors := dcscCustom;

  if ((Options and ETO_OPAQUE) <> 0) then
  begin
    Width := Rect^.Right - Rect^.Left;
    Height := Rect^.Bottom - Rect^.Top;
    EnsureGCColor(DC, dccCurrentBackColor, True, False);
    gdk_draw_rectangle(DevCtx.Drawable, DevCtx.GC, 1,
                       Rect^.Left+DCOrigin.X, Rect^.Top+DCOrigin.Y,
                       Width, Height);
  end;

  EnsureGCColor(DC, dccCurrentTextColor, True, False);
  Foreground := nil;//StyleForegroundColor(CurrentTextColor.ColorRef, nil);

  CurDx:=Dx;
  CurStr:=Str;
  LineStart:=Str;
  if LineLen < 0 then
  begin
    LineLen:=Count;
    if Count> 0 then
      DoTextOut(TxtPt.X, TxtPt.Y, LineStart, LineLen);
  end else
  begin  //write multiple lines
    StrEnd := Str + Count;
    while LineStart < StrEnd do
    begin
      LineEnd := LineStart + LineLen;
      if LineLen>0 then
        DoTextOut(TxtPt.X, TxtPt.Y, LineStart, LineLen);
      inc(TxtPt.Y, LineHeight);
      LineStart := LineEnd + 1; // skip #13
      if (LineStart<StrEnd) and (LineStart^ in [#10,#13])
      and (LineStart^ <> LineEnd^) then
        inc(LineStart); // skip #10
      Count := StrEnd - LineStart;
      LineLen := FindLineLen(LineStart, Count);
      if LineLen < 0 then
        LineLen := Count;
    end;
  end;

  Result := True;
  Assert(False, Format('trace:< [TGtk2WidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
end;

{------------------------------------------------------------------------------
  Function: GetCursorPos
  Params:  lpPoint: The cursorposition
  Returns: True if succesful

 ------------------------------------------------------------------------------}
function TGtk2WidgetSet.GetCursorPos(var lpPoint: TPoint ): Boolean;
begin
  gdk_display_get_pointer(gdk_display_get_default(), nil, @lpPoint.X, @lpPoint.Y, nil);
  Result := True;
end;

{------------------------------------------------------------------------------
  function TGtk2WidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar;
    Count: Integer; var Size: TSize): Boolean;

 ------------------------------------------------------------------------------}
function TGtk2WidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
var
  UseFont : PPangoLayout;
begin
  //inherited GetTextExtentPoint;
  Result := IsValidDC(DC);
  if not Result then Exit;
  if Count <= 0 then
  begin
    FillChar(Size, SizeOf(Size), 0);
    Exit;
  end;

  UseFont := GetGtkFont(TGtkDeviceContext(DC));

  UpdateDCTextMetric(TGtkDeviceContext(DC));

  TGtk2DeviceContext(DC).SetText(UseFont, Str, Count);
  pango_layout_get_pixel_size(UseFont, @Size.cX, @Size.cY);
  //DebugLn(['TGtk2WidgetSet.GetTextExtentPoint Str="',copy(Str,1,Count),' Count=',Count,' X=',Size.cx,' Y=',Size.cY]);

end;

function TGtk2WidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
{$ifdef GTK_2_8}
  gdk_display_warp_pointer(gdk_display_get_default(), gdk_screen_get_default(), X, Y);
  Result := True;
{$else}
  Result:=inherited SetCursorPos(X, Y);
{$endif}
end;

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TGtk2WidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  GtkWindow: PGtkWindow;
begin
  Result:=false;
  GtkWindow:=PGtkWindow(hWnd);
  if GtkWindow=nil then
    RaiseGDBException('TGtk2WidgetSet.ShowWindow  hWnd is nil');
  if not GtkWidgetIsA(PGtkWidget(GtkWindow),GTK_TYPE_WINDOW) then begin
    DebugLn(['TGtk2WidgetSet.ShowWindow ',GetWidgetDebugReport(PGTKWidget(GtkWindow))]);
    RaiseGDBException('TGtk2WidgetSet.ShowWindow  hWnd is not a gtkwindow');
  end;

  //debugln('TGtk2WidgetSet.ShowWindow A ',GetWidgetDebugReport(PGtkWidget(GtkWindow)),' nCmdShow=',dbgs(nCmdShow),' SW_MINIMIZE=',dbgs(SW_MINIMIZE=nCmdShow));

  case nCmdShow of

  SW_SHOWNORMAL:
    begin
      gtk_window_deiconify(GtkWindow);
      gtk_window_unmaximize(GtkWindow);
    end;

  SW_HIDE:
    gdk_window_hide(PgtkWidget(GtkWindow)^.Window);

  SW_MINIMIZE:
    gtk_window_iconify(GtkWindow);

  SW_SHOWMAXIMIZED:
    gtk_window_maximize(GtkWindow);

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
function TGtk2WidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar;
  Count: Integer) : Boolean;
var
  DevCtx: TGtk2DeviceContext absolute DC;
  DCOrigin: TPoint;
  yOffset: integer;
begin
  Result := IsValidDC(DC);
  if not Result then Exit;
  if Count <= 0 then Exit;
  
  UpdateDCTextMetric(DevCtx);
  DCOrigin := DevCtx.Offset;
  
  with DevCtx.DCTextMetric.TextMetric do
    yOffset := tmHeight-tmDescent-tmAscent;
  if yOffset < 0 then
    yOffset := 0;
    
  DevCtx.SelectedColors := dcscCustom;
  EnsureGCColor(DC, dccCurrentTextColor, True, False);

  DevCtx.DrawTextWithColors(Str, Count,
    X + DCOrigin.X, Y + DCOrigin.Y + yOffset,
    nil, nil);
end;

function TGtk2WidgetSet.UpdateWindow(Handle: HWND): Boolean;
var
  CurWidget: PGtkWidget;
begin
  CurWidget:=PGTKWidget(Handle);
  //DebugLn(['TGtk2WidgetSet.UpdateWindow ',GetWidgetDebugReport(CurWidget)]);
  if GTK_WIDGET_DRAWABLE(CurWidget) then begin
    //DebugLn(['TGtk2WidgetSet.UpdateWindow DRAWING']);
    gtk_widget_queue_draw(CurWidget);
    gdk_window_process_updates(CurWidget^.window,TRUE);
    Result:=true;
  end else
    Result:=false;
end;

//##apiwiz##eps##   // Do not remove

{$IfDef ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$EndIf}
