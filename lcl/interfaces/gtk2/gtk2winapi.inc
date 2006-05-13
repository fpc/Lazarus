{%MainUnit gtk2int.pp}
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


function TGtk2WidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc;
var
  paintrect : TGDKRectangle;
begin
  result := Inherited BeginPaint(Handle, PS);

  If (Handle <> 0) and (not GTK_WIDGET_DOUBLE_BUFFERED((PGTKWidget(Handle)))) then
  begin
    paintrect.x := PS.rcPaint.Left;
    paintrect.y := PS.rcPaint.Top;
    paintrect.width := PS.rcPaint.Right- PS.rcPaint.Left;
    paintrect.height := PS.rcPaint.Bottom - PS.rcPaint.Top;
    if (paintrect.width <= 0) or (paintrect.height <=0) then begin
      paintrect.x := 0;
      paintrect.y := 0;
      gdk_drawable_get_size(TDeviceContext(Result).Drawable, @paintrect.width, @paintrect.height);
    end;
    gdk_window_freeze_updates(TDeviceContext(Result).Drawable);
    gdk_window_begin_paint_rect (TDeviceContext(Result).Drawable, @paintrect);
  end;
end;

function TGtk2WidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
var
  GdiObject: PGdiObject;
  FullString: String;
  aFamily,aStyle: String;
  aSize: Integer;

  procedure LoadDefaultFont;
  begin
    DisposeGDIObject(GdiObject);
    GdiObject:=CreateDefaultFont;
  end;
  
begin
  result := 0;
  GdiObject := NewGdiObject(gdiFont);
  try
    with LogFont do begin
      if lfFaceName[0] = #0
      then begin
        Assert(false,'ERROR: [Tgt2kObject.CreateFontIndirectEx] No fontname');
        Exit;
      end;
      
      if CompareText(lfFacename,'default')=0 then begin
        LoadDefaultFont;
        Result := HFONT(GdiObject);
        exit;
      end;


      
      FontNameToPangoFontDescStr(LongFontname, aFamily, aStyle, aSize);
      
      // if font specified size, prefer this instead of 'posibly' inacurate
      // lfHeight note that lfHeight may actually have a most acurate value
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
      else
        FullString:=IntToStr(aSize);
      FullString := AFamily + ' ' + aStyle + ' ' + FullString;
      

      GdiObject^.GDIFontObject :=
        pango_font_description_from_string(PChar(FullString));

      If lfWeight <> FW_DONTCARE then
        pango_font_description_set_weight(GdiObject^.GDIFontObject, lfWeight);

      if lfItalic <> 0 then
        pango_font_description_set_style(GdiObject^.GDIFontObject,
                                         PANGO_STYLE_ITALIC);

      if (aSize=0) and (lfHeight<>0) then begin
        // a size is not specified, try to calculate one based on lfHeight
        // and use this value not in the font name but set this value appart
        // NOTE: in gtk2.8 is possible to use pango_font_description_set_absolute_size
        // which would be great with the given lfheight value, but older gtk2 version
        // doesn't have this funtion
        if lfHeight<0 then
          aSize:= (abs(lfheight) * 72) div ScreenInfo.PixelsPerInchX
        else
          aSize:=lfHeight;
        pango_font_description_set_size( GdiObject^.GDIFontObject, aSize*PANGO_SCALE);
      end;


      GdiObject^.StrikeOut := lfStrikeOut <> 0;
      GdiObject^.Underline := lfUnderline <> 0;

    end;
  finally
    if GdiObject^.GDIFontObject = nil
    then begin
      DisposeGDIObject(GdiObject);
      Result := 0;
    end
    else begin
      Result := HFONT(GdiObject);
    end;
  end;
end;


{------------------------------------------------------------------------------
  Function: EndPaint
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
 
Function TGtk2WidgetSet.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
begin
  If (Handle <> 0) and (not GTK_WIDGET_DOUBLE_BUFFERED((PGTKWidget(Handle)))) then
  begin
    if PS.HDC <> 0 then begin
      gdk_window_thaw_updates(TDeviceContext(PS.HDC).Drawable);
      gdk_window_end_paint (TDeviceContext(PS.HDC).Drawable);
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
  LineStart, LineEnd, StrEnd: PChar;
  Width, Height: Integer;
  TopY, LineLen, LineHeight : Integer;
  TxtPt : TPoint;
  DCOrigin: TPoint;

  UnRef,
  Underline,
  StrikeOut : Boolean;

  Foreground : PGDKColor;
  
  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;

  procedure DoTextOut(X,Y : Integer; Str : Pchar; Count: Integer);
  var
    aRect : TRect;
  begin
    with TDeviceContext(DC) do begin
      //fix me... and what about UTF-8 conversion?
      //this could be a massive problem since we
      //will need to know before hand what the current
      //locale is, and if we stored UTF-8 string this would break
      //cross-compatibility with GTK1.2 and win32 interfaces.....

      pango_layout_set_text(Layout, Str, Count);

      aRect := Classes.Rect(0,0,0,0);
      pango_layout_get_pixel_size(Layout, @arect.Right, @arect.Bottom);

      OffsetRect(aRect, X+DCOrigin.X,Y+DCOrigin.Y);

      gdk_draw_layout_with_colors(drawable, gc, aRect.Left, aRect.Top, Layout, Foreground, nil);
    end;
  end;

  procedure DrawTextLine;
  begin
    with TDeviceContext(DC) do begin
      DoTextOut(TxtPt.X, TxtPt.Y, LineStart, LineLen);
    end;
  end;

begin
  Assert(False, Format('trace:> [TGtk2WidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Uninitialized GC');
      Result := False;
    end
    else if ((Options and (ETO_OPAQUE+ETO_CLIPPED)) <> 0)
    and (Rect=nil) then begin
      DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Rect=nil');
      Result := False;
    end else begin
      // TODO: implement other parameters.

      UseFontDesc:=nil;
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;
        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;
        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;
      
      If (UseFontDesc = nil) or (GetStyleWidget(lgsdefault)=nil) then
        DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Missing Font')
      else begin
        // to reduce flickering calculate first and then paint
        
        DCOrigin:=GetDCOffset(TDeviceContext(DC));

        Layout := gtk_widget_create_pango_layout (GetStyleWidget(lgsdefault), nil);

        If (Layout = nil) then
          DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Missing Pango Layout')
        else begin
          pango_layout_set_font_description(Layout, UseFontDesc);

          AttrList := pango_layout_get_attributes(Layout);

          If (AttrList = nil) then
            AttrList := pango_attr_list_new();

          If Underline then
            Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
          else
            Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

          pango_attr_list_change(AttrList,Attr);

          Attr := pango_attr_strikethrough_new(StrikeOut);
          pango_attr_list_change(AttrList,Attr);

          if (Options and ETO_CLIPPED) <> 0 then
          begin
            X := Rect^.Left;
            Y := Rect^.Top;
            IntersectClipRect(DC, Rect^.Left, Rect^.Top,
                              Rect^.Right, Rect^.Bottom);
          end;

          LineLen := FindChar(#10,Str,Count);
          TopY := Y;
          UpdateDCTextMetric(TDeviceContext(DC));
          TxtPt.X := X + DCOrigin.X;
          LineHeight := DCTextMetric.TextMetric.tmHeight;
          TxtPt.Y := TopY + LineHeight + DCOrigin.Y;

          SelectedColors := dcscCustom;

          if ((Options and ETO_OPAQUE) <> 0) then
          begin
            Width := Rect^.Right - Rect^.Left;
            Height := Rect^.Bottom - Rect^.Top;
            EnsureGCColor(DC, dccCurrentBackColor, True, False);
            gdk_draw_rectangle(Drawable, GC, 1,
                               Rect^.Left+DCOrigin.X, Rect^.Top+DCOrigin.Y,
                               Width, Height);
          end;

          EnsureGCColor(DC, dccCurrentTextColor, True, False);
          pango_layout_set_attributes(Layout, AttrList);

          Foreground := StyleForegroundColor(CurrentTextColor.ColorRef, nil);

          LineStart:=Str;
          if LineLen < 0 then begin
            LineLen:=Count;
            if Count> 0 then DrawTextLine;
          end else
          Begin  //write multiple lines
            StrEnd:=Str+Count;
            while LineStart < StrEnd do begin
              LineEnd:=LineStart+LineLen;
              if LineLen>0 then DrawTextLine;
              inc(TxtPt.Y,LineHeight);
              LineStart:=LineEnd+1; // skip #10
              if (LineStart<StrEnd) and (LineStart^=#13) then
                inc(LineStart); // skip #10
              Count:=StrEnd-LineStart;
              LineLen:=FindChar(#10,LineStart,Count);
              if LineLen<0 then
                LineLen:=Count;
            end;
          end;

          g_object_unref(Layout);
          Result := True;
        end;
        If UnRef then
           pango_font_description_free(UseFontDesc);
      end;
    end;
  end;
  Assert(False, Format('trace:< [TGtk2WidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
end;

{------------------------------------------------------------------------------
  Function: GetCursorPos
  Params:  lpPoint: The cursorposition
  Returns: True if succesful

 ------------------------------------------------------------------------------}
function TGtk2WidgetSet.GetCursorPos(var lpPoint: TPoint ): Boolean;
{$IfnDef GTK2_2} //we need a GTK2_2 FLAG somehow
  {$IfNDef Win32}
  var
    root, child: pointer;
    winx, winy: Integer;
    xmask: Cardinal;
    TopList, List: PGList;
  {$EndIf}
{$EndIf}
begin
  Result := False;
{$IfDef GTK2_2} //we need a GTK2_2 FLAG somehow
  gdk_display_get_pointer(gdk_display_get_default(), nil, @lpPoint.X, @lpPoint.Y, nil);
  Result := True;
{$Else}
  {$IfNDef Win32}
   TopList := gdk_window_get_toplevels;
   List := TopList;
   while List <> nil do
   begin
     if (List^.Data <> nil)
     and gdk_window_is_visible(List^.Data)
     then begin
       XQueryPointer(gdk_x11_drawable_get_xdisplay (List^.Data),
                     gdk_x11_drawable_get_xid(List^.Data),
                     @root, @child, @lpPoint.X, @lpPoint.Y, @winx, @winy, @xmask);

       Result := True;
       Break;
     end;
     List := g_list_next(List);
   end;

   if TopList <> nil
   then g_list_free(TopList);
   {$Else}
      // Win32 Todo
      DebugLn('ToDo(Win32): TGtk2WidgetSet.GetCursorPos');
  {$EndIf}
{$EndIf}
end;

function TGtk2WidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
var
  UnRef,
  Underline,
  StrikeOut : Boolean;

  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
begin
  Result := IsValidDC(DC);
  if Result and (Count>0)
  then with TDeviceContext(DC) do
  begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;
        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;
        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;

      If UseFontDesc = nil then
        DebugLn('WARNING: [TGtk2WidgetSet.GetTextExtentPoint] Missing Font')
      else begin
        GetStyle(lgsdefault);
        Layout := gtk_widget_create_pango_layout (GetStyleWidget(lgsdefault), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        //fix me... what about &&, can we strip and do do markup substitution?
        If Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        pango_layout_set_attributes(Layout, AttrList);

        pango_layout_set_single_paragraph_mode(Layout, TRUE);
        pango_layout_set_width(Layout, -1);

        pango_layout_set_alignment(Layout, PANGO_ALIGN_LEFT);

        //fix me... and what about UTF-8 conversion?
        //this could be a massive problem since we
        //will need to know before hand what the current
        //locale is, and if we stored UTF-8 string this would break
        //cross-compatibility with GTK1.2 and win32 interfaces.....

        pango_layout_set_text(Layout, Str, Count);

        pango_layout_get_pixel_size(Layout, @Size.cX, @Size.cY);

        g_object_unref(Layout);

        Result := True;
        If UnRef then
          pango_font_description_free(UseFontDesc);
      end;
  end;
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
Function TGtk2WidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar;
  Count: Integer) : Boolean;
var
  DCOrigin: TPoint;
  aRect : TRect;

  UnRef,
  Underline,
  StrikeOut : Boolean;

  Layout : PPangoLayout;
  UseFontDesc : PPangoFontDescription;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
begin
  Result := IsValidDC(DC);
  if Result and (Count>0)
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtk2WidgetSet.TextOut] Uninitialized GC');
    end
    else begin
      if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
      then begin
        UseFontDesc := GetDefaultFontDesc(true);
        UnRef := True;
        Underline := False;
        StrikeOut := False;
      end
      else begin
        UseFontDesc := CurrentFont^.GDIFontObject;
        UnRef := False;
        Underline := CurrentFont^.Underline;
        StrikeOut := CurrentFont^.StrikeOut;
      end;

      If UseFontDesc = nil then
        DebugLn('WARNING: [TGtk2WidgetSet.TextOut] Missing Font')
      else begin
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        GetStyle(lgsdefault);
        Layout := gtk_widget_create_pango_layout (GetStyleWidget(lgsdefault), nil);
        pango_layout_set_font_description(Layout, UseFontDesc);
        AttrList := pango_layout_get_attributes(Layout);

        If (AttrList = nil) then
          AttrList := pango_attr_list_new();

        //fix me... what about &&, can we strip and do do markup substitution?
        If Underline then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(StrikeOut);
        pango_attr_list_change(AttrList,Attr);

        SelectedColors := dcscCustom;
        EnsureGCColor(DC, dccCurrentTextColor, True, False);
        pango_layout_set_attributes(Layout, AttrList);
        pango_layout_set_single_paragraph_mode(Layout, TRUE);
        pango_layout_set_width(Layout, -1);

        pango_layout_set_alignment(Layout, PANGO_ALIGN_LEFT);

        //fix me... and what about UTF-8 conversion?
        //this could be a massive problem since we
        //will need to know before hand what the current
        //locale is, and if we stored UTF-8 string this would break
        //cross-compatibility with GTK1.2 and win32 interfaces.....

        pango_layout_set_text(Layout, Str, Count);

        aRect := Rect(0,0,0, 0);
        pango_layout_get_pixel_size(Layout, @arect.Right, @arect.Bottom);

        OffsetRect(aRect, X+DCOrigin.X,Y+DCOrigin.Y);

        EnsureGCColor(DC, dccCurrentTextColor, True, False);

        gdk_draw_layout_with_colors(drawable, GC, aRect.Left, aRect.Top, Layout, nil, nil);
        g_object_unref(Layout);
        Result := True;
        If UnRef then
          pango_font_description_free(UseFontDesc);
      end;
    end;
  end;
end;

//##apiwiz##eps##   // Do not remove

{$IfDef ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$EndIf}
