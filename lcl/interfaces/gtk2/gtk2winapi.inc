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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  FontNameRegistry, Foundry, FamilyName, WeightName,
  Slant, SetwidthName, AddStyleName, PixelSize,
  PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
  CharSetRegistry, CharSetCoding: string;
  FullString : AnsiString;

  procedure LoadDefaultFont;
  begin
    DisposeGDIObject(GdiObject);
    GdiObject:=CreateDefaultFont;
  end;

begin
  Result := 0;
  GDIObject := NewGDIObject(gdiFont);
  Try
    // set default values
    FontNameRegistry := '*';
    Foundry := '*';
    FamilyName := '*';
    WeightName := '*';
    Slant := '*';
    SetwidthName := '*';
    AddStyleName := '*';
    PixelSize := '*';
    PointSize := '*';
    ResolutionX := '*';
    ResolutionY := '*';
    Spacing := '*';
    AverageWidth := '*';
    CharSetRegistry := '*';
    CharSetCoding := '*';

    // check if LongFontName is in XLFD format
    if IsFontNameXLogicalFontDesc(LongFontName) then begin
      FontNameRegistry := ExtractXLFDItem(LongFontName,0);
      Foundry          := ExtractXLFDItem(LongFontName,1);
      FamilyName       := ExtractXLFDItem(LongFontName,2);
      WeightName       := ExtractXLFDItem(LongFontName,3);
      Slant            := ExtractXLFDItem(LongFontName,4);
      SetwidthName     := ExtractXLFDItem(LongFontName,5);
      AddStyleName     := ExtractXLFDItem(LongFontName,6);
      PixelSize        := ExtractXLFDItem(LongFontName,7);
      PointSize        := ExtractXLFDItem(LongFontName,8);
      ResolutionX      := ExtractXLFDItem(LongFontName,9);
      ResolutionY      := ExtractXLFDItem(LongFontName,10);
      Spacing          := ExtractXLFDItem(LongFontName,11);
      AverageWidth     := ExtractXLFDItem(LongFontName,12);
      CharSetRegistry  := ExtractXLFDItem(LongFontName,13);
      CharSetCoding    := ExtractXLFDItem(LongFontName,14);
    end else
      if (LongFontName <> '') {and (Screen.Fonts.IndexOf(LongFontName) > 0) }then
        FamilyName := LongFontName;

    with LogFont do begin
      if lfFaceName[0] = #0
      then begin
        Assert(false,'ERROR: [Tgt2kObject.CreateFontIndirectEx] No fontname');
        Exit;
      end;

      if (FamilyName = '') or (CompareText(FamilyName,'*')=0) then begin
        FamilyName := StrPas(lfFaceName);
        if CompareText(FamilyName,'default')=0 then begin
          LoadDefaultFont;
          exit;
        end;
        FullString := FamilyName;
        if IsFontNameXLogicalFontDesc(LongFontName) then
          if Abs(lfHeight)=0 then
             FullString := FullString + ' 12'
          else
            FullString := FullString + ' ' + IntToStr(Abs(lfHeight));
      end
      else begin
        FullString := FamilyName;
        if IsFontNameXLogicalFontDesc(LongFontName) then
          if (PointSize = '') or (CompareText(PointSize,'*')=0) then
            FullString := FullString + ' 12'
          else
            FullString := FullString + ' ' + IntToStr(StrToInt(PointSize) div 10);
      end;
      
      if FontNameRegistry='' then ;
      if Foundry='' then ;
      if WeightName='' then ;
      if Slant='' then ;
      if SetwidthName='' then ;
      if AddStyleName='' then ;
      if PixelSize='' then ;
      if ResolutionX='' then ;
      if ResolutionY='' then ;
      if Spacing='' then ;
      if AverageWidth='' then ;
      if CharSetCoding='' then ;
      if CharSetRegistry='' then ;

      GdiObject^.GDIFontObject :=
                          pango_font_description_from_string(PChar(FullString));
      If lfWeight <> FW_DONTCARE then
        pango_font_description_set_weight(GdiObject^.GDIFontObject, lfWeight);

      if lfItalic = 0 then
        pango_font_description_set_style(GdiObject^.GDIFontObject,
                                         PANGO_STYLE_NORMAL)
      else
        pango_font_description_set_style(GdiObject^.GDIFontObject,
                                         PANGO_STYLE_ITALIC);

      GdiObject^.StrikeOut := lfStrikeOut <> 0;
      GdiObject^.Underline := lfUnderline <> 0;

      Result := HFONT(GdiObject);
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

  RGBColor : Longint;
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
      
      GetStyle(lgsdefault);
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
          LineHeight := DCTextMetric.TextMetric.tmAscent;
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
          RGBColor := ColorToRGB(CurrentTextColor.ColorRef);

          Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                            gushort(GetGValue(RGBColor)) shl 8,
                                            gushort(GetBValue(RGBColor)) shl 8);

          pango_attr_list_change(AttrList,Attr);

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

  RGBColor : Longint;

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

        RGBColor := ColorToRGB(CurrentTextColor.ColorRef);

        Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                        gushort(GetGValue(RGBColor)) shl 8,
                                        gushort(GetBValue(RGBColor)) shl 8);

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

  RGBColor : Longint;
  Foreground : PGDKColor;
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
        EnsureGCColor(DC, dccCurrentBackColor, True, False);
        EnsureGCColor(DC, dccCurrentTextColor, True, False);

        RGBColor := ColorToRGB(CurrentTextColor.ColorRef);

        Attr := pango_attr_foreground_new(gushort(GetRValue(RGBColor)) shl 8,
                                        gushort(GetGValue(RGBColor)) shl 8,
                                        gushort(GetBValue(RGBColor)) shl 8);

        pango_attr_list_change(AttrList,Attr);
        pango_layout_set_attributes(Layout, AttrList);

        Foreground := StyleForegroundColor(CurrentTextColor.ColorRef, nil);

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
        FillRect(DC,aRect,hBrush(CurrentBrush));

        gdk_draw_layout_with_colors(drawable, gc, aRect.Left, aRect.Top, Layout, Foreground, nil);
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

{ =============================================================================

  $Log$
  Revision 1.14  2005/02/23 01:12:47  marc
  + Added RemoveProp winapi call
  * Some maintenace on winapi/lclintf files

  Revision 1.13  2005/02/16 23:56:51  mattias
  fixed some gtk2 intf mem leaks, clean ups

  Revision 1.12  2005/01/23 11:43:38  mattias
  fixed expandingtabs in synedit painting

  Revision 1.11  2004/11/08 19:11:55  mattias
  disabled hardly used gtk FillScreenFont, this should be only done on demand, improved getting default font family for gtk

  Revision 1.10  2004/08/04 15:48:44  mazen
  * fix UTF8 text display in source note book

  Revision 1.9  2004/05/11 12:16:48  mattias
  replaced writeln by debugln

  Revision 1.8  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.7  2004/03/05 00:41:15  marc
  * Renamed TGtk2Object to TGtk2WidgetSet

  Revision 1.6  2004/02/04 12:48:17  mattias
  added CLX colors

  Revision 1.5  2003/12/26 10:16:54  mattias
  changed TColorRef from longword to longint

  Revision 1.4  2003/10/15 20:33:37  ajgenius
  add csForm, start fixing Style matching for syscolors and fonts

  Revision 1.3  2003/10/03 04:01:22  ajgenius
  minor pango/gtk2 font fixes

  Revision 1.2  2003/10/02 03:35:29  ajgenius
  more fixes for GTK2, synedit now mostly-useable

  Revision 1.1  2003/09/22 20:08:56  ajgenius
  break GTK2 object and winapi into includes like the GTK interface


}
