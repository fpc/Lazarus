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
  Control: TWinControl;
begin
  result := Inherited BeginPaint(Handle, PS);
  
  if Handle <> 0 then Control := TWinControl(GetLCLObject(Pointer(Handle)))
  else Control := nil;

  If (Control <> nil) and (not GTK_WIDGET_DOUBLE_BUFFERED((PGTKWidget(Handle)))) and (Control.DoubleBuffered) then
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

    with LogFont do begin
      if lfFaceName[0] = #0
      then begin
        Assert(false,'ERROR: [Tgt2kObject.CreateFontIndirectEx] No fontname');
        Exit;
      end;
      
      if (lfHeight=0) and (CompareText(lfFacename,'default')=0) then begin
        {$IFDEF VerboseFonts}
        DebugLn(['TGtk2WidgetSet.CreateFontIndirectEx Creating default font']);
        {$ENDIF}
        GdiObject:=CreateDefaultFont;
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
      else
        FullString:=IntToStr(aSize);
      FullString := AFamily + ' ' + aStyle + ' ' + FullString;

      PangoDesc := pango_font_description_from_string(PChar(FullString));

      if lfWeight <> FW_DONTCARE then
        pango_font_description_set_weight(PangoDesc,lfWeight);

      if lfItalic <> 0 then
        pango_font_description_set_style(PangoDesc,PANGO_STYLE_ITALIC);

      if (aSize=0) and (lfHeight<>0) then begin
        // a size is not specified, try to calculate one based on lfHeight
        // and use this value not in the font name but set this value appart
        // NOTE: in gtk2.8 is possible to use pango_font_description_set_absolute_size
        // which would be great with the given lfheight value, but older gtk2 version
        // doesn't have this funtion
        if lfHeight<0 then
          aSize:= (abs(lfheight) * 72 * PANGO_SCALE) div ScreenInfo.PixelsPerInchX
        else
          aSize:=lfHeight*PANGO_SCALE;
        pango_font_description_set_size(PangoDesc, aSize);
      end;
      
      // create font
      // TODO: use context widget (CreateFontIndirectEx needs a parameter for this: Context: HWnd)
      GdiObject := NewGdiObject(gdiFont);
      GdiObject^.GDIFontObject:=gtk_widget_create_pango_layout(
                                               GetStyleWidget(lgsdefault), nil);
      CurFont:=GdiObject^.GDIFontObject;

      pango_layout_set_font_description(CurFont,PangoDesc);

      if (LogFont.lfUnderline<>0) or (LogFont.lfStrikeOut<>0) then begin
        AttrListTemporary := false;
        AttrList := pango_layout_get_attributes(CurFont);
        if (AttrList = nil) then begin
          AttrList := pango_attr_list_new();
          AttrListTemporary := true;
        end;
        if LogFont.lfUnderline<>0 then
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_SINGLE)
        else
          Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);
        pango_attr_list_change(AttrList,Attr);

        Attr := pango_attr_strikethrough_new(LogFont.lfStrikeOut<>0);
        pango_attr_list_change(AttrList,Attr);

        if AttrListTemporary then
          pango_attr_list_unref(AttrList);
      end;

      pango_layout_set_single_paragraph_mode(CurFont, TRUE);
      pango_layout_set_width(CurFont, -1);
      pango_layout_set_alignment(CurFont, PANGO_ALIGN_LEFT);
    end;
  finally
    if (CachedFont=nil)
    and (GdiObject<>nil) and (GdiObject^.GDIFontObject <> nil) then begin
      // add to cache
      CachedFont:=FontCache.Add(GdiObject^.GDIFontObject,LogFont,LongFontName);
      if CachedFont<>nil then begin
        CachedFont.PangoFontDescription:=PangoDesc;
        PangoDesc:=nil;
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
        Result := HFONT(GdiObject);
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


{------------------------------------------------------------------------------
  Function: EndPaint
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
 
Function TGtk2WidgetSet.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
var
  Control: TWinControl;
begin

  if Handle <> 0 then Control := TWinControl(GetLCLObject(Pointer(Handle)))
  else Control := nil;

  If (Control <> nil) and (not GTK_WIDGET_DOUBLE_BUFFERED((PGTKWidget(Handle)))) and (Control.DoubleBuffered) then
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
  TopY, LineLen, LineHeight: Integer;
  TxtPt: TPoint;
  DCOrigin: TPoint;
  Foreground: PGDKColor;
  UseFont: PPangoLayout;
  CurDx: PInteger;
  CurStr: PChar;

  procedure DoTextOut(X,Y : Integer; Str: Pchar; CurCount: Integer);
  var
    DevCtx: TDeviceContext;
    CurScreenX: LongInt;
    CharLen: LongInt;
  begin
    DevCtx:=TDeviceContext(DC);
    if (Dx<>nil) then begin
      CurScreenX:=X;
      while CurCount>0 do begin
        CharLen:=UTF8CharacterLength(CurStr);
        //gdk_draw_glyphs(DevCtx.drawable,DevCtx.gc );
        pango_layout_set_text(UseFont, CurStr, CharLen);
        gdk_draw_layout_with_colors(DevCtx.drawable, DevCtx.gc, CurScreenX, Y,
                                    UseFont, Foreground, nil);
        //gdk_draw_rectangle(DevCtx.Drawable,DevCtx.GC,1,CurScreenX,Y,3,3);
        inc(CurScreenX,CurDx^);
        inc(CurDx);
        inc(CurStr,CharLen);
        dec(CurCount,CharLen);
      end;
    end else begin
      pango_layout_set_text(UseFont, Str, Count);
      gdk_draw_layout_with_colors(DevCtx.drawable, DevCtx.gc, X, Y, UseFont,
                                  Foreground, nil);
    end;
  end;

begin
  //DebugLn(['TGtk2WidgetSet.ExtTextOut X=',X,' Y=',Y,' Str="',copy(Str,1,Count),'" Count=',Count,' DX=',dbgs(DX)]);
  Assert(False, Format('trace:> [TGtk2WidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := IsValidDC(DC);
  if Result
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Uninitialized GC');
      Result := False;
      exit;
    end;
    if ((Options and (ETO_OPAQUE+ETO_CLIPPED)) <> 0)
    and (Rect=nil) then begin
      DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Rect=nil');
      Result := False;
      exit;
    end;

    UseFont:=nil;
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil) then begin
      UseFont := GetDefaultGtkFont(false);
    end else begin
      UseFont := CurrentFont^.GDIFontObject;
    end;
    
    if (UseFont = nil) then begin
      DebugLn('WARNING: [TGtk2WidgetSet.ExtTextOut] Missing Font');
      Result:=false;
      exit;
    end;

    // to reduce flickering calculate first and then paint
    
    DCOrigin:=GetDCOffset(TDeviceContext(DC));

    if (Options and ETO_CLIPPED) <> 0 then begin
      X := Rect^.Left;
      Y := Rect^.Top;
      IntersectClipRect(DC, Rect^.Left, Rect^.Top,
                        Rect^.Right, Rect^.Bottom);
    end;

    LineLen := FindLineLen(Str,Count);
    TopY := Y;
    UpdateDCTextMetric(TDeviceContext(DC));
    TxtPt.X := X + DCOrigin.X;
    LineHeight := DCTextMetric.TextMetric.tmHeight;
    TxtPt.Y := TopY + DCOrigin.Y;

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
    Foreground := nil;//StyleForegroundColor(CurrentTextColor.ColorRef, nil);

    CurDx:=Dx;
    CurStr:=Str;
    LineStart:=Str;
    if LineLen < 0 then begin
      LineLen:=Count;
      if Count> 0 then
        DoTextOut(TxtPt.X, TxtPt.Y, LineStart, LineLen);
    end else
    Begin  //write multiple lines
      StrEnd:=Str+Count;
      while LineStart < StrEnd do begin
        LineEnd:=LineStart+LineLen;
        if LineLen>0 then
          DoTextOut(TxtPt.X, TxtPt.Y, LineStart, LineLen);
        inc(TxtPt.Y,LineHeight);
        LineStart:=LineEnd+1; // skip #13
        if (LineStart<StrEnd) and (LineStart^ in [#10,#13])
        and (LineStart^ <> LineEnd^) then
          inc(LineStart); // skip #10
        Count:=StrEnd-LineStart;
        LineLen:=FindLineLen(LineStart,Count);
        if LineLen<0 then
          LineLen:=Count;
      end;
    end;

    Result := True;
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
    root, child: TWindow;
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
  if Result and (Count>0)
  then with TDeviceContext(DC) do
  begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      UseFont := GetDefaultGtkFont(false);
    end
    else begin
      UseFont := CurrentFont^.GDIFontObject;
    end;

    if UseFont = nil then begin
      DebugLn('WARNING: [TGtk2WidgetSet.GetTextExtentPoint] Missing Font');
      Result:=false;
      exit;
    end;

    UpdateDCTextMetric(TDeviceContext(DC));

    pango_layout_set_text(UseFont, Str, Count);
    pango_layout_get_pixel_size(UseFont, @Size.cX, @Size.cY);
    //DebugLn(['TGtk2WidgetSet.GetTextExtentPoint Str="',copy(Str,1,Count),' Count=',Count,' X=',Size.cx,' Y=',Size.cY]);

    Result := True;
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
  yOffset: integer;
  UseFont: PPangoLayout;
begin
  Result := IsValidDC(DC);
  if Result and (Count>0)
  then with TDeviceContext(DC) do
  begin
    if GC = nil
    then begin
      DebugLn('WARNING: [TGtk2WidgetSet.TextOut] Uninitialized GC');
      exit(false);
    end;
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      UseFont := GetDefaultGtkFont(false);
    end
    else begin
      UseFont := CurrentFont^.GDIFontObject;
    end;

    If UseFont = nil then begin
      DebugLn('WARNING: [TGtk2WidgetSet.TextOut] Missing Font');
      exit(false);
    end;
    UpdateDCTextMetric(TDeviceContext(DC));

    DCOrigin:=GetDCOffset(TDeviceContext(DC));
    with DCTextMetric.TextMetric do
      yOffset:= tmHeight-tmDescent-tmAscent;
    if yOffset<0 then yOffset:=0;
    
    SelectedColors := dcscCustom;
    EnsureGCColor(DC, dccCurrentTextColor, True, False);

    pango_layout_set_text(UseFont, Str, Count);
    EnsureGCColor(DC, dccCurrentTextColor, True, False);

    //DebugLn(['TGtk2WidgetSet.TextOut Str="',copy(Str,1,Count),'" X=',X+DCOrigin.X,',',Y+DCOrigin.Y+yOffset]);
    gdk_draw_layout_with_colors(drawable, GC,
                        X+DCOrigin.X, Y+DCOrigin.Y+yOffset, UseFont, nil, nil);

    Result := True;
  end;
end;

//##apiwiz##eps##   // Do not remove

{$IfDef ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$EndIf}
