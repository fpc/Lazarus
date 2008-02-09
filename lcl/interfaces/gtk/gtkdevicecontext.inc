{%MainUnit gtkdef.pp}

{******************************************************************************
                                   TGtkDeviceContext
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
{$ENDIF}

{ TDeviceContext }

procedure TGtkDeviceContext.SetClipRegion(const AValue: PGdiObject);
begin
  ChangeGDIObject(fClipRegion, AValue);
end;

function TGtkDeviceContext.GetGDIObjects(ID: TGDIType): PGdiObject;
begin
  case ID of
  gdiBitmap: Result:=CurrentBitmap;
  gdiFont: Result:=CurrentFont;
  gdiBrush: Result:=CurrentBrush;
  gdiPen: Result:=CurrentPen;
  gdiPalette: Result:=CurrentPalette;
  gdiRegion: Result:=ClipRegion;
  end;
end;

{------------------------------------------------------------------------------
  function GetOffset

  Returns the DC offset for the DC Origin.
 ------------------------------------------------------------------------------}
function TGtkDeviceContext.GetOffset: TPoint;
var
  Fixed: Pointer;
  Adjustment: PGtkAdjustment;
begin
  if Self = nil
  then begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;

  Result := FOrigin;
  {$ifndef gtk1}
  if (FWidget <> nil) then
  begin
    Fixed := GetFixedWidget(FWidget);
    if GTK_WIDGET_NO_WINDOW(FWidget) and
       GTK_WIDGET_NO_WINDOW(Fixed) and
       not GtkWidgetIsA(FWidget, GTKAPIWidget_GetType) then
    begin
      Inc(Result.X, FWidget^.Allocation.x);
      Inc(Result.y, FWidget^.Allocation.y);
    end;
  end;
  {$endif}
  
  if not FSpecialOrigin then Exit;
  if FWidget = nil then Exit;
  
  {$ifdef gtk1}
  Fixed := GetFixedWidget(FWidget);
  {$endif}
  if not GtkWidgetIsA(Fixed, GTK_LAYOUT_GET_TYPE) then Exit;
  
  Adjustment := gtk_layout_get_hadjustment(Fixed);
  if Adjustment <> nil
  then Dec(Result.X, Trunc(Adjustment^.Value - Adjustment^.Lower));

  Adjustment := gtk_layout_get_vadjustment(Fixed);
  if Adjustment <> nil
  then Dec(Result.Y, Trunc(Adjustment^.Value-Adjustment^.Lower));
end;

function TGtkDeviceContext.GetOwnedGDIObjects(ID: TGDIType): PGdiObject;
begin
  Result:=fOwnedGDIObjects[ID];
end;

procedure TGtkDeviceContext.SetCurrentBitmap(const AValue: PGdiObject);
begin
  ChangeGDIObject(FCurrentBitmap,AValue);
end;

procedure TGtkDeviceContext.SetCurrentBrush(const AValue: PGdiObject);
begin
  ChangeGDIObject(FCurrentBrush,AValue);
  if FSelectedColors = dcscBrush
  then FSelectedColors := dcscCustom;
end;

procedure TGtkDeviceContext.SetCurrentFont(const AValue: PGdiObject);
begin
  ChangeGDIObject(FCurrentFont,AValue);
end;

procedure TGtkDeviceContext.SetCurrentPalette(const AValue: PGdiObject);
begin
  ChangeGDIObject(FCurrentPalette,AValue);
end;

procedure TGtkDeviceContext.SetCurrentPen(const AValue: PGdiObject);
begin
  ChangeGDIObject(FCurrentPen,AValue);
  if FSelectedColors = dcscPen
  then FSelectedColors := dcscCustom;
end;

procedure TGtkDeviceContext.ChangeGDIObject(var GDIObject: PGdiObject;
  const NewValue: PGdiObject);
begin
  if GdiObject = NewValue then exit;
  if GdiObject <> nil
  then begin
    dec(GdiObject^.DCCount);
    if GdiObject^.DCCount < 0 then
      RaiseGDBException('');
  end;

  GdiObject := NewValue;

  if GdiObject <> nil
  then begin
    inc(GdiObject^.DCCount);
  end;
end;

procedure TGtkDeviceContext.SetGDIObjects(ID: TGDIType; const AValue: PGdiObject);
begin
  case ID of
    gdiBitmap:  ChangeGDIObject(fCurrentBitmap,AValue);
    gdiFont:    ChangeGDIObject(fCurrentFont,AValue);
    gdiBrush:   ChangeGDIObject(fCurrentBrush,AValue);
    gdiPen:     ChangeGDIObject(fCurrentPen,AValue);
    gdiPalette: ChangeGDIObject(fCurrentPalette,AValue);
    gdiRegion:  ChangeGDIObject(fClipRegion,AValue);
  end;
end;

procedure TGtkDeviceContext.SetOwnedGDIObjects(ID: TGDIType;
  const AValue: PGdiObject);
begin
//MWE: this is not right. all objects except bitmaps can be selected in more than one DC

  if fOwnedGDIObjects[ID]=AValue then exit;
  if fOwnedGDIObjects[ID]<>nil then
    fOwnedGDIObjects[ID]^.Owner:=nil;
  fOwnedGDIObjects[ID]:=AValue;
  if fOwnedGDIObjects[ID]<>nil then
    fOwnedGDIObjects[ID]^.Owner:=Self;
end;

procedure TGtkDeviceContext.SetROP2(AROP: Integer);
var
  Func: TGdkFunction;
begin
  case AROP of
    R2_COPYPEN:     Func := GDK_COPY;
    R2_NOT:         Func := GDK_INVERT;
    R2_XORPEN:      Func := GDK_XOR;
    R2_BLACK:       Func := GDK_CLEAR;
    R2_MASKPEN:     Func := GDK_AND;
    R2_MASKPENNOT:  Func := GDK_AND_REVERSE;
    R2_MASKNOTPEN:  Func := GDK_AND_INVERT;
    R2_NOP:         Func := GDK_NOOP;
    R2_MERGEPEN:    Func := GDK_OR;
    R2_NOTXORPEN:   Func := GDK_EQUIV;
    R2_MERGEPENNOT: Func := GDK_OR_REVERSE;
    R2_NOTCOPYPEN:  Func := GDK_COPY_INVERT;
    R2_NOTMASKPEN:  Func := GDK_NAND;
    //R2_NOTMERGEPEN: Func := GDK_NOR;
    R2_WHITE:       Func := GDK_SET;
  else
    Func := GDK_COPY;
  end;
  
  gdk_gc_set_function(GC, Func);
end;

procedure TGtkDeviceContext.SetSelectedColors(AValue: TDevContextSelectedColorsType);
begin
  if FSelectedColors = AValue then Exit;
  FSelectedColors := AValue;
  
  case FSelectedColors of
    dcscPen: SelectPenProps;
    dcscBrush: SelectBrushProps;
    dcscFont: SelectTextProps;
  end;
end;

procedure TGtkDeviceContext.SetTextMetricsValid(AValid: Boolean);
begin
  if AValid
  then Include(FFlags, dcfTextMetricsValid)
  else Exclude(FFlags, dcfTextMetricsValid);
end;

procedure TGtkDeviceContext.SetWidget(AWidget: PGtkWidget; AWindow: PGdkWindow;
                                      AWithChildWindows: Boolean; ADoubleBuffer: PGdkDrawable);

  procedure RaiseWidgetWithoutClientArea;
  begin
    RaiseGDBException('TGtkDeviceContext.SetWidget: widget ' + DbgS(AWidget) + ' has no client area');
  end;

  procedure RaiseWidgetAlreadySet;
  begin
    RaiseGDBException('TGtkDeviceContext.SetWidget: widget already set');
  end;
  
  procedure RaiseUnableToRealize;
  begin
    RaiseGDBException('TGtkDeviceContext.SetWidget: Unable to realize GdkWindow');
  end;
  
var
  ClientWidget: PGtkWidget;
begin
  if FWidget <> nil
  then RaiseWidgetAlreadySet;

  FWithChildWindows := AWithChildWindows;
  FWidget := AWidget;

  if AWidget = nil
  then begin
    // screen: ToDo: multiple desktops
    {$ifdef gtk1}
    FDrawable := @gdk_root_parent;
    {$else}
    FDrawable := gdk_screen_get_root_window(gdk_screen_get_default);
    {$endif}
  end
  else begin
    if ADoubleBuffer <> nil
    then begin
      Include(FFlags, dcfDoubleBuffer);
      FOriginalDrawable := AWindow;
      FDrawable := ADoubleBuffer;
    end
    else begin
      // create a new devicecontext for this window
      Exclude(FFlags, dcfDoubleBuffer);
      
      if AWindow = nil
      then begin
        ClientWidget := GetFixedWidget(AWidget);
        if ClientWidget = nil then RaiseWidgetWithoutClientArea;

        AWindow := GetControlWindow(ClientWidget);
        if AWindow = nil
        then begin
          //force creation
          gtk_widget_realize(ClientWidget);
          AWindow := GetControlWindow(ClientWidget);
          if AWindow = nil then RaiseUnableToRealize;
        end;
      end
      else begin
        ClientWidget := AWidget;
      end;

      FSpecialOrigin := GtkWidgetIsA(ClientWidget, GTK_LAYOUT_GET_TYPE);
      FDrawable := AWindow;

      {$IFDEF Gtk1}
      {$note todo: check if this is still needed} // now gc is a property
      GetGC;
      {$ELSE}
      // GC is created on demand
      {$ENDIF}
    end;
  end;

  gdk_color_black(gdk_colormap_get_system, @CurrentTextColor.Color);
  BuildColorRefFromGDKColor(CurrentTextColor);
  gdk_color_white(gdk_colormap_get_system, @CurrentBackColor.Color);
  BuildColorRefFromGDKColor(CurrentBackColor);


  {$ifdef GTK1}
  GetFont;
  GetBrush;
  GetPen;
  {$else}
  // font, brush, pen are created on demand
  {$endIf}
end;

procedure TGtkDeviceContext.Clear;
var
  g: TGDIType;
  
  procedure WarnOwnedGDIObject;
  begin
    DebugLn(['TDeviceContext.Clear ',dbghex(PtrInt(Self)),' OwnedGDIObjects[',ord(g),']<>nil']);
  end;
  
begin
  FWidget := nil;
  FDrawable := nil;
  FGC := nil;
  FillChar(FGCValues, SizeOf(FGCValues), 0);

  FOrigin.X := 0;
  FOrigin.Y := 0;
  FSpecialOrigin := False;
  PenPos.X:=0;
  PenPos.Y:=0;
  
  CurrentBitmap:=nil;
  CurrentFont:=nil;
  CurrentPen:=nil;
  CurrentBrush:=nil;
  CurrentPalette:=nil;
  ClipRegion:=nil;
  FillChar(CurrentTextColor,SizeOf(CurrentTextColor),0);
  FillChar(CurrentBackColor,SizeOf(CurrentBackColor),0);

  SelectedColors:=dcscCustom;
  SavedContext:=nil;
  FFlags := [];
  
  for g:=Low(TGDIType) to high(TGDIType) do
    if OwnedGDIObjects[g]<>nil then
      WarnOwnedGDIObject;
end;

{------------------------------------------------------------------------------
  Function: CopyData - used by RestoreDC and SaveDC
  Params:  DestinationDC:  a dc to copy data to
           ClearSource: set true to make a move operation
           MoveGDIOwnerShip: set true to pass the ownership of the GDI objects
                             to Destination
  Returns: True if succesful

  Creates a copy DC from the given DC
 ------------------------------------------------------------------------------}
function TGtkDeviceContext.CopyDataFrom(ASource: TGtkDeviceContext; AClearSource, AMoveGDIOwnerShip, ARestore: Boolean): Boolean;
  procedure RaiseRestoreDifferentWidget;
  begin
    RaiseGDBException('TGtkDeviceContext.CopyDataFrom: restore widget differs');
  end;

  procedure RaiseWidgetAlreadySet;
  begin
    RaiseGDBException('TGtkDeviceContext.CopyDataFrom: widget already set');
  end;
  
var
  g: TGDIType;
  CurGDIObject: PGDIObject;
begin
  Result := (Self <> nil) and (ASource <> nil);
  if not Result then Exit;
  
  if ARestore
  then begin
    if FWidget <> ASource.FWidget
    then RaiseRestoreDifferentWidget;
  end
  else begin
    if FWidget <> nil
    then RaiseWidgetAlreadySet;
    FWidget := ASource.FWidget;
  end;

  FWithChildWindows := ASource.FWithChildWindows;
  FDrawable := ASource.FDrawable;
  FOriginalDrawable := ASource.FOriginalDrawable;

  if FGC <> nil
  then begin
    // free old GC
    gdk_gc_unref(FGC);
    FGC := nil;
    Exclude(FFlags, dcfPenSelected);
  end;

  if (ASource.FGC <> nil) and (FDrawable <> nil)
  then begin
    gdk_gc_get_values(ASource.FGC, @FGCValues);
    FGC := gdk_gc_new_with_values(FDrawable, @FGCValues, 3 { $3FF});
    Exclude(FFlags, dcfPenSelected);
  end;

  FOrigin := ASource.FOrigin;
  FSpecialOrigin := ASource.FSpecialOrigin;
  PenPos := ASource.PenPos;

  if dcfTextMetricsValid in ASource.Flags
  then begin
    Include(FFlags, dcfTextMetricsValid);
    DCTextMetric := ASource.DCTextMetric;
  end
  else
    Exclude(FFlags, dcfTextMetricsValid);

  for g:=Low(TGDIType) to High(TGDIType) do
  begin
    GDIObjects[g] := ASource.GDIObjects[g];
    if AClearSource then
      ASource.GDIObjects[g] := nil;

    if AMoveGDIOwnerShip
    then begin
      if OwnedGDIObjects[g]<>nil
      then begin
        DeleteObject(HGDIOBJ(PtrUInt(OwnedGDIObjects[g])));
      end;

      CurGDIObject := ASource.OwnedGDIObjects[g];
      
      if CurGDIObject<>nil
      then begin
        ASource.OwnedGDIObjects[g] := nil;
        OwnedGDIObjects[g] := CurGDIObject;
      end;
    end;
  end;
  CopyGDIColor(ASource.CurrentTextColor, CurrentTextColor);
  CopyGDIColor(ASource.CurrentBackColor, CurrentBackColor);

  SelectedColors := dcscCustom;
  SavedContext := nil;
end;

procedure TGtkDeviceContext.CreateBrush;
begin
  if FCurrentBrush <> nil then Exit;
  CurrentBrush := GtkWidgetset.CreateDefaultBrush;
  OwnedGDIObjects[gdiBrush] := FCurrentBrush;
end;

procedure TGtkDeviceContext.CreateFont;
{$IFDEF Gtk2}
var
  ClientWidget: PGtkWidget;
{$ENDIF}
begin
  if FCurrentFont <> nil then exit;

  // create font
  {$ifdef gtk1}
  if FGCValues.Font <> nil
  then begin
    CurrentFont := GtkWidgetset.NewGDIObject(gdiFont);
    FCurrentFont^.GDIFontObject := FGCValues.Font;
    FontCache.Reference(FCurrentFont^.GDIFontObject);
  end
  else
    CurrentFont := GtkWidgetset.CreateDefaultFont;
    
  {$else}
  if FWidget <> nil
  then begin
    ClientWidget := GetFixedWidget(FWidget);

    CurrentFont := GtkWidgetset.NewGDIObject(gdiFont);
    FCurrentFont^.GDIFontObject := gtk_widget_create_pango_layout(ClientWidget, nil);

    {$ifdef fontconsistencychecks}
    if FontCache.FindGTKFont(FCurrentFont^.GDIFontObject) <> nil then
      RaiseGDBException('inconsistency: font already in cache, maybe freed, but not removed from cache');
    {$endif}

    FontCache.AddWithoutName(FCurrentFont^.GDIFontObject);

    {$ifdef fontconsistencychecks}
    // MWE: are we paranoid or so ? (if you can't trust the cache, don't use it or stop coding)
    // MG: some people are coding without knowing about the cache
    if FontCache.FindGTKFont(FCurrentFont^.GDIFontObject) = nil then
      RaiseGDBException('inconsistency: font added to cache, but can not be found');
    {$endif}
  end
  else
    CurrentFont := GtkWidgetset.CreateDefaultFont;
  {$endif}
  OwnedGDIObjects[gdiFont] := FCurrentFont;
end;

function TGtkDeviceContext.CreateGC: PGdkGC;
{$IFDEF Gtk1}
var
  CurWidget: PGtkWidget;
  CurWindow: PGdkWindow;
{$ENDIF}
begin
  // create GC

  if Drawable <> nil
  then begin
    if FWithChildWindows
    then begin
      FillChar(FGCValues, SizeOf(FGCValues), 0);
      FGCValues.subwindow_mode := GDK_INCLUDE_INFERIORS;
      Result := gdk_gc_new_with_values(Drawable, @FGCValues, GDK_GC_FUNCTION or GDK_GC_SUBWINDOW);
    end
    else begin
      Result := gdk_gc_new(Drawable);
    end;
  end
  else begin
    // create default GC
    {$IFDEF Gtk1}
    CurWidget := GetStyleWidget(lgsWindow);
    CurWindow := CurWidget^.window;
    Result := gdk_gc_new(CurWindow);
    {$ELSE}
    Result := gdk_gc_new(gdk_screen_get_root_window(gdk_screen_get_default));
    {$ENDIF}
  end;
  if Result = nil then Exit;

  gdk_gc_set_function(Result, GDK_COPY);
  gdk_gc_get_values(Result, @FGCValues);
end;

procedure TGtkDeviceContext.CreateBitmap;
begin
  if FCurrentBitmap <> nil then Exit;
  CurrentBitmap := GTKWidgetset.CreateDefaultGDIBitmap;
  OwnedGDIObjects[gdiBitmap] := FCurrentBitmap;
end;

procedure TGtkDeviceContext.CreateGDIObject(AGDIType: TGDIType);
begin
  case AGDIType of
    gdiFont: CreateFont;
    gdiBrush: CreateBrush;
    gdiPen: CreatePen;
    gdiBitmap: CreateBitmap;
  else
    RaiseGDBException('TGtkDeviceContext.CreateGDIObject');
  end;
end;

procedure TGtkDeviceContext.CreatePen;
begin
  if FCurrentPen <> nil then exit;
  CurrentPen := GtkWidgetSet.CreateDefaultPen;
  OwnedGDIObjects[gdiPen] := FCurrentPen;
end;


function TGtkDeviceContext.GetGC: pgdkGC;
begin
  if FGC = nil
  then FGC := CreateGC;
  Result := FGC;
end;

function TGtkDeviceContext.GetFont: PGdiObject;
begin
  if FCurrentFont = nil
  then CreateFont;

  Result := FCurrentFont;
end;

function TGtkDeviceContext.GetBrush: PGdiObject;
begin
  if FCurrentBrush = nil
  then CreateBrush;

  Result := FCurrentBrush;
end;

function TGtkDeviceContext.GetPen: PGdiObject;
begin
  if FCurrentPen = nil
  then CreatePen;
  
  Result := FCurrentPen;
end;

function TGtkDeviceContext.GetROP2: Integer;
begin
  case GetFunction of
    GDK_COPY:         result := R2_COPYPEN;
    GDK_INVERT:       result := R2_NOT;
    GDK_XOR:          result := R2_XORPEN;
    GDK_CLEAR:        result := R2_BLACK;
    GDK_AND:          result := R2_MASKPEN;
    GDK_AND_REVERSE:  result := R2_MASKPENNOT;
    GDK_AND_INVERT:   result := R2_MASKNOTPEN;
    GDK_NOOP:         result := R2_NOP;
    GDK_OR:           result := R2_MERGEPEN;
    GDK_EQUIV:        result := R2_NOTXORPEN;
    GDK_OR_REVERSE:   result := R2_MERGEPENNOT;
    GDK_COPY_INVERT:  result := R2_NOTCOPYPEN;
    GDK_NAND:         result := R2_NOTMASKPEN;
    //GDK_NOR:          result := R2_NOTMERGEPEN;
    GDK_SET:          result := R2_WHITE;
  else
    result := R2_COPYPEN;
  end;
end;

function TGtkDeviceContext.HasGC: Boolean;
begin
  Result := FGC <> nil;
end;

function TGtkDeviceContext.IsNullBrush: boolean;
begin
  Result := (FCurrentBrush <> nil) and (FCurrentBrush^.IsNullBrush);
end;


function TGtkDeviceContext.IsNullPen: boolean;
begin
  Result := (FCurrentPen <> nil) and (FCurrentPen^.IsNullPen);
end;

procedure TGtkDeviceContext.ResetGCClipping;
begin
  if FGC = nil then Exit;

  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$endif}
  gdk_gc_set_clip_mask(FGC, nil);
  gdk_gc_set_clip_origin (FGC, 0,0);
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$endif}

  SelectRegion;
end;

function TGtkDeviceContext.SelectBitmap(AGdiObject: PGdiObject): PGdiObject;
var
  NewDrawable: PGdkPixmap;
begin
  // always create, because a valid GDIObject is needed to restore
  Result := GetBitmap;
  if CurrentBitmap = AGDIObject then Exit;

  CurrentBitmap := AGDIObject;
  with FCurrentBitmap^ do
    case GDIBitmapType of
      gbPixmap: NewDrawable := GDIPixmapObject.Image;
      gbBitmap: NewDrawable := GDIBitmapObject;
    else
      DebugLn('[TGtkDeviceContext.SelectBitmap] - Unknown bitmaptype, DC=0x%p', [Pointer(Self)]);
      Exit;
    end;

  // no drawable: this is normal, when restoring the default bitmap (FreeDC)
  if NewDrawable = nil then Exit;
  

  if FGC <> nil
  then gdk_gc_unref(FGC);
  FDrawable := NewDrawable;
  FGC := gdk_gc_new(FDrawable);
  gdk_gc_set_function(FGC, GDK_COPY);
  SelectedColors := dcscCustom;
end;

{------------------------------------------------------------------------------
  Procedure: TGtkDeviceContext.SelectBrushProps
  Params:
  Returns: Nothing

  Sets the forecolor and fill according to the brush
 ------------------------------------------------------------------------------}
procedure TGtkDeviceContext.SelectBrushProps;
begin
  if IsNullBrush then Exit;
  
  // Force brush
  GetBrush;

  EnsureGCColor(HDC(Self), dccCurrentBackColor, True, True);//BKColor
  EnsureGCColor(HDC(Self), dccGDIBrushColor, CurrentBrush^.GDIBrushFill = GDK_Solid, False);//Brush Color

  if CurrentBrush^.GDIBrushFill = GDK_Solid then Exit;
  if CurrentBrush^.GDIBrushPixmap = nil then Exit;

  gdk_gc_set_fill(GC, CurrentBrush^.GDIBrushFill);
  if CurrentBrush^.GDIBrushFill = GDK_STIPPLED
  then gdk_gc_set_stipple(GC, CurrentBrush^.GDIBrushPixmap)
  else gdk_gc_set_tile(GC, CurrentBrush^.GDIBrushPixmap);

  gdk_gc_get_values(GC, @FGCValues);
end;

function TGtkDeviceContext.SelectObject(AGdiObject: PGdiObject): PGdiObject;
begin
  case AGdiObject^.GDIType of
    gdiBitmap: Result := SelectBitmap(AGdiObject);
    gdiPen:    Result := SelectPen(AGdiObject);
  else
    // we only handle bitmaps here atm
    Result := PGdiObject(GTKWidgetSet.SelectObject(HDC(Self), HGDIOBJ(AGdiObject)));
  end;
end;

function TGtkDeviceContext.SelectPen(AGdiObject: PGdiObject): PGdiObject;
begin
  Result := GetPen;// always create, because a valid GDIObject is needed to restore
  if CurrentPen = AGDIObject then Exit;

  CurrentPen := AGDIObject;
  Exclude(FFlags, dcfPenSelected);
  if FGC <> nil
  then SelectPenProps;
  SelectedColors := dcscCustom;
end;

{------------------------------------------------------------------------------
  Procedure: TGtkDeviceContext.SelectPenProps
  Params:  DC: a (LCL)devicecontext
  Returns: Nothing

  Sets the forecolor and fill according to the pen
 ------------------------------------------------------------------------------}
procedure TGtkDeviceContext.SelectPenProps;
const
{$ifdef windows}
  OS_multiplier = 1;
{$else}
  OS_multiplier = 3;
{$endif}

  procedure SetDashes(const ADashes: array of gint8);
  begin
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    laz_gdk_gc_set_dashes(GC, 0, @ADashes[0], Length(ADashes));
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  end;

begin
//  if IsNullPen then Exit;

  EnsureGCColor(HDC(Self), dccCurrentBackColor, True, True);//BKColor
  EnsureGCColor(HDC(Self), dccGDIPenColor, False, False);//Pen Color

  if dcfPenSelected in FFlags then Exit;
  Exclude(FFlags, dcfPenInvalid);
  if GC = nil then Exit;

  // force pen
  GetPen;

  CurrentPen^.IsNullPen := CurrentPen^.GDIPenStyle = PS_NULL;
  
  if (CurrentPen^.GDIPenStyle = PS_SOLID)
  or (CurrentPen^.GDIPenStyle = PS_INSIDEFRAME)
  then begin
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    gdk_gc_set_line_attributes(GC, CurrentPen^.GDIPenWidth, GDK_LINE_SOLID, GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  end
  else begin
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    gdk_gc_set_line_attributes(GC, CurrentPen^.GDIPenWidth, GDK_LINE_ON_OFF_DASH, GDK_CAP_NOT_LAST, GDK_JOIN_MITER);

    // Paul Ishenin: I comparet patterns with windows and changed numbers to make them the same
    // but under linux dot is thinner than in windows, so I added os_multiplier to make them the same
    // in resulting image

    case CurrentPen^.GDIPenStyle of
      PS_DASH:       SetDashes([6*OS_multiplier,2*OS_multiplier]);
      PS_DOT:        SetDashes([1*OS_multiplier,1*OS_multiplier]);
      PS_DASHDOT:    SetDashes([3*OS_multiplier,2*OS_multiplier,1*OS_multiplier,2*OS_multiplier]);
      PS_DASHDOTDOT: SetDashes([3*OS_multiplier,1*OS_multiplier,1*OS_multiplier,1*OS_multiplier,1*OS_multiplier,1*OS_multiplier]);
      //This is DEADLY!!!
      //PS_NULL:       gdk_gc_set_dashes(GC, 0, [0,4], 2);
    end;
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  end;
  gdk_gc_get_values(GC,@FGCValues);
  Include(FFlags, dcfPenSelected);
end;

{------------------------------------------------------------------------------
  Procedure SelectRegion

  Applies the current clipping region of the DC (DeviceContext) to the
  gc (GDK Graphic context - pgdkGC)
 ------------------------------------------------------------------------------}
procedure TGtkDeviceContext.SelectRegion;
var
  RGNType : Longint;
begin
  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}

  // force GC
  GetGC;
  
  // Clear
  gdk_gc_set_clip_region(FGC,  nil);
  gdk_gc_set_clip_rectangle(FGC,  nil);

  if ClipRegion <> nil
  then begin
    RGNType := RegionType(ClipRegion^.GDIRegionObject);
    if (RGNType <> ERROR) and (RGNType <> NULLREGION)
    then gdk_gc_set_clip_region(FGC,  ClipRegion^.GDIRegionObject);
  end;

  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
end;

{------------------------------------------------------------------------------
  Procedure: TGtkDeviceContext.SelectTextProps
  Params:
  Returns: Nothing

  Sets the forecolor and fill according to the Textcolor
 ------------------------------------------------------------------------------}
procedure TGtkDeviceContext.SelectTextProps;
begin
  EnsureGCColor(HDC(Self), dccCurrentBackColor, True, True);//BKColor
  EnsureGCColor(HDC(Self), dccCurrentTextColor, False, False);//Font Color
end;

function TGtkDeviceContext.GetBitmap: PGdiObject;
begin
  if FCurrentBitmap = nil
  then CreateBitmap;

  Result := FCurrentBitmap;
end;

