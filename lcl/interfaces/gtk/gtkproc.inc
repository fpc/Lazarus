// included by gtkproc.pp

{******************************************************************************
                        Misc Support Functs  
 ******************************************************************************
   used by:
     GTKObject
     GTKWinAPI
     GTKCallback
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
{$ENDIF}

function gtk_widget_get_xthickness(Style : PGTKStyle) : gint;
begin  
  result := Style^.{$IfNDef GTK2}klass^.{$EndIF}xthickness;
end;

function gtk_widget_get_ythickness(Style : PGTKStyle) : gint;
begin  
  result := Style^.{$IfNDef GTK2}klass^.{$EndIF}ythickness;
end;

{------------------------------------------------------------------------------
  procedure RaiseException(const Msg: string);

  Raises an exception.
  gdb does not catch fpc Exception objects, therefore this procedure raises
  a standard AV which is catched by gdb.
 ------------------------------------------------------------------------------}
procedure RaiseException(const Msg: string);
begin
  writeln('ERROR in gtk-interface: ',Msg);
  // creates an exception, that gdb catches:
  writeln('Creating gdb catchable error:');
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

{------------------------------------------------------------------------------
  function CreatePChar(const s: string): PChar;

  Allocates a new PChar
 ------------------------------------------------------------------------------}
function CreatePChar(const s: string): PChar;
begin
  Result:=StrAlloc(length(s) + 1);
  StrPCopy(Result, s);
end;

{------------------------------------------------------------------------------
  function ComparePChar(P1, P2: PChar): boolean;

  Checks if P1 and P2 have the same content.
 ------------------------------------------------------------------------------}
function ComparePChar(P1, P2: PChar): boolean;
begin
  if (P1<>P2) then begin
    if (P1<>nil) and (P2<>nil) then begin
      while (P1^=P2^) do begin
        if P1^<>#0 then begin
          inc(P1);
          inc(P2);
        end else begin
          Result:=true;
          exit;
        end;
      end;
    end;
    Result:=false;
  end else begin
    Result:=true;
  end;
end;

{------------------------------------------------------------------------------
  function FindChar(c: char; p:PChar; Max: integer): integer;
 ------------------------------------------------------------------------------}
function FindChar(c: char; p:PChar; Max: integer): integer;
begin
  Result:=0;
  while (Result<Max) do begin
    if p[Result]<>c then
      inc(Result)
    else
      exit;
  end;
  Result:=-1;
end;


{------------------------------------------------------------------------------
  function GtkWidgetIsA(Widget: PGtkWidget; AType: TGtkType): boolean;

  The GTK_IS_XXX macro functions in the fpc gtk1.x bindings are not correct.
  They just test the highest level.
  This function checks as the real C macros.
 ------------------------------------------------------------------------------}
function GtkWidgetIsA(Widget: PGtkWidget; AType: TGtkType): boolean;
begin
  {$IFDEF GTK2}
  Result:=(Widget<>nil)
    and (PGtkTypeObject(Widget)^.g_class<>nil)
    and gtk_type_is_a(PGtkTypeClass(PGtkTypeObject(Widget)^.g_class)^.g_type,
                      AType);
  {$ELSE}
  Result:=(Widget<>nil)
    and (PGtkTypeObject(Widget)^.klass<>nil)
    and gtk_type_is_a(PGtkTypeClass(PGtkTypeObject(Widget)^.klass)^.thetype,
                      AType);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function WidgetIsDestroyingHandle(Widget: PGtkWidget): boolean;

  Tests if Destruction Mark is set.
 ------------------------------------------------------------------------------}
function WidgetIsDestroyingHandle(Widget: PGtkWidget): boolean;
begin
  Result:=gtk_object_get_data(PGtkObject(Widget),'LCLDestroyingHandle')<>nil;
end;

{------------------------------------------------------------------------------
  procedure SetWidgetIsDestroyingHandle(Widget: PGtkWidget);

  Marks widget for destruction.
 ------------------------------------------------------------------------------}
procedure SetWidgetIsDestroyingHandle(Widget: PGtkWidget);
begin
  gtk_object_set_data(PGtkObject(Widget),'LCLDestroyingHandle',Widget);
end;

{------------------------------------------------------------------------------
  function ComponentIsDestroyingHandle(AWinControl: TWinControl): boolean;

  Tests if Destruction Mark is set.
 ------------------------------------------------------------------------------}
function ComponentIsDestroyingHandle(AWinControl: TWinControl): boolean;
begin
  Result:=
    (AWinControl<>nil) and (AWinControl is TWinControl)
    and (AWinControl.HandleAllocated)
    and WidgetIsDestroyingHandle(PGtkWidget(AWinControl.Handle));
end;

{------------------------------------------------------------------------------
  function LockOnChange(GtkObject: PGtkObject; LockOffset: integer): integer;

  Adds LockOffset to the OnChangeLock and returns the result.
 ------------------------------------------------------------------------------}
function LockOnChange(GtkObject: PGtkObject; LockOffset: integer): integer;
begin
  Result:=Integer(gtk_object_get_data(GtkObject,'OnChangeLock'));
  if LockOffset<>0 then begin
    inc(Result,LockOffset);
    gtk_object_set_data(GtkObject,'OnChangeLock',Pointer(Result));
  end;
end;

{------------------------------------------------------------------------------
  procedure SetComboBoxText(ComboWidget: PGtkCombo; const NewText: string);

  Sets the text of the combobox entry.
 ------------------------------------------------------------------------------}
procedure SetComboBoxText(ComboWidget: PGtkCombo; NewText: PChar);
begin
  //writeln('SetComboBoxText ',HexStr(Cardinal(ComboWidget),8),' "',NewText,'"');
  // lock combobox, so that no OnChange event is fired
  LockOnChange(PGtkObject(ComboWidget^.entry),+1);
  // set text
  if NewText <> nil then
    gtk_entry_set_text(PGtkEntry(ComboWidget^.entry), NewText)
  else
    gtk_entry_set_text(PGtkEntry(ComboWidget^.entry), #0);
  // unlock combobox
  LockOnChange(PGtkObject(ComboWidget^.entry),-1);
end;

{------------------------------------------------------------------------------
  function GetComboBoxItemIndex(ComboBox: TComboBox): integer;

  Returns the current ItemIndex of a TComboBox
 ------------------------------------------------------------------------------}
function GetComboBoxItemIndex(ComboBox: TComboBox): integer;
begin
  Result:=ComboBox.Items.IndexOf(ComboBox.Text);
end;

{------------------------------------------------------------------------------
  procedure SetComboBoxItemIndex(ComboBox: TComboBox; Index: integer);

  Returns the current ItemIndex of a TComboBox
 ------------------------------------------------------------------------------}
procedure SetComboBoxItemIndex(ComboBox: TComboBox; Index: integer);
var
  ComboWidget: PGtkCombo;
begin
  ComboWidget:=PGtkCombo(ComboBox.Handle);
  gtk_list_select_item(PGtkList(ComboWidget^.list),Index);
  if Index>=0 then
    SetComboBoxText(ComboWidget,PChar(ComboBox.Items[Index]));
end;

{------------------------------------------------------------------------------
  function GtkPaintMessageToPaintMessage(GtkPaintMsg: TLMGtkPaint): TLMPaint;

  Converts a LM_GtkPaint message to a LM_PAINT message
 ------------------------------------------------------------------------------}
function GtkPaintMessageToPaintMessage(GtkPaintMsg: TLMGtkPaint): TLMPaint;
begin
  Result.Msg:=LM_PAINT;
  Result.DC:=GetDC(THandle(GtkPaintMsg.Widget));
  Result.Unused:=0;
  Result.Result:=0;
end;

{------------------------------------------------------------------------------
  Function: NewGDIRawImage
  Params:  Width, Height: Size of the image
           Depth:         Depth of the image
  Returns: a GDIRawImage

  Creates a RawImage
 ------------------------------------------------------------------------------}
function NewGDIRawImage(const AWidth, AHeight: Integer; const ADepth: Byte): PGDIRawImage;
begin
  Result := AllocMem(SizeOf(TGDIRawImage) + ((AWidth * AHeight) - 1) * SizeOf(TGDIRGB));
//  FillChar(Result^, SizeOf(TGDIRawImage), 0);
  with Result^ do
  begin
    Height := AHeight;
    Width := AWidth;
    Depth := ADepth;
  end;
end;

{------------------------------------------------------------------------------
  Function: AllocGDKColor
  Params:  AColor: A RGB color (TColor)
  Returns: an Allocated GDKColor

  Allocated a GDKColor from a winapi color
 ------------------------------------------------------------------------------}
function AllocGDKColor(const AColor: LongInt): TGDKColor;
begin
  with Result do
  begin
    Red :=   ((AColor shl 8) and $00FF00) or ((AColor       ) and $0000FF);
    Green := ((AColor      ) and $00FF00) or ((AColor shr 8 ) and $0000FF);
    Blue :=  ((AColor shr 8) and $00FF00) or ((AColor shr 16) and $0000FF);
  end;
  gdk_colormap_alloc_color(gdk_colormap_get_system, @Result, False, True);
end;

{------------------------------------------------------------------------------
  Function: CopyDCData
  Params:  DestinationDC:  a dc to copy data to
           SourceDC: a dc to copy data from
  Returns: True if succesful

  Creates a copy DC from the given DC
 ------------------------------------------------------------------------------}
function CopyDCData(DestinationDC, SourceDC: TDeviceContext): Boolean;
var
  GCValues: TGDKGCValues;
begin
  Assert(False, Format('Trace:> [CopyDCData] DestDC:0x%x, SourceDC:0x%x', [Integer(DestinationDC), Integer(SourceDC)]));
  Result := (DestinationDC <> nil) and (SourceDC <> nil);
  if Result
  then begin
    with DestinationDC do
    begin
      Wnd := SourceDC.Wnd;
      Drawable := SourceDC.Drawable;
      if GC<>nil then begin
        gdk_gc_unref(GC);
        GC:=nil;
        DCFlags:=DCFlags-[dcfPenSelected];
      end;
      if (SourceDC.GC <> nil) and (Drawable <> nil) then begin
        gdk_gc_get_values(SourceDC.GC, @GCValues);
        GC := gdk_gc_new_with_values(Drawable, @GCValues, 3 { $3FF});
        DCFlags:=DCFlags-[dcfPenSelected];
      end;
      
      Origin := SourceDC.Origin;
      SpecialOrigin := SourceDC.SpecialOrigin;
      PenPos := SourceDC.PenPos;
      
      if (dcfTextMetricsValid in SourceDC.DCFlags) then begin
        Include(DCFlags,dcfTextMetricsValid);
        DCTextMetric := SourceDC.DCTextMetric;
      end else
        Exclude(DCFlags,dcfTextMetricsValid);
      CurrentBitmap := SourceDC.CurrentBitmap;
      CurrentFont := SourceDC.CurrentFont;
      CurrentPen := SourceDC.CurrentPen;
      CurrentBrush := SourceDC.CurrentBrush;
      //CurrentPalette := SourceDC.CurrentPalette;
      CopyGDIColor(SourceDC.CurrentTextColor,CurrentTextColor);
      CopyGDIColor(SourceDC.CurrentBackColor,CurrentBackColor);
      ClipRegion := SourceDC.ClipRegion;

      SelectedColors := dcscCustom;
      SavedContext := nil;
    end;
  end;
  Assert(False, Format('Trace:< [CopyDCData] DestDC:0x%x, SourceDC:0x%x --> %d', [Integer(DestinationDC), Integer(SourceDC), Integer(Result)]));
end;

Function RegionType(RGN : PGDKRegion) : Longint;
var
  aRect : TGDKRectangle;
  rRGN  : hRGN;
begin
  If RGN = nil then
    Result := ERROR
  else
    If gdk_region_empty(RGN) then
      Result := NULLREGION
  else begin
    gdk_region_get_clipbox(RGN,@aRect);
    With aRect do
      rRGN := CreateRectRgn(X, Y, X + Width, Y + Height);
    if gdk_region_equal(PGDIObject(rRGN)^.GDIRegionObject, RGN) then
      Result := SIMPLEREGION
    else
      Result := COMPLEXREGION;
    DeleteObject(rRGN);
  end;
end;

{------------------------------------------------------------------------------
  Procedure SelectGDIRegion(const DC: HDC);

  Applies the current clipping region of the DC (DeviceContext) to the
  gc (GDK Graphic context - pgdkGC)
 ------------------------------------------------------------------------------}
Procedure SelectGDIRegion(const DC: HDC);
var
  Region: PGdiObject;
  DCOrigin: TPoint;
  RGNType : Longint;
begin
  with TDeviceContext(DC) do
  begin
    gdk_gc_set_clip_region(gc,  nil);
    gdk_gc_set_clip_rectangle (gc,  nil);
    If (ClipRegion <> 0) then begin
      Region:=PGDIObject(ClipRegion);
      RGNType := RegionType(Region^.GDIRegionObject);
      If (RGNType <> ERROR) and (RGNType <> NULLREGION) then begin
        DCOrigin:=GetDCOffset(TDeviceContext(DC));
        if (DCOrigin.X<>0) or (DCOrigin.Y<>0) then
          gdk_region_offset(Region^.GDIRegionObject,DCOrigin.X,DCOrigin.Y);
        gdk_gc_set_clip_region(gc,  PGDIObject(ClipRegion)^.GDIRegionObject);
        if (DCOrigin.X<>0) or (DCOrigin.Y<>0) then
          gdk_region_offset(Region^.GDIRegionObject,-DCOrigin.X,-DCOrigin.Y);
      end;
    end;
  end;
end;

Procedure FreeGDIColor(GDIColor: PGDIColor);
begin
  if (cfColorAllocated in GDIColor^.ColorFlags) then begin
    if (GDIColor^.Colormap <> nil) then
      gdk_colormap_free_colors(GDIColor^.Colormap,@(GDIColor^.Color), 1);

    //GDIColor.Color.Pixel := -1;
    Exclude(GDIColor^.ColorFlags,cfColorAllocated);
  end;
end;

procedure SetGDIColorRef(var GDIColor: TGDIColor; NewColorRef: TColorRef);
begin
  if GDIColor.ColorRef=NewColorRef then exit;
  FreeGDIColor(@GDIColor);
  GDIColor.ColorRef:=NewColorRef;
end;

Procedure AllocGDIColor(DC: hDC; GDIColor: PGDIColor);
var
  RGBColor : Longint;
begin
  if DC=0 then ;
  if not (cfColorAllocated in GDIColor^.ColorFlags) then begin
    Case TColor(GDIColor^.ColorRef) of
      clScrollbar..clEndColors:
        RGBColor := GetSysColor(GDIColor^.ColorRef and $FF);
      else
        RGBColor := GDIColor^.ColorRef and $FFFFFF;
    end;

    With GDIColor^.Color do begin
      Red := gushort(GetRValue(RGBColor)) shl 8;
      Green := gushort(GetGValue(RGBColor)) shl 8;
      Blue := gushort(GetBValue(RGBColor)) shl 8;
      Pixel := 0;
    end;

    {with TDeviceContext(DC) do
      If CurrentPalette <> nil then
        GDIColor.Colormap := CurrentPalette^.PaletteColormap
      else}
        GDIColor^.Colormap := GDK_Colormap_get_system;

    gdk_colormap_alloc_color(GDIColor^.Colormap, @(GDIColor^.Color),True,True);
    Include(GDIColor^.ColorFlags,cfColorAllocated);
  end;
end;

procedure BuildColorRefFromGDKColor(var GDIColor: TGDIColor);
begin
  GDIColor.ColorRef:=TGDKColorToTColor(GDIColor.Color);
  Include(GDIColor.ColorFlags,cfColorAllocated);
end;

Procedure EnsureGCColor(DC: hDC; ColorType: TDevContextsColorType;
  IsSolidBrush: Boolean; AsBackground: Boolean);
var
  GC: PGDKGC;
  GDIColor: PGDIColor;

  Procedure EnsureAsGCValues;
  var
    AllocFG : Boolean;
  begin
    FreeGDIColor(GDIColor);
    With GetSysGCValues(GDIColor^.ColorRef) do begin
      gdk_gc_set_fill(GC, fill);
      AllocFG := Foreground.Pixel = 0;
      If AllocFG then
        gdk_colormap_alloc_color(GDK_Colormap_get_system,@Foreground,True,True);
      gdk_gc_set_foreground(GC, @foreground);
      Case Fill of
        GDK_TILED :
          If Tile <> nil then
          begin
            gdk_gc_set_ts_origin(GC, ts_x_origin, ts_y_origin);
            gdk_gc_set_tile(GC, Tile);
          end;
        GDK_STIPPLED,
        GDK_OPAQUE_STIPPLED:
          If stipple <> nil then begin
            gdk_gc_set_background(GC, @background);
            gdk_gc_set_ts_origin(GC, ts_x_origin, ts_y_origin);
            gdk_gc_set_stipple(GC, stipple);
          end;
      end;
      If AllocFG then
        gdk_colormap_free_colors(GDK_Colormap_get_system, @Foreground,1);
    end;
  end;

  Procedure EnsureAsColor;
  begin
    AllocGDIColor(DC, GDIColor);
    If AsBackground then
      gdk_gc_set_background(GC, @(GDIColor^.Color))
    else begin
      gdk_gc_set_fill(GC, GDK_SOLID);
      gdk_gc_set_foreground(GC, @(GDIColor^.Color));
    end;
  end;
  
begin
  GC:=TDeviceContext(DC).GC;
  with TDeviceContext(DC) do
  begin
    case ColorType of
    dccCurrentBackColor: GDIColor:=@CurrentBackColor;
    dccCurrentTextColor: GDIColor:=@CurrentTextColor;
    dccGDIBrushColor   : GDIColor:=@(CurrentBrush^.GDIBrushColor);
    dccGDIPenColor     : GDIColor:=@(CurrentPen^.GDIPenColor);
    else
      exit;
    end;
    Case TColor(GDIColor^.ColorRef) of
      clScrollbar,
      clInfoBk,
      clMenu,
      clHighlight,
      clHighlightText,
      clBtnFace:  //often have a BK Pixmap
        If IsSolidBrush then
          EnsureAsGCValues
        else
          EnsureAsColor;//GC's with Pixmaps can't work w/Hatch's (yet)
      clBtnShadow,
      clBtnHighlight,
      clBtnText,
      clInfoText,
      clWindow,
      clWindowText,
      clMenuText,
      clGrayText ://should never have a BK Pixmap
        EnsureAsGCValues;
      else
        EnsureAsColor;
    end;
  end;
end;

procedure CopyGDIColor(var SourceGDIColor, DestGDIColor: TGDIColor);
begin
  SetGDIColorRef(DestGDIColor,SourceGDIColor.ColorRef);
end;

function CompareGDIColor(const Color1, Color2: TGDIColor): boolean;
begin
  Result:=Color1.ColorRef=Color2.ColorRef;
end;

function CompareGDIFill(const Fill1, Fill2: TGdkFill): boolean;
begin
  Result:=Fill1=Fill2;
end;

function CompareGDIBrushes(Brush1, Brush2: PGdiObject): boolean;
begin
  Result:=Brush1^.IsNullBrush=Brush2^.IsNullBrush;
  if Result then begin
    Result:=CompareGDIColor(Brush1^.GDIBrushColor,Brush2^.GDIBrushColor);
    if Result then begin
      Result:=CompareGDIFill(Brush1^.GDIBrushFill,Brush2^.GDIBrushFill);
      if Result then begin
        Result:=Brush1^.GDIBrushPixMap=Brush2^.GDIBrushPixMap;
      end;
    end;
  end;
end;

//-----------------------------------------------------------------------------

{ Palette Index<->RGB Hash Functions }

type
  TIndexRGB = record
    Index: longint;
    RGB: longint;
  end;
  PIndexRGB = ^TIndexRGB;

function GetIndexAsKey(p: pointer): pointer;
begin
  Result:=Pointer(PIndexRGB(p)^.Index + 1);
end;

function GetRGBAsKey(p: pointer): pointer;
begin
  Result:=Pointer(PIndexRGB(p)^.RGB + 1);
end;

function PaletteIndexToIndexRGB(Pal : PGDIObject; I : longint): PIndexRGB;
var
  HashItem: PDynHashArrayItem;
begin
  Result := nil;
  HashItem:=Pal^.IndexTable.FindHashItemWithKey(Pointer(I + 1));
  if HashItem<>nil then
    Result:=PIndexRGB(HashItem^.Item);
end;

function PaletteRGBToIndexRGB(Pal : PGDIObject; RGB : longint): PIndexRGB;
var
  HashItem: PDynHashArrayItem;
begin
  Result := nil;
  HashItem:=Pal^.RGBTable.FindHashItemWithKey(Pointer(RGB + 1));
  if HashItem<>nil then
    Result:=PIndexRGB(HashItem^.Item);
end;

{ Palette Index<->RGB lookup Functions }

function PaletteIndexExists(Pal : PGDIObject; I : longint): Boolean;
begin
  Result := Pal^.IndexTable.ContainsKey(Pointer(I + 1));
end;

function PaletteRGBExists(Pal : PGDIObject; RGB : longint): Boolean;
begin
  Result := Pal^.RGBTable.ContainsKey(Pointer(RGB + 1));
end;

function PaletteAddIndex(Pal : PGDIObject; I, RGB : Longint): Boolean;
var
  IndexRGB: PIndexRGB;
begin
  New(IndexRGB);
  IndexRGB^.Index:=I;
  IndexRGB^.RGB:=RGB;
  Pal^.IndexTable.Add(IndexRGB);
  Result := PaletteIndexExists(Pal, I);
  If Not Result then
    Dispose(IndexRGB)
  else begin
    Pal^.RGBTable.Add(IndexRGB);
    Result := PaletteRGBExists(Pal, RGB);
    If not Result then begin
      Pal^.IndexTable.Remove(IndexRGB);
      Dispose(IndexRGB);
    end;
  end;
end;

function PaletteDeleteIndex(Pal : PGDIObject; I : Longint): Boolean;
var
  RGBIndex : PIndexRGB;
begin
  RGBIndex := PaletteIndextoIndexRGB(Pal,I);
  Result := RGBIndex = nil;
  If not Result then begin
    Pal^.IndexTable.Remove(RGBIndex);
    If PaletteRGBExists(Pal, RGBIndex^.RGB) then
      Pal^.RGBTable.Remove(RGBIndex);
    Dispose(RGBIndex);
  end;
end;

function PaletteIndexToRGB(Pal : PGDIObject; I : longint): longint;
var
  RGBIndex : PIndexRGB;
begin
  RGBIndex := PaletteIndextoIndexRGB(Pal,I);
  if RGBIndex = nil then
    Result := -1//InvalidRGB
  else
    Result := RGBIndex^.RGB;
end;

function PaletteRGBToIndex(Pal : PGDIObject; RGB : longint): longint;
var
  RGBIndex : PIndexRGB;
begin
  RGBIndex := PaletteRGBtoIndexRGB(Pal,RGB);
  if RGBIndex = nil then
    Result:=-1//InvalidIndex
  else
    Result := RGBIndex^.Index;
end;

Procedure InitializePalette(Pal : PGDIObject; Entries : PPALETTEENTRY; RGBCount : Longint);
var
  PalEntries : PPALETTEENTRY;
  I : Integer;
  RGBValue : Longint;
begin
  PalEntries := Entries;
  For I := 0 to RGBCount - 1 do begin
    If PaletteIndexExists(Pal, I) then
      PaletteDeleteIndex(Pal, I);
    With PalEntries[I] do
      RGBValue := RGB(peRed, peGreen, peBlue) {or (peFlags shl 32)??};
    If not PaletteRGBExists(Pal, RGBValue) then
      PaletteAddIndex(Pal, I, RGBValue);
  end;
end;

{------------------------------------------------------------------------------
  Procedure: GTKEventState2ShiftState
  Params:    KeyState: The gtk keystate
  Returns:   the TShiftState for the given KeyState

  GTKEventState2ShiftState converts a GTK event state to a LCL/Delphi TShiftState
 ------------------------------------------------------------------------------}
function GTKEventState2ShiftState(KeyState: Word): TShiftState;
begin
  result:=[];
  if (KeyState and GDK_SHIFT_MASK)   <> 0 then Result := Result + [ssShift];
  if (KeyState and GDK_LOCK_MASK)    <> 0 then Result := Result + [ssCaps];
  if (KeyState and GDK_CONTROL_MASK) <> 0 then Result := Result + [ssCtrl];
  if (KeyState and GDK_MOD1_MASK)    <> 0 then Result := Result + [ssAlt];
//if (KeyState and GDK_MOD2_MASK)    <> 0 then Result := Result + [??ssWindows??];
  if (KeyState and GDK_MOD3_MASK)    <> 0 then Result := Result + [ssNum];
  if (KeyState and GDK_MOD4_MASK)    <> 0 then Result := Result + [ssSuper];
  if (KeyState and GDK_MOD5_MASK)    <> 0 then Result := Result + [ssScroll];
  if (KeyState and GDK_BUTTON1_MASK) <> 0 then Result := Result + [ssLeft];
  if (KeyState and GDK_BUTTON2_MASK) <> 0 then Result := Result + [ssMiddle];
  if (KeyState and GDK_BUTTON3_MASK) <> 0 then Result := Result + [ssRight];
//if (KeyState and GDK_BUTTON4_MASK) <> 0 then Result := Result + [??WheelMouse??];
//if (KeyState and GDK_BUTTON5_MASK) <> 0 then Result := Result + [??WheelMouse??];
  if (KeyState and GDK_RELEASE_MASK) <> 0 then Result := Result + [ssAltGr];
end;


{------------------------------------------------------------------------------
 ------------------------------------------------------------------------------}
function KeyToListCode(KeyCode, VirtKeyCode: Word; Extended: boolean): integer;
begin
  if VirtKeyCode = VK_UNKNOWN then
    Result := KEYMAP_VKUNKNOWN or KeyCode
  else
    Result := VirtKeyCode;
  if Extended then
    Result := Result or KEYMAP_EXTENDED;
end;

{------------------------------------------------------------------------------
  Procedure: GetGTKKeyInfo
  Params:  Event:      Requested info  
           KeyCode:    the ASCII key code of the eventkey
           VirtualKey: the virtual key code of the eventkey
           SysKey:     True if the key is a syskey
           Extended:   True if the key is an extended key
           Toggle:     True if the key is a toggle key and its value is on
  Returns: Nothing
  
  GetGTKKeyInfo returns information about the given key event
 ------------------------------------------------------------------------------}
procedure GetGTKKeyInfo(const Event: PGDKEventKey; var KeyCode,VirtualKey: Word;
  var SysKey, Extended, Toggle: Boolean; var ShiftState: TShiftState);
begin
  VirtualKey := VK_UNKNOWN;
  KeyCode := $FFFF;
  ShiftState := GTKEventState2ShiftState(Event^.State);
  SysKey := (ssAlt in ShiftState);
  Extended := False;
  Toggle := False;
  
  {write('GetGTKKeyInfo Event^.KeyVal=',Event^.KeyVal,' Event^.Length=',Event^.Length,' ');
  if Event^.Length>0 then write(ord(Event^.theString[0]));
  writeln('');}

  //writeln('GetGTKKeyInfo ',Event^.KeyVal,' ',HexStr(Cardinal(Event^.KeyVal),8));

  case Event^.KeyVal of
    // Normal ASCII chars
    32..255:
      begin
        // Assign key code
        KeyCode := Event^.KeyVal;
        
        //TODO: create VK_ code --> [*] and [8] have a different KeyCode but same VK_ code on US keyboard
        case Chr(KeyCode) of
          '@': VirtualKey:=VK_AT;
          'A'..'Z', 
          '0'..'9', 
          ' ':      VirtualKey := KeyCode;
          'a'..'z': VirtualKey := KeyCode - Ord('a') + Ord('A');
          '/': VirtualKey := VK_SLASH;
          ',': VirtualKey := VK_COMMA;
          '=': VirtualKey := VK_EQUAL;
          '*': VirtualKey := VK_MULTIPLY;
          '+': VirtualKey := VK_ADD;
          '-': VirtualKey := VK_SUBTRACT;
          '.': VirtualKey := VK_POINT;
        end;
        // look for control code
        if (ssCtrl in ShiftState) and (Chr(KeyCode) in ['@'..'Z']) then
          Dec(KeyCode, Ord('@'));
      end;
      
    {$ifdef GTK2}GDK_key_dead_circumflex{$else}GDK_dead_circumflex{$EndIf}:
      begin
        KeyCode := Ord('^');
      end;
      
    {$ifdef GTK2}GDK_KEY_KP_Space{$else}GDK_KP_Space{$EndIf}:
      begin
        KeyCode := VK_SPACE;
        VirtualKey := VK_SPACE;
      end;

    {$ifdef GTK2}GDK_KEY_KP_ENTER{$else}GDK_KP_ENTER{$EndIf}:
     begin
        KeyCode := VK_Return;
        VirtualKey := VK_Return;
      end;

    {$ifdef GTK2}
    GDK_KEY_Tab, GDK_KEY_ISO_Left_Tab, GDK_KEY_KP_Tab
    {$else}
    GDK_Tab, GDK_ISO_Left_Tab, GDK_KP_Tab
    {$EndIf}:
      begin
        KeyCode := VK_TAB;
        VirtualKey := VK_TAB;
      end;

    {$ifdef GTK2}GDK_KEY_RETURN{$else}GDK_Return{$EndIf}:
      begin
        KeyCode := VK_RETURN;
        VirtualKey := VK_RETURN;
      end;

    {$ifdef GTK2}GDK_KEY_LINEFEED{$else}GDK_LINEFEED{$EndIf}:
      begin
        KeyCode := $0A;
        VirtualKey := $0A;
      end;
 
    // Cursor block / keypad
    {$ifdef GTK2}GDK_KEY_INSERT{$else}GDK_INSERT{$EndIf}:
      begin
        VirtualKey := VK_INSERT;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_HOME{$else}GDK_HOME{$EndIf}:
      begin
        VirtualKey := VK_HOME;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_LEFT{$else}GDK_LEFT{$EndIf}:
      begin
        VirtualKey := VK_LEFT;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_UP{$else}GDK_UP{$EndIf}:
      begin
        VirtualKey := VK_UP;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_RIGHT{$else}GDK_RIGHT{$EndIf}:
      begin
        VirtualKey := VK_RIGHT;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_DOWN{$else}GDK_DOWN{$EndIf}:
      begin
        VirtualKey := VK_DOWN;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_Page_Up{$else}GDK_Page_Up{$EndIf}:
      begin
        VirtualKey := VK_PRIOR;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_Page_Down{$else}GDK_Page_Down{$EndIf}:
      begin
        VirtualKey := VK_NEXT;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_KEY_End{$else}GDK_End{$EndIf}:
      begin
        VirtualKey := VK_END;
        Extended := True;
      end;
    
    {$ifdef GTK2}GDK_KEY_KP_INSERT{$else}GDK_KP_INSERT{$EndIf}:      
      VirtualKey := VK_INSERT;
    {$ifdef GTK2}GDK_KEY_KP_HOME{$else}GDK_KP_HOME{$EndIf}:      
      VirtualKey := VK_HOME;
    {$ifdef GTK2}GDK_KEY_KP_LEFT{$else}GDK_KP_LEFT{$EndIf}:      
      VirtualKey := VK_LEFT;
    {$ifdef GTK2}GDK_KEY_KP_UP{$else}GDK_KP_UP{$EndIf}:      
      VirtualKey := VK_UP;
    {$ifdef GTK2}GDK_KEY_KP_RIGHT{$else}GDK_KP_RIGHT{$EndIf}:      
      VirtualKey := VK_RIGHT;
    {$ifdef GTK2}GDK_KEY_KP_DOWN{$else}GDK_KP_DOWN{$EndIf}:      
      VirtualKey := VK_DOWN;
    {$ifdef GTK2}GDK_KEY_KP_PAGE_UP{$else}GDK_KP_PAGE_UP{$EndIf}:      
      VirtualKey := VK_PRIOR;
    {$ifdef GTK2}GDK_KEY_KP_PAGE_DOWN{$else}GDK_KP_PAGE_DOWN{$EndIf}:      
      VirtualKey := VK_NEXT;
    {$ifdef GTK2}GDK_KEY_KP_END{$else}GDK_KP_END{$EndIf}:      
      VirtualKey := VK_END;

    {$ifdef GTK2}GDK_KEY_Num_Lock{$else}GDK_Num_Lock{$EndIf}:      
      VirtualKey := VK_NUMLOCK;
    {$ifdef GTK2}GDK_KEY_KP_F1{$else}GDK_KP_F1{$EndIf}:      
      VirtualKey := VK_F1;
    {$ifdef GTK2}GDK_KEY_KP_F2{$else}GDK_KP_F2{$EndIf}:      
      VirtualKey := VK_F2;
    {$ifdef GTK2}GDK_KEY_KP_F3{$else}GDK_KP_F3{$EndIf}:      
      VirtualKey := VK_F3;
    {$ifdef GTK2}GDK_KEY_KP_F4{$else}GDK_KP_F4{$EndIf}:      
      VirtualKey := VK_F4;

    {$ifdef GTK2}GDK_KEY_KP_EQUAL{$else}GDK_KP_EQUAL{$EndIf}:      
      begin
        VirtualKey := VK_EQUAL;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('=');
      end;

    {$ifdef GTK2}GDK_KEY_KP_Multiply{$else}GDK_KP_Multiply{$EndIf}:      
      begin
        VirtualKey := VK_MULTIPLY;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('*');
      end;

    {$ifdef GTK2}GDK_KEY_KP_Add{$else}GDK_KP_Add{$EndIf}:      
      begin
        VirtualKey := VK_ADD;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('+');
      end;

    {$ifdef GTK2}GDK_KEY_KP_Separator{$else}GDK_KP_Separator{$EndIf}:      
      begin
        VirtualKey := VK_SEPARATOR;
//        if not CtrlDown then KeyCode := Ord('????'); 
      end;

    {$ifdef GTK2}GDK_KEY_KP_Subtract{$else}GDK_KP_Subtract{$EndIf}:      
      begin
        VirtualKey := VK_SUBTRACT;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('-');
      end;

    {$ifdef GTK2}GDK_KEY_KP_Decimal{$else}GDK_KP_Decimal{$EndIf}:      
      begin
        VirtualKey := VK_DECIMAL;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('.');
      end;

    {$ifdef GTK2}GDK_KEY_KP_Divide{$else}GDK_KP_Divide{$EndIf}:      
      begin
        VirtualKey := VK_DIVIDE;
        Extended := True;
        if not (ssCtrl in ShiftState) then KeyCode := Ord('/');
      end;

    {$ifdef GTK2}
    GDK_KEY_KP_0..GDK_KEY_KP_9
    {$else}
    GDK_KP_0..GDK_KP_9
    {$EndIf}:      
      begin
        VirtualKey := VK_NUMPAD0 + (Event^.KeyVal -
                              {$ifdef GTK2}GDK_KEY_KP_0{$else}GDK_KP_0{$EndIf});
        if not (ssCtrl in ShiftState) then
          KeyCode := Ord('0') + (Event^.KeyVal -
                              {$ifdef GTK2}GDK_KEY_KP_0{$else}GDK_KP_0{$EndIf});
      end;

    {$ifdef GTK2}GDK_KEY_BackSpace{$else}GDK_BackSpace{$EndIf}:      
      VirtualKey := VK_BACK;

    {$ifdef GTK2}GDK_KEY_Clear{$else}GDK_Clear_Key{$EndIf}:      
      VirtualKey := VK_CLEAR;
    {$ifdef GTK2}GDK_KEY_Pause{$else}GDK_Pause{$EndIf}:      
      VirtualKey := VK_PAUSE;
    {$ifdef GTK2}GDK_KEY_Scroll_Lock{$else}GDK_Scroll_Lock{$EndIf}:      
      VirtualKey := VK_SCROLL;
    {$ifdef GTK2}GDK_KEY_Sys_Req{$else}GDK_Sys_Req{$EndIf}:      
      VirtualKey := VK_SNAPSHOT;
    {$ifdef GTK2}GDK_KEY_Escape{$else}GDK_Escape{$EndIf}:      
      VirtualKey := VK_ESCAPE;
    {$ifdef GTK2}GDK_KEY_Delete{$else}GDK_Delete_Key{$EndIf}:      
      VirtualKey := VK_DELETE;
    
//    GDK_Multi_key = $FF20;
//    GDK_SingleCandidate = $FF3C;
//    GDK_MultipleCandidate = $FF3D;
//    GDK_PreviousCandidate = $FF3E;

    {$ifdef GTK2}GDK_KEY_Kanji{$else}GDK_Kanji{$EndIf}:      
      VirtualKey := VK_KANJI;

//    GDK_Muhenkan = $FF22;
//    GDK_Henkan_Mode = $FF23;
//    GDK_Henkan = $FF23;
//    GDK_Romaji = $FF24;
//    GDK_Hiragana = $FF25;
//    GDK_Katakana = $FF26;
//    GDK_Hiragana_Katakana = $FF27;
//    GDK_Zenkaku = $FF28;
//    GDK_Hankaku = $FF29;
//    GDK_Zenkaku_Hankaku = $FF2A;
//    GDK_Touroku = $FF2B;
//    GDK_Massyo = $FF2C;
//    GDK_Kana_Lock = $FF2D;
//    GDK_Kana_Shift = $FF2E;
//    GDK_Eisu_Shift = $FF2F;
//    GDK_Eisu_toggle = $FF30;
//    GDK_Zen_Koho = $FF3D;
//    GDK_Mae_Koho = $FF3E;

    {$ifdef GTK2}GDK_Key_Select{$else}GDK_Select{$EndIf}:      
      VirtualKey := VK_SELECT;
    {$ifdef GTK2}GDK_Key_Print{$else}GDK_Print{$EndIf}:      
      VirtualKey := VK_PRINT;
    {$ifdef GTK2}GDK_Key_Execute{$else}GDK_Execute{$EndIf}:      
      VirtualKey := VK_EXECUTE;

    {$ifdef GTK2}GDK_Key_Menu{$else}GDK_Menu{$EndIf}:      
      VirtualKey := VK_MENU;
//    GDK_Find = $FF68;
    {$ifdef GTK2}GDK_Key_Cancel{$else}GDK_Cancel{$EndIf}:      
      VirtualKey := VK_CANCEL;
    {$ifdef GTK2}GDK_Key_Help{$else}GDK_Help{$EndIf}:      
      VirtualKey := VK_HELP;
    {$ifdef GTK2}GDK_Key_Break{$else}GDK_Break{$EndIf}:      
      VirtualKey := VK_CANCEL;
    {$ifdef GTK2}GDK_Key_Mode_switch{$else}GDK_Mode_switch{$EndIf}:      
      VirtualKey := VK_MODECHANGE;
//    GDK_script_switch = $FF7E;

    {$ifdef GTK2}GDK_Key_Caps_Lock{$else}GDK_Caps_Lock{$EndIf}:      
      VirtualKey := VK_CAPITAL;
//    GDK_Shift_Lock = $FFE6;

    {$ifdef GTK2}GDK_Key_Shift_L{$else}GDK_Shift_L{$EndIf}:      
      begin
        VirtualKey := VK_SHIFT;
      end;

    {$ifdef GTK2}GDK_Key_Shift_R{$else}GDK_Shift_R{$EndIf}:      
      begin
        VirtualKey := VK_SHIFT;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_Key_Control_L{$else}GDK_Control_L{$EndIf}:      
      begin
        VirtualKey := VK_CONTROL;
      end;

    {$ifdef GTK2}GDK_Key_Control_R{$else}GDK_Control_R{$EndIf}:      
      begin
        VirtualKey := VK_CONTROL;
        Extended := True;
      end;

    {$ifdef GTK2}GDK_Key_Alt_L{$else}GDK_Alt_L{$EndIf}:      
      begin
        SysKey := True;
        VirtualKey:= VK_MENU; 
      end; 

    {$ifdef GTK2}GDK_Key_Alt_R{$else}GDK_Alt_R{$EndIf}:      
      begin
        SysKey := True;
        VirtualKey:= VK_MENU; 
        Extended := True;
      end;
    
    // Function keys
    {$ifdef GTK2}
      GDK_KEY_F1..GDK_KEY_F24   
    {$else}
      GDK_F1..GDK_F24
    {$EndIf}:      
      VirtualKey := VK_F1 + (Event^.KeyVal -
                                  {$ifdef GTK2}GDK_Key_F1{$else}GDK_F1{$EndIf});

    //By VVI - fixing cyrillic keys
    //GDK_* is like a koi8-r, it is KOI8-R code +$600.
    {$ifdef GTK2}
      GDK_KEY_cyrillic_io..GDK_KEY_cyrillic_Capital_hardsign
    {$else}
      GDK_cyrillic_io..GDK_cyrillic_Capital_hardsign
    {$EndIf}:      
     KeyCode := Event^.KeyVal mod $100;
  end;

  if VirtualKey=VK_UNKNOWN then begin
    // map all other keys to VK_IRREGULAR + KeyCode
    if ShortInt(KeyCode)>=0 then
      VirtualKey := MapIrregularVirtualKey(VK_IRREGULAR + KeyCode)
    else
      VirtualKey := VK_IRREGULAR;
  end;
  //writeln('GetGTKKeyInfo END ',VirtualKey,' VK_L=',VK_L);
end;

{------------------------------------------------------------------------------
  Procedure: StoreCommonDialogSetup
  Params:    ADialog: TCommonDialog
  Returns:   none

  Stores the size of a TCommonDialog.
 ------------------------------------------------------------------------------}
procedure StoreCommonDialogSetup(ADialog: TCommonDialog);
var DlgWindow: PGtkWidget;
begin
  if (ADialog=nil) or (ADialog.Handle=0) then exit;
  DlgWindow:=PGtkWidget(ADialog.Handle);
  if DlgWindow^.Allocation.Width>0 then
    ADialog.Width:=DlgWindow^.Allocation.Width;
  if DlgWindow^.Allocation.Height>0 then
    ADialog.Height:=DlgWindow^.Allocation.Height;
end;

{------------------------------------------------------------------------------
  Procedure: DestroyCommonDialogAddOns
  Params:    ADialog: TCommonDialog
  Returns:   none

  Free the memory of additional data of a TCommonDialog
 ------------------------------------------------------------------------------}
procedure DestroyCommonDialogAddOns(ADialog: TCommonDialog);
var
  DlgWindow: PGtkWidget;
  HistoryList: TList; // list of TFileSelHistoryListEntry
  FilterList: TList; // list of TFileSelFilterListEntry
  AHistoryEntry: PFileSelHistoryEntry;
  AFilterEntry: PFileSelFilterEntry;
  i: integer;
begin
  if (ADialog=nil) or (not ADialog.HandleAllocated) then exit;
  DlgWindow:=PGtkWidget(ADialog.Handle);
  gtk_window_set_transient_for(PGtkWindow(DlgWindow),nil);
  if ADialog is TOpenDialog then begin
  
    // free history
    HistoryList:=TList(gtk_object_get_data(PGtkObject(DlgWindow),
                                           'LCLHistoryList'));
    if HistoryList<>nil then begin
      for i:=0 to HistoryList.Count-1 do begin
        AHistoryEntry:=PFileSelHistoryEntry(HistoryList[i]);
        StrDispose(AHistoryEntry^.Filename);
        AHistoryEntry^.Filename:=nil;
        Dispose(AHistoryEntry);
      end;
      HistoryList.Free;
      gtk_object_set_data(PGtkObject(DlgWindow),'LCLHistoryList',nil);
    end;

    // free filter
    FilterList:=TList(gtk_object_get_data(PGtkObject(DlgWindow),
                                          'LCLFilterList'));
    if FilterList<>nil then begin
      for i:=0 to FilterList.Count-1 do begin
        AFilterEntry:=PFileSelFilterEntry(FilterList[i]);
        StrDispose(AFilterEntry^.Description);
        AFilterEntry^.Description:=nil;
        StrDispose(AFilterEntry^.Mask);
        AFilterEntry^.Mask:=nil;
        Dispose(AFilterEntry);
      end;
      FilterList.Free;
      gtk_object_set_data(PGtkObject(DlgWindow),'LCLFilterList',nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Procedure: DeliverMessage
  Params:    Message: the message to process
  Returns:   True if handled

  Generic function which calls the WindowProc if defined, otherwise the
  dispatcher
 ------------------------------------------------------------------------------}
function DeliverMessage(const Target: Pointer; var AMessage): Integer;
begin
  if Target=nil then writeln('[DeliverMessage] Target = nil');
  {$IFDEF VerboseDeliverMessage}
  writeln('DeliverMessage ',HexStr(Cardinal(Target),8),
    ' ',TComponent(Target).Name,':',TObject(Target).ClassName,
    ' Message=',GetMessageName(TLMessage(AMessage).Msg));
  {$ENDIF}
  if (TLMessage(AMessage).Msg=LM_PAINT)
  or (TLMessage(AMessage).Msg=LM_GtkPaint) then
    CurrentSentPaintMessageTarget:=TObject(Target);
  if TObject(Target) is TControl then
  begin
    TControl(Target).WindowProc(TLMessage(AMessage));
  end
  else
  begin
    TObject(Target).Dispatch(TLMessage(AMessage));
  end;
  
  Result := TLMessage(AMessage).Result;
  CurrentSentPaintMessageTarget:=nil;
end;

{------------------------------------------------------------------------------
  Function: ObjectToGTKObject
  Params: AObject: A LCL Object
  Returns:  The GTKObject of the given object

  Returns the GTKObject of the given object, nil if no object available
 ------------------------------------------------------------------------------}
function ObjectToGTKObject(const AObject: TObject): PGtkObject;
var
  handle : HWND;
begin
  Handle := 0;
  if not assigned(AObject) then
  begin
    assert (false, 'TRACE:  [ObjectToGtkObject] Object not assigned');
  end
  else if (AObject is TWinControl) then
  begin
    if TWinControl(AObject).HandleAllocated then
      handle := TWinControl(AObject).Handle;
  end
  else if (AObject is TMenuItem) then
  begin 
    if TMenuItem(AObject).HandleAllocated then
      handle := TMenuItem(AObject).Handle;
  end
  else if (AObject is TMenu) then
  begin 
    if TMenu(AObject).HandleAllocated then
      handle := TMenu(AObject).Items.Handle;
  end
  else if (AObject is TCommonDialog) then
  begin
    {if TCommonDialog(AObject).HandleAllocated then }
    handle := TCommonDialog(AObject).Handle;
  end
  else begin
    Assert(False, Format('Trace:  [ObjectToGtkObject] Message received with unhandled class-type <%s>', [AObject.ClassName])); 
  end;
  Result := PGTKObject(handle);
  if handle = 0 then
    Assert (false, 'Trace:  [ObjectToGtkObject]****** Warning: handle = 0 *******');
end;


(***********************************************************************
  Widget member functions
************************************************************************)

// ----------------------------------------------------------------------
// the main widget is the widget passed as handle to the winAPI
// main data is stored in the fixed form to get a reference to its parent
// ----------------------------------------------------------------------
function GetMainWidget(const Widget: Pointer): Pointer;
begin
  if Widget<>nil then begin
    Result := gtk_object_get_data(Widget, 'Main');
    if Result = nil then Result := Widget; // the widget is the main widget itself.
  end else
    RaiseException('GetMainWidget Widget=nil');
end;

procedure SetMainWidget(const ParentWidget, ChildWidget: Pointer);
begin
  if ChildWidget<>nil then begin
    if (ParentWidget<>ChildWidget) then
      gtk_object_set_data(ChildWidget, 'Main', ParentWidget)
    else
      raise Exception.Create('SetMainWidget ChildWidget=ParentWidget');
  end else
    RaiseException('SetMainWidget ChildWidget=nil');
end;

{ ------------------------------------------------------------------------------
 Get the fixed widget of a widget.
 Every LCL control with a clientarea, has at least a main widget for the control
 and a fixed widget for the client area. If the Fixed widget is not set, use
 the widget itself as a fallback.
------------------------------------------------------------------------------ }
function GetFixedWidget(const Widget: Pointer): Pointer;
begin
  if Widget<>nil then begin
    Result := gtk_object_get_data(Widget, 'Fixed');
    if Result = nil then Result:= Widget;
  end else begin
    RaiseException('GetFixedWidget Widget=nil');
  end;
end;

{ ------------------------------------------------------------------------------
 Set the fixed widget of a widget.
 Every LCL control with a clientarea, has at least a main widget for the control
 and a fixed widget for the client area.
------------------------------------------------------------------------------ }
procedure SetFixedWidget(const ParentWidget, FixedWidget: Pointer);
begin
  if (ParentWidget<>nil) then
    gtk_object_set_data(ParentWidget, 'Fixed', FixedWidget)
  else
    RaiseException('SetFixedWidget ParentWidget=nil');
end;

{------------------------------------------------------------------------------
  Procedure FixedMoveControl(Parent, Child : PGTKWIdget; Left, Top : Longint);
  
  Move a childwidget on a client area (fixed or layout widget).
------------------------------------------------------------------------------}
Procedure FixedMoveControl(Parent, Child : PGTKWIdget; Left, Top : Longint);
begin
  If GTKWidgetIsA(Parent, GTK_Layout_Get_Type) then
    gtk_Layout_move(PGtkLayout(Parent), Child, Left, Top)
  else
    If GTKWidgetIsA(Parent, GTK_Fixed_Get_Type) then
      gtk_fixed_move(PGtkFixed(Parent), Child, Left, Top)
  else
    WriteLn('[FixedMoveControl] WARNING: Invalid Fixed Widget');
end;

{------------------------------------------------------------------------------
  Procedure FixedPutControl(Parent, Child : PGTKWIdget; Left, Top : Longint);

  Add a childwidget onto a client area (fixed or layout widget).
------------------------------------------------------------------------------}
Procedure FixedPutControl(Parent, Child : PGTKWidget; Left, Top : Longint);

  procedure RaiseInvalidFixedWidget;
  begin
    // this is in a separate procedure for optimisation
    WriteLn('[FixedPutControl] WARNING: Invalid Fixed Widget.',
      ' Parent=',HexStr(Cardinal(Parent),8),
      ' Child=',HexStr(Cardinal(Child),8)
      );
  end;

begin
  If GTKWidgetIsA(Parent, GTK_Fixed_Get_Type) then
    gtk_fixed_put(PGtkFixed(Parent), Child, Left, Top)
  else If GTKWidgetIsA(Parent, GTK_Layout_Get_Type) then
    gtk_Layout_Put(PGtkLayout(Parent), Child, Left, Top)
  else
    RaiseInvalidFixedWidget;
end;

{------------------------------------------------------------------------------
  Function GetControlWindow(Widget: Pointer) : PGDKWindow;

  Get the gdkwindow of a widget.
------------------------------------------------------------------------------}
Function GetControlWindow(Widget: Pointer) : PGDKWindow;
begin
  If Widget <> nil then begin
    If not GTKWidgetIsA(PGTKWidget(Widget), GTK_Layout_Get_Type) then
      Result := PGTKWidget(Widget)^.Window
    else
      Result := PGtkLayout(Widget)^.bin_window;
  end else
    RaiseException('GetControlWindow Widget=nil');
end;

{------------------------------------------------------------------------------
  function GetDCOffset(DC: TDeviceContext): TPoint;

  Returns the DC offset for the DC Origin.
 ------------------------------------------------------------------------------}
function GetDCOffset(DC: TDeviceContext): TPoint;
var
  Fixed : PGTKWIdget;
  Adjustment: PGtkAdjustment;
begin
  if (DC<>nil) then begin
    Result:=DC.Origin;
    if (DC.SpecialOrigin) and (DC.Wnd<>0) then begin
      Fixed := GetFixedWidget(PGTKWidget(DC.Wnd));
      if GtkWidgetIsA(Fixed,GTK_LAYOUT_GET_TYPE) then begin
        // ToDo: add comment
        Adjustment:=gtk_layout_get_hadjustment(PGtkLayout(Fixed));
        if Adjustment<>nil then
          dec(Result.X,Trunc(Adjustment^.Value-Adjustment^.Lower));
        Adjustment:=gtk_layout_get_vadjustment(PGtkLayout(Fixed));
        if Adjustment<>nil then
          dec(Result.Y,Trunc(Adjustment^.Value-Adjustment^.Lower));
      end;
    end;
  end else begin
    Result.X:=0;
    Result.Y:=0;
  end;
end;

// ----------------------------------------------------------------------
// Creates a WinWidget info structure for the given widget
// Info needed by the API of a HWND (=Widget) 
//
// This structure obsoletes:
//   "core-child", "fixed", "class"
// ----------------------------------------------------------------------
function CreateWidgetInfo(const Widget: Pointer): PWinWidgetInfo;
begin
  if Widget = nil then Result:= nil
  else begin
    New(Result);
    FillChar(Result^, SizeOf(Result^), 0);
    gtk_object_set_data(Widget, 'widgetinfo', Result);
  end;
end;

function GetWidgetInfo(const Widget: Pointer;
  const Create: Boolean): PWinWidgetInfo;
var MainWidget: PGtkObject;
begin
  if Widget <> nil then begin
    MainWidget:= GetMainWidget(Widget);
    if MainWidget = nil then MainWidget:= Widget;
    Result:= gtk_object_get_data(MainWidget, 'widgetinfo');
    if (Result = nil) and Create then begin
      Result := CreateWidgetInfo(MainWidget);
      Result^.ImplementationWidget:= PGtkWidget(MainWidget);
    end;  
  end else
    Result:=nil;
end;

procedure FreeWinWidgetInfo(Widget: Pointer);
var
  WinWidgetInfo: PWinWidgetInfo;
begin
  if Widget=nil then exit;
  WinWidgetInfo := gtk_object_get_data(Widget, 'widgetinfo');
  if WinWidgetInfo<>nil then begin
    Dispose(WinWidgetInfo);
    gtk_object_set_data(Widget,'widgetinfo',nil);
  end;
end;

{-------------------------------------------------------------------------------
  procedure DestroyWidget(Widget: PGtkWidget);

-------------------------------------------------------------------------------}
procedure DestroyWidget(Widget: PGtkWidget);
begin
  FreeWinWidgetInfo(Widget);
  gtk_widget_destroy(Widget);
end;

{-------------------------------------------------------------------------------
  function GetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook): PGtkWidget;

  Retrieves the DummyWidget associated with the ANoteBookWidget
-------------------------------------------------------------------------------}
function GetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook): PGtkWidget;
begin
  Result:=gtk_object_get_data(PGtkObject(ANoteBookWidget),'LCLDummyPage');
end;

{-------------------------------------------------------------------------------
  procedure SetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook;
    DummyWidget: PGtkWidget): PGtkWidget;

  Associates the DummyWidget with the ANoteBookWidget
-------------------------------------------------------------------------------}
procedure SetGtkNoteBookDummyPage(ANoteBookWidget: PGtkNoteBook;
  DummyWidget: PGtkWidget);
begin
  gtk_object_set_data(PGtkObject(ANoteBookWidget),'LCLDummyPage',DummyWidget);
end;

{------------------------------------------------------------------------------
  UpdateNoteBookClientWidget
  Params: ANoteBook: TObject

  This procedure updates the 'Fixed' object data.
  * obsolete *
------------------------------------------------------------------------------}
procedure UpdateNoteBookClientWidget(ANoteBook: TObject);
var
  ClientWidget: PGtkWidget;
  NoteBookWidget: PGtkNotebook;
begin
  if not TCustomNotebook(ANoteBook).HandleAllocated then exit;
  NoteBookWidget:=PGtkNotebook(TCustomNotebook(ANoteBook).Handle);
  ClientWidget:=nil;
  SetFixedWidget(NoteBookWidget,ClientWidget);
end;

{-------------------------------------------------------------------------------
 Some need the LCLobject which created this widget.

-------------------------------------------------------------------------------}
procedure SetLCLObject(const Widget: Pointer; const AnObject: TObject);
begin
  if (Widget <> nil) then
    gtk_object_set_data(Widget, 'Class', Pointer(AnObject));
end;

function GetLCLObject(const Widget: Pointer): TObject;
begin
  Result := TObject(gtk_object_get_data(Widget, 'Class'));
end;

function GetParentLCLObject(Widget: PGtkWidget): TObject;
begin
  while (Widget<>nil) do begin
    Result:=GetLCLObject(Widget);
    if Result<>nil then exit;
    Widget:=Widget^.Parent;
  end;
  Result:=nil;
end;

{-------------------------------------------------------------------------------
 Some need the HiddenLCLobject which created a parent of this widget.

-------------------------------------------------------------------------------}
procedure SetHiddenLCLObject(const Widget: Pointer; const AnObject: TObject);
begin
  if (Widget <> nil) then
    gtk_object_set_data(Widget, 'LCLHiddenClass', Pointer(AnObject));
end;

function GetHiddenLCLObject(const Widget: Pointer): TObject;
begin
  Result := TObject(gtk_object_get_data(Widget, 'LCLHiddenClass'));
end;

{-------------------------------------------------------------------------------
  GetWidgetScreenPos

  Returns the absolute left top position of a widget on the screen.
-------------------------------------------------------------------------------}
function GetWidgetOrigin(TheWidget: PGtkWidget): TPoint;
var
  TheWindow: PGdkWindow;
  {$IFDEF RaiseExceptionOnNilPointers}
  LCLObject: TObject;
  {$ENDIF}
begin
  TheWindow:=GetControlWindow(TheWidget);
  if TheWindow<>nil then
    gdk_window_get_origin(TheWindow,@Result.X,@Result.Y)
  else begin
    {$IFDEF RaiseExceptionOnNilPointers}
    LCLobject:=GetLCLObject(TheWidget);
    write('GetWidgetOrigin ');
    if LCLObject=nil then
      write(' LCLObject=nil')
    else if LCLObject is TControl then
      write(' LCLObject=',TControl(LCLObject).Name,':',TControl(LCLObject).ClassName)
    else
      write(' LCLObject=',TControl(LCLObject).ClassName);
    writeln('');
    RaiseException('GetWidgetOrigin Window=nil');
    {$ENDIF}
    Result.X:=0;
    Result.Y:=0;
  end;
  // check if the gdkwindow is the clientwindow of the parent
  if gtk_widget_get_parent_window(TheWidget)=TheWindow then begin
    // the widget is using its parent window
    // -> adjust the coordinates
    inc(Result.X,TheWidget^.Allocation.X);
    inc(Result.Y,TheWidget^.Allocation.Y);
  end;
end;

{-------------------------------------------------------------------------------
  GetWidgetClientScreenPos

  Returns the absolute left top position of a widget's client area
  on the screen.
-------------------------------------------------------------------------------}
function GetWidgetClientOrigin(TheWidget: PGtkWidget): TPoint;
var
  ClientWidget: PGtkWidget;
  ClientWindow: PGdkWindow;
begin
  ClientWidget:=GetFixedWidget(TheWidget);
  if ClientWidget<>TheWidget then begin
    ClientWindow:=GetControlWindow(ClientWidget);
    if ClientWindow<>nil then begin
      gdk_window_get_origin(ClientWindow,@Result.X,@Result.Y);
      exit;
    end;
  end;
  Result:=GetWidgetOrigin(TheWidget);
end;

{-------------------------------------------------------------------------------
  TranslateGdkPointToClientArea

  Translates SourcePos relative to SourceWindow to a coordinate relative to the
  client area of the LCL WinControl.
-------------------------------------------------------------------------------}
function TranslateGdkPointToClientArea(SourceWindow: PGdkWindow;
  SourcePos: TPoint;  DestinationWidget: PGtkWidget): TPoint;
var
  SrcWindowOrigin: TPoint;
  ClientAreaWindowOrigin: TPoint;
  Src2ClientAreaVector: TPoint;
begin
  if SourceWindow=nil then begin
    {$IFDEF RaiseExceptionOnNilPointers}
    RaiseException('TranslateGdkPointToClientArea Window=nil');
    {$ENDIF}
    writeln('WARNING: TranslateGdkPointToClientArea SourceWindow=nil');
  end;
  gdk_window_get_origin(SourceWindow,@SrcWindowOrigin.X,@SrcWindowOrigin.Y);
  ClientAreaWindowOrigin:=GetWidgetClientOrigin(DestinationWidget);
  Src2ClientAreaVector.X:=ClientAreaWindowOrigin.X-SrcWindowOrigin.X;
  Src2ClientAreaVector.Y:=ClientAreaWindowOrigin.Y-SrcWindowOrigin.Y;
  Result.X:=SourcePos.X-Src2ClientAreaVector.X;
  Result.Y:=SourcePos.Y-Src2ClientAreaVector.Y;
end;

{------------------------------------------------------------------------------
  Function: UpdateMouseCaptureControl
  Params: none
  Returns:  none

  Sets MCaptureControl to the current capturing widget.
 ------------------------------------------------------------------------------}
procedure UpdateMouseCaptureControl;
var
  OldMouseCaptureWidget,
  CurMouseCaptureWidget: PGtkWidget;
begin
  OldMouseCaptureWidget:=MouseCaptureWidget;
  CurMouseCaptureWidget:=gtk_grab_get_current;

  if OldMouseCaptureWidget<>CurMouseCaptureWidget then begin
    // notify the new capture control
    MouseCaptureWidget:=CurMouseCaptureWidget;
    MouseCapureByLCL:=false;
    if MouseCaptureWidget<>nil then
      SendMessage(HWnd(MouseCaptureWidget), LM_CAPTURECHANGED, 0,
        HWnd(OldMouseCaptureWidget));
  end;
end;

{------------------------------------------------------------------------------
  procedure ReleaseLCLMouseCapture;

  If the current mouse capture was captured by the LCL, release the capture.
 ------------------------------------------------------------------------------}
procedure ReleaseMouseCapture(OnlyIfCapturedByLCL: boolean);
var
  OldCaptureWidget: PGtkWidget;
begin
  if OnlyIfCapturedByLCL and not MouseCapureByLCL then exit;
  {$IfNDef ActivateMouseCapture}
  exit;
  {$EndIf}
  repeat
    OldCaptureWidget:=gtk_grab_get_current;
    if OldCaptureWidget<>nil then
      gtk_grab_remove(OldCaptureWidget)
    else
      break;
  until false;
end;

{------------------------------------------------------------------------------
  procedure: SetCursor
  Params:  AWinControl : TWinControl
  Returns: Nothing

  Sets the cursor for a widget.
 ------------------------------------------------------------------------------}
procedure SetCursor(AWinControl : TWinControl; Data: Pointer);

  procedure DoSetCursor(AWindow: PGdkWindow; Cursor: pGDKCursor);
  begin
    if Cursor <> nil then
      gdk_window_set_cursor(AWindow, Cursor);
  end;

  procedure SetDesigningCursor(AWindow: PGdkWindow; Cursor: PGdkCursor);
  var
    ChildWindows, ListEntry: PGList;
  begin
    DoSetCursor(AWindow, Cursor);
    
    ChildWindows:=gdk_window_get_children(AWindow);
    ListEntry:=ChildWindows;
    while ListEntry<>nil do begin
      SetDesigningCursor(PGdkWindow(ListEntry^.Data), Cursor);
      ListEntry:=ListEntry^.Next;
    end;
    g_list_free(ChildWindows);
  end;

var
  AWidget, FixWidget: PGtkWidget;
  AWindow: PGdkWindow;
  NewCursor: PGdkCursor;
begin
  if not ((AWinControl is TWinControl) and AWinControl.HandleAllocated)
  then exit;
  
  AWidget:= PGtkWidget(AWinControl.Handle);

  if csDesigning in AWinControl.ComponentState then begin

    AWindow:=GetControlWindow(AWidget);
    if AWindow = nil then exit;
    if Data = nil then
      SetDesigningCursor(AWindow, GetGDKMouseCursor(crDefault))
    else begin
      NewCursor:= GetGDKMouseCursor(Integer(Data));
      if NewCursor <> nil then SetDesigningCursor(AWindow, NewCursor);
    end;  
    
  end else begin

    FixWidget:= GetFixedWidget(AWidget);
    AWindow:= GetControlWindow(FixWidget);
    if AWindow = nil then exit;
    
    NewCursor:= GetGDKMouseCursor(AWinControl.Cursor);
    if NewCursor <> nil then DoSetCursor(AWindow, NewCursor);

  end;
end;

function GtkWindowIsModal(GtkWindow: PGtkWindow): boolean;
begin
  writeln('GtkWindowIsModal  This test is buggy');
  Result := False;
  {$IfDef GTK2}
    if assigned(GtkWindow) then
      result := gtk_window_get_modal(GtkWindow);
  {$else}
    if assigned(GtkWindow) then
      Result:=(GtkWindow^.flag0 and bm_modal)<>0;
  {$EndIF}
end;

{-------------------------------------------------------------------------------
  procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
    const ACallBackProc: Pointer; const ReqSignalMask: TGdkEventMask;
    Flags: TConnectSignalFlags);
    
  Connects a gtk signal handler.
-------------------------------------------------------------------------------}
procedure InitDesignSignalMasks;
var
  SignalType: TDesignSignalType;
begin
  DesignSignalMasks[dstUnknown]:=0;
  for SignalType:=Low(TDesignSignalType) to High(TDesignSignalType) do
    DesignSignalMasks[SignalType]:=1 shl ord(SignalType);
end;

function DesignSignalNameToType(Name: PChar; After: boolean): TDesignSignalType;
begin
  for Result:=Low(TDesignSignalType) to High(TDesignSignalType) do
    if ComparePChar(DesignSignalNames[Result],Name)
    and (DesignSignalAfter[Result]=After) then exit;
  Result:=dstUnknown;
end;

function GetDesignSignalMask(Widget: PGtkWidget): TDesignSignalMask;
begin
  Result:=TDesignSignalMask(gtk_object_get_data(PGtkObject(Widget),
                                                'LCLDesignMask'));
end;

procedure SetDesignSignalMask(Widget: PGtkWidget; NewMask: TDesignSignalMask);
begin
  gtk_object_set_data(PGtkObject(Widget),'LCLDesignMask',Pointer(NewMask));
end;

function GetDesignOnlySignalFlag(Widget: PGtkWidget;
  DesignSignalType: TDesignSignalType): boolean;
begin
  Result:=(GetDesignSignalMask(Widget)
          and DesignSignalMasks[DesignSignalType])<>0;
end;

procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask; SFlags: TConnectSignalFlags);
var
  RealizeHandler, Handler: PGTKHandler;
  RealizeID, SignalID: guint;
  WinWidgetInfo: PWinWidgetInfo;
  MainWidget: PGtkWidget;
  OldDesignMask, NewDesignMask: TDesignSignalMask;
  DesignSignalType: TDesignSignalType;
begin
  if ACallBackProc = nil then exit;

  // first loop through the handlers to:
  // - check if a handler already exists
  // - Find the realize handler to change data
  Handler := gtk_object_get_data_by_id (AnObject, gtk_handler_quark);
  SignalID := gtk_signal_lookup(ASignal, GTK_OBJECT_TYPE(AnObject));
  if csfConnectRealize in SFlags then
    RealizeID := gtk_signal_lookup('realize', GTK_OBJECT_TYPE(AnObject))
  else
    RealizeID := 0;
  RealizeHandler := nil;
  DesignSignalType:=DesignSignalNameToType(ASignal,csfAfter in SFlags);

  while (Handler <> nil) do begin
    with Handler^ do
    begin
      // check if signal is already connected
      if  (Id > 0)
      and (Signal_ID = SignalID)
      and (Func = TGTKSignalFunc(ACallBackProc))
      and (func_data = Pointer(LCLComponent))
      and (((flags and bmSignalAfter)<>0)=(csfAfter in SFlags))
      then begin
        Assert(False, Format('Trace:WARNING: [TGTKObject.SetCallback] %s signal <%s> set twice', [LCLComponent.ClassName, ASignal]));
        // signal is already connected
        // update the DesignSignalMask
        if (DesignSignalType<>dstUnknown)
        and (not (csfDesignOnly in SFlags)) then begin
          OldDesignMask:=GetDesignSignalMask(PGtkWidget(AnObject));
          NewDesignMask:=
            OldDesignMask and not DesignSignalMasks[DesignSignalType];
          if OldDesignMask<>NewDesignMask then
            SetDesignSignalMask(PGtkWidget(AnObject),NewDesignMask);
        end;
        Exit;
      end;

      // look for realize handler
      if  (csfConnectRealize in SFlags)
      and (Id > 0)
      and (Signal_ID = RealizeID)
      and (Func = TGTKSignalFunc(@GTKRealizeCB))
      and (func_data = Pointer(LCLComponent))
      and ((flags and bmSignalAfter)=0) // test if not after
      then
        RealizeHandler := Handler;

      Handler := Next;
    end;
  end;

  // if we are here, then no handler was defined yet
  // -> register handler
  //if (Msg=LM_LBUTTONUP) then writeln('CONNECT ',ReqSignalMask,' Widget=',HexStr(Cardinal(AnObject),8));
  if csfAfter in SFlags then
    gtk_signal_connect_after(AnObject, ASignal,
                             TGTKSignalFunc(ACallBackProc),LCLComponent)
  else
    gtk_signal_connect      (AnObject, ASignal,
                             TGTKSignalFunc(ACallBackProc),LCLComponent);

  // update signal mask which will be set in the realize handler
  if (csfUpdateSignalMask in SFlags) and (ReqSignalMask <> 0) then begin
    MainWidget:=GetMainWidget(PGtkWidget(AnObject));
    if MainWidget=nil then
      MainWidget:=PGtkWidget(AnObject);
    WinWidgetInfo:=GetWidgetInfo(MainWidget,true);
    WinWidgetInfo^.EventMask:=WinWidgetInfo^.EventMask or ReqSignalMask;
  end;
  // -> register realize handler
  if (csfConnectRealize in SFlags)
  and (RealizeHandler = nil) and (RealizeID<>0) then begin
    //writeln('REALIZE CONNECT Widget=',HexStr(Cardinal(AnObject),8));
    gtk_signal_connect(AnObject, 'realize',
      TGTKSignalFunc(@GTKRealizeCB), LCLComponent);
    gtk_signal_connect_after(AnObject, 'realize',
      TGTKSignalFunc(@GTKRealizeAfterCB), LCLComponent);
  end;

  // update the DesignSignalMask
  if (DesignSignalType<>dstUnknown) then begin
    OldDesignMask:=GetDesignSignalMask(PGtkWidget(AnObject));
    if csfDesignOnly in SFlags then
      NewDesignMask:=OldDesignMask or DesignSignalMasks[DesignSignalType]
    else
      NewDesignMask:=OldDesignMask and not DesignSignalMasks[DesignSignalType];
    if OldDesignMask<>NewDesignMask then
      SetDesignSignalMask(PGtkWidget(AnObject),NewDesignMask);
  end;
end;

procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc,LCLComponent,ReqSignalMask,
                [csfConnectRealize,csfUpdateSignalMask]);
end;

procedure ConnectSignalAfter(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent;
  const ReqSignalMask: TGdkEventMask);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc,LCLComponent,ReqSignalMask,
                [csfConnectRealize,csfUpdateSignalMask,csfAfter]);
end;

procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc,LCLComponent,0);
end;

procedure ConnectSignalAfter(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; LCLComponent: TComponent);
begin
  ConnectSignalAfter(AnObject,ASignal,ACallBackProc,LCLComponent,0);
end;

{------------------------------------------------------------------------------
  procedure: ConnectInternalWidgetsSignals
  Params:  AWidget: PGtkWidget; AWinControl: TWinControl
  Returns: Nothing

  Connects hidden child widgets signals.
  Many gtk widgets create internally child widgets (e.g. scrollbars). In
  Design mode these widgets should not auto react themselves, but instead send
  messages to the lcl. Therefore these widgets are connected also to our
  signal handlers.
  This procedure is called by the realize-after handler of all LCL widgets
  and each time the design mode of a LCL control changes.
 ------------------------------------------------------------------------------}
procedure ConnectInternalWidgetsSignals(AWidget: PGtkWidget;
  AWinControl: TWinControl);

  function WidgetIsInternal(TheWidget: PGtkWidget): boolean;
  begin
    Result:=(TheWidget<>nil)
      and (PGtkWidget(AWinControl.Handle)<>TheWidget)
      and (GetMainWidget(TheWidget)=nil);
  end;

  procedure ConnectSignals(TheWidget: PGtkWidget); forward;

  procedure ConnectChilds(TheWidget: PGtkWidget);
  var
    ContainerWidget: PGtkContainer;
    ScrolledWindow: PGtkScrolledWindow;
    BinWidget: PGtkBin;
    ChildEntry: PGSList;
    ChildWidget: PGtkWidget;
  begin
    //if AWinControl is TListView then writeln('ConnectChilds A ',HexStr(Cardinal(TheWidget),8));
    if GtkWidgetIsA(TheWidget,GTK_TYPE_CONTAINER) then begin
      //if AWinControl is TListView then writeln('ConnectChilds B ');
      // this is a container widget -> connect all childs
      ContainerWidget:=PGtkContainer(TheWidget);
      {$IfDef GTK2}
      {$else}
      ChildEntry:=ContainerWidget^.resize_widgets;
      {$endif}
      while ChildEntry<>nil do begin
        ChildWidget:=PGtkWidget(ChildEntry^.Data);
        ConnectSignals(ChildWidget);
        ChildEntry:=ChildEntry^.Next;
      end;
    end;
    if GtkWidgetIsA(TheWidget,GTK_TYPE_BIN) then begin
      //if AWinControl is TListView then writeln('ConnectChilds C ');
      BinWidget:=PGtkBin(TheWidget);
      ConnectSignals(BinWidget^.child);
    end;
    if GtkWidgetIsA(TheWidget,GTK_TYPE_SCROLLED_WINDOW) then begin
      //if AWinControl is TListView then writeln('ConnectChilds D ');
      ScrolledWindow:=PGtkScrolledWindow(TheWidget);
      ConnectSignals(ScrolledWindow^.hscrollbar);
      ConnectSignals(ScrolledWindow^.vscrollbar);
    end;
    if GtkWidgetIsA(TheWidget,GTK_TYPE_COMBO) then begin
      //if AWinControl is TListView then writeln('ConnectChilds E ');
      ConnectSignals(PGtkCombo(TheWidget)^.entry);
      ConnectSignals(PGtkCombo(TheWidget)^.button);
    end;
  end;

  procedure ConnectSignals(TheWidget: PGtkWidget);
  var
    LCLObject, HiddenLCLObject: TObject;
    DesignSignalType: TDesignSignalType;
    DesignFlags: TConnectSignalFlags;
  begin
    //if AWinControl is TListView then writeln('ConnectSignals A ',HexStr(Cardinal(TheWidget),8));
    if TheWidget=nil then exit;
    
    // check if TheWidget belongs to another LCL object
    LCLObject:=GetLCLObject(TheWidget);
    HiddenLCLObject:=GetHiddenLCLObject(TheWidget);
    if (LCLObject<>nil) and (LCLObject<>AWinControl) then begin
      exit;
    end;
    if (HiddenLCLObject<>nil) and (HiddenLCLObject<>AWinControl) then begin
      exit;
    end;

    //if AWinControl is TListView then writeln('ConnectSignals B ',HexStr(Cardinal(TheWidget),8));
    // connect signals needed for design mode:
    for DesignSignalType:=Low(TDesignSignalType) to High(TDesignSignalType) do
    begin
      if DesignSignalType=dstUnknown then continue;
      DesignFlags:=[csfDesignOnly];
      if DesignSignalAfter[DesignSignalType] then
        Include(DesignFlags,csfAfter);
      ConnectSignal(PGtkObject(TheWidget),DesignSignalNames[DesignSignalType],
                    DesignSignalFuncs[DesignSignalType],AWinControl,0,
                    DesignFlags);
    end;

    if WidgetIsInternal(TheWidget) then
      // mark widget as 'hidden' connected
      SetHiddenLCLObject(TheWidget,AWinControl);

    // connect recursively ...
    ConnectChilds(TheWidget);
  end;
  
begin
  if (AWinControl=nil) or (AWidget=nil)
  or (not (csDesigning in AWinControl.ComponentState)) then exit;
  ConnectSignals(AWidget);
end;

// ----------------------------------------------------------------------
// The Accelgroup and AccelKey is needed by menus
// ----------------------------------------------------------------------
function GetAccelGroup(const Widget: PGtkWidget;
  CreateIfNotExists: boolean): PGTKAccelGroup;
begin
  Result := PGTKAccelGroup(gtk_object_get_data(PGtkObject(Widget),'AccelGroup'));
  if (Result=nil) and CreateIfNotExists then begin
    {$IFDEF VerboseAccelerator}
    writeln('GetAccelGroup CREATING  Widget=',HexStr(Cardinal(Widget),8),' CreateIfNotExists=',CreateIfNotExists);
    {$ENDIF}
    Result:=gtk_accel_group_new;
    SetAccelGroup(Widget,Result);
    if GtkWidgetIsA(Widget,GTK_TYPE_WINDOW) then
      ShareWindowAccelGroups(Widget);
  end;
end;

procedure SetAccelGroup(const Widget: PGtkWidget;
  const AnAccelGroup: PGTKAccelGroup);
begin
  if (Widget = nil) then exit;
  gtk_object_set_data(PGtkObject(Widget), 'AccelGroup', AnAccelGroup);
  if AnAccelGroup<>nil then begin
    // attach group to widget
    {$IFDEF VerboseAccelerator}
    writeln('SetAccelGroup AnAccelGroup=',HexStr(Cardinal(AnAccelGroup),8),' IsMenu=',GtkWidgetIsA(Widget,GTK_MENU_TYPE));
    {$ENDIF}
    if GtkWidgetIsA(Widget,GTK_TYPE_MENU) then
      gtk_menu_set_accel_group(PGtkMenu(Widget), AnAccelGroup)
    else begin
      {$IfDef GTK2}
      {$else}
      gtk_accel_group_attach(AnAccelGroup, PGtkObject(Widget));
      {$endif}
    end;
  end;
end;

procedure FreeAccelGroup(const Widget: PGtkWidget);
var
  AccelGroup: PGTKAccelGroup;
begin
  AccelGroup:=GetAccelGroup(Widget,false);
  if AccelGroup<>nil then begin
    {$IFDEF VerboseAccelerator}
    writeln('FreeAccelGroup  AccelGroup=',HexStr(Cardinal(AccelGroup),8));
    {$ENDIF}
    gtk_accel_group_unref(AccelGroup);
    SetAccelGroup(Widget,nil);
  end;
end;

procedure ShareWindowAccelGroups(AWindow: PGtkWidget);

  procedure AttachUnique(TheWindow: PGtkWidget; TheAccelGroup: PGTKAccelGroup);
  begin
    {$IfDef GTK2}
    {$else}
    if (TheAccelGroup=nil)
    or ((TheAccelGroup^.attach_objects<>nil)
      and (g_slist_find(TheAccelGroup^.attach_objects, TheWindow)<>nil))
    then
      exit;
    gtk_accel_group_attach(TheAccelGroup, PGtkObject(TheWindow));
    {$endif}
  end;

var
  TheForm, CurForm: TCustomForm;
  i: integer;
  TheAccelGroup, CurAccelGroup: PGTKAccelGroup;
  CurWindow: PGtkWidget;
begin
  TheForm:=TCustomForm(GetLCLObject(AWindow));

  // check if visible TCustomForm (not frame)
  if (TheForm=nil) or (not (TheForm is TCustomForm))
  or (not TheForm.Visible) or (TheForm.Parent<>nil)
  or (csDesigning in TheForm.ComponentState)
  then exit;
  
  // check if modal form
  if fsModal in TheForm.FormState then begin
    // a modal form does not share accelerators
    exit;
  end;

  // check if there is an accelerator group
  TheAccelGroup:=GetAccelGroup(AWindow,false);

  // this is a normal form
  // -> share accelerators with all other visible normal forms
  for i:=0 to Screen.FormCount-1 do begin
    CurForm:=Screen.Forms[i];
    if (CurForm=TheForm)
    or (not CurForm.HandleAllocated)
    or (not CurForm.Visible)
    or (fsModal in CurForm.FormState)
    or (CurForm.Parent<>nil)
    or (csDesigning in CurForm.ComponentState)
    then continue;
    
    CurWindow:=PGtkWidget(CurForm.Handle);
    CurAccelGroup:=GetAccelGroup(CurWindow,false);
    {$IFDEF VerboseAccelerator}
    writeln('ShareWindowAccelGroups ',TheForm.Name,':',TheForm.ClassName,
            ' <-> ',CurForm.Name,':',CurForm.ClassName);
    {$ENDIF}

    // cross connect
    AttachUnique(CurWindow,TheAccelGroup);
    AttachUnique(AWindow,CurAccelGroup);
  end;
end;

procedure UnshareWindowAccelGroups(AWindow: PGtkWidget);

  procedure Detach(TheWindow: PGtkWidget; TheAccelGroup: PGTKAccelGroup);
  begin
    {$IfDef GTK2}
    {$else}
    if (TheAccelGroup=nil)
    or (TheAccelGroup^.attach_objects=nil)
    or (g_slist_find(TheAccelGroup^.attach_objects, TheWindow)=nil) then
      exit;
    gtk_accel_group_detach(TheAccelGroup, PGtkObject(TheWindow));
    {$endif}
  end;

var
  TheForm, CurForm: TCustomForm;
  i: integer;
  TheAccelGroup, CurAccelGroup: PGTKAccelGroup;
  CurWindow: PGtkWidget;
begin
  TheForm:=TCustomForm(GetLCLObject(AWindow));

  // check if TCustomForm
  if (TheForm=nil) or (not (TheForm is TCustomForm))
  then exit;

  TheAccelGroup:=GetAccelGroup(AWindow,false);

  // -> unshare accelerators with all other forms
  for i:=0 to Screen.FormCount-1 do begin
    CurForm:=Screen.Forms[i];
    if (CurForm=TheForm)
    or (not CurForm.HandleAllocated)
    then continue;

    CurWindow:=PGtkWidget(CurForm.Handle);
    CurAccelGroup:=GetAccelGroup(CurWindow,false);
    {$IFDEF VerboseAccelerator}
    writeln('UnshareWindowAccelGroups ',TheForm.Name,':',TheForm.ClassName,
            ' <-> ',CurForm.Name,':',CurForm.ClassName);
    {$ENDIF}

    // unlink
    Detach(CurWindow,TheAccelGroup);
    Detach(AWindow,CurAccelGroup);
  end;
end;

function GetAccelGroupForComponent(Component: TComponent;
  CreateIfNotExists: boolean): PGTKAccelGroup;
var
  Control: TControl;
  MenuItem: TMenuItem;
  Form: TCustomForm;
  Menu: TMenu;
begin
  Result:=nil;
  if Component=nil then exit;
  
  if Component is TMenuItem then begin
    MenuItem:=TMenuItem(Component);
    Menu:=MenuItem.GetParentMenu;
    if (Menu=nil) or (Menu.Parent=nil) then exit;
    {$IFDEF VerboseAccelerator}
    writeln('GetAccelGroupForComponent A ',Component.Name,':',Component.ClassName);
    {$ENDIF}
    Result:=GetAccelGroupForComponent(Menu.Parent,CreateIfNotExists);
  end else if Component is TControl then begin
    Control:=TControl(Component);
    while Control.Parent<>nil do Control:=Control.Parent;
    if Control is TCustomForm then begin
      Form:=TCustomForm(Control);
      if Form.HandleAllocated then begin
        Result:=GetAccelGroup(PGtkWidget(Form.Handle),CreateIfNotExists);
        {$IFDEF VerboseAccelerator}
        writeln('GetAccelGroupForComponent C ',Component.Name,':',Component.ClassName);
        {$ENDIF}
      end;
    end;
  end;
  {$IFDEF VerboseAccelerator}
  writeln('GetAccelGroupForComponent END ',Component.Name,':',Component.ClassName,' Result=',HexStr(Cardinal(Result),8));
  {$ENDIF}
end;

function GetAccelKey(Widget: PGtkWidget): PAcceleratorKey;
begin
  Result := PAcceleratorKey(gtk_object_get_data(PGtkObject(Widget),'AccelKey'));
end;

{$Ifdef GTK2}
const
  GDK_VOIDSYMBOL = $FFFFFF;
{$EndIf}

function SetAccelKey(const Widget: PGtkWidget;
  Key: guint; Mods: TGdkModifierType; const Signal: string): PAcceleratorKey;
begin
  if (Widget = nil) then exit;
  Result:=GetAccelKey(Widget);
  if Result=nil then begin
    if Key<>GDK_VOIDSYMBOL then begin
      New(Result);
      FillChar(Result^,SizeOf(Result),0);
    end;
  end else begin
    if Key=GDK_VOIDSYMBOL then begin
      Dispose(Result);
      Result:=nil;
    end;
  end;
  if (Result<>nil) then begin
    Result^.Key:=Key;
    Result^.Mods:=Mods;
    Result^.Signal:=Signal;
    Result^.Realized:=false;
  end;
  {$IFDEF VerboseAccelerator}
  writeln('SetAccelKey Widget=',HexStr(Cardinal(Widget),8),
    ' Key=',Key,' Mods=',HexStr(Cardinal(Mods),8),
    ' Signal="',Signal,'" Result=',HexStr(Cardinal(Result),8));
  {$ENDIF}
  gtk_object_set_data(PGtkObject(Widget), 'AccelKey', Result);
end;

procedure ClearAccelKey(Widget: PGtkWidget);
begin
  SetAccelKey(Widget,GDK_VOIDSYMBOL,0,'');
end;

procedure RealizeAccelerator(Component: TComponent; Widget : PGtkWidget);
var
  AccelKey: PAcceleratorKey;
  AccelGroup: PGTKAccelGroup;
begin
  if (Component=nil) or (Widget=nil) then
    RaiseException('RealizeAccelerate: invalid input');

  // Set the accelerator
  AccelKey:=GetAccelKey(Widget);
  if (AccelKey=nil) or (AccelKey^.Realized) then exit;
  
  if AccelKey^.Key<>GDK_VOIDSYMBOL then begin
    AccelGroup:=GetAccelGroupForComponent(Component,true);
    if AccelGroup<>nil then begin
      {$IFDEF VerboseAccelerator}
      writeln('RealizeAccelerator Add Accelerator ',
        Component.Name,':',Component.ClassName,
        ' Widget=',HexStr(Cardinal(Widget),8),
        ' Signal=',AccelKey^.Signal,
        ' Key=',AccelKey^.Key,' Mods=',AccelKey^.Mods,
        '');
      {$ENDIF}
      gtk_widget_add_accelerator(Widget, PChar(AccelKey^.Signal),
        AccelGroup, AccelKey^.Key, AccelKey^.Mods, GTK_ACCEL_VISIBLE);
      AccelKey^.Realized:=true;
    end else begin
      AccelKey^.Realized:=false;
    end;
  end else begin
    AccelKey^.Realized:=true;
  end;
end;

procedure UnrealizeAccelerator(Widget : PGtkWidget);
var
  AccelKey: PAcceleratorKey;
begin
  if (Widget=nil) then
    RaiseException('UnrealizeAccelerate: invalid input');
    
  AccelKey:=GetAccelKey(Widget);
  if (AccelKey=nil) or (not AccelKey^.Realized) then exit;

  if AccelKey^.Signal<>'' then begin
    {$IFDEF VerboseAccelerator}
    writeln('UnrealizeAccelerator  ',
      ' Widget=',HexStr(Cardinal(Widget),8),
      ' Signal=',AccelKey^.Signal,
      ' Key=',AccelKey^.Key,' Mods=',AccelKey^.Mods,
      '');
    {$ENDIF}
    {$Ifdef GTK2}
    {$else}
    gtk_widget_remove_accelerators(Widget, PChar(AccelKey^.Signal), false);
    {$EndIf}
  end;
  AccelKey^.Realized:=false;
end;

procedure RegroupAccelerator(Widget: PGtkWidget);
begin
  UnrealizeAccelerator(Widget);
  RealizeAccelerator(TComponent(GetLCLObject(Widget)),Widget);
end;

procedure Accelerate(Component: TComponent; const Widget : PGtkWidget;
  const Key: guint; Mods: TGdkModifierType; const Signal : string);
var
  OldAccelKey: PAcceleratorKey;
begin
  if (Component=nil) or (Widget=nil) or (Signal='') then
    RaiseException('Accelerate: invalid input');
  {$IFDEF VerboseAccelerator}
  writeln('Accelerate ',Component.Name,':',Component.ClassName,' Key=',Key,' Mods=',HexStr(Cardinal(Mods),8),' Signal=',Signal);
  {$ENDIF}
  
  // delete old accelerator key
  OldAccelKey:=GetAccelKey(Widget);
  if (OldAccelKey <> nil) then begin
    if (OldAccelKey^.Key=Key) and (OldAccelKey^.Mods=Mods)
    and (OldAccelKey^.Signal=Signal)
    then begin
      // no change
      exit;
    end;
      
    UnrealizeAccelerator(Widget);
  end;

  // Set the accelerator
  SetAccelKey(Widget,Key,Mods,Signal);
  if (Key<>GDK_VOIDSYMBOL) and (not (csDesigning in Component.ComponentState))
  then
    RealizeAccelerator(Component,Widget);
end;

procedure Accelerate(Component: TComponent; const Widget : PGtkWidget;
  const Msg: TLMShortCut; const Signal : string);
var
  GDKModifier: TGdkModifierType;
  GDKKey: guint;
begin
  { Map the shift states }
  GDKModifier:= 0;
  if ssShift in Msg.NewModifier then GDKModifier:= GDK_SHIFT_MASK;
  if ssAlt in Msg.NewModifier then GDKModifier:= GDKModifier + GDK_MOD1_MASK;
  if ssCtrl in Msg.NewModifier then GDKModifier:= GDKModifier + GDK_CONTROL_MASK;
  GDKKey:= VK2GDK(Msg.NewKey);

  Accelerate(Component,Widget,GDKKey,GDKModifier,Signal);
end;


{------------------------------------------------------------------------------
  procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
    var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);

  Extracts some information from the Handle of a TGraphic
 ------------------------------------------------------------------------------}
procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
var
  GDIObject: PGdiObject;
begin
  IconImg:=nil;
  IconMask:=nil;
  Width:=0;
  Height:=0;
  if (LCLGraphic=nil) then exit;
  if LCLGraphic is TBitmap then
    GDIObject:=PgdiObject(TBitmap(LCLGraphic).Handle)
  else
    GDIObject:=nil;
  if GDIObject<>nil then begin
    IconImg:=GDIObject^.GDIBitmapObject;
    IconMask:=GDIObject^.GDIBitmapMaskObject;
    if IconImg<>nil then
      gdk_window_get_size (IconImg, @Width, @Height);
  end;
end;

{------------------------------------------------------------------------------
  procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
    var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);

  Extracts some information from the Handle of the icon of a TMenuItem
 ------------------------------------------------------------------------------}
procedure GetGdkPixmapFromMenuItem(LCLMenuItem: TMenuItem;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
begin
  IconImg:=nil;
  IconMask:=nil;
  Width:=0;
  Height:=0;
  if LCLMenuItem=nil then exit;
  if LCLMenuItem.Graphic<>nil then
    GetGdkPixmapFromGraphic(LCLMenuItem.Graphic,IconImg,IconMask,Width,Height);
end;

{------------------------------------------------------------------------------
  function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;

  Returns the gtk klass of a menuitem widget.
 ------------------------------------------------------------------------------}
function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;
begin
  {$ifdef GTK2}
  Result:=GTK_MENU_ITEM_CLASS(gtk_object_get_class(widget));
  {$else}
  Result:=GTK_MENU_ITEM_CLASS(PGtkObject(widget)^.klass);
  {$endif}
end;

{------------------------------------------------------------------------------
  function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;

  Returns the gtk klass of a checkmenuitem widget.
 ------------------------------------------------------------------------------}
function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;
begin
  {$ifdef GTK2}
  Result:=GTK_CHECK_MENU_ITEM_CLASS(gtk_object_get_class(widget));
  {$else}
  Result:=GTK_CHECK_MENU_ITEM_CLASS(PGtkObject(widget)^.klass);
  {$endif}
end;

{------------------------------------------------------------------------------
  function GetRadioMenuItemGroup(LCLMenuItem: TMenuItem): PGSList;

  Returns the radio group list with the GroupIndex of the MenuItem
 ------------------------------------------------------------------------------}
function GetRadioMenuItemGroup(LCLMenuItem: TMenuItem): PGSList;
var
  ParentMenuItem: TMenuItem;
  i: integer;
begin
  Result:=nil;
  if (LCLMenuItem=nil) or (LCLMenuItem.GroupIndex=0) then exit;
  ParentMenuItem:=LCLMenuItem.Parent;
  if ParentMenuItem=nil then exit;
  for i:=0 to ParentMenuItem.Count-1 do begin
    if ParentMenuItem[i].RadioItem
    and (ParentMenuItem[i].GroupIndex=LCLMenuItem.GroupIndex)
    and (ParentMenuItem[i]<>LCLMenuItem)
    and ParentMenuItem[i].HandleAllocated
    and GtkWidgetIsA(Pointer(ParentMenuItem[i].Handle),
                      GTK_TYPE_RADIO_MENU_ITEM) then
    begin
      Result:=gtk_radio_menu_item_group(
                                 GTK_RADIO_MENU_ITEM(Pointer(ParentMenuItem[i].Handle)));
      exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function GetRadioMenuItemGroup(MenuItem: PGtkRadioMenuItem): PGSList;

  Returns the radio group list with the GroupIndex of the MenuItem
 ------------------------------------------------------------------------------}
function GetRadioMenuItemGroup(MenuItem: PGtkRadioMenuItem): PGSList;
begin
  if MenuItem=nil then
    Result:=nil
  else
    Result:=GetRadioMenuItemGroup(TMenuItem(GetLCLObject(MenuItem)));
end;

{------------------------------------------------------------------------------
  procedure UpdateRadioGroupChecks(RadioGroup: PGSList);

  Set 'checked' for all menuitems in the group
 ------------------------------------------------------------------------------}
procedure UpdateRadioGroupChecks(RadioGroup: PGSList);
var
  CurListItem: PGSList;
  MenuItem: PGtkMenuItem;
  LCLMenuItem: TMenuItem;
begin
  if RadioGroup=nil then exit;
  CurListItem:=RadioGroup;
  // set active radiomenuitem
  while CurListItem<>nil do begin
    MenuItem:=PGtkMenuItem(CurListItem^.Data);
    if MenuItem<>nil then begin
      LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
      if (LCLMenuItem<>nil) and LCLMenuItem.Checked then begin
        gtk_check_menu_item_set_active(PGtkCheckMenuItem(MenuItem),
                                       LCLMenuItem.Checked);
      end;
    end;
    CurListItem:=CurListItem^.Next;
  end;
  CurListItem:=RadioGroup;
  // deactivate the other radiomenuitems
  while CurListItem<>nil do begin
    MenuItem:=PGtkMenuItem(CurListItem^.Data);
    if MenuItem<>nil then begin
      LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
      if (LCLMenuItem<>nil) then begin
        gtk_check_menu_item_set_active(PGtkCheckMenuItem(MenuItem),
                                       LCLMenuItem.Checked);
      end;
    end;
    CurListItem:=CurListItem^.Next;
  end;
end;

{------------------------------------------------------------------------------
  procedure DrawMenuItemIcon(MenuItem: PGtkCheckMenuItem;
    area: PGdkRectangle); cdecl;

  Handler for drawing the icon of a menuitem.
 ------------------------------------------------------------------------------}
procedure DrawMenuItemIcon(MenuItem: PGtkCheckMenuItem;
  Area: PGdkRectangle); cdecl;
var
  Widget: PGtkWidget;
  Container: PgtkContainer;
  ALeft, ATop, BorderWidth: gint;
  LCLMenuItem: TMenuItem;
  IconImg, IconMask: PGdkPixmap;
  IconWidth, IconHeight: integer;
  AWindow: PGdkWindow;
begin
  if (MenuItem=nil) then exit;
  if not (GTK_WIDGET_DRAWABLE (PGtkWidget(MenuItem))) then exit;

  // get icon
  LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
  GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,IconWidth,IconHeight);
  if IconImg=nil then begin
    // call default draw function
    OldCheckMenuItemDrawProc(MenuItem,Area);
    exit;
  end;

  // calculate left and top
  Widget := PGtkWidget (MenuItem);
  AWindow:=GetControlWindow(Widget);
  if AWindow=nil then exit;
  Container := GTK_CONTAINER (MenuItem);
  BorderWidth := Container^.flag0 and bm_TGtkContainer_border_width;
  ALeft := (BorderWidth + gtk_widget_get_xthickness(gtk_widget_get_style(Widget)) + 2)
           +((PGtkMenuItem(MenuItem)^.toggle_size-IconWidth) div 2);
  ATop := (Widget^.Allocation.Height - IconHeight) div 2;

  // draw icon
  gdk_gc_set_clip_mask(gtk_widget_get_style(Widget)^.Black_gc, IconMask);
  gdk_gc_set_clip_origin(gtk_widget_get_style(Widget)^.Black_gc,ALeft,ATop);
  gdk_draw_pixmap(AWindow,gtk_widget_get_style(Widget)^.Black_gc,
     IconImg,0,0,ALeft,ATop,-1,-1);
  gdk_gc_set_clip_mask(gtk_widget_get_style(Widget)^.Black_gc, nil);
end;

{------------------------------------------------------------------------------
  procedure MenuSizeRequest(widget:PGtkWidget;
    requisition:PGtkRequisition); cdecl;

  SizeAllocate Handler for check menuitem widgets.
 ------------------------------------------------------------------------------}
procedure MenuSizeRequest(widget:PGtkWidget; requisition:PGtkRequisition); cdecl;
var
  CurToggleSize, MaxToggleSize: integer;
  MenuShell: PGtkMenuShell;
  ListItem: PGList;
  MenuItem, CheckMenuItem: PGtkMenuItem;
  LCLMenuItem: TMenuItem;
  IconImg, IconMask: PGdkPixmap;
  Width, Height: integer;
begin
  MaxToggleSize:=0;
  MenuShell:=GTK_MENU_SHELL(widget);
  ListItem:=MenuShell^.Children;
  CheckMenuItem:=nil;
  while ListItem<>nil do begin
    MenuItem:=PGtkMenuItem(ListItem^.Data);
    if GTK_IS_CHECK_MENU_ITEM(PGtkWidget(MenuItem)) then begin
      CheckMenuItem:=MenuItem;
      CurToggleSize:=OldCheckMenuItemToggleSize;
      LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
      if LCLMenuItem<>nil then begin
        GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,Width,Height);
        if IconImg<>nil then begin
          if CurToggleSize<Width then
            CurToggleSize:=Width;
        end;
      end;
      if MaxToggleSize<CurToggleSize then
        MaxToggleSize:=CurToggleSize;
    end;
    ListItem:=ListItem^.Next;
  end;
  {$Ifdef GTK2}
  {$else}
  if CheckMenuItem<>nil then
    MENU_ITEM_CLASS(PGtkWidget(CheckMenuItem))^.toggle_size:=MaxToggleSize;
  {$EndIf}
  OldMenuSizeRequestProc(Widget,requisition);
end;

{------------------------------------------------------------------------------
  procedure UpdateInnerMenuItem(LCLMenuItem: TMenuItem;
    MenuItemWidget: PGtkWidget);

  Update the inner widgets of a menuitem widget.
 ------------------------------------------------------------------------------}
procedure UpdateInnerMenuItem(LCLMenuItem: TMenuItem;
  MenuItemWidget: PGtkWidget);
var
  HBoxWidget: PGtkWidget;
  LabelWidget: PGtkAccelLabel;

  procedure SetMenuItemLabelText(LCLMenuItem: TMenuItem;
    MenuItemWidget: PGtkWidget);
  var
    ShortCutPos: integer;
    s: string;
    LabelWidget: PGtkLabel;
  begin
    if (MenuItemWidget=nil) or (LCLMenuItem=nil) then exit;
    LabelWidget:=gtk_object_get_data(PGtkObject(MenuItemWidget), 'LCLLabel');
    if LabelWidget=nil then begin
    end;
    //Check for a shortcut key
    s:=LCLMenuItem.Caption;
    ShortCutPos := pos('&', s);
    if ShortCutPos <> 0 then begin
      if (LCLMenuItem.Parent<>nil)
      and (LCLMenuItem.Parent.HandleAllocated)
      and GtkWidgetIsA(PGtkWidget(LCLMenuItem.Parent.Handle),GTK_TYPE_MENU_BAR)
      then begin
        // this is a menu item in the main bar of a form
        // -> accelerator should be Alt+Key
        s[ShortCutPos] := '_';
        Accelerate(LCLMenuItem,MenuItemWidget,
          gtk_label_parse_uline(LabelWidget,PChar(s)),
          GDK_MOD1_MASK,'activate_item');
      end else begin
        // Because gnome changes menuitem shortcuts via keyboard, we can't
        // set the accelerator.
        // It would be cool, to know if a window manager with the gnome feature
        // is running, but there is probably no reliable code to do that, so we
        // simply delete all ampersands and don't set the letter shortcut.
        DeleteAmpersands(s);
        gtk_label_set_text(LabelWidget,PChar(s));
        {Accelerate(LCLMenuItem,MenuItemWidget,
          gtk_label_parse_uline(LabelWidget,PChar(s)),0,'activate_item');}
      end;
    end
    else begin
      gtk_label_set_text(LabelWidget,PChar(s));
    end;
  end;

  procedure CreateIcon;
  var
    IconImg, IconMask: PGdkPixmap;
    IconWidth, IconHeight: integer;
    MinHeightWidget: PGtkWidget;
  begin
    // the icon will be painted instead of the toggle
    // of a normal gtkcheckmenuitem

    // get the icon
    GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,IconWidth,IconHeight);
    if IconImg<>nil then begin
      // set the toggle width
      GTK_MENU_ITEM(MenuItemWidget)^.toggle_size:=IconWidth;
      GTK_MENU_ITEM(MenuItemWidget)^.flag0:=
        PGtkMenuItem(MenuItemWidget)^.flag0
          or bm_show_toggle_indicator;

      // set our own draw handler
      if OldCheckMenuItemDrawProc=nil then
        OldCheckMenuItemDrawProc:=
          CHECK_MENU_ITEM_CLASS(MenuItemWidget)^.draw_indicator;
      CHECK_MENU_ITEM_CLASS(MenuItemWidget)^.draw_indicator:=@DrawMenuItemIcon;

      // add a dummy widget for the icon height
      MinHeightWidget:=gtk_label_new('');
      gtk_widget_show(MinHeightWidget);
      gtk_widget_set_usize(MinHeightWidget,1,IconHeight);
      gtk_box_pack_start(GTK_BOX(HBoxWidget),MinHeightWidget,false,false,0);
    end else
      MinHeightWidget:=nil;
    gtk_object_set_data(PGtkObject(MenuItemWidget),
                        'LCLMinHeight',MinHeightWidget);
  end;

  procedure CreateLabel;
  begin
    // create a label for the Caption
    LabelWidget:=PGtkAccelLabel(gtk_accel_label_new(''));
    gtk_misc_set_alignment(GTK_MISC (LabelWidget), 0.0, 0.5);
    gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLLabel', LabelWidget);
    gtk_container_add(GTK_CONTAINER(HBoxWidget),PgtkWidget(LabelWidget));
    SetMenuItemLabelText(LCLMenuItem,MenuItemWidget);
    gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(LabelWidget),MenuItemWidget);
    gtk_widget_show(PGtkWidget(LabelWidget));
  end;

begin
  HBoxWidget:=gtk_object_get_data(PGtkObject(MenuItemWidget), 'LCLHBox');
  if HBoxWidget=nil then begin
    // create inner widgets
    if LCLMenuItem.Caption='-' then begin
      // a separator is an empty gtkmenuitem
      exit;
    end;
    HBoxWidget:=gtk_hbox_new(false,0);
    gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLHBox', HBoxWidget);
    CreateIcon;
    CreateLabel;
    gtk_container_add(GTK_CONTAINER(MenuItemWidget),HBoxWidget);
    gtk_widget_show(HBoxWidget);
  end else begin
    // there are already inner widgets
    if LCLMenuItem.Caption='-' then begin
      // a separator is an empty gtkmenuitem -> delete the inner widgets
      DestroyWidget(HBoxWidget);
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLHBox', nil);
    end else begin
      // just update the content
      SetMenuItemLabelText(LCLMenuItem,MenuItemWidget);
    end;
  end;
end;

{------------------------------------------------------------------------------
  function CreateMenuItem(LCLMenuItem: TMenuItem): Pointer;

  Creates a new menuitem widget.
 ------------------------------------------------------------------------------}
function CreateMenuItem(LCLMenuItem: TMenuItem): Pointer;
var
  MenuItemWidget: PGtkWidget;
begin
  // create the menuitem widget (normal, check or radio)
  if LCLMenuItem.Caption='-' then
    // create separator
    MenuItemWidget:=gtk_menu_item_new
  else if LCLMenuItem.RadioItem and not LCLMenuItem.HasIcon then begin
    MenuItemWidget:=gtk_radio_menu_item_new(nil);
  end else if LCLMenuItem.IsCheckItem or LCLMenuItem.HasIcon then begin
    MenuItemWidget:=gtk_check_menu_item_new;
  end else
    MenuItemWidget:=gtk_menu_item_new;
  if GtkWidgetIsA(MenuItemWidget,GTK_TYPE_CHECK_MENU_ITEM) then begin
    // set 'ShowAlwaysCheckable'
    gtk_check_menu_item_set_show_toggle(PGtkCheckMenuItem(MenuItemWidget),
      LCLMenuItem.ShowAlwaysCheckable);
    // set 'Checked'
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(MenuItemWidget),
      LCLMenuItem.Checked);
    {$ifdef GTK2}
    {$else}
    if (OldCheckMenuItemToggleSize=0) then
      OldCheckMenuItemToggleSize:=MENU_ITEM_CLASS(MenuItemWidget)^.toggle_size;
    {$endif}
  end;

  // set attributes (enabled and rightjustify)
  gtk_widget_set_sensitive(MenuItemWidget, LCLMenuItem.Enabled);
  if LCLMenuItem.RightJustify then
    gtk_menu_item_right_justify(PGtkMenuItem(MenuItemWidget));

  // create the hbox containing the label and the control
  UpdateInnerMenuItem(LCLMenuItem,MenuItemWidget);

  gtk_widget_show(MenuItemWidget);
  Result:=MenuItemWidget;
end;

{------------------------------------------------------------------------------
  TgtkObject SetSizeNotification
  Params: Widget: PGtkWidget  A widget that is the handle of a lcl control.

  When the gtk sends a size signal, it is not send directly to the LCL. All gtk
  size/move messages are collected and only the last one for each widget is sent
  to the LCL.
  This is neccessary, because the gtk sends size messages several times and
  it replays resizes. Since the LCL reacts to every size notification and
  resizes child controls, this results in a perpetuum mobile.
 ------------------------------------------------------------------------------}
procedure SaveSizeNotification(Widget: PGtkWidget);
{$IFDEF VerboseSizeMsg}
var
  LCLControl: TWinControl;
{$ENDIF}
begin
  {$IFDEF VerboseSizeMsg}
  write('SaveSizeNotification Widget=',HexStr(Cardinal(Widget),8));
  LCLControl:=TWinControl(GetLCLObject(Widget));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then
      writeln(' ',LCLControl.Name,':',LCLControl.ClassName)
    else
      writeln(' ERROR: ',LCLControl.ClassName);
  end else begin
    writeln(' ERROR: LCLControl=nil');
  end;
  {$ENDIF}
  if not FWidgetsResized.Contains(Widget) then
    FWidgetsResized.Add(Widget);
end;

{------------------------------------------------------------------------------
  TgtkObject SaveClientSizeNotification
  Params: FixWidget: PGtkWidget  A widget that is the fixed widget
                                 of a lcl control.

  When the gtk sends a size signal, it is not send directly to the LCL. All gtk
  size/move messages are collected and only the last one for each widget is sent
  to the LCL.
  This is neccessary, because the gtk sends size messages several times and
  it replays resizes. Since the LCL reacts to every size notification and
  resizes child controls, this results in a perpetuum mobile.
 ------------------------------------------------------------------------------}
procedure SaveClientSizeNotification(FixWidget: PGtkWidget);
{$IFDEF VerboseSizeMsg}
var
  LCLControl: TWinControl;
  MainWidget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF VerboseSizeMsg}
  MainWidget:=GetMainWidget(FixWidget);
  write('SaveClientSizeNotification',
    ' FixWidget=',HexStr(Cardinal(FixWidget),8),
    ' MainWIdget=',HexStr(Cardinal(MainWidget),8));
  LCLControl:=TWinControl(GetLCLObject(MainWidget));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then
      writeln(' ',LCLControl.Name,':',LCLControl.ClassName)
    else
      writeln(' ERROR: ',LCLControl.ClassName);
  end else begin
    writeln(' ERROR: LCLControl=nil');
  end;
  {$ENDIF}
  if not FFixWidgetsResized.Contains(FixWidget) then
    FFixWidgetsResized.Add(FixWidget);
end;

{-------------------------------------------------------------------------------
  CreateTopologicalSortedWidgets
  Params: HashArray: TDynHashArray  of PGtkWidget
  
  Creates a topologically sorted TList of PGtkWidget.
-------------------------------------------------------------------------------}
function CreateTopologicalSortedWidgets(HashArray: TDynHashArray): TList;
type
  PTopologicalEntry = ^TTopologicalEntry;
  TTopologicalEntry = record
      Widget: PGtkWidget;
      ParentLevel: integer;
    end;
    
  function GetParentLevel(AControl: TControl): integer;
  // nil has lvl -1
  // a control without parent has lvl 0
  begin
    Result:=-1;
    while AControl<>nil do begin
      inc(Result);
      AControl:=AControl.Parent;
    end;
  end;
  
var
  TopologicalList: PTopologicalEntry;
  HashItem: PDynHashArrayItem;
  i, Lvl, MaxLevel: integer;
  LCLControl: TControl;
  LevelCounts: PInteger;
begin
  //writeln(' KKK0');
  Result:=TList.Create;
  if HashArray.Count=0 then exit;
  
  // put all widgets into an array and calculate their parent levels
  GetMem(TopologicalList,SizeOf(TTopologicalEntry)*HashArray.Count);
  HashItem:=HashArray.FirstHashItem;
  i:=0;
  MaxLevel:=0;
  //writeln(' KKK1 HashArray.Count=',HashArray.Count);
  while HashItem<>nil do begin
    TopologicalList[i].Widget:=HashItem^.Item;
    //writeln(' KKK21 i=',i,' Widget=',HexStr(Cardinal(TopologicalList[i].Widget),8));
    LCLControl:=TControl(GetLCLObject(TopologicalList[i].Widget));
    if (LCLControl=nil) or (not (LCLControl is TControl)) then
      RaiseException('CreateTopologicalSortedWidgets: '
                             +'Widget without LCL control');
    Lvl:=GetParentLevel(LCLControl);
    TopologicalList[i].ParentLevel:=Lvl;
    if MaxLevel<Lvl then
      MaxLevel:=Lvl;
    //writeln(' KKK2 i=',i,' Lvl=',Lvl,' MaxLvl=',MaxLevel,' LCLControl=',LCLControl.Name,':',LCLControl.ClassName);
    inc(i);
    HashItem:=HashItem^.Next;
  end;
  inc(MaxLevel);
  
  // bucket sort the widgets
  
  // count each number of levels (= bucketsizes)
  GetMem(LevelCounts,SizeOf(Integer)*MaxLevel);
  FillChar(LevelCounts^,SizeOf(Integer)*MaxLevel,0);
  for i:=0 to HashArray.Count-1 do
    inc(LevelCounts[TopologicalList[i].ParentLevel]);

  // calculate bucketends
  for i:=1 to MaxLevel-1 do
    inc(LevelCounts[i],LevelCounts[i-1]);

  // bucket sort the widgets in Result
  Result.Count:=HashArray.Count;
  for i:=0 to HashArray.Count-1 do
    Result[i]:=nil;
  for i:=0 to HashArray.Count-1 do begin
    Lvl:=TopologicalList[i].ParentLevel;
    dec(LevelCounts[Lvl]);
    //writeln(' KKK5 i=',i,' Lvl=',Lvl,' LevelCounts[Lvl]=',LevelCounts[Lvl],
    //  ' Widget=',HexStr(Cardinal(TopologicalList[i].Widget),8));
    Result[LevelCounts[Lvl]]:=TopologicalList[i].Widget;
  end;
  
  FreeMem(LevelCounts);
  FreeMem(TopologicalList);
end;



Procedure ReportNotObsolete(const Texts : String);
Begin
  Writeln('*********************************************');
  Writeln('*********************************************');
  Writeln('*************Non-Obsolete report*************');
  Writeln('*********************************************');
  Writeln('*************'+Texts+'*is being used yet.****');
  Writeln('*******Please remove this function from******');
  Writeln('*******the obsolete section in gtkproc.inc***');
  Writeln('*********************************************');
  Writeln('*********************************************');
  Writeln('*********************************************');
  Writeln('*********************************************');
end;

function TGDKColorToTColor(const value : TGDKColor) : TColor;
begin
  Result := ((Value.Blue shr 8) shl 16) + ((Value.Green shr 8) shl 8)
           + (Value.Red shr 8);
end;

function TColortoTGDKColor(const value : TColor) : TGDKColor;
var
   newColor : TGDKColor;
   num : LongInt;                                          // example assume value := $00FF2510

begin
   NewColor.pixel := 0;
                                                           // value - $00FF2510
   num := (value shr 8) shl 8;                             //   num - $00FF2500
   newColor.red := (value - num) * 257;                    //   red - $00000010  * 257

                                                           // value - $00FF2510
   num := (value shr 16) shl 8;                            //   num - $0000FF00
                                                           //value2 - $0000FF25
   newColor.green :=  ((value shr 8) - num) * 257;         // green = $00000025  * 257

                                                           // value - $00FF2510
   newColor.blue := (value shr 16) * 257;                  //  blue - $000000FF  * 257

   result := newColor;
end;

{------------------------------------------------------------------------------
  Function: UpdateSysColorMap
  Params:  none
  Returns: none

  Reads the system colors.
 ------------------------------------------------------------------------------}
procedure UpdateSysColorMap(Widget: PGtkWidget);
{ $DEFINE VerboseUpdateSysColorMap}
{$IFDEF VerboseUpdateSysColorMap}
  function GdkColorAsString(c: TgdkColor): string;
  begin
    Result:='LCL='+HexStr(Cardinal(TGDKColorToTColor(c)),8)
             +' Pixel='+HexStr(Cardinal(c.Pixel),8)
             +' Red='+HexStr(Cardinal(c.Red),8)
             +' Green='+HexStr(Cardinal(c.Green),8)
             +' Blue='+HexStr(Cardinal(c.Blue),8)
             ;
  end;
{$ENDIF}
var
  MainStyle: PGtkStyle;
begin
  if Widget=nil then exit;
  gtk_widget_set_rc_style(Widget);
  MainStyle:=gtk_widget_get_style(Widget);
  if MainStyle=nil then exit;
  with MainStyle^ do begin
  
    {$IFDEF VerboseUpdateSysColorMap}
    if rc_style<>nil then begin
      with rc_style^ do begin
        writeln('rc_style:');
        writeln(' FG GTK_STATE_NORMAL ',GdkColorAsString(fg[GTK_STATE_NORMAL]));
        writeln(' FG GTK_STATE_ACTIVE ',GdkColorAsString(fg[GTK_STATE_ACTIVE]));
        writeln(' FG GTK_STATE_PRELIGHT ',GdkColorAsString(fg[GTK_STATE_PRELIGHT]));
        writeln(' FG GTK_STATE_SELECTED ',GdkColorAsString(fg[GTK_STATE_SELECTED]));
        writeln(' FG GTK_STATE_INSENSITIVE ',GdkColorAsString(fg[GTK_STATE_INSENSITIVE]));
        writeln('');
        writeln(' BG GTK_STATE_NORMAL ',GdkColorAsString(bg[GTK_STATE_NORMAL]));
        writeln(' BG GTK_STATE_ACTIVE ',GdkColorAsString(bg[GTK_STATE_ACTIVE]));
        writeln(' BG GTK_STATE_PRELIGHT ',GdkColorAsString(bg[GTK_STATE_PRELIGHT]));
        writeln(' BG GTK_STATE_SELECTED ',GdkColorAsString(bg[GTK_STATE_SELECTED]));
        writeln(' BG GTK_STATE_INSENSITIVE ',GdkColorAsString(bg[GTK_STATE_INSENSITIVE]));
        writeln('');
        writeln(' TEXT GTK_STATE_NORMAL ',GdkColorAsString(text[GTK_STATE_NORMAL]));
        writeln(' TEXT GTK_STATE_ACTIVE ',GdkColorAsString(text[GTK_STATE_ACTIVE]));
        writeln(' TEXT GTK_STATE_PRELIGHT ',GdkColorAsString(text[GTK_STATE_PRELIGHT]));
        writeln(' TEXT GTK_STATE_SELECTED ',GdkColorAsString(text[GTK_STATE_SELECTED]));
        writeln(' TEXT GTK_STATE_INSENSITIVE ',GdkColorAsString(text[GTK_STATE_INSENSITIVE]));
        writeln('');
      end;
    end;

    writeln('MainStyle:');
    writeln(' FG GTK_STATE_NORMAL ',GdkColorAsString(fg[GTK_STATE_NORMAL]));
    writeln(' FG GTK_STATE_ACTIVE ',GdkColorAsString(fg[GTK_STATE_ACTIVE]));
    writeln(' FG GTK_STATE_PRELIGHT ',GdkColorAsString(fg[GTK_STATE_PRELIGHT]));
    writeln(' FG GTK_STATE_SELECTED ',GdkColorAsString(fg[GTK_STATE_SELECTED]));
    writeln(' FG GTK_STATE_INSENSITIVE ',GdkColorAsString(fg[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' BG GTK_STATE_NORMAL ',GdkColorAsString(bg[GTK_STATE_NORMAL]));
    writeln(' BG GTK_STATE_ACTIVE ',GdkColorAsString(bg[GTK_STATE_ACTIVE]));
    writeln(' BG GTK_STATE_PRELIGHT ',GdkColorAsString(bg[GTK_STATE_PRELIGHT]));
    writeln(' BG GTK_STATE_SELECTED ',GdkColorAsString(bg[GTK_STATE_SELECTED]));
    writeln(' BG GTK_STATE_INSENSITIVE ',GdkColorAsString(bg[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' TEXT GTK_STATE_NORMAL ',GdkColorAsString(text[GTK_STATE_NORMAL]));
    writeln(' TEXT GTK_STATE_ACTIVE ',GdkColorAsString(text[GTK_STATE_ACTIVE]));
    writeln(' TEXT GTK_STATE_PRELIGHT ',GdkColorAsString(text[GTK_STATE_PRELIGHT]));
    writeln(' TEXT GTK_STATE_SELECTED ',GdkColorAsString(text[GTK_STATE_SELECTED]));
    writeln(' TEXT GTK_STATE_INSENSITIVE ',GdkColorAsString(text[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' LIGHT GTK_STATE_NORMAL ',GdkColorAsString(light[GTK_STATE_NORMAL]));
    writeln(' LIGHT GTK_STATE_ACTIVE ',GdkColorAsString(light[GTK_STATE_ACTIVE]));
    writeln(' LIGHT GTK_STATE_PRELIGHT ',GdkColorAsString(light[GTK_STATE_PRELIGHT]));
    writeln(' LIGHT GTK_STATE_SELECTED ',GdkColorAsString(light[GTK_STATE_SELECTED]));
    writeln(' LIGHT GTK_STATE_INSENSITIVE ',GdkColorAsString(light[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' DARK GTK_STATE_NORMAL ',GdkColorAsString(dark[GTK_STATE_NORMAL]));
    writeln(' DARK GTK_STATE_ACTIVE ',GdkColorAsString(dark[GTK_STATE_ACTIVE]));
    writeln(' DARK GTK_STATE_PRELIGHT ',GdkColorAsString(dark[GTK_STATE_PRELIGHT]));
    writeln(' DARK GTK_STATE_SELECTED ',GdkColorAsString(dark[GTK_STATE_SELECTED]));
    writeln(' DARK GTK_STATE_INSENSITIVE ',GdkColorAsString(dark[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' MID GTK_STATE_NORMAL ',GdkColorAsString(mid[GTK_STATE_NORMAL]));
    writeln(' MID GTK_STATE_ACTIVE ',GdkColorAsString(mid[GTK_STATE_ACTIVE]));
    writeln(' MID GTK_STATE_PRELIGHT ',GdkColorAsString(mid[GTK_STATE_PRELIGHT]));
    writeln(' MID GTK_STATE_SELECTED ',GdkColorAsString(mid[GTK_STATE_SELECTED]));
    writeln(' MID GTK_STATE_INSENSITIVE ',GdkColorAsString(mid[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' BASE GTK_STATE_NORMAL ',GdkColorAsString(base[GTK_STATE_NORMAL]));
    writeln(' BASE GTK_STATE_ACTIVE ',GdkColorAsString(base[GTK_STATE_ACTIVE]));
    writeln(' BASE GTK_STATE_PRELIGHT ',GdkColorAsString(base[GTK_STATE_PRELIGHT]));
    writeln(' BASE GTK_STATE_SELECTED ',GdkColorAsString(base[GTK_STATE_SELECTED]));
    writeln(' BASE GTK_STATE_INSENSITIVE ',GdkColorAsString(base[GTK_STATE_INSENSITIVE]));
    writeln('');
    writeln(' BLACK ',GdkColorAsString(black));
    writeln(' WHITE ',GdkColorAsString(white));
    {$ENDIF}
    
    {$IFDEF NewSysColors}
    SysColorMap[COLOR_SCROLLBAR] := TGDKColorToTColor(bg[GTK_STATE_ACTIVE]);
    SysColorMap[COLOR_BACKGROUND] := TGDKColorToTColor(bg[GTK_STATE_PRELIGHT]);
    SysColorMap[COLOR_ACTIVECAPTION] := TGDKColorToTColor(text[GTK_STATE_ACTIVE]);
    SysColorMap[COLOR_INACTIVECAPTION] := TGDKColorToTColor(text[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_MENU] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_WINDOW] := TGDKColorToTColor(white);
    SysColorMap[COLOR_WINDOWFRAME] := TGDKColorToTColor(black);
    SysColorMap[COLOR_MENUTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_WINDOWTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_CAPTIONTEXT] := TGDKColorToTColor(text[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_ACTIVEBORDER] := TGDKColorToTColor(bg[GTK_STATE_ACTIVE]);
    SysColorMap[COLOR_INACTIVEBORDER] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_APPWORKSPACE] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_HIGHLIGHT] := TGDKColorToTColor(bg[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_HIGHLIGHTTEXT] := TGDKColorToTColor(text[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_BTNFACE] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_BTNSHADOW] := TGDKColorToTColor(fg[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_GRAYTEXT] := TGDKColorToTColor(text[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_BTNTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_INACTIVECAPTIONTEXT] := TGDKColorToTColor(text[GTK_STATE_INSENSITIVE]);
    SysColorMap[COLOR_BTNHIGHLIGHT] := TGDKColorToTColor(fg[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_3DDKSHADOW] := TGDKColorToTColor(black);
    SysColorMap[COLOR_3DLIGHT] := TGDKColorToTColor(fg[GTK_STATE_SELECTED]);
    SysColorMap[COLOR_INFOTEXT] := TGDKColorToTColor(text[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_INFOBK] := TGDKColorToTColor(bg[GTK_STATE_PRELIGHT]);
    SysColorMap[COLOR_HOTLIGHT] := TGDKColorToTColor(fg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_GRADIENTACTIVECAPTION] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    SysColorMap[COLOR_GRADIENTINACTIVECAPTION] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
    {$ENDIF}
  end;
(*
    $C0C0C0,     {COLOR_SCROLLBAR}
    $808000,     {COLOR_BACKGROUND}
    $800000,     {COLOR_ACTIVECAPTION}
    $808080,     {COLOR_INACTIVECAPTION}
    $C0C0C0,     {COLOR_MENU}
    $FFFFFF,     {COLOR_WINDOW}
    $000000,     {COLOR_WINDOWFRAME}
    $000000,     {COLOR_MENUTEXT}
    $000000,     {COLOR_WINDOWTEXT}
    $FFFFFF,     {COLOR_CAPTIONTEXT}
    $C0C0C0,     {COLOR_ACTIVEBORDER}
    $C0C0C0,     {COLOR_INACTIVEBORDER}
    $808080,     {COLOR_APPWORKSPACE}
    $800000,     {COLOR_HIGHLIGHT}
    $FFFFFF,     {COLOR_HIGHLIGHTTEXT}
    $D0D0D0,     {COLOR_BTNFACE}
    $808080,     {COLOR_BTNSHADOW}
    $808080,     {COLOR_GRAYTEXT}
    $000000,     {COLOR_BTNTEXT}
    $C0C0C0,     {COLOR_INACTIVECAPTIONTEXT}
    $F0F0F0,     {COLOR_BTNHIGHLIGHT}
    $000000,     {COLOR_3DDKSHADOW}
    $C0C0C0,     {COLOR_3DLIGHT}
    $000000,     {COLOR_INFOTEXT}
    $E1FFFF,     {COLOR_INFOBK}
    $000000,     {unasigned}
    $000000,     {COLOR_HOTLIGHT}
    $000000,     {COLOR_GRADIENTACTIVECAPTION}
    $000000      {COLOR_GRADIENTINACTIVECAPTION}
  *)

end;


{------------------------------------------------------------------------------
  Function: WaitForClipbrdAnswerDummyTimer
  
  this is a helper function for WaitForClipboardAnswer
 ------------------------------------------------------------------------------}
function WaitForClipbrdAnswerDummyTimer(Client: Pointer): gint; cdecl;
begin
  Result:=1; // go on, make sure getting a message at least every second
end;

{------------------------------------------------------------------------------
  Function: WaitForClipboardAnswer
  Params: none
  Returns: true, if clipboard data arrived

  waits til clipboard/selection answer arrived (max 1 second)
  ! While waiting the messagequeue will be processed !
 ------------------------------------------------------------------------------}
function WaitForClipboardAnswer(c: PClipboardEventData): boolean;
var
  StartTime, CurTime: TSystemTime;
  Timer: cardinal;

  function ValidDateSelection : boolean;
  begin
    {$ifdef GTK2}
    result := c^.Data.Selection<>NIL;
    {$else}
    result := c^.Data.Selection<>0;
    {$endif}
  end;
  
begin
  Result:=false;
  {$IFDEF DEBUG_CLIPBOARD}
  writeln('[TgtkObject.WaitForClipboardAnswer] A');
  {$ENDIF}
  if (ValidDateSelection) or (c^.Waiting) or (c^.Stopping) then begin
    //writeln('[TgtkObject.WaitForClipboardAnswer] B');
    Result:=(ValidDateSelection);
    exit;
  end;
  c^.Waiting:=true;
  DateTimeToSystemTime(Time,StartTime);
  //writeln('[TgtkObject.WaitForClipboardAnswer] C');
  Application.ProcessMessages;
  //writeln('[TgtkObject.WaitForClipboardAnswer] D');
  if (ValidDateSelection) or (c^.Stopping) then begin
    //writeln('[TgtkObject.WaitForClipboardAnswer] E  Yeah, Response received');
    Result:=(ValidDateSelection);
    exit;
  end;
  //writeln('[TgtkObject.WaitForClipboardAnswer] F');
  // start a timer to make sure not waiting forever
  Timer := gtk_timeout_add(500, @WaitForClipbrdAnswerDummyTimer, nil);
  try
    repeat
      // just wait ...
      {$IFDEF DEBUG_CLIPBOARD}
      writeln('[TgtkObject.WaitForClipboardAnswer] G');
      {$ENDIF}
      Application.HandleMessage;
      if (ValidDateSelection) or (c^.Stopping) then begin
        {$IFDEF DEBUG_CLIPBOARD}
        writeln('[TgtkObject.WaitForClipboardAnswer] E  Yeah, Response received');
        {$ENDIF}
        Result:=(ValidDateSelection);
        exit;
      end;
      DateTimeToSystemTime(Time,CurTime);
    until (CurTime.Second*1000+CurTime.MilliSecond
           -StartTime.Second*1000-StartTime.MilliSecond
           >1000);
  finally
    {$IFDEF DEBUG_CLIPBOARD}
    writeln('[TgtkObject.WaitForClipboardAnswer] H');
    {$ENDIF}
    // stop the timer
    gtk_timeout_remove(Timer);
//writeln('[TgtkObject.WaitForClipboardAnswer] END');
  end;
  { $IFDEF DEBUG_CLIPBOARD}
  writeln('[TgtkObject.WaitForClipboardAnswer] WARNING: no answer received in time');
  { $ENDIF}
end;

{------------------------------------------------------------------------------
  Function: RequestSelectionData
  Params: ClipboardWidget - widget with connected signals 'selection_get'
                            and 'selection_clear_event'
          ClipboardType
          FormatID - the selection target format wanted
  Returns: the TGtkSelectionData record

  requests the format FormatID of clipboard of type ClipboardType and
  waits til clipboard/selection answer arrived (max 1 second)
  ! While waiting the messagequeue will be processed !
 ------------------------------------------------------------------------------}
function RequestSelectionData(ClipboardWidget: PGtkWidget;
  ClipboardType: TClipboardType;  FormatID: cardinal): TGtkSelectionData;
var
  TimeID: cardinal;
  i: integer;
  c: PClipboardEventData;
begin
  {$IFDEF DEBUG_CLIPBOARD}
  writeln('[RequestSelectionData] FormatID=',FormatID);
  {$ENDIF}
  FillChar(Result,SizeOf(TGtkSelectionData),0);
  if (ClipboardWidget=nil) or (FormatID=0) 
  or (ClipboardTypeAtoms[ClipboardType]=0) then exit;
  
  TimeID:=1000;
  repeat
    repeat
      inc(TimeID);
      if TimeID>1100 then exit;
      i:=ClipboardSelectionData.Count-1;
      while (i>=0) 
      and (PClipboardEventData(ClipboardSelectionData[i])^.TimeID<>TimeID) do
        dec(i);
    until (i<0);
    New(c);
    c^.TimeID:=TimeID;
    FillChar(c^.Data,SizeOf(TGtkSelectionData),0);
    ClipboardSelectionData.Add(c);
    {$IFDEF DEBUG_CLIPBOARD}
    writeln('[RequestSelectionData] TimeID=',TimeID);
    {$ENDIF}
    if gtk_selection_convert(ClipboardWidget, ClipboardTypeAtoms[ClipboardType],
                             FormatID, TimeID)<>0
    then
      break;
    ClipboardSelectionData.Remove(c);
    Dispose(c);
  until false;
  try
    if not WaitForClipboardAnswer(c) then exit;
    Result:=c^.Data;
  finally
    ClipboardSelectionData.Remove(c);
    Dispose(c);
  end;
end;

{------------------------------------------------------------------------------
  Function: FreeClipboardTargetEntries
  Params: ClipboardType
  Returns: -

  frees the memory of a ClipboardTargetEntries list
 ------------------------------------------------------------------------------}
procedure FreeClipboardTargetEntries(ClipboardType: TClipboardType);
var i: integer;
begin
  if ClipboardTargetEntries[ClipboardType]<>nil then begin
    for i:=0 to ClipboardTargetEntryCnt[ClipboardType]-1 do
      StrDispose(ClipboardTargetEntries[ClipboardType][i].Target);
    FreeMem(ClipboardTargetEntries[ClipboardType]);
  end;
end;

Function CreateFormContents(var FormWidget : Pointer) : Pointer;
var
  ScrolledWidget,
  LayoutWidget: Pointer;
begin
  // Create the VBox. We need that to place controls outside
  // the client area (like menu)
  Result := gtk_vbox_new(False, 0);
  If FormWidget = nil then
    FormWidget := Result;

  // Create the form client area
  ScrolledWidget := gtk_scrolled_window_new(nil,nil);
  gtk_box_pack_end(Result, ScrolledWidget, True, True, 0);
  gtk_widget_show(ScrolledWidget);

  gtk_object_set_data(FormWidget,'scroll_area', ScrolledWidget);

  LayoutWidget := gtk_layout_new(nil, nil);
  gtk_container_add(ScrolledWidget, LayoutWidget);
  gtk_widget_show(LayoutWidget);
  SetFixedWidget(FormWidget, LayoutWidget);
  SetMainWidget(FormWidget, LayoutWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(ScrolledWidget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(ScrolledWidget)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(ScrolledWidget),
                                 GTK_POLICY_NEVER,
                                 GTK_POLICY_NEVER);
end;

{------------------------------------------------------------------------------
  Function: IndexOfStyle
  Params: WName
  Returns: Index of Style

  Returns the Index within the Styles property of WNAME
 ------------------------------------------------------------------------------}
function IndexOfStyle(const WName : String): integer;
begin
  if Styles<>nil then begin
    for Result:=0 to Styles.Count-1 do
      if AnsiCompareText(WName,Styles[Result])=0 then exit;
  end;
  Result:=-1;
end;

{------------------------------------------------------------------------------
  Function: ReleaseStyle
  Params: WName
  Returns: nothing

  Tries to release a Style corresponding to the Widget Name passed, aka 'button',
  'default', checkbox', etc. This should only be called on theme change or on
  application terminate.
 ------------------------------------------------------------------------------}
Type
  PStyleObject = ^TStyleObject;
  TStyleObject = Record
    Style : PGTKStyle;
    Widget : PGTKWidget;
  end;

Function NewStyleObject : PStyleObject;
begin
  New(Result);
  Result^.Widget := nil;
  Result^.Style := nil;
end;

Procedure FreeStyleObject(var StyleObject : PStyleObject);
begin
  If StyleObject <> nil then begin
    If StyleObject^.Widget <> nil then
      GTK_Widget_Destroy(StyleObject^.Widget);
    If StyleObject^.Style <> nil then
      If StyleObject^.Style^.Ref_Count > 0 then
        GTK_Style_Unref(StyleObject^.Style);
    Dispose(StyleObject);
    StyleObject := nil;
  end;
end;

Procedure ReleaseStyle(const WName : String);
var
  l : Longint;
  s : PStyleObject;
begin
  If Not Assigned(Styles) then
    exit;
  l := IndexOfStyle(WName);
  If l >= 0 then begin
    If Styles.Objects[l] <> nil then
      Try
        s := PStyleObject(Styles.Objects[l]);
        FreeStyleObject(S);
      Except
        Writeln('[ReleaseStyle] : Unable To Unreference Style');
      end;
    Styles.Delete(l);
  end;
end;

{------------------------------------------------------------------------------
  Function: GetStyle
  Params: none
  Returns: Returns a Corresponding Style

  Tries to get the Style corresponding to the Widget Name passed, aka 'button',
  'default', checkbox', etc. for use within such routines as DrawFrameControl
  to attempt to supply theme dependent drawing. Styles are stored in a TStrings
  list which is only updated on theme change, to ensure fast efficient retrieval
  of Styles.
 ------------------------------------------------------------------------------}
function GetStyle(const WName : String) : PGTKStyle;
var
  Tp : Pointer;
  l : Longint;
  StyleObject : PStyleObject;
begin
  Result := nil;
  If Not Assigned(Styles) then
    exit;
  l:=IndexOfStyle(WName);
  If l < 0 then begin
    StyleObject := NewStyleObject;
    Tp:=nil;
    If AnsiCompareText(WName,'button')=0 then
      StyleObject^.Widget := GTK_BUTTON_NEW
    else
      If AnsiCompareText(WName,'default')=0 then
        StyleObject^.Widget := GTK_WIDGET_NEW(GTK_WIDGET_TYPE,nil,[])
    else
      If AnsiCompareText(WName,'window')=0 then
        StyleObject^.Widget := GTK_WINDOW_NEW(0)
    else
      If AnsiCompareText(WName,'checkbox')=0 then begin
        StyleObject^.Widget := GTK_CHECK_BUTTON_NEW;
      end
    else
      If AnsiCompareText(WName,'radiobutton')=0 then
        StyleObject^.Widget := GTK_RADIO_BUTTON_NEW(nil)
    else
      If AnsiCompareText(WName,'menu')=0 then
        StyleObject^.Widget := GTK_MENU_NEW
    else
      If AnsiCompareText(WName,'menuitem')=0 then
        StyleObject^.Widget := GTK_MENU_ITEM_NEW
    else
      If AnsiCompareText(WName,'scrollbar')=0 then
        StyleObject^.Widget := gtk_hscrollbar_new(nil)//can't dif. between Horiz/Vert. Styles
    else
      If AnsiCompareText(WName,'tooltip')=0 then begin
        TP := gtk_tooltips_new;
        StyleObject^.Widget := nil;
        GTK_Tooltips_Force_Window(TP);
        gtk_widget_ensure_style(PGTKTooltips(TP)^.Tip_Window);
        StyleObject^.Style:=GTK_RC_GET_STYLE(PGTKTooltips(TP)^.Tip_Window);
      end
    else
      If AnsiCompareText(WName,'gtk_default')=0 then begin
        StyleObject^.Widget := nil;
        StyleObject^.Style := gtk_style_new;
      end
    else begin
      FreeStyleObject(StyleObject);
      exit;
    end;
    If (StyleObject^.Widget <> nil) then begin
      gtk_widget_ensure_style(StyleObject^.Widget);
      StyleObject^.Style:=GTK_RC_GET_STYLE(StyleObject^.Widget);
    end;
    If StyleObject^.Style <> nil then
      If AnsiCompareText(WName,'gtk_default')<>0 then
        StyleObject^.Style:=GTK_Style_Ref(StyleObject^.Style);
    if StyleObject^.Style <> nil then begin
      Styles.AddObject(WName, TObject(StyleObject));
      Result:=StyleObject^.Style;
      If StyleObject^.Widget <> nil then
        UpdateSysColorMap(StyleObject^.Widget);
    end
    else
      If AnsiCompareText(WName,'default')<>0 then
        Result := GetStyle('default');
    If Tp<>nil then
      GTK_Object_Destroy(Tp);
  end else
    Result := PStyleObject(Styles.Objects[l])^.Style;
end;

Function GetStyleWidget(const WName : String) : PGTKWidget;
var
  l : Longint;
begin
  Result := nil;
  l:=IndexOfStyle(WName);
  If (l > -1) or (GetStyle(WName) <> nil) then begin
    l:=IndexOfStyle(WName);
    Result := PStyleObject(Styles.Objects[l])^.Widget;
  end;
end;

{------------------------------------------------------------------------------
  Function: LoadDefaultFont
  Params: none
  Returns: Returns the default Font

  For Text/Font Routines: if the Font is invalid, this can be used instead, or
  if the DT_internal flag is used(aka use system font) this is used. This is
  also the font returned by GetStockObject(SYSTEM_FONT).

  It attempts to get the font from the default Style, or if none is available,
  a new style(aka try and get GTK builtin values), if that fails tries to get
  a generic fixed font, if THAT fails, it gets whatever font is available.
  If the result is not nil it MUST be GDK_FONT_UNREF'd when done.
 ------------------------------------------------------------------------------}
function LoadDefaultFont: PGDKFont;
var
  Style : PGTKStyle;
begin
  Result := nil;
  Style := GetStyle('default');
  if Style = nil then
    Style := GetStyle('gtk_default');

  If Style <> nil then begin
    If Style^.Font <> nil then
       Result := Style^.Font
    else
      If (Style^.RC_Style <> nil) and (Style^.RC_Style^.font_name <> nil)
      then
        Result := gdk_font_load(Style^.RC_Style^.font_name);
  end;

  If Result = nil then
    Result := gdk_fontset_load('-*-fixed-*-*-*-*-*-120-*-*-*-*-*-*');
  if Result = nil then
    Result := gdk_fontset_load('-*-*-*-*-*-*-*-*-*-*-*-*-*-*');

  If Result <> nil then
    Result := gdk_font_ref(Result);
end;

Function GetSysGCValues(Color : TColorRef) : TGDKGCValues;
var
  Style : PGTKStyle;
  GC : PGDKGC;
  Pixmap : PGDKPixmap;
  SysColor : TColorRef;
begin
  Color := Color and $FF;

  {Set defaults in case something goes wrong}
  FillChar(Result, SizeOf(Result), 0);
  SysColor := GetSysColor(Color);
  Result.foreground.Red := RGB(0,GetRValue(SysColor),0);
  Result.foreground.Green := RGB(0,GetGValue(SysColor),0);
  Result.foreground.Blue := RGB(0,GetBValue(SysColor),0);
  Result.Fill := GDK_Solid;
  {$IfDef Disable_GC_SysColors}
    exit;
  {$EndIf}
  Case Color of
    {These are WM/X defined, but might be possible to get}

    {COLOR_BACKGROUND
    COLOR_CAPTIONTEXT
    COLOR_INACTIVECAPTIONTEXT}

    {These Are incompatible or WM defined}
    
    {COLOR_ACTIVECAPTION
    COLOR_INACTIVECAPTION
    COLOR_GRADIENTACTIVECAPTION
    COLOR_GRADIENTINACTIVECAPTION
    COLOR_WINDOWFRAME
    COLOR_ACTIVEBORDER
    COLOR_INACTIVEBORDER}

    COLOR_INFOBK :
      begin
        Style := GetStyle('tooltip');

        If Style = nil then
          exit;

        Pixmap := Style^.bg_pixmap[GTK_STATE_NORMAL];
        If Pixmap <> nil then begin
          Result.Fill := GDK_Tiled;
          Result.Tile := Pixmap;
        end else begin
          GC := Style^.bg_gc[GTK_STATE_NORMAL];
          If GC = nil then begin
            Result.Fill := GDK_Solid;
            Result.foreground := Style^.bg[GTK_STATE_PRELIGHT];
          end
          else
            GDK_GC_Get_Values(GC, @Result);
        end;
      end;

    COLOR_INFOTEXT :
      begin
        Style := GetStyle('tooltip');

        If Style = nil then
          exit;

        GC := Style^.fg_gc[GTK_STATE_NORMAL];
        If GC = nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.fg[GTK_STATE_NORMAL];
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_Menu,
    COLOR_SCROLLBAR,
    COLOR_BTNFACE :
      begin
        Case Color of
          COLOR_BTNFACE : Style := GetStyle('window');
          COLOR_MENU : Style := GetStyle('menu');
          COLOR_SCROLLBAR : Style := GetStyle('scrollbar');
        end;
          
        If Style = nil then
          exit;
          
        Pixmap := Style^.bg_pixmap[GTK_STATE_NORMAL];
        If Pixmap <> nil then begin
          Result.Fill := GDK_Tiled;
          Result.Tile := Pixmap;
        end else begin
          GC := Style^.bg_gc[GTK_STATE_NORMAL];
          If GC = nil then begin
            Result.Fill := GDK_Solid;
            Result.foreground := Style^.fg[GTK_STATE_NORMAL];
          end
          else
            GDK_GC_Get_Values(GC, @Result);
        end;
      end;

    COLOR_3DDKSHADOW,
    COLOR_BTNSHADOW :
      begin
        Style := GetStyle('button');
        If Style = nil then
          exit;
        GC := Style^.dark_gc[GTK_STATE_NORMAL];
        If GC = nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.dark[GTK_STATE_NORMAL];
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_GRAYTEXT :
      begin
        Style := GetStyle('default');
        If Style = nil then
          exit;
        GC := Style^.text_gc[GTK_STATE_INSENSITIVE];
        GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_MENUTEXT,
    COLOR_WINDOWTEXT,
    COLOR_BTNTEXT :
      begin
        Case Color of
          COLOR_BTNTEXT : Style := GetStyle('button');
          COLOR_MENUTEXT : Style := GetStyle('menuitem');
          COLOR_WINDOWTEXT : Style := GetStyle('default');
        end;
        If Style = nil then
          exit;
        GC := Style^.text_gc[GTK_STATE_NORMAL];
        If GC = nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.text[GTK_STATE_NORMAL];
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_3DLIGHT,
    COLOR_BTNHIGHLIGHT :
      begin
        Style := GetStyle('button');
        If Style = nil then
          exit;
        GC := Style^.light_gc[GTK_STATE_NORMAL];
        If GC = nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.light[GTK_STATE_NORMAL];
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_WINDOW :
      begin
        Style := GetStyle('default');
        If Style = nil then
          exit;
        GC := Style^.base_gc[GTK_STATE_NORMAL];
        If GC = nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.base[GTK_STATE_NORMAL];
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_HIGHLIGHT :
      begin
        Style := GetStyle('default');
        If Style = nil then
          exit;
        GC := Style^.bg_gc[GTK_STATE_SELECTED];
        If GC = nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.bg[GTK_STATE_SELECTED];
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_HIGHLIGHTTEXT :
      begin
        Style := GetStyle('default');
        If Style = nil then
          exit;
        GC := Style^.bg_gc[GTK_STATE_PRELIGHT];
        If GC = nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.bg[GTK_STATE_PRELIGHT];
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    {?????????????
    COLOR_HOTLIGHT :
      begin
      end;
    ?????????????}

    {?????????????????
    COLOR_APPWORKSPACE :
      begin
      end;
    ?????????????????}
  end;
end;

Function DeleteAmpersands(var Str : String) : Longint;
// convert double ampersands to single & and delete single &
// return the position of the letter after the first deleted single ampersand
// in the new string
var
  Tmp : String;
  SrcPos, DestPos, SrcLen: integer;
begin
  Result := -1;
  
  // for speedup reasons check if Str must be changed
  SrcLen:=length(Str);
  SrcPos:=SrcLen;
  while (SrcPos>=1) and (Str[SrcPos]<>'&') do dec(SrcPos);
  if SrcPos<1 then exit;

  // copy Str to Tmp and convert ampersands on the fly
  SetLength(Tmp,SrcLen);
  SrcPos:=1;
  DestPos:=1;
  while (SrcPos<=SrcLen) do begin
    if Str[SrcPos]<>'&' then begin
      // copy normal char
      Tmp[DestPos]:=Str[SrcPos];
      inc(SrcPos);
      inc(DestPos);
    end else begin
      inc(SrcPos);
      if (SrcPos<=SrcLen) and (Str[SrcPos]='&') then begin
        // double ampersand
        Tmp[DestPos]:='&';
        inc(DestPos);
        inc(SrcPos);
      end else begin
        // single ampersand
        if Result<1 then Result:=DestPos;
      end;
    end;
  end;
  SetLength(Tmp,DestPos-1);
  Str:=Tmp;
end;

{-------------------------------------------------------------------------------
  Function Ampersands2Underscore(Src: PChar) : PChar;

  Creates a new PChar. Deletes escaping ampersands, replaces the first single
  ampersand with an underscore and deleting all other single ampersands.
-------------------------------------------------------------------------------}
function Ampersands2Underscore(Src: PChar) : PChar;
var
  i, j: Longint;
  ShortenChars, FirstAmpersand, NewLength, SrcLength: integer;
begin
  // count ampersands and find first ampersand
  ShortenChars:= 0;  // chars to delete
  FirstAmpersand:= -1;
  SrcLength:= StrLen(Src);

  { Look for amperands. If found, check if it is an escaped ampersand.
    If it is, don't count it in. }
  i:=0;
  while i<SrcLength do begin
    if Src[i] = '&' then begin
      if (i < SrcLength - 1) and (Src[i+1] = '&') then begin
        // escaping ampersand found
        inc(ShortenChars);
        inc(i,2);
        Continue;
      end else begin
        // single ampersand found
        if (FirstAmpersand < 0) then
          // the first will be replaced ...
          FirstAmpersand:= i
        else
          // ... and all others will be deleted
          inc(ShortenChars);
      end;	
    end;
    inc(i);
  end;
  // create new PChar
  NewLength:= SrcLength - ShortenChars;

  Result:=StrAlloc(NewLength+1); // +1 for #0 char at end

  // copy string without ampersands
  i:=0;
  j:=0;
  while (j < NewLength) do begin
    if Src[i] <> '&' then begin
      // copy normal char
      Result[j]:= Src[i];
    end else begin
      // ampersand
      if (i < (SrcLength - 1)) and (Src[i+1] = '&') then begin
        // escaping ampersand found
        inc(i);
        Result[j]:='&';
      end else begin
        // single ampersand found
        if i = FirstAmpersand then begin
          // replace first single ampersand with underscore
          Result[j]:='_';
        end else begin
          // delete single ampersand
          dec(j);
        end;
      end;
    end;
    Inc(i);
    Inc(j);
  end;
  Result[NewLength]:=#0;
end;

{-------------------------------------------------------------------------------
  Function RemoveAmpersands(Src: PChar; LineLength : Longint) : PChar;

  Creates a new PChar removing all escaping ampersands.
-------------------------------------------------------------------------------}
function RemoveAmpersands(Src: PChar; LineLength : Longint) : PChar;
var
  i, j: Longint;
  ShortenChars, NewLength, SrcLength: integer;
begin
  // count ampersands and find first ampersand
  ShortenChars:= 0;  // chars to delete
  SrcLength:= LineLength;

  { Look for amperands. If found, check if it is an escaped ampersand.
    If it is, don't count it in. }
  i:=0;
  while i<SrcLength do begin
    if Src[i] = '&' then begin
      if (i < SrcLength - 1) and (Src[i+1] = '&') then begin
        // escaping ampersand found
        inc(ShortenChars);
        inc(i,2);
        Continue;
      end
      else
        inc(ShortenChars);
    end;
    inc(i);
  end;
  // create new PChar
  NewLength:= SrcLength - ShortenChars;

  Result:=StrAlloc(NewLength+1); // +1 for #0 char at end

  // copy string without ampersands
  i:=0;
  j:=0;
  while (j < NewLength) do begin
    if Src[i] <> '&' then begin
      // copy normal char
      Result[j]:= Src[i];
    end else begin
      // ampersand
      if (i < (SrcLength - 1)) and (Src[i+1] = '&') then begin
        // escaping ampersand found
        inc(i);
        Result[j]:='&';
      end else
        // delete single ampersand
        dec(j);
    end;
    Inc(i);
    Inc(j);
  end;
  Result[NewLength]:=#0;
end;

{-------------------------------------------------------------------------------
  Function GetTextExtentIgnoringAmpersands(Font : PGDKFont; Str : PChar; 
                    LineLength : Longint; lbearing, rbearing, width, ascent, descent : Pgint);

  Gets text extent of a string, ignoring escaped Ampersands.
-------------------------------------------------------------------------------}
Procedure GetTextExtentIgnoringAmpersands(Font : PGDKFont; Str : PChar;
  LineLength : Longint; lbearing, rbearing, width, ascent, descent : Pgint);
var
  NewStr : PChar;
  i: integer;
begin
  NewStr:=Str;
  // first check if Str contains an ampersand:
  if (Str<>nil) then begin
    i:=0;
    while (not (Str[i] in [#0,'&'])) do inc(i);
    if Str[i]='&' then begin
      NewStr := RemoveAmpersands(Str, LineLength);
      LineLength:=StrLen(NewStr);
    end;
  end;
  gdk_text_extents(Font, NewStr, LineLength,
                   lbearing, rBearing, width, ascent, descent);
  if NewStr<>Str then
    StrDispose(NewStr);
end;

{------------------------------------------------------------------------------
  function FontIsDoubleByteCharsFont(TheFont: PGdkFont): boolean;
  
  This is only a heuristic
 ------------------------------------------------------------------------------}
function FontIsDoubleByteCharsFont(TheFont: PGdkFont): boolean;
var
  SingleCharLen, DoubleCharLen: integer;
begin
  SingleCharLen:=gdk_text_width(TheFont, 'A', 1);
  DoubleCharLen:=gdk_text_width(TheFont, 'AA', 2);
  Result:=(SingleCharLen=0) and (DoubleCharLen>0);
end;

{------------------------------------------------------------------------------
  Method:   GDKPixel2GDIRGB
  Params:
            Pixel - a GDK Pixel, refers to Index in Colormap/Visual
            Visual - a GDK Visual, if nil, the System Default is used
            Colormap - a GDK Colormap, if nil, the System Default is used
  Returns:  TGDIRGB

  A convenience function for use with GDK Image's. It takes a pixel value
  retrieved from gdk_image_get_pixel, and uses the passed Visual and Colormap
  to try and look up actual RGB values.
 ------------------------------------------------------------------------------}
Function GDKPixel2GDIRGB(Pixel : Longint; Visual : PGDKVisual;
  Colormap : PGDKColormap) : TGDIRGB;
var
  Color : TGDKColor;
  GdkColorContext : PGdkColorContext;
begin
  FillChar(Result, SizeOf(TGDIRGB),0);

  If (Visual = nil) or (Colormap = nil) then begin
    Visual := GDK_Visual_Get_System;
    Colormap := GDK_Colormap_Get_System;
  end;
    
  gdk_error_trap_push;

  Color.Pixel := Pixel;
  GdkColorContext := gdk_color_context_new(Visual,Colormap);
  gdk_color_context_query_color(GdkColorContext,@Color);
  gdk_color_context_free(GdkColorContext);

  Result.Red := Color.Red shr 8;
  Result.Green := Color.Green shr 8;
  Result.Blue := Color.Blue shr 8;

  gdk_error_trap_pop;
end;

{------------------------------------------------------------------------------
  Function GetWindowDecorations(AForm : TCustomForm) : Longint;
  
 ------------------------------------------------------------------------------}
Function GetWindowDecorations(AForm : TCustomForm) : Longint;
var
  ABorderStyle: TFormBorderStyle;
begin
  if not (csDesigning in AForm.ComponentState) then
    ABorderStyle:=AForm.BorderStyle
  else
    ABorderStyle:=bsSizeable;
    
  Case ABorderStyle of
    bsNone : Result := 0;

    bsSingle : Result := GDK_DECOR_BORDER or GDK_DECOR_TITLE or
                 GDK_DECOR_MENU or GDK_DECOR_MINIMIZE or
                 GDK_DECOR_MAXIMIZE;

    bsSizeable : Result := GDK_DECOR_ALL;

    bsDialog : Result := GDK_DECOR_BORDER or GDK_DECOR_TITLE or
                 GDK_DECOR_MENU or GDK_DECOR_MINIMIZE;

    bsToolWindow : Result := GDK_DECOR_BORDER or GDK_DECOR_TITLE or
                 GDK_DECOR_MENU or GDK_DECOR_MINIMIZE;

    bsSizeToolWin :Result := GDK_DECOR_BORDER or GDK_DECOR_TITLE or
                   GDK_DECOR_MENU or GDK_DECOR_MINIMIZE;
  end;
end;

{------------------------------------------------------------------------------
  Function GetWindowFunction(AForm : TCustomForm) : Longint;

 ------------------------------------------------------------------------------}
Function GetWindowFunction(AForm : TCustomForm) : Longint;
var
  ABorderStyle: TFormBorderStyle;
begin
  if not (csDesigning in AForm.ComponentState) then
    ABorderStyle:=AForm.BorderStyle
  else
    ABorderStyle:=bsSizeable;

  Case ABorderStyle of
    bsNone : Result := GDK_FUNC_MOVE or GDK_FUNC_CLOSE;

    bsSingle : Result := GDK_FUNC_RESIZE or GDK_FUNC_MOVE or
                GDK_FUNC_MINIMIZE or GDK_FUNC_CLOSE;

    bsSizeable : Result := GDK_FUNC_ALL;

    bsDialog : Result := GDK_FUNC_CLOSE or GDK_FUNC_MINIMIZE or
                GDK_FUNC_MOVE;

    bsToolWindow : Result := GDK_FUNC_RESIZE or GDK_FUNC_MOVE or
                GDK_FUNC_MINIMIZE or GDK_FUNC_CLOSE;

    bsSizeToolWin : Result := GDK_FUNC_RESIZE or GDK_FUNC_MOVE or
                GDK_FUNC_MINIMIZE or GDK_FUNC_CLOSE or
                GDK_FUNC_RESIZE;
  end;
end;

function GetGDKMouseCursor(Cursor: TCursor): PGdkCursor;
begin
  if (Cursor<crLow) or (Cursor>crHigh) then
    Cursor:=crDefault;
  if GDKMouseCursors[Cursor]=nil then
    GDKMouseCursors[Cursor]:=gdk_cursor_new(CursorToGDKCursor[Cursor]);
  Result:=GDKMouseCursors[Cursor];
end;

Procedure FreeGDKCursors;
var
  i: integer;
begin
  for i:=Low(GDKMouseCursors) to High(GDKMouseCursors) do begin
    if GDKMouseCursors[i]<>nil then begin
      gdk_Cursor_Destroy(GDKMouseCursors[i]);
      GDKMouseCursors[i]:=nil;
    end;
  end;
end;

Procedure FillScreenFonts(ScreenFonts : TStrings);
var
  theFonts : PPChar;
  Tmp: AnsiString;
  I, N: Integer;
begin
  ScreenFonts.Clear;
  {$IfNdef Win32}
  If X11Display = nil then
    X11Display := XOpenDisplay(GDK_GET_DISPLAY);
  theFonts := XListFonts(X11Display,PChar('-*-*-*-*-*-*-*-*-*-*-*-*-*-*'), 10000, @N);
  For I := 0 to N - 1 do
    If theFonts[I] <> nil then begin
      Tmp := ExtractFamilyFromXLFDName(AnsiString(theFonts[I]));
      If Tmp <> '' then
        If ScreenFonts.IndexOf(Tmp) < 0 then
          ScreenFonts.Append(Tmp);
    end;
   XFreeFontNames(theFonts);
  {$EndIf Win32}
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

// included by gtkproc.pp

{ =============================================================================

  $Log$
  Revision 1.182  2003/06/13 21:08:53  mattias
  moved TColorButton to dialogs.pp

  Revision 1.181  2003/06/13 14:26:17  ajgenius
  some fixes toward gtk2

  Revision 1.180  2003/06/13 10:09:04  mattias
  fixed Set/GetPixel

  Revision 1.179  2003/06/10 00:46:16  mattias
  fixed aligning controls

  Revision 1.178  2003/06/03 10:29:22  mattias
  implemented updates between source marks and breakpoints

  Revision 1.177  2003/06/03 08:02:33  mattias
  implemented showing source lines in breakpoints dialog

  Revision 1.176  2003/05/27 17:58:31  mattias
  fixed range checks

  Revision 1.175  2003/05/26 21:42:35  mattias
  fixed typos

  Revision 1.174  2003/05/26 21:28:22  mattias
  fixed absolute file

  Revision 1.173  2003/05/26 20:05:21  mattias
  made compiling gtk2 interface easier

  Revision 1.172  2003/05/19 08:16:33  mattias
  fixed allocation of dc backcolor

  Revision 1.171  2003/05/01 11:44:03  mattias
  fixed changing menuitem separator and normal

  Revision 1.170  2003/04/26 10:45:34  mattias
  fixed right control release

  Revision 1.169  2003/04/20 20:32:40  mattias
  implemented removing, re-adding, updating project dependencies

  Revision 1.168  2003/04/04 14:59:40  ajgenius
  started fixin for gtk2

  Revision 1.167  2003/04/03 17:42:13  mattias
  added exception handling for createpixmapindirect

  Revision 1.166  2003/04/02 13:23:24  mattias
  fixed default font

  Revision 1.165  2003/03/28 19:39:54  mattias
  started typeinfo for double extended

  Revision 1.164  2003/03/25 13:00:39  mattias
  implemented TMemo.SelLength, improved OI hints

  Revision 1.163  2003/03/17 13:00:35  mattias
  improved but not fixed transient windows

  Revision 1.162  2003/03/15 09:42:50  mattias
  fixed transient windows

  Revision 1.161  2003/03/09 21:13:32  mattias
  localized gtk interface

  Revision 1.160  2003/03/02 23:08:31  mattias
  fixed TComboBox.OnChange

  Revision 1.159  2003/02/18 22:56:23  mattias
  fixed key grabbing

  Revision 1.158  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.157  2003/01/24 11:58:01  mattias
  fixed clipboard waiting and kwrite targets

  Revision 1.156  2003/01/01 11:11:50  mattias
  fixed testall example

  Revision 1.155  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.154  2002/12/22 23:13:31  mattias
  fixed mem leak of tooltips in GetStyle

  Revision 1.153  2002/12/22 22:42:55  mattias
  custom controls now support child wincontrols

  Revision 1.152  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.151  2002/12/05 22:16:32  mattias
  double byte char font started

  Revision 1.150  2002/11/23 13:48:46  mattias
  added Timer patch from Vincent Snijders

  Revision 1.149  2002/11/09 18:13:35  lazarus
  MG: fixed gdkwindow checks

  Revision 1.148  2002/11/05 20:03:42  lazarus
  MG: implemented hints

  Revision 1.147  2002/11/02 22:25:38  lazarus
  MG: implemented TMethodList and Application Idle handlers

  Revision 1.146  2002/10/30 12:37:26  lazarus
  MG: mouse cursors are now allocated on demand

  Revision 1.145  2002/10/28 21:04:26  lazarus
  AJ: fixed mem leek in FillScreenFonts

  Revision 1.144  2002/10/28 18:17:04  lazarus
  MG: impoved focussing, unfocussing on destroy and fixed unit search

  Revision 1.143  2002/10/27 22:37:12  lazarus
  MG: added verbosity to delivermessage

  Revision 1.142  2002/10/27 11:51:35  lazarus
  MG: fixed memleaks

  Revision 1.141  2002/10/25 15:27:03  lazarus
  AJ: Moved form contents creation to gtkproc for code
      reuse between GNOME and GTK, and to make GNOME MDI
      programming easier later on.

  Revision 1.140  2002/10/22 12:12:09  lazarus
  MG: accelerators are now shared between non modal forms

  Revision 1.139  2002/10/21 22:12:48  lazarus
  MG: fixed frmactivate

  Revision 1.138  2002/10/21 18:21:38  lazarus
  AJ:minor styles improvement; fixed drawing checks under all(?) themes

  Revision 1.137  2002/10/21 14:40:52  lazarus
  MG: fixes for 1.1

  Revision 1.136  2002/10/21 13:51:58  lazarus
  AJ: GetDefaultFont - try to get GTK builtin value if style fails

  Revision 1.135  2002/10/21 13:15:24  lazarus
  AJ:Try and fall back on default style if nil(aka default theme)

  Revision 1.134  2002/10/21 03:23:36  lazarus
  AJ: rearranged GTK init stuff for proper GNOME init & less duplication between interfaces

  Revision 1.133  2002/10/20 21:54:04  lazarus
  MG: fixes for 1.1

  Revision 1.132  2002/10/20 21:49:11  lazarus
  MG: fixes for fpc1.1

  Revision 1.131  2002/10/20 19:03:57  lazarus
  AJ: minor fixes for FPC 1.1

  Revision 1.130  2002/10/18 16:08:10  lazarus
  AJ: Partial HintWindow Fix; Added Screen.Font & Font.Name PropEditor; Started to fix ComboBox DropDown size/pos

  Revision 1.129  2002/10/17 21:00:18  lazarus
  MG: fixed uncapturing of mouse

  Revision 1.128  2002/10/17 15:09:33  lazarus
  MG: made mouse capturing more strict

  Revision 1.127  2002/10/15 22:28:06  lazarus
  AJ: added forcelinebreaks

  Revision 1.126  2002/10/15 16:01:37  lazarus
  MG: fixed timers

  Revision 1.125  2002/10/15 07:01:30  lazarus
  MG: fixed timer checking

  Revision 1.124  2002/10/10 19:59:41  lazarus
  MG: get always a default font

  Revision 1.123  2002/10/10 19:43:17  lazarus
  MG: accelerated GetTextMetrics

  Revision 1.122  2002/10/10 08:57:25  lazarus
  MG: applied cyrillic patch from vasily

  Revision 1.121  2002/10/10 08:51:15  lazarus
  MG: added paint messages for some gtk internal widgets

  Revision 1.120  2002/10/09 10:22:55  lazarus
  MG: fixed client origin coordinates

  Revision 1.119  2002/10/08 23:44:00  lazarus
  AJ: started GNOME interface & modified gtk interface so everything is public/protected

  Revision 1.118  2002/10/08 14:10:02  lazarus
  MG: added TDeviceContext.SelectedColors

  Revision 1.117  2002/10/08 13:42:25  lazarus
  MG: added TDevContextColorType

  Revision 1.116  2002/10/08 10:08:47  lazarus
  MG: accelerated GDIColor allocating

  Revision 1.115  2002/10/07 20:50:59  lazarus
  MG: accelerated SelectGDKPenProps

  Revision 1.114  2002/10/06 17:55:46  lazarus
  MG: JITForms now sets csDesigning before creation

  Revision 1.113  2002/10/05 10:37:22  lazarus
  MG: fixed TComboBox.ItemIndex on CreateWnd

  Revision 1.112  2002/10/04 20:46:53  lazarus
  MG: improved TComboBox.SetItemIndex

  Revision 1.111  2002/10/04 16:38:15  lazarus
  MG: no OnChange event when app sets Text of TComboBox

  Revision 1.110  2002/10/03 14:47:32  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.109  2002/10/03 06:55:45  lazarus
  MG: fixed Ampersands2Underscore

  Revision 1.108  2002/10/01 10:05:50  lazarus
  MG: changed PDeviceContext into class TDeviceContext

  Revision 1.107  2002/09/30 22:39:22  lazarus
  MG: fixed setcursor

  Revision 1.106  2002/09/30 20:19:13  lazarus
  MG: fixed flickering of modal forms

  Revision 1.105  2002/09/29 15:08:43  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.104  2002/09/27 20:52:24  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.103  2002/09/26 21:29:30  lazarus
  MWE: Fixed window color

  Revision 1.102  2002/09/20 13:11:13  lazarus
  MG: fixed TPanel and Frame3D

  Revision 1.101  2002/09/19 16:45:54  lazarus
  MG: fixed Menu.Free and gdkwindow=nil bug

  Revision 1.100  2002/09/18 17:07:29  lazarus
  MG: added patch from Andrew

  Revision 1.99  2002/09/16 15:56:02  lazarus
  Resize cursors in designer.

  Revision 1.98  2002/09/12 16:49:05  lazarus
  MG: fixed SelectClipRegion

  Revision 1.97  2002/09/12 15:53:10  lazarus
  MG: small bugfixes

  Revision 1.96  2002/09/12 15:35:57  lazarus
  MG: small bugfixes

  Revision 1.95  2002/09/10 06:49:21  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.94  2002/09/08 10:02:00  lazarus
  MG: fixed streaming visible=false

  Revision 1.93  2002/09/06 22:32:21  lazarus
  Enabled cursor property + property editor.

  Revision 1.92  2002/09/06 19:45:11  lazarus
  Cleanups plus a fix to TPanel parent/drawing problem.

  Revision 1.91  2002/09/06 16:46:17  lazarus
  MG: improved GetDCOffset

  Revision 1.90  2002/09/06 16:38:25  lazarus
  MG: added GetDCOffset

  Revision 1.89  2002/09/06 15:57:36  lazarus
  MG: fixed notebook client area, send messages and minor bugs

  Revision 1.88  2002/09/05 10:12:08  lazarus

  New dialog for multiline caption of TCustomLabel.
  Prettified TStrings property editor.
  Memo now has automatic scrollbars (not fully working), WordWrap and Scrollbars property
  Removed saving of old combo text (it broke things and is not needed). Cleanups.

  Revision 1.87  2002/09/03 20:02:01  lazarus
  Intermediate UI patch to show a bug.

  Revision 1.86  2002/09/03 11:32:51  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.85  2002/09/03 08:07:21  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.84  2002/09/02 19:10:32  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.83  2002/08/31 11:37:11  lazarus
  MG: fixed destroying combobox

  Revision 1.82  2002/08/31 10:55:16  lazarus
  MG: fixed range check error in ampersands2underscore

  Revision 1.81  2002/08/31 07:58:22  lazarus
  MG: fixed resetting comobobox text

  Revision 1.80  2002/08/30 12:32:23  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.79  2002/08/29 00:07:02  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.78  2002/08/28 09:40:50  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.77  2002/08/27 18:45:14  lazarus
  MG: propedits text improvements from Andrew, uncapturing, improved comobobox

  Revision 1.76  2002/08/27 06:40:51  lazarus
  MG: ShortCut support for buttons from Andrew

  Revision 1.75  2002/08/24 12:55:00  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.74  2002/08/24 07:09:04  lazarus
  MG: fixed bracket hilighting

  Revision 1.73  2002/08/24 06:51:23  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.72  2002/08/23 07:05:17  lazarus
  MG: started form renaming

  Revision 1.71  2002/08/22 16:43:36  lazarus
  MG: improved theme support from Andrew

  Revision 1.70  2002/08/22 16:22:39  lazarus
  MG: started debugging of mouse capturing

  Revision 1.69  2002/08/22 07:30:16  lazarus
  MG: freeing more unused GCs

  Revision 1.68  2002/08/21 13:35:25  lazarus
  MG: accelerations for synedit

  Revision 1.67  2002/08/21 11:29:36  lazarus
  MG: fixed mem some leaks in ide and gtk

  Revision 1.66  2002/08/21 10:46:37  lazarus
  MG: fixed unreleased gdiRegions

  Revision 1.65  2002/08/19 20:34:48  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.64  2002/08/19 18:00:03  lazarus
  MG: design signals for gtk internal widgets

  Revision 1.63  2002/08/19 08:53:45  lazarus
  MG: fixed broken commit

  Revision 1.62  2002/08/19 08:50:28  lazarus
  MG: fixed parser for Clx enums and empty param lists

  Revision 1.61  2002/08/17 11:38:04  lazarus
  MG: fixed keygrabbing key translation

  Revision 1.60  2002/08/16 17:47:39  lazarus
  MG: added some IDE menuicons, fixed submenu indicator bug

  Revision 1.59  2002/08/15 15:46:49  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.58  2002/08/15 15:11:01  lazarus
  MG: fixed showing menu accelarator shortcuts

  Revision 1.57  2002/08/15 13:37:58  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.56  2002/08/05 07:43:29  lazarus
  MG: fixed BadCursor bug and Circle Reference of FixedWidget of csPanel

  Revision 1.55  2002/08/04 07:09:29  lazarus
  MG: fixed client events

  Revision 1.54  2002/07/23 07:40:52  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.53  2002/07/20 13:47:04  lazarus
  MG: fixed eventmask for realized windows

  Revision 1.52  2002/07/09 17:18:23  lazarus
  MG: fixed parser for external vars

  Revision 1.51  2002/06/26 15:11:10  lazarus
  MG: added new tool: Guess misplaced $IFDEF/$ENDIF

  Revision 1.50  2002/06/21 18:27:28  lazarus
  MG: non visual component icons are now centered

  Revision 1.49  2002/06/21 17:54:24  lazarus
  MG: in design mode the mouse cursor is now also set for hidden gdkwindows

  Revision 1.48  2002/06/21 16:59:16  lazarus
  MG: TControl.Cursor is now set, reduced auto reaction of widgets in design mode

  Revision 1.47  2002/06/19 19:46:10  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.46  2002/06/14 14:57:07  lazarus
  MG: fixed open file at cursor search path

  Revision 1.45  2002/06/11 13:41:11  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.44  2002/06/09 14:00:42  lazarus
  MG: fixed persistent caret and implemented Form.BorderStyle=bsNone

  Revision 1.43  2002/06/04 15:17:23  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.42  2002/05/31 06:45:23  lazarus
  MG: deactivated new system colors, till we got a consistent solution

  Revision 1.41  2002/05/30 14:11:13  lazarus
  MG: added filters and history to TOpenDialog

  Revision 1.40  2002/05/29 21:44:39  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.39  2002/05/28 19:39:46  lazarus
  MG: added gtk rc file support and started stule dependent syscolors

  Revision 1.38  2002/05/13 14:47:02  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.37  2002/05/12 04:56:21  lazarus
  MG: client rect bugs nearly completed

  Revision 1.36  2002/05/10 06:05:57  lazarus
  MG: changed license to LGPL

  Revision 1.35  2002/05/09 12:41:30  lazarus
  MG: further clientrect bugfixes

  Revision 1.34  2002/05/06 08:50:37  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.33  2002/04/26 12:26:51  lazarus
  MG: improved clean up

  Revision 1.32  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

  Revision 1.31  2002/03/31 22:01:38  lazarus
  MG: fixed unreleased/unpressed Ctrl/Alt/Shift

  Revision 1.30  2002/03/25 17:59:23  lazarus
  GTK Cleanup
  Shane

  Revision 1.29  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.28  2001/12/10 11:16:00  lazarus
  MG: added GDK_dead_circumflex key

  Revision 1.26  2001/11/16 20:08:41  lazarus
  Object inspector has hints now.
  Shane

  Revision 1.25  2001/11/12 16:56:08  lazarus
  MG: CLIPBOARD

  Revision 1.24  2001/10/31 16:29:23  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.23  2001/10/08 12:57:07  lazarus
  MG: fixed GetPixel

  Revision 1.22  2001/10/08 08:05:08  lazarus
  MG: fixed TColorDialog set color

  Revision 1.21  2001/10/07 07:28:34  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.20  2001/09/30 08:34:52  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.19  2001/06/20 17:34:37  lazarus
  MG: fixed unknown special key code

  Revision 1.17  2001/06/20 13:35:51  lazarus
  MG: added VK_IRREGULAR and key grabbing

  Revision 1.16  2001/06/16 09:14:39  lazarus
  MG: added lazqueue and used it for the messagequeue

  Revision 1.15  2001/06/05 10:32:06  lazarus
  MG: small bugfixes for bitbtn, handles

  Revision 1.14  2001/03/21 23:48:29  lazarus
  MG: fixed window positions

  Revision 1.12  2001/03/19 14:44:22  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.10  2001/01/25 21:38:57  lazarus
  MWE:
    * fixed lil bug I commetted yesterday (listbox crash)

  Revision 1.9  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.8  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.7  2001/01/08 21:59:36  lazarus
  MWE:
    ~ applieed patch from Peter Vreman to reflect compiler fix

  Revision 1.6  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.5  2000/10/09 22:50:32  lazarus
  MWE:
    * fixed some selection code
    + Added selection sample

  Revision 1.4  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.3  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.2  2000/07/30 21:48:34  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

  Revision 1.8  2000/06/29 18:08:56  lazarus
  Shane
    Looking for the editor problem I made a few changes.  I changed everything back to the original though.

  Revision 1.7  2000/06/19 18:21:22  lazarus
  Spinedit was never getting created
  Shane

  Revision 1.6  2000/06/14 21:51:27  lazarus
  MWE:
    + Added menu accelerators. Not finished

  Revision 1.5  2000/05/11 22:04:16  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.4  2000/05/10 22:52:58  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.3  2000/05/10 01:45:12  lazarus
  Replaced writelns with Asserts.
  Put ERROR and WARNING messages back to writelns.            CAW

  Revision 1.2  2000/05/08 15:56:59  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.1  2000/03/30 22:51:42  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.11  2000/03/30 21:57:44  lazarus
  MWE:
    + Added some general functions to Get/Set the Main/Fixed/CoreChild
      widget
    + Started with graphic scalig/depth stuff. This is way from finished

  Hans-Joachim Ott <hjott@compuserve.com>:
    + Added some improvements for TMEMO

  Revision 1.10  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.9  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.8  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.7  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.6  2000/01/22 20:07:47  lazarus
  Some cleanups. It needs much more cleanup than this.
  Worked around a compiler bug (?) in mwCustomEdit.
  Reverted some changes to font generation and increased font size.

  Revision 1.5  1999/09/17 14:58:54  lazarus
  Changes made to editor.pp
  Can now press END and some other similiar keys work.  Typing works,
  but doesn't paint correctly yet.

  Revision 1.4  1999/07/31 06:39:30  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
