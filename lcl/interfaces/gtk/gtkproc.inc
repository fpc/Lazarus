{%MainUnit gtkproc.pp}

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
  //{$C+}
  //{$DEFINE ASSERT_IS_ON}
{$ENDIF}

function gtk_widget_get_xthickness(Style : PGTKStyle) : gint;
begin  
  If (Style <> nil) then begin
    {$IfNDef GTK2}
      If (Style^.klass = nil) then
        result := 0
      else
    {$EndIf}
        result := Style^.{$IfNDef GTK2}klass^.{$EndIF}xthickness
  end else
    result := 0;
end;

function gtk_widget_get_ythickness(Style : PGTKStyle) : gint;
begin  
  If (Style <> nil) then begin
    {$IfNDef GTK2}
      If (Style^.klass = nil) then
        result := 0
      else
    {$EndIf}
        result := Style^.{$IfNDef GTK2}klass^.{$EndIF}ythickness
  end else
    result := 0;
end;

function gtk_widget_get_xthickness(Style : PGTKWidget) : gint; overload;
begin
  result := gtk_widget_get_xthickness(gtk_widget_get_style(Style));
end;

function gtk_widget_get_ythickness(Style : PGTKWidget) : gint; overload;
begin
  result := gtk_widget_get_ythickness(gtk_widget_get_style(Style));
end;

procedure gdk_event_key_get_string(Event : PGDKEventKey; var theString : Pointer);
begin
  {$IfDef GTK2}
  theString := Pointer(Event^._String);
  {$Else}
  theString := Pointer(Event^.TheString);
  {$EndIF}
end;

procedure gdk_event_key_set_string(Event: PGDKEventKey; const NewString: PChar
  );
var
  OldString: PChar;
begin
  {$IfDef GTK2}
  OldString := Pointer(Event^._String);
  {$Else}
  OldString := Pointer(Event^.TheString);
  {$EndIF}
  // MG: should we set Event^.length := 0; or is this used for mem allocation?
  if (OldString<>nil) then begin
    if (NewString<>nil) then
      OldString[0]:=NewString[0]
    else
      OldString[0]:=#0;
  end;
end;

function gdk_event_get_type(Event : Pointer) : TGdkEventType;
begin
  {$IfDef GTK2}
  result := PGdkEvent(Event)^._type;
  {$Else}
  result := PGdkEvent(Event)^.TheType;
  {$EndIF}
end;

procedure RememberKeyEventWasHandledByLCL(Event: PGdkEventKey;
  BeforeEvent: boolean);
var
  HandledEvent: TLCLHandledKeyEvent;
  EventList: TList;
begin
  if KeyEventWasHandledByLCL(Event,BeforeEvent) then exit;
  if BeforeEvent then begin
    if LCLHandledKeyEvents=nil then
      LCLHandledKeyEvents:=TList.Create;
    EventList:=LCLHandledKeyEvents;
  end else begin
    if LCLHandledKeyAfterEvents=nil then
      LCLHandledKeyAfterEvents:=TList.Create;
    EventList:=LCLHandledKeyAfterEvents;
  end;
  HandledEvent:=TLCLHandledKeyEvent.Create(Event);
  EventList.Add(HandledEvent);
  while EventList.Count>10 do begin
    HandledEvent:=TLCLHandledKeyEvent(EventList[0]);
    HandledEvent.Free;
    EventList.Delete(0);
  end;
end;

function KeyEventWasHandledByLCL(Event: PGdkEventKey; BeforeEvent: boolean
  ): boolean;
var
  i: Integer;
  HandledEvent: TLCLHandledKeyEvent;
  EventList: TList;
begin
  Result:=false;
  if BeforeEvent then
    EventList:=LCLHandledKeyEvents
  else
    EventList:=LCLHandledKeyAfterEvents;
  if EventList=nil then exit;
  for i:=0 to EventList.Count-1 do begin
    HandledEvent:=TLCLHandledKeyEvent(EventList[i]);
    if HandledEvent.IsEqual(Event) then begin
      Result:=true;
      exit;
    end;
  end;
end;

{$IfNdef GTK2}
function gtk_class_get_type(aclass : Pointer) : TGtkType;
begin
  If (aclass <> nil) then
    result := PGtkTypeClass(aclass)^.thetype
  else
    result := 0;
end;

function gtk_object_get_class(anobject : Pointer) : Pointer;
begin
  If (anobject <> nil) then
    result := PGtkTypeObject(anobject)^.klass
  else
    result := nil;
end;

function gtk_window_get_modal(window:PGtkWindow):gboolean;
begin
  if assigned(Window) then
    result := (Window^.flag0 and bm_modal)<>0
  else
    result := False;
end;

function gtk_bin_get_child(bin : PGTKBin) : PGTKWidget;
begin
  if (bin <> nil) then
    result := bin^.Child
  else
    result := nil;
end;

Procedure gtk_menu_item_set_right_justified(menu_item : PGtkMenuItem; right_justified : gboolean);
begin
  if right_justified then
    menu_item^.flag0:=menu_item^.flag0 or bm_right_justify
  else
    menu_item^.flag0:=menu_item^.flag0 and (not bm_right_justify);
end;

Function gtk_check_menu_item_get_active(menu_item : PGtkCheckMenuItem) : gboolean;
begin
   Result:=(menu_item^.flag0 and bm_checkmenuitem_active <> 0);
end;

Procedure gtk_menu_append(menu : PGTKWidget; Item : PGtkWidget);
begin
  gtk.gtk_menu_append(PGTKMenu(menu), Item);
end;

Procedure gtk_menu_insert(menu : PGtkWidget; Item : PGTKWidget; Index : gint);
begin
  gtk.gtk_menu_insert(PGTKMenu(menu), Item, Index);
end;

Procedure gtk_menu_bar_insert(menubar : PGtkWidget; Item : PGTKWidget; Index : gint);
begin
  gtk.gtk_menu_bar_insert(PGtkMenuBar(menubar), Item, Index);
end;

Function gtk_image_new :PGTKWidget;
begin
  result := gtk.gtk_image_new(nil,nil);
end;

Function gtk_toolbar_new : PGTKWidget;
begin
  result := gtk.gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_BOTH);
end;

Procedure gtk_color_selection_get_current_color(colorsel : PGTKColorSelection; Color : PGDKColor);
var
  colorArray : array[0..2] of double;
begin
  gtk_color_selection_get_color(colorsel, @colorArray[0]);
  Color^.pixel := 0;
  Color^.red := gushort(TruncToCardinal(colorArray[0] * $FFFF));
  Color^.green := gushort(TruncToCardinal(colorArray[1] * $FFFF));
  Color^.blue := gushort(TruncToCardinal(colorArray[2] * $FFFF));
  {$IFDEF VerboseColorDialog}
  DebugLn('gtk_color_selection_get_current_color ',
    ' Red=',DbgS(Color^.Red),
    ' Green=',DbgS(Color^.Green),
    ' Blue=',DbgS(Color^.Blue),
    '');
  {$ENDIF}
end;

Procedure gtk_color_selection_set_current_color(colorsel : PGTKColorSelection;
  Color : PGDKColor);
var
  SelectionColor: PGDouble;
begin
  {$IFDEF VerboseColorDialog}
  DebugLn('gtk_color_selection_set_current_color ',
    ' Red=',DbgS(Color^.Red),
    ' Green=',DbgS(Color^.Green),
    ' Blue=',DbgS(Color^.Blue),
    '');
  {$ENDIF}
  GetMem(SelectionColor,4*SizeOf(GDouble));
  try
    SelectionColor[0]:=gdouble(Color^.Red)/65535;
    SelectionColor[1]:=gdouble(Color^.Green)/65535;
    SelectionColor[2]:=gdouble(Color^.Blue)/65535;
    SelectionColor[3]:=0.0;
    gtk_color_selection_set_color(colorSel,SelectionColor);
  finally
    FreeMem(SelectionColor);
  end;
end;

procedure gdk_image_unref(Image : PGdkImage);
begin
  gdk_window_unref(PGdkWindow(Image));
end;

Function gdk_image_get_colormap(Image : PGDKImage) : PGdkColormap;
begin
  result := gdk_window_get_colormap(PGdkWindow(Image));
end;

Procedure gdk_colormap_query_color(colormap : PGDKColormap; Pixel : gulong; Result : PGDKColor);
var
  GdkColorContext: PGdkColorContext;
begin
  if (Colormap = nil) or (Result = nil) then exit;
  GdkColorContext:= gdk_color_context_new(gdk_colormap_get_visual(colormap),colormap);
  Result^.Pixel := Pixel;
  gdk_color_context_query_color(GdkColorContext, Result);
  gdk_color_context_free(GdkColorContext);
end;

Function gdk_region_intersect(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_intersect(source1, source2);
end;

Function gdk_region_union(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_union(source1, source2);
end;

Function gdk_region_subtract(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_subtract(source1, source2);
end;

Function gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_regions_xor(source1, source2);
end;

function gdk_region_copy(region: PGDKRegion): PGDKRegion;
var
  EmptyRegion: PGdkRegion;
begin
  EmptyRegion := gdk_region_new;
  Result := gdk_regions_union(region, EmptyRegion);
  gdk_region_destroy(EmptyRegion);
end;

function gdk_region_rectangle(rect: PGdkRectangle): PGDKRegion;
var
  EmptyRegion: PGdkRegion;
begin
  EmptyRegion := gdk_region_new;
  Result := gdk_region_union_with_rect(EmptyRegion,Rect);
  gdk_region_destroy(EmptyRegion);
end;

Function gdk_pixmap_create_from_xpm_d (window : PGdkWindow; var mask : PGdkBitmap; transparent_color : PGdkColor; data : PPgchar) : PGdkPixmap;
begin
  result := gdk.gdk_pixmap_create_from_xpm_d(window, @mask, transparent_color, data)
end;

Function gdk_pixmap_colormap_create_from_xpm_d (window : PGdkWindow; colormap: PGdkColormap; var mask : PGdkBitmap; transparent_color : PGdkColor; data : PPgchar) : PGdkPixmap;
begin
  result := gdk.gdk_pixmap_colormap_create_from_xpm_d(window, colormap, @mask, transparent_color, data)
end;

Function gdk_pixmap_colormap_create_from_xpm (window : PGdkWindow; colormap: PGdkColormap; var mask : PGdkBitmap; transparent_color : PGdkColor; filename : Pgchar) : PGdkPixmap;
begin
  result := gdk.gdk_pixmap_colormap_create_from_xpm(window, colormap, @mask, transparent_color, filename)
end;

{$IfNDef NoGdkPixbufLib}
Procedure gdk_pixbuf_render_pixmap_and_mask(pixbuf : PGdkPixbuf; var pixmap_return : PGdkPixmap; var mask_return : PGdkBitmap; alpha_threshold : gint);
begin
  gdkpixbuf.gdk_pixbuf_render_pixmap_and_mask(pixbuf, @pixmap_return, @mask_return, alpha_threshold);
end;
{$EndIf}

Function gdk_drawable_get_depth(Drawable : PGDKDrawable) : gint;
begin
  gdk_window_get_geometry(Drawable, nil, nil, nil, nil, @result);
end;

Procedure gdk_drawable_get_size(Drawable : PGDKDrawable; Width, Height : PGInt);
begin
  gdk_window_get_geometry(Drawable, nil, nil, Width, Height, nil);
end;

Function gdk_drawable_get_image(Drawable : PGDKDrawable; x, y, width, height : gint) : PGdkImage;
begin
  result := gdk_image_get(Drawable, x, y, width, height);
end;

Function gdk_drawable_get_colormap(Drawable : PGDKDrawable) : PGdkColormap;
begin
  result := gdk_window_get_colormap(Drawable);
end;

{$EndIf}

{$Ifdef GTK2}
function gtk_class_get_type(aclass : Pointer) : TGtkType;
begin
  If (aclass <> nil) then
    result := PGtkTypeClass(aclass)^.g_Type
  else
    result := 0;
end;

function gtk_object_get_class(anobject : Pointer) : Pointer;
begin
  If (anobject <> nil) then
    result := PGtkTypeObject(anobject)^.g_Class
  else
    result := nil;
end;

function gtk_window_get_modal(window:PGtkWindow):gboolean;
begin
  if assigned(Window) then
    result := GTK2.gtk_window_get_modal(window)
  else
    result := False;
end;

Function gdk_region_union_with_rect(region:PGdkRegion; rect:PGdkRectangle) : PGdkRegion;
begin
  result := gdk_region_copy(region);
  GDK2.gdk_region_union_with_rect(result, rect);
end;

Function gdk_region_intersect(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_region_copy(source1);
  GDK2.gdk_region_intersect(result, source2);
end;

Function gdk_region_union(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_region_copy(source1);
  GDK2.gdk_region_union(result, source2);
end;

Function gdk_region_subtract(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_region_copy(source1);
  GDK2.gdk_region_subtract(result, source2);
end;

Function gdk_region_xor(source1:PGdkRegion; source2:PGdkRegion) : PGdkRegion;
begin
  result := gdk_region_copy(source1);
  GDK2.gdk_region_xor(result, source2);
end;

Procedure gdk_text_extents(FontDesc : PPangoFontDescription; Str : PChar;
  LineLength : Longint; lbearing, rbearing, width, ascent, descent : Pgint);
var
  Layout : PPangoLayout;
  AttrList : PPangoAttrList;
  Attr : PPangoAttribute;
  Extents : TPangoRectangle;
begin
  GetStyle(lgsDefault);
  Layout := gtk_widget_create_pango_layout (GetStyleWidget(lgsDefault), nil);
  pango_layout_set_font_description(Layout, FontDesc);
  AttrList := pango_layout_get_attributes(Layout);

  If (AttrList = nil) then
    AttrList := pango_attr_list_new();

  Attr := pango_attr_underline_new(PANGO_UNDERLINE_NONE);

  pango_attr_list_change(AttrList,Attr);

  Attr := pango_attr_strikethrough_new(False);
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

  pango_layout_set_text(Layout,  Str, Linelength);

  if Assigned(width) then
    pango_layout_get_pixel_size(Layout, width, nil);

  pango_layout_get_extents(Layout, nil, @Extents);
  g_object_unref(Layout);

  if Assigned(lbearing) then
    lbearing^ := PANGO_LBEARING(extents) div PANGO_SCALE;

  if Assigned(rbearing) then
    rBearing^ := PANGO_RBEARING(extents) div PANGO_SCALE;

  if Assigned(ascent) then
    ascent^ := PANGO_ASCENT(extents) div PANGO_SCALE;

  if Assigned(descent) then
    descent^ := PANGO_DESCENT(extents) div PANGO_SCALE;
end;

{$EndIf}

procedure BeginGDKErrorTrap;
begin
  Inc(GdkTrapCalls);
  if GdkTrapIsSet then
    exit;

  gdk_error_trap_push; //try to prevent GDK Bad Drawable/X Windows Errors
                         // from killing us...

  {$IfDef GDK_ERROR_TRAP_FLUSH}
  gdk_flush; //only for debugging purposes DO NOT enable by default.
               // slows things down intolerably for actual use, if we ever
               // have a real need for it, it should be called from that
               // specific function, since this gets called constantly during
               // drawing.
  {$EndIf}
  
  GdkTrapIsSet:=true;
end;

procedure EndGDKErrorTrap;
var
  Xerror : gint;
begin
  Dec(GdkTrapCalls);
  if (not GdkTrapIsSet) then
    RaiseGDBException('EndGDKErrorTrap without BeginGDKErrorTrap');
  if (GdkTrapCalls > 0) then
    exit;
    
  Xerror := gdk_error_trap_pop;

  GdkTrapIsSet:=false;

  {$IfDef REPORT_GDK_ERRORS}
  If (Xerror<>0) then
    RaiseException('A GDK/X Error occured, this is normally fatal. The error code was : ' + IntToStr(Xerror));
  {$EndIf}
end;


{------------------------------------------------------------------------------
  procedure RaiseException(const Msg: string);

  Raises an exception.
  gdb does not catch fpc Exception objects, therefore this procedure raises
  a standard AV which is catched by gdb.
 ------------------------------------------------------------------------------}
procedure RaiseException(const Msg: string);
begin
  DebugLn('ERROR in gtk-interface: ',Msg);
  // creates an exception, that gdb catches:
  DebugLn('Creating gdb catchable error:');
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

procedure RaiseException(const Msg: string; Args: array of const);
begin
  RaiseException(Format(Msg, Args));
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
  Result:=(Widget<>nil)
    and (gtk_object_get_class(Widget)<>nil)
    and gtk_type_is_a(gtk_class_get_type(gtk_object_get_class(Widget)), AType);
end;

{------------------------------------------------------------------------------
  function GetWidgetClassName(Widget: PGtkWidget): string;

  Returns the gtk class name of Widget.
 ------------------------------------------------------------------------------}
function GetWidgetClassName(Widget: PGtkWidget): string;
var
  AType: TGtkType;
  ClassPGChar: Pgchar;
  ClassLen: Integer;
begin
  Result:='';
  if (gtk_object_get_class(Widget)=nil) then begin
    Result:='<Widget without class>';
    exit;
  end;
  AType:=gtk_class_get_type(gtk_object_get_class(Widget));
  ClassPGChar:=gtk_type_name(AType);
  if ClassPGChar=nil then begin
    Result:='<Widget without classname>';
    exit;
  end;
  ClassLen:=strlen(ClassPGChar);
  SetLength(Result,ClassLen);
  if ClassLen>0 then
    Move(ClassPGChar[0],Result[1],ClassLen);
end;

function GetWidgetDebugReport(Widget: PGtkWidget): string;
var
  LCLObject: TObject;
  AWinControl: TWinControl;
  MainWidget: PGtkWidget;
  WinWidgetInfo: PWinWidgetInfo;
  FixedWidget: PGTKWidget;
begin
  if Widget = nil
  then begin
    Result := 'nil';
    exit;
  end;
  Result := Format('%p=%s %s', [Pointer(Widget), GetWidgetClassName(Widget), WidgetFlagsToString(Widget)]);
  LCLObject:=GetNearestLCLObject(Widget);
  Result := Result + Format(' LCLObject=%p', [Pointer(LCLObject)]);
  if LCLObject=nil then exit;
  if LCLObject is TControl then
    Result:=Result+'='+TControl(LCLObject).Name+':'+LCLObject.ClassName
  else
    Result:=Result+'='+LCLObject.ClassName;
  if LCLObject is TWinControl then begin
    AWinControl:=TWinControl(LCLObject);
    if AWinControl.HandleAllocated then begin
      MainWidget:=PGTKWidget(AWinControl.Handle);
      if MainWidget=Widget
      then Result:=Result+'<Is MainWidget>'
      else Result:=Result+Format('<MainWidget=%p=%s>', [Pointer(MainWidget), GetWidgetClassName(MainWidget)]);
      FixedWidget:=GetFixedWidget(MainWidget);
      if FixedWidget=Widget then
        Result:=Result+'<Is FixedWidget>';
      WinWidgetInfo:=GetWidgetInfo(MainWidget,false);
      if WinWidgetInfo<>nil then begin
        if WinWidgetInfo^.CoreWidget = Widget then
          Result:=Result+'<Is CoreWidget>';
      end;
    end
    else begin
      Result:=Result+'<NOT HandleAllocated>'
    end;
  end;
end;

function GetWindowDebugReport(AWindow: PGDKWindow): string;
var
  p: Pgpointer;
  Widget: PGtkWidget;
  WindowType: TGdkWindowType;
  Width: Integer;
  Height: Integer;
  {$ifdef gtk1}
  Visual: PGdkVisual;
  {$endif}
  TypeAsStr: String;
begin
  Result:=DbgS(AWindow);
  if AWindow=nil then exit;
  
  // window type
  WindowType:=gdk_window_get_type(AWindow);
  case WindowType of
  GDK_WINDOW_ROOT: TypeAsStr:='Root';
  GDK_WINDOW_TOPLEVEL: TypeAsStr:='TopLvl';
  GDK_WINDOW_CHILD: TypeAsStr:='Child';
  GDK_WINDOW_DIALOG: TypeAsStr:='Dialog';
  GDK_WINDOW_TEMP: TypeAsStr:='Temp';
  {$ifdef gtk1}
  GDK_WINDOW_PIXMAP: TypeAsStr:='Pixmap';
  {$endif gtk1}
  GDK_WINDOW_FOREIGN: TypeAsStr:='Foreign';
  else TypeAsStr:='Unknown';
  end;
  Result:=Result+' Type='+TypeAsStr;
  
  DebugLn(Result);
  // user data
  if WindowType in [GDK_WINDOW_ROOT,GDK_WINDOW_TOPLEVEL,GDK_WINDOW_CHILD,
    GDK_WINDOW_DIALOG]
  then begin
    p:=nil;
    gdk_window_get_user_data(AWindow,p);
    if GtkWidgetIsA(PGTKWidget(p),GTKAPIWidget_GetType) then begin
      Widget:=PGTKWidget(p);
      Result:=Result+'<Widget['+GetWidgetDebugReport(Widget)+']>';
    end else begin
      Result:=Result+'<UserData='+DbgS(p)+']>';
    end;
  end;

  // size
  gdk_window_get_size(AWindow,@Width,@Height);
  Result:=Result+' Size='+IntToStr(Width)+'x'+IntToStr(Height);

  {$ifdef gtk1}
  // visual
  Visual:=gdk_window_get_visual(AWindow);
  if Visual<>nil then begin
    if WindowType in [GDK_WINDOW_PIXMAP] then begin
      Result:=Result+' Depth='+IntToStr(Visual^.bits_per_rgb);
    end;
  end;
  {$endif gtk1}
end;

function GetStyleDebugReport(AStyle: PGTKStyle): string;
begin
  Result:='[';
  if AStyle=nil then
    Result:=Result+'nil'
  else begin
    Result:=Result+'FG[N]:='+GdkColorToStr(@AStyle^.fg[GTK_STATE_NORMAL])+' ';
    Result:=Result+'BG[N]:='+GdkColorToStr(@AStyle^.bg[GTK_STATE_NORMAL])+' ';
    Result:=Result+'BG_Pixmap[N]:='+DbgS(AStyle^.bg_pixmap[GTK_STATE_NORMAL])+' ';
    Result:=Result+'rc_style='+GetRCStyleDebugReport(AStyle^.rc_style);
  end;
  Result:=Result+']';
end;

function GetRCStyleDebugReport(AStyle: PGtkRcStyle): string;
begin
  Result:='[';
  if AStyle=nil then
    Result:=Result+'nil'
  else begin
    Result:=Result+'name="'+AStyle^.name+'" ';
{$IFDEF GTK1}
    Result:=Result+'font_name="'+AStyle^.font_name+'" ';
    Result:=Result+'fontset_name="'+AStyle^.fontset_name+'" ';
{$ELSE GTK1}
    {$WARNING TODO find GTK2 font naming}
{$ENDIF GTK1}
    Result:=Result+'bg_pixmap_name[N]="'+AStyle^.bg_pixmap_name[GTK_STATE_NORMAL]+'" ';
{$IFDEF GTK1}
    Result:=Result+'engine='+DbgS(AStyle^.engine);
{$ELSE GTK1}
    {$WARNING TODO find GTK2 theme engine}
{$ENDIF GTK1}
  end;
  Result:=Result+']';
end;

function WidgetFlagsToString(Widget: PGtkWidget): string;
begin
  Result:='[';
  if Widget=nil then
    Result:=Result+'nil'
  else begin
    if GTK_WIDGET_REALIZED(Widget) then
      Result:=Result+'R';
    if GTK_WIDGET_MAPPED(Widget) then
      Result:=Result+'M';
    if GTK_WIDGET_VISIBLE(Widget) then
      Result:=Result+'V';
    if GTK_WIDGET_DRAWABLE(Widget) then
      Result:=Result+'D';
    if GTK_WIDGET_CAN_FOCUS(Widget) then
      Result:=Result+'F';
  end;
  Result:=Result+']';
end;

function GdkColorToStr(Color: PGDKColor): string;
begin
  if Color=nil then
    Result:='nil'
  else
    Result:='R'+HexStr(Color^.Red,4)+'G'+HexStr(Color^.Green,4)
           +'B'+HexStr(Color^.Blue,4);
end;

function GetWidgetStyleReport(Widget: PGtkWidget): string;
var
  AStyle: PGtkStyle;
  ARCStyle: PGtkRcStyle;
begin
  Result:='';
  if Widget=nil then exit;
  AStyle:=gtk_widget_get_style(Widget);
  if AStyle=nil then begin
    Result:='nil';
    exit;
  end;
  Result:=Result+'attach_count='+dbgs(AStyle^.attach_count);
  ARCStyle:=AStyle^.rc_style;
  if ARCStyle=nil then begin
    Result:=Result+' rc_style=nil';
  end else begin
    Result:=Result+' rc_style=[';
{$IFDEF GTK1}
    Result:=Result+ARCStyle^.font_name+',';
    Result:=Result+ARCStyle^.fontset_name+',';
{$ELSE GTK1}
    {$WARNING TODO find GTK2 font naming}
{$ENDIF GTK1}
    Result:=Result+']';
  end;
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
var
  Info: PWidgetInfo;
begin
  Info := GetWidgetInfo(GtkObject, True);
  if Info = nil
  then begin
    Result := 0;
    Exit;
  end;
  
  Inc(Info^.ChangeLock, LockOffset);
  Result := Info^.ChangeLock;
end;

{------------------------------------------------------------------------------
  procedure HideCaretOfWidgetGroup(ChildWidget: PGtkWidget;
    var MainWidget: PGtkWidget; var CaretWasVisible: boolean);

  Find main widget and if it is a API widget, hide caret.
 ------------------------------------------------------------------------------}
procedure HideCaretOfWidgetGroup(ChildWidget: PGtkWidget;
  var MainWidget: PGtkWidget; var CaretWasVisible: boolean);
var
  LCLObject: TObject;
  IsAPIWidget: Boolean;
begin
  MainWidget:=ChildWidget;
  LCLObject:=GetNearestLCLObject(ChildWidget);
  if (LCLObject is TWinControl) then
    MainWidget:=PGtkWidget(TWinControl(LCLObject).Handle);
  IsAPIWidget:=GtkWidgetIsA(MainWidget,GTKAPIWidget_GetType);
  CaretWasVisible:=false;
  if IsAPIWidget then
    GTKAPIWidget_HideCaret(PGTKAPIWidget(MainWidget),CaretWasVisible);
end;

{------------------------------------------------------------------------------
  procedure SetComboBoxText(ComboWidget: PGtkCombo; const NewText: string);

  Sets the text of the combobox entry.
 ------------------------------------------------------------------------------}
procedure SetComboBoxText(ComboWidget: PGtkCombo; NewText: PChar);
begin
  //DebugLn('SetComboBoxText ',DbgS(ComboWidget),' "',NewText,'"');
  // lock combobox, so that no OnChange event is fired
  LockOnChange(PGtkObject(ComboWidget^.entry),+1);
  // set text
  if NewText = nil then NewText:=#0; // gtk expects at least a #0
  //DebugLn('SetComboBoxText A ',DbgS(NewText));
  gtk_entry_set_text(PGtkEntry(ComboWidget^.entry), NewText);
  // unlock combobox
  LockOnChange(PGtkObject(ComboWidget^.entry),-1);
end;

function GetComboBoxText(ComboWidget: PGtkCombo): string;
begin
  Result:=StrPas(gtk_entry_get_text(PGtkEntry(ComboWidget^.entry)));
end;

{------------------------------------------------------------------------------
  function GetComboBoxItemIndex(ComboBox: TCustomComboBox): integer;

  Returns the current ItemIndex of a TComboBox
 ------------------------------------------------------------------------------}
function GetComboBoxItemIndex(ComboBox: TCustomComboBox): integer;
var
  ComboWidget: PGtkCombo;
  ComboStrings: TStrings;
  CurText: String;
begin
  ComboWidget:=PGtkCombo(ComboBox.Handle);
  ComboStrings:=TStrings(gtk_object_get_data(PGtkObject(ComboWidget),'LCLList'));
  CurText:=GetComboBoxText(ComboWidget);
  Result:=ComboStrings.IndexOf(CurText);
end;

{------------------------------------------------------------------------------
  procedure SetComboBoxItemIndex(ComboBox: TCustomComboBox; Index: integer);

  Returns the current ItemIndex of a TComboBox
 ------------------------------------------------------------------------------}
procedure SetComboBoxItemIndex(ComboBox: TCustomComboBox; Index: integer);
var
  ComboWidget: PGtkCombo;
  ComboStrings: TStrings;
begin
  ComboWidget:=PGtkCombo(ComboBox.Handle);
  gtk_list_select_item(PGtkList(ComboWidget^.list),Index);
  if Index>=0 then begin
    ComboStrings:=TStrings(gtk_object_get_data(PGtkObject(ComboWidget),'LCLList'));
    if Index < ComboStrings.Count
    then SetComboBoxText(ComboWidget, PChar(ComboStrings[Index]))
    else SetComboBoxText(ComboWidget, '#error#');
  end;
end;

procedure SetLabelAlignment(LabelWidget: PGtkLabel;
  const NewAlignment: TAlignment);
const
  cLabelAlignX : array[TAlignment] of gfloat = (0.0, 1.0, 0.5);
  cLabelAlignY : array[TTextLayout] of gfloat = (0.0, 0.5, 1.0);
  cLabelAlign : array[TAlignment] of TGtkJustification =
    (GTK_JUSTIFY_LEFT, GTK_JUSTIFY_RIGHT, GTK_JUSTIFY_CENTER);
begin
  gtk_label_set_justify(LabelWidget, cLabelAlign[NewAlignment]);
  gtk_misc_set_alignment(GTK_MISC(LabelWidget), cLabelAlignX[NewAlignment],
                        cLabelAlignY[tlTop]);
end;

{------------------------------------------------------------------------------
  function GtkPaintMessageToPaintMessage(var GtkPaintMsg: TLMGtkPaint;
    FreeGtkPaintMsg: boolean): TLMPaint;

  Converts a LM_GtkPaint message to a LM_PAINT message
 ------------------------------------------------------------------------------}
function GtkPaintMessageToPaintMessage(var GtkPaintMsg: TLMGtkPaint;
  FreeGtkPaintMsg: boolean): TLMPaint;
var
  PS : PPaintStruct;
begin
  Result.Msg:=LM_PAINT;
  New(PS);
  PS^.hDC:=0;
  If GtkPaintMsg.Data.RepaintAll then
    PS^.rcPaint := Rect(0,0,0,0)
  else
    PS^.rcPaint := GtkPaintMsg.Data.Rect;

  Result.DC:=BeginPaint(THandle(GtkPaintMsg.Data.Widget), PS^);
  Result.PaintStruct:=PS;
  Result.Result:=0;
  if FreeGtkPaintMsg then
    FreeThenNil(GtkPaintMsg.Data);
end;

procedure FinalizePaintMessage(Msg: PLMessage);
var
  PS : PPaintStruct;
  DC : TDeviceContext;
begin
  if (Msg^.Msg=LM_PAINT) or (Msg^.Msg=LM_INTERNALPAINT) then begin
    If Msg^.LParam <> 0 then begin
       PS := PPaintStruct(Msg^.LParam);
       If Msg^.WParam<>0 then
         DC := TDeviceContext(Msg^.WParam)
       else
         DC := TDeviceContext(PS^.hdc);
       EndPaint(THandle(DC.wnd), PS^);
       Dispose(PS);
       Msg^.LParam:=0;
       Msg^.WParam:=0;
    end else
      if Msg^.WParam<>0 then begin
        ReleaseDC(0,Msg^.WParam);
        Msg^.WParam:=0;
      end;
  end else
  if Msg^.Msg=LM_GtkPAINT then begin
    FreeThenNil(TLMGtkPaintData(Msg^.WParam));
  end;
end;

procedure FinalizePaintTagMsg(Msg: PMsg);
var
  PS : PPaintStruct;
  DC : TDeviceContext;
begin
  if (Msg^.Message=LM_PAINT) or (Msg^.Message=LM_INTERNALPAINT) then begin
    If Msg^.LParam <> 0 then begin
       PS := PPaintStruct(Msg^.LParam);
       If Msg^.WParam<>0 then
         DC := TDeviceContext(Msg^.WParam)
       else
         DC := TDeviceContext(PS^.hdc);
       EndPaint(THandle(DC.wnd), PS^);
       Dispose(PS);
       Msg^.LParam:=0;
       Msg^.WParam:=0;
    end else
      if Msg^.WParam<>0 then begin
        ReleaseDC(0,Msg^.WParam);
        Msg^.WParam:=0;
      end;
  end else
  if Msg^.Message=LM_GtkPAINT then begin
    FreeThenNil(TObject(Msg^.WParam));
  end;
end;

procedure SetGCRasterOperation(TheGC: PGDKGC; Rop: Cardinal);
begin
  Case ROP of
    WHITENESS,
    BLACKNESS,
    SRCCOPY :
      GDK_GC_Set_Function(TheGC, GDK_Copy);
    SRCPAINT :
      GDK_GC_Set_Function(TheGC, GDK_NOOP);
    SRCAND :
      GDK_GC_Set_Function(TheGC, GDK_Clear);
    SRCINVERT :
      GDK_GC_Set_Function(TheGC, GDK_XOR);
    SRCERASE :
      GDK_GC_Set_Function(TheGC, GDK_AND);
    NOTSRCCOPY :
      GDK_GC_Set_Function(TheGC, GDK_OR_REVERSE);
    NOTSRCERASE :
      GDK_GC_Set_Function(TheGC, GDK_AND);
    MERGEPAINT :
      GDK_GC_Set_Function(TheGC, GDK_Copy_Invert);
    DSTINVERT :
      GDK_GC_Set_Function(TheGC, GDK_INVERT);
    else begin
      gdk_gc_set_function(TheGC, GDK_COPY);
      DebugLn('WARNING: [SetRasterOperation] Got unknown/unsupported CopyMode!!');
    end;
  end;
end;

procedure MergeClipping(DestinationDC: TDeviceContext; DestinationGC: PGDKGC;
  X, Y, Width, Height: integer; ClipMergeMask: PGdkPixmap;
  ClipMergeMaskX, ClipMergeMaskY: integer;
  var NewClipMask: PGdkPixmap);
// merge ClipMergeMask into the destination clipping mask at the
// destination rectangle
var
  temp_gc : PGDKGC;
  temp_color : TGDKColor;
  Region: PGdiObject;
  RGNType : Longint;
  OffsetXY: TPoint;
  //ClipMergeMaskWidth, ClipMergeMaskHeight: integer;
begin
  {$IFDEF VerboseStretchCopyArea}
  DebugLn('MergeClipping START DestinationDC=',DbgS(DestinationDC),
    ' DestinationGC=',DbgS(DestinationGC),
    ' X=',X,' Y=',Y,' Width=',Width,' Height=',Height,
    ' ClipMergeMask=',DbgS(ClipMergeMask),
    ' ClipMergeMaskX=',ClipMergeMaskX,' ClipMergeMaskY=',ClipMergeMaskY);
  {$ENDIF}

  // activate clipping region of destination
  SelectGDIRegion(HDC(DestinationDC));
  NewClipMask := nil;
  if (ClipMergeMask = nil) then exit;

  BeginGDKErrorTrap;
  // create temporary mask with the size of the destination rectangle
  NewClipMask := PGdkBitmap(gdk_pixmap_new(nil, width, height, 1));
  // create temporary GC for combination mask
  temp_gc := gdk_gc_new(NewClipMask);
  gdk_gc_set_clip_region(temp_gc, nil); // no default clipping
  gdk_gc_set_clip_rectangle(temp_gc, nil);

  // clear mask
  temp_color.pixel := 0;
  gdk_gc_set_foreground(temp_gc, @temp_color);
  gdk_draw_rectangle(NewClipMask, temp_gc, 1, 0, 0, width, height);
  gdk_draw_rectangle(NewClipMask, temp_gc, 0, 0, 0, width, height);

  // copy the destination clipping mask into the temporary mask
  with DestinationDC do begin
    If (ClipRegion <> 0) then begin
      Region:=PGDIObject(ClipRegion);
      RGNType := RegionType(Region^.GDIRegionObject);
      If (RGNType <> ERROR) and (RGNType <> NULLREGION) then begin
        // destination has a clipping mask
        {$IFDEF VerboseStretchCopyArea}
        DebugLn('MergeClipping Destination has clipping mask -> apply to temp GC');
        {$ENDIF}
        // -> copy the destination clipping mask to the temporary mask
        //    The X,Y coordinate in the destination relates to
        //    0,0 in the temporary mask.
        //    The clip region of dest is always at 0,0 in dest
        OffsetXY:=Point(-X,-Y);
        // 1. Move the region
        gdk_region_offset(Region^.GDIRegionObject,OffsetXY.X,OffsetXY.Y);
        // 2. Apply region to temporary mask
        gdk_gc_set_clip_region(temp_gc, Region^.GDIRegionObject);
        // 3. Undo moving the region
        gdk_region_offset(Region^.GDIRegionObject,-OffsetXY.X,-OffsetXY.Y);
      end;
    end;
  end;

  // merge the source clipping mask into the temporary mask
  //gdk_window_get_size(ClipMergeMask,@ClipMergeMaskWidth,@ClipMergeMaskHeight);
  //DebugLn('MergeClipping A MergeMask Size=',ClipMergeMaskWidth,',',ClipMergeMaskHeight);
  gdk_draw_pixmap(NewClipMask, temp_gc,
                  ClipMergeMask, ClipMergeMaskX, ClipMergeMaskY,
                  0, 0, Width, Height);

  // free the temporary GC
  gdk_gc_destroy(temp_gc);

  // apply the new mask to the destination GC
  // The new mask has only the size of the destination rectangle, not of
  // the whole destination. Apply it to destination and move it to the right
  // position.
  gdk_gc_set_clip_mask(DestinationGC, NewClipMask);
  gdk_gc_set_clip_origin(DestinationGC, x, y);
  EndGDKErrorTrap;
end;

procedure ResetGCClipping(DC: HDC; GC: PGDKGC);
begin
  BeginGDKErrorTrap;
  gdk_gc_set_clip_mask(GC, nil);
  gdk_gc_set_clip_origin (GC, 0,0);
  SelectGDIRegion(DC);
  EndGDKErrorTrap;
end;

function ScalePixmap(ScaleGC: PGDKGC;
  SrcPixmap: PGdkPixmap; SrcX, SrcY, SrcWidth, SrcHeight: integer;
  SrcColorMap: PGdkColormap;
  NewWidth, NewHeight: integer;
  var NewPixmap: PGdkPixmap) : Boolean;
{$Ifndef NoGdkPixbufLib}
var
  ScaleSrc, ScaleDest: PGDKPixbuf;
  ShrinkWidth,
  ShrinkHeight : Boolean;
  ScaleMethod : TGDKINTERPTYPE;
  DummyMask: PGdkPixmap;
  SrcWholeWidth, SrcWholeHeight: integer;
  {$IFDEF VerboseStretchCopyArea}
  NewWholeWidth, NewWholeHeight: integer;
  {$ENDIF}
begin
  {$IFDEF VerboseStretchCopyArea}
  DebugLn('ScalePixmap ScaleGC=',DbgS(ScaleGC),
    ' SrcPixmap=[',GetWindowDebugReport(SrcPixmap),']',
    ' SrcX=',SrcX,' SrcY=',SrcY,' SrcWidth=',SrcWidth,' SrcHeight=',SrcHeight,
    ' NewPixmap=[',GetWindowDebugReport(NewPixmap),']',
    ' NewWidth=',NewWidth,' NewHeight=',NewHeight);
  {$ENDIF}
  Result := False;
  if SrcPixmap=nil then begin
    DebugLn('WARNING: ScalePixmap SrcPixmap=nil');
    exit;
  end;
  if NewPixmap<>nil then begin
    DebugLn('WARNING: ScalePixmap NewPixmap<>nil');
    exit;
  end;

  ScaleSRC := nil;
  ScaleDest := nil;

  gdk_window_get_size(PGDKWindow(SrcPixmap),@SrcWholeWidth,@SrcWholeHeight);
  if SrcX+SrcWidth>SrcWholeWidth then begin
    DebugLn('WARNING: ScalePixmap SrcX+SrcWidth>SrcWholeWidth');
  end;
  if SrcY+SrcHeight>SrcWholeHeight then begin
    DebugLn('WARNING: ScalePixmap SrcY+SrcHeight>SrcWholeHeight');
  end;

  // calculate ScaleMethod
  ShrinkWidth := NewWidth < SrcWidth;
  ShrinkHeight := NewHeight < SrcHeight;
  //GDKPixbuf Scaling is not done in the same way as Windows
  //but by rights ScaleMethod should really be chosen based
  //on the destination device's internal flag
  {GDK_INTERP_NEAREST,GDK_INTERP_TILES,
  GDK_INTERP_BILINEAR,GDK_INTERP_HYPER);}
  If ShrinkWidth and ShrinkHeight then
    ScaleMethod := GDK_INTERP_TILES
  else
    If ShrinkWidth or ShrinkHeight then
      ScaleMethod := GDK_INTERP_BILINEAR//GDK_INTERP_HYPER
    else
      ScaleMethod := GDK_INTERP_BILINEAR;

  // Creating PixBuf from pixmap
  {$IFDEF VerboseStretchCopyArea}
  DebugLn('ScalePixmap Creating PixBuf from pixmap SrcWhole=',SrcWholeWidth,',',SrcWholeHeight);
  {$ENDIF}
  ScaleSRC := gdk_pixbuf_get_from_drawable(nil,SrcPixmap,
    SrcColorMap,0,0,SrcX,SrcY,SrcWidth,SrcHeight);
  If ScaleSRC = nil then begin
    DebugLn('WARNING: ScalePixmap ScaleSRC=nil');
    exit;
  end;

  // Scaling PixBuf
  {$IFDEF VerboseStretchCopyArea}
  DebugLn('ScalePixmap Scaling PixBuf ',
    ' Width=',gdk_pixbuf_get_width(ScaleSrc),
    ' Height=',gdk_pixbuf_get_height(ScaleSrc),
    ' HasAlpha=',gdk_pixbuf_get_has_alpha(ScaleSrc),
    ' RowStride=',gdk_pixbuf_get_rowstride(ScaleSrc),
    '');
  {$ENDIF}
  ScaleDest := gdk_pixbuf_scale_simple(ScaleSRC,NewWidth,NewHeight,ScaleMethod);
  GDK_Pixbuf_Unref(ScaleSRC);
  If ScaleDest = nil then begin
    DebugLn('WARNING: ScalePixmap ScaleDest=nil');
    exit;
  end;
  BeginGDKErrorTrap;

  // Creating pixmap from scaled pixbuf
  {$IFDEF VerboseStretchCopyArea}
  DebugLn('ScalePixmap Creating pixmap from scaled pixbuf',
    ' Width=',gdk_pixbuf_get_width(ScaleDest),
    ' Height=',gdk_pixbuf_get_height(ScaleDest),
    ' HasAlpha=',gdk_pixbuf_get_has_alpha(ScaleDest),
    ' RowStride=',gdk_pixbuf_get_rowstride(ScaleDest),
    '');
  {$ENDIF}
  DummyMask:=nil;
  gdk_pixbuf_render_pixmap_and_mask(ScaleDest,NewPixmap,DummyMask,0);

  // clean up
  {$IFDEF VerboseStretchCopyArea}
  gdk_window_get_size(PGDKWindow(NewPixmap),@NewWholeWidth,@NewWholeHeight);
  DebugLn('ScalePixmap RESULT NewPixmap=',DbgS(NewPixmap),
    ' DummyMask=',DbgS(DummyMask),
    ' NewWidth=',NewWholeWidth,' NewHeight=',NewWholeHeight,
    '');
  {$ENDIF}
  if DummyMask<>nil then gdk_pixmap_unref(DummyMask);
  EndGDKErrorTrap;
  GDK_Pixbuf_Unref(ScaleDest);
  Result := True;
{$Else not NoGdkPixbufLib}
begin
  DebugLn('ScalePixmap GDKPixbuf support has been disabled, no stretching is available!');
  Result := True;
{$EndIf}
end;

procedure DrawImageListIconOnWidget(ImgList: TCustomImageList;
  Index: integer; DestWidget: PGTKWidget);
begin
  DrawImageListIconOnWidget(ImgList,Index,DestWidget,true,true,0,0);
end;

procedure DrawImageListIconOnWidget(ImgList: TCustomImageList;
  Index: integer; DestWidget: PGTKWidget;
  CenterHorizontally, CenterVertically: boolean;
  DestLeft, DestTop: integer);
// draw icon of imagelist centered on gdkwindow
var
  Bitmap, MaskBitmap: TBitmap;
  ImageRect: TRect;
  ImageWidth: Integer;
  ImageHeight: Integer;
  WindowWidth, WindowHeight: integer;
  DestDC: HDC;
begin
  //DebugLn('DrawImageListIconOnWidget A ',ImgList.Name,':',ImgList.ClassName,
  //  ' Index=',Index,
  //  ' DestWindow=[',GetWidgetDebugReport(DestWidget),']');
  if ImgList=nil then exit;
  if (Index<0) or (Index>=ImgList.Count) then exit;
  if (DestWidget=nil) then exit;
  ImgList.GetInternalImage(Index,Bitmap,MaskBitmap,ImageRect);
  ImageWidth:=ImageRect.Right-ImageRect.Left;
  ImageHeight:=ImageRect.Bottom-ImageRect.Top;
  if (ImageWidth<1) or (ImageHeight<1) then exit;
  
  WindowWidth:=DestWidget^.allocation.width;
  WindowHeight:=DestWidget^.allocation.height;
  if CenterHorizontally then
    DestLeft:=DestWidget^.allocation.x+((WindowWidth-ImageWidth) div 2);
  if CenterVertically then
    DestTop:=DestWidget^.allocation.y+((WindowHeight-ImageHeight) div 2);
  DestDC:=GetDC(HDC(DestWidget));

  //DebugLn('DrawImageListIconOnWidget B DestXY=',DestLeft,',',DestTop,
  //  ' DestWindowSize=',WindowWidth,',',WindowWidth,
  //  ' SrcRect=',ImageRect.Left,',',ImageRect.Top,',',ImageWidth,'x',ImageHeight);
  StretchBlt(DestDC, DestLeft,DestTop, ImageWidth, ImageHeight,
    Bitmap.Canvas.Handle,ImageRect.Left,ImageRect.Top,ImageWidth,ImageHeight,
    SRCCOPY);
  ReleaseDC(HDC(DestWidget),DestDC);
end;

function CreateGdkBitmap(Window: PGdkWindow;
  Width, Height: integer): PGdkBitmap;
var
  DummyData: Pointer;
begin
  // I didn't found a simple gdk_bitmap_new function. So, I create some
  // dummy data and use gdk_bitmap_create_from_data
  GetMem(DummyData,(((Width*Height)+7) shr 3)+1);
  Result:=gdk_bitmap_create_from_data(Window,DummyData,Width,Height);
  FreeMem(DummyData);
end;

function ExtractGdkBitmap(Bitmap: PGdkBitmap; const SrcRect: TRect): PGdkBitmap;
var
  MaxRect: TRect;
  SourceRect: TRect;
  SrcWidth: Integer;
  SrcHeight: Integer;
  GC: PGdkGC;
begin
  Result:=nil;
  if Bitmap=nil then exit;
  MaxRect:=Rect(0,0,0,0);
  gdk_window_get_size(Bitmap,@MaxRect.Right,@MaxRect.Bottom);
  IntersectRect(SourceRect,SrcRect,MaxRect);
  SrcWidth:=SourceRect.Right-SourceRect.Left;
  SrcHeight:=SourceRect.Bottom-SourceRect.Top;
  DebugLn('ExtractGdkBitmap SourceRect=',dbgs(SourceRect));
  if (SrcWidth<1) or (SrcHeight<1) then exit;
  Result:=CreateGdkBitmap(nil,SrcWidth,SrcHeight);
  GC := GDK_GC_New(Result);
  gdk_window_copy_area(Result,GC,0,0,Bitmap,
                       SourceRect.Left,SourceRect.Top,SrcWidth,SrcHeight);
  GDK_GC_Unref(GC);
end;

{$IfDef Win32}
Procedure gdk_window_copy_area(Dest : PGDKWindow; GC : PGDKGC; DestX,
  DestY : Longint; SRC : PGDKWindow; XSRC, YSRC, Width, Height : Longint);
begin
  gdk_draw_pixmap(Dest, GC, Src, XSrc, YSrc, DestX, DestY, Width, Height);
End;
{$EndIf}

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
  {$IFDEF DebugGDK}
  BeginGDKErrorTrap;
  {$ENDIF}
  gdk_colormap_alloc_color(gdk_colormap_get_system, @Result, False, True);
  {$IFDEF DebugGDK}
  EndGDKErrorTrap;
  {$ENDIF}
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
//  Assert(False, Format('Trace:> [CopyDCData] DestDC:0x%x, SourceDC:0x%x', [Integer(DestinationDC), Integer(SourceDC)]));
  Result := (DestinationDC <> nil) and (SourceDC <> nil);
  if Result
  then begin
    with DestinationDC do
    begin
      Wnd := SourceDC.Wnd;
      Drawable := SourceDC.Drawable;
      if GC<>nil then begin
        BeginGDKErrorTrap;
        gdk_gc_unref(GC);
        EndGDKErrorTrap;
        GC:=nil;
        DCFlags:=DCFlags-[dcfPenSelected];
      end;
      if (SourceDC.GC <> nil) and (Drawable <> nil) then begin
        BeginGDKErrorTrap;
        gdk_gc_get_values(SourceDC.GC, @GCValues);
        GC := gdk_gc_new_with_values(Drawable, @GCValues, 3 { $3FF});
        EndGDKErrorTrap;
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
//  Assert(False, Format('Trace:< [CopyDCData] DestDC:0x%x, SourceDC:0x%x --> %d', [Integer(DestinationDC), Integer(SourceDC), Integer(Result)]));
end;

Function RegionType(RGN: PGDKRegion) : Longint;
var
  aRect : TGDKRectangle;
  SimpleRGN: PGdkRegion;
begin
  {$IFDEF DebugGDK}
  BeginGDKErrorTrap;
  {$ENDIF}
  If RGN = nil then
    Result := ERROR
  else
    If gdk_region_empty(RGN) then
      Result := NULLREGION
  else begin
    gdk_region_get_clipbox(RGN,@aRect);
    SimpleRGN := gdk_region_rectangle(@aRect);
    if gdk_region_equal(SimpleRGN, RGN) then
      Result := SIMPLEREGION
    else
      Result := COMPLEXREGION;
    gdk_region_destroy(SimpleRGN);
  end;
  {$IFDEF DebugGDK}
  EndGDKErrorTrap;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Procedure SelectGDIRegion(const DC: HDC);

  Applies the current clipping region of the DC (DeviceContext) to the
  gc (GDK Graphic context - pgdkGC)
 ------------------------------------------------------------------------------}
Procedure SelectGDIRegion(const DC: HDC);
var
  Region: PGdiObject;
  RGNType : Longint;
begin
  with TDeviceContext(DC) do
  begin
    {$IFDEF DebugGDK}
    BeginGDKErrorTrap;
    {$ENDIF}
    gdk_gc_set_clip_region(gc,  nil);
    gdk_gc_set_clip_rectangle (gc,  nil);
    If (ClipRegion <> 0) then begin
      Region:=PGDIObject(ClipRegion);
      RGNType := RegionType(Region^.GDIRegionObject);
      If (RGNType <> ERROR) and (RGNType <> NULLREGION) then begin
        gdk_gc_set_clip_region(gc,  PGDIObject(ClipRegion)^.GDIRegionObject);
      end;
    end;
    {$IFDEF DebugGDK}
    EndGDKErrorTrap;
    {$ENDIF}
   end;
end;

function GDKRegionAsString(RGN: PGDKRegion): string;
var
  aRect: TGDKRectangle;
begin
  Result:=DbgS(RGN);
  BeginGDKErrorTrap;
  gdk_region_get_clipbox(RGN,@aRect);
  EndGDKErrorTrap;
  Result:=Result+'(x='+IntToStr(Integer(aRect.x))+',y='+IntToStr(Integer(aRect.y))+',w='
                    +IntToStr(aRect.Width)+',h='+IntToStr(aRect.Height)+' '
                    +'Type='+IntToStr(RegionType(RGN))+')';
end;

function CreateRectGDKRegion(const ARect: TRect): PGDKRegion;
var
  GDkRect: TGDKRectangle;
begin
  GDkRect.x:=ARect.Left;
  GDkRect.y:=ARect.Top;
  GDkRect.Width:=ARect.Right-ARect.Left;
  GDkRect.Height:=ARect.Bottom-ARect.Top;
  {$IFDEF DebugGDK}
  BeginGDKErrorTrap;
  {$ENDIF}
  Result:=gdk_region_rectangle(@GDKRect);
  {$IFDEF DebugGDK}
  EndGDKErrorTrap;
  {$ENDIF}
end;

Procedure FreeGDIColor(GDIColor: PGDIColor);
begin
  if (cfColorAllocated in GDIColor^.ColorFlags) then begin
    if (GDIColor^.Colormap <> nil) then begin
      BeginGDKErrorTrap;
      gdk_colormap_free_colors(GDIColor^.Colormap,@(GDIColor^.Color), 1);
      EndGDKErrorTrap;
    end;
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
    RGBColor := ColorToRGB(GDIColor^.ColorRef);

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
  IsSolidBrush, AsBackground: Boolean);
var
  GC: PGDKGC;
  GDIColor: PGDIColor;

  Procedure EnsureAsGCValues;
  var
    AllocFG : Boolean;
    SysGCValues: TGdkGCValues;
  begin
    FreeGDIColor(GDIColor);
    SysGCValues:=GetSysGCValues(GDIColor^.ColorRef,
                               PGtkWidget(TDeviceContext(DC).Wnd));
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    With SysGCValues do begin
      gdk_gc_set_fill(GC, fill);
      AllocFG := Foreground.Pixel = 0;
      If AllocFG then
        if not gdk_colormap_alloc_color(GDK_Colormap_get_system,@Foreground,
                                        True,True)
        then begin
          DebugLn('NOTE: EnsureGCColor.EnsureAsGCValues gdk_colormap_alloc_color failed ',
            ' Foreground=',
            DbgS(Foreground.red),',',
            DbgS(Foreground.green),',',
            DbgS(Foreground.blue),
            ' GDIColor^.ColorRef=',DbgS(GDIColor^.ColorRef)
            );
        end;
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
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  end;

  Procedure EnsureAsColor;
  begin
    AllocGDIColor(DC, GDIColor);
    //DebugLn('EnsureAsColor ',DbgS(GDIColor^.ColorRef),' AsBackground=',AsBackground);
    {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
    If AsBackground then
      gdk_gc_set_background(GC, @(GDIColor^.Color))
    else begin
      gdk_gc_set_fill(GC, GDK_SOLID);
      gdk_gc_set_foreground(GC, @(GDIColor^.Color));
    end;
    {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  end;

begin
  GC:=TDeviceContext(DC).GC;
  GDIColor:=nil;
  with TDeviceContext(DC) do
  begin
    case ColorType of
    dccCurrentBackColor: GDIColor:=@CurrentBackColor;
    dccCurrentTextColor: GDIColor:=@CurrentTextColor;
    dccGDIBrushColor   : GDIColor:=@(CurrentBrush^.GDIBrushColor);
    dccGDIPenColor     : GDIColor:=@(CurrentPen^.GDIPenColor);
    end;
  end;
  if GDIColor=nil then exit;
  
  // FPC bug workaround:
  // clScrollbar = $80000000 can't be used in case statements
  if TColor(GDIColor^.ColorRef)=clScrollbar then begin
    //often have a BK Pixmap
    If IsSolidBrush then
      EnsureAsGCValues
    else
      EnsureAsColor;//GC's with Pixmaps can't work w/Hatch's (yet)
    exit;
  end;
  
  Case TColor(GDIColor^.ColorRef) of
    //clScrollbar: see above
    clInfoBk,
    clMenu,
    clHighlight,
    clHighlightText,
    clBtnFace,
    clWindow,
    clForm:
      //often have a BK Pixmap
      If IsSolidBrush then
        EnsureAsGCValues
      else
        EnsureAsColor;//GC's with Pixmaps can't work w/Hatch's (yet)

    clBtnShadow,
    clBtnHighlight,
    clBtnText,
    clInfoText,
    clWindowText,
    clMenuText,
    clGrayText:
      //should never have a BK Pixmap
      EnsureAsGCValues;
    else
      EnsureAsColor;
  end;
end;

procedure CopyGDIColor(var SourceGDIColor, DestGDIColor: TGDIColor);
begin
  SetGDIColorRef(DestGDIColor,SourceGDIColor.ColorRef);
end;

function IsBackgroundColor(Color: TColor): boolean;
begin
  Result:=(Color=clForm) or (Color=clInfoBk);
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

procedure InitializePalette(const Pal: PGDIObject; const Entries: PPaletteEntry; const RGBCount: Longint);
var
  I: Integer;
  RGBValue: Longint;
begin
  for I := 0 to RGBCount - 1 do
  begin
    if PaletteIndexExists(Pal, I) then
      PaletteDeleteIndex(Pal, I);
    with Entries[I] do
      RGBValue := RGB(peRed, peGreen, peBlue) {or (peFlags shl 32)??};
    if not PaletteRGBExists(Pal, RGBValue) then
      PaletteAddIndex(Pal, I, RGBValue);
  end;
end;

{------------------------------------------------------------------------------
  Procedure: KeySymToVKeyArray
  Params:    AKeySym: The keysym the array is requested for
             ACreate: True if the array should be created if it does not exist
  Returns:   PVKeyArray: The VKey array for the LSByte of KeySym

  Retrieves or constructs a VKeyArray for VK lookup on KeySyms.
 ------------------------------------------------------------------------------}
function KeySymToVKeyArray(const AKeySym: Cardinal; const ACreate: Boolean): PVKeyArray1;
var
  K: Byte;
  P2: PVKeyArray2;
  P3: PVKeyArray3;
begin
  Result := nil;

  K := Byte(AKeySym shr 24);
  P3 := MKeySymToVK[K];
  if P3 = nil
  then begin
    if not ACreate then Exit;
    New(P3);
    FillChar(P3^, SizeOf(P3^), 0);
    MKeySymToVK[K] := P3;
  end;

  K := Byte(AKeySym shr 16);
  P2 := P3^[K];
  if P2 = nil
  then begin
    if not ACreate then Exit;
    New(P2);
    FillChar(P2^, SizeOf(P2^), 0);
    P3^[K] := P2;
  end;

  K := Byte(AKeySym shr 8);
  Result := P2^[K];
  if Result = nil
  then begin
    if not ACreate then Exit;
    New(Result);
    FillChar(Result^, SizeOf(Result^), 0);
    P2^[K] := Result;
  end;
end;

{------------------------------------------------------------------------------
  Procedure: KeySymToVKey
  Params:    AKeySym: The keysym the array is requested for
  Returns:   The VKey for the KeySym

  Retrieves a VKey for a KeySyms.
 ------------------------------------------------------------------------------}
function KeySymToVKey(const AKeySym: Cardinal): TVKeyRecord;
var
  P: PVKeyArray1;
begin
  P := KeySymToVKeyArray(AKeySym, False);
  if P = nil
  then begin
    Result.VKey := $FF;
    Result.Flags := $FF;
  end
  else begin
    Result := P^[AKeySym and $FF];
  end;
end;

function HandleGTKKeyUpDown(Widget: PGtkWidget; Event: PGdkEventKey;
  Data: gPointer; BeforeEvent: boolean) : GBoolean;
{off $DEFINE VerboseKeyboard}
var
  Msg: TLMKey;
  EventStopped: Boolean;
  EventString: PChar; // GTK1 and GTK2 workaround
                      // (and easy access to bytes)
  Character: TUTF8Char;

  procedure StopKeyEvent(const AnEventName: PChar);
  begin
    {$IFDEF VerboseKeyboard}
    DebugLn('StopKeyEvent AnEventName="',AnEventName,'" BeforeEvent=',dbgs(BeforeEvent));
    {$ENDIF}
    if not EventStopped
    then begin
      g_signal_stop_emission_by_name(PGtkObject(Widget), AnEventName);
      EventStopped := True;
    end;

    //MWE: still need to skip on win32 ?
    {MWE:.$IfNDef Win32}
    if EventString <> nil
    then begin
      gdk_event_key_set_string(Event,#0);
      Event^.length:=0;
    end;
    {MWE:.$EndIf}

    Event^.KeyVal := 0;
  end;

  function CanSendChar: Boolean;
  begin
    Result := False;
    //debugln('GDK_KEY_PRESS CanSendChar BeforeEvent=',dbgs(BeforeEvent),' Event^.Length=',dbgs(Event^.Length),' Event^.KeyVal=',dbgs(Event^.KeyVal),'=$',HexStr(Event^.KeyVal,4),' ',binStr(Event^.State,16));
    if Event^.Length > 1 then Exit;

    // to be delphi compatible we should not send a space here
    if Event^.KeyVal = GDK_KEY_KP_SPACE then Exit;

    // Check if CTRL is pressed
    if ((Event^.State and GDK_CONTROL_MASK) <> 0)
    then begin
      // Check if we pressed ^@
      if (Event^.Length = 0)
      and (Event^.KeyVal = GDK_KEY_AT)
      then begin
        Result := True;
        Exit;
      end;
      // check if we send the ^Char subset
      if (Event^.Length = 1) and (EventString <> nil)
      then begin
        Result := (EventString^ > #0) and (EventString^ < ' ');
      end;
      Exit;
    end;
    Result := (Event^.Length>0);
              //and (not (Event^.KeyVal in [0..$F000,GDK_KP_0..GDK_KP_9]);
  end;

var
  VKey: TVKeyRecord;
  CommonKeyData: Integer;
  Flags: Integer;
  SysKey: Boolean;
  FocusedWidget: PGtkWidget;
  LCLObject: TObject;
  FocusedWinControl: TWinControl;
  HandledByLCL: Boolean;
  TargetWidget: PGtkWidget;
  TargetData: gPointer;
  KeyPressesChar: char;
begin
  Result := CallBackDefaultReturn;

  EventStopped := False;
  HandledByLCL:=KeyEventWasHandledByLCL(Event,BeforeEvent);

  {$IFDEF VerboseKeyboard}
  DebugLn('[HandleGTKKeyUpDown] ',TControl(Data).Name,':',TControl(Data).ClassName,
    ' ',dbgs(Event^.{$IFDEF GTK1}theType{$ELSE}_Type{$ENDIF}),' Widget=',GetWidgetClassName(Widget),
    ' Before=',dbgs(BeforeEvent),' HandledByLCL=',dbgs(HandledByLCL));
  {$ENDIF}

  // handle every key event only once
  if HandledByLCL then exit;

  TargetWidget:=Widget;
  TargetData:=Data;

  // The gtk sends keys first to the gtkwindow and then to the focused control.
  // The LCL expects only once to the focused control.
  // And some gtk widgets (combo) eats keys, so that the LCL has no chance to
  // handle it. Therefore keys to the form are immediately redirected to the
  // focused control without changing the normal gtk event path.
  if GtkWidgetIsA(Widget,gtk_window_get_type) then begin
    FocusedWidget:=PGtkWindow(Widget)^.focus_widget;
    if FocusedWidget<>nil then begin
      LCLObject:=GetNearestLCLObject(FocusedWidget);
      if LCLObject is TWinControl then begin
        FocusedWinControl:=TWinControl(LCLObject);
        if FocusedWidget<>Widget then begin
          {$IFDEF VerboseKeyboard}
          DebugLn('[HandleGTKKeyUpDown] REDIRECTING ',
            ' FocusedWidget=',GetWidgetClassName(FocusedWidget),
            ' Control=',FocusedWinControl.Name,':',FocusedWinControl.ClassName);
          {$ENDIF}
          // redirect key to lcl control
          TargetWidget:=FocusedWidget;
          TargetData:=FocusedWinControl;
        end;
      end;
    end;
  end;

  // remember this event
  RememberKeyEventWasHandledByLCL(Event,BeforeEvent);

  if TargetWidget=nil then exit;

  gdk_event_key_get_string(Event, EventString);
  FillChar(Msg,SizeOf(Msg),0);

  VKey := KeySymToVKey(Event^.keyval);

  Flags := 0;
  if (VKey.Flags and VKEY_FLAG_EXT) <> 0
  then Flags := KF_EXTENDED;

  SysKey := False;
  if (VKey.Flags and VKEY_FLAG_ALT) = 0
  then begin
    // VKey is without ALT so Alt is syskey
    SysKey := (Event^.State and GDK_MOD1_MASK) <> 0;
  end
  else begin
    // VKey is with ALT so SHIFT Alt is syskey
    SysKey := (Event^.State and (GDK_MOD1_MASK or GDK_SHIFT_MASK))
               = (GDK_MOD1_MASK or GDK_SHIFT_MASK);
  end;

  if SysKey
  then Flags := Flags or KF_ALTDOWN;
  CommonKeyData := MVKeyInfo[VKey.VKey].KeyCode shl 16; // ScanCode

  case gdk_event_get_type(Event) of
  GDK_KEY_RELEASE:
    begin
      {$IFDEF VerboseKeyboard}
      DebugLn('[HandleGTKKeyUpDown] GDK_KEY_RELEASE VKey=',dbgs(VKey.VKey));
      {$ENDIF}

      Msg.CharCode := VKey.VKey;
      if BeforeEvent then begin
        if SysKey then
          Msg.msg := CN_SYSKEYUP
        else
          Msg.msg := CN_KEYUP;
      end else begin
        if SysKey then
          Msg.msg := LM_SYSKEYUP
        else
          Msg.msg := LM_KEYUP;
      end;

      Flags := Flags or KF_UP or KF_REPEAT;

      Msg.KeyData := CommonKeyData or (Flags shl 16) or $0001 {always};

      // send the message directly to the LCL
      Msg.Result:=0;
      NotifyApplicationUserInput(Msg.Msg);
      Result := DeliverMessage(TargetData, Msg) = 0;

      if Msg.CharCode <> VKey.VKey
      then begin
        // key was handled by LCL
        StopKeyEvent('key_release_event');
      end;
    end;

  GDK_KEY_PRESS:
    begin
      {$IFDEF VerboseKeyboard}
      DebugLn('[HandleGTKKeyUpDown] GDK_KEY_PRESS VKey=',dbgs(VKey.VKey));
      {$ENDIF}

      Msg.CharCode := VKey.VKey;

      if BeforeEvent then begin
        if SysKey then
          Msg.msg := CN_SYSKEYDOWN
        else
          Msg.msg := CN_KEYDOWN;
      end else begin
        if SysKey then
          Msg.msg := LM_SYSKEYDOWN
        else
          Msg.msg := LM_KEYDOWN;
      end;

      // todo  repeat
      // Flags := Flags or KF_REPEAT;

      Msg.KeyData := CommonKeyData or (Flags shl 16) or $0001 {TODO:  repeatcount};

      // send the message directly to the LCL
      NotifyApplicationUserInput(Msg.Msg);
      Result := DeliverMessage(TargetData, Msg) = 0;

      if Msg.CharCode <> Vkey.Vkey
      then begin
        // key was changed by LCL
        StopKeyEvent('key_press_event');
      end;

      if (not EventStopped) and CanSendChar
      then begin
        EventTrace('char', data);

        KeyPressesChar:=#0;
        if Event^.Length = 1 then begin
          // ASCII key was pressed
          KeyPressesChar := EventString^;
        end else if Event^.KeyVal<128 then begin
          // non ASCII key was pressed
          //{$IFDEF GTK2}
          //Msg.CharCode := gdk_keyval_to_unicode(Event^.KeyVal);
          //{$ELSE}
          KeyPressesChar := chr(byte(Event^.KeyVal));
          //{$ENDIF}
        end;
        //debugln('GDK_KEY_PRESS ',dbgs(ord(KeyPressesChar)),' BeforeEvent=',dbgs(BeforeEvent),' ',DbgSName(TObject(TargetData)),' SysKey=',dbgs(SysKey));

        if KeyPressesChar<>#0 then begin
          // ASCII key: send a normal KeyPress Event for Delphi compatibility
          FillChar(Msg,SizeOf(Msg),0);

          Msg.KeyData := CommonKeyData;

          if BeforeEvent then begin
            if SysKey then
              Msg.msg := CN_SYSCHAR
            else
              Msg.msg := CN_CHAR
          end else begin
            if SysKey then
              Msg.msg := LM_SYSCHAR
            else
              Msg.msg := LM_CHAR;
          end;

          Msg.Result:=0;
          Msg.CharCode:=ord(KeyPressesChar);
          // send the message directly (not queued) to the LCL
          //debugln('GDK_KEY_PRESS ',DbgSName(TObject(TargetData)),' ',dbgs(Msg.msg));
          Result := DeliverMessage(TargetData, Msg) = 0;

          if (ord(KeyPressesChar)<>Msg.CharCode)
          then begin
            // key was changed by lcl
            //DebugLn('HandleGTKKeyUpDown A ',Msg.CharCode,' BeforeEvent=',BeforeEvent);
            if (Msg.CharCode=0) or (Msg.CharCode>=128) then
              // key set to invalid => just stop
              StopKeyEvent('key_press_event')
            else begin
              // try to change the key
              EventString^:=chr(Msg.CharCode);
              EventString[1]:=#0;
              Event^.KeyVal:=Msg.CharCode;
              gdk_event_key_set_string(Event,EventString);
            end;
          end;
        end;
        
      end;

      if (not EventStopped) and (not BeforeEvent) then begin
        // send the UTF8 keypress
        // try to get the UTF8 representation of the key
        {$IFDEF GTK1}
        Character:='';
        if (Event^.length>0) and (Event^.length<7) then begin
          SetLength(Character,Event^.length);
          System.Move(Event^.thestring^,Character[1],length(Character));
        end;
        {$ELSE GTK2}
        Character := UnicodeToUTF8(gdk_keyval_to_unicode(Event^.KeyVal));
        {$ENDIF GTK2}
        {$IFDEF VerboseKeyboard}
        debugln('[HandleGTKKeyUpDown] GDK_KEY_PRESS UTF8="',DbgStr(Character),'"');
        {$ENDIF}
        if Character<>'' then begin
          LCLObject:=GetNearestLCLObject(TargetWidget);
          if LCLObject is TWinControl then begin
            Result:=TWinControl(LCLObject).IntfUTF8KeyPress(Character,1);
            if Result or (Character='') then
              StopKeyEvent('key_press_event');
          end;
        end;
      end;
    end;
  end;
  
  {$IFDEF Gtk1}
  Result:=true;
  {$ELSE}
  Result:=EventStopped;
  {$ENDIF}
  //DebugLn('[HandleGTKKeyUpDown] ',DbgSName(TObject(Data)),' Result=',dbgs(Result));
end;

{------------------------------------------------------------------------------
  Procedure: InitKeyboardTables
  Params:    none
  Returns:   none

  Initializes the CharToVK and CKeyToVK tables
 ------------------------------------------------------------------------------}
{$IFDEF UNIX}
{$DEFINE InitKeyboardTables}
procedure InitKeyboardTables;

  procedure FindVKeyInfo(const AKeySym: TKeySym; var AVKey: Byte;
    var AExtended, AHasMultiVK: Boolean);
  var
    ByteKey: Byte;
  begin
    AExtended := False;
    AHasMultiVK := False;
    AVKey := $FF;

    case AKeySym of
      32..255: begin
        ByteKey:=Byte(AKeySym);
        case Chr(ByteKey) of // Normal ASCII chars
          // only unshifted is needed for first match
          // 'A'..'Z',
          '0'..'9',
          ' ':      AVKey := ByteKey;
          'a'..'z': AVKey := ByteKey - Ord('a') + Ord('A');
          '+': AVKey := VK_OEM_PLUS;
          ',': AVKey := VK_OEM_COMMA;
          '-': AVKey := VK_OEM_MINUS;
          '.': AVKey := VK_OEM_PERIOD;

          // try the US keycodes first
          ';': AVKey := VK_OEM_1;
          '/': AVKey := VK_OEM_2;
          '`': AVKey := VK_OEM_3;
          '[': AVKey := VK_OEM_4;
          '\': AVKey := VK_OEM_5;
          ']': AVKey := VK_OEM_6;
          '''': AVKey := VK_OEM_7;
        end;
      end;

      GDK_KEY_Tab,
      GDK_KEY_ISO_Left_Tab,
      GDK_KEY_KP_Tab:       AVKey := VK_TAB;
      GDK_KEY_RETURN:       AVKey := VK_RETURN;
//    GDK_KEY_LINEFEED;     AVKey := $0A;

      // Cursor block / keypad
      GDK_KEY_INSERT:
      begin
        AExtended := True;
        AVKey := VK_INSERT;
      end;
      GDK_KEY_HOME:
      begin
        AExtended := True;
        AVKey := VK_HOME;
      end;
      GDK_KEY_LEFT:
      begin
        AExtended := True;
        AVKey := VK_LEFT;
      end;
      GDK_KEY_UP:
      begin
        AExtended := True;
        AVKey := VK_UP;
      end;
      GDK_KEY_RIGHT:
      begin
        AExtended := True;
        AVKey := VK_RIGHT;
      end;
      GDK_KEY_DOWN:
      begin
        AExtended := True;
        AVKey := VK_DOWN;
      end;
      GDK_KEY_PAGE_UP:
      begin
        AExtended := True;
        AVKey := VK_PRIOR;
      end;
      GDK_KEY_PAGE_DOWN:
      begin
        AExtended := True;
        AVKey := VK_NEXT;
      end;
      GDK_KEY_END:
      begin
        AExtended := True;
        AVKey := VK_END;
      end;
      
      // Keypad
      GDK_KEY_KP_ENTER:
      begin
        AExtended := True;
        AVKey := VK_Return;
      end;
      GDK_KEY_KP_Space, GDK_KEY_KP_Begin:
      begin
        AVKey := VK_CLEAR;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_INSERT:
      begin
        // Keypad key is not extended
        AVKey := VK_INSERT;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_HOME:
      begin
        // Keypad key is not extended
        AVKey := VK_HOME;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_LEFT:
      begin
        // Keypad key is not extended
        AVKey := VK_LEFT;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_UP:
      begin
        // Keypad key is not extended
        AVKey := VK_UP;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_RIGHT:
      begin
        // Keypad key is not extended
        AVKey := VK_RIGHT;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_DOWN:
      begin
        // Keypad key is not extended
        AVKey := VK_DOWN;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_PAGE_UP:
      begin
        // Keypad key is not extended
        AVKey := VK_PRIOR;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_PAGE_DOWN:
      begin
        // Keypad key is not extended
        AVKey := VK_NEXT;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_END:
      begin
        // Keypad key is not extended
        AVKey := VK_END;
        AHasMultiVK := True;
      end;
      GDK_KEY_Num_Lock:
      begin
        AExtended := True;
        AVKey := VK_NUMLOCK;
      end;
      GDK_KEY_KP_F1..GDK_KEY_KP_F4:
      begin
        // I guess it is extended to differentiate between normal Fn
        AExtended := True;
        AVKey := VK_F1 + AKeySym - GDK_KEY_KP_F1;
      end;
      GDK_KEY_KP_Multiply:
      begin
        // Keypad key is not extended
        AVKey := VK_MULTIPLY;
      end;
      GDK_KEY_KP_Add:
      begin
        // Keypad key is not extended
        AVKey := VK_ADD;
      end;
      GDK_KEY_KP_Separator:
      begin
        // Keypad key is not extended
        AVKey := VK_SEPARATOR;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_Subtract:
      begin
        // Keypad key is not extended
        AVKey := VK_SUBTRACT;
      end;
      GDK_KEY_KP_Decimal:
      begin
        // Keypad key is not extended
        AVKey := VK_DECIMAL;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_Delete:
      begin
        // Keypad key is not extended
        AVKey := VK_DELETE;
        AHasMultiVK := True;
      end;
      GDK_KEY_KP_Divide:
      begin
        AExtended := True;
        AVKey := VK_DIVIDE;
      end;
      GDK_KEY_KP_0..GDK_KEY_KP_9:
      begin
        // Keypad key is not extended, it is identified by VK
        AVKey := VK_NUMPAD0 + AKeySym - GDK_KEY_KP_0;
        AHasMultiVK := True;
      end;
      
      GDK_KEY_BackSpace:    AVKey := VK_BACK;
      GDK_KEY_Clear:        AVKey := VK_CLEAR;
      GDK_KEY_Pause:        AVKey := VK_PAUSE;
      GDK_KEY_Scroll_Lock:  AVKey := VK_SCROLL;
      GDK_KEY_Sys_Req:      AVKey := VK_SNAPSHOT;
      GDK_KEY_Escape:       AVKey := VK_ESCAPE;
      GDK_KEY_Delete:       AVKey := VK_DELETE;

      GDK_KEY_Kanji:        AVKey := VK_KANJI;

      GDK_Key_Select:       AVKey := VK_SELECT;
      GDK_Key_Print:        AVKey := VK_PRINT;
      GDK_Key_Execute:      AVKey := VK_EXECUTE;
      GDK_Key_Cancel:       AVKey := VK_CANCEL;
      GDK_Key_Help:         AVKey := VK_HELP;
      GDK_Key_Break:        AVKey := VK_CANCEL;
      GDK_Key_Mode_switch:  AVKey := VK_MODECHANGE;
      GDK_Key_Caps_Lock:    AVKey := VK_CAPITAL;
      GDK_Key_Shift_L:      AVKey := VK_SHIFT;
      GDK_Key_Shift_R:      AVKey := VK_SHIFT;
      GDK_Key_Control_L:    AVKey := VK_CONTROL;
      GDK_Key_Control_R:    AVKey := VK_CONTROL;
//      GDK_Key_Meta_L:       AVKey := VK_MENU;  //shifted alt, so it is found by alt
//      GDK_Key_Meta_R:       AVKey := VK_MENU;
      GDK_Key_Alt_L:        AVKey := VK_MENU;
      GDK_Key_Alt_R:        AVKey := VK_MENU;
      GDK_Key_Super_L:      AVKey := VK_LWIN;
      GDK_Key_Super_R:      AVKey := VK_RWIN;
      GDK_Key_Menu:         AVKey := VK_APPS;

      // Function keys
      GDK_KEY_F1..GDK_KEY_F24:  AVKey := VK_F1 + AKeySym - GDK_Key_F1;
      
      // Extra keys on a "internet" keyboard
      GDKX_KEY_Sleep:
      begin
        AExtended := True;
        AVKey := VK_SLEEP;
      end;
      GDKX_KEY_AudioLowerVolume:
      begin
        AExtended := True;
        AVKey := VK_VOLUME_DOWN;
      end;
      GDKX_KEY_AudioMute:
      begin
        AExtended := True;
        AVKey := VK_VOLUME_MUTE;
      end;
      GDKX_KEY_AudioRaiseVolume:
      begin
        AExtended := True;
        AVKey := VK_VOLUME_UP;
      end;
      GDKX_KEY_AudioPlay:
      begin
        AExtended := True;
        AVKey := VK_MEDIA_PLAY_PAUSE;
      end;
      GDKX_KEY_AudioStop:
      begin
        AExtended := True;
        AVKey := VK_MEDIA_STOP;
      end;
      GDKX_KEY_AudioPrev:
      begin
        AExtended := True;
        AVKey := VK_MEDIA_PREV_TRACK;
      end;
      GDKX_KEY_AudioNext:
      begin
        AExtended := True;
        AVKey := VK_MEDIA_NEXT_TRACK;
      end;
      GDKX_KEY_Mail:
      begin
        AExtended := True;
        AVKey := VK_LAUNCH_MAIL;
      end;
      GDKX_KEY_HomePage:
      begin
        AExtended := True;
        AVKey := VK_BROWSER_HOME;
      end;
      GDKX_KEY_Back:
      begin
        AExtended := True;
        AVKey := VK_BROWSER_BACK;
      end;
      GDKX_KEY_Forward:
      begin
        AExtended := True;
        AVKey := VK_BROWSER_FORWARD;
      end;
      GDKX_KEY_Stop:
      begin
        AExtended := True;
        AVKey := VK_BROWSER_STOP;
      end;
      GDKX_KEY_Refresh:
      begin
        AExtended := True;
        AVKey := VK_BROWSER_REFRESH;
      end;
      GDKX_KEY_WWW:
      begin
        AExtended := True;
        AVKey := VK_BROWSER_HOME;
      end;
      GDKX_KEY_Favorites:
      begin
        AExtended := True;
        AVKey := VK_BROWSER_FAVORITES;
      end;
      GDKX_KEY_AudioMedia:
      begin
        AExtended := True;
        AVKey := VK_LAUNCH_MEDIA_SELECT;
      end;
      GDKX_KEY_MyComputer:
      begin
        AExtended := True;
        AVKey := VK_LAUNCH_APP1;
      end;
      GDKX_KEY_Calculator:
      begin
        AExtended := True;
        AVKey := VK_LAUNCH_APP2;
      end;                    
      
      // For faster cases, group by families
      $400..$4FF: begin
        // Katakana
      end;

      $500..$5FF: begin
        // Arabic
        case AKeySym of
          GDK_KEY_arabic_hamza:                  AVKey := VK_X;
          GDK_KEY_arabic_hamzaonwaw:             AVKey := VK_C;
          GDK_KEY_arabic_hamzaonyeh:             AVKey := VK_Z;
          GDK_KEY_arabic_alef:                   AVKey := VK_H;
          GDK_KEY_arabic_beh:                    AVKey := VK_F;
          GDK_KEY_arabic_tehmarbuta:             AVKey := VK_M;
          GDK_KEY_arabic_teh:                    AVKey := VK_J;
          GDK_KEY_arabic_theh:                   AVKey := VK_E;
          GDK_KEY_arabic_jeem:                   AVKey := VK_OEM_4;
          GDK_KEY_arabic_hah:                    AVKey := VK_P;
          GDK_KEY_arabic_khah:                   AVKey := VK_O;
          GDK_KEY_arabic_dal:                    AVKey := VK_OEM_6;
          GDK_KEY_arabic_thal:                   AVKey := VK_OEM_3;
          GDK_KEY_arabic_ra:                     AVKey := VK_V;
          GDK_KEY_arabic_zain:                   AVKey := VK_OEM_PERIOD;
          GDK_KEY_arabic_seen:                   AVKey := VK_S;
          GDK_KEY_arabic_sheen:                  AVKey := VK_A;
          GDK_KEY_arabic_sad:                    AVKey := VK_W;
          GDK_KEY_arabic_dad:                    AVKey := VK_Q;
          GDK_KEY_arabic_tah:                    AVKey := VK_OEM_7;
          GDK_KEY_arabic_zah:                    AVKey := VK_OEM_2;
          GDK_KEY_arabic_ain:                    AVKey := VK_U;
          GDK_KEY_arabic_ghain:                  AVKey := VK_Y;
          GDK_KEY_arabic_feh:                    AVKey := VK_T;
          GDK_KEY_arabic_qaf:                    AVKey := VK_R;
          GDK_KEY_arabic_kaf:                    AVKey := VK_OEM_1;
          GDK_KEY_arabic_lam:                    AVKey := VK_G;
          GDK_KEY_arabic_meem:                   AVKey := VK_L;
          GDK_KEY_arabic_noon:                   AVKey := VK_K;
          GDK_KEY_arabic_heh:                    AVKey := VK_I;
          GDK_KEY_arabic_waw:                    AVKey := VK_OEM_COMMA;
          GDK_KEY_arabic_alefmaksura:            AVKey := VK_N;
          GDK_KEY_arabic_yeh:                    AVKey := VK_D;
        end;
      end;

      $600..$6FF: begin
        // Cyrillic

        // MWE:
        // These VK codes are not compatible with all cyrillic KBlayouts
        // Example:
        // VK_A on a russian layout generates a cyrillic_EF
        // VK_A on a serbian layout generates a cyrillic_A
        //
        // Mapping cyrillic_A to VK_A is easier so that encoding is used.
        // Maybe in future we can take the KBLayout into account
        case AKeySym of
          GDK_KEY_cyrillic_a..GDK_KEY_cyrillic_ze:
          begin
            AVKey := VK_A + AKeySym - GDK_KEY_cyrillic_a;
          end;
          // Capital is not needed, the lower will match
          //GDK_KEY_cyrillic_A..GDK_KEY_cyrillic_ZE:
          //begin
          //  AVKey := VK_A + AKeySym - GDK_KEY_cyrillic_A;
          //end;
        end;
      end;

      $700..$7FF: begin
        // Greek
        case AKeySym of
          // Capital is not needed, the lower will match
          GDK_KEY_greek_alpha:           AVKey := VK_A;
          GDK_KEY_greek_beta:            AVKey := VK_B;
          GDK_KEY_greek_gamma:           AVKey := VK_G;
          GDK_KEY_greek_delta:           AVKey := VK_D;
          GDK_KEY_greek_epsilon:         AVKey := VK_E;
          GDK_KEY_greek_zeta:            AVKey := VK_Z;
          GDK_KEY_greek_eta:             AVKey := VK_H;
          GDK_KEY_greek_theta:           AVKey := VK_U;
          GDK_KEY_greek_iota:            AVKey := VK_I;
          GDK_KEY_greek_kappa:           AVKey := VK_K;
          GDK_KEY_greek_lamda:           AVKey := VK_L;
          GDK_KEY_greek_mu:              AVKey := VK_M;
          GDK_KEY_greek_nu:              AVKey := VK_N;
          GDK_KEY_greek_xi:              AVKey := VK_J;
          GDK_KEY_greek_omicron:         AVKey := VK_O;
          GDK_KEY_greek_pi:              AVKey := VK_P;
          GDK_KEY_greek_rho:             AVKey := VK_R;
          GDK_KEY_greek_sigma:           AVKey := VK_S;
          GDK_KEY_greek_finalsmallsigma: AVKey := VK_W;
          GDK_KEY_greek_tau:             AVKey := VK_T;
          GDK_KEY_greek_upsilon:         AVKey := VK_Y;
          GDK_KEY_greek_phi:             AVKey := VK_F;
          GDK_KEY_greek_chi:             AVKey := VK_X;
          GDK_KEY_greek_psi:             AVKey := VK_C;
          GDK_KEY_greek_omega:           AVKey := VK_V;
        end;
      end;
      
      $C00..$CFF: begin
        // Hebrew
        // Shifted keys will produce A..Z so the VK codes will be assigned there
      end;
      
      $D00..$DFF: begin
        // Thai
        // To many differences to assign VK codes through lookup 
        // Thai Kedmanee and Thai Pattachote are complete different layouts
      end;
      
      $E00..$EFF: begin
        // Korean
      end;
    end;
  end;

  procedure NextFreeVK(var AFreeVK: Byte);
  begin
    case AFreeVK of
      $96: AFreeVK := $E1;
      $E1: AFreeVK := $E3;
      $E4: AFreeVK := $E6;
      $E6: AFreeVK := $E9;
      $F5: begin
        DebugLn('[WARNING] Out of OEM specific VK codes, changing to unassigned');
        AFreeVK := $88;
      end;
      $8F: AFreeVK := $97;
      $9F: AFreeVK := $D8;
      $DA: AFreeVK := $E5;
      $E5: AFreeVK := $E8;
      $E8: begin
        DebugLn('[WARNING] Out of unassigned VK codes, assigning $FF');
        AFreeVK := $FF;
      end;
      $FF: AFreeVK := $FF; // stay there
    else
      Inc(AFreeVK);
    end;
  end;
  
const
  KEYFLAGS: array[0..3] of Byte = (
    $00,
    VKEY_FLAG_SHIFT,
    VKEY_FLAG_ALT,
    VKEY_FLAG_SHIFT or VKEY_FLAG_ALT
  );
  EXTFLAG: array[Boolean] of Byte = (
    $00,
    VKEY_FLAG_EXT
  );
  MULTIFLAG: array[Boolean] of Byte = (
    $00,
    VKEY_FLAG_MULTI_VK
  );
  XEVENTSTATE: array[0..3] of DWord = (
    0,
    GDK_SHIFT_MASK,
    GDK_MOD1_MASK,
    GDK_MOD1_MASK or GDK_SHIFT_MASK
  );

var
  Display: Pointer;
  ByteKey: Byte;
  n, m: Integer;
  LoKey, HiKey: Integer;
  KeySym: array[0..3] of TKeySym;
  VKey, FreeVK, Flags, idx: Byte;
  HasMultiVK, DoMultiVK, Extended, HasKey, ComputeVK, WarningShown: Boolean;
  K: PVKeyArray1;
  KeySymChars: array[0..16] of Char;
  KeySymCharLen: Integer;
  XKeyEvent: TXKeyEvent;
begin
  Display := X11Display;
  if Display = nil then Exit;
  
  // Init dummy XEvent to retrieve the char corresponding to a key
  FillChar(XKeyEvent, SizeOf(XKeyEvent), 0);
  XKeyEvent._Type := GDK_KEY_PRESS;
  XKeyEvent.Display := Display;
  XKeyEvent.Same_Screen := True;

  // Retrieve the KeyCode bounds
  XDisplayKeyCodes(Display, @LoKey, @HiKey);

  Assert((LoKey >= 0) and (HiKey <= 255)); // perdef
  FreeVK := $92; // first OEM specific VK
  WarningShown := False;
  for n := LoKey to HiKey do
  begin
    VKey := $FF;
    HasKey := False;
    for m := 0 to 3 do
    begin
      ByteKey:=Byte(n);
      KeySym[m] := XKeyCodeToKeysym(Display, ByteKey, m);
      if (VKey = $FF) and (KeySym[m] <> 0)
      then begin
        HasKey := True;
        FindVKeyInfo(KeySym[m], VKey, Extended, HasMultiVK);
      end;
    end;
    // Continue if there is no keysym found
    if not HasKey then Continue;
    
    ComputeVK := VKey = $FF;
    if ComputeVK
    then begin
      VKey := FreeVK;
      NextFreeVK(FreeVK);
    end;
    
    // The keypadkeys have 2 VK_keycodes :(
    // In that case we have to FIndKeyInfo for every keysym
    DoMultiVK := HasMultiVK;
    
    for m := 0 to 3 do
    begin
      if KeySym[m] = 0 then Continue;
      if (m > 1) and (KeySym[m] = KeySym[m - 2]) then Continue;

      if DoMultiVK
      then FindVKeyInfo(KeySym[m], VKey, Extended, HasMultiVK);

      if VKey = $FF
      then Flags := $FF
      else begin
        Flags := KEYFLAGS[m] or EXTFLAG[Extended] or MULTIFLAG[DoMultiVK];
        MVKeyInfo[VKey].KeySym[m] := KeySym[m];
      end;

      K := KeySymToVKeyArray(KeySym[m], True);
      idx := KeySym[m] and $FF;

      // some X servers define separate keycodes for "dead-key" chars.
      // So we might have already a VK assigned
      if K^[idx].VKey <> 0
      then begin
        // VK assigned
        // if the current VK is computed then return it to the pool
        // else use the new VK. If the assigned VK was computed, bad luck.
        // Some day we might this do smarter, but since those extra keys
        // are at the end, we probably wont hit the situation.
        if ComputeVK
        then begin
          FreeVK := VKey;
          VKey := K^[idx].VKey;
          Flags := K^[idx].Flags;
        end;
      end;
      K^[idx].VKey := VKey;
      K^[idx].Flags := Flags;

      // Retrieve the chars for this KeySym
      XKeyEvent.KeyCode := n;
      XKeyEvent.State := XEVENTSTATE[m];
      KeySymCharLen := XLookupString(@XKeyEvent, KeySymChars, SizeOf(KeySymChars) - 1, nil, nil);
      if (KeySymCharLen > 0) and (KeySymChars[KeySymCharLen - 1] = #0)
      then Dec(KeySymCharLen)
      else KeySymChars[KeySymCharLen] := #0;
      if (KeySymCharLen <= 0) then Continue;

      // Warn if the KeySymChar is longer than 1.
      if KeySymCharLen > 1
      then begin
        if not WarningShown
        then begin
          WarningShown := True;
          DebugLn('[WARNING] *******************************************************');
          DebugLn('[WARNING] **                                                   **');
          DebugLn('[WARNING] ** Multibyte character encodings (like UTF8) are not **');
          DebugLn('[WARNING] ** supported at the moment.                          **');
          DebugLn('[WARNING] ** For full keyboard event support, make sure that   **');
          DebugLn('[WARNING] ** the LANG environment var has no UTF8              **');
          DebugLn('[WARNING] **                                                   **');
          DebugLn('[WARNING] *******************************************************');
        end;
        Continue;
      end;

      // If we are here length(KeySymChars) = 1

      MCharToVK[KeySymChars[0]].VKey := VKey;
      MCharToVK[KeySymChars[0]].Flags := Flags;

      if VKey <> $FF
      then MVKeyInfo[VKey].KeyChar[m] := KeySymChars[0];
    end;
    MKeyCodeToVK[n] := VKey;
    if VKey <> $FF
    then MVKeyInfo[VKey].KeyCode := Byte(n);
  end;
end;
{$ENDIF}

{$IFDEF WIN32}
{$DEFINE InitKeyboardTables}
procedure InitKeyboardTables;
var
  n: Integer;
  VKey: Byte;
begin
  for n := 0 to 255 do
  begin
    MCharToVK[Chr(n)] := TVKeyRecord(Windows.VkKeyScan(Chr(n)));
    VKey := MapVirtualKey(n, 3);
    MKeyCodeToVK[n] := VKey;
    if VKey <> $FF
      then MVKeyInfo[VKey].KeyCode := Byte(n);
    //WARNING: the other fields of MVKeyInfo are not set.
    //At this moment it is not clear, what they should be.
    //This is code is to be removed when the keyboard handling is updated for
    //gtk2, (gtk1 won't be supported on windows), so it will be obsolete soon
  end
end;
{$ENDIF}

{$IFNDEF InitKeyboardTables}
procedure InitKeyboardTables;
begin
  DebugLn('[WARNING] Keyboardtables not initialized (platform not supported)');
end;
{$ENDIF}

{------------------------------------------------------------------------------
  Procedure: DoneKeyboardTables
  Params:    none
  Returns:   none

  Frees the dynamic keyboard tables
 ------------------------------------------------------------------------------}
procedure DoneKeyboardTables;
var
  n1, n2, n3: Byte;
  K1: PVKeyArray1;
  K2: PVKeyArray2;
  K3: PVKeyArray3;
  i: Integer;
begin
  for n3 := 0 to 255 do
  begin
    K3 := MKeySymToVK[n3];
    if K3 = nil then Continue;
    for n2 := 0 to 255 do
    begin
      K2 := K3^[n2];
      if K2 = nil then Continue;
      for n1 := 0 to 255 do
      begin
        K1 := K2^[n1];
        if K1 = nil then Continue;
        Dispose(K1);
      end;
      Dispose(K2);
    end;
    Dispose(K3);
  end;
  
  if LCLHandledKeyEvents<>nil then begin
    for i:=0 to LCLHandledKeyEvents.Count-1 do
      TObject(LCLHandledKeyEvents[i]).Free;
    LCLHandledKeyEvents.Free;
    LCLHandledKeyEvents:=nil;
    for i:=0 to LCLHandledKeyAfterEvents.Count-1 do
      TObject(LCLHandledKeyAfterEvents[i]).Free;
    LCLHandledKeyAfterEvents.Free;
    LCLHandledKeyAfterEvents:=nil;
  end;
end;

{------------------------------------------------------------------------------
  Function:  CharToVKandFlags
  Params:    AChar: A character to translate
  Returns:   LoByte: The VK code
             HiByte: The A|C|S conbination the get this key

 ------------------------------------------------------------------------------}
function CharToVKandFlags(const AChar: Char): Word;
begin
  Result := MCharToVK[AChar].VKey
         or (MCharToVK[AChar].Flags and VKEY_FLAG_KEY_MASK) shl 8;
end;

{------------------------------------------------------------------------------
  Function:  GetVKeyInfo
  Params:    AVKey: A virtual key to get the info for
  Returns:   A Info record

  This function is more a safety to make sure MVkeyInfo isn't accessed out of
  it's bounds
 ------------------------------------------------------------------------------}
function GetVKeyInfo(const AVKey: Byte): TVKeyInfo;
begin
  Result := MVKeyInfo[AVKey];
end;

{------------------------------------------------------------------------------
  Function:  IsToggleKey
  Params:    AVKey: A Virtual key
  Returns:   True if the requeste dkey is a toggle key

 ------------------------------------------------------------------------------}
function IsToggleKey(const AVKey: Byte): Boolean;
begin
  Result := AVKey in [VK_CAPITAL, VK_SCROLL, VK_NUMLOCK];
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
  FileSelWidget: PGtkFileSelection;
  LCLFilterMenu, LCLHistoryMenu: PGTKWidget;
begin
  if (ADialog=nil) or (not ADialog.HandleAllocated) then exit;
  DlgWindow:=PGtkWidget(ADialog.Handle);
  {$IFDEF VerboseTransient}
  DebugLn('DestroyCommonDialogAddOns ',ADialog.Name,':',ADialog.ClassName);
  {$ENDIF}
  gtk_window_set_transient_for(PGtkWindow(DlgWindow),nil);
  if ADialog is TOpenDialog then begin

    FileSelWidget:=GTK_FILE_SELECTION(DlgWindow);
    FreeWidgetInfo(FileSelWidget^.selection_entry);
    FreeWidgetInfo(FileSelWidget^.dir_list);
    FreeWidgetInfo(FileSelWidget^.file_list);
    LCLFilterMenu:=PGTKWidget(gtk_object_get_data(PGtkObject(FileSelWidget),
                                       'LCLFilterMenu'));
    if LCLFilterMenu<>nil then FreeWidgetInfo(LCLFilterMenu);
    LCLHistoryMenu:=PGTKWidget(gtk_object_get_data(PGtkObject(FileSelWidget),
                                       'LCLHistoryMenu'));
    if LCLHistoryMenu<>nil then FreeWidgetInfo(LCLHistoryMenu);

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

    // free preview handle
    if ADialog is TPreviewFileDialog then begin
      if TPreviewFileDialog(ADialog).PreviewFileControl<>nil then
        TPreviewFileDialog(ADialog).PreviewFileControl.Handle:=0;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Procedure: PopulateFileAndDirectoryLists
  Params:    FileSelection: PGtkFileSelection;
              Mask: string (File mask, such as *.txt)
  Returns:   none

  Populate the directory and file lists according to the given mask
 ------------------------------------------------------------------------------}
procedure PopulateFileAndDirectoryLists(FileSelection: PGtkFileSelection;
  const Mask: string);
var
  Dirs, Files: PGtkCList;
  Text: array [0..1] of Pgchar;
  Info: TSearchRec;
  DirName: PChar;
  Dir: string;
  StrList: TStringList;
  
  procedure Add(List: PGtkCList; const s: string);
  begin
    Text[0] := PChar(s);
    gtk_clist_append(List, Text);
  end;
  
  procedure AddList(List: PGtkCList);
  var
    i: integer;
  begin
    for i:=0 to StrList.Count-1 do
      Add(List, StrList[i]);
  end;
  
begin
  StrList := TStringList.Create;
  dirs := PGtkCList(FileSelection^.dir_list);
  files := PGtkCList(FileSelection^.file_list);
  DirName := gtk_file_selection_get_filename(FileSelection);
  if DirName <> nil then begin
    SetString(Dir, DirName, strlen(DirName));
    SetLength(Dir, LastDelimiter(PathDelim,Dir));
  end else
    Dir := '';
  Text[1] := nil;
  gtk_clist_freeze(Dirs);
  gtk_clist_clear(Dirs);
  gtk_clist_freeze(Files);
  gtk_clist_clear(Files);
  { Add all directories }
  Strlist.Add('..'+PathDelim);
  if FindFirst(AppendPathDelim(Dir)+GetAllFilesMask, faAnyFile and faDirectory,
    Info) = 0
  then
    repeat
      if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.')
      and (Info.Name <> '..') then
        StrList.Add(AppendPathDelim(Info.Name));
    until FindNext(Info) <> 0;
  FindClose(Info);
  StrList.Sorted := True;
  AddList(Dirs);
  // add required files
  StrList.Clear;
  Dir := AppendPathDelim(Dir)+Mask;
  if Mask = '' then Dir := Dir + GetAllFilesMask;
  if FindFirst(Dir, faAnyFile, Info) = 0 then
    repeat
      if (Info.Attr and faDirectory) <> faDirectory then
        Strlist.Add(Info.Name);
    until FindNext(Info) <> 0;
  FindClose(Info);
  AddList(Files);
  StrList.Free;
  gtk_clist_thaw(Dirs);
  gtk_clist_thaw(Files);
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
  if Target=nil then DebugLn('[DeliverMessage] Target = nil');
  {$IFDEF VerboseDeliverMessage}
  DebugLn('DeliverMessage ',DbgS(Target),
    ' ',TComponent(Target).Name,':',TObject(Target).ClassName,
    ' Message=',GetMessageName(TLMessage(AMessage).Msg));
  {$ENDIF}
  if (TLMessage(AMessage).Msg=LM_PAINT)
  or (TLMessage(AMessage).Msg=LM_INTERNALPAINT)
  or (TLMessage(AMessage).Msg=LM_GtkPaint) then
    CurrentSentPaintMessageTarget:=TObject(Target);
  try
    if TObject(Target) is TControl
    then TControl(Target).WindowProc(TLMessage(AMessage))
    else TObject(Target).Dispatch(TLMessage(AMessage));
  except
    Application.HandleException(nil);
  end;

  Result := TLMessage(AMessage).Result;
  CurrentSentPaintMessageTarget:=nil;
end;

{------------------------------------------------------------------------------
  Function: ObjectToGTKObject
  Params: AnObject: A LCL Object
  Returns:  The GTKObject of the given object

  Returns the GTKObject of the given object, nil if no object available
 ------------------------------------------------------------------------------}
function ObjectToGTKObject(const AnObject: TObject): PGtkObject;
var
  handle : HWND;
begin
  Handle := 0;
  if not assigned(AnObject) then
  begin
    assert (false, 'TRACE:  [ObjectToGtkObject] Object not assigned');
  end
  else if (AnObject is TWinControl) then
  begin
    if TWinControl(AnObject).HandleAllocated then
      handle := TWinControl(AnObject).Handle;
  end
  else if (AnObject is TMenuItem) then
  begin 
    if TMenuItem(AnObject).HandleAllocated then
      handle := TMenuItem(AnObject).Handle;
  end
  else if (AnObject is TMenu) then
  begin 
    if TMenu(AnObject).HandleAllocated then
      handle := TMenu(AnObject).Items.Handle;
  end
  else if (AnObject is TCommonDialog) then
  begin
    {if TCommonDialog(AObject).HandleAllocated then }
    handle := TCommonDialog(AnObject).Handle;
  end
  else begin
    Assert(False, Format('Trace:  [ObjectToGtkObject] Message received with unhandled class-type <%s>', [AnObject.ClassName]));
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
  if Widget = nil
  then raise EInterfaceException.Create('GetMainWidget Widget=nil');
  
  Result := gtk_object_get_data(Widget, 'Main');
  if Result = nil then Result := Widget; // the widget is the main widget itself.
end;

procedure SetMainWidget(const ParentWidget, ChildWidget: Pointer);
begin
  if ParentWidget = nil
  then raise EInterfaceException.Create('SetMainWidget ParentWidget=nil');
  if ChildWidget = nil
  then raise EInterfaceException.Create('SetMainWidget ChildWidget=nil');
  if ParentWidget = ChildWidget
  then raise EInterfaceException.Create('SetMainWidget ParentWidget=ChildWidget');

  gtk_object_set_data(ChildWidget, 'Main', ParentWidget)
end;

{ ------------------------------------------------------------------------------
 Get the fixed widget of a widget.
 Every LCL control with a clientarea, has at least a main widget for the control
 and a fixed widget for the client area. If the Fixed widget is not set, use
 try to get it trough WinWidgetInfo
------------------------------------------------------------------------------ }
//TODO: remove when WinWidgetInfo implementation is complete
function GetFixedWidget(const Widget: Pointer): Pointer;
var
  WidgetInfo: PWinWidgetInfo;
begin
  if Widget = nil
  then raise EInterfaceException.Create('GetFixedWidget Widget=nil');

  WidgetInfo := GetWidgetInfo(Widget, False);
  if WidgetInfo <> nil
  then Result := WidgetInfo^.ClientWidget
  else Result := nil;
  if Result <> nil then Exit;
  
  Result := gtk_object_get_data(Widget, 'Fixed');
  // A last resort
  if Result = nil then Result := Widget;
end;

{ ------------------------------------------------------------------------------
 Set the fixed widget of a widget.
 Every LCL control with a clientarea, has at least a main widget for the control
 and a fixed widget for the client area.
------------------------------------------------------------------------------ }
procedure SetFixedWidget(const ParentWidget, FixedWidget: Pointer);
var
  WidgetInfo: PWinWidgetInfo;
begin
  if ParentWidget = nil
  then raise EInterfaceException.Create('SetFixedWidget ParentWidget=nil');
  
  WidgetInfo := GetWidgetInfo(ParentWidget, True);
  WidgetInfo^.ClientWidget := FixedWidget;
  //TODO: remove old compatebility
  gtk_object_set_data(ParentWidget, 'Fixed', FixedWidget)
end;

{-------------------------------------------------------------------------------
  Set the LCLobject which created this widget.

-------------------------------------------------------------------------------}
procedure SetLCLObject(const Widget: Pointer; const AnObject: TObject);
var
  WidgetInfo: PWinWidgetInfo;
begin
  if Widget = nil
  then raise EInterfaceException.Create('SetLCLObject Widget=nil');
  if AnObject = nil
  then raise EInterfaceException.Create('SetLCLObject AnObject=nil');

  WidgetInfo := GetWidgetInfo(Widget, True);
  WidgetInfo^.LCLObject := AnObject;
  //TODO: remove old compatebility
  gtk_object_set_data(Widget, 'Class', Pointer(AnObject));
end;

//TODO: cleanup when WidgetInfo is fully implemented
function GetLCLObject(const Widget: Pointer): TObject;
var
  WidgetInfo: PWinWidgetInfo;
begin
  if Widget = nil
  then raise EInterfaceException.Create('GetLCLObject Widget=nil');

  WidgetInfo := GetWidgetInfo(Widget);
  if WidgetInfo <> nil
  then Result := WidgetInfo^.LCLObject
  else Result := nil;

  // Fallback;
  if Result = nil
  then Result := TObject(gtk_object_get_data(Widget, 'Class'));
end;

{-------------------------------------------------------------------------------
 Some need the HiddenLCLobject which created a parent of this widget.

 MWE: is this obsolete ?
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
  function GetNearestLCLObject(Widget: PGtkWidget): TObject;
  
  Retrieves the LCLObject belonging to the widget. If the widget is created as
  child of a main widget, the parent is queried.
  
  This function probably obsoletes Get/SetMainWidget
-------------------------------------------------------------------------------}
//TODO: check if Get/SetMainWidget is still required
function GetNearestLCLObject(Widget: PGtkWidget): TObject;
begin
  while (Widget<>nil) do begin
    Result:=GetLCLObject(Widget);
    if Result<>nil then exit;
    Widget:=Widget^.Parent;
  end;
  Result:=nil;
end;

{------------------------------------------------------------------------------
  Procedure FixedMoveControl(Parent, Child : PGTKWIdget; Left, Top : Longint);
  
  Move a childwidget on a client area (fixed or layout widget).
------------------------------------------------------------------------------}
Procedure FixedMoveControl(Parent, Child : PGTKWIdget; Left, Top : Longint);
begin
  If GTKWidgetIsA(Parent, GTK_Layout_Get_Type) then
    // parent is layout
    gtk_Layout_move(PGtkLayout(Parent), Child, Left, Top)
  else If GTKWidgetIsA(Parent, GTK_Fixed_Get_Type) then begin
    // parent is fixed
    gtk_fixed_move(PGtkFixed(Parent), Child, gint16(Left), gint16(Top))
  end else
    // parent is invalid
    DebugLn('[FixedMoveControl] WARNING: Invalid Fixed Widget');
end;

{------------------------------------------------------------------------------
  Procedure FixedPutControl(Parent, Child : PGTKWIdget; Left, Top : Longint);

  Add a childwidget onto a client area (fixed or layout widget).
------------------------------------------------------------------------------}
Procedure FixedPutControl(Parent, Child : PGTKWidget; Left, Top : Longint);

  procedure RaiseInvalidFixedWidget;
  begin
    // this is in a separate procedure for optimisation
    DebugLn('[FixedPutControl] WARNING: Invalid Fixed Widget.',
      ' Parent=',DbgS(Parent),
      ' Child=',DbgS(Child)
      );
  end;

begin
  //DebugLn('FixedPutControl Parent=[',GetWidgetDebugReport(Parent),']',
  //  ' Child=[',GetWidgetDebugReport(Child),']');
  If GTKWidgetIsA(Parent, GTK_Fixed_Get_Type) then
    gtk_fixed_put(PGtkFixed(Parent), Child, gint16(Left), gint16(Top))
  else If GTKWidgetIsA(Parent, GTK_Layout_Get_Type) then
    gtk_Layout_Put(PGtkLayout(Parent), Child, Left, Top)
  else
    RaiseInvalidFixedWidget;
end;

function GetWinControlWidget(Child: PGtkWidget): PGtkWidget;
// return the first widget, which is associated with a TWinControl handle
var
  LCLParent: TObject;
begin
  Result:=nil;
  LCLParent:=GetNearestLCLObject(Child);
  if (LCLParent=nil) or (not (LCLParent is TWinControl))
  or (not TWinControl(LCLParent).HandleAllocated)
  then exit;
  Result:=PGtkWidget(TWinControl(LCLParent).Handle);
end;

function GetWinControlFixedWidget(Child: PGtkWidget): PGtkWidget;
begin
  Result:=GetWinControlWidget(Child);
  if Result=nil then exit;
  Result:=GetFixedWidget(Result);
end;

function FindFixedChildListItem(ParentFixed: PGtkFixed; Child: PGtkWidget): PGList;
begin
  Result:=ParentFixed^.children;
  while (Result<>nil) do begin
    if (Result^.Data<>nil) and (PGtkFixedChild(Result^.Data)^.Widget=Child) then
      exit;
    Result:=Result^.Next;
  end;
end;

function FindFixedLastChildListItem(ParentFixed: PGtkFixed): PGList;
begin
  Result:=g_list_last(ParentFixed^.children);
end;

function GetFixedChildListWidget(Item: PGList): PGtkWidget;
begin
  Result:=PGtkFixedChild(Item^.Data)^.Widget;
end;

{------------------------------------------------------------------------------
  procedure MoveGListLinkBehind(First, Item, After: PGList);

  Move the list item 'Item' behind the list item 'After'.
  If After=nil then insert as first item.
------------------------------------------------------------------------------}
procedure MoveGListLinkBehind(First, Item, After: PGList);
var
  Data: Pointer;
  NewPos: Integer;
begin
  if (Item=After) or (Item^.Next=After) then exit;
  if (g_list_position(First,Item)<0) then
    RaiseException('MoveGListLinkBehind Item not found');
  if (After<>nil) and (g_list_position(First,After)<0) then
    RaiseException('MoveGListLinkBehind After not found');
  Data:=Item^.Data;
  g_list_remove_link(First,Item);
  if After<>nil then begin
    NewPos:=g_list_position(First,After)+1;
  end else begin
    NewPos:=0;
  end;
  g_list_insert(First,Data,NewPos);
end;

procedure MoveGListLink(First: PGList; FromIndex, ToIndex: integer);
var
  Item: PGList;
  InsertAfter: PGList;
  i: Integer;
begin
  if (FromIndex=ToIndex) then exit;
  Item:=First;
  i:=0;
  while (i<FromIndex) do begin
    Item:=Item^.next;
    inc(i);
  end;
  // unbind
  if Item^.next<>nil then Item^.next^.prev:=Item^.prev;
  if Item^.prev<>nil then Item^.prev^.next:=Item^.next;
  Item^.next:=nil;
  Item^.prev:=nil;
  // insert
  if ToIndex=0 then begin
    Item^.next:=First;
    First^.prev:=Item;
  end else begin
    i:=0;
    InsertAfter:=First;
    while (i<ToIndex-1) do begin
      if InsertAfter^.next=nil then break;
      InsertAfter:=InsertAfter^.next;
      inc(i);
    end;
    Item^.prev:=InsertAfter;
    Item^.next:=InsertAfter^.next;
    InsertAfter^.next:=Item;
    if Item^.next<>nil then Item^.next^.prev:=Item;
  end;
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
    {$Ifdef GTK2}
      if (DC.Wnd<>0) and GTK_WIDGET_NO_WINDOW(PGTKWidget(DC.Wnd))
        and (not GtkWidgetIsA(PGTKWidget(DC.Wnd),GTKAPIWidget_GetType))
      then begin
        Inc(Result.X, PGTKWidget(DC.Wnd)^.Allocation.x);
        Inc(Result.y, PGTKWidget(DC.Wnd)^.Allocation.y);
      end;
    {$EndIf}
    if (DC.SpecialOrigin) and (DC.Wnd<>0) then begin
      Fixed := GetFixedWidget(PGTKWidget(DC.Wnd));
      if GtkWidgetIsA(Fixed,GTK_LAYOUT_GET_TYPE) then begin
        Adjustment:=gtk_layout_get_hadjustment(PGtkLayout(Fixed));
        if Adjustment<>nil then
          dec(Result.X,TruncToInt(Adjustment^.Value-Adjustment^.Lower));
        Adjustment:=gtk_layout_get_vadjustment(PGtkLayout(Fixed));
        if Adjustment<>nil then
          dec(Result.Y,TruncToInt(Adjustment^.Value-Adjustment^.Lower));
      end;
    end;
  end else begin
    Result.X:=0;
    Result.Y:=0;
  end;
end;



{------------------------------------------------------------------------------
  function CreateWidgetInfo(const AWidget: Pointer): PWidgetInfo;

 Creates a WidgetInfo structure for the given widget
 Info needed by the API of a HWND (=Widget)

 This structure obsoletes all other object data, like
   "core-child", "fixed", "class"
 ------------------------------------------------------------------------------}
function CreateWidgetInfo(const AWidget: Pointer): PWidgetInfo;
begin
  if AWidget = nil then Result:= nil
  else begin
    New(Result);
    FillChar(Result^, SizeOf(Result^), 0);
    gtk_object_set_data(AWidget, 'widgetinfo', Result);
  end;
end;

function CreateWidgetInfo(const AWidget: Pointer; const AObject: TObject;
  const AParams: TCreateParams): PWidgetInfo;
begin
  Result := CreateWidgetInfo(AWidget);
  if Result = nil then Exit;
  
  Result^.LCLObject := AObject;
  // in most cases the created widget is the core widget
  // so default to it
  Result^.CoreWidget := AWidget;
  Result^.Style := AParams.Style;
  Result^.ExStyle := AParams.ExStyle;
  Result^.WndProc := PtrInt(AParams.WindowClass.lpfnWndProc);
end;

function GetWidgetInfo(const AWidget: Pointer {; const Create: Boolean = False}): PWidgetInfo;
begin
  Result := GetWidgetInfo(AWidget, False);
end;

function GetWidgetInfo(const AWidget: Pointer;
  const ACreate: Boolean): PWidgetInfo;
var
  MainWidget: PGtkObject;
begin
  if AWidget <> nil
  then begin
    MainWidget := GetMainWidget(AWidget);
    if MainWidget = nil then MainWidget := AWidget;
    Result := gtk_object_get_data(MainWidget, 'widgetinfo');
    if (Result = nil) and ACreate
    then begin
      Result := CreateWidgetInfo(MainWidget);
      // use the main widget as default
      Result^.CoreWidget := PGtkWidget(MainWidget);
    end;  
  end
  else Result := nil;
end;

procedure FreeWidgetInfo(AWidget: Pointer);
var
  Info: PWidgetInfo;
begin
  if AWidget = nil then Exit;
  Info := gtk_object_get_data(AWidget, 'widgetinfo');
  if Info = nil then Exit;

  if Info^.DoubleBuffer <> nil then
    gdk_pixmap_unref(Info^.DoubleBuffer);

  if (Info^.UserData <> nil) and (Info^.DataOwner) then begin
     FreeMem(Info^.UserData);
     Info^.UserData := nil;
  end;
  gtk_object_set_data(AWidget,'widgetinfo',nil);

  Dispose(Info);
end;

{-------------------------------------------------------------------------------
  procedure DestroyWidget(Widget: PGtkWidget);

-------------------------------------------------------------------------------}
procedure DestroyWidget(Widget: PGtkWidget);
begin
  FreeWidgetInfo(Widget);
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
  function GetGtkNoteBookPageCount(ANoteBookWidget: PGtkNoteBook): integer;

  Returns the number of pages in a PGtkNotebook
-------------------------------------------------------------------------------}
function GetGtkNoteBookPageCount(ANoteBookWidget: PGtkNoteBook): integer;
var
  AListItem: PGList;
begin
  Result:=0;
  if ANoteBookWidget=nil then exit;
  AListItem:=ANoteBookWidget^.children;
  while AListItem<>nil do begin
    inc(Result);
    AListItem:=AListItem^.Next;
  end;
end;

{$ifndef VER1_0}
var
{$else}
const
{$endif}
  {$IFDef GTK1}
  NoteBookCloseBtnPixmapImg: PGdkPixmap = nil;
  NoteBookCloseBtnPixmapMask: PGdkPixmap = nil;
  {$Else}
  NoteBookCloseBtnPixbuf: PGdkPixbuf = nil;
  {$EndIf}

{-------------------------------------------------------------------------------
  procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);

  Removes the dummy page.
  See also AddDummyNoteBookPage
-------------------------------------------------------------------------------}
procedure RemoveDummyNoteBookPage(NoteBookWidget: PGtkNotebook);
var
  DummyWidget: PGtkWidget;
begin
  DummyWidget:=GetGtkNoteBookDummyPage(NoteBookWidget);
  if DummyWidget=nil then exit;
  gtk_notebook_remove_page(NoteBookWidget,
                           gtk_notebook_page_num(NoteBookWidget,DummyWidget));
  DummyWidget:=nil;
  SetGtkNoteBookDummyPage(NoteBookWidget,DummyWidget);
end;

{-------------------------------------------------------------------------------
  method GetNoteBookCloseBtnImage
  Params:
  Result: none

  Loads the image for the close button in the tabs of the TCustomNoteBook(s).
-------------------------------------------------------------------------------}
{$IfDef GTK1}
procedure GetNoteBookCloseBtnImage(Window: PGdkWindow;
  var Img, Mask: PGdkPixmap);
begin
  if (NoteBookCloseBtnPixmapImg=nil)
  and (Window<>nil) then begin
    LoadXPMFromLazResource('tnotebook_close_tab',Window,
                        NoteBookCloseBtnPixmapImg,NoteBookCloseBtnPixmapMask);
  end;
  Img:=NoteBookCloseBtnPixmapImg;
  Mask:=NoteBookCloseBtnPixmapMask;
end;
{$Else}
procedure GetNoteBookCloseBtnImage(var Img: PGdkPixbuf);
begin
  if (NoteBookCloseBtnPixbuf=nil) then
    LoadPixbufFromLazResource('tnotebook_close_tab', NoteBookCloseBtnPixbuf);
  Img:=NoteBookCloseBtnPixbuf;
end;
{$EndIF}

{-------------------------------------------------------------------------------
  method UpdateNotebookPageTab
  Params: ANoteBook: TCustomNotebook; APage: TCustomPage
  Result: none

  Updates the tab of a page of a notebook. This contains the image to the left
  side, the label, the close button, the menu image and the menu label.
-------------------------------------------------------------------------------}
procedure UpdateNotebookPageTab(ANoteBook, APage: TObject);
var
  TheNoteBook: TCustomNotebook;
  ThePage: TCustomPage;

  NoteBookWidget: PGtkWidget;  // the notebook
  PageWidget: PGtkWidget;      // the page (content widget)
  TabWidget: PGtkWidget;       // the tab (hbox containing a pixmap, a label
                               //          and a close button)
  TabImageWidget: PGtkWidget;  // the icon widget in the tab (a fixed widget)
  TabLabelWidget: PGtkWidget;  // the label in the tab
  TabCloseBtnWidget: PGtkWidget;// the close button in the tab
  TabCloseBtnImageWidget: PGtkWidget; // the pixmap in the close button
  MenuWidget: PGtkWidget;      // the popup menu (hbox containing a pixmap and
                               // a label)
  MenuImageWidget: PGtkWidget; // the icon widget in the popup menu item (a fixed widget)
  MenuLabelWidget: PGtkWidget; // the label in the popup menu item

  procedure UpdateTabImage;
  var
    HasIcon: Boolean;
    IconSize: TPoint;
  begin
    HasIcon:=false;
    IconSize:=Point(0,0);
    if (TheNoteBook.Images<>nil)
    and (ThePage.ImageIndex>=0)
    and (ThePage.ImageIndex<TheNoteBook.Images.Count) then begin
      // page has valid image
      IconSize:=Point(TheNoteBook.Images.Width,TheNoteBook.Images.Height);
      HasIcon:=(IconSize.X>0) and (IconSize.Y>0);
    end;

    if HasIcon then begin
      // page has an image
      if TabImageWidget<>nil then begin
        // there is already an icon widget for the image in the tab
        // -> resize the icon widget
        gtk_widget_set_usize(TabImageWidget,IconSize.X,IconSize.Y);
      end else begin
        // there is no pixmap for the image in the tab
        // -> insert one ot the left side of the label
        TabImageWidget:= gtk_label_new(#0);
        g_signal_connect(PgtkObject(TabImageWidget), 'expose_event',
                           TGTKSignalFunc(@PageIconWidgetExposeAfter), ThePage);
        {$IFNDEF GTK2}
        g_signal_connect(PgtkObject(TabImageWidget), 'draw',
                             TGTKSignalFunc(@PageIconWidgetDrawAfter), ThePage);
        {$ENDIF}
        gtk_object_set_data(PGtkObject(TabWidget),'TabImage',TabImageWidget);
        gtk_widget_set_usize(TabImageWidget,IconSize.X,IconSize.Y);
        gtk_widget_show(TabImageWidget);
        gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabImageWidget);
        gtk_box_reorder_child(PGtkBox(TabWidget),TabImageWidget,0);
      end;
      if MenuImageWidget<>nil then begin
        // there is already an icon widget for the image in the menu
        // -> resize the icon widget
        gtk_widget_set_usize(MenuImageWidget,IconSize.X,IconSize.Y);
      end else begin
        // there is no icon widget for the image in the menu
        // -> insert one at the left side of the label
        MenuImageWidget:=gtk_label_new(#0);
        g_signal_connect_after(PgtkObject(MenuImageWidget), 'expose_event',
                          TGTKSignalFunc(@PageIconWidgetExposeAfter), ThePage);
        {$IFNDEF GTK2}
        g_signal_connect_after(PgtkObject(MenuImageWidget), 'draw',
                             TGTKSignalFunc(@PageIconWidgetDrawAfter), ThePage);
        {$ENDIF}
        gtk_widget_set_usize(MenuImageWidget,IconSize.X,IconSize.Y);
        gtk_object_set_data(PGtkObject(MenuWidget),'TabImage',MenuImageWidget);
        gtk_widget_show(MenuImageWidget);
        gtk_box_pack_start_defaults(PGtkBox(MenuWidget),MenuImageWidget);
        gtk_box_reorder_child(PGtkBox(MenuWidget),MenuImageWidget,0);
      end;
    end else begin
      // page does not have an image
      if TabImageWidget<>nil then begin
        // there is a pixmap for an old image in the tab
        // -> remove the icon widget
        DestroyWidget(TabImageWidget);
        gtk_object_set_data(PGtkObject(TabWidget), 'TabImage', nil);
        TabImageWidget:=nil;
      end;
      if MenuImageWidget<>nil then begin
        // there is a pixmap for an old image in the menu
        // -> remove the icon widget
        DestroyWidget(MenuImageWidget);
        gtk_object_set_data(PGtkObject(MenuWidget), 'TabImage', nil);
        MenuImageWidget:=nil;
      end;
    end;
  end;

  procedure UpdateTabLabel;
  var TheCaption: PChar;
  begin
    TheCaption:=PChar(ThePage.Caption);
    if TheCaption=nil then
      TheCaption:=#0;
    gtk_label_set_text(PGtkLabel(TabLabelWidget),TheCaption);
    if MenuLabelWidget<>nil then
      gtk_label_set_text(PGtkLabel(MenuLabelWidget),TheCaption);
  end;

  procedure UpdateTabCloseBtn;
  var
    {$IfDef GTK1}
    Img: PGdkPixmap;
    Mask: PGdkBitmap;
    {$Else}
    Img: PGdkPixbuf;
    {$EndIf}
  begin
    {$IfDef GTK1}
    //debugln('UpdateTabCloseBtn ',GetWidgetDebugReport(NoteBookWidget));
    GetNoteBookCloseBtnImage(GetControlWindow(NoteBookWidget),Img,Mask);
    {$Else}
    GetNoteBookCloseBtnImage(Img);
    {$EndIf}
    //debugln('UpdateTabCloseBtn ',dbgs(nboShowCloseButtons in TheNotebook.Options),' ',dbgs(Img<>nil));
    if (nboShowCloseButtons in TheNotebook.Options) and (Img<>nil) then begin
      // close buttons enabled
      if TabCloseBtnWidget=nil then begin
        // there is no close button yet
        // -> add one to the right side of the label in the tab
        TabCloseBtnWidget:=gtk_button_new;
        gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn',
                            TabCloseBtnWidget);
        begin
          // put a pixmap into the button
         {$IfDef GTK1}
          TabCloseBtnImageWidget:=gtk_pixmap_new(Img,Mask);
         {$Else}
          TabCloseBtnImageWidget:=gtk_image_new_from_pixbuf(Img);
         {$EndIf}
          gtk_object_set_data(PGtkObject(TabCloseBtnWidget),'TabCloseBtnImage',
                              TabCloseBtnImageWidget);
          gtk_widget_show(TabCloseBtnImageWidget);
          gtk_container_add(PGtkContainer(TabCloseBtnWidget),
                            TabCloseBtnImageWidget);
        end;
        gtk_widget_show(TabCloseBtnWidget);
        g_signal_connect(PGtkObject(TabCloseBtnWidget), 'clicked',
          TGTKSignalFunc(@gtkNoteBookCloseBtnClicked), APage);
        gtk_box_pack_start_defaults(PGtkBox(TabWidget),TabCloseBtnWidget);
      end;
    end else begin
      // close buttons disabled
      if TabCloseBtnWidget<>nil then begin
        // there is a close button
        // -> remove it
        gtk_object_set_data(PGtkObject(TabWidget), 'TabCloseBtn',
                            nil);
        DestroyWidget(TabCloseBtnWidget);
        TabCloseBtnWidget:=nil;
      end;
    end;
  end;

begin
  ThePage:=TCustomPage(APage);
  TheNoteBook:=TCustomNotebook(ANoteBook);
  if (APage=nil) or (not ThePage.HandleAllocated) then exit;
  if TheNoteBook=nil then begin
    TheNoteBook:=TCustomNotebook(ThePage.Parent);
    if TheNoteBook=nil then exit;
  end;
  NoteBookWidget:=PGtkWidget(TWinControl(TheNoteBook).Handle);
  PageWidget:=PGtkWidget(TWinControl(ThePage).Handle);

  // get the tab container and the tab components: pixmap, label and closebtn
  TabWidget:=gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                        PageWidget);
  if TabWidget<>nil then begin
    TabImageWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabImage');
    TabLabelWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabLabel');
    TabCloseBtnWidget:=gtk_object_get_data(PGtkObject(TabWidget),'TabCloseBtn');
  end else begin
    TabImageWidget:=nil;
    TabLabelWidget:=nil;
    TabCloseBtnWidget:=nil;
  end;

  // get the menu container and its components: pixmap and label
  MenuWidget:=gtk_notebook_get_menu_label(PGtkNoteBook(NotebookWidget),
                                          PageWidget);
  if MenuWidget<>nil then begin
    MenuImageWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabImage');
    MenuLabelWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabLabel');
  end else begin
    MenuImageWidget:=nil;
    MenuLabelWidget:=nil;
  end;

  UpdateTabImage;
  UpdateTabLabel;
  UpdateTabCloseBtn;
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
  if TheWindow<>nil then begin
    BeginGDKErrorTrap;
    gdk_window_get_origin(TheWindow,@Result.X,@Result.Y);
    EndGDKErrorTrap;
  end else begin
    {$IFDEF RaiseExceptionOnNilPointers}
    LCLobject:=GetLCLObject(TheWidget);
    DbgOut('GetWidgetOrigin ');
    if LCLObject=nil then
      DbgOut(' LCLObject=nil')
    else if LCLObject is TControl then
      DbgOut(' LCLObject=',TControl(LCLObject).Name,':',TControl(LCLObject).ClassName)
    else
      DbgOut(' LCLObject=',TControl(LCLObject).ClassName);
    DebugLn('');
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
      BeginGDKErrorTrap;
      gdk_window_get_origin(ClientWindow,@Result.X,@Result.Y);
      {$Ifdef GTK2}
      if GTK_WIDGET_NO_WINDOW(ClientWidget)
      then begin
        Inc(Result.X, ClientWidget^.Allocation.X);
        Inc(Result.Y, ClientWidget^.Allocation.Y);
      end;
      {$EndIf}
      EndGDKErrorTrap;
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
    DebugLn('WARNING: TranslateGdkPointToClientArea SourceWindow=nil');
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
    // the mouse grab changed
    // -> this means the gtk itself has changed the mouse grab
    {$IFDEF VerboseMouseCapture}
    DebugLn('UpdateMouseCaptureControl Capture changed from ',
      '[',GetWidgetDebugReport(OldMouseCaptureWidget),']',
      ' to [',GetWidgetDebugReport(CurMouseCaptureWidget),']');
    {$ENDIF}

    // notify the new capture control
    MouseCaptureWidget:=CurMouseCaptureWidget;
    MouseCaptureType:=mctGTK;
    if MouseCaptureWidget<>nil then begin
      // the MouseCaptureWidget is probably not a main widget
      SendMessage(HWnd(MouseCaptureWidget), LM_CAPTURECHANGED, 0,
        HWnd(OldMouseCaptureWidget));
    end;
  end;
end;

procedure IncreaseMouseCaptureIndex;
begin
  if MouseCaptureIndex<$ffffffff then
    inc(MouseCaptureIndex)
  else
    MouseCaptureIndex:=0;
end;

procedure CaptureMouseForWidget(Widget: PGtkWidget; Owner: TMouseCaptureType);
var
  CaptureWidget: PGtkWidget;
  NowIndex: Cardinal;
begin
  {$IFDEF VerboseMouseCapture}
  DebugLn('CaptureMouseForWidget START ',GetWidgetDebugReport(Widget));
  {$ENDIF}
  if not (Owner in [mctGTKIntf,mctLCL]) then exit;
  // not every widget can capture the mouse
  CaptureWidget:=GetDefaultMouseCaptureWidget(Widget);
  if CaptureWidget=nil then exit;

  UpdateMouseCaptureControl;
  if (MouseCaptureType<>mctGTK) then begin
    // we are capturing
    if (MouseCaptureWidget=CaptureWidget) then begin
      // we are already capturing this widget
      exit;
    end;
    // release old capture
    ReleaseMouseCapture;
  end;

  {$IFDEF VerboseMouseCapture}
  DebugLn('CaptureMouseForWidget Start Capturing for ',GetWidgetDebugReport(CaptureWidget));
  {$ENDIF}
  IncreaseMouseCaptureIndex;
  NowIndex:=MouseCaptureIndex;
  if not gtk_widget_has_focus(CaptureWidget) then
    gtk_widget_grab_focus(CaptureWidget);
  if NowIndex=MouseCaptureIndex then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('CaptureMouseForWidget Commit Capturing for ',GetWidgetDebugReport(CaptureWidget));
    {$ENDIF}
    MouseCaptureWidget:=CaptureWidget;
    MouseCaptureType:=Owner;
    gtk_grab_add(CaptureWidget);
  end;
end;

function GetDefaultMouseCaptureWidget(Widget: PGtkWidget
  ): PGtkWidget;
var
  WidgetInfo: PWinWidgetInfo;
  LCLObject: TObject;
begin
  Result:=nil;
  if Widget=nil then exit;
  if GtkWidgetIsA(Widget,GTKAPIWidget_GetType) then begin
    WidgetInfo:=GetWidgetInfo(Widget,false);
    if WidgetInfo<>nil then
      Result:=WidgetInfo^.CoreWidget;
    exit;
  end;
  LCLObject:=GetNearestLCLObject(Widget);
  if LCLObject=nil then exit;
  if ((TWinControl(LCLObject) is TCustomSplitter)
  or (TWinControl(LCLObject) is TToolButton))
  and (TWinControl(LCLObject).HandleAllocated)
  then begin
    WidgetInfo:=GetWidgetInfo(PGtkWidget(TWinControl(LCLObject).Handle),false);
    if WidgetInfo<>nil then
      Result:=WidgetInfo^.CoreWidget;
  end;
end;

{------------------------------------------------------------------------------
  procedure ReleaseMouseCapture;

  If the current mouse capture was captured by the LCL or the gtk intf, release
  the capture. Don't release mouse captures of the gtk, because captures must
  be balanced and this is already done by the gtk.
 ------------------------------------------------------------------------------}
procedure ReleaseMouseCapture;
var
  OldMouseCaptureWidget: PGtkWidget;
begin
  {$IFDEF VerboseMouseCapture}
  DebugLn('ReleaseMouseCapture ',ord(MouseCaptureType),' MouseCaptureWidget=[',GetWidgetDebugReport(MouseCaptureWidget),']');
  {$ENDIF}
  if MouseCaptureType=mctGTK then exit;
  OldMouseCaptureWidget:=MouseCaptureWidget;
  MouseCaptureWidget:=nil;
  MouseCaptureType:=mctGTK;
  if OldMouseCaptureWidget<>nil then
    gtk_grab_remove(OldMouseCaptureWidget);
end;

{------------------------------------------------------------------------------
  procedure: SetCursor
  Params:  AWinControl : TWinControl
  Returns: Nothing

  Sets the cursor for a widget.
 ------------------------------------------------------------------------------}
procedure SetCursor(AWinControl : TWinControl; ACursor: TCursor);

  procedure DoSetCursor(AWindow: PGdkWindow; Cursor: pGDKCursor);
  begin
    if Cursor <> nil then begin
      gdk_window_set_cursor(AWindow, Cursor);
    end;
  end;

  procedure SetCursorRecursive(AWindow: PGdkWindow; Cursor: PGdkCursor);
  var
    ChildWindows, ListEntry: PGList;
  begin
    DoSetCursor(AWindow, Cursor);
    
    ChildWindows:=gdk_window_get_children(AWindow);

    ListEntry:=ChildWindows;
    while ListEntry<>nil do begin
      SetCursorRecursive(PGdkWindow(ListEntry^.Data), Cursor);
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
    if ACursor = crDefault then
      SetCursorRecursive(AWindow, GetGDKMouseCursor(crDefault))
    else begin
      NewCursor:= GetGDKMouseCursor(ACursor);
      if NewCursor <> nil then SetCursorRecursive(AWindow, NewCursor);
    end;  
    
  end else begin

    FixWidget:= GetFixedWidget(AWidget);
    AWindow:= GetControlWindow(FixWidget);
    if AWindow = nil then exit;
    
    NewCursor:= GetGDKMouseCursor(AWinControl.Cursor);
    if NewCursor <> nil then DoSetCursor(AWindow, NewCursor);
  end;
end;

{-------------------------------------------------------------------------------
  procedure: SignalConnect
  Params:  AWidget: PGTKWidget
           ASignal: PChar
           AProc:   Pointer
           AInfo:   PWidgetInfo
  Returns: Nothing

  Connects a gtk signal handler.
  This is wrappers to get around gtk casting
-------------------------------------------------------------------------------}
procedure SignalConnect(const AWidget: PGTKWidget; const ASignal: PChar;
  const AProc: Pointer; const AInfo: PWidgetInfo);
begin
  g_signal_connect(PGtkObject(AWidget), ASignal, TGTKSignalFunc(AProc), AInfo);
end;

{-------------------------------------------------------------------------------
  procedure: SignalConnectAfter
  Params:  AWidget: PGTKWidget
           ASignal: PChar
           AProc:   Pointer
           AInfo:   PGtkWSWidgetInfo
  Returns: Nothing

  Connects a gtk signal after handler. 
  This is wrappers to get around gtk casting
-------------------------------------------------------------------------------}
procedure SignalConnectAfter(const AWidget:PGTKWidget; const ASignal: PChar;
  const AProc: Pointer; const AInfo: PWidgetInfo);
begin
  g_signal_connect_after(PGTKObject(AWidget), ASignal, TGTKSignalFunc(AProc), AInfo);
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
  Result:=TDesignSignalMask(PtrInt(gtk_object_get_data(PGtkObject(Widget),
                                                'LCLDesignMask')));
end;

procedure SetDesignSignalMask(Widget: PGtkWidget; NewMask: TDesignSignalMask);
begin
  gtk_object_set_data(PGtkObject(Widget),'LCLDesignMask',Pointer(PtrInt(NewMask)));
end;

function GetDesignOnlySignalFlag(Widget: PGtkWidget;
  DesignSignalType: TDesignSignalType): boolean;
begin
  Result:=(GetDesignSignalMask(Widget)
          and DesignSignalMasks[DesignSignalType])<>0;
end;

function SignalConnected(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject;
  const ASFlags: TConnectSignalFlags): boolean;
{$IFDEF Gtk1}
var
  Handler: PGTKHandler;
  SignalID: guint;
begin
  Handler := gtk_object_get_data_by_id (AnObject, gtk_handler_quark);
  SignalID := g_signal_lookup(ASignal, GTK_OBJECT_TYPE(AnObject));
  if SignalID<0 then
    RaiseGDBException('SignalConnected');

  while (Handler <> nil) do begin
    with Handler^ do
    begin
      // check if signal is already connected
      //debugln('ConnectSignal Id=',dbgs(Id));
      if  (Id > 0)
      and (Signal_ID = SignalID)
      and (Func = TGTKSignalFunc(ACallBackProc))
      and (func_data = Pointer(ALCLObject))
      and (((flags and bmSignalAfter)<>0)=(csfAfter in ASFlags))
      then begin
        // signal is already connected
        Result:=true;
        Exit;
      end;

      Handler := Next;
    end;
  end;
  Result:=false;
end;
{$ELSE}
begin
  Result:=g_signal_handler_find(AnObject,
    G_SIGNAL_MATCH_FUNC or G_SIGNAL_MATCH_DATA,
    0,0,nil,ACallBackProc,ALCLObject)<>0;
end;
{$ENDIF}

procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject;
  const AReqSignalMask: TGdkEventMask; const ASFlags: TConnectSignalFlags);
var
  WinWidgetInfo: PWinWidgetInfo;
  MainWidget: PGtkWidget;
  OldDesignMask, NewDesignMask: TDesignSignalMask;
  DesignSignalType: TDesignSignalType;
  RealizeConnected: Boolean;
  HasRealizeSignal: Boolean;
begin
  if ACallBackProc = nil then
    RaiseGDBException('ConnectSignal');

  // first loop through the handlers to:
  // - check if a handler already exists
  // - Find the realize handler to change data
  DesignSignalType:=DesignSignalNameToType(ASignal,csfAfter in ASFlags);
  if SignalConnected(AnObject,ASignal,ACallBackProc,ALCLObject,ASFlags) then
  begin
    // signal is already connected
    // update the DesignSignalMask
    if (DesignSignalType <> dstUnknown)
    and (not (csfDesignOnly in ASFlags))
    then begin
      OldDesignMask := GetDesignSignalMask(PGtkWidget(AnObject));
      NewDesignMask :=
        OldDesignMask and not DesignSignalMasks[DesignSignalType];
      if OldDesignMask <> NewDesignMask
      then SetDesignSignalMask(PGtkWidget(AnObject),NewDesignMask);
    end;
    Exit;
  end;
  
  // if we are here, then no handler was defined yet
  // -> register handler
  //if (Msg=LM_LBUTTONUP) then DebugLn('CONNECT ',ReqSignalMask,' Widget=',DbgS(AnObject));
  //debugln('ConnectSignal ',DbgSName(ALCLObject),' ',ASignal,' After=',dbgs(csfAfter in ASFlags));
  if csfAfter in ASFlags then
    g_signal_connect_after(AnObject, ASignal,
                           TGTKSignalFunc(ACallBackProc), ALCLObject)
  else
    g_signal_connect      (AnObject, ASignal,
                           TGTKSignalFunc(ACallBackProc), ALCLObject);

  // update signal mask which will be set in the realize handler
  if (csfUpdateSignalMask in ASFlags) and (AReqSignalMask <> 0)
  then begin
    MainWidget := GetMainWidget(PGtkWidget(AnObject));
    if MainWidget=nil
    then MainWidget := PGtkWidget(AnObject);
    WinWidgetInfo := GetWidgetInfo(MainWidget,true);
    WinWidgetInfo^.EventMask := WinWidgetInfo^.EventMask or AReqSignalMask;
  end;
  
  // -> register realize handler
  if (csfConnectRealize in ASFlags) then begin
    HasRealizeSignal:=g_signal_lookup('realize', GTK_OBJECT_TYPE(AnObject))>0;
    if HasRealizeSignal then begin
      RealizeConnected:=SignalConnected(AnObject,'realize',@GTKRealizeCB,
                                        ALCLObject,[]);
      if not RealizeConnected then begin
        g_signal_connect(AnObject, 'realize',
          TGTKSignalFunc(@GTKRealizeCB), ALCLObject);
        g_signal_connect_after(AnObject, 'realize',
          TGTKSignalFunc(@GTKRealizeAfterCB), ALCLObject);
      end;
    end;
  end;

  // update the DesignSignalMask
  if (DesignSignalType <> dstUnknown)
  then begin
    OldDesignMask:=GetDesignSignalMask(PGtkWidget(AnObject));
    if csfDesignOnly in ASFlags then
      NewDesignMask:=OldDesignMask or DesignSignalMasks[DesignSignalType]
    else
      NewDesignMask:=OldDesignMask and not DesignSignalMasks[DesignSignalType];
    if OldDesignMask<>NewDesignMask then
      SetDesignSignalMask(PGtkWidget(AnObject),NewDesignMask);
  end;
end;

procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject;
  const AReqSignalMask: TGdkEventMask);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc, ALCLObject, AReqSignalMask,
                [csfConnectRealize,csfUpdateSignalMask]);
end;

procedure ConnectSignalAfter(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject;
  const AReqSignalMask: TGdkEventMask);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc, ALCLObject, AReqSignalMask,
                [csfConnectRealize,csfUpdateSignalMask,csfAfter]);
end;

procedure ConnectSignal(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject);
begin
  ConnectSignal(AnObject,ASignal,ACallBackProc, ALCLObject, 0);
end;

procedure ConnectSignalAfter(const AnObject:PGTKObject; const ASignal: PChar;
  const ACallBackProc: Pointer; const ALCLObject: TObject);
begin
  ConnectSignalAfter(AnObject,ASignal,ACallBackProc, ALCLObject, 0);
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
    ScrolledWindow: PGtkScrolledWindow;
    BinWidget: PGtkBin;
    {$IFDEF Gtk2}
    ChildEntry2: PGList;
    {$ELSE}
    ChildEntry: PGSList;
    {$ENDIF}
    ChildWidget: PGtkWidget;
  begin
    //if AWinControl is TListView then DebugLn('ConnectChilds A ',DbgS(TheWidget));
    if GtkWidgetIsA(TheWidget,GTK_TYPE_CONTAINER) then begin
      //if AWinControl is TListView then DebugLn('ConnectChilds B ');
      // this is a container widget -> connect all childs
      {$IFDEF Gtk2}
      ChildEntry2:=gtk_container_get_children(PGtkContainer(TheWidget));
      while ChildEntry2<>nil do begin
        ChildWidget:=PGtkWidget(ChildEntry2^.Data);
        if ChildWidget<>TheWidget then
          ConnectSignals(ChildWidget);
        ChildEntry2:=ChildEntry2^.Next;
      end;
      {$ELSE}
      ChildEntry:=PGtkContainer(TheWidget)^.resize_widgets;
      while ChildEntry<>nil do begin
        ChildWidget:=PGtkWidget(ChildEntry^.Data);
        ConnectSignals(ChildWidget);
        ChildEntry:=ChildEntry^.Next;
      end;
      {$endif}
    end;
    if GtkWidgetIsA(TheWidget,GTK_TYPE_BIN) then begin
      //if AWinControl is TListView then DebugLn('ConnectChilds C ');
      BinWidget:=PGtkBin(TheWidget);
      ConnectSignals(BinWidget^.child);
    end;
    if GtkWidgetIsA(TheWidget,GTK_TYPE_SCROLLED_WINDOW) then begin
      //if AWinControl is TListView then DebugLn('ConnectChilds D ');
      ScrolledWindow:=PGtkScrolledWindow(TheWidget);
      ConnectSignals(ScrolledWindow^.hscrollbar);
      ConnectSignals(ScrolledWindow^.vscrollbar);
    end;
    if GtkWidgetIsA(TheWidget,GTK_TYPE_COMBO) then begin
      //if AWinControl is TListView then DebugLn('ConnectChilds E ');
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
    //if AWinControl is TListView then DebugLn('ConnectSignals A ',DbgS(TheWidget));
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

    //if AWinControl is TListView then DebugLn('ConnectSignals B ',DbgS(TheWidget));
    // connect signals needed for design mode:
    for DesignSignalType:=Low(TDesignSignalType) to High(TDesignSignalType) do
    begin
      if DesignSignalType=dstUnknown then continue;
      if (not DesignSignalBefore[DesignSignalType])
      and (not DesignSignalAfter[DesignSignalType]) then
        continue;

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
    DebugLn('GetAccelGroup CREATING  Widget=',DbgS(Widget),' CreateIfNotExists=',CreateIfNotExists);
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
    DebugLn('SetAccelGroup AnAccelGroup=',DbgS(AnAccelGroup),' IsMenu=',GtkWidgetIsA(Widget,GTK_MENU_TYPE));
    {$ENDIF}
    if GtkWidgetIsA(Widget,GTK_TYPE_MENU) then
      gtk_menu_set_accel_group(PGtkMenu(Widget), AnAccelGroup)
    else begin
      {$IfDef GTK2}
      Assert(GtkWidgetIsA(Widget,GTK_TYPE_WINDOW));
      gtk_window_add_accel_group(GTK_WINDOW(widget), AnAccelGroup)
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
    DebugLn('FreeAccelGroup  AccelGroup=',DbgS(AccelGroup));
    {$ENDIF}
    gtk_accel_group_unref(AccelGroup);
    SetAccelGroup(Widget,nil);
  end;
end;

procedure ShareWindowAccelGroups(AWindow: PGtkWidget);

  procedure AttachUnique(TheWindow: PGtkWidget; TheAccelGroup: PGTKAccelGroup);
  begin
    {$IfDef GTK2}
    if (TheWindow=nil) or (TheAccelGroup=nil)
      or (TheAccelGroup^.acceleratables=nil)
      or (g_slist_find(TheAccelGroup^.acceleratables, TheWindow)=nil) then
    exit;
    gtk_window_add_accel_group(GTK_WINDOW(TheWindow), TheAccelGroup);
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
  then
    exit;
  
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
    DebugLn('ShareWindowAccelGroups ',TheForm.Name,':',TheForm.ClassName,
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
    if (TheWindow=nil) or (TheAccelGroup=nil)
      or (TheAccelGroup^.acceleratables=nil)
      or (g_slist_find(TheAccelGroup^.acceleratables, TheWindow)=nil) then
    exit;
    gtk_window_remove_accel_group(GTK_WINDOW(TheWindow), TheAccelGroup);
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
    DebugLn('UnshareWindowAccelGroups ',TheForm.Name,':',TheForm.ClassName,
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
    DebugLn('GetAccelGroupForComponent A ',Component.Name,':',Component.ClassName);
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
        DebugLn('GetAccelGroupForComponent C ',Component.Name,':',Component.ClassName);
        {$ENDIF}
      end;
    end;
  end;
  {$IFDEF VerboseAccelerator}
  DebugLn('GetAccelGroupForComponent END ',Component.Name,':',Component.ClassName,' Result=',DbgS(Result));
  {$ENDIF}
end;

function GetAccelKey(Widget: PGtkWidget): PAcceleratorKey;
begin
  Result := PAcceleratorKey(gtk_object_get_data(PGtkObject(Widget),'AccelKey'));
end;

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
  DebugLn('SetAccelKey Widget=',DbgS(Widget),
    ' Key=',Key,' Mods=',DbgS(Mods),
    ' Signal="',Signal,'" Result=',DbgS(Result));
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
      DebugLn('RealizeAccelerator Add Accelerator ',
        Component.Name,':',Component.ClassName,
        ' Widget=',DbgS(Widget),
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
    DebugLn('UnrealizeAccelerator  ',
      ' Widget=',DbgS(Widget),
      ' Signal=',AccelKey^.Signal,
      ' Key=',AccelKey^.Key,' Mods=',AccelKey^.Mods,
      '');
    {$ENDIF}
    {$Ifdef GTK2}
       DebugLn('ToDo: gtkproc.inc UnrealizeAccelerator');
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
  DebugLn('Accelerate ',Component.Name,':',Component.ClassName,' Key=',Key,' Mods=',DbgS(Mods),' Signal=',Signal);
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
  const NewShortCut: TShortCut; const Signal : string);
var
  GDKModifier: TGdkModifierType;
  GDKKey: guint;
  NewKey: word;
  NewModifier: TShiftState;
begin
  { Map the shift states }
  GDKModifier:= 0;
  ShortCutToKey(NewShortCut, NewKey, NewModifier);
  if ssShift in NewModifier then GDKModifier:= GDKModifier + GDK_SHIFT_MASK;
  if ssAlt in NewModifier then GDKModifier:= GDKModifier + GDK_MOD1_MASK;
  if ssCtrl in NewModifier then GDKModifier:= GDKModifier + GDK_CONTROL_MASK;

  // Send the unmodified keysym ?
  if (ssShift in NewModifier)
  and ((NewKey < VK_F1) or (NewKey > VK_F24))
  then GDKKey := GetVKeyInfo(NewKey).KeySym[1]
  else GDKKey := GetVKeyInfo(NewKey).KeySym[0];

  Accelerate(Component,Widget,GDKKey,GDKModifier,Signal);
end;

{-------------------------------------------------------------------------------
  method TGtkWidgetSet LoadPixbufFromLazResource
  Params: const ResourceName: string;
          var Pixbuf: PGdkPixbuf
  Result: none

  Loads a pixbuf from a lazarus resource. The resource must be a XPM file.
-------------------------------------------------------------------------------}
{$IfNDef NoGdkPixbufLib}
procedure LoadPixbufFromLazResource(const ResourceName: string;
  var Pixbuf: PGdkPixbuf);
var
  ImgData: PPChar;
begin
  Pixbuf:=nil;
  try
    ImgData:=LazResourceXPMToPPChar(ResourceName);
  except
    on e: Exception do
      DebugLn('WARNING: TGtkWidgetSet.LoadXPMFromLazResource: '+e.Message);
  end;
  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
  pixbuf:=gdk_pixbuf_new_from_xpm_data(ImgData);
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  FreeMem(ImgData);
end;
{$EndIF}

{-------------------------------------------------------------------------------
  method LoadXPMFromLazResource
  Params: const ResourceName: string;
          Window: PGdkWindow;
          var PixmapImg, PixmapMask: PGdkPixmap
  Result: none

  Loads a pixmap from a lazarus resource. The resource must be a XPM file.
-------------------------------------------------------------------------------}
procedure LoadXPMFromLazResource(const ResourceName: string;
  Window: PGdkWindow; var PixmapImg, PixmapMask: PGdkPixmap);
var
  ImgData: PPGChar;
begin
  PixmapImg:=nil;
  PixmapMask:=nil;
  try
    ImgData:=PPGChar(LazResourceXPMToPPChar(ResourceName));
  except
    on e: Exception do
      DebugLn('WARNING: TGtkWidgetSet.LoadXPMFromLazResource: '+e.Message);
  end;
  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
  PixmapImg:=gdk_pixmap_create_from_xpm_d(Window,PixmapMask,nil,ImgData);
  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  FreeMem(ImgData);
end;

{------------------------------------------------------------------------------
  procedure GetGdkPixmapFromGraphic(LCLGraphic: TGraphic;
    var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);

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
      gdk_window_get_size(IconImg, @Width, @Height);
  end;
end;

{------------------------------------------------------------------------------
  procedure GetGdkPixmapFromMenuItem(LCLMenuItem: TMenuItem;
    var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
 ------------------------------------------------------------------------------}
procedure GetGdkPixmapFromMenuItem(LCLMenuItem: TMenuItem;
  var IconImg, IconMask: PGdkPixmap; var Width, Height: integer);
begin
  IconImg:=nil;
  IconMask:=nil;
  Width:=0;
  Height:=0;
  if LCLMenuItem=nil then exit;
  if LCLMenuItem.HasBitmap then
    GetGdkPixmapFromGraphic(LCLMenuItem.Bitmap,IconImg,IconMask,Width,Height);
end;

{------------------------------------------------------------------------------
  function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;

  Returns the gtk klass of a menuitem widget.
 ------------------------------------------------------------------------------}
function MENU_ITEM_CLASS(widget: PGtkWidget): PGtkMenuItemClass;
begin
  Result:=GTK_MENU_ITEM_CLASS(gtk_object_get_class(widget));
end;

{------------------------------------------------------------------------------
  function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;

  Returns the gtk klass of a checkmenuitem widget.
 ------------------------------------------------------------------------------}
function CHECK_MENU_ITEM_CLASS(widget: PGtkWidget): PGtkCheckMenuItemClass;
begin
  Result:=GTK_CHECK_MENU_ITEM_CLASS(gtk_object_get_class(widget));
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
  procedure LockRadioGroupOnChange(RadioGroup: PGSList; const ADelta: Integer);

  Calls LockOnChange for all groupmembers
 ------------------------------------------------------------------------------}
procedure LockRadioGroupOnChange(RadioGroup: PGSList; const ADelta: Integer);
begin
  while RadioGroup <> nil do
  begin
    if RadioGroup^.Data <> nil
    then LockOnChange(PgtkObject(RadioGroup^.Data), ADelta);
    RadioGroup := RadioGroup^.Next;
  end;
end;

{------------------------------------------------------------------------------
  procedure UpdateRadioGroupChecks(RadioGroup: PGSList);

  Set 'checked' for all menuitems in the group
 ------------------------------------------------------------------------------}
procedure UpdateRadioGroupChecks(RadioGroup: PGSList);
var
  CurListItem: PGSList;
  MenuItem: PGtkCheckMenuItem;
  LCLMenuItem: TMenuItem;
begin
  // Check if it is a single entry
  if (RadioGroup = nil) or (RadioGroup^.Next = nil)
  then Exit;
  
  // Lock whole group for update
  LockRadioGroupOnChange(RadioGroup, +1);
  CurListItem := RadioGroup;
  try
    // set active radiomenuitem
    while CurListItem <> nil do
    begin
      MenuItem := PGtkCheckMenuItem(CurListItem^.Data);
      if MenuItem<>nil
      then begin
        LCLMenuItem := TMenuItem(GetLCLObject(MenuItem));
        if  (LCLMenuItem <> nil)
        and (gtk_check_menu_item_get_active(MenuItem) <> LCLMenuItem.Checked)
        then gtk_check_menu_item_set_active(MenuItem, LCLMenuItem.Checked);
      end;
      CurListItem := CurListItem^.Next;
    end;
  finally
    // Unlock whole group for update
    LockRadioGroupOnChange(RadioGroup, -1);
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
  AWindow: PGdkWindow;
  IconWidth, IconHeight: integer;
  IconSize: TPoint;
begin
  if (MenuItem=nil) then exit;
  if not (GTK_WIDGET_DRAWABLE (PGtkWidget(MenuItem))) then exit;

  // get icon
  LCLMenuItem:=TMenuItem(GetLCLObject(MenuItem));
  if not LCLMenuItem.HasIcon then begin
    // call default draw function
    OldCheckMenuItemDrawProc(MenuItem,Area);
    exit;
  end;
  IconSize:=LCLMenuItem.GetIconSize;
  IconWidth:=IconSize.X;
  IconHeight:=IconSize.Y;

  // calculate left and top
  Widget := PGtkWidget(MenuItem);
  AWindow:=GetControlWindow(Widget);
  if AWindow=nil then exit;
  Container := GTK_CONTAINER (MenuItem);
  BorderWidth := Container^.flag0 and bm_TGtkContainer_border_width;
  ALeft := {$Ifdef GTK2}Widget^.Allocation.x + {$EndIf}
           (BorderWidth + gtk_widget_get_xthickness(gtk_widget_get_style(Widget)) + 2)
           +((PGtkMenuItem(MenuItem)^.toggle_size-IconWidth) div 2);
  ATop := {$Ifdef GTK2} Widget^.Allocation.y + {$EndIf}
          (Widget^.Allocation.Height - IconHeight) div 2;

  // draw icon
  if (LCLMenuItem.HasBitmap) then begin
    GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,IconWidth,IconHeight);
    gdk_gc_set_clip_mask(gtk_widget_get_style(Widget)^.Black_gc, IconMask);
    gdk_gc_set_clip_origin(gtk_widget_get_style(Widget)^.Black_gc,ALeft,ATop);
    gdk_draw_pixmap(AWindow,gtk_widget_get_style(Widget)^.Black_gc,
       IconImg,0,0,ALeft,ATop,-1,-1);
    gdk_gc_set_clip_mask(gtk_widget_get_style(Widget)^.Black_gc, nil);
  end else begin
    DrawImageListIconOnWidget(LCLMenuItem.GetImageList,LCLMenuItem.ImageIndex,
      Widget,false,false,ALeft,ATop);
  end;
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
  MenuItem: PGtkMenuItem;
  CheckMenuItem: PGtkMenuItem;
  LCLMenuItem: TMenuItem;
  IconSize: TPoint;
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
        IconSize:=LCLMenuItem.GetIconSize;
        {if IconSize.X>100 then
          debugln('MenuSizeRequest LCLMenuItem=',LCLMenuItem.Name,' ',LCLMenuItem.Caption,
            ' ');}
        if CurToggleSize<IconSize.X then
          CurToggleSize:=IconSize.X;
      end;
      if MaxToggleSize<CurToggleSize then
        MaxToggleSize:=CurToggleSize;
    end;
    ListItem:=ListItem^.Next;
  end;
  //DebugLn('MenuSizeRequest A MaxToggleSize=',MaxToggleSize);
  {$IFDEF Gtk2}
  // Gtk2ToDo
  if CheckMenuItem<>nil then begin
    GTK_MENU_ITEM(CheckMenuItem)^.toggle_size := 0;
    gtk_menu_item_toggle_size_allocate(GTK_MENU_ITEM(CheckMenuItem),MaxToggleSize);
    GTK_MENU_ITEM(CheckMenuItem)^.toggle_size := MaxToggleSize;
  end;
  {$ELSE}
  if CheckMenuItem<>nil then
    MENU_ITEM_CLASS(PGtkWidget(CheckMenuItem))^.toggle_size:=MaxToggleSize;
  {$ENDIF}
  //DebugLn('MenuSizeRequest B ',GetWidgetDebugReport(widget),' ',requisition^.width,',',requisition^.height);
  OldMenuSizeRequestProc(Widget,requisition);
  //DebugLn('MenuSizeRequest C ',GetWidgetDebugReport(widget),' ',requisition^.width,',',requisition^.height);
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
          GDK_MOD1_MASK,{$Ifdef GTK2}'activate'{$Else}'activate_item'{$EndIF});
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
    IconWidth, IconHeight: integer;
    MinHeightWidget: PGtkWidget;
    IconSize: TPoint;
  begin
    // the icon will be painted instead of the toggle
    // of a normal gtkcheckmenuitem

    // get the icon
    //GetGdkPixmapFromMenuItem(LCLMenuItem,IconImg,IconMask,IconWidth,IconHeight);
    if LCLMenuItem.HasIcon then begin
      IconSize:=LCLMenuItem.GetIconSize;
      IconWidth:=IconSize.X;
      IconHeight:=IconSize.Y;
      // set the toggle width
      GTK_MENU_ITEM(MenuItemWidget)^.toggle_size:=guint16(IconWidth);
      
      GTK_MENU_ITEM(MenuItemWidget)^.flag0:=
        PGtkMenuItem(MenuItemWidget)^.flag0 or
          {$IFDEF Gtk2}
          bm_TGtkCheckMenuItem_always_show_toggle;
          {$ELSE}
          bm_show_toggle_indicator;
          {$ENDIF}

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
    if (OldCheckMenuItemToggleSize=0) then begin
      gtk_menu_item_toggle_size_request(GTK_MENU_ITEM(MenuItemWidget), @OldCheckMenuItemToggleSize);
      OldCheckMenuItemToggleSize := GTK_MENU_ITEM(MenuItemWidget)^.toggle_size;
    end;
    {$else}
    if (OldCheckMenuItemToggleSize=0) then
      OldCheckMenuItemToggleSize:=MENU_ITEM_CLASS(MenuItemWidget)^.toggle_size;
    {$endif}
    g_signal_connect_after(PGTKObject(MenuItemWidget), 'toggled',
      TGTKSignalFunc(@GTKCheckMenuToggeledCB), Pointer(LCLMenuItem));
  end;


  // set attributes (enabled and rightjustify)
  gtk_widget_set_sensitive(MenuItemWidget,
                           LCLMenuItem.Enabled and (LCLMenuItem.Caption<>'-'));
  if LCLMenuItem.RightJustify then
    gtk_menu_item_right_justify(PGtkMenuItem(MenuItemWidget));

  // create the hbox containing the label and the control
  UpdateInnerMenuItem(LCLMenuItem,MenuItemWidget);

  gtk_widget_show(MenuItemWidget);
  Result:=MenuItemWidget;
end;

function CreateStatusBarPanel(StatusBar: TObject; Index: integer): PGtkWidget;
begin
  Result:=gtk_statusbar_new;
  gtk_widget_show(Result);
  // other properties are set in UpdateStatusBarPanels
end;

procedure UpdateStatusBarPanels(StatusBar: TObject;
  StatusBarWidget: PGtkWidget);
var
  AStatusBar: TStatusBar;
  HBox: PGtkWidget;
  CurPanelCount: integer;
  NewPanelCount: Integer;
  CurStatusPanelWidget: PGtkWidget;
  ListItem: PGList;
  i: Integer;
  ExpandItem: boolean;
begin
  //DebugLn('UpdateStatusBarPanels ',DbgS(StatusBar));

  AStatusBar:=StatusBar as TStatusBar;
  HBox:=PGtkWidget(StatusBarWidget);
  if (not GtkWidgetIsA(StatusBarWidget,GTK_HBOX_GET_TYPE)) then
    RaiseGDBException('');

  // create needed panels
  CurPanelCount:=integer(g_list_length(PGtkBox(HBox)^.children));
  if AStatusBar.SimplePanel or (AStatusBar.Panels.Count<1) then
    NewPanelCount:=1
  else
    NewPanelCount:=AStatusBar.Panels.Count;
  while CurPanelCount<NewPanelCount do begin
    CurStatusPanelWidget:=CreateStatusBarPanel(StatusBar,CurPanelCount);
    ExpandItem:=(CurPanelCount=NewPanelCount-1);
    gtk_box_pack_start(PGtkBox(HBox),CurStatusPanelWidget,
                       ExpandItem,ExpandItem,0);
    inc(CurPanelCount);
  end;

  // remove unneeded panels
  while CurPanelCount>NewPanelCount do begin
    CurStatusPanelWidget:=PGtkBoxChild(
              g_list_nth_data(PGtkBox(HBox)^.children,CurPanelCount-1))^.Widget;
    DestroyConnectedWidgetCB(CurStatusPanelWidget,true);
    dec(CurPanelCount);
  end;

  // check new panel count
  CurPanelCount:=integer(g_list_length(PGtkBox(HBox)^.children));
  //DebugLn('TGtkWidgetSet.UpdateStatusBarPanels B ',Dbgs(StatusBar),' NewPanelCount=',dbgs(NewPanelCount),' CurPanelCount=',dbgs(CurPanelCount));
  if CurPanelCount<>NewPanelCount then
    RaiseGDBException('');

  // set panel properties
  ListItem:=PGTKBox(HBox)^.children;
  i:=0;
  while ListItem<>nil do begin
    CurStatusPanelWidget:=PGtkBoxChild(PGTKWidget(ListItem^.data))^.widget;
    ExpandItem:=(ListItem^.next=nil);
    gtk_box_set_child_packing(PGtkBox(HBox),CurStatusPanelWidget,
      ExpandItem,ExpandItem,0,GTK_PACK_START);
    UpdateStatusBarPanel(StatusBar,i,CurStatusPanelWidget);
    inc(i);
    ListItem:=ListItem^.next;
  end;
end;

procedure UpdateStatusBarPanel(StatusBar: TObject; Index: integer;
  StatusPanelWidget: PGtkWidget);
var
  AStatusBar: TStatusBar;
  CurPanel: TStatusPanel;
  FrameWidget: PGtkWidget;
  LabelWidget: PGtkLabel;
  PanelText: String;
  ContextID: LongWord;
  NewShadowType: TGtkShadowType;
  NewJustification: TGtkJustification;
begin
  //DebugLn('UpdateStatusBarPanel ',DbgS(StatusBar),' Index=',dbgs(Index));
  AStatusBar:=StatusBar as TStatusBar;

  CurPanel:=nil;
  if (not AStatusBar.SimplePanel) and (AStatusBar.Panels.Count>Index) then
    CurPanel:=AStatusBar.Panels[Index];
  //DebugLn('Panel ',Index,' ',GetWidgetClassName(StatusPanelWidget),
  //  ' frame=',GetWidgetClassName(PGTKStatusBar(StatusPanelWidget)^.frame),
  //  ' thelabel=',GetWidgetClassName(PGTKStatusBar(StatusPanelWidget)^.thelabel),
  // '');
  FrameWidget:=PGTKStatusBar(StatusPanelWidget)^.frame;
  LabelWidget:=PGtkLabel({$ifdef gtk2}PGTKStatusBar(StatusPanelWidget)^._label{$else}PGTKStatusBar(StatusPanelWidget)^.thelabel{$endif});

  // Text
  if AStatusBar.SimplePanel then
    PanelText:=AStatusBar.SimpleText
  else if CurPanel<>nil then
    PanelText:=CurPanel.Text
  else
    PanelText:='';
  ContextID:=gtk_statusbar_get_context_id(PGTKStatusBar(StatusPanelWidget),
                                          'state');
  //DebugLn('  PanelText="',PanelText,'"');
  if PanelText<>'' then
    gtk_statusbar_push(PGTKStatusBar(StatusPanelWidget),ContextID,
                       PGChar(PanelText))
  else
    gtk_statusbar_push(PGTKStatusBar(StatusPanelWidget),ContextID,'');


  // Alignment
  if CurPanel<>nil then begin
    //DebugLn('  Alignment="',ord(CurPanel.Alignment),'"');
    case CurPanel.Alignment of
    taLeftJustify: NewJustification:=GTK_JUSTIFY_LEFT;
    taRightJustify: NewJustification:=GTK_JUSTIFY_RIGHT;
    taCenter: NewJustification:=GTK_JUSTIFY_CENTER;
    else
      NewJustification:=GTK_JUSTIFY_LEFT;
    end;
    gtk_label_set_justify(LabelWidget,NewJustification);
  end;

  // Bevel
  if CurPanel<>nil then begin
    case CurPanel.Bevel of
    pbNone: NewShadowType:=GTK_SHADOW_NONE;
    pbLowered: NewShadowType:=GTK_SHADOW_IN;
    pbRaised: NewShadowType:=GTK_SHADOW_OUT;
    else
      NewShadowType:=GTK_SHADOW_IN;
    end;
    gtk_frame_set_shadow_type(PGtkFrame(FrameWidget),NewShadowType);
  end;

  // Width
  if (CurPanel<>nil) then begin
    //DebugLn('  CurPanel.Width="',CurPanel.Width,'"');
    gtk_widget_set_usize(StatusPanelWidget,CurPanel.Width,
      StatusPanelWidget^.allocation.height);
  end;
end;

{------------------------------------------------------------------------------
  SaveSizeNotification
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
  DbgOut('SaveSizeNotification Widget=',DbgS(Widget));
  LCLControl:=TWinControl(GetLCLObject(Widget));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then
      DebugLn(' ',LCLControl.Name,':',LCLControl.ClassName)
    else
      DebugLn(' ERROR: ',LCLControl.ClassName);
  end else begin
    DebugLn(' ERROR: LCLControl=nil');
  end;
  {$ENDIF}
  if not FWidgetsResized.Contains(Widget) then
    FWidgetsResized.Add(Widget);
end;

{------------------------------------------------------------------------------
  SaveClientSizeNotification
  Params: FixWidget: PGtkWidget  A widget that is the fixed widget
                                 of a lcl control.

  When the gtk sends a size signal, it is not sent directly to the LCL. All gtk
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
  //write('SaveClientSizeNotification',
  //  ' FixWidget=',DbgS(FixWidget),
  //  ' MainWIdget=',DbgS(MainWidget));
  LCLControl:=TWinControl(GetLCLObject(MainWidget));
  if (LCLControl<>nil) then begin
    if LCLControl is TWinControl then begin
      //DebugLn('SaveClientSizeNotification ',LCLControl.Name,':',LCLControl.ClassName,
      //  ' FixWidget=',DbgS(FixWidget),
      //  ' MainWidget=',DbgS(MainWidget));
    end else begin
      DbgOut('ERROR: SaveClientSizeNotification ',
        ' LCLControl=',LCLControl.ClassName,
        ' FixWidget=',DbgS(FixWidget),
        ' MainWidget=',DbgS(MainWidget));
      RaiseGDBException('SaveClientSizeNotification');
    end;
  end else begin
    DbgOut('ERROR: SaveClientSizeNotification LCLControl=nil',
      ' FixWidget=',DbgS(FixWidget),
      ' MainWIdget=',DbgS(MainWidget));
    RaiseGDBException('SaveClientSizeNotification');
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
  //DebugLn(' KKK0');
  Result:=TList.Create;
  if HashArray.Count=0 then exit;
  
  // put all widgets into an array and calculate their parent levels
  GetMem(TopologicalList,SizeOf(TTopologicalEntry)*HashArray.Count);
  HashItem:=HashArray.FirstHashItem;
  i:=0;
  MaxLevel:=0;
  //DebugLn(' KKK1 HashArray.Count=',HashArray.Count);
  while HashItem<>nil do begin
    TopologicalList[i].Widget:=HashItem^.Item;
    //DebugLn(' KKK21 i=',i,' Widget=',DbgS(TopologicalList[i].Widget));
    LCLControl:=TControl(GetLCLObject(TopologicalList[i].Widget));
    if (LCLControl=nil) or (not (LCLControl is TControl)) then
      RaiseException('CreateTopologicalSortedWidgets: '
                             +'Widget without LCL control');
    Lvl:=GetParentLevel(LCLControl);
    TopologicalList[i].ParentLevel:=Lvl;
    if MaxLevel<Lvl then
      MaxLevel:=Lvl;
    //DebugLn(' KKK2 i=',i,' Lvl=',Lvl,' MaxLvl=',MaxLevel,' LCLControl=',LCLControl.Name,':',LCLControl.ClassName);
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
    //DebugLn(' KKK5 i=',i,' Lvl=',Lvl,' LevelCounts[Lvl]=',LevelCounts[Lvl],
    //  ' Widget=',DbgS(TopologicalList[i].Widget));
    Result[LevelCounts[Lvl]]:=TopologicalList[i].Widget;
  end;
  
  FreeMem(LevelCounts);
  FreeMem(TopologicalList);
end;

procedure GetGTKDefaultWidgetSize(AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer);
var
  Widget: PGtkWidget;
  Requisition: TGtkRequisition;
begin
  Widget := PGtkWidget(AWinControl.Handle);
  // set size to default
  gtk_widget_set_usize(Widget,-1,-1);
  // ask default size
  gtk_widget_size_request(Widget,@Requisition);
  PreferredWidth:=Requisition.width;
  PreferredHeight:=Requisition.height;
  // set new size
  gtk_widget_set_usize(Widget,AWinControl.Width,AWinControl.Height);
  //debugln('GetGTKDefaultSize PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

Procedure ReportNotObsolete(const Texts : String);
Begin
  DebugLn('*********************************************');
  DebugLn('*********************************************');
  DebugLn('*************Non-Obsolete report*************');
  DebugLn('*********************************************');
  DebugLn('*************'+Texts+'*is being used yet.****');
  DebugLn('*******Please remove this function from******');
  DebugLn('*******the obsolete section in gtkproc.inc***');
  DebugLn('*********************************************');
  DebugLn('*********************************************');
  DebugLn('*********************************************');
  DebugLn('*********************************************');
end;

function TGDKColorToTColor(const value : TGDKColor) : TColor;
begin
  Result := ((Value.Blue shr 8) shl 16) + ((Value.Green shr 8) shl 8)
           + (Value.Red shr 8);
end;

function TColortoTGDKColor(const value : TColor) : TGDKColor;
var
  newColor : TGDKColor;
begin
  if Value<0 then begin
    FillChar(Result,SizeOf(Result),0);
    exit;
  end;

  newColor.pixel := 0;
  newColor.red   := (value and $ff) * 257;
  newColor.green := ((value shr 8) and $ff) * 257;
  newColor.blue  := ((value shr 16) and $ff) * 257;

  Result := newColor;
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
    Result:='LCL='+DbgS(TGDKColorToTColor(c))
             +' Pixel='+DbgS(c.Pixel)
             +' Red='+DbgS(c.Red)
             +' Green='+DbgS(c.Green)
             +' Blue='+DbgS(c.Blue)
             ;
  end;
{$ENDIF}
var
  MainStyle: PGtkStyle;
begin
  if Widget=nil then exit;
  {$IFDEF NoStyle}
  exit;
  {$ENDIF}
  //debugln('UpdateSysColorMap ',GetWidgetDebugReport(Widget));
  gtk_widget_set_rc_style(Widget);
  MainStyle:=gtk_widget_get_style(Widget);
  if MainStyle=nil then exit;
  with MainStyle^ do begin
  
    {$IFDEF VerboseUpdateSysColorMap}
    if rc_style<>nil then begin
      with rc_style^ do begin
        DebugLn('rc_style:');
        DebugLn(' FG GTK_STATE_NORMAL ',GdkColorAsString(fg[GTK_STATE_NORMAL]));
        DebugLn(' FG GTK_STATE_ACTIVE ',GdkColorAsString(fg[GTK_STATE_ACTIVE]));
        DebugLn(' FG GTK_STATE_PRELIGHT ',GdkColorAsString(fg[GTK_STATE_PRELIGHT]));
        DebugLn(' FG GTK_STATE_SELECTED ',GdkColorAsString(fg[GTK_STATE_SELECTED]));
        DebugLn(' FG GTK_STATE_INSENSITIVE ',GdkColorAsString(fg[GTK_STATE_INSENSITIVE]));
        DebugLn('');
        DebugLn(' BG GTK_STATE_NORMAL ',GdkColorAsString(bg[GTK_STATE_NORMAL]));
        DebugLn(' BG GTK_STATE_ACTIVE ',GdkColorAsString(bg[GTK_STATE_ACTIVE]));
        DebugLn(' BG GTK_STATE_PRELIGHT ',GdkColorAsString(bg[GTK_STATE_PRELIGHT]));
        DebugLn(' BG GTK_STATE_SELECTED ',GdkColorAsString(bg[GTK_STATE_SELECTED]));
        DebugLn(' BG GTK_STATE_INSENSITIVE ',GdkColorAsString(bg[GTK_STATE_INSENSITIVE]));
        DebugLn('');
        DebugLn(' TEXT GTK_STATE_NORMAL ',GdkColorAsString(text[GTK_STATE_NORMAL]));
        DebugLn(' TEXT GTK_STATE_ACTIVE ',GdkColorAsString(text[GTK_STATE_ACTIVE]));
        DebugLn(' TEXT GTK_STATE_PRELIGHT ',GdkColorAsString(text[GTK_STATE_PRELIGHT]));
        DebugLn(' TEXT GTK_STATE_SELECTED ',GdkColorAsString(text[GTK_STATE_SELECTED]));
        DebugLn(' TEXT GTK_STATE_INSENSITIVE ',GdkColorAsString(text[GTK_STATE_INSENSITIVE]));
        DebugLn('');
      end;
    end;

    DebugLn('MainStyle:');
    DebugLn(' FG GTK_STATE_NORMAL ',GdkColorAsString(fg[GTK_STATE_NORMAL]));
    DebugLn(' FG GTK_STATE_ACTIVE ',GdkColorAsString(fg[GTK_STATE_ACTIVE]));
    DebugLn(' FG GTK_STATE_PRELIGHT ',GdkColorAsString(fg[GTK_STATE_PRELIGHT]));
    DebugLn(' FG GTK_STATE_SELECTED ',GdkColorAsString(fg[GTK_STATE_SELECTED]));
    DebugLn(' FG GTK_STATE_INSENSITIVE ',GdkColorAsString(fg[GTK_STATE_INSENSITIVE]));
    DebugLn('');
    DebugLn(' BG GTK_STATE_NORMAL ',GdkColorAsString(bg[GTK_STATE_NORMAL]));
    DebugLn(' BG GTK_STATE_ACTIVE ',GdkColorAsString(bg[GTK_STATE_ACTIVE]));
    DebugLn(' BG GTK_STATE_PRELIGHT ',GdkColorAsString(bg[GTK_STATE_PRELIGHT]));
    DebugLn(' BG GTK_STATE_SELECTED ',GdkColorAsString(bg[GTK_STATE_SELECTED]));
    DebugLn(' BG GTK_STATE_INSENSITIVE ',GdkColorAsString(bg[GTK_STATE_INSENSITIVE]));
    DebugLn('');
    DebugLn(' TEXT GTK_STATE_NORMAL ',GdkColorAsString(text[GTK_STATE_NORMAL]));
    DebugLn(' TEXT GTK_STATE_ACTIVE ',GdkColorAsString(text[GTK_STATE_ACTIVE]));
    DebugLn(' TEXT GTK_STATE_PRELIGHT ',GdkColorAsString(text[GTK_STATE_PRELIGHT]));
    DebugLn(' TEXT GTK_STATE_SELECTED ',GdkColorAsString(text[GTK_STATE_SELECTED]));
    DebugLn(' TEXT GTK_STATE_INSENSITIVE ',GdkColorAsString(text[GTK_STATE_INSENSITIVE]));
    DebugLn('');
    DebugLn(' LIGHT GTK_STATE_NORMAL ',GdkColorAsString(light[GTK_STATE_NORMAL]));
    DebugLn(' LIGHT GTK_STATE_ACTIVE ',GdkColorAsString(light[GTK_STATE_ACTIVE]));
    DebugLn(' LIGHT GTK_STATE_PRELIGHT ',GdkColorAsString(light[GTK_STATE_PRELIGHT]));
    DebugLn(' LIGHT GTK_STATE_SELECTED ',GdkColorAsString(light[GTK_STATE_SELECTED]));
    DebugLn(' LIGHT GTK_STATE_INSENSITIVE ',GdkColorAsString(light[GTK_STATE_INSENSITIVE]));
    DebugLn('');
    DebugLn(' DARK GTK_STATE_NORMAL ',GdkColorAsString(dark[GTK_STATE_NORMAL]));
    DebugLn(' DARK GTK_STATE_ACTIVE ',GdkColorAsString(dark[GTK_STATE_ACTIVE]));
    DebugLn(' DARK GTK_STATE_PRELIGHT ',GdkColorAsString(dark[GTK_STATE_PRELIGHT]));
    DebugLn(' DARK GTK_STATE_SELECTED ',GdkColorAsString(dark[GTK_STATE_SELECTED]));
    DebugLn(' DARK GTK_STATE_INSENSITIVE ',GdkColorAsString(dark[GTK_STATE_INSENSITIVE]));
    DebugLn('');
    DebugLn(' MID GTK_STATE_NORMAL ',GdkColorAsString(mid[GTK_STATE_NORMAL]));
    DebugLn(' MID GTK_STATE_ACTIVE ',GdkColorAsString(mid[GTK_STATE_ACTIVE]));
    DebugLn(' MID GTK_STATE_PRELIGHT ',GdkColorAsString(mid[GTK_STATE_PRELIGHT]));
    DebugLn(' MID GTK_STATE_SELECTED ',GdkColorAsString(mid[GTK_STATE_SELECTED]));
    DebugLn(' MID GTK_STATE_INSENSITIVE ',GdkColorAsString(mid[GTK_STATE_INSENSITIVE]));
    DebugLn('');
    DebugLn(' BASE GTK_STATE_NORMAL ',GdkColorAsString(base[GTK_STATE_NORMAL]));
    DebugLn(' BASE GTK_STATE_ACTIVE ',GdkColorAsString(base[GTK_STATE_ACTIVE]));
    DebugLn(' BASE GTK_STATE_PRELIGHT ',GdkColorAsString(base[GTK_STATE_PRELIGHT]));
    DebugLn(' BASE GTK_STATE_SELECTED ',GdkColorAsString(base[GTK_STATE_SELECTED]));
    DebugLn(' BASE GTK_STATE_INSENSITIVE ',GdkColorAsString(base[GTK_STATE_INSENSITIVE]));
    DebugLn('');
    DebugLn(' BLACK ',GdkColorAsString(black));
    DebugLn(' WHITE ',GdkColorAsString(white));
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
    SysColorMap[COLOR_FORM] := TGDKColorToTColor(bg[GTK_STATE_NORMAL]);
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
function WaitForClipbrdAnswerDummyTimer(Client: Pointer): {$IFDEF Gtk2}gboolean{$ELSE}gint{$ENDIF}; cdecl;
begin
  if CLient=nil then ;
  Result:=GdkTrue; // go on, make sure getting a message at least every second
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
    result := c^.Data.Selection<>0;
  end;
  
begin
  Result:=false;
  {$IFDEF DEBUG_CLIPBOARD}
  DebugLn('[WaitForClipboardAnswer] A');
  {$ENDIF}
  if (ValidDateSelection) or (c^.Waiting) or (c^.Stopping) then begin
    //DebugLn('[WaitForClipboardAnswer] B');
    Result:=(ValidDateSelection);
    exit;
  end;
  c^.Waiting:=true;
  DateTimeToSystemTime(Time,StartTime);
  //DebugLn('[WaitForClipboardAnswer] C');
  Application.ProcessMessages;
  //DebugLn('[WaitForClipboardAnswer] D');
  if (ValidDateSelection) or (c^.Stopping) then begin
    //DebugLn('[WaitForClipboardAnswer] E  Yeah, Response received');
    Result:=(ValidDateSelection);
    exit;
  end;
  //DebugLn('[WaitForClipboardAnswer] F');
  // start a timer to make sure not waiting forever
  Timer := gtk_timeout_add(500, @WaitForClipbrdAnswerDummyTimer, nil);
  try
    repeat
      // just wait ...
      {$IFDEF DEBUG_CLIPBOARD}
      DebugLn('[WaitForClipboardAnswer] G');
      {$ENDIF}
      Application.HandleMessage;
      if (ValidDateSelection) or (c^.Stopping) then begin
        {$IFDEF DEBUG_CLIPBOARD}
        DebugLn('[WaitForClipboardAnswer] E  Yeah, Response received');
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
    DebugLn('[WaitForClipboardAnswer] H');
    {$ENDIF}
    // stop the timer
    gtk_timeout_remove(Timer);
//DebugLn('[WaitForClipboardAnswer] END');
  end;
  { $IFDEF DEBUG_CLIPBOARD}
  DebugLn('[WaitForClipboardAnswer] WARNING: no answer received in time');
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
  DebugLn('[RequestSelectionData] FormatID=',dbgs(FormatID));
  {$ENDIF}
  FillChar(Result,SizeOf(TGtkSelectionData),0);
  if (ClipboardWidget=nil) or (FormatID=0) 
  or (ClipboardTypeAtoms[ClipboardType]=0) then exit;
  
  TimeID:=1000;
  repeat
    repeat
      inc(TimeID);
      if TimeID>1010 then exit;
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
    DebugLn('[RequestSelectionData] TimeID=',dbgs(TimeID),' ',GdkAtomToStr(ClipboardTypeAtoms[ClipboardType]),' FormatID=',GdkAtomToStr(FormatID));
    {$ENDIF}
    if gtk_selection_convert(ClipboardWidget, ClipboardTypeAtoms[ClipboardType],
                             FormatID, TimeID)<>GdkFalse
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

{------------------------------------------------------------------------------
  function GdkAtomToStr(const Atom: TGdkAtom): string;
  
  Returns the associated string
 ------------------------------------------------------------------------------}
function GdkAtomToStr(const Atom: TGdkAtom): string;
var
  p: Pgchar;
begin
  p:=gdk_atom_name(Atom);
  Result:=p;
  g_free(p);
end;

{-------------------------------------------------------------------------------
  Function CreateFormContents(AForm: TCustomForm;
    var FormWidget: Pointer): Pointer;

  Creates the contents for the form (normally a hbox plus a client area.
  The hbox is needed for the menu.) The FormWidget is the main widget, for which
  the client area is associated. If FormWidget=nil then the hbox will be used
  as main widget.
-------------------------------------------------------------------------------}
Function CreateFormContents(AForm: TCustomForm;
  var FormWidget: Pointer): Pointer;
var
  ScrolledWidget, ClientAreaWidget: PGtkWidget;
  WindowStyle: PGtkStyle;
begin
  // Create the VBox. We need that to place controls outside
  // the client area (like menu)
  Result := gtk_vbox_new(False, 0);
  If FormWidget = nil then
    FormWidget := Result;

  // Create the form client area (a scrolled window with a gtklayout
  // with the style of a window)
  ScrolledWidget := gtk_scrolled_window_new(nil,nil);
  gtk_box_pack_end(Result, ScrolledWidget, True, True, 0);
  gtk_widget_show(ScrolledWidget);
  ClientAreaWidget := gtk_layout_new(nil, nil);
  WindowStyle:=GetStyle(lgsWindow);
  gtk_widget_set_style(ClientAreaWidget,WindowStyle);
  //debugln('CreateFormContents Style=',GetStyleDebugReport(WindowStyle));
  gtk_container_add(PGtkContainer(ScrolledWidget), ClientAreaWidget);

  gtk_object_set_data(FormWidget,odnScrollArea,ScrolledWidget);

  gtk_widget_show(ClientAreaWidget);
  SetFixedWidget(FormWidget, ClientAreaWidget);
  SetMainWidget(FormWidget, ClientAreaWidget);

  if ScrolledWidget<>nil then begin
    GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(ScrolledWidget)^.hscrollbar,
                           GTK_CAN_FOCUS);
    GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(ScrolledWidget)^.vscrollbar,
                           GTK_CAN_FOCUS);
    gtk_scrolled_window_set_policy(PGtkScrolledWindow(ScrolledWidget),
                                   GTK_POLICY_NEVER,GTK_POLICY_NEVER);
  end;
end;

function IndexOfStyle(aStyle: TLazGtkStyle): integer;
begin
  Result:=IndexOfStyleWithName(LazGtkStyleNames[aStyle]);
end;

{------------------------------------------------------------------------------
  Function: IndexOfWithNameStyle
  Params: WName
  Returns: Index of Style

  Returns the Index within the Styles property of WNAME
 ------------------------------------------------------------------------------}
function IndexOfStyleWithName(const WName : String): integer;
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
  
var
  StandardStyles: array[TLazGtkStyle] of PStyleObject;

Function NewStyleObject : PStyleObject;
begin
  New(Result);
  Result^.Widget := nil;
  Result^.Style := nil;
end;

Procedure FreeStyleObject(var StyleObject : PStyleObject);
// internal function to dispose a styleobject
// it does *not* remove it from the style lists
begin
  If StyleObject <> nil then begin
    If StyleObject^.Widget <> nil then begin
      // first unref
      gtk_widget_unref(StyleObject^.Widget);
      // then destroy
      GTK_Widget_Destroy(StyleObject^.Widget);
    end;
    If StyleObject^.Style <> nil then
      If StyleObject^.Style^.{$IFDEF Gtk2}attach_count{$ELSE}Ref_Count{$ENDIF}>0
      then
        GTK_Style_Unref(StyleObject^.Style);
    Dispose(StyleObject);
    StyleObject := nil;
  end;
end;

procedure ReleaseAllStyles;
var
  StyleObject: PStyleObject;
  lgs: TLazGtkStyle;
  i: Integer;
begin
  if Styles=nil then exit;
  for i:=Styles.Count-1 downto 0 do begin
    StyleObject:=PStyleObject(Styles.Objects[i]);
    FreeStyleObject(StyleObject);
  end;
  Styles.Clear;
  for lgs:=Low(TLazGtkStyle) to High(TLazGtkStyle) do
    StandardStyles[lgs]:=nil;
end;

procedure ReleaseStyle(aStyle: TLazGtkStyle);
var
  StyleObject: PStyleObject;
  l: Integer;
begin
  if Styles=nil then exit;
  if aStyle in [lgsUserDefined] then
    RaiseException('');// user styles are defined by name
  StyleObject:=StandardStyles[aStyle];
  if StyleObject<>nil then begin
    l:=IndexOfStyle(aStyle);
    Styles.Delete(l);
    StandardStyles[aStyle]:=nil;
    FreeStyleObject(StyleObject);
  end;
end;

Procedure ReleaseStyleWithName(const WName : String);
var
  l : Longint;
  s : PStyleObject;
begin
  if Styles=nil then exit;
  l := IndexOfStyleWithName(WName);
  If l >= 0 then begin
    If Styles.Objects[l] <> nil then
      Try
        s := PStyleObject(Styles.Objects[l]);
        FreeStyleObject(S);
      Except
        DebugLn('[ReleaseStyle] : Unable To Unreference Style');
      end;
    Styles.Delete(l);
  end;
end;

function GetStyle(aStyle: TLazGtkStyle): PGTKStyle;
begin
  if Styles=nil then exit(nil);
  if aStyle in [lgsUserDefined] then
    RaiseException('');// user styles are defined by name
  if StandardStyles[aStyle]<>nil then
    // already created
    Result:=StandardStyles[aStyle]^.Style
  else
    // create it
    Result:=GetStyleWithName(LazGtkStyleNames[aStyle]);
end;

{------------------------------------------------------------------------------
  Function: GetStyleWithName
  Params: none
  Returns: Returns a Corresponding Style

  Tries to get the Style corresponding to the Widget Name passed, aka 'button',
  'default', checkbox', etc. for use within such routines as DrawFrameControl
  to attempt to supply theme dependent drawing. Styles are stored in a TStrings
  list which is only updated on theme change, to ensure fast efficient retrieval
  of Styles.
 ------------------------------------------------------------------------------}
function GetStyleWithName(const WName: String) : PGTKStyle;

  function CreateStyleNotebook: PGTKWidget;
  var
    NoteBookWidget: PGtkNotebook;
    //NoteBookPageWidget: PGtkWidget;
    NoteBookPageClientAreaWidget: PGtkWidget;
    NoteBookTabLabel: PGtkWidget;
    NoteBookTabMenuLabel: PGtkWidget;
  begin
    Result:=gtk_notebook_new;
    NoteBookWidget := PGtkNoteBook(Result);
    //NoteBookPageWidget := gtk_hbox_new(false, 0);
    NoteBookPageClientAreaWidget := gtk_fixed_new;
    gtk_widget_show(NoteBookPageClientAreaWidget);
    //gtk_container_add(GTK_CONTAINER(NoteBookPageWidget),
    //                  NoteBookPageClientAreaWidget);
    //gtk_widget_show(NoteBookPageWidget);
    NoteBookTabLabel:=gtk_label_new('Lazarus');
    gtk_widget_show(NoteBookTabLabel);
    NoteBookTabMenuLabel:=gtk_label_new('Lazarus');
    gtk_widget_show(NoteBookTabMenuLabel);
    gtk_notebook_append_page_menu(NoteBookWidget,NoteBookPageClientAreaWidget,
                                  NoteBookTabLabel,NoteBookTabMenuLabel);
    gtk_widget_set_usize(Result,200,200);
  end;

var
  Tp : Pointer;
  l : Longint;
  StyleObject : PStyleObject;
  NoName: PGChar;
  lgs: TLazGtkStyle;
  WidgetName: String;
  //VBox: PGtkWidget;
  AddToStyleWindow: Boolean;
  StyleWindowWidget: PGtkWidget;
  Requisition: TGtkRequisition;
  WindowFixedWidget: PGtkWidget;
begin
  Result := nil;
  if Styles=nil then exit;
  {$IFDEF NoStyle}
  exit;
  {$ENDIF}
  
  If (WName='') then exit;
  l:=IndexOfStyleWithName(WName);
  //DebugLn('GetStyleWithName START ',WName,' ',l);

  If l >= 0 then begin
    StyleObject:=PStyleObject(Styles.Objects[l]);
    Result := StyleObject^.Style;

  end else begin
    // create a new style object
    StyleObject := NewStyleObject;
    lgs:=lgsUserDefined;
    Tp:=nil;
    AddToStyleWindow:=true;
    // create a style widget
    If AnsiCompareText(WName,LazGtkStyleNames[lgsButton])=0 then begin
        StyleObject^.Widget := GTK_BUTTON_NEW;
        lgs:=lgsButton;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsLabel])=0 then begin
        StyleObject^.Widget := GTK_LABEL_NEW('StyleLabel');
        lgs:=lgsLabel;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsDefault])=0 then begin
        lgs:=lgsDefault;
        AddToStyleWindow:=false;
        NoName:=nil;
        StyleObject^.Widget :=
          // GTK2 does not allow to instantiate the abstract base Widget
          // so we use the "invisible" widget, which should never be defined
          // by the theme
          GTK_WIDGET_NEW(
            {$IFDEF Gtk2}GTK_TYPE_INVISIBLE{$ELSE}GTK_WIDGET_TYPE{$ENDIF},
            NoName,[]);
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsWindow])=0 then begin
        lgs:=lgsWindow;
        StyleObject^.Widget := GTK_WINDOW_NEW(GTK_WINDOW_TOPLEVEL);
        AddToStyleWindow:=false;
        gtk_widget_hide(StyleObject^.Widget);
        // create the fixed widget
        // (where to put all style widgets, that need a parent for realize)
        //VBox:=gtk_vbox_new(false,0);
        //gtk_widget_show(VBox);
        //gtk_container_add(PGtkContainer(StyleObject^.Widget), VBox);
        //gtk_object_set_data(PGtkObject(StyleObject^.Widget),'vbox',VBox);
        WindowFixedWidget:=gtk_fixed_new;
        gtk_widget_show(WindowFixedWidget);
        gtk_container_add(PGtkContainer(StyleObject^.Widget), WindowFixedWidget);
        gtk_object_set_data(PGtkObject(StyleObject^.Widget),'fixedwidget',WindowFixedWidget);
        gtk_widget_realize(StyleObject^.Widget);
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsCheckbox])=0 then begin
        lgs:=lgsCheckbox;
        StyleObject^.Widget := GTK_CHECK_BUTTON_NEW;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsRadiobutton])=0 then begin
        lgs:=lgsRadiobutton;
        StyleObject^.Widget := GTK_RADIO_BUTTON_NEW(nil);
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsMenu])=0 then begin
        lgs:=lgsMenu;
        AddToStyleWindow:=false;
        StyleObject^.Widget := GTK_MENU_NEW;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsMenuitem])=0 then begin
        lgs:=lgsMenuitem;
        AddToStyleWindow:=false;
        StyleObject^.Widget := GTK_MENU_ITEM_NEW;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsList])=0 then begin
        lgs:=lgsList;
        StyleObject^.Widget := GTK_LIST_NEW;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsVerticalScrollbar])=0 then begin
        lgs:=lgsVerticalScrollbar;
        StyleObject^.Widget := gtk_vscrollbar_new(nil);
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsHorizontalScrollbar])=0 then begin
        lgs:=lgsHorizontalScrollbar;
        StyleObject^.Widget := gtk_hscrollbar_new(nil);
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsVerticalPaned])=0 then begin
        lgs:=lgsVerticalPaned;
        StyleObject^.Widget := gtk_vpaned_new;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsHorizontalPaned])=0 then begin
        lgs:=lgsHorizontalPaned;
        StyleObject^.Widget := gtk_hpaned_new;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsNotebook])=0 then begin
        lgs:=lgsNotebook;
        StyleObject^.Widget := CreateStyleNotebook;
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsTooltip])=0 then begin
        lgs:=lgsTooltip;
        AddToStyleWindow:=false;
        TP := gtk_tooltips_new;
        StyleObject^.Widget := nil;
        GTK_Tooltips_Force_Window(TP);
        gtk_widget_ensure_style(PGTKTooltips(TP)^.Tip_Window);
        StyleObject^.Style:=gtk_widget_get_style(PGTKTooltips(TP)^.Tip_Window);
      end
    else
      If AnsiCompareText(WName,LazGtkStyleNames[lgsGTK_Default])=0 then begin
        lgs:=lgsGTK_Default;
        AddToStyleWindow:=false;
        StyleObject^.Widget := nil;
        StyleObject^.Style := gtk_style_new;
      end
    else begin
      // unknown style name -> bug
      FreeStyleObject(StyleObject);
      AddToStyleWindow:=false;
      RaiseException('');
    end;
    
    if (lgs<>lgsUserDefined) and (StandardStyles[lgs]<>nil) then begin
      // consistency error
      RaiseException('');
    end;
    
    // ensure style of the widget
    If (StyleObject^.Widget <> nil) then begin
      gtk_widget_ref(StyleObject^.Widget);

      // put style widget on style window, so that it can be realized
      if AddToStyleWindow then begin
        gtk_widget_show_all(StyleObject^.Widget);
        StyleWindowWidget:=GetStyleWidget(lgsWindow);
        WindowFixedWidget:=PGTKWidget(
               gtk_object_get_data(PGtkObject(StyleWindowWidget),'fixedwidget'));
        //DebugLn('AddToStyleWindow A ',GetWidgetDebugReport(StyleObject^.Widget));
        //gtk_box_pack_end(PGTKBox(VBox), WindowFixedWidget, True, True, 0);
        gtk_fixed_put(PGtkFixed(WindowFixedWidget),StyleObject^.Widget,0,0);
        gtk_widget_set_usize(StyleObject^.Widget,200,200);
      end;

      WidgetName:='LazStyle'+WName;
      gtk_widget_set_name(StyleObject^.Widget,PChar(WidgetName));
      gtk_widget_ensure_style(StyleObject^.Widget);
      gtk_widget_size_request(StyleObject^.Widget, @Requisition);
      StyleObject^.Style:=gtk_widget_get_style(StyleObject^.Widget);
      // ToDo: find out, why sometimes the style is not initialized.
      // for example: why the following occurs:
      If AnsiCompareText(WName,'button')=0 then begin
        if StyleObject^.Style^.light_gc[GTK_STATE_NORMAL]=nil then begin
          //DebugLn('GetStyleWithName ',WName);
          if not GtkWidgetIsA(StyleObject^.Widget,GTK_WINDOW_GET_TYPE) then begin
            gtk_widget_realize(StyleObject^.Widget);
          end;
        end;
      end;
    end;
    
    // increase refcount of style
    If StyleObject^.Style <> nil then
      If AnsiCompareText(WName,LazGtkStyleNames[lgsGTK_Default])<>0 then
        StyleObject^.Style:=GTK_Style_Ref(StyleObject^.Style);
        
    // if successful add to style objects list
    if StyleObject^.Style <> nil then begin
      Styles.AddObject(WName, TObject(StyleObject));
      if lgs<>lgsUserDefined then
        StandardStyles[lgs]:=StyleObject;
      Result:=StyleObject^.Style;
      If (StyleObject^.Widget <> nil)
      and (AnsiCompareText(WName,LazGtkStyleNames[lgsWindow])=0) then
        UpdateSysColorMap(StyleObject^.Widget);
        
      // ToDo: create all gc of the style
      //gtk_widget_set_rc_style(StyleObject^.Widget);
    end
    else begin
      // no success, clean up
      FreeStyleObject(StyleObject);
      DebugLn('WARNING: GetStyleWithName ',WName,' failed');
    end;
    
    // clean up
    If Tp<>nil then
      GTK_Object_Destroy(Tp);
      
  end;
end;

function GetStyleWidget(aStyle: TLazGtkStyle) : PGTKWidget;
begin
  if aStyle in [lgsUserDefined] then
    RaiseException('');// user styles are defined by name
  if StandardStyles[aStyle]<>nil then
    // already created
    Result:=StandardStyles[aStyle]^.Widget
  else
    // create it
    Result:=GetStyleWidgetWithName(LazGtkStyleNames[aStyle]);
end;

Function GetStyleWidgetWithName(const WName : String) : PGTKWidget;
var
  l : Longint;
begin
  Result := nil;
  // init style
  GetStyleWithName(WName);
  // return widget
  l:=IndexOfStyleWithName(WName);
  If l>=0 then
    Result := PStyleObject(Styles.Objects[l])^.Widget;
end;

{------------------------------------------------------------------------------
  Function: LoadDefaultFont(Desc)
  Params: none
  Returns: Returns the default Font(or Pango Font Description if using PANGO)

  For Text/Font Routines: if the Font is invalid, this can be used instead, or
  if the DT_internal flag is used(aka use system font) this is used. This is
  also the font returned by GetStockObject(SYSTEM_FONT).

  It attempts to get the font from the default Style, or if none is available,
  a new style(aka try and get GTK builtin values), if that fails tries to get
  a generic fixed font, if THAT fails, it gets whatever font is available.
  If the result is not nil it MUST be GDK_FONT_UNREF'd when done.
 ------------------------------------------------------------------------------}
{$Ifdef GTK2}
function LoadDefaultFontDesc: PPangoFontDescription;
var
  Style : PGTKStyle;
begin
  Result := nil;
  Style := GetStyle(lgsDefault);
  if Style = nil then
    Style := GetStyle(lgsGTK_Default);

  If (Style <> nil) then begin
     Result := pango_font_description_copy(Style^.font_desc);
  end;

  If Result = nil then
    Result := pango_font_description_from_string('sans 12');
    
  if Result = nil then
    Result := pango_font_description_from_string('12');
end;
{$Else}
function LoadDefaultFont: PGDKFont;
var
  Style : PGTKStyle;
begin
  Result := nil;
  Style := GetStyle(lgsDefault);
  if Style = nil then
    Style := GetStyle(lgsGTK_Default);

  If Style <> nil then begin
    Result := Style^.Font;
    If Result = nil then
      {$IFNDEF NoStyle}
      If (Style^.RC_Style <> nil) then begin
        if (Style^.RC_Style^.font_name <> nil) then
          Result := gdk_font_load(Style^.RC_Style^.font_name);
      end;
      {$ENDIF}
  end;

  If Result = nil then
    Result := gdk_fontset_load('-*-fixed-*-*-*-*-*-120-*-*-*-*-*-*');
  if Result = nil then
    Result := gdk_fontset_load('-*-*-*-*-*-*-*-*-*-*-*-*-*-*');

  If Result <> nil then
    Result := gdk_font_ref(Result);
end;
{$EndIf}

function GetDefaultFontName: string;
var
  Style: PGtkStyle;
  {$IFDEF GTK2}
  PangoFontDesc: PPangoFontDescription;
  {$ENDIF}
begin
  Result:='';
  Style := GetStyle(lgsDefault);
  if Style = nil then
    Style := GetStyle(lgsGTK_Default);

  If Style <> nil then begin
    {$IFDEF GTK1}
      {$IFNDEF NoStyle}
      If (Style^.RC_Style <> nil) then begin
        if (Style^.RC_Style^.font_name <> nil) then
          Result := Style^.RC_Style^.font_name;
      end;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF GTK2}
    If (Style <> nil) then begin
      PangoFontDesc := pango_font_description_copy(Style^.font_desc);
      if PangoFontDesc<>nil then begin
        Result:=pango_font_description_get_family(PangoFontDesc);
      end;
    end;
    {$ENDIF}
  end;
end;

procedure RealizeGDKColor(ColorMap: PGdkColormap; Color: PGDKColor);
var
  AllocResult: gboolean;
begin
  if ColorMap=nil then ColorMap:=gdk_colormap_get_system;
  if (Color^.pixel = 0)
  and ((Color^.red<>0) or (Color^.blue<>0) or (Color^.green<>0)) then
    gdk_colormap_alloc_colors(ColorMap, Color, 1, false, true, @AllocResult)
  else
    gdk_colormap_query_color(ColorMap,Color^.pixel, Color);
end;

procedure RealizeGtkStyleColor(Style: PGTKStyle; Color: PGDKColor);
begin
  if (Style<>nil) then
    RealizeGDKColor(Style^.ColorMap,Color)
  else
    RealizeGDKColor(nil,Color);
end;

Function GetSysGCValues(Color: TColorRef;
  ThemeWidget: PGtkWidget): TGDKGCValues;
// ThemeWidget can be nil

  function GetWidgetWithBackgroundWindow(Widget: PGtkWidget): PGtkWidget;
  // returns the gtk widget which has the background gdk window
  var
    WindowOwnerWidget: PGtkWidget;
  begin
    Result:=Widget;
    if Result=nil then exit;
    if Result^.window=nil then exit;
    gdk_window_get_user_data(Result^.window,@WindowOwnerWidget);
    Result:=WindowOwnerWidget;
    if Result=nil then exit;
  end;

var
  Style: PGTKStyle;
  GC: PGDKGC;
  Pixmap: PGDKPixmap;
  SysColor: TColorRef;
  BaseColor: TColorRef;
  Red, Green, Blue: byte;
begin
  BaseColor := Color and $FF;

  {Set defaults in case something goes wrong}
  FillChar(Result, SizeOf(Result), 0);
  Style:=nil;
  GC:=nil;
  Pixmap:=nil;

  SysColor := ColorToRGB(BaseColor);
  Result.Fill := GDK_Solid;
  RedGreenBlue(TColor(SysColor),Red,Green,Blue);
  Result.foreground.Red:=gushort(Red) shl 8+Red;
  Result.foreground.Green:=gushort(Green) shl 8+Green;
  Result.foreground.Blue:=gushort(Blue) shl 8+Blue;

  {$IfDef Disable_GC_SysColors}
  exit;
  {$EndIf}
  Case BaseColor of
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
        Style := GetStyle(lgsTooltip);

        If Style = nil then
          Style := GetStyle(lgsWindow);

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
        Style := GetStyle(lgsTooltip);

        If Style = nil then
          Style := GetStyle(lgsWindow);

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

    COLOR_FORM,
    COLOR_MENU,
    COLOR_SCROLLBAR,
    COLOR_BTNFACE :
      begin
        Case BaseColor of
          COLOR_FORM: Style := GetStyle(lgsWindow);
          COLOR_BTNFACE: Style := GetStyle(lgsButton);
          COLOR_MENU: Style := GetStyle(lgsMenu);
          COLOR_SCROLLBAR: Style := GetStyle(lgsHorizontalScrollbar);
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
            Result.foreground := Style^.bg[GTK_STATE_NORMAL];
          end
          else
            GDK_GC_Get_Values(GC, @Result);
        end;
      end;

    COLOR_3DDKSHADOW,
    COLOR_BTNSHADOW :
      begin
        Style := GetStyle(lgsButton);
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
        Style := GetStyle(lgsDefault);
        If Style = nil then
          exit;
        GC := Style^.text_gc[GTK_STATE_INSENSITIVE];
        if GC=nil then begin
          Result.Fill := GDK_Solid;
          Result.foreground := Style^.text[GTK_STATE_NORMAL];
        end else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_MENUTEXT,
    COLOR_BTNTEXT :
      begin
        Case BaseColor of
          COLOR_BTNTEXT : Style := GetStyle(lgsButton);
          COLOR_MENUTEXT : Style := GetStyle(lgsMenuitem);
        end;
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

    COLOR_WINDOWTEXT:
      begin
        Style := GetStyle(lgsDefault);
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
        Style := GetStyle(lgsButton);
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
        ThemeWidget:=GetWidgetWithBackgroundWindow(ThemeWidget);
        if ThemeWidget<>nil then begin
          if GtkWidgetIsA(ThemeWidget,GTK_TYPE_LIST_ITEM) then
            Style:=GetStyle(lgsList);
          if Style=nil then
            Style:=PGtkStyle(gtk_widget_get_style(ThemeWidget));
        end;
        if Style=nil then
          Style := GetStyle(lgsDefault);
        If Style = nil then
          exit;
        GC := Style^.base_gc[GTK_STATE_NORMAL];
        If (GC = nil) then begin
          Result.Fill := GDK_Solid;
          if Style^.base[GTK_STATE_NORMAL].Pixel<>0 then begin
            Result.foreground := Style^.base[GTK_STATE_NORMAL];
            Result.background := Style^.base[GTK_STATE_NORMAL];
          end;
        end
        else
          GDK_GC_Get_Values(GC, @Result);
      end;

    COLOR_HIGHLIGHT :
      begin
        Style := GetStyle(lgsDefault);
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
        Style := GetStyle(lgsDefault);
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

  RealizeGtkStyleColor(Style,@Result.foreground);
end;

Function StyleForegroundColor(Color: TColorRef;
  DefaultColor: PGDKColor): PGDKColor;
var
  style : PGTKStyle;
begin
  style := nil;
  Result := DefaultColor;

  Case TColor(Color) of
    clINFOTEXT :
      begin
        Style := GetStyle(lgsTooltip);

        If Style = nil then
          exit;

        Result := @Style^.fg[GTK_STATE_NORMAL];
      end;

    cl3DDKSHADOW,
    clBTNSHADOW :
      begin
        Style := GetStyle(lgsButton);
        If Style = nil then
          exit;
        Result := @Style^.dark[GTK_STATE_NORMAL];
      end;

    clGRAYTEXT :
      begin
        Style := GetStyle(lgsDefault);
        If Style = nil then
          exit;
        Result := @Style^.text[GTK_STATE_INSENSITIVE];
      end;

    clMENUTEXT,
    clBTNTEXT :
      begin
        Case TColor(Color) of
          clBTNTEXT : Style := GetStyle(lgsButton);
          clMENUTEXT : Style := GetStyle(lgsMenuitem);
        end;
        If Style = nil then
          exit;
        Result := @Style^.fg[GTK_STATE_NORMAL];
      end;

    clWINDOWTEXT:
      begin
        Style := GetStyle(lgsDefault);
        If Style = nil then
          exit;
        Result := @Style^.text[GTK_STATE_NORMAL];
      end;

    cl3DLIGHT,
    clBTNHIGHLIGHT :
      begin
        Style := GetStyle(lgsButton);
        If Style = nil then
          exit;
        Result := @Style^.light[GTK_STATE_NORMAL];
      end;

    clHIGHLIGHTTEXT :
      begin
        Style := GetStyle(lgsDefault);
        If Style = nil then
          exit;
        Result := @Style^.bg[GTK_STATE_PRELIGHT];
      end;
  end;

  If Result = nil then
    Result := DefaultColor;

  if (Result <> nil) and (Result <> DefaultColor) then
    RealizeGtkStyleColor(Style,Result);
end;

Procedure StyleFillRectangle(drawable : PGDKDrawable; GC : PGDKGC; Color : TColorRef; x, y, width, height : gint);
var
  style : PGTKStyle;
  widget : PGTKWidget;
  state : TGTKStateType;
  detail : pgchar;
begin
  style := nil;
  
  Case TColor(Color) of
   { clMenu:
      begin
        Style := GetStyle('menuitem');
        widget := GetStyleWidget('menuitem');
        state := GTK_STATE_NORMAL;
        detail := 'menuitem';
      end;

    clBtnFace :
      begin
        Style := GetStyle('button');
        widget := GetStyleWidget('button');
        state := GTK_STATE_NORMAL;
        detail := 'button';
      end;

    clWindow :
      begin
        Style := GetStyle('default');
        widget := GetStyleWidget('default');
        state := GTK_STATE_NORMAL;
        detail := 'list';
      end;   }

    clInfoBk :
      begin
        Style := GetStyle(lgsWindow);
        widget := GetStyleWidget(lgsWindow);
 //       Style := GetStyle('tooltip');
        state := GTK_STATE_NORMAL;
        detail := 'tooltip';
      end;

    clForm :
      begin
        Style := GetStyle(lgsWindow);
        widget := GetStyleWidget(lgsWindow);
        state := GTK_STATE_NORMAL;
        detail := 'window';
      end;
  end;

  if Assigned(Style) then
    gtk_paint_flat_box(style, drawable, state, GTK_SHADOW_NONE, nil, widget,
                       detail, x, y, width, height)
  else
    gdk_draw_rectangle(drawable, GC, 1, x, y, width, height);
end;

procedure UpdateWidgetStyleOfControl(AWinControl: TWinControl);
var
  RCStyle : PGtkRCStyle;
  Widget, FixWidget : PGTKWidget;
  NewColor: TGdkColor;
  MainWidget: PGtkWidget;
  FontHandle: HFONT;
  FreeFontName: boolean;
  FreeFontSetName: boolean;

  procedure CreateRCStyle;
  begin
    if RCStyle=nil then
      RCStyle:=gtk_rc_style_new;
  end;
  
  procedure SetRCFont(FontGdiObject: PGdiObject);
  {$IFDEF GTK1}
  var
    FontDesc: TGdkFontCacheDescriptor;
  {$ENDIF}
  begin
    {$IFDEF GTK1}
    CreateRCStyle;
    FontDesc:=FontCache.FindADescriptor(FontGdiObject^.GDIFontObject);
    if (FontDesc<>nil) and (FontDesc.xlfd<>'') then begin
      RCStyle:=gtk_rc_style_new;
      g_free(RCStyle^.font_name);
      RCStyle^.font_name:=g_strdup(PChar(FontDesc.xlfd));
      g_free(RCStyle^.fontset_name);
      RCStyle^.fontset_name:=g_strdup(PChar(FontDesc.xlfd));

      //DebugLn('UpdateWidgetStyleOfControl.SetRCFont ',DbgSName(AWinControl),' Widget=',GetWidgetDebugReport(Widget),' Style=',GetWidgetStyleReport(Widget));
    end;
    {$ENDIF}
  end;

begin
  {$IFDEF NoStyle}
  exit;
  {$ENDIF}

  if not AWinControl.HandleAllocated then exit;

  MainWidget:=PGtkWidget(AWinControl.Handle);
  FixWidget:=GetFixedWidget(MainWidget);
  If (FixWidget <> nil) and (FixWidget<>MainWidget) then
    Widget := FixWidget
  else begin
    Widget := MainWidget;
  end;

  if not GTK_WIDGET_REALIZED(Widget) then exit;
  //debugln('UpdateWidgetStyleOfControl ',GetWidgetDebugReport(Widget));

  RCStyle:=nil;
  FreeFontName:=false;
  FreeFontSetName:=false;
  try
    // set default background
    if (AWinControl.Color=clNone) then begin
      // clNone => remove default background
      if (FixWidget<>nil) and (FixWidget^.Window<>nil) then begin
        gdk_window_set_back_pixmap(FixWidget^.Window,nil,GdkFalse);
      end;
    end
    else
    if AWinControl.ColorIsStored
    and ((AWinControl.Color and SYS_COLOR_BASE)=0) then begin
      // set background to user defined color

      // don't set background for custom controls, which paint themselves
      // (this prevents flickering)
      if (csOpaque in AWinControl.ControlStyle)
      and GtkWidgetIsA(MainWidget,GTKAPIWidget_GetType) then exit;

      NewColor:=TColorToTGDKColor(AWinControl.Color);

      CreateRCStyle;
      RCStyle^.bg[GTK_STATE_NORMAL]:=NewColor;

      // Indicate which colors the GtkRcStyle will affect;
      // unflagged colors will follow the theme
      RCStyle^.color_flags[GTK_STATE_NORMAL]:=
        RCStyle^.color_flags[GTK_STATE_NORMAL] or GTK_RC_BG;

      {for i:=0 to 4 do begin
        RCStyle^.bg[i]:=NewColor;

        // Indicate which colors the GtkRcStyle will affect;
        // unflagged colors will follow the theme
        RCStyle^.color_flags[i]:=
          RCStyle^.color_flags[i] or GTK_RC_BG;
      end;}
      
      //DebugLn('UpdateWidgetStyleOfControl ',DbgSName(AWinControl),' Color=',DbgS(AWinControl.Color));
    end;
    
    {if (AWinControl is TCustomForm) then begin
      gdk_window_set_back_pixmap(FixWidget^.Window,nil,GdkFalse);

      NewColor:=TColorToTGDKColor(clRed);

      CreateRCStyle;
      for i:=0 to 4 do begin
        debugln('UpdateWidgetStyleOfControl i=',dbgs(i),' ',RCStyle^.bg_pixmap_name[i],' ',RCStyle^.Name);
        RCStyle^.bg[i]:=NewColor;

        // Indicate which colors the GtkRcStyle will affect;
        // unflagged colors will follow the theme
        RCStyle^.color_flags[i]:=
          RCStyle^.color_flags[i] or GTK_RC_BG;
      end;
    end;}
    
    // set font color
    if (AWinControl.Font.Color and SYS_COLOR_BASE)=0 then begin
      //NewColor:=TColorToTGDKColor(AWinControl.Font.Color);
      NewColor:=AllocGDKColor(AWinControl.Font.Color);
      //debugln('UpdateWidgetStyleOfControl New Font Color=',dbgs(NewColor.Pixel),' ',dbgs(NewColor.Red),' ',dbgs(NewColor.Green),' ',dbgs(NewColor.Blue));
      CreateRCStyle;

      {for i:=0 to 4 do begin
        RCStyle^.text[i]:=NewColor;
        RCStyle^.fg[i]:=NewColor;
        RCStyle^.bg[i]:=NewColor;
        RCStyle^.base[i]:=NewColor;
        RCStyle^.color_flags[i]:=
          RCStyle^.color_flags[i] or 15;
      end;}

      RCStyle^.text[GTK_STATE_NORMAL]:=NewColor;

      // Indicate which colors the GtkRcStyle will affect;
      // unflagged colors will follow the theme
      RCStyle^.color_flags[GTK_STATE_NORMAL]:=
        RCStyle^.color_flags[GTK_STATE_NORMAL] or GTK_RC_TEXT;

      //DebugLn('UpdateWidgetStyleOfControl Font Color ',DbgSName(AWinControl),' Color=',DbgS(AWinControl.Font.Color));
    end;

    // set font (currently only TCustomLabel)
    if GtkWidgetIsA(Widget,gtk_label_get_type)
    or GtkWidgetIsA(Widget,gtk_editable_get_type)
    and ((AWinControl.Font.Name<>DefFontData.Name)
      or (AWinControl.Font.Size<>0)
      or (AWinControl.Font.Style<>[]))
    then begin
       // allocate font
       FontHandle:=AWinControl.Font.Handle;
       if FontHandle<>0 then
         SetRCFont(PGdiObject(FontHandle));
    end;
    
  finally
    if RCStyle<>nil then begin
      //DebugLn('UpdateWidgetStyleOfControl Apply Modifications ',AWinControl.Name,' ',GetWidgetClassName(Widget));
      gtk_widget_modify_style(Widget,RCStyle);

      if FreeFontName then begin
        {$ifdef gtk1}
        g_free(RCStyle^.font_name);
        RCStyle^.font_name:=nil;
        {$else}
        pango_font_description_free(RCStyle^.font_desc);
        RCStyle^.font_desc:=nil;
        {$endif}
      end;
      if FreeFontSetName then begin
        {$ifdef gtk1}
        g_free(RCStyle^.fontset_name);
        RCStyle^.fontset_name:=nil;
        {$endif}
      end;
      //DebugLn('UpdateWidgetStyleOfControl END ',DbgSName(AWinControl),' Widget=',GetWidgetDebugReport(Widget),' Style=',GetWidgetStyleReport(Widget));
      gtk_rc_style_unref(RCStyle);
    end;
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
  function Ampersands2Underscore(const ASource: String): String;

  Deletes escaping ampersands, replaces the first single
  ampersand with an underscore and deleting all other single ampersands.
-------------------------------------------------------------------------------}
function Ampersands2Underscore(const ASource: String): String;
var
  n: Integer;
  FirstFound: Boolean;
begin
  //TODO: escape underscores
  FirstFound := False;
  Result := ASource;
  n := 1;
  while n <= Length(Result) do
  begin
    if Result[n] = '&'
    then begin
      if (n < Length(Result))
      and (Result[n + 1] = '&')
      then begin
        // we got a &&, remove the first
        Delete(Result, n, 1);
        Inc(n);
        Continue;
      end;
      if FirstFound
      then begin
        // simply remove it
        Delete(Result, n, 1);
        Continue;
      end;
      // if we are here it's our first
      FirstFound := True;
      Result[n] := '_';
    end;
    Inc(n);
  end;
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
  function RemoveAmpersands(const ASource: String): String;

  Removing all escaping ampersands.
-------------------------------------------------------------------------------}
function RemoveAmpersands(const ASource: String): String;
var
  n: Integer;
begin
  Result := ASource;
  n := 1;
  while n <= Length(Result) do
  begin
    if Result[n] = '&'
    then begin
      if (n < Length(Result))
      and (Result[n + 1] = '&')
      then begin
        // we got a &&, remove the first
        Delete(Result, n, 1);
        Inc(n);
        Continue;
      end;
      // simply remove it
      Delete(Result, n, 1);
      Continue;
    end;
    Inc(n);
  end;
end;

{-------------------------------------------------------------------------------
  procedure LabelFromAmpersands(var AText, APattern: String; var AAccelChar: Char)

  Removes all escaping ampersands, creates an underscore pattern and returns
  the first ampersand char as accelerator char
-------------------------------------------------------------------------------}
procedure LabelFromAmpersands(var AText, APattern: String; var AAccelChar: Char);
var
  n: Integer;
  FirstFound: Boolean;
begin
  //TODO: escape underscores
  FirstFound := False;
  APattern := StringOfChar(' ', Length(AText));
  AAccelChar := #0;
  n := 1;
  while n <= Length(AText) do
  begin
    case AText[n] of
      '&': begin
        if (n < Length(AText))
        and (AText[n + 1] = '&')
        then begin
          // we got a &&, remove the first
          Delete(AText, n, 1);
          Delete(APattern, n, 1);
          Inc(n);
          Continue;
        end;

        Delete(AText, n, 1);
        Delete(APattern, n, 1);
        if FirstFound
        then Continue; // simply remove it

        // if we are here it's our first
        FirstFound := True;
        AAccelChar := System.lowerCase(AText[n]);
        // is there a next char we can underline ?
        if n <= Length(APattern)
        then APattern[n] := '_';
      end;
      '_': begin
        AText[n] := ' ';
        APattern[n] := '_';
      end;
    end;
    Inc(n);
  end;
end;


{-------------------------------------------------------------------------------
  Function GetTextExtentIgnoringAmpersands(Font : PGDKFont; Str : PChar;
                    LineLength : Longint; lbearing, rbearing, width, ascent, descent : Pgint);

  Gets text extent of a string, ignoring escaped Ampersands.
-------------------------------------------------------------------------------}
{$Ifdef GTK2}
Procedure GetTextExtentIgnoringAmpersands(FontDesc : PPangoFontDescription; Str : PChar;
  LineLength : Longint; lbearing, rbearing, width, ascent, descent : Pgint);
{$Else}
Procedure GetTextExtentIgnoringAmpersands(Font : PGDKFont; Str : PChar;
  LineLength : Longint; lbearing, rbearing, width, ascent, descent : Pgint);
{$EndIf}
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
{$Ifdef GTK2}
  gdk_text_extents(FontDesc, NewStr, LineLength,
                   lbearing, rBearing, width, ascent, descent);
{$Else}
  gdk_text_extents(Font, NewStr, LineLength,
                   lbearing, rBearing, width, ascent, descent);
{$EndIf}
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
  DoubleCharLen:=gdk_text_width(TheFont, #0'A', 2);
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
Function GDKPixel2GDIRGB(Pixel: Longint; Visual: PGDKVisual;
  Colormap: PGDKColormap) : TGDIRGB;
var
  Color: TGDKColor;
begin
  FillChar(Result, SizeOf(TGDIRGB),0);

  If (Visual = nil) or (Colormap = nil) then begin
    Visual := GDK_Visual_Get_System;
    Colormap := GDK_Colormap_Get_System;
  end;
  
  gdk_colormap_query_color(colormap, pixel, @color);

  Result.Red := Color.Red shr 8;
  Result.Green := Color.Green shr 8;
  Result.Blue := Color.Blue shr 8;
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
  {$IFDEF GTK1}
  {$IFDEF UNIX}
  theFonts : PPChar;
  {$ENDIF UNIX}
  {$Else}
  Widget : PGTKWidget;
  Context : PPangoContext;
  families : PPPangoFontFamily;
  {$EndIf}
  Tmp: AnsiString;
  I, N: Integer;
begin
  ScreenFonts.Clear;
  {$IFDEF GTK1}
  {$IFDEF UNIX}
  theFonts := XListFonts(X11Display,PChar('-*-*-*-*-*-*-*-*-*-*-*-*-*-*'), 10000, @N);
  debugln('FillScreenFonts N=',dbgs(N));
  for I := 0 to N - 1 do
    if theFonts[I] <> nil then begin
      Tmp := ExtractFamilyFromXLFDName(theFonts[I]);
      if Tmp <> '' then
        if ScreenFonts.IndexOf(Tmp) < 0 then
          ScreenFonts.Append(Tmp);
    end;
   XFreeFontNames(theFonts);
  {$ENDIF UNIX}
  {$ELSE}
   Widget := GetStyleWidget(lgsDefault);
   if Widget = nil then begin
     exit;//raise an error here I guess
   end;
   Context := gtk_widget_get_pango_context(Widget);
   if Context = nil then begin
     exit;//raise an error here I guess
   end;
   families := nil;
   pango_context_list_families(Context, @families, @n);

   for I := 0 to N - 1 do
     if families[I] <> nil then begin
       Tmp := StrPas(pango_font_family_get_name(families[I]));
       if Tmp <> '' then
         if ScreenFonts.IndexOf(Tmp) < 0 then
           ScreenFonts.Append(Tmp);
    end;
   if (families <> nil) then
     g_free(families);
  {$ENDIF GTK2}
end;

function GetTextHeight(DCTextMetric: TDevContextTextMetric): integer;
// IMPORTANT: Before this call:  UpdateDCTextMetric(TDeviceContext(DC));
begin
  {$IfDef Win32}
  Result := DCTextMetric.TextMetric.tmHeight div 2;
  {$Else}
  Result := DCTextMetric.TextMetric.tmAscent;
  {$EndIf}
end;

{$IFDEF GTK1}
{$IFNDEF VER1_0}
var
{$ELSE}
const
{$ENDIF}
  FirstScreenCalled: Boolean = False;
  FirstScreenResult: Boolean = False;
{$IFDEF UseXinerama}
{ Copy record definition from Xinerama unit.
  Can't use the unit itself, as it forces the executable to
  refer to the libraray }
type
   TXineramaScreenInfo = record
        screen_number : cint;
        x_org         : cshort;
        y_org         : cshort;
        width         : cshort;
        height        : cshort;
     end;
   PXineramaScreenInfo = ^TXineramaScreenInfo;

function GetFirstScreen: Boolean;
var
  nMonitors: cint;
  XineramaScreenInfo: PXineramaScreenInfo;
  opcode, firstevent, firsterror: cint;
  XineramaLib: TLibHandle;
  pXineramaIsActive: function (dpy: PDisplay):TBool;cdecl;
  pXineramaQueryScreens: function (dpy: PDisplay;
    number: Pcint): PXineramaScreenInfo;cdecl;
begin
  if not FirstScreenCalled then begin
    if XQueryExtension(X11Display, 'XINERAMA', @opcode, @firstevent,
      @firsterror)
    then begin
      XineramaLib := LoadLibrary('libXinerama.so');
      if XineramaLib <> nil then begin
        Pointer(pXineramaIsActive) :=
                                GetProcAddress(XineramaLib, 'XineramaIsActive');
        Pointer(pXineramaQueryScreens) :=
                            GetProcAddress(XineramaLib, 'XineramaQueryScreens');
        if (pXineramaIsActive <> nil) and (pXineramaQueryScreens <> nil) and
          pXineramaIsActive(X11Display)
        then begin
          XineramaScreenInfo := pXineramaQueryScreens(X11Display, @nMonitors);
          if XineramaScreenInfo <> nil then begin
            if (nMonitors > 0) and (nMonitors < 10) then begin
              FirstScreen.x := XineramaScreenInfo^.width;
              FirstScreen.y := XineramaScreenInfo^.height;
              FirstScreenResult := True;
            end;
            XFree(XineramaScreenInfo);
          end;
        end;
        // Do not FreeLibrary(XineramaLib) because it causes the X11 library to
        // crash on exit
      end;
    end;
    FirstScreenCalled := True;
  end;
  Result := FirstScreenResult;
end;
{$ENDIF UseXinerama}
{$ENDIF Gtk1}

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

// included by gtkproc.pp

{ =============================================================================

  $Log$
  Revision 1.351  2005/03/21 08:24:59  mattias
  gtk intf sorting directories in filedialg  from Jesus

  Revision 1.350  2005/03/07 21:59:45  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.349  2005/03/05 23:00:16  mattias
  adding panels to statusbar during designing now sets SimplePanel:=false

  Revision 1.348  2005/03/05 20:40:57  mattias
  allowing gtk1 ascii keys

  Revision 1.347  2005/03/04 12:21:56  mattias
  fixed TShape FPCanvas issue

  Revision 1.346  2005/03/02 16:47:20  mattias
  fixed loading forms under fpc 1.9.9

  Revision 1.345  2005/02/28 18:21:43  mattias
  fixed gtk file dialog with empty filter  from Collin

  Revision 1.344  2005/02/26 18:39:56  mattias
  fixed gtk open dialog go up

  Revision 1.343  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.342  2005/02/21 07:02:00  vincents
  fixed fpc 1.0.x compilation

  Revision 1.341  2005/02/20 11:03:36  mattias
  added finnish translation for windows codepage  from Seppo

  Revision 1.340  2005/02/20 10:43:04  mattias
  added dyn xinerama use patch  from C Western

  Revision 1.339  2005/02/19 20:36:55  mattias
  xinerama hack is now only enabled when compiled with -dUseXinerama

  Revision 1.338  2005/02/19 20:33:09  mattias
  fixed gtk2 key handle result

  Revision 1.337  2005/02/19 16:30:47  mattias
  fixed 1.0.10 compilation

  Revision 1.336  2005/02/19 16:19:19  mattias
  added xinerama recognition fro gtk1/fpc1_9+  from C Western

  Revision 1.335  2005/02/17 02:10:53  mattias
  fixed gtk2 intf checking if signal already connected

  Revision 1.334  2005/02/16 23:31:16  mattias
  fixed define templates quotes

  Revision 1.333  2005/02/16 22:55:59  mattias
  improved gtk intf file dialog filter  from C Western

  Revision 1.332  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.331  2005/02/03 21:10:07  vincents
  fixed compilation of gtk2 interface on windows

  Revision 1.330  2005/01/24 16:06:42  mattias
  implemented DoubleClick for quick selection in new dialog

  Revision 1.329  2005/01/22 23:53:43  mattias
  fixed gtk2 intf  from Peter Vreman

  Revision 1.328  2005/01/16 11:40:10  mattias
  fixed TGtkWidgetSet.ExtSelectClipRGN for DCOrigin

  Revision 1.327  2005/01/11 21:40:29  micha
  fix gtk compilation for tstatictext.layout

  Revision 1.326  2005/01/01 16:31:14  mattias
  implemented changing font size and name of TEdit for gtk1

  Revision 1.325  2004/12/31 11:59:47  mattias
  published TEdit.Color - only useful under windows, gtk1 ignores it

  Revision 1.324  2004/12/01 16:17:17  mattias
  updated fpdoc sceletons for lcl and gtk intf

  Revision 1.323  2004/11/29 01:12:36  mattias
  added SysKey messages to gtk intf and LCL

  Revision 1.322  2004/11/28 00:55:44  mattias
  deactivated sending SYSKey messages in gtk intf - they are not used anyway

  Revision 1.321  2004/11/20 11:20:06  mattias
  implemented creating classes at run time from any TComponent descendant

  Revision 1.320  2004/11/17 07:46:32  mattias
  fixed postcript printer TextOut/TextEntend  from Olivier

  Revision 1.319  2004/11/10 18:23:56  mattias
  impementing changing a TLabel.Font properties Size, Height, Name, Style - set only at Handle creation time

  Revision 1.318  2004/11/10 15:25:32  mattias
  updated memcheck.pas from heaptrc.pp

  Revision 1.317  2004/11/08 19:11:55  mattias
  disabled hardly used gtk FillScreenFont, this should be only done on demand, improved getting default font family for gtk

  Revision 1.316  2004/11/03 14:18:36  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.315  2004/10/16 15:36:49  mattias
  implemented gtkwscomctrls.TGtkWSStatusBar

  Revision 1.314  2004/10/16 08:59:26  vincents
  fixed fpc 1.0 compilation

  Revision 1.313  2004/10/15 12:04:09  mattias
  calling updating notebook tab after realize, needed for close btns

  Revision 1.312  2004/09/30 10:35:50  mazen
  * Fix compile probelem under GTK2 related to
    THandle <--> Pointer conversion

  Revision 1.311  2004/09/25 15:05:39  mattias
  implemented Rename Identifier

  Revision 1.310  2004/09/24 18:00:52  micha
  convert LM_NB_UPDATETAB message to interface method

  Revision 1.309  2004/09/17 20:30:13  vincents
  replaced write by DbgOut

  Revision 1.308  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.307  2004/09/12 19:00:17  mazen
  *Font width is now used with GTK2

  Revision 1.306  2004/09/11 10:02:38  mattias
  fixed TLazIntfImage.LoadFromDevice

  Revision 1.305  2004/09/10 16:28:51  mattias
  implemented very rudimentary TTabControl

  Revision 1.304  2004/09/05 10:39:01  mattias
  fixed gtk1 intf key handler result

  Revision 1.303  2004/09/04 22:24:16  mattias
  added default values for compiler skip options and improved many parts of synedit for UTF8

  Revision 1.302  2004/09/02 17:59:59  mattias
  removed double KeyPress method in synedit

  Revision 1.301  2004/09/02 16:01:24  mazen
  * fix compile probelm using gtk1

  Revision 1.300  2004/09/02 14:58:14  mazen
  * fixed theType/_Type related to GTK1/GTK2 difference
  * fix return error which causes GTK2 not to call AfterEvent
  * Test on BeforeEvent and EventStopped now exists directly
    in key press, as no more things are done.
  * _string/theString field are depreciated, please don't use
    them with GTK2

  Revision 1.299  2004/09/02 09:17:00  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.298  2004/08/30 15:46:22  mazen
  * Fix a compile problem, still need to find the correct way to fix that.

  Revision 1.297  2004/08/30 10:49:20  mattias
  fixed focus catch for combobox csDropDownList

  Revision 1.296  2004/08/29 10:13:59  mattias
  fixed makefile

  Revision 1.295  2004/08/28 10:22:13  mattias
  added hints for long props in OI  from Andrew Haines

  Revision 1.294  2004/08/18 20:49:02  mattias
  simple forms can now be child controls

  Revision 1.293  2004/08/17 19:01:37  mattias
  gtk intf now ignores size notifications of unrealized widgets

  Revision 1.292  2004/08/16 16:03:52  mattias
  added UniCode keyvals

  Revision 1.291  2004/08/12 15:50:46  mazen
  + add support for passing non ASCII key values
  * need to check for $F000 if it is the correct value

  Revision 1.290  2004/08/03 09:01:54  mattias
  LCL now handles for non win32 CN_CHAR

  Revision 1.289  2004/07/30 14:26:11  mazen
  * move HandleGtkKeyUpDown to gtkProc.inc make it visible to gtk2
    this allow saving a call in a hevely called callback

  Revision 1.288  2004/07/10 18:17:30  mattias
  added Delphi ToDo support, Application.WndProc, small bugfixes  from Colin

  Revision 1.287  2004/07/03 11:11:09  mattias
  TGTKListStringList now keeps selection on Put and Move

  Revision 1.286  2004/06/28 15:45:48  mattias
  fixed a mem violation in gtk intf paint msg conversion

  Revision 1.285  2004/06/28 09:48:46  mattias
  added valgrind flag to compiler options

  Revision 1.284  2004/06/24 17:45:33  mattias
  fixed TMenuItem.GetIconSize

  Revision 1.283  2004/06/19 21:06:38  mattias
  menu separators are now created disabled

  Revision 1.282  2004/06/17 21:24:19  mattias
  implemented painting menuitem icons from ImageList

  Revision 1.281  2004/06/11 12:53:50  vincents
  fixed memleak in WidgetInfo.UserData used by BitBtn

  Revision 1.280  2004/05/16 23:24:41  marc
  + Added WSBitBtn interface
  + Implemented WSBitBtn interface for gtk

  Revision 1.279  2004/05/14 12:53:25  mattias
  improved grids e.g. OnPrepareCanvas   patch from Jesus

  Revision 1.278  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.277  2004/05/11 09:49:47  mattias
  started sending CN_KEYUP

  Revision 1.276  2004/04/19 09:30:04  marc
  * Fixed compilation for gtk2

  Revision 1.275  2004/04/11 18:58:26  micha
  fix (lm_)setcursor changes for gtk target

  Revision 1.274  2004/04/08 18:27:51  mattias
  fixed memleak in TDefaultComponentEditor.Edit

  Revision 1.273  2004/04/03 18:08:40  mattias
  fixed TLabel.AutoWrap=true and label on formless parent in gtk intf

  Revision 1.272  2004/03/28 12:49:23  mattias
  implemented mask merge and extraction for raw images

  Revision 1.271  2004/03/24 01:21:41  marc
  * Simplified signals for gtkwsbutton

  Revision 1.270  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.269  2004/03/18 00:55:56  mattias
  fixed memleak in gtk opendlg

  Revision 1.268  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.267  2004/03/06 17:12:19  mattias
  fixed CreateBrushIndirect

  Revision 1.266  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.265  2004/03/05 00:31:52  marc
  * Renamed TGtkObject to TGtkWidgetSet

  Revision 1.264  2004/02/28 10:16:02  mattias
  fixed 1.0.x compilation

  Revision 1.263  2004/02/28 00:34:36  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.262  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.261  2004/02/23 23:15:14  mattias
  improved FindDragTarget

  Revision 1.260  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.259  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.258  2004/02/08 11:31:32  mattias
  TMenuItem.Bitmap is now auto created on read. Added TMenuItem.HasBitmap

  Revision 1.257  2004/02/07 18:04:14  mattias
  fixed grids OnDrawCells

  Revision 1.256  2004/02/04 12:48:17  mattias
  added CLX colors

  Revision 1.255  2004/02/03 23:42:43  marc
  * Fixed Shift+Fn menu captions

  Revision 1.254  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.253  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.252  2004/02/02 00:41:06  mattias
  TScrollBar now automatically checks Align and Anchors for useful values

  Revision 1.251  2004/01/27 21:32:11  mattias
  improved changing style of controls

  Revision 1.250  2004/01/27 10:09:44  mattias
  fixed renaming of DFM to LFM

  Revision 1.249  2004/01/26 11:55:35  mattias
  fixed resizing synedit

  Revision 1.248  2004/01/23 13:55:30  mattias
  style widgets are now realized, so all values are initialized

  Revision 1.247  2004/01/22 11:23:36  mattias
  started MaskBlt for gtkIF and applied patch for dir dlg in env opts from Vincent

  Revision 1.246  2004/01/18 11:03:01  mattias
  added finnish translation

  Revision 1.245  2004/01/14 20:09:50  mattias
  added TColorDialog debugging

  Revision 1.244  2004/01/13 16:39:02  mattias
  changed consistency stops during var renaming to errors

  Revision 1.243  2004/01/12 23:56:10  mattias
  improved double buffering, only one issue left: parent gdkwindow paint messages

  Revision 1.242  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.241  2004/01/10 22:34:20  mattias
  started double buffering for gtk intf

  Revision 1.240  2004/01/09 13:49:43  mattias
  improved gtk intf key fetching and OI keyboard navigation

  Revision 1.239  2004/01/05 01:18:15  mattias
  implemented Double Buffering for synedit and deactivated multi buffering in TGTKObject.ExtTextOut

  Revision 1.238  2004/01/04 16:44:33  mattias
  updated gtk2 package

  Revision 1.237  2004/01/03 23:14:59  mattias
  default font can now change height and fixed gtk crash

  Revision 1.236  2003/12/26 15:23:30  mattias
  started message editor and fixed some range checks

  Revision 1.235  2003/12/26 10:59:25  mattias
  fixed color coversion range check

  Revision 1.234  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.233  2003/12/18 15:15:13  ajgenius
  fix NIL style crash and GTK2 Compiling

  Revision 1.232  2003/11/30 18:35:20  mattias
  fixed fpc 1.9.1 warns

  Revision 1.231  2003/11/29 13:17:38  mattias
  made gtklayout using window theme at start

  Revision 1.230  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.229  2003/11/25 08:59:01  mattias
  fixed a few more black colors

  Revision 1.228  2003/11/23 13:13:35  mattias
  added clWindow for gtklistitem

  Revision 1.227  2003/11/16 01:56:15  mattias
  changed TMenuItem.Graphic to TMenuItem.Bitmap

  Revision 1.226  2003/11/15 15:30:34  marc
  * Fixed range chek errors in KeySymtoVKeyArray

  Revision 1.225  2003/11/07 22:50:44  mattias
  fixed finding sysutilh.inc

  Revision 1.224  2003/11/01 10:27:41  mattias
  fpc 1.1 fixes, started scrollbar hiding, started polymorphing client areas

  Revision 1.223  2003/10/30 21:26:23  mattias
  removed some hints

  Revision 1.222  2003/10/24 21:28:16  marc
  Added cleanup code for keyboard tables

  Revision 1.221  2003/10/22 20:37:31  ajgenius
  fix accel group test to remove GTK2 warnings

  Revision 1.220  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.219  2003/10/19 16:33:10  marc
  * Fixed VKey keypad handling

  Revision 1.218  2003/10/17 03:21:21  ajgenius
  fix GTK2 compiling for new Keyboard changes

  Revision 1.217  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.216  2003/10/15 20:33:37  ajgenius
  add csForm, start fixing Style matching for syscolors and fonts

  Revision 1.215  2003/10/06 16:13:52  ajgenius
  partly fixed gtk2 mouse offsets;
  added new includes to gtk2 lpk

  Revision 1.214  2003/10/03 01:25:01  ajgenius
  add more gtk1i<->gtk2 key & event wrappers,
  move more GTK2 workarounds from gtk to gtk2 interface,
  start GTK2 interface SetCallback

  Revision 1.213  2003/09/25 20:44:42  ajgenius
  minor changes for gtk2

  Revision 1.212  2003/09/25 16:02:16  ajgenius
  try to catch GDK/X drawable errors and raise an AV to stop killing App

  Revision 1.211  2003/09/24 17:23:54  ajgenius
  more work toward GTK2 - partly fix CheckListBox, & MenuItems

  Revision 1.210  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.209  2003/09/19 00:41:52  ajgenius
  remove USE_PANGO define since pango now apears to work properly.

  Revision 1.208  2003/09/18 14:06:30  ajgenius
  fixed Tgtkobject.drawtext for Pango till the native pango one works better

  Revision 1.207  2003/09/17 19:40:46  ajgenius
  Initial DoubleBuffering Support for GTK2

  Revision 1.206  2003/09/17 15:26:41  mattias
  fixed removing TCustomPage

  Revision 1.205  2003/09/12 17:40:46  ajgenius
  fixes for GTK2(accel groups, menu accel, 'draw'),
  more work toward Pango(DrawText now works, UpdateDCTextMetric mostly works)

  Revision 1.204  2003/09/11 21:33:11  ajgenius
  partly fixed TWinControl(csFixed)

  Revision 1.203  2003/09/10 18:03:46  ajgenius
  more changes for pango -
  partly fixed ref counting,
  added Pango versions of TextOut, CreateFontIndirectEx, and GetTextExtentPoint to the GTK2 interface

  Revision 1.202  2003/09/10 02:33:41  ajgenius
  fixed TColotDialog for GTK2

  Revision 1.201  2003/09/09 20:46:38  ajgenius
  more implementation toward pango for gtk2

  Revision 1.200  2003/09/09 04:15:08  ajgenius
  more updates for GTK2, more GTK1 wrappers, removal of more ifdef's, partly fixed signals

  Revision 1.199  2003/09/06 22:56:03  ajgenius
  started gtk2 stock icon overrides
  partial/temp(?) workaround for dc paint offsets

  Revision 1.198  2003/09/06 20:23:53  ajgenius
  fixes for gtk2
  added more wrappers for gtk1/gtk2 converstion and sanity
  removed pointless version $Ifdef GTK2 etc
  IDE now "runs" Tcontrol drawing/using problems
  renders it unuseable however

  Revision 1.197  2003/09/06 17:24:52  ajgenius
  gtk2 changes for pixmap, getcursorpos, mouse events workaround

  Revision 1.196  2003/09/05 19:29:38  mattias
  Success: The first gtk2 application ran without error

  Revision 1.195  2003/09/05 19:03:19  ajgenius
  removed a redundant routine which broke gtk1

  Revision 1.194  2003/09/05 18:19:54  ajgenius
  Make GTK2 "compile". linking fails still
  (Makefile.fpc needs pkgconfig libs/GTK2 linking rules,
  but not sure how not sure how) and when linked via a make script
  (like gtk2 examples do) apps still won't work(yet).  I think we
  need to do a lot of work to make sure incompatible(also to get rid
  of deprecated) things are done in GTK2 interface itself, and just
  use more $Ifdef GTK1 in the gtk interface itself.

  Revision 1.193  2003/09/04 10:51:30  mattias
  fixed default size of preview widget

  Revision 1.192  2003/08/30 18:53:08  mattias
  using default colors, when theme does not define them

  Revision 1.191  2003/08/29 21:21:07  mattias
  fixes for gtk2

  Revision 1.190  2003/08/28 09:10:00  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.189  2003/08/27 21:14:42  mattias
  fixed a few things for gtk2 intf

  Revision 1.188  2003/07/21 23:43:32  marc
  * Fixed radiogroup menuitems

  Revision 1.187  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.186  2002/08/18 16:50:09  mattias
  fixes for debugging

  Revision 1.185  2002/08/17 23:41:35  mattias
  many clipping fixes

  Revision 1.184  2003/06/19 09:26:58  mattias
  fixed changing unitname during update

  Revision 1.183  2003/06/18 00:10:38  marc
  + Added exceptionhandler while delivering messages

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
  MG: TNoteBook now starts with no Page and TCustomPage has no auto names

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
  MG: fixed initial size of TCustomPage

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
