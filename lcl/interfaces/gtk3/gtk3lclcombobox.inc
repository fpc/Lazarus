{%MainUnit gtk3widgets.pas}

procedure LCLGtkComboBoxSetShrinkClassRecursive(W: PGtkWidget; AAdd: Boolean); forward;

procedure LCLGtkComboBoxGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
  LCLW: Integer;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredWidth invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredWidth widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredWidth cannot get ParentClass !');
    Exit;
  end;
  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) or not Assigned(AControl.LCLObject) then
    Exit;

  if AControl.LCLWidth > 0 then
    LCLW := AControl.LCLWidth
  else
    LCLW := AControl.LCLObject.Width;

  if LCLW > 0 then
  begin
    if min_width^ > LCLW then
      min_width^ := LCLW;
    nat_width^ := LCLW;
  end;
end;

procedure LCLGtkComboBoxGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl:TGtk3Widget;
  ParentClass:PGtkWidgetClass;
  ACombo: PGtkComboBox;
  aContext: PPangoContext;
  aPangoLayout: PPangoLayout;
  ABorder, AMargin, APadding: TGtkBorder;
  w, h, xpad, ypad: gint;
  SavedW, SavedH: gint;
  aList: PGList;
begin

  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredHeight invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredHeight widget param is not PGtkWidget.');
    // WriteLn('Error: widget is not a valid GtkWidget');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));

  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredHeight cannot get ParentClass !');
    exit;
  end;

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if Assigned(AControl) and Assigned(AControl.LCLObject) then
    LCLGtkComboBoxSetShrinkClassRecursive(widget, not AControl.LCLObject.AutoSize);

  if Assigned(AControl) and Assigned(AControl.LCLObject) and AControl.LCLObject.AutoSize then
  begin
    widget^.get_size_request(@SavedW, @SavedH);
    widget^.set_size_request(-1, -1);
    ParentClass^.get_preferred_height(widget, min_height, nat_height);
    widget^.set_size_request(SavedW, SavedH);
  end else
    ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredHeight cannot get TGtk3Widget for widget parameter.');
    exit;
  end;

  if AControl.LCLObject.AutoSize then
    exit;

  if AControl.LCLHeight = 0 then
  begin
    if AControl.LCLObject.Height >= min_height^ then
    begin
      min_height^ := Max(min_height^ div 2, AControl.LCLObject.Height);
      nat_height^ := Max(min_height^, AControl.LCLObject.Height);
    end
    else
      nat_height^ := min_height^;
  end else
  begin
    if AControl.LCLHeight >= min_height^ then
    begin
      min_height^ := Max(min_height^ div 2, AControl.LCLHeight);
      nat_height^ := Max(min_height^, AControl.LCLHeight);
    end
    else
      nat_height^ := min_height^;
  end;
end;

procedure LCLGtkComboBoxGetPreferredHeightForWidth(widget: PGtkWidget; width: gint; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then exit;
  if not Gtk3IsWidget(widget) then exit;
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.get_preferred_height_for_width) then exit;
  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if Assigned(AControl) and Assigned(AControl.LCLObject) then
    LCLGtkComboBoxSetShrinkClassRecursive(widget, not AControl.LCLObject.AutoSize);
  ParentClass^.get_preferred_height_for_width(widget, width, min_height, nat_height);
  if not Assigned(AControl) then exit;
  if AControl.LCLObject.AutoSize then exit;
  if AControl.LCLHeight > 0 then
  begin
    if AControl.LCLHeight >= min_height^ then
    begin
      min_height^ := Max(min_height^ div 2, AControl.LCLHeight);
      nat_height^ := AControl.LCLHeight;
    end
    else
      nat_height^ := min_height^;
  end else
  begin
    if AControl.LCLObject.Height >= min_height^ then
    begin
      min_height^ := Max(min_height^ div 2, AControl.LCLObject.Height);
      nat_height^ := AControl.LCLObject.Height;
    end
    else
      nat_height^ := min_height^;
  end;
end;

function LCLGtkComboBoxDraw(widget: PGtkWidget; cairo: Pcairo_t): gboolean; cdecl;
var
  ParentClass: PGtkWidgetClass;
  Alloc: TGtkAllocation;
  {$IFDEF GTK3DEBUGCOMBOBOX}
  ARect: TGdkRectangle;
  dx, dy, dw, dh: Double;
  {$ENDIF}
begin
  widget^.get_allocation(@Alloc);
  //Prevent cairo bleeding over right corner of combobox
  cairo_save(cairo);
  cairo_rectangle(cairo, 0, 0, Alloc.width, Alloc.height);
  cairo_clip(cairo);
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  {$IFDEF GTK3DEBUGCOMBOBOX}
  gdk_cairo_get_clip_rectangle(cairo, @ARect);
  cairo_clip_extents(cairo, @dx, @dy, @dw, @dh);
  writeln('LCLGtkComboBoxDraw alloc=',dbgs(RectFromGdkRect(Alloc)),' ClipRect=',dbgs(RectFromGdkRect(ARect)),' CairoClip x=',dbgs(dx),' y=',dbgs(dy),' w=',dbgs(dw),' h=',dbgs(dh));
  {$ENDIF}
  Result := ParentClass^.draw(widget, cairo);
  cairo_restore(cairo);
end;

procedure LCLGtkComboBoxClassInit(klass: PGTypeClass; data: Pointer); cdecl;
  {$IFDEF GTK3DEBUGCOMBOBOX}
  procedure WalkTypeHierarchy(klass: PGTypeClass);
  begin
    while Assigned(klass) do
    begin
      WriteLn('Class type: ', g_type_name(klass^.g_type));
      klass := g_type_class_peek_parent(klass);
    end;
  end;
  {$ENDIF}
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkComboBoxGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkComboBoxGetPreferredHeight;
  AWidgetClass^.get_preferred_height_for_width := @LCLGtkComboBoxGetPreferredHeightForWidth;
  AWidgetClass^.draw := @LCLGtkComboBoxDraw;
  {$IFDEF GTK3DEBUGCOMBOBOX}
  WalkParentClassHierarchy(klass);
  {$ENDIF}
end;

procedure LCLGtkComboBoxInit(instance: PGTypeInstance; klass: PGTypeClass); cdecl;
var
  combowidget:PGtkComboBox;
  {%H-}AStyle:PGtkStyleContext;
  //Alloc:TGtkAllocation;
begin
  combowidget := PGtkComboBox(instance);
  {we can initialize here any default values
  Alloc.x := 0;
  Alloc.Y := 0;
  Alloc.Width := 75; // default
  Alloc.Height := 25; // default
  comboWidget^.set_allocation(@Alloc);
  }
  AStyle := comboWidget^.get_style_context;
end;

const
  LCL_COMBO_SHRINK_CLASS = 'lcl-combo-shrink';

procedure LCLGtkComboBoxInstallAppCSS;
const
  CSS =
    '.lcl-combo-shrink, .lcl-combo-shrink * {' +
    ' transition: none;' +
    ' min-height: 0px;' +
    ' padding-top: 0px;' +
    ' padding-bottom: 0px;' +
    ' margin-top: 0px;' +
    ' margin-bottom: 0px;' +
    ' }';
var
  Provider: PGtkCssProvider;
  Screen: PGdkScreen;
begin
  Provider := gtk_css_provider_new;
  gtk_css_provider_load_from_data(Provider, CSS, -1, nil);
  Screen := gdk_screen_get_default;
  if Screen <> nil then
    gtk_style_context_add_provider_for_screen(Screen,
      PGtkStyleProvider(Provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(Provider);
end;

procedure LCLGtkComboBoxSetShrinkClassRecursive(W: PGtkWidget; AAdd: Boolean);

  procedure ToggleOnContext(AW: PGtkWidget);
  var
    Ctx: PGtkStyleContext;
  begin
    if not Gtk3IsWidget(AW) then exit;
    Ctx := AW^.get_style_context;
    if Ctx = nil then exit;
    if AAdd then
    begin
      if not gtk_style_context_has_class(Ctx, LCL_COMBO_SHRINK_CLASS) then
        gtk_style_context_add_class(Ctx, LCL_COMBO_SHRINK_CLASS);
    end
    else
    begin
      if gtk_style_context_has_class(Ctx, LCL_COMBO_SHRINK_CLASS) then
        gtk_style_context_remove_class(Ctx, LCL_COMBO_SHRINK_CLASS);
    end;
  end;

var
  Children, L: PGList;
  Combo: PGtkComboBox;
begin
  if not Gtk3IsWidget(W) then exit;
  ToggleOnContext(W);
  if Gtk3IsContainer(W) then
  begin
    Children := PGtkContainer(W)^.get_children;
    L := Children;
    while L <> nil do
    begin
      LCLGtkComboBoxSetShrinkClassRecursive(PGtkWidget(L^.data), AAdd);
      L := L^.next;
    end;
    if Children <> nil then g_list_free(Children);
  end;
  if Gtk3IsComboBox(W) then
  begin
    Combo := PGtkComboBox(W);
    if Assigned(Combo^.priv3^.cell_view) then
      ToggleOnContext(PGtkWidget(Combo^.priv3^.cell_view));
    if Assigned(Combo^.priv3^.button) then
      ToggleOnContext(Combo^.priv3^.button);
    if Assigned(Combo^.priv3^.arrow) then
      ToggleOnContext(Combo^.priv3^.arrow);
  end;
end;

var
  lclCombotype: TGType = 0;

function LCLGtkComboBoxGetType: TGType; cdecl;
var
  Query: TGTypeQuery;
  Info: TGTypeInfo;
  ParentType: TGType;
begin
  if lclCombotype = 0 then
  begin
    ParentType := gtk_combo_box_get_type;
    g_type_query(ParentType, @Query);
    FillChar(Info{%H-}, SizeOf(Info), 0);
    Info.class_size    := guint16(Query.class_size);
    Info.class_init    := @LCLGtkComboBoxClassInit;
    Info.instance_size := guint16(Query.instance_size);
    Info.instance_init := @LCLGtkComboBoxInit;
    lclCombotype := g_type_register_static(ParentType,
      'LCLGtkComboBox', @Info, G_TYPE_FLAG_NONE);
    LCLGtkComboBoxInstallAppCSS;
  end;
  Result := lclCombotype;
end;

function LCLGtkComboBoxNewWithModelAndEntry(model: PGtkTreeModel): PGtkComboBox;
begin
  Result := PGtkComboBox(g_object_new(LCLGtkComboBoxGetType,
                         'model', [model,
                         'has-entry', gboolean(true),
                         nil]));
end;

function LCLGtkComboBoxNewWithModel(model: PGtkTreeModel): PGtkWidget;
begin
  Result := PGtkComboBox(g_object_new(LCLGtkComboBoxGetType,
                         'model',[model,
                         'has-entry', gboolean(false),
                         nil]));
end;

