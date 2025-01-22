{%MainUnit gtk3widgets.pas}

const
  GTK_COMBO_BOX_CLASS_SIZE = SizeOf(TGtkComboBoxClass); //1048
  GTK_COMBO_BOX_INSTANCE_SIZE = SizeOf(TGtkComboBox); //56


procedure LCLGtkComboBoxGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl:TGtk3Widget;
  ParentClass:PGtkWidgetClass;
begin

  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredWidth invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredWidth widget param is not PGtkWidget.');
    // WriteLn('Error: widget is not a valid GtkWidget');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));

  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredWidth cannot get ParentClass !');
    exit;
  end;

  //check what parent class says about minimum and natural width.
  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredWidth cannot get TGtk3Widget for widget parameter.');
    exit;
  end;

  // we don't check autosize since combo width is free size
  if AControl.LCLWidth = 0 then
  begin
    min_width^ := Max(min_width^ div 2, AControl.LCLObject.Width);
    nat_width^ := Max(min_width^, AControl.LCLObject.Width);
  end else
  begin
    min_width^ := Max(min_width^ div 2, AControl.LCLWidth);
    nat_width^ := Max(min_width^, AControl.LCLWidth);
  end;
end;

procedure LCLGtkComboBoxGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl:TGtk3Widget;
  ParentClass:PGtkWidgetClass;
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

  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkComboBoxGetPreferredHeight cannot get TGtk3Widget for widget parameter.');
    exit;
  end;

  if AControl.LCLObject.AutoSize then
    exit; // keep gtk calculated height.

  if AControl.LCLHeight = 0 then
  begin
    min_height^ := Max(min_height^ div 2, AControl.LCLObject.Height);
    nat_height^ := Max(min_height^, AControl.LCLObject.Height);
  end else
  begin
    min_height^ := Max(min_height^ div 2, AControl.LCLHeight);
    nat_height^ := Max(min_height^, AControl.LCLHeight);
  end;
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
  {$IFDEF GTK3DEBUGCOMBOBOX}
  //debug - looks ok.
  //WalkTypeHierarchy(klass);
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

var
  lclCombotype: TGType = 0;

function LCLGtkComboBoxGetType: TGType; cdecl;
const
  lcl_combo_box_type_info: TGTypeInfo = (
    class_size: GTK_COMBO_BOX_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkComboBoxClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_COMBO_BOX_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkComboBoxInit;
    value_table: nil;
  );
begin
  if lclCombotype = 0 then
    lclCombotype := g_type_register_static(gtk_combo_box_get_type, 'LCLGtkComboBox', PGTypeInfo(@lcl_combo_box_type_info), G_TYPE_FLAG_NONE);
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

