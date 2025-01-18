{%MainUnit gtk3widgets.pas}
const
  GTK_BUTTON_CLASS_SIZE = SizeOf(TGtkButtonClass);
  GTK_BUTTON_INSTANCE_SIZE = SizeOf(TGtkButton);

procedure LCLGtkButtonGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkButtonGetPreferredWidth invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkButtonGetPreferredWidth widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

  // we respect ws width of button if autosize is true.
  if AControl.LCLObject.AutoSize then
    exit;
  // Custom width handling
  if AControl.LCLWidth = 0 then
  begin
    min_width^ := Max(50, AControl.LCLObject.Width);
    nat_width^ := Max(50, AControl.LCLObject.Width);
  end else
  begin
    min_width^ := Max(50, AControl.LCLWidth);
    nat_width^ := Max(50, AControl.LCLWidth);
  end;
end;

procedure LCLGtkButtonGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkButtonGetPreferredHeight invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkButtonGetPreferredHeight widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

 // we respect ws height if autosize is true.
  if AControl.LCLObject.AutoSize then
    exit;

  // Custom height handling
  if AControl.LCLHeight = 0 then
  begin
    min_height^ := Max(20, AControl.LCLObject.Height);
    nat_height^ := Max(20, AControl.LCLObject.Height);
  end else
  begin
    min_height^ := Max(20, AControl.LCLHeight);
    nat_height^ := Max(20, AControl.LCLHeight);
  end;
end;

procedure LCLGtkButtonClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkButtonGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkButtonGetPreferredHeight;
end;

procedure LCLGtkButtonInstanceInit(instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
var
  buttonWidget: PGtkButton;
begin
  buttonWidget := PGtkButton(instance);
  // Custom initialization logic (if needed)
end;

var
  LCLGtkButtonType: TGType = 0;

function LCLGtkButtonGetType: TGType; cdecl;
const
  lcl_button_type_info: TGTypeInfo = (
    class_size: GTK_BUTTON_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkButtonClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_BUTTON_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkButtonInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkButtonType = 0 then
    LCLGtkButtonType := g_type_register_static(gtk_button_get_type, 'LCLGtkButton', @lcl_button_type_info, G_TYPE_FLAG_NONE);
  Result := LCLGtkButtonType;
end;

function LCLGtkButtonNew: PGtkButton;
begin
  Result := PGtkButton(g_object_new(LCLGtkButtonGetType(),'label',['', nil]));
end;


