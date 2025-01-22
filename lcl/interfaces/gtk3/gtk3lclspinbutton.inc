{%MainUnit gtk3widgets.pas}
const
  GTK_SPIN_BUTTON_CLASS_SIZE = SizeOf(TGtkSpinButtonClass);
  GTK_SPIN_BUTTON_INSTANCE_SIZE = SizeOf(TGtkSpinButton);

procedure LCLGtkSpinButtonGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredWidth invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredWidth widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredWidth cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_width(widget, min_width, nat_width);
  // writeln('LCLSpinButton preferred min_width=',min_width^,' natural w=',nat_width^);
  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredWidth cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

  // Custom width handling, gtk's spin button is very ugly about width.
  // Gtk's minimum size for GtkSpinButton is 102 px at 96 dpi, so pretty big.
  // 75 with applied css is 1 char and +- buttons
  // 90 should be LCL default, so we see at least two numbers with default font.
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

procedure LCLGtkSpinButtonGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredHeight invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredHeight widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredHeight cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkSpinButtonGetPreferredHeight cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

 // we respect ws height if autosize is true.
  if AControl.LCLObject.AutoSize then
    exit;

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

procedure LCLGtkSpinButtonClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkSpinButtonGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkSpinButtonGetPreferredHeight;
end;

procedure LCLGtkSpinButtonInstanceInit({%H-}instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
// var
//  buttonWidget: PGtkSpinButton;
begin
  //buttonWidget := PGtkSpinButton(instance);
  // Custom initialization logic (if needed)
  //AStyleContext := buttonwidget^.get_style_context;
  //AStyleContext^.get_
end;

var
  LCLGtkSpinButtonType: TGType = 0;

function LCLGtkSpinButtonGetType: TGType; cdecl;
const
  lcl_spin_button_type_info: TGTypeInfo = (
    class_size: GTK_SPIN_BUTTON_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkSpinButtonClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_SPIN_BUTTON_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkSpinButtonInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkSpinButtonType = 0 then
    LCLGtkSpinButtonType := g_type_register_static(gtk_spin_button_get_type, 'LCLGtkSpinButton', @lcl_spin_button_type_info, G_TYPE_FLAG_NONE);
  Result := LCLGtkSpinButtonType;
end;

function LCLGtkSpinButtonNew: PGtkSpinButton;
begin
  Result := PGtkSpinButton(g_object_new(LCLGtkSpinButtonGetType(),'wrap',[gboolean(False), nil]));
end;


