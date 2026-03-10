{%MainUnit gtk3widgets.pas}
const
  GTK_FRAME_CLASS_SIZE = SizeOf(TGtkFrameClass);
  GTK_FRAME_INSTANCE_SIZE = SizeOf(TGtkFrame);

procedure LCLGtkFrameGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredWidth invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredWidth widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredWidth cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_width(widget, min_width, nat_width);
  // writeln('LCLFrame preferred min_width=',min_width^,' natural w=',nat_width^);
  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredWidth cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

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

procedure LCLGtkFrameGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredHeight invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredHeight widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredHeight cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkFrameGetPreferredHeight cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

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

procedure LCLGtkFrameSizeAllocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  labelWidget: PGtkWidget;
  labelMinH: gint;
begin
  //Guard: skip tiny allocations that would cause GTK cascade loops.
  //We only use the label widget's minimum height - no CSS context calls,
  //which can assert/crash when the style context is not yet initialized.
  if allocation^.height < 2 then
    Exit;

  labelWidget := PGtkFrame(widget)^.get_label_widget;
  if Assigned(labelWidget) and Gtk3IsWidget(labelWidget) and labelWidget^.get_visible then
  begin
    labelMinH := 0;
    gtk_widget_get_preferred_height(labelWidget, @labelMinH, nil);
    if (labelMinH > 0) and (allocation^.height < labelMinH) then
      Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;
  ParentClass^.size_allocate(widget, allocation);
end;

procedure LCLGtkFrameClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  //AWidgetClass^.size_allocate := @LCLGtkFrameSizeAllocate;
  AWidgetClass^.get_preferred_width := @LCLGtkFrameGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkFrameGetPreferredHeight;
end;

procedure LCLGtkFrameInstanceInit(instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
begin
  //
end;

var
  LCLGtkFrameType: TGType = 0;

function LCLGtkFrameGetType: TGType; cdecl;
const
  lcl_frame_type_info: TGTypeInfo = (
    class_size: GTK_FRAME_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkFrameClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_FRAME_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkFrameInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkFrameType = 0 then
    LCLGtkFrameType := g_type_register_static(gtk_frame_get_type, 'LCLGtkFrame', @lcl_frame_type_info, G_TYPE_FLAG_NONE);
  Result := LCLGtkFrameType;
end;

function LCLGtkFrameNew: PGtkFrame;
begin
                                                                      {in gtk3 this is default shadow}
  Result := PGtkFrame(g_object_new(LCLGtkFrameGetType(),'shadow-type',[GTK_SHADOW_ETCHED_IN, nil]));
end;

const
  GTK_FIXED_CLASS_SIZE = SizeOf(TGtkFixedClass);
  GTK_FIXED_INSTANCE_SIZE = SizeOf(TGtkFixed);

procedure LCLGtkFixedGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
    Exit;
  { LCLGtkFixed (Toolbars): TRULY SOFT WIDTH }
  { We force 10px to break the expansion loop. Child positions (e.g. x=1950) }
  { in a docked IDE otherwise 'push' the window to be too wide. }
  min_width^ := 10;
  nat_width^ := 10;
end;

procedure LCLGtkFixedGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkFixedGetPreferredHeight invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkFixedGetPreferredHeight widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkFixedGetPreferredHeight cannot get ParentClass !');
    Exit;
  end;

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkFixedGetPreferredHeight cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  { LCLGtkFixed (Toolbars): KEEP HARD HEIGHT to prevent 1px collapse.
    Only apply the floor when the control is intended to be visible.
    When LCL explicitly sets Height=0 (control collapsed), report 0 so GTK
    does not prevent the widget from collapsing to zero. }
  if AControl.LCLHeight > 0 then
  begin
    min_height^ := Max(min_height^, AControl.LCLHeight);
    nat_height^ := Max(nat_height^, min_height^);
  end else if Assigned(AControl.LCLObject) and (AControl.LCLObject.Height > 0) then
  begin
    min_height^ := Max(min_height^, AControl.LCLObject.Height);
    nat_height^ := Max(nat_height^, min_height^);
  end else
  begin
    { Height=0: collapsed intentionally — ensure at least 0, not 10 }
    if min_height^ < 0 then min_height^ := 0;
    nat_height^ := Max(nat_height^, min_height^);
  end;
end;

procedure LCLGtkFixedClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkFixedGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkFixedGetPreferredHeight;
end;

procedure LCLGtkFixedInstanceInit(instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
begin
  //
end;

var
  LCLGtkFixedType: TGType = 0;

function LCLGtkFixedGetType: TGType; cdecl;
const
  lcl_fixed_type_info: TGTypeInfo = (
    class_size: GTK_FIXED_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkFixedClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_FIXED_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkFixedInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkFixedType = 0 then
    LCLGtkFixedType := g_type_register_static(gtk_fixed_get_type, 'LCLGtkFixed', @lcl_fixed_type_info, G_TYPE_FLAG_NONE);
  Result := LCLGtkFixedType;
end;

function LCLGtkFixedNew: PGtkFixed;
begin
                                                                      {in gtk3 this is default border-width}
  Result := PGtkFixed(g_object_new(LCLGtkFixedGetType(),'border-width',[0, nil]));
end;



