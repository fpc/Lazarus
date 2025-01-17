{%MainUnit gtk3widgets.pas}

const
  GTK_ENTRY_CLASS_SIZE = SizeOf(TGtkEntryClass);
  GTK_ENTRY_INSTANCE_SIZE = SizeOf(TGtkEntry);

procedure LCLGtkEntryGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkEntryGetPreferredWidth invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkEntryGetPreferredWidth widget param is not PGtkWidget.');
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

  // we pass our width , no matter autosize is on or off.
  if AControl.LCLWidth = 0 then
  begin
    min_width^ := Max(30, AControl.LCLObject.Width);
    nat_width^ := Max(30, AControl.LCLObject.Width);
  end else
  begin
    min_width^ := Max(30, AControl.LCLWidth);
    nat_width^ := Max(30, AControl.LCLWidth);
  end;
end;

procedure LCLGtkEntryGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkEntryGetPreferredHeight invalid params.');
    Exit;
  end;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkEntryGetPreferredHeight widget param is not PGtkWidget.');
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

procedure LCLGtkEntryClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkEntryGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkEntryGetPreferredHeight;
end;

procedure LCLGtkEntryInstanceInit(instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
var
  entryWidget: PGtkEntry;
  //Alloc: TGtkAllocation;
  {%H-}AStyle:PGtkStyleContext;
begin
  entryWidget := PGtkEntry(instance);

  { Initialize default allocation
  Alloc.x := 0;
  Alloc.Y := 0;
  Alloc.Width := 75; // Default width
  Alloc.Height := 25; // Default height
  entryWidget^.set_allocation(@Alloc);
  }
  AStyle := entryWidget^.get_style_context;
  // writeln('StyleContext ? ',AStyle <> nil,' Scale=',AStyle^.get_scale);
end;

var
  LCLGtkEntryType: TGType = 0;

function LCLGtkEntryGetType: TGType; cdecl;
const
  lcl_entry_type_info: TGTypeInfo = (
    class_size: GTK_ENTRY_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkEntryClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_ENTRY_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkEntryInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkEntryType = 0 then
    LCLGtkEntryType := g_type_register_static(gtk_entry_get_type, 'LCLGtkEntry', @lcl_entry_type_info, G_TYPE_FLAG_NONE);
  Result := LCLGtkEntryType;
end;

function LCLGtkEntryNew: PGtkEntry;
begin
  Result := PGtkEntry(g_object_new(LCLGtkEntryGetType(), 'editable', [gboolean(True), nil]));
end;


