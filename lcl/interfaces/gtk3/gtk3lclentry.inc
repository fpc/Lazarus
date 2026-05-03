{%MainUnit gtk3widgets.pas}

procedure LCLGtkEntryGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
  APango: PPangoLayout;
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
    DebugLn('Error: LCLGtkEntryGetPreferredWidth cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkEntryGetPreferredWidth cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

  APango := pango_layout_new(Widget^.get_pango_context);
  APango^.set_text('W', 1);
  APango^.get_size(min_width, nat_width);
  APango^.unref;
  min_width^ := PANGO_PIXELS(min_width^) * 2 + 2;

  if AControl.LCLWidth > 0 then
    nat_width^ := AControl.LCLWidth
  else
  if Assigned(AControl.LCLObject) and (AControl.LCLObject.Width > 0) then
    nat_width^ := AControl.LCLObject.Width
  else
    nat_width^ := min_width^;
  if nat_width^ < min_width^ then
    nat_width^ := min_width^;

  //apply constraints ONLY if we are primary widget, not eg our spin edit implemenation.
  if (AControl.Widget = widget) and Assigned(AControl.LCLObject.Constraints) then
    with AControl.LCLObject.Constraints do
    begin
      if MaxWidth > 0 then
      begin
        if min_width^ > MaxWidth then min_width^ := MaxWidth;
        if nat_width^ > MaxWidth then nat_width^ := MaxWidth;
      end;
      if MinWidth > 0 then
      begin
        if min_width^ < MinWidth then min_width^ := MinWidth;
        if nat_width^ < MinWidth then nat_width^ := MinWidth;
      end;
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
    DebugLn('Error: LCLGtkEntryGetPreferredHeight cannot get ParentClass !');
    Exit;
  end;

  // Call parent class implementation
  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
  begin
    DebugLn('Error: LCLGtkEntryGetPreferredHeight cannot get TGtk3Widget for widget parameter.');
    Exit;
  end;

  if AControl.Widget <> widget then
  begin
    if nat_height^ < min_height^ then
      nat_height^ := min_height^;
    exit;
  end;

  if not AControl.LCLObject.AutoSize then
  begin
    if AControl.LCLHeight > 0 then
      nat_height^ := AControl.LCLHeight
    else if AControl.LCLObject.Height > 0 then
      nat_height^ := AControl.LCLObject.Height;
  end;

  if Assigned(AControl.LCLObject.Constraints) then
    with AControl.LCLObject.Constraints do
    begin
      if MaxHeight > 0 then
      begin
        if min_height^ > MaxHeight then min_height^ := MaxHeight;
        if nat_height^ > MaxHeight then nat_height^ := MaxHeight;
      end;
      if MinHeight > 0 then
      begin
        if min_height^ < MinHeight then min_height^ := MinHeight;
        if nat_height^ < MinHeight then nat_height^ := MinHeight;
      end;
    end;
  if nat_height^ < min_height^ then nat_height^ := min_height^;
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
var
  Query: TGTypeQuery;
  Info: TGTypeInfo;
  ParentType: TGType;
begin
  if LCLGtkEntryType = 0 then
  begin
    ParentType := gtk_entry_get_type;
    g_type_query(ParentType, @Query);
    FillChar(Info{%H-}, SizeOf(Info), 0);
    Info.class_size    := guint16(Query.class_size);
    Info.class_init    := @LCLGtkEntryClassInit;
    Info.instance_size := guint16(Query.instance_size);
    Info.instance_init := @LCLGtkEntryInstanceInit;
    LCLGtkEntryType := g_type_register_static(ParentType,
      'LCLGtkEntry', @Info, G_TYPE_FLAG_NONE);
  end;
  Result := LCLGtkEntryType;
end;

function LCLGtkEntryNew: PGtkEntry;
begin
  Result := PGtkEntry(g_object_new(LCLGtkEntryGetType(), 'editable', [gboolean(True), nil]));
end;


