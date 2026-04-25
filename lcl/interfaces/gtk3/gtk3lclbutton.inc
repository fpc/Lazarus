{%MainUnit gtk3widgets.pas}

procedure LCLGtkButtonGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
  LCLW: Integer;
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

  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) or not Assigned(AControl.LCLObject) then
    Exit;

  if AControl.LCLObject.AutoSize then
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

procedure LCLGtkButtonGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
  LCLH: Integer;
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

  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) or not Assigned(AControl.LCLObject) then
    Exit;

  if AControl.LCLObject.AutoSize then
    Exit;

  if AControl.LCLHeight > 0 then
    LCLH := AControl.LCLHeight
  else
    LCLH := AControl.LCLObject.Height;

  if LCLH > 0 then
  begin
    if min_height^ > LCLH then
      min_height^ := LCLH;
    nat_height^ := LCLH;
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

procedure LCLGtkButtonInstanceInit({%H-}instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
//var
//  buttonWidget: PGtkButton;
begin
  //buttonWidget := PGtkButton(instance);
  // Custom initialization logic (if needed)
end;

var
  LCLGtkButtonType: TGType = 0;

function LCLGtkButtonGetType: TGType; cdecl;
var
  Query: TGTypeQuery;
  Info: TGTypeInfo;
  ParentType: TGType;
begin
  if LCLGtkButtonType = 0 then
  begin
    ParentType := gtk_button_get_type;
    g_type_query(ParentType, @Query);
    FillChar(Info{%H-}, SizeOf(Info), 0);
    Info.class_size    := guint16(Query.class_size);
    Info.class_init    := @LCLGtkButtonClassInit;
    Info.instance_size := guint16(Query.instance_size);
    Info.instance_init := @LCLGtkButtonInstanceInit;
    LCLGtkButtonType := g_type_register_static(ParentType,
      'LCLGtkButton', @Info, G_TYPE_FLAG_NONE);
  end;
  Result := LCLGtkButtonType;
end;

function LCLGtkButtonNew: PGtkButton;
begin
  Result := PGtkButton(g_object_new(LCLGtkButtonGetType(),'label',['', nil]));
end;


