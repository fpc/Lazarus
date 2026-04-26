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
  begin
    if (AControl.LCLWidth > 0) and (nat_width^ > AControl.LCLWidth) then
      nat_width^ := AControl.LCLWidth;
    min_width^ := 1;
    Exit;
  end;

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

var
  LCLGtkButtonCssProvider: PGtkCssProvider = nil;

const
  LCLBtnTightClass: PChar = 'lcl-tight';
  LCLBtnExtWKey: PChar = 'lcl-theme-extw';
  LCLBtnExtHKey: PChar = 'lcl-theme-exth';
  LCLButtonCssData: PChar =
    'button.lcl-tight, button.lcl-tight.text-button, button.lcl-tight.image-button {' +
    '  transition: none;' +
    '  padding-left: 0;' +
    '  padding-right: 0;' +
    '  padding-top: 0;' +
    '  padding-bottom: 0;' +
    '  margin: 0;' +
    '  min-width: 0;' +
    '  min-height: 0;' +
    '}';

procedure LCLGtkButtonEnsureCssProvider(widget: PGtkWidget);
var
  Err: PGError;
  Ok: gboolean;
  AControl: TGtk3Widget;
  ALclObj: TObject;
begin
  if LCLGtkButtonCssProvider <> nil then
    Exit;
  LCLGtkButtonCssProvider := gtk_css_provider_new;
  Err := nil;
  Ok := gtk_css_provider_load_from_data(LCLGtkButtonCssProvider, LCLButtonCssData, -1, @Err);
  if not Ok then
  begin
    ALclObj := nil;
    AControl := TGtk3Widget(HwndFromGtkWidget(widget));
    if Assigned(AControl) then
      ALclObj := AControl.LCLObject;
    if Assigned(Err) then
      DebugLn('Gtk3: LCLGtkButton CSS provider load_from_data failed (',
        DbgSName(ALclObj), '): ', Err^.message)
    else
      DebugLn('Gtk3: LCLGtkButton CSS provider load_from_data failed (',
        DbgSName(ALclObj), ')');
  end;
  if Assigned(Err) then
    g_error_free(Err);
  gtk_style_context_add_provider_for_screen(gdk_screen_get_default,
    PGtkStyleProvider(LCLGtkButtonCssProvider),
    GTK_STYLE_PROVIDER_PRIORITY_USER);
end;

procedure LCLGtkButtonSizeAllocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  StyleCtx: PGtkStyleContext;
  Padding, Border, Margin: TGtkBorder;
  ExtentsW, ExtentsH: Integer;
  NeedTight, HasTight: Boolean;
begin
  if not Gtk3IsWidget(widget) or not Assigned(allocation) then
    Exit;

  LCLGtkButtonEnsureCssProvider(widget);

  StyleCtx := widget^.get_style_context;
  if Assigned(StyleCtx) then
  begin
    HasTight := StyleCtx^.has_class(LCLBtnTightClass);
    if not HasTight then
    begin
      StyleCtx^.get_padding(StyleCtx^.get_state, @Padding);
      StyleCtx^.get_border(StyleCtx^.get_state, @Border);
      StyleCtx^.get_margin(StyleCtx^.get_state, @Margin);
      ExtentsW := Padding.left + Padding.right + Border.left + Border.right + Margin.left + Margin.right;
      ExtentsH := Padding.top + Padding.bottom + Border.top + Border.bottom + Margin.top + Margin.bottom;
      g_object_set_data(PGObject(widget), LCLBtnExtWKey, Pointer(PtrInt(ExtentsW)));
      g_object_set_data(PGObject(widget), LCLBtnExtHKey, Pointer(PtrInt(ExtentsH)));
    end else
    begin
      ExtentsW := PtrInt(g_object_get_data(PGObject(widget), LCLBtnExtWKey));
      ExtentsH := PtrInt(g_object_get_data(PGObject(widget), LCLBtnExtHKey));
    end;
    NeedTight := (allocation^.width <= ExtentsW) or (allocation^.height <= ExtentsH);
    if NeedTight <> HasTight then
    begin
      if NeedTight then
        StyleCtx^.add_class(LCLBtnTightClass)
      else
        StyleCtx^.remove_class(LCLBtnTightClass);
    end;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if Assigned(ParentClass) and Assigned(ParentClass^.size_allocate) then
    ParentClass^.size_allocate(widget, allocation);
end;

procedure LCLGtkButtonClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkButtonGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkButtonGetPreferredHeight;
  AWidgetClass^.size_allocate := @LCLGtkButtonSizeAllocate;
end;

procedure LCLGtkButtonInstanceInit({%H-}instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
begin
  //
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


