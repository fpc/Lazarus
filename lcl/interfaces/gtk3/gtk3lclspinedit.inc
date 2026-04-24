{%MainUnit gtk3widgets.pas}

{
  LCLGtkSpinEdit - Custom composite spin edit widget for GTK3.
  Replaces GtkSpinButton which has horizontal +/- buttons, and does not fit
  into Lazarus philosophy of spin controls which have vertical up-down.
  Layout: [GtkEntry 4/5 width][UpDown 1/5 width]
  UpDown is a vertical GtkBox with up/down arrow buttons.
  Uses GtkAdjustment for value/range/step management.
  Created by Željan Rikalo
}

const
  LCL_SPIN_BUTTON_WIDTH_RATIO = 5;


procedure LCLSpinEditUpClicked(AButton: PGtkButton; AData: gpointer); cdecl; forward;
procedure LCLSpinEditDownClicked(AButton: PGtkButton; AData: gpointer); cdecl; forward;
procedure LCLSpinEditEntryChanged(AEditable: PGtkEditable; AData: gpointer); cdecl; forward;
function LCLSpinEditEntryKeyPress(AWidget: PGtkWidget; AEvent: PGdkEventKey; AData: gpointer): gboolean; cdecl; forward;
function LCLSpinEditEntryScroll(AWidget: PGtkWidget; AEvent: PGdkEventScroll; AData: gpointer): gboolean; cdecl; forward;
procedure LCLSpinEditAdjValueChanged(AAdjustment: PGtkAdjustment; AData: gpointer); cdecl; forward;
procedure LCLSpinEditInsertText(AEditable: PGtkEditable; ANewText: PgChar;
  ANewTextLen: gint; APosition: Pgint; AData: gpointer); cdecl; forward;
function LCLSpinEditGetData(AWidget: PGtkWidget): PLCLSpinEditData; forward;
function LCLSpinEditEntryFocusEvent(AWidget: PGtkWidget; AEvent: PGdkEvent; AData: gpointer): gboolean; cdecl; forward;
function LCLSpinEditBoxFocus(AWidget: PGtkWidget; ADirection: TGtkDirectionType; AData: gpointer): gboolean; cdecl; forward;
procedure LCLSpinEditPasteClipboard(AEntry: PGtkEntry; AData: gpointer); cdecl; forward;
function LCLSpinEditChildEnterLeave(AWidget: PGtkWidget; AEvent: PGdkEventCrossing; AData: gpointer): gboolean; cdecl; forward;

const
  LCL_SPIN_DATA_KEY = 'lcl-spin-data';


function LCLSpinFormatValue(AValue: Double; ADigits: guint): String;
begin
  if ADigits = 0 then
    Result := IntToStr(Round(AValue))
  else
    Result := FloatToStrF(AValue, ffFixed, 15, ADigits);
end;

procedure LCLSpinUpdateEntryFromAdj(AData: PLCLSpinEditData);
var
  S: String;
begin
  if AData^.UpdatingEntry then
    Exit;
  AData^.UpdatingEntry := True;
  try
    S := LCLSpinFormatValue(AData^.Adjustment^.get_value, AData^.Digits);
    AData^.Entry^.set_text(PgChar(S));
  finally
    AData^.UpdatingEntry := False;
  end;
end;


procedure LCLSpinEditUpClicked(AButton: PGtkButton; AData: gpointer); cdecl;
var
  Data: PLCLSpinEditData;
  AStep, APage, AVal, AMax: gdouble;
begin
  Data := PLCLSpinEditData(AData);
  if Data^.IsReadOnly then Exit;
  if Assigned(AButton) and Assigned(Data^.Entry) and
     not PGtkWidget(Data^.Entry)^.has_focus then
    PGtkWidget(Data^.Entry)^.grab_focus;
  AStep := Data^.Adjustment^.get_step_increment;
  AVal := Data^.Adjustment^.get_value;
  AMax := Data^.Adjustment^.get_upper;
  if AVal + AStep <= AMax then
    Data^.Adjustment^.set_value(AVal + AStep)
  else if AVal < AMax then
    Data^.Adjustment^.set_value(AMax);
end;

procedure LCLSpinEditDownClicked(AButton: PGtkButton; AData: gpointer); cdecl;
var
  Data: PLCLSpinEditData;
  AStep, AVal, AMin: gdouble;
begin
  Data := PLCLSpinEditData(AData);
  if Data^.IsReadOnly then Exit;
  if Assigned(AButton) and Assigned(Data^.Entry) and
     not PGtkWidget(Data^.Entry)^.has_focus then
    PGtkWidget(Data^.Entry)^.grab_focus;
  AStep := Data^.Adjustment^.get_step_increment;
  AVal := Data^.Adjustment^.get_value;
  AMin := Data^.Adjustment^.get_lower;
  if AVal - AStep >= AMin then
    Data^.Adjustment^.set_value(AVal - AStep)
  else if AVal > AMin then
    Data^.Adjustment^.set_value(AMin);
end;

function LCLSpinEditUpButtonPress(AWidget: PGtkWidget; AEvent: PGdkEventButton; AData: gpointer): gboolean; cdecl;
begin
  if (AEvent <> nil) and
     (AEvent^.type_ in [GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS]) and
     (AEvent^.button = 1) then
    LCLSpinEditUpClicked(PGtkButton(AWidget), AData);
  Result := False;
end;

function LCLSpinEditDownButtonPress(AWidget: PGtkWidget; AEvent: PGdkEventButton; AData: gpointer): gboolean; cdecl;
begin
  if (AEvent <> nil) and
     (AEvent^.type_ in [GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS]) and
     (AEvent^.button = 1) then
    LCLSpinEditDownClicked(PGtkButton(AWidget), AData);
  Result := False;
end;

procedure LCLSpinEditEntryChanged(AEditable: PGtkEditable; AData: gpointer); cdecl;
var
  Data: PLCLSpinEditData;
  S: String;
  V, AMin, AMax: Double;
begin
  Data := PLCLSpinEditData(AData);
  if Data^.UpdatingEntry then
    Exit;
  S := Data^.Entry^.get_text;
  if TryStrToFloat(S, V) then
  begin
    AMin := Data^.Adjustment^.get_lower;
    AMax := Data^.Adjustment^.get_upper;
    if V < AMin then V := AMin;
    if V > AMax then V := AMax;
    Data^.UpdatingEntry := True;
    try
      Data^.Adjustment^.set_value(V);
    finally
      Data^.UpdatingEntry := False;
    end;
  end;
end;

function LCLSpinEditEntryKeyPress(AWidget: PGtkWidget; AEvent: PGdkEventKey; AData: gpointer): gboolean; cdecl;
var
  Data: PLCLSpinEditData;
begin
  Result := False;
  Data := PLCLSpinEditData(AData);
  case AEvent^.keyval of
    GDK_KEY_Up:
    begin
      LCLSpinEditUpClicked(nil, AData);
      Result := True;
    end;
    GDK_KEY_Down:
    begin
      LCLSpinEditDownClicked(nil, AData);
      Result := True;
    end;
  end;
end;

function LCLSpinEditEntryScroll(AWidget: PGtkWidget; AEvent: PGdkEventScroll; AData: gpointer): gboolean; cdecl;
var
  Data: PLCLSpinEditData;
begin
  Result := True;
  Data := PLCLSpinEditData(AData);
  case AEvent^.direction of
    GDK_SCROLL_UP:
      LCLSpinEditUpClicked(nil, AData);
    GDK_SCROLL_DOWN:
      LCLSpinEditDownClicked(nil, AData);
  else
    Result := False;
  end;
  //Select all text after scroll (Qt6, win32 behavior) - next key input replaces value.
  //gtk2 have ugly behaviour when wheeling over spin.
  if Result and Assigned(Data^.Entry) then
    PGtkEditable(Data^.Entry)^.select_region(0, -1);
end;

procedure LCLSpinEditAdjValueChanged(AAdjustment: PGtkAdjustment; AData: gpointer); cdecl;
var
  Data: PLCLSpinEditData;
begin
  Data := PLCLSpinEditData(AData);
  LCLSpinUpdateEntryFromAdj(Data);
end;

procedure LCLSpinEditInsertText(AEditable: PGtkEditable; ANewText: PgChar;
  ANewTextLen: gint; APosition: Pgint; AData: gpointer); cdecl;
var
  Data: PLCLSpinEditData;
  I: Integer;
  C: Char;
  Allow: Boolean;
begin
  Data := PLCLSpinEditData(AData);
  if Data^.UpdatingEntry then
    Exit;
  //wtSpinEdit always validates input - only numbers allowed
  for I := 0 to ANewTextLen - 1 do
  begin
    C := ANewText[I];
    Allow := (C in ['0'..'9', '-', '+']) or
             ((Data^.Digits > 0) and (C = DefaultFormatSettings.DecimalSeparator)) or
             (C = DefaultFormatSettings.ThousandSeparator);
    if not Allow then
    begin
      g_signal_stop_emission_by_name(PGObject(AEditable), 'insert-text');
      Exit;
    end;
  end;
end;

procedure LCLSpinEditPasteClipboard(AEntry: PGtkEntry; AData: gpointer); cdecl;
var
  Data: PLCLSpinEditData;
  AClipboard: PGtkClipboard;
  AText: PgChar;
  V: Double;
  APos: gint;
begin
  g_signal_stop_emission_by_name(PGObject(AEntry), 'paste-clipboard');
  Data := PLCLSpinEditData(AData);
  if Data^.IsReadOnly or not Data^.EditorEnabled then
    Exit;
  AClipboard := gtk_clipboard_get(gdk_atom_intern('CLIPBOARD', False));
  AText := gtk_clipboard_wait_for_text(AClipboard);
  if AText = nil then Exit;
  try
    //Only paste if clipboard content is a valid number, idea used from qt6
    if TryStrToFloat(String(AText), V) then
    begin
      PGtkEditable(AEntry)^.delete_selection;
      APos := PGtkEditable(AEntry)^.get_position;
      PGtkEditable(AEntry)^.insert_text(AText, Length(String(AText)), @APos);
      PGtkEditable(AEntry)^.set_position(APos);
    end;
  finally
    g_free(AText);
  end;
end;

function LCLSpinEditEntryFocusEvent(AWidget: PGtkWidget; AEvent: PGdkEvent;
  AData: gpointer): gboolean; cdecl;
var
  ACtl: TGtk3Widget;
begin
  Result := False;
  ACtl := TGtk3Widget(AData);
  if not Assigned(ACtl) or not Assigned(ACtl.LCLObject) then
    Exit;
  ACtl.GtkEventFocus(AWidget, AEvent);
end;

function LCLSpinEditBoxFocus(AWidget: PGtkWidget; ADirection: TGtkDirectionType;
  AData: gpointer): gboolean; cdecl;
var
  Data: PLCLSpinEditData;
begin
  Result := False;
  Data := LCLSpinEditGetData(AWidget);
  if not Assigned(Data) or not Assigned(Data^.Entry) then
    Exit;
  if PGtkWidget(Data^.Entry)^.is_focus then
  begin
    //Entry is already focused, let GTK move focus to next widget outside our container
    Result := False;
  end else
  begin
    //Entry not focused, grab it, prevent GTK from traversing buttons.
    PGtkWidget(Data^.Entry)^.grab_focus;
    Result := True;
  end;
end;

function LCLSpinEditChildEnterLeave(AWidget: PGtkWidget; AEvent: PGdkEventCrossing;
  AData: gpointer): gboolean; cdecl;
var
  ACtl: TGtk3Widget;
  Data: PLCLSpinEditData;
  Msg: TLMessage;
  SubWidgetPtr: gpointer;
  Alloc: TGtkAllocation;
  mx, my: gint;
  ADevice: PGdkDevice;
  Inside: Boolean;
  P: TPoint;
begin
  Result := False;
  if AEvent^.mode <> GDK_CROSSING_NORMAL then
    Exit;
  ACtl := TGtk3Widget(AData);
  if not Assigned(ACtl) or not Assigned(ACtl.LCLObject) then
    Exit;
  Data := LCLSpinEditGetData(ACtl.Widget);
  if not Assigned(Data) then Exit;

  if AEvent^.type_ = GDK_ENTER_NOTIFY then
  begin
    if not Data^.MouseInside then
    begin
      Data^.MouseInside := True;
      FillChar(Msg{%H-}, SizeOf(Msg), 0);
      Msg.Msg := CM_MOUSEENTER;
      ACtl.DeliverMessage(Msg, True);
    end;
  end
  else // GDK_LEAVE_NOTIFY
  begin
    if Data^.MouseInside then
    begin
      // Try subwindow check first (X11 may set it)
      if Assigned(AEvent^.subwindow) then
      begin
        SubWidgetPtr := nil;
        gdk_window_get_user_data(AEvent^.subwindow, @SubWidgetPtr);
        if Assigned(SubWidgetPtr) and
           gtk_widget_is_ancestor(PGtkWidget(SubWidgetPtr), ACtl.Widget) then
          Exit;
      end;
      // Use ScreenToClient then HBox allocation for bounds check
      ADevice := gdk_event_get_device(PGdkEvent(AEvent));
      if Assigned(ADevice) then
      begin
        gdk_device_get_position(ADevice, nil, @mx, @my);
        P.X := mx;
        P.Y := my;
        P := ACtl.LCLObject.ScreenToClient(P);
        ACtl.Widget^.get_allocation(@Alloc);
        Inside := (P.X >= 0) and (P.Y >= 0) and
                  (P.X < Alloc.width) and (P.Y < Alloc.height);
        if Inside then
          Exit;
      end;
      Data^.MouseInside := False;
      FillChar(Msg{%H-}, SizeOf(Msg), 0);
      Msg.Msg := CM_MOUSELEAVE;
      ACtl.DeliverMessage(Msg, True);
    end;
  end;
end;

procedure LCLSpinEditGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then Exit;
  if not Gtk3IsWidget(widget) then Exit;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then Exit;

  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then Exit;

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

procedure LCLSpinEditGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  Data: PLCLSpinEditData;
  EntryMinH, EntryNatH: gint;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then Exit;
  if not Gtk3IsWidget(widget) then Exit;

  Data := LCLSpinEditGetData(widget);
  if Assigned(Data) and Assigned(Data^.Entry) then
  begin
    PGtkWidget(Data^.Entry)^.get_preferred_height(@EntryMinH, @EntryNatH);
    min_height^ := EntryMinH;
    nat_height^ := EntryNatH;
  end else
  begin
    with PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class))^ do
      if Assigned(get_preferred_height) then
        get_preferred_height(widget, min_height, nat_height);
  end;

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then Exit;

  // Respect ws height if autosize is true
  if AControl.LCLObject.AutoSize then
    Exit;

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

procedure LCLSpinEditSizeAllocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  Data: PLCLSpinEditData;
  BtnW: gint;
begin
  Data := LCLSpinEditGetData(widget);
  if Assigned(Data) and Assigned(Data^.BtnBox) then
  begin
    BtnW := Max(16, allocation^.height div 4);
    PGtkWidget(Data^.BtnBox)^.set_size_request(BtnW, -1);
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if Assigned(ParentClass) and Assigned(ParentClass^.size_allocate) then
    ParentClass^.size_allocate(widget, allocation);
end;

function LCLSpinEditFocus(AWidget: PGtkWidget; ADirection: TGtkDirectionType): gboolean; cdecl;
var
  Data: PLCLSpinEditData;
  EntryHasFocus: gboolean;
begin
  Result := False;
  Data := LCLSpinEditGetData(AWidget);
  if not Assigned(Data) or not Assigned(Data^.Entry) then
    Exit;
  EntryHasFocus := PGtkWidget(Data^.Entry)^.has_focus;
  if EntryHasFocus then
    Result := False
  else
  begin
    PGtkWidget(Data^.Entry)^.grab_focus;
    Result := True;
  end;
end;

procedure LCLSpinEditGrabFocus(AWidget: PGtkWidget); cdecl;
var
  Data: PLCLSpinEditData;
begin
  //Always redirect grab_focus to internal GtkEntry
  Data := LCLSpinEditGetData(AWidget);
  if Assigned(Data) and Assigned(Data^.Entry) then
    PGtkWidget(Data^.Entry)^.grab_focus
  else
  begin
    with PGtkWidgetClass(g_type_class_peek_parent(AWidget^.g_type_instance.g_class))^ do
      if Assigned(grab_focus) then
        grab_focus(AWidget);
  end;
end;

procedure LCLSpinEditClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLSpinEditGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLSpinEditGetPreferredHeight;
  AWidgetClass^.size_allocate := @LCLSpinEditSizeAllocate;
  AWidgetClass^.focus := @LCLSpinEditFocus;
  AWidgetClass^.grab_focus := @LCLSpinEditGrabFocus;
end;

procedure LCLSpinEditInstanceInit({%H-}instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
begin
end;

var
  LCLSpinEditType: TGType = 0;

function LCLSpinEditGetType: TGType;
var
  Query: TGTypeQuery;
  Info: TGTypeInfo;
  ParentType: TGType;
begin
  if LCLSpinEditType = 0 then
  begin
    ParentType := gtk_box_get_type;
    g_type_query(ParentType, @Query);
    FillChar(Info{%H-}, SizeOf(Info), 0);
    Info.class_size    := guint16(Query.class_size);
    Info.class_init    := @LCLSpinEditClassInit;
    Info.instance_size := guint16(Query.instance_size);
    Info.instance_init := @LCLSpinEditInstanceInit;
    LCLSpinEditType := g_type_register_static(ParentType,
      'LCLSpinEdit', @Info, G_TYPE_FLAG_NONE);
  end;
  Result := LCLSpinEditType;
end;

//heart of our customized SpinEdit
function LCLSpinEditNew(AMin, AMax, AStep: Double): PGtkBox;
var
  Data: PLCLSpinEditData;
  HBox: PGtkBox;
  BtnBox: PGtkBox;
  Entry: PGtkEntry;
  BtnUp, BtnDown: PGtkButton;
  Adj: PGtkAdjustment;
  AStyleCtx: PGtkStyleContext;
  ACSSProvider: PGtkCssProvider;
begin
  //HBox container is our FWidget, not focusable, works as focusProxy, but it is our ClientRect etc
  HBox := PGtkBox(g_object_new(LCLSpinEditGetType, 'orientation', [GTK_ORIENTATION_HORIZONTAL, 'spacing', 0, nil]));
  PGtkWidget(HBox)^.set_can_focus(False);

  Adj := PGtkAdjustment(gtk_adjustment_new(0, AMin, AMax, AStep, AStep * 10, 0));

  Entry := PGtkEntry(LCLGtkEntryNew);
  Entry^.set_hexpand(True);
  Entry^.set_halign(GTK_ALIGN_FILL);

  BtnUp := PGtkButton(gtk_button_new);
  BtnDown := PGtkButton(gtk_button_new);

  //use theme arrows or +-, whatever theme needs already scaled to the DPI
  PGtkContainer(BtnUp)^.add(PGtkWidget(gtk_image_new_from_icon_name('pan-up-symbolic', GTK_ICON_SIZE_BUTTON)));
  PGtkContainer(BtnDown)^.add(PGtkWidget(gtk_image_new_from_icon_name('pan-down-symbolic', GTK_ICON_SIZE_BUTTON)));

  ACSSProvider := gtk_css_provider_new;
  gtk_css_provider_load_from_data(ACSSProvider,
    'button.lcl-spin-btn { min-height: 0; min-width: 0; padding: 0px 2px; margin: 0; }', -1, nil);
  AStyleCtx := PGtkWidget(BtnUp)^.get_style_context;
  AStyleCtx^.add_class('lcl-spin-btn');
  AStyleCtx^.add_provider(PGtkStyleProvider(ACSSProvider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  AStyleCtx := PGtkWidget(BtnDown)^.get_style_context;
  AStyleCtx^.add_class('lcl-spin-btn');
  AStyleCtx^.add_provider(PGtkStyleProvider(ACSSProvider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(ACSSProvider);

  BtnUp^.set_relief(GTK_RELIEF_NORMAL);
  BtnDown^.set_relief(GTK_RELIEF_NORMAL);

  BtnUp^.set_can_focus(False);
  BtnDown^.set_can_focus(False);

  PGtkWidget(BtnUp)^.set_vexpand(True);
  PGtkWidget(BtnDown)^.set_vexpand(True);

  BtnBox := PGtkBox(gtk_box_new(GTK_ORIENTATION_VERTICAL, 0));
  PGtkWidget(BtnBox)^.set_can_focus(False);
  BtnBox^.set_homogeneous(True);
  BtnBox^.pack_start(PGtkWidget(BtnUp), True, True, 0);
  BtnBox^.pack_start(PGtkWidget(BtnDown), True, True, 0);

  HBox^.pack_start(PGtkWidget(Entry), True, True, 0);
  HBox^.pack_end(PGtkWidget(BtnBox), False, False, 0);

  AStyleCtx := PGtkWidget(HBox)^.get_style_context;
  AStyleCtx^.add_class('spinbutton');

  New(Data);
  Data^.Entry := Entry;
  Data^.BtnUp := BtnUp;
  Data^.BtnDown := BtnDown;
  Data^.BtnBox := BtnBox;
  Data^.Adjustment := Adj;
  Data^.Digits := 0;
  Data^.Numeric := False;
  Data^.UpdatingEntry := False;
  Data^.IsReadOnly := False;
  Data^.EditorEnabled := True;
  Data^.MouseInside := False;

  g_object_set_data(PGObject(HBox), LCL_SPIN_DATA_KEY, Data);

  g_signal_connect_data(PGObject(BtnUp), 'button-press-event',
    TGCallback(@LCLSpinEditUpButtonPress), Data, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(BtnDown), 'button-press-event',
    TGCallback(@LCLSpinEditDownButtonPress), Data, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(Entry), 'changed',
    TGCallback(@LCLSpinEditEntryChanged), Data, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(Entry), 'insert-text',
    TGCallback(@LCLSpinEditInsertText), Data, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(Entry), 'paste-clipboard',
    TGCallback(@LCLSpinEditPasteClipboard), Data, nil, G_CONNECT_DEFAULT);
  // key-press-event NOT connected, all key handling goes through WidgetEvent !!!
  g_signal_connect_data(PGObject(Entry), 'scroll-event',
    TGCallback(@LCLSpinEditEntryScroll), Data, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(Adj), 'value-changed',
    TGCallback(@LCLSpinEditAdjValueChanged), Data, nil, G_CONNECT_DEFAULT);

  Entry^.add_events(1 shl Ord(GDK_SCROLL_MASK) or
    1 shl Ord(GDK_ENTER_NOTIFY_MASK) or 1 shl Ord(GDK_LEAVE_NOTIFY_MASK));
  PGtkWidget(BtnBox)^.add_events(1 shl Ord(GDK_SCROLL_MASK) or
    1 shl Ord(GDK_ENTER_NOTIFY_MASK) or 1 shl Ord(GDK_LEAVE_NOTIFY_MASK));
  PGtkWidget(BtnUp)^.add_events(1 shl Ord(GDK_SCROLL_MASK) or
    1 shl Ord(GDK_ENTER_NOTIFY_MASK) or 1 shl Ord(GDK_LEAVE_NOTIFY_MASK));
  PGtkWidget(BtnDown)^.add_events(1 shl Ord(GDK_SCROLL_MASK) or
    1 shl Ord(GDK_ENTER_NOTIFY_MASK) or 1 shl Ord(GDK_LEAVE_NOTIFY_MASK));
  g_signal_connect_data(PGObject(BtnBox), 'scroll-event',
    TGCallback(@LCLSpinEditEntryScroll), Data, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(BtnUp), 'scroll-event',
    TGCallback(@LCLSpinEditEntryScroll), Data, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(BtnDown), 'scroll-event',
    TGCallback(@LCLSpinEditEntryScroll), Data, nil, G_CONNECT_DEFAULT);

  gtk_container_set_focus_chain(PGtkContainer(HBox), g_list_append(nil, Entry));

  LCLSpinUpdateEntryFromAdj(Data);

  PGtkWidget(HBox)^.show_all;

  Result := HBox;
end;

function LCLSpinEditGetData(AWidget: PGtkWidget): PLCLSpinEditData;
begin
  Result := PLCLSpinEditData(g_object_get_data(PGObject(AWidget), LCL_SPIN_DATA_KEY));
end;

function LCLSpinEditGetEntry(AWidget: PGtkWidget): PGtkEntry;
var
  Data: PLCLSpinEditData;
begin
  Data := LCLSpinEditGetData(AWidget);
  if Assigned(Data) then
    Result := Data^.Entry
  else
    Result := nil;
end;

function LCLSpinEditGetAdjustment(AWidget: PGtkWidget): PGtkAdjustment;
var
  Data: PLCLSpinEditData;
begin
  Data := LCLSpinEditGetData(AWidget);
  if Assigned(Data) then
    Result := Data^.Adjustment
  else
    Result := nil;
end;

procedure LCLSpinEditFreeData(AWidget: PGtkWidget);
var
  Data: PLCLSpinEditData;
begin
  Data := LCLSpinEditGetData(AWidget);
  if Assigned(Data) then
  begin
    g_object_set_data(PGObject(AWidget), LCL_SPIN_DATA_KEY, nil);
    Dispose(Data);
  end;
end;
