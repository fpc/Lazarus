{%MainUnit gtk3widgets.pas}

(*
  Owner-draw and measure subclasses for all LCL menu item variants.

 Four GType subclasses are registered here:
   LCLGtkMenuItem: inherits from GtkMenuItem
   LCLGtkCheckMenuItem: inherits from GtkCheckMenuItem
   LCLGtkRadioMenuItem: inherits from GtkRadioMenuItem
   LCLGtkImageMenuItem: inherits from GtkImageMenuItem

 Each overrides get_preferred_width/height so that TMenuItem.DoMeasureItem
 can influence the item size. Each var is called once !

 Owner-draw  - TMenuItem.DoDrawItem is handled by the MenuItemDraw signal
 handler connected for every item type in ConnectMenuSignals. That approach
 works uniformly across all GTK menu item types without per-type vfunc
 overrides. Gtk2 does not have implementation for OwnerDraw at all,
 my tests shows that this approach works ok. zeljan.
 *)

const
  GTK_MENU_ITEM_CLASS_SIZE = SizeOf(TGtkMenuItemClass);
  GTK_MENU_ITEM_INSTANCE_SIZE = SizeOf(TGtkMenuItem);
  GTK_CHECK_MENU_ITEM_CLASS_SIZE = SizeOf(TGtkCheckMenuItemClass);
  GTK_CHECK_MENU_ITEM_INSTANCE_SIZE = SizeOf(TGtkCheckMenuItem);
  GTK_RADIO_MENU_ITEM_CLASS_SIZE = SizeOf(TGtkRadioMenuItemClass);
  GTK_RADIO_MENU_ITEM_INSTANCE_SIZE = SizeOf(TGtkRadioMenuItem);
  GTK_IMAGE_MENU_ITEM_CLASS_SIZE = SizeOf(TGtkImageMenuItemClass);
  GTK_IMAGE_MENU_ITEM_INSTANCE_SIZE = SizeOf(TGtkImageMenuItem);

function LCLGtkMenuItemMakeMeasureCairo: Pcairo_t;
var
  Surf: Pcairo_surface_t;
begin
  Surf := cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
  Result := cairo_create(Surf);
  cairo_surface_destroy(Surf);
end;

function LCLGtkMenuItemMeasure(AMenuItem: TMenuItem;
  var AWidth, AHeight: Integer): Boolean;
var
  ACairo: Pcairo_t;
  ADC: TGtk3DeviceContext;
  ACanvas: TCanvas;
begin
  Result := False;
  ACairo := LCLGtkMenuItemMakeMeasureCairo;
  if not Assigned(ACairo) then
    Exit;

  ADC := TGtk3DeviceContext.CreateFromCairo(nil, ACairo);
  ACanvas := TCanvas.Create;
  ACanvas.Handle := HDC(ADC);
  try
    Result := AMenuItem.GTK3MeasureItem(ACanvas, AWidth, AHeight);
  finally
    ACanvas.Handle := 0;
    ACanvas.Free;
    ADC.Free;
    cairo_destroy(ACairo);
  end;
end;

procedure LCLGtkMenuItemGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  GtkItem: TGtk3MenuItem;
  AMenuItem: TMenuItem;
  AParentMenu: TMenu;
  AWidth, AHeight: Integer;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
    Exit;
  min_width^ := 0;
  nat_width^ := 0;
  if not Gtk3IsWidget(widget) then
    Exit;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
    Exit;
  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  GtkItem := TGtk3MenuItem(HwndFromGtkWidget(widget));
  if not Assigned(GtkItem) or not Assigned(GtkItem.MenuItem) then
    Exit;
  AMenuItem := GtkItem.MenuItem;

  // Skip when no measure handler is registered
  if not Assigned(AMenuItem.OnMeasureItem) then
  begin
    AParentMenu := AMenuItem.GetParentMenu;
    if not Assigned(AParentMenu) or not Assigned(AParentMenu.OnMeasureItem) then
      Exit;
  end;

  AWidth := nat_width^;
  AHeight := 0;
  if LCLGtkMenuItemMeasure(AMenuItem, AWidth, AHeight) then
    nat_width^ := Max(min_width^, AWidth);
end;

procedure LCLGtkMenuItemGetPreferredHeight(widget: PGtkWidget;
  min_height, nat_height: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  GtkItem: TGtk3MenuItem;
  AMenuItem: TMenuItem;
  AParentMenu: TMenu;
  AWidth, AHeight: Integer;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
    Exit;
  min_height^ := 0;
  nat_height^ := 0;
  if not Gtk3IsWidget(widget) then
    Exit;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
    Exit;
  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  // GtkMenuBar items share one bar height, overriding nat_height here
  // propagates to the entire bar, squeezes sibling widgets into negative
  // allocations and causes a resize loop.
  if Gtk3WidgetIsA(widget^.get_parent, gtk_menu_bar_get_type) then
    Exit;

  GtkItem := TGtk3MenuItem(HwndFromGtkWidget(widget));
  if not Assigned(GtkItem) or not Assigned(GtkItem.MenuItem) then
    Exit;
  AMenuItem := GtkItem.MenuItem;

  if not Assigned(AMenuItem.OnMeasureItem) then
  begin
    AParentMenu := AMenuItem.GetParentMenu;
    if not Assigned(AParentMenu) or not Assigned(AParentMenu.OnMeasureItem) then
      Exit;
  end;

  AWidth := 0;
  AHeight := nat_height^;
  if LCLGtkMenuItemMeasure(AMenuItem, AWidth, AHeight) then
    nat_height^ := Max(min_height^, AHeight);
end;

procedure LCLGtkMenuItemAnyInstanceInit({%H-}instance: PGTypeInstance;
  {%H-}klass: PGTypeClass); cdecl;
begin
end;

procedure LCLGtkMenuItemClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  W: PGtkWidgetClass;
begin
  W := PGtkWidgetClass(klass);
  W^.get_preferred_width := @LCLGtkMenuItemGetPreferredWidth;
  W^.get_preferred_height := @LCLGtkMenuItemGetPreferredHeight;
end;

var
  LCLGtkMenuItemType: TGType = 0;

function LCLGtkMenuItemGetType: TGType;
const
  Info: TGTypeInfo = (
    class_size: GTK_MENU_ITEM_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkMenuItemClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_MENU_ITEM_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkMenuItemAnyInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkMenuItemType = 0 then
    LCLGtkMenuItemType := g_type_register_static(gtk_menu_item_get_type,
      'LCLGtkMenuItem', @Info, G_TYPE_FLAG_NONE);
  Result := LCLGtkMenuItemType;
end;

function LCLGtkMenuItemNew: PGtkMenuItem;
begin
  Result := PGtkMenuItem(g_object_newv(LCLGtkMenuItemGetType, 0, nil));
end;

procedure LCLGtkCheckMenuItemClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  W: PGtkWidgetClass;
begin
  W := PGtkWidgetClass(klass);
  W^.get_preferred_width := @LCLGtkMenuItemGetPreferredWidth;
  W^.get_preferred_height := @LCLGtkMenuItemGetPreferredHeight;
end;

var
  LCLGtkCheckMenuItemType: TGType = 0;

function LCLGtkCheckMenuItemGetType: TGType;
const
  Info: TGTypeInfo = (
    class_size: GTK_CHECK_MENU_ITEM_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkCheckMenuItemClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_CHECK_MENU_ITEM_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkMenuItemAnyInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkCheckMenuItemType = 0 then
    LCLGtkCheckMenuItemType := g_type_register_static(gtk_check_menu_item_get_type,
      'LCLGtkCheckMenuItem', @Info, G_TYPE_FLAG_NONE);
  Result := LCLGtkCheckMenuItemType;
end;

function LCLGtkCheckMenuItemNew: PGtkCheckMenuItem;
begin
  Result := PGtkCheckMenuItem(g_object_newv(LCLGtkCheckMenuItemGetType, 0, nil));
end;

procedure LCLGtkRadioMenuItemClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  W: PGtkWidgetClass;
begin
  W := PGtkWidgetClass(klass);
  W^.get_preferred_width := @LCLGtkMenuItemGetPreferredWidth;
  W^.get_preferred_height := @LCLGtkMenuItemGetPreferredHeight;
end;

var
  LCLGtkRadioMenuItemType: TGType = 0;

function LCLGtkRadioMenuItemGetType: TGType;
const
  Info: TGTypeInfo = (
    class_size: GTK_RADIO_MENU_ITEM_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkRadioMenuItemClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_RADIO_MENU_ITEM_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkMenuItemAnyInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkRadioMenuItemType = 0 then
    LCLGtkRadioMenuItemType := g_type_register_static(gtk_radio_menu_item_get_type,
      'LCLGtkRadioMenuItem', @Info, G_TYPE_FLAG_NONE);
  Result := LCLGtkRadioMenuItemType;
end;

function LCLGtkRadioMenuItemNew: PGtkRadioMenuItem;
begin
  Result := PGtkRadioMenuItem(g_object_newv(LCLGtkRadioMenuItemGetType, 0, nil));
end;

procedure LCLGtkImageMenuItemClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  W: PGtkWidgetClass;
begin
  W := PGtkWidgetClass(klass);
  W^.get_preferred_width := @LCLGtkMenuItemGetPreferredWidth;
  W^.get_preferred_height := @LCLGtkMenuItemGetPreferredHeight;
end;

var
  LCLGtkImageMenuItemType: TGType = 0;

function LCLGtkImageMenuItemGetType: TGType;
const
  Info: TGTypeInfo = (
    class_size: GTK_IMAGE_MENU_ITEM_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkImageMenuItemClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_IMAGE_MENU_ITEM_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkMenuItemAnyInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkImageMenuItemType = 0 then
    LCLGtkImageMenuItemType := g_type_register_static(gtk_image_menu_item_get_type,
      'LCLGtkImageMenuItem', @Info, G_TYPE_FLAG_NONE);
  Result := LCLGtkImageMenuItemType;
end;

function LCLGtkImageMenuItemNew: PGtkImageMenuItem;
begin
  Result := PGtkImageMenuItem(g_object_newv(LCLGtkImageMenuItemGetType, 0, nil));
end;
