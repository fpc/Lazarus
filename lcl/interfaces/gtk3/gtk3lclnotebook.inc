{%MainUnit gtk3widgets.pas}
const
  GTK_NOTEBOOK_CLASS_SIZE = SizeOf(TGtkNotebookClass);
  GTK_NOTEBOOK_INSTANCE_SIZE = SizeOf(TGtkNotebook);

procedure LCLGtkNotebookGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredWidth invalid params.');
    Exit;
  end;

  min_width^ := 0;
  nat_width^ := 0;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredWidth widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredWidth cannot get ParentClass !');
    Exit;
  end;

  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
    Exit;

  //Keep min_width from the parent class: it encodes the tab-bar CSS minimum.
  //Resetting it to 0 allows GTK3 to allocate < CSS minimum, causing negative
  //content widths in the CSS gadget distributor. Fixes warning msgs in console.
  if AControl.LCLWidth = 0 then
    nat_width^ := Max(min_width^, AControl.LCLObject.Width)
  else
    nat_width^ := Max(min_width^, AControl.LCLWidth);
end;

procedure LCLGtkNotebookGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredHeight invalid params.');
    Exit;
  end;

  min_height^ := 0;
  nat_height^ := 0;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredHeight widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredHeight cannot get ParentClass !');
    Exit;
  end;

  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
    Exit;

  //Keep min_height from the parent class: it encodes the CSS minimum for the
  //tab bar. Resetting it to 0 allowed GTK3 to hand us allocations smaller
  //than the tab bar needs, driving the CSS gadget distributor negative and
  //producing size >= 0 assertion failures in GtkScrollbar / LCLGtkNotebook.
  if AControl.LCLHeight = 0 then
    nat_height^ := Max(min_height^, AControl.LCLObject.Height)
  else
    nat_height^ := Max(min_height^, AControl.LCLHeight);
end;

procedure LCLGtkNotebookSizeAllocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_h, nat_h: gint;
  ClampedAlloc: TGtkAllocation;
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;
  //Clamp allocation height to the parent-class CSS minimum.
  //size_allocate bypasses get_preferred_height, so even a correct minimum
  //returned there won't prevent direct allocations smaller than the tab-bar
  //height, which drive the CSS gadget distributor negative and produce
  //size >= 0 assertions in LCLGtkNotebook, GtkScrollbar and GtkScrolledWindow.
  ParentClass^.get_preferred_height(widget, @min_h, @nat_h);
  ClampedAlloc := allocation^;
  if ClampedAlloc.height < min_h then
    ClampedAlloc.height := min_h;
  ParentClass^.size_allocate(widget, @ClampedAlloc);
end;

procedure LCLGtkNotebookClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkNotebookGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkNotebookGetPreferredHeight;
  AWidgetClass^.size_allocate := @LCLGtkNotebookSizeAllocate;
end;

procedure LCLGtkNotebookInstanceInit({%H-}instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
begin
  //
end;

var
  LCLGtkNotebookType: TGType = 0;

function LCLGtkNotebookGetType: TGType; cdecl;
const
  lcl_Notebook_type_info: TGTypeInfo = (
    class_size: GTK_NOTEBOOK_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkNotebookClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_NOTEBOOK_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkNotebookInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkNotebookType = 0 then
    LCLGtkNotebookType := g_type_register_static(gtk_notebook_get_type, 'LCLGtkNotebook', @lcl_notebook_type_info, G_TYPE_FLAG_NONE);
  Result := LCLGtkNotebookType;
end;

function LCLGtkNotebookNew: PGtkNotebook;
begin                                                         {in gtk3 this is default}
  Result := PGtkNotebook(g_object_new(LCLGtkNotebookGetType(),'show-tabs',[gboolean(True), nil]));
end;




