{%MainUnit gtk3widgets.pas}
const
  GTK_NOTEBOOK_CLASS_SIZE = SizeOf(TGtkNotebookClass);
  GTK_NOTEBOOK_INSTANCE_SIZE = SizeOf(TGtkNotebook);

{Return the CSS-minimum height of the notebook tab bar.
 The first call measures via get_preferred_height and caches the result in
 g_object_data so that subsequent SizeAllocate and Draw calls are free.
 min_h is stable for the widget lifetime under a given CSS theme, a stale
 value only affects the guard thresholds, not visual correctness.}
function LCLGtkNotebookGetCachedMinH(widget: PGtkWidget;
  ParentClass: PGtkWidgetClass): gint;
var
  nat_h: gint;
  cached: gpointer;
begin
  cached := g_object_get_data(PGObject(widget), 'lcl-nb-min-h');
  if cached <> nil then
    Result := gint(PtrInt(cached))
  else
  begin
    nat_h := 0;
    Result := 0;
    ParentClass^.get_preferred_height(widget, @Result, @nat_h);
    if Result > 0 then
      g_object_set_data(PGObject(widget), 'lcl-nb-min-h', gpointer(PtrInt(Result)));
  end;
end;

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
  min_h: gint;
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

  {Use cached min_h to avoid re-traversing the full widget tree on every
   layout pass. gtk_layout_allocate_child calls get_preferred_size on every
   child during each allocation — for a form with a 15-tab notebook this means
   15-label measurement on every resize frame. After the first measurement the
   CSS tab-bar minimum is stable; subsequent queries are answered from cache.
   LCLGtkNotebookGetCachedMinH measures via ParentClass^.get_preferred_height
   exactly once and stores the result in g_object_data lcl-nb-min-h.}
  min_h := LCLGtkNotebookGetCachedMinH(widget, ParentClass);
  min_height^ := min_h;

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
    Exit;

  //Keep min_height from the parent class: it encodes the CSS minimum for the
  //tab bar. Resetting it to 0 allowed GTK3 to hand us allocations smaller
  //than the tab bar needs, driving the CSS gadget distributor negative and
  //producing size >= 0 assertion failures in GtkScrollbar / LCLGtkNotebook.
  if AControl.LCLHeight = 0 then
    nat_height^ := Max(min_h, AControl.LCLObject.Height)
  else
    nat_height^ := Max(min_h, AControl.LCLHeight);
end;

procedure LCLGtkNotebookSizeAllocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_h: gint;
  ClampedAlloc: TGtkAllocation;
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;
  //Skip tiny placeholder allocations (height < 2) BEFORE clamping.
  if allocation^.height < 2 then
    Exit;
  //Clamp allocation height to the parent-class CSS minimum.
  //size_allocate bypasses get_preferred_height, so even a correct minimum
  //returned there won't prevent direct allocations smaller than the tab-bar
  //height, which drive the CSS gadget distributor negative and produce
  //size >= 0 assertions in LCLGtkNotebook, GtkScrollbar and GtkScrolledWindow.
  //Use cached min_h to avoid re-measuring the entire widget tree on every pass.
  min_h := LCLGtkNotebookGetCachedMinH(widget, ParentClass);
  ClampedAlloc := allocation^;
  if ClampedAlloc.height < min_h then
    ClampedAlloc.height := min_h;

  //Skip.The real allocation arrives once the parent
  //has real dimensions.
  if (min_h > 0) and (ClampedAlloc.height <= min_h) then
    Exit;
  ParentClass^.size_allocate(widget, @ClampedAlloc);
end;

function LCLGtkNotebookDraw(widget: PGtkWidget; cr: Pcairo_t): gboolean; cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_h: gint;
  alloc: TGtkAllocation;
begin
  Result := gboolean(False);
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.draw) then
    Exit;
  widget^.get_allocation(@alloc);
  //Guard against drawing when the allocated height is insufficient for the
  //tab-bar header CSS gadget.
  if alloc.height < 2 then
    Exit;
  //Use cached min_h — get_preferred_height traverses the full widget tree and
  //is too expensive to call on every draw frame during resize.
  min_h := LCLGtkNotebookGetCachedMinH(widget, ParentClass);
  if (min_h > 0) and (alloc.height < min_h) then
    Exit;
  Result := ParentClass^.draw(widget, cr);
end;

procedure LCLGtkNotebookClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkNotebookGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkNotebookGetPreferredHeight;
  AWidgetClass^.size_allocate := @LCLGtkNotebookSizeAllocate;
  AWidgetClass^.draw := @LCLGtkNotebookDraw;
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




