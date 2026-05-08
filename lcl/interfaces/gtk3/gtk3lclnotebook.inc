{%MainUnit gtk3widgets.pas}

procedure LCLGtkNotebookGetPreferredHeightForWidth(widget: PGtkWidget;
  width: gint; minimum_height: Pgint; natural_height: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_w: gint;
  nat_w: gint;
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
    Exit;
  ParentClass^.get_preferred_width(widget, @min_w, @nat_w);
  if (min_w > 0) and (width < min_w) then
  begin
    if Assigned(ParentClass^.get_preferred_height) then
      ParentClass^.get_preferred_height(widget, minimum_height, natural_height);
    Exit;
  end;
  if Assigned(ParentClass^.get_preferred_height_for_width) then
    ParentClass^.get_preferred_height_for_width(widget, width, minimum_height, natural_height);
end;

procedure LCLGtkNotebookGetPreferredWidthForHeight(widget: PGtkWidget;
  height: gint; minimum_width: Pgint; natural_width: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_h: gint;
  nat_h: gint;
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
    Exit;
  ParentClass^.get_preferred_height(widget, @min_h, @nat_h);
  if (min_h > 0) and (height < min_h) then
  begin
    if Assigned(ParentClass^.get_preferred_width) then
      ParentClass^.get_preferred_width(widget, minimum_width, natural_width);
    Exit;
  end;
  if Assigned(ParentClass^.get_preferred_width_for_height) then
    ParentClass^.get_preferred_width_for_height(widget, height, minimum_width, natural_width);
end;

procedure LCLGtkNotebookSizeAllocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_h, min_w: gint;
  nat_h, nat_w: gint;
  tooSmall: Boolean;
  Last1W, Last1H, Last2W, Last2H: PtrInt;
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;

  Last1W := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev-aw'));
  Last1H := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev-ah'));
  Last2W := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev2-aw'));
  Last2H := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev2-ah'));

  if ((allocation^.width <> Last1W) or (allocation^.height <> Last1H)) and
     (allocation^.width = Last2W) and (allocation^.height = Last2H) and
     (Last2W <> 0) and (Last2H <> 0) then
  begin
    gtk_widget_set_allocation(widget, allocation);
    gtk_widget_set_clip(widget, allocation);
    Exit;
  end;

  if (allocation^.width <> Last1W) or (allocation^.height <> Last1H) then
  begin
    g_object_set_data(PGObject(widget), 'lcl-prev2-aw', Pointer(PtrInt(Last1W)));
    g_object_set_data(PGObject(widget), 'lcl-prev2-ah', Pointer(PtrInt(Last1H)));
    g_object_set_data(PGObject(widget), 'lcl-prev-aw', Pointer(PtrInt(allocation^.width)));
    g_object_set_data(PGObject(widget), 'lcl-prev-ah', Pointer(PtrInt(allocation^.height)));
  end;

  min_h := 0; nat_h := 0; min_w := 0; nat_w := 0;
  ParentClass^.get_preferred_height(widget, @min_h, @nat_h);
  ParentClass^.get_preferred_width(widget, @min_w, @nat_w);

  tooSmall := ((min_h > 0) and (allocation^.height < min_h)) or
    ((min_w > 0) and (allocation^.width < min_w));

  if tooSmall then
  begin
    gtk_widget_set_allocation(widget, allocation);
    gtk_widget_set_clip(widget, allocation);
    Exit;
  end;
  ParentClass^.size_allocate(widget, allocation);
end;

procedure LCLGtkNotebookClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.size_allocate := @LCLGtkNotebookSizeAllocate;
  AWidgetClass^.get_preferred_height_for_width := @LCLGtkNotebookGetPreferredHeightForWidth;
  AWidgetClass^.get_preferred_width_for_height := @LCLGtkNotebookGetPreferredWidthForHeight;
end;

procedure LCLGtkNotebookInstanceInit({%H-}instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
begin
end;

var
  LCLGtkNotebookType: TGType = 0;

function LCLGtkNotebookGetType: TGType; cdecl;
var
  Query: TGTypeQuery;
  Info: TGTypeInfo;
  ParentType: TGType;
begin
  if LCLGtkNotebookType = 0 then
  begin
    ParentType := gtk_notebook_get_type;
    g_type_query(ParentType, @Query);
    FillChar(Info{%H-}, SizeOf(Info), 0);
    Info.class_size    := guint16(Query.class_size);
    Info.class_init    := @LCLGtkNotebookClassInit;
    Info.instance_size := guint16(Query.instance_size);
    Info.instance_init := @LCLGtkNotebookInstanceInit;
    LCLGtkNotebookType := g_type_register_static(ParentType,
      'LCLGtkNotebook', @Info, G_TYPE_FLAG_NONE);
  end;
  Result := LCLGtkNotebookType;
end;

function LCLGtkNotebookNew: PGtkNotebook;
begin
  Result := PGtkNotebook(g_object_new(LCLGtkNotebookGetType(), 'show-tabs', [gboolean(True), nil]));
end;
