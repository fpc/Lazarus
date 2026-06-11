{%MainUnit gtk3widgets.pas}

procedure LCLGtkNotebookGetPreferredHeightForWidth(widget: PGtkWidget;
  width: gint; minimum_height: Pgint; natural_height: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_w: gint;
  nat_w: gint;
begin
  minimum_height^ := 1;
  natural_height^ := 1;
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
    Exit;
  ParentClass^.get_preferred_width(widget, @min_w, @nat_w);
  if (min_w > 0) and (width < min_w) then
  begin
    if Assigned(ParentClass^.get_preferred_height) then
      ParentClass^.get_preferred_height(widget, minimum_height, natural_height);
  end else
  if Assigned(ParentClass^.get_preferred_height_for_width) then
    ParentClass^.get_preferred_height_for_width(widget, width, minimum_height, natural_height);
  minimum_height^ := 1;
  if natural_height^ < minimum_height^ then
    natural_height^ := minimum_height^;
end;

procedure LCLGtkNotebookGetPreferredWidthForHeight(widget: PGtkWidget;
  height: gint; minimum_width: Pgint; natural_width: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_h: gint;
  nat_h: gint;
begin
  minimum_width^ := 1;
  natural_width^ := 1;
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
    Exit;
  ParentClass^.get_preferred_height(widget, @min_h, @nat_h);
  if (min_h > 0) and (height < min_h) then
  begin
    if Assigned(ParentClass^.get_preferred_width) then
      ParentClass^.get_preferred_width(widget, minimum_width, natural_width);
  end else
  if Assigned(ParentClass^.get_preferred_width_for_height) then
    ParentClass^.get_preferred_width_for_height(widget, height, minimum_width, natural_width);
  minimum_width^ := 1;
  if natural_width^ < minimum_width^ then
    natural_width^ := minimum_width^;
end;

procedure LCLGtkNotebookSizeAllocate(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  min_h, min_w: gint;
  nat_h, nat_w: gint;
  tooSmall: Boolean;
  Last1W, Last1H, Last2W, Last2H: PtrInt;
  PingCount: PtrInt;
  StoredGen: PtrUInt;
  TopLevel: PGtkWidget;
  WinGen: PtrUInt;
  AdjAlloc: TGtkAllocation;
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;

  Last1W := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev-aw'));
  Last1H := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev-ah'));
  Last2W := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev2-aw'));
  Last2H := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev2-ah'));
  PingCount := PtrInt(g_object_get_data(PGObject(widget), 'lcl-pingpong-cnt'));
  StoredGen := PtrUInt(g_object_get_data(PGObject(widget), 'lcl-layout-gen'));

  TopLevel := gtk_widget_get_toplevel(widget);
  if gtk_widget_is_toplevel(TopLevel) then
    WinGen := PtrUInt(g_object_get_data(PGObject(TopLevel), 'lcl-win-layout-gen'))
  else
    WinGen := 0;

  if WinGen <> StoredGen then
  begin
    PingCount := 0;
    Last1W := 0;
    Last1H := 0;
    Last2W := 0;
    Last2H := 0;
    g_object_set_data(PGObject(widget), 'lcl-pingpong-cnt', nil);
    g_object_set_data(PGObject(widget), 'lcl-prev-aw',  nil);
    g_object_set_data(PGObject(widget), 'lcl-prev-ah',  nil);
    g_object_set_data(PGObject(widget), 'lcl-prev2-aw', nil);
    g_object_set_data(PGObject(widget), 'lcl-prev2-ah', nil);
    g_object_set_data(PGObject(widget), 'lcl-layout-gen', Pointer(PtrUInt(WinGen)));
  end;

  if ((allocation^.width <> Last1W) or (allocation^.height <> Last1H)) and
     (allocation^.width = Last2W) and (allocation^.height = Last2H) and
     (Last2W <> 0) and (Last2H <> 0) then
  begin
    Inc(PingCount);
    g_object_set_data(PGObject(widget), 'lcl-pingpong-cnt', Pointer(PtrInt(PingCount)));
    if PingCount > 1 then
    begin
      {$IFDEF GTK3DEBUGSIZE}
      writeln(Format('LCLGtkNotebookSizeAllocate PINGPONG-SKIP alloc x %d y %d w %d h %d Last1 %dx%d Last2 %dx%d cnt %d',[allocation^.x, allocation^.y, allocation^.width, allocation^.height, Last1W, Last1H, Last2W, Last2H, PingCount]));
      {$ENDIF}
      gtk_widget_set_allocation(widget, allocation);
      gtk_widget_set_clip(widget, allocation);
      Exit;
    end;
    {$IFDEF GTK3DEBUGSIZE}
    writeln(Format('LCLGtkNotebookSizeAllocate PINGPONG-ALLOW alloc x %d y %d w %d h %d Last1 %dx%d Last2 %dx%d cnt %d',[allocation^.x, allocation^.y, allocation^.width, allocation^.height, Last1W, Last1H, Last2W, Last2H, PingCount]));
    {$ENDIF}
  end
  else
    if ((allocation^.width <> Last1W) or (allocation^.height <> Last1H)) and
       ((allocation^.width <> Last2W) or (allocation^.height <> Last2H)) then
      g_object_set_data(PGObject(widget), 'lcl-pingpong-cnt', nil);

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
    {$IFDEF GTK3DEBUGSIZE}
    writeln(Format('LCLGtkNotebookSizeAllocate tooSmall-CLAMPUP alloc x %d y %d w %d h %d min %dx%d',[allocation^.x, allocation^.y, allocation^.width, allocation^.height, min_w, min_h]));
    {$ENDIF}
    AdjAlloc := allocation^;
    if (min_w > 0) and (AdjAlloc.width < min_w) then
      AdjAlloc.width := min_w;
    if (min_h > 0) and (AdjAlloc.height < min_h) then
      AdjAlloc.height := min_h;
    ParentClass^.size_allocate(widget, @AdjAlloc);
    gtk_widget_set_allocation(widget, allocation);
    gtk_widget_set_clip(widget, allocation);
    Exit;
  end;
  {$IFDEF GTK3DEBUGSIZE}
  writeln(Format('LCLGtkNotebookSizeAllocate FULL alloc x %d y %d w %d h %d Last1 %dx%d Last2 %dx%d',[allocation^.x, allocation^.y, allocation^.width, allocation^.height, Last1W, Last1H, Last2W, Last2H]));
  {$ENDIF}
  ParentClass^.size_allocate(widget, allocation);
end;

procedure LCLGtkNotebookGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  base_min, base_nat: gint;
  {$IFDEF GTK3DEBUGSIZE}
  ADbgWidget: TGtk3Widget;
  {$ENDIF}
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  base_min := 0;
  base_nat := 0;
  if Assigned(ParentClass) and Assigned(ParentClass^.get_preferred_width) then
    ParentClass^.get_preferred_width(widget, @base_min, @base_nat);
  {$IFDEF GTK3DEBUGSIZE}
  ADbgWidget := TGtk3Widget(HwndFromGtkWidget(widget));
  if Assigned(ADbgWidget) and Assigned(ADbgWidget.LCLObject) then
    DebugLn(Format('LCLGtkNotebook.GetPreferredWidth %s base_min=%d base_nat=%d -> min=1 nat=%d',
      [dbgsName(ADbgWidget.LCLObject), base_min, base_nat, base_nat]));
  {$ENDIF}
  min_width^ := 1;
  nat_width^ := Max(base_nat, 1);
end;

procedure LCLGtkNotebookGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  ParentClass: PGtkWidgetClass;
  base_min, base_nat: gint;
  {$IFDEF GTK3DEBUGSIZE}
  ADbgWidget: TGtk3Widget;
  {$ENDIF}
begin
  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  base_min := 0;
  base_nat := 0;
  if Assigned(ParentClass) and Assigned(ParentClass^.get_preferred_height) then
    ParentClass^.get_preferred_height(widget, @base_min, @base_nat);
  {$IFDEF GTK3DEBUGSIZE}
  ADbgWidget := TGtk3Widget(HwndFromGtkWidget(widget));
  if Assigned(ADbgWidget) and Assigned(ADbgWidget.LCLObject) then
    DebugLn(Format('LCLGtkNotebook.GetPreferredHeight %s base_min=%d base_nat=%d -> min=1 nat=%d',
      [dbgsName(ADbgWidget.LCLObject), base_min, base_nat, base_nat]));
  {$ENDIF}
  min_height^ := 1;
  nat_height^ := Max(base_nat, 1);
end;

procedure LCLGtkNotebookClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.size_allocate := @LCLGtkNotebookSizeAllocate;
  AWidgetClass^.get_preferred_width := @LCLGtkNotebookGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkNotebookGetPreferredHeight;
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
