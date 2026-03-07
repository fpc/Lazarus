{%MainUnit gtk3widgets.pas}

var
  {$note rethink this, so we don't use global variables, besides that what if theme changes.}
  LCLScrolledWinExtentsH: gint = 0; // vscrollbar CSS top+bottom extents, cached
  LCLScrolledWinExtentsW: gint = 0; // hscrollbar CSS left+right extents, cached

procedure LCLGtkScrolledWindowComputeExtents(widget: PGtkWidget);
var
  scrollbar: PGtkWidget;
  ctx: PGtkStyleContext;
  border, padding, margin: TGtkBorder;
  extents: gint;
begin
  if LCLScrolledWinExtentsH = 0 then
  begin
    scrollbar := PGtkScrolledWindow(widget)^.get_vscrollbar;
    if Assigned(scrollbar) and Gtk3IsWidget(scrollbar) then
    begin
      ctx := scrollbar^.get_style_context;
      if Assigned(ctx) then
      begin
        FillChar(border{%H-}, SizeOf(border), 0);
        FillChar(padding{%H-}, SizeOf(padding), 0);
        FillChar(margin{%H-}, SizeOf(margin), 0);
        ctx^.get_border(GTK_STATE_FLAG_NORMAL, @border);
        ctx^.get_padding(GTK_STATE_FLAG_NORMAL, @padding);
        ctx^.get_margin(GTK_STATE_FLAG_NORMAL, @margin);
        extents := gint(border.top)  + gint(border.bottom)
                 + gint(padding.top) + gint(padding.bottom)
                 + gint(margin.top)  + gint(margin.bottom);
        if extents > 0 then
          LCLScrolledWinExtentsH := extents;
      end;
    end;
    if LCLScrolledWinExtentsH = 0 then
      LCLScrolledWinExtentsH := 10; // fallback (eg Adwaita: 5+5)
  end;

  if LCLScrolledWinExtentsW = 0 then
  begin
    scrollbar := PGtkScrolledWindow(widget)^.get_hscrollbar;
    if Assigned(scrollbar) and Gtk3IsWidget(scrollbar) then
    begin
      ctx := scrollbar^.get_style_context;
      if Assigned(ctx) then
      begin
        FillChar(border{%H-}, SizeOf(border), 0);
        FillChar(padding{%H-}, SizeOf(padding), 0);
        FillChar(margin{%H-}, SizeOf(margin), 0);
        ctx^.get_border(GTK_STATE_FLAG_NORMAL, @border);
        ctx^.get_padding(GTK_STATE_FLAG_NORMAL, @padding);
        ctx^.get_margin(GTK_STATE_FLAG_NORMAL, @margin);
        extents := gint(border.left)  + gint(border.right)
                 + gint(padding.left) + gint(padding.right)
                 + gint(margin.left)  + gint(margin.right);
        if extents > 0 then
          LCLScrolledWinExtentsW := extents;
      end;
    end;
    if LCLScrolledWinExtentsW = 0 then
      LCLScrolledWinExtentsW := 10; // fallback
  end;
end;

procedure LCLGtkScrolledWindowSizeAllocate(widget: PGtkWidget;
  allocation: PGtkAllocation); cdecl;
var
  ParentClass: PGtkWidgetClass;
  vscroll, hscroll: PGtkWidget;
  threshH, threshW, minSize: gint;
  vpolicy, hpolicy: TGtkPolicyType;
  vActive, hActive: Boolean;
begin
  ParentClass := PGtkWidgetClass(
    g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;

  vpolicy := GTK_POLICY_NEVER;
  hpolicy := GTK_POLICY_NEVER;
  gtk_scrolled_window_get_policy(PGtkScrolledWindow(widget), @hpolicy, @vpolicy);

  vActive := (vpolicy <> GTK_POLICY_NEVER) and (vpolicy <> GTK_POLICY_EXTERNAL);
  hActive := (hpolicy <> GTK_POLICY_NEVER) and (hpolicy <> GTK_POLICY_EXTERNAL);

  // EXTERNAL/NEVER on both axes: GTK never shows scrollbars, no cascade risk.
  if not vActive and not hActive then
  begin
    ParentClass^.size_allocate(widget, allocation);
    exit;
  end;

  if (LCLScrolledWinExtentsH = 0) or (LCLScrolledWinExtentsW = 0) then
    LCLGtkScrolledWindowComputeExtents(widget);

  vscroll := PGtkScrolledWindow(widget)^.get_vscrollbar;
  hscroll := PGtkScrolledWindow(widget)^.get_hscrollbar;

  // Mapped vscroll: inner gadgets (trough/slider) cascade - use preferred_height.
  // Unmapped vscroll: get_preferred_height still reads from CSS even when not
  // mapped, GTK CSS layout is map-independent.
  threshH := LCLScrolledWinExtentsH;
  if vActive and Assigned(vscroll) and Gtk3IsWidget(vscroll) then
  begin
    minSize := 0;
    gtk_widget_get_preferred_height(vscroll, @minSize, nil);
    if (minSize > 0) and
       (vscroll^.get_mapped or
        (allocation^.height < minSize + LCLScrolledWinExtentsH)) then
      threshH := minSize + LCLScrolledWinExtentsH;
  end;

  threshW := LCLScrolledWinExtentsW;

  // Dual-scrollbar: GTK subtracts one bar's thickness from the other's allocation.
  if vActive and hActive then
  begin
    if Assigned(hscroll) and Gtk3IsWidget(hscroll) then
    begin
      minSize := 0;
      gtk_widget_get_preferred_height(hscroll, @minSize, nil);
      if minSize > 0 then
        inc(threshH, minSize);
    end;
    if Assigned(vscroll) and Gtk3IsWidget(vscroll) then
    begin
      minSize := 0;
      gtk_widget_get_preferred_width(vscroll, @minSize, nil);
      if minSize > 0 then
        inc(threshW, minSize);
    end;
  end;

  if (allocation^.width < threshW) or (allocation^.height < threshH) then
    exit;

  ParentClass^.size_allocate(widget, allocation);
end;

procedure LCLGtkScrolledWindowClassInit(klass: PGTypeClass;
  {%H-}data: Pointer); cdecl;
var
  W: PGtkWidgetClass;
begin
  W := PGtkWidgetClass(klass);
  W^.size_allocate := @LCLGtkScrolledWindowSizeAllocate;
end;

procedure LCLGtkScrolledWindowInstanceInit({%H-}instance: PGTypeInstance;
  {%H-}klass: PGTypeClass); cdecl;
begin
end;

var
  LCLGtkScrolledWindowType: TGType = 0;

function LCLGtkScrolledWindowGetType: TGType;
var
  Query: TGTypeQuery;
  Info: TGTypeInfo;
  ParentType: TGType;
begin
  if LCLGtkScrolledWindowType = 0 then
  begin
    ParentType := gtk_scrolled_window_get_type;
    g_type_query(ParentType, @Query);
    FillChar(Info{%H-}, SizeOf(Info), 0);
    Info.class_size := guint16(Query.class_size);
    Info.class_init := @LCLGtkScrolledWindowClassInit;
    Info.instance_size := guint16(Query.instance_size);
    Info.instance_init := @LCLGtkScrolledWindowInstanceInit;
    LCLGtkScrolledWindowType := g_type_register_static(ParentType,
      'LCLGtkScrolledWindow', @Info, G_TYPE_FLAG_NONE);
  end;
  Result := LCLGtkScrolledWindowType;
end;

function LCLGtkScrolledWindowNew: PGtkScrolledWindow;
begin
  Result := PGtkScrolledWindow(g_object_newv(LCLGtkScrolledWindowGetType, 0, nil));
end;
