{%MainUnit gtk3widgets.pas}

var
  {$note rethink this, so we don't use global variables, besides that what if theme changes.}
  LCLScrolledWinExtentsH: gint = 0; // vscrollbar CSS top+bottom extents, cached
  LCLScrolledWinExtentsW: gint = 0; // hscrollbar CSS left+right extents, cached
  OrigGtkScrollbarSizeAllocate: procedure(widget: PGtkWidget; allocation: PGtkAllocation); cdecl = nil;
  GtkScrollbarHijackInstalled: Boolean = False;

procedure LCLGtkScrollbarSizeAllocateHijack(widget: PGtkWidget; allocation: PGtkAllocation); cdecl;
var
  min_w, nat_w, min_h, nat_h: gint;
  klass: PGtkWidgetClass;
  ClampedAlloc: TGtkAllocation;
  NeedClamp: Boolean;
begin
  if not Assigned(OrigGtkScrollbarSizeAllocate) then
    exit;
  klass := PGtkWidgetClass(g_type_class_peek(gtk_scrollbar_get_type()));
  min_w := 0;
  nat_w := 0;
  min_h := 0;
  nat_h := 0;
  if Assigned(klass) then
  begin
    if Assigned(klass^.get_preferred_width) then
      klass^.get_preferred_width(widget, @min_w, @nat_w);
    if Assigned(klass^.get_preferred_height) then
      klass^.get_preferred_height(widget, @min_h, @nat_h);
  end;

  NeedClamp := ((min_w > 0) and (allocation^.width < min_w)) or
    ((min_h > 0) and (allocation^.height < min_h));

  if NeedClamp then
  begin
    ClampedAlloc := allocation^;
    if (min_w > 0) and (ClampedAlloc.width < min_w) then
      ClampedAlloc.width := min_w;
    if (min_h > 0) and (ClampedAlloc.height < min_h) then
      ClampedAlloc.height := min_h;
    OrigGtkScrollbarSizeAllocate(widget, @ClampedAlloc);
    exit;
  end;
  OrigGtkScrollbarSizeAllocate(widget, allocation);
end;

procedure InstallGtkScrollbarVFuncHijack;
var
  klass: PGtkWidgetClass;
begin
  if GtkScrollbarHijackInstalled then
    exit;

  GtkScrollbarHijackInstalled := True;
  klass := PGtkWidgetClass(g_type_class_ref(gtk_scrollbar_get_type()));

  if not Assigned(klass) then
    exit;

  if Assigned(klass^.size_allocate) and
     (klass^.size_allocate <> @LCLGtkScrollbarSizeAllocateHijack) then
  begin
    OrigGtkScrollbarSizeAllocate := klass^.size_allocate;
    klass^.size_allocate := @LCLGtkScrollbarSizeAllocateHijack;
  end;
end;

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
        //writeln('LCLGtkScrolledWindowComputeExtents margin=',margin.top,' ',margin.bottom,' border=',border.top,' ',border.bottom,' padding=',padding.top,' ',padding.bottom);
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
  vpolicy, hpolicy: TGtkPolicyType;
  Last1W, Last1H, Last2W, Last2H: PtrInt;
  FreezeW, FreezeH: PtrInt;
  threshH, threshW, minSize: gint;
  negH, negW: gint;
  vScrollMin: gint;
  vActive, hActive: Boolean;
  BinChild: PGtkWidget;
  ChildAlloc: TGtkAllocation;
  LayoutW, LayoutH: guint;
  ClampedAlloc: TGtkAllocation;
  ChildPrefH: gint;
  wOscBelow, hOscBelow: Boolean;
  AControl: TGtk3Widget;
  WaylandAutoSizeForm: Boolean;
  {$IFDEF GTK3DEBUGSIZE}
  ADbgCtl: TGtk3Widget;
  ADbgName: string;
  {$ENDIF}
begin
  if not GtkScrollbarHijackInstalled then
    InstallGtkScrollbarVFuncHijack;

  ParentClass := PGtkWidgetClass(
    g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;

  vpolicy := GTK_POLICY_NEVER;
  hpolicy := GTK_POLICY_NEVER;
  gtk_scrolled_window_get_policy(PGtkScrolledWindow(widget), @hpolicy, @vpolicy);

  if (LCLScrolledWinExtentsH = 0) or (LCLScrolledWinExtentsW = 0) then
    LCLGtkScrolledWindowComputeExtents(widget);

  vscroll := PGtkScrolledWindow(widget)^.get_vscrollbar;
  hscroll := PGtkScrolledWindow(widget)^.get_hscrollbar;

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  WaylandAutoSizeForm := Gtk3WidgetSet.IsWayland and Assigned(AControl) and
    Assigned(AControl.LCLObject) and (AControl.LCLObject is TCustomForm) and
    TCustomForm(AControl.LCLObject).AutoSize;

  if Assigned(vscroll) and Gtk3IsWidget(vscroll) then
  begin
    if WaylandAutoSizeForm then
    begin
      if (vpolicy = GTK_POLICY_EXTERNAL) and vscroll^.get_visible then
        vscroll^.hide
      else if (vpolicy <> GTK_POLICY_EXTERNAL) and not vscroll^.get_visible then
        vscroll^.show;
    end
    else
    begin
      if ((vpolicy = GTK_POLICY_EXTERNAL) or (vpolicy = GTK_POLICY_NEVER)) and vscroll^.get_visible then
        vscroll^.hide
      else if (vpolicy <> GTK_POLICY_EXTERNAL) and (vpolicy <> GTK_POLICY_NEVER) and not vscroll^.get_visible then
        vscroll^.show;
    end;
  end;
  if Assigned(hscroll) and Gtk3IsWidget(hscroll) then
  begin
    if WaylandAutoSizeForm then
    begin
      if (hpolicy = GTK_POLICY_EXTERNAL) and hscroll^.get_visible then
        hscroll^.hide
      else if (hpolicy <> GTK_POLICY_EXTERNAL) and not hscroll^.get_visible then
        hscroll^.show;
    end
    else
    begin
      if ((hpolicy = GTK_POLICY_EXTERNAL) or (hpolicy = GTK_POLICY_NEVER)) and hscroll^.get_visible then
        hscroll^.hide
      else if (hpolicy <> GTK_POLICY_EXTERNAL) and (hpolicy <> GTK_POLICY_NEVER) and not hscroll^.get_visible then
        hscroll^.show;
    end;
  end;

  vActive := (vpolicy <> GTK_POLICY_NEVER) and (vpolicy <> GTK_POLICY_EXTERNAL);
  hActive := (hpolicy <> GTK_POLICY_NEVER) and (hpolicy <> GTK_POLICY_EXTERNAL);
  threshH := LCLScrolledWinExtentsH;
  threshW := LCLScrolledWinExtentsW;
  vScrollMin := 0;
  if vActive and Assigned(vscroll) and Gtk3IsWidget(vscroll) then
  begin
    minSize := 0;
    gtk_widget_get_preferred_height(vscroll, @minSize, nil);
    if minSize > 0 then
    begin
      vScrollMin := minSize;
      inc(threshH, minSize);
    end;
    minSize := 0;
    gtk_widget_get_preferred_width(vscroll, @minSize, nil);
    if minSize > 0 then
      inc(threshW, minSize);
  end;
  if hActive and Assigned(hscroll) and Gtk3IsWidget(hscroll) then
  begin
    minSize := 0;
    gtk_widget_get_preferred_height(hscroll, @minSize, nil);
    if minSize > 0 then
      inc(threshH, minSize);
    minSize := 0;
    gtk_widget_get_preferred_width(hscroll, @minSize, nil);
    if minSize > 0 then
      inc(threshW, minSize);
  end;

  negH := 0;
  negW := 0;
  ParentClass^.get_preferred_height(widget, @negH, @minSize);
  ParentClass^.get_preferred_width(widget, @negW, @minSize);
  {$IFDEF GTK3DEBUGSIZE}
  ADbgCtl := TGtk3Widget(HwndFromGtkWidget(widget));
  if Assigned(ADbgCtl) and Assigned(ADbgCtl.LCLObject) then
  begin
    if Assigned(ADbgCtl.LCLObject.Parent) then
      ADbgName := dbgsName(ADbgCtl.LCLObject) + ' parent=' + dbgsName(ADbgCtl.LCLObject.Parent)
    else
    if Assigned(ADbgCtl.LCLObject.Owner) then
      ADbgName := dbgsName(ADbgCtl.LCLObject) + ' owner=' + dbgsName(ADbgCtl.LCLObject.Owner)
    else
      ADbgName := dbgsName(ADbgCtl.LCLObject) + ' (orphan)';
  end else
    ADbgName := 'nil';
  {$ENDIF}

  if (allocation^.width <= 1) or (allocation^.height <= 1) then
  begin
    {$IFDEF GTK3DEBUGSIZE}
    DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d SKIP collapse-zero (thresh=%dx%d neg=%dx%d)',
      [ADbgName, allocation^.width, allocation^.height, threshW, threshH, negW, negH]));
    {$ENDIF}
    gtk_widget_set_allocation(widget, allocation);
    gtk_widget_set_clip(widget, allocation);
    exit;
  end;

  if (allocation^.height < negH) or (allocation^.width < negW) then
  begin
    BinChild := gtk_bin_get_child(PGtkBin(widget));
    ChildPrefH := 0;
    if vActive and (allocation^.height >= 2) and
       Assigned(BinChild) and Gtk3IsWidget(BinChild) then
      gtk_widget_get_preferred_height(BinChild, nil, @ChildPrefH);
    LayoutW := 0;
    LayoutH := 0;
    if vActive and Assigned(BinChild) and Gtk3IsWidget(BinChild) and
       g_type_check_instance_is_a(PGTypeInstance(BinChild), gtk_layout_get_type) then
      gtk_layout_get_size(PGtkLayout(BinChild), @LayoutW, @LayoutH);
    {$IFDEF GTK3DEBUGSIZE}
    DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d BELOW-NEG negH=%d vScrollMin=%d cprefH=%d LayoutH=%d allocH-vs-(negH-vMin)=%d',
      [ADbgName, allocation^.width, allocation^.height, negH, vScrollMin,
       ChildPrefH, LayoutH, allocation^.height - (negH - vScrollMin)]));
    {$ENDIF}
    if vActive and (LayoutH > 0) and (gint(LayoutH) <= negH) then
    begin
      {$IFDEF GTK3DEBUGSIZE}
      DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d BELOW-NEG content-fits LayoutH=%d -> NEVER policy parent call',
        [ADbgName, allocation^.width, allocation^.height, LayoutH]));
      {$ENDIF}
      gtk_scrolled_window_set_policy(PGtkScrolledWindow(widget), hpolicy, GTK_POLICY_NEVER);
      ParentClass^.size_allocate(widget, allocation);
      g_object_set_data(PGObject(widget), 'lcl-gsw-synced', Pointer(PtrInt(1)));
      ChildAlloc.x := 0;
      ChildAlloc.y := 0;
      ChildAlloc.width := allocation^.width;
      ChildAlloc.height := allocation^.height;
      if Assigned(BinChild) and Gtk3IsWidget(BinChild) then
        gtk_widget_size_allocate(BinChild, @ChildAlloc);
      gtk_widget_set_allocation(widget, allocation);
      gtk_widget_set_clip(widget, allocation);
      exit;
    end;
    {$IFDEF GTK3DEBUGSIZE}
    DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d BELOW-NEG thresh=%dx%d neg=%dx%d -> clamped parent call',
      [ADbgName, allocation^.width, allocation^.height, threshW, threshH, negW, negH]));
    {$ENDIF}
    ClampedAlloc.x := allocation^.x;
    ClampedAlloc.y := allocation^.y;
    ClampedAlloc.width := Max(allocation^.width, negW);
    ClampedAlloc.height := Max(allocation^.height, negH);
    g_object_set_data(PGObject(widget), 'lcl-gsw-clamping', widget);
    ParentClass^.size_allocate(widget, @ClampedAlloc);
    g_object_set_data(PGObject(widget), 'lcl-gsw-clamping', nil);
    g_object_set_data(PGObject(widget), 'lcl-gsw-synced', Pointer(PtrInt(1)));
    if Assigned(BinChild) and Gtk3IsWidget(BinChild) then
    begin
      ChildAlloc.x := 0;
      ChildAlloc.y := 0;
      ChildAlloc.width := allocation^.width;
      ChildAlloc.height := allocation^.height;
      {$IFDEF GTK3DEBUGSIZE}
      DebugLn(Format('LCLGSW.SizeAllocate %s -> child override alloc=%dx%d',
        [ADbgName, ChildAlloc.width, ChildAlloc.height]));
      {$ENDIF}
      gtk_widget_size_allocate(BinChild, @ChildAlloc);
    end;
    gtk_widget_set_allocation(widget, allocation);
    gtk_widget_set_clip(widget, allocation);
    exit;
  end;

  Last1W := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev-aw'));
  Last1H := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev-ah'));
  Last2W := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev2-aw'));
  Last2H := PtrInt(g_object_get_data(PGObject(widget), 'lcl-prev2-ah'));
  wOscBelow := (allocation^.width <> Last1W) and (allocation^.width < threshW);
  hOscBelow := (allocation^.height <> Last1H) and (allocation^.height < threshH);
  if ((allocation^.width <> Last1W) or (allocation^.height <> Last1H)) and
     (allocation^.width = Last2W) and (allocation^.height = Last2H) and
     (Last2W <> 0) and (Last2H <> 0) and
     (wOscBelow or hOscBelow) then
  begin
    {$IFDEF GTK3DEBUGSIZE}
    DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d OSCILLATE-BELOW (prev %dx%d / prev2 %dx%d thresh=%dx%d) - SKIP parent',
      [ADbgName, allocation^.width, allocation^.height, Last1W, Last1H, Last2W, Last2H, threshW, threshH]));
    {$ENDIF}
    if Gtk3WidgetSet.IsWayland then
    begin
      g_object_set_data(PGObject(widget), 'lcl-freeze-aw', Pointer(Last1W));
      g_object_set_data(PGObject(widget), 'lcl-freeze-ah', Pointer(Last1H));
    end;
    gtk_widget_set_allocation(widget, allocation);
    gtk_widget_set_clip(widget, allocation);
    exit;
  end;
  if (allocation^.width <> Last1W) or (allocation^.height <> Last1H) then
  begin
    g_object_set_data(PGObject(widget), 'lcl-prev2-aw', Pointer(PtrInt(Last1W)));
    g_object_set_data(PGObject(widget), 'lcl-prev2-ah', Pointer(PtrInt(Last1H)));
    g_object_set_data(PGObject(widget), 'lcl-prev-aw', Pointer(PtrInt(allocation^.width)));
    g_object_set_data(PGObject(widget), 'lcl-prev-ah', Pointer(PtrInt(allocation^.height)));
  end;

  if Gtk3WidgetSet.IsWayland then
  begin
    FreezeW := PtrInt(g_object_get_data(PGObject(widget), 'lcl-freeze-aw'));
    FreezeH := PtrInt(g_object_get_data(PGObject(widget), 'lcl-freeze-ah'));
    if (FreezeW <> 0) or (FreezeH <> 0) then
    begin
      if (allocation^.width >= threshW) and (allocation^.height >= threshH) then
      begin
        g_object_set_data(PGObject(widget), 'lcl-freeze-aw', nil);
        g_object_set_data(PGObject(widget), 'lcl-freeze-ah', nil);
      end else if (allocation^.width = FreezeW) and (allocation^.height = FreezeH) then
      begin
        {$IFDEF GTK3DEBUGSIZE}
        DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d WAYLAND-FREEZE-SKIP (freeze=%dx%d)',
          [ADbgName, allocation^.width, allocation^.height, FreezeW, FreezeH]));
        {$ENDIF}
        gtk_widget_set_allocation(widget, allocation);
        gtk_widget_set_clip(widget, allocation);
        exit;
      end else
      begin
        g_object_set_data(PGObject(widget), 'lcl-freeze-aw', nil);
        g_object_set_data(PGObject(widget), 'lcl-freeze-ah', nil);
      end;
    end;
  end;

  {$IFDEF GTK3DEBUGSIZE}
  if (allocation^.width < threshW) or (allocation^.height < threshH) then
    DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d BELOW thresh=%dx%d neg=%dx%d (passing through)',
      [ADbgName, allocation^.width, allocation^.height, threshW, threshH, negW, negH]))
  else
    DebugLn(Format('LCLGSW.SizeAllocate %s alloc=%dx%d OK thresh=%dx%d neg=%dx%d',
      [ADbgName, allocation^.width, allocation^.height, threshW, threshH, negW, negH]));
  {$ENDIF}

  ParentClass^.size_allocate(widget, allocation);
  g_object_set_data(PGObject(widget), 'lcl-gsw-synced', Pointer(PtrInt(1)));
end;

function LCLGtkScrolledWindowGetRequestMode(widget: PGtkWidget): TGtkSizeRequestMode; cdecl;
begin
  { GtkScrolledWindow normally propagates its child's request mode.
    GtkTreeView returns GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH,
    which causes gtk_widget_get_preferred_size to call get_preferred_height_for_width
    instead of get_preferred_height - bypassing our override entirely.
    Force CONSTANT_SIZE so GTK always calls get_preferred_height/width separately,
    where we can enforce LCL-managed sizes, including collapse to 0. }
  Result := GTK_SIZE_REQUEST_CONSTANT_SIZE;
  {$IFDEF GTK3DEBUGHEIGHTZERO}
  writeln(Format('LCLGSW.GetRequestMode: widget=%p → CONSTANT_SIZE', [widget]));
  {$ENDIF}
end;

procedure LCLGtkScrolledWindowGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AParent, AGrandParent: PGtkWidget;
  ALCLWidget: TGtk3Widget;
begin
  min_width^ := 1;
  nat_width^ := 10;
  AParent := widget^.get_parent;
  if AParent <> nil then
  begin
    AGrandParent := AParent^.get_parent;
    if AGrandParent <> nil then
    begin
      ALCLWidget := TGtk3Widget(g_object_get_data(PGObject(AGrandParent), 'lclwidget'));
      if Assigned(ALCLWidget) and Assigned(ALCLWidget.LCLObject) and
         (csDesigning in ALCLWidget.LCLObject.ComponentState) then
        nat_width^ := ALCLWidget.LCLObject.Width;
    end;
  end;
  {$IFDEF GTK3DEBUGSIZE}
  ALCLWidget := TGtk3Widget(HwndFromGtkWidget(widget));
  if Assigned(ALCLWidget) and Assigned(ALCLWidget.LCLObject) then
    DebugLn(Format('LCLGSW.GetPreferredWidth %s min=%d nat=%d',
      [dbgsName(ALCLWidget.LCLObject), min_width^, nat_width^]));
  {$ENDIF}
end;

procedure LCLGtkScrolledWindowGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
  LCLIntendedH: gint;
begin
  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  LCLIntendedH := -1; // -1 = unknown (no AControl)
  if Assigned(AControl) and Assigned(AControl.LCLObject) then
  begin
    { LCLObject.Height is the authoritative LCL value - already updated when
      LCL sets Height:=0. LCLHeight is a GTK-side cache that may still hold
      the previous allocation (e.g. 58px from .lfm) at this point. }
    LCLIntendedH := AControl.LCLObject.Height;
  end;

  {$IFDEF GTK3DEBUGHEIGHTZERO}
  writeln(Format('LCLGSW.GetPrefH: widget=%p lcl=%s LCLIntendedH=%d LCLHeight=%d child_vis=%s mapped=%s',
    [widget, dbgsName(AControl.LCLObject), LCLIntendedH,
     IfThen(Assigned(AControl), AControl.LCLHeight, -999),
     BoolToStr(widget^.get_child_visible, True),
     BoolToStr(widget^.get_mapped, True)]));
  {$ENDIF}

  { When LCL collapses a control (Height=0 or Height=1 - the latter is the
    1px minimum that AlignControls leaves as arithmetic leftover for alBottom
    controls whose Height was explicitly set to 0), return 0 so that GtkLayout
    does not inflate the widget to its scrolled-content preferred height.
    Without this, a TListBox with Height:=0 would still appear at ~42px. issue #42103}
  if LCLIntendedH <= 1 then
  begin
    min_height^ := 0;
    nat_height^ := 0;
    {$IFDEF GTK3DEBUGHEIGHTZERO}
    writeln('LCLGSW.GetPrefH: LCLIntendedH=0 → returning (0,0)');
    {$ENDIF}
    exit;
  end;

  { For the positive case, prefer LCLHeight (actual GTK allocation) if set,
    fall back to LCLObject.Height. }
  if Assigned(AControl) and (AControl.LCLHeight > 0) then
    LCLIntendedH := AControl.LCLHeight
  else if LCLIntendedH < 0 then
    LCLIntendedH := 0;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  min_height^ := 1;
  nat_height^ := Max(nat_height^, LCLIntendedH);
  nat_height^ := Max(nat_height^, min_height^);
  {$IFDEF GTK3DEBUGHEIGHTZERO}
  writeln(Format('LCLGSW.GetPrefH: positive -> min=%d nat=%d (LCLIntendedH=%d)',
    [min_height^, nat_height^, LCLIntendedH]));
  {$ENDIF}
  {$IFDEF GTK3DEBUGSIZE}
  if Assigned(AControl) and Assigned(AControl.LCLObject) then
    DebugLn(Format('LCLGSW.GetPrefH %s min=%d nat=%d LCLIntH=%d LCLHeight=%d LCLObj.H=%d',
      [dbgsName(AControl.LCLObject), min_height^, nat_height^, LCLIntendedH,
       AControl.LCLHeight, AControl.LCLObject.Height]));
  {$ENDIF}
end;

function LCLGtkScrolledWindowDraw(widget: PGtkWidget; cr: Pcairo_t): gboolean; cdecl;
var
  ParentClass: PGtkWidgetClass;
  aw, ah: gint;
begin
  Result := False;
  aw := gtk_widget_get_allocated_width(widget);
  ah := gtk_widget_get_allocated_height(widget);

  if (aw <= 0) or (ah <= 0) then
    exit;

  if g_object_get_data(PGObject(widget), 'lcl-gsw-synced') = nil then
    exit;

  ParentClass := PGtkWidgetClass(
    g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if Assigned(ParentClass) and Assigned(ParentClass^.draw) then
    Result := ParentClass^.draw(widget, cr);
end;

procedure LCLGtkScrolledWindowClassInit(klass: PGTypeClass;
  {%H-}data: Pointer); cdecl;
var
  W: PGtkWidgetClass;
begin
  W := PGtkWidgetClass(klass);
  W^.size_allocate := @LCLGtkScrolledWindowSizeAllocate;
  W^.get_request_mode := @LCLGtkScrolledWindowGetRequestMode;
  W^.get_preferred_width := @LCLGtkScrolledWindowGetPreferredWidth;
  W^.get_preferred_height := @LCLGtkScrolledWindowGetPreferredHeight;
  W^.draw := @LCLGtkScrolledWindowDraw;
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
var
  scroll: PGtkWidget;
begin
  Result := PGtkScrolledWindow(g_object_newv(LCLGtkScrolledWindowGetType, 0, nil));
  scroll := PGtkWidget(Result^.get_hscrollbar);
  if Assigned(scroll) and Gtk3IsWidget(scroll) then
  begin
    scroll^.set_no_show_all(True);
    scroll^.hide;
  end;
  scroll := PGtkWidget(Result^.get_vscrollbar);
  if Assigned(scroll) and Gtk3IsWidget(scroll) then
  begin
    scroll^.set_no_show_all(True);
    scroll^.hide;
  end;
end;

procedure LCLGtkLayoutMapChild(child: PGtkWidget; {%H-}data: gpointer); cdecl;
begin
  if child^.get_visible and child^.get_child_visible then
    if not child^.get_mapped then
      gtk_widget_map(child);
end;

procedure LCLGtkLayoutMap(widget: PGtkWidget); cdecl;
var
  BinWin: PGdkWindow;
  WinWin: PGdkWindow;
begin
  gtk_widget_set_mapped(widget, True);
  gtk_container_foreach(PGtkContainer(widget), @LCLGtkLayoutMapChild, nil);
  BinWin := gtk_layout_get_bin_window(PGtkLayout(widget));
  if Assigned(BinWin) then
    BinWin^.show;
  WinWin := gtk_widget_get_window(widget);
  if Assigned(WinWin) then
    WinWin^.show;
end;

procedure LCLGtkLayoutClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  W: PGtkWidgetClass;
begin
  W := PGtkWidgetClass(klass);
  W^.map := @LCLGtkLayoutMap;
end;

procedure LCLGtkLayoutInstanceInit({%H-}instance: PGTypeInstance;
  {%H-}klass: PGTypeClass); cdecl;
begin
end;

var
  LCLGtkLayoutType: TGType = 0;

function LCLGtkLayoutGetType: TGType;
var
  Query: TGTypeQuery;
  Info: TGTypeInfo;
  ParentType: TGType;
begin
  if LCLGtkLayoutType = 0 then
  begin
    ParentType := gtk_layout_get_type;
    g_type_query(ParentType, @Query);
    FillChar(Info{%H-}, SizeOf(Info), 0);
    Info.class_size := guint16(Query.class_size);
    Info.class_init := @LCLGtkLayoutClassInit;
    Info.instance_size := guint16(Query.instance_size);
    Info.instance_init := @LCLGtkLayoutInstanceInit;
    LCLGtkLayoutType := g_type_register_static(ParentType,
      'LCLGtkLayout', @Info, G_TYPE_FLAG_NONE);
  end;
  Result := LCLGtkLayoutType;
end;

function LCLGtkLayoutNew: PGtkLayout;
begin
  Result := PGtkLayout(g_object_newv(LCLGtkLayoutGetType, 0, nil));
end;
