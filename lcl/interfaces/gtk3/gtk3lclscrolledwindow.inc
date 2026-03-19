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
  {$IFDEF GTK3DEBUGHEIGHTZERO}
  ADbgCtl: TGtk3Widget;
  {$ENDIF}
begin
  {$IFDEF GTK3DEBUGHEIGHTZERO}
  ADbgCtl := TGtk3Widget(HwndFromGtkWidget(widget));
  writeln(Format('LCLGSW.SizeAllocate: widget=%p type=%s alloc={x=%d y=%d w=%d h=%d} realized=%s mapped=%s child_vis=%s has_win=%s lcl=%s',
    [widget, g_type_name(widget^.g_type_instance.g_class^.g_type),
     allocation^.x, allocation^.y, allocation^.width, allocation^.height,
     BoolToStr(widget^.get_realized, True),
     BoolToStr(widget^.get_mapped, True),
     BoolToStr(widget^.get_child_visible, True),
     BoolToStr(widget^.get_has_window, True),
     IfThen(Assigned(ADbgCtl) and Assigned(ADbgCtl.LCLObject),
       ADbgCtl.LCLObject.Name + ':' + ADbgCtl.LCLObject.ClassName, 'nil')]));
  {$ENDIF}
  ParentClass := PGtkWidgetClass(
    g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) or not Assigned(ParentClass^.size_allocate) then
    Exit;

  vpolicy := GTK_POLICY_NEVER;
  hpolicy := GTK_POLICY_NEVER;
  gtk_scrolled_window_get_policy(PGtkScrolledWindow(widget), @hpolicy, @vpolicy);

  vActive := (vpolicy <> GTK_POLICY_NEVER) and (vpolicy <> GTK_POLICY_EXTERNAL);
  hActive := (hpolicy <> GTK_POLICY_NEVER) and (hpolicy <> GTK_POLICY_EXTERNAL);

  if (LCLScrolledWinExtentsH = 0) or (LCLScrolledWinExtentsW = 0) then
    LCLGtkScrolledWindowComputeExtents(widget);

  vscroll := PGtkScrolledWindow(widget)^.get_vscrollbar;
  hscroll := PGtkScrolledWindow(widget)^.get_hscrollbar;

  { MINIMUM THRESHOLDS (Correct Orientation) }
  
  // threshH: Needs horizontal scrollbar thickness + CSS extents
  threshH := LCLScrolledWinExtentsH;
  if hActive and Assigned(hscroll) and Gtk3IsWidget(hscroll) then
  begin
    minSize := 0;
    gtk_widget_get_preferred_height(hscroll, @minSize, nil);
    if minSize > 0 then
      inc(threshH, minSize);
  end;

  // threshW: Needs vertical scrollbar thickness + CSS extents
  threshW := LCLScrolledWinExtentsW;
  if vActive and Assigned(vscroll) and Gtk3IsWidget(vscroll) then
  begin
    minSize := 0;
    gtk_widget_get_preferred_width(vscroll, @minSize, nil);
    if minSize > 0 then
      inc(threshW, minSize);
  end;

  { SURGICAL PROTECTION }
  { Zero/tiny allocation: control is intentionally collapsed (Height:=0, or the
    1px minimum AlignControls leaves for alBottom controls set to Height=0).
    Do NOT call ParentClass: for AUTOMATIC vpolicy the scrollbar machinery tries
    to lay out inside a <=1px area => cascade. Just exit.
    set_child_visible(False) is handled in TGtk3Widget.SetBounds for h<=1. }
  if (allocation^.width = 0) or (allocation^.height <= 1) then
  begin
    {$IFDEF GTK3DEBUGHEIGHTZERO}
    writeln(Format('LCLGSW.SizeAllocate: h=0 EXIT (child_vis=%s mapped=%s)',
      [BoolToStr(widget^.get_child_visible, True), BoolToStr(widget^.get_mapped, True)]));
    {$ENDIF}
    exit;
  end;

  { Non-zero but too small - clamp to prevent scrollbar cascade. }
  if (allocation^.width < threshW) or (allocation^.height < threshH) then
  begin
    if allocation^.width < threshW then allocation^.width := threshW;
    if allocation^.height < threshH then allocation^.height := threshH;
  end;

  ParentClass^.size_allocate(widget, allocation);
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
begin
  { LCLGtkScrolledWindow: TRULY SOFT WIDTH }
  { We force 10px to break the expansion loop. Child positions (e.g. x=1950) }
  { in a docked IDE otherwise 'push' the window to be too wide. }
  min_width^ := 10;
  nat_width^ := 10;
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
    [widget,
     IfThen(Assigned(AControl) and Assigned(AControl.LCLObject),
       AControl.LCLObject.Name + ':' + AControl.LCLObject.ClassName, 'nil'),
     LCLIntendedH,
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

  min_height^ := Max(10, Min(min_height^, LCLIntendedH));
  nat_height^ := Max(nat_height^, LCLIntendedH);
  nat_height^ := Max(nat_height^, min_height^);
  {$IFDEF GTK3DEBUGHEIGHTZERO}
  writeln(Format('LCLGSW.GetPrefH: positive → min=%d nat=%d (LCLIntendedH=%d)',
    [min_height^, nat_height^, LCLIntendedH]));
  {$ENDIF}
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
