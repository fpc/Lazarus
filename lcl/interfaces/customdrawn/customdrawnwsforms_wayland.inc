{%MainUnit customdrawnwsforms.pp}

{ TCDWSCustomForm -- Wayland host implementation. }

{ Remove a NAME=VALUE entry from the process env. Walks BaseUnix.envp
  (which is the kernel-provided env array, exposed by FPC's RTL via
  the symbol operatingsystem_parameter_envp -- no libc) and unlinks
  the matching slot by shifting later entries down. }
procedure WLUnsetEnv(const Name: AnsiString);
var
  i, j, NameLen: Integer;
  Entry: PAnsiChar;
  Match: Boolean;
begin
  if envp = nil then Exit;
  NameLen := Length(Name);
  i := 0;
  while envp[i] <> nil do
  begin
    Entry := envp[i];
    Match := True;
    for j := 1 to NameLen do
      if Entry[j - 1] <> Name[j] then
      begin
        Match := False;
        Break;
      end;
    if Match and (Entry[NameLen] = '=') then
    begin
      { Shift later entries down by one slot, including the trailing nil. }
      j := i;
      while envp[j] <> nil do
      begin
        envp[j] := envp[j + 1];
        Inc(j);
      end;
      Exit;
    end;
    Inc(i);
  end;
end;

{ Allocate (or re-allocate) a wl_shm pool of size W*H*4. Idempotent for
  unchanged size. Releases the previous mapping/buffer if the size
  changes. The buffer's pixel format is XRGB8888. }

class procedure TCDWSCustomForm.WLEnsurePool(WI: TWaylandWindowInfo;
  AWidth, AHeight: LongInt);
var
  Stride: LongInt;
  Size: PtrInt;
  Fd: cint;
  Data: Pointer;
begin
  if (AWidth <= 0) or (AHeight <= 0) then Exit;
  if (WI.PoolBuffer <> nil) and
     (WI.PoolWidth = AWidth) and (WI.PoolHeight = AHeight) then
    Exit;

  if WI.PoolData <> nil then
  begin
    FpMUnMap(WI.PoolData, WI.PoolSize);
    WI.PoolData := nil;
  end;
  if WI.PoolBuffer <> nil then
  begin
    WI.PoolBuffer.DestroyRequest;
    WI.PoolBuffer := nil;
  end;
  if WI.Pool <> nil then
  begin
    WI.Pool.DestroyRequest;
    WI.Pool := nil;
  end;

  Stride := AWidth * 4;
  Size := PtrInt(Stride) * PtrInt(AHeight);
  Fd := AllocateAnonymousFile(Size);
  Data := FpMMap(nil, Size, PROT_READ or PROT_WRITE, MAP_SHARED, Fd, 0);
  if PtrUInt(Data) = High(PtrUInt) then
  begin
    FpClose(Fd);
    raise Exception.CreateFmt('mmap failed (errno=%d)', [fpgeterrno]);
  end;

  WI.Pool := CDWidgetset.FWlShm.CreatePool(Fd, Size);
  WI.PoolBuffer := WI.Pool.CreateBuffer(0, AWidth, AHeight, Stride, WL_SHM_FORMAT_XRGB8888);
  CDWidgetset.FWlDisplay.Connection.Flush;
  FpClose(Fd);

  WI.PoolData   := Data;
  WI.PoolSize   := Size;
  WI.PoolWidth  := AWidth;
  WI.PoolHeight := AHeight;
  WI.PoolStride := Stride;
end;

{ Paint the form into the shm buffer, then attach + damage + commit.
  Uses LCLSendPaintMsgToTheForm to ask the LCL to fill WI.Canvas, then
  copies the canvas pixels into the wl_shm buffer. }

class procedure TCDWSCustomForm.WLPaintForm(WI: TWaylandWindowInfo);
var
  W, H, Y: LongInt;
  RowBytes: LongInt;
  SrcRow, DstRow: PByte;
begin
  if (WI = nil) or (WI.LCLForm = nil) then Exit;
  W := WI.LCLForm.Width;
  H := WI.LCLForm.Height;
  if (W <= 0) or (H <= 0) then Exit;

  WLEnsurePool(WI, W, H);

  { Allocate / resize the form's TCDForm.Image + .Canvas pair. We use
    the helper directly (not WI.UpdateImageAndCanvas) because TCDForm
    inherits .UpdateImageAndCanvas from TCDBaseControl, where it
    updates the ControlImage / ControlCanvas fields -- *not* .Image /
    .Canvas, which is what RenderForm draws into. The X11 host has the
    same arrangement; see customdrawnwsforms_x11.inc EvPaint. }
  UpdateControlLazImageAndCanvas(WI.Image, WI.Canvas, W, H, clfBGRA32);
  if (WI.Canvas = nil) or (WI.Image = nil) then Exit;

  { Let the LCL fill the form's offscreen canvas. }
  RenderForm(WI.Image, WI.Canvas, WI.LCLForm);

  { Copy from the LazIntfImage row-by-row into the wl_shm buffer.
    Both are 32 bpp; the LazCanvas image is BGRA-ordered (clfBGRA32),
    and XRGB8888 on a little-endian wire is byte order B,G,R,X -- so a
    straight memcpy gives the correct on-screen colours. }
  RowBytes := W * 4;
  for Y := 0 to H - 1 do
  begin
    SrcRow := PByte(WI.Image.GetDataLineStart(Y));
    DstRow := PByte(WI.PoolData) + Y * WI.PoolStride;
    Move(SrcRow^, DstRow^, RowBytes);
  end;

  WI.Surface.Attach(WI.PoolBuffer, 0, 0);
  WI.Surface.Damage(0, 0, W, H);
  WI.Surface.Commit;
end;



{ Forms with these classes are mapped as xdg_popup, not xdg_toplevel:
  short-lived transient surfaces that need the compositor's grab /
  auto-dismiss / on-screen-positioning. Detection by class-name string
  is the standard Lazarus widgetset cross-cutting check (the LCL's own
  Cocoa / Qt glue uses the same idiom) and keeps customdrawnwsmenus.pas
  free of Wayland-specific imports. THintWindow is forward-looking: a
  follow-up commit will route hint windows through the same path. }
function WLIsPopupClass(F: TCustomForm): Boolean;
begin
  Result := (F <> nil)
    and ( (F.ClassName = 'TCDPopUpMenuForm')
       or (F is THintWindow) );
end;

class function TCDWSCustomForm.DoCreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  WI: TWaylandWindowInfo;
  WSet: TCDWidgetSet;
  EnvToken: string;
  ParentWI: TWaylandWindowInfo;
  Anchor: TRect;
begin
  { LCL class method -- no widget-set Self. Resolve the typed view of
    the LCL global once and use it through the rest of this method, so
    callbacks bind as method pointers on a real instance. }
  WSet := CDWidgetset;

  WI := TWaylandWindowInfo.Create;
  WI.LCLForm := TCustomForm(AWinControl);

  WI.Surface  := WSet.FWlCompositor.CreateSurface;
  WI.XdgSurf  := WSet.FXdgWmBase.GetXdgSurface(WI.Surface);
  WI.XdgSurf.OnConfigure := @WSet.WLHandleSurfaceConfigure;

  if WLIsPopupClass(WI.LCLForm) then
  begin
    { Popup branch: defer role assignment + first commit to DoShowHide.
      The LCL's TCDWSPopupMenu.Popup sets Left/Top before triggering
      handle creation (via Canvas access for TextHeight) but only sets
      Width/Height + adds child items afterwards. Calling get_popup
      with the default 320x240 bounds would lock in a wrong-size
      surface; we wait for ShowHide(visible=True), by which point all
      bounds and children are in place. }
    WI.PopupPending := True;
  end
  else
  begin
    WI.Toplevel := WI.XdgSurf.GetToplevel;
    WI.Toplevel.OnConfigure := @WSet.WLHandleToplevelConfigure;
    WI.Toplevel.OnClose     := @WSet.WLHandleToplevelClose;
    if AWinControl.Caption <> '' then
      WI.Toplevel.SetTitle(AWinControl.Caption);
    WI.Toplevel.SetAppId('org.lazarus.customdrawn');
    if AParams.WndParent <> 0 then
    begin
      ParentWI := TWaylandWindowInfo(AParams.WndParent);
      if (ParentWI <> nil) and (ParentWI.Toplevel <> nil) then
        WI.Toplevel.SetParent(ParentWI.Toplevel);
    end;
  end;

  WI.PendingDraw := True;
  CDWidgetset.FWlWindowList.Add(WI);

  { Consume any launcher-supplied XDG_ACTIVATION_TOKEN exactly once.
    This tells the compositor that the newly launched window has arrived;
    without it many compositors keep showing
    the loading/busy cursor until a hard timeout expires. We unset
    the env var afterwards per the xdg-activation spec, so that any
    process this app spawns later doesn't inherit and re-use it.
    Toplevel-only: most compositors ignore activation tokens for popup
    surfaces. }
  if (CDWidgetset.FXdgActivation <> nil)
     and (CDWidgetset.FWlWindowList.Count = 1)
     and (WI.Toplevel <> nil) then
  begin
    EnvToken := GetEnvironmentVariable('XDG_ACTIVATION_TOKEN');
    if EnvToken <> '' then
    begin
      CDWidgetset.FXdgActivation.Activate(EnvToken, WI.Surface);
      WLUnsetEnv('XDG_ACTIVATION_TOKEN');
    end;
  end;

  { Initial commit asks the compositor for the first xdg_surface.configure.
    Skip this for popup-pending WIs: an xdg_surface with no role assigned
    is a protocol error to commit. The deferred path in DoShowHide
    commits after attaching the popup role. }
  if not WI.PopupPending then
  begin
    WI.Surface.Commit;
    CDWidgetset.FWlDisplay.Connection.Flush;
  end;

  Result := TLCLHandle(WI);
end;

class procedure TCDWSCustomForm.DoShowHide(const AWinControl: TWinControl);
begin
  { No-op: the WS layer routes show/hide through ShowHide (override),
    not this method. Wayland has no explicit hide -- visibility is
    implied by attaching / detaching a buffer. Real hide-by-detach is
    a follow-up. }
end;

class function TCDWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
begin
  Result := DoCreateHandle(AWinControl, AParams);
end;

class procedure TCDWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
var
  WI: TWaylandWindowInfo;
  Idx: Integer;
begin
  if AWinControl.Handle = 0 then Exit;
  WI := TWaylandWindowInfo(AWinControl.Handle);
  { Drop any focus references before we Free the WI. Wayland may not
    deliver a final keyboard.leave / pointer.leave when we destroy the
    surface ourselves (the focus loss is implicit), so without this
    the next key / motion event dereferences a freed pointer. }
  if CDWidgetset.FWlKeyboardFocus = WI then
    CDWidgetset.FWlKeyboardFocus := nil;
  if CDWidgetset.FWlPointerFocus = WI then
    CDWidgetset.FWlPointerFocus := nil;
  { TCDWidgetSet.FocusedControl / FocusedIntfControl point at LCL
    TWinControls. By the time a form's DestroyHandle runs,
    TWinControl.Destroy has already freed the form's children -- we
    can't safely walk Parent to test "does this focused control belong
    to me?". Clear unconditionally: if we don't, the next focus shift
    LCLSendKillFocusMsg's a dangling pointer. We may discard a
    legitimate focus belonging to another (still-alive) form, but the
    next focus event re-establishes it. }
  CDWidgetset.FocusedControl := nil;
  CDWidgetset.FocusedIntfControl := nil;
  Idx := CDWidgetset.FWlWindowList.IndexOf(WI);
  if Idx >= 0 then CDWidgetset.FWlWindowList.Delete(Idx);
  if WI.PoolData <> nil then FpMUnMap(WI.PoolData, WI.PoolSize);
  if WI.PoolBuffer <> nil then WI.PoolBuffer.DestroyRequest;
  if WI.Pool <> nil then WI.Pool.DestroyRequest;
  { xdg-shell teardown order: role object (toplevel / popup) -> xdg_surface
    -> wl_surface. Toplevel and Popup are mutually exclusive. }
  if WI.Popup    <> nil then WI.Popup.DestroyRequest;
  if WI.Toplevel <> nil then WI.Toplevel.DestroyRequest;
  if WI.XdgSurf <> nil then WI.XdgSurf.DestroyRequest;
  if WI.Surface <> nil then WI.Surface.DestroyRequest;
  WI.Free;
end;

class procedure TCDWSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  WI: TWaylandWindowInfo;
begin
  if AWinControl.Handle = 0 then Exit;
  WI := TWaylandWindowInfo(AWinControl.Handle);
  WI.PendingDraw := True;
end;

class procedure TCDWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  { TODO: future decoration work. If we use client-side decorations,
    close/min/max buttons have to be drawn and handled by customdrawn. }
end;

class procedure TCDWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  { TODO: future decoration work. Either draw CSD matching BorderStyle
    or negotiate SSD via zxdg_decoration_manager_v1 where the compositor
    supports server-side decorations. }
end;

class procedure TCDWSCustomForm.SetIcon(const AForm: TCustomForm;
  const Small, Big: HICON);
begin
  { See TCDWidgetSet.AppSetIcon: xdg-shell has no per-window icon
    request; shells normally resolve icons from app_id / desktop-entry
    metadata. }
end;

class procedure TCDWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
  { Taskbar visibility is compositor/shell policy rather than an
    xdg-shell client setting. }
end;

class procedure TCDWSCustomForm.SetRealPopupParent(const ACustomForm: TCustomForm;
  const APopupParent: TCustomForm);
var
  WI, ParentWI: TWaylandWindowInfo;
begin
  { Hooked from TCustomForm.SetPopupParent (and the same code path
    LCL takes when a non-modal child's PopupMode/PopupParent change).
    Forward to xdg_toplevel.set_parent: passing nil detaches. }
  if ACustomForm.Handle = 0 then Exit;
  WI := TWaylandWindowInfo(ACustomForm.Handle);
  if (WI = nil) or (WI.Toplevel = nil) then Exit;
  if (APopupParent = nil) or (APopupParent.Handle = 0) then
  begin
    WI.Toplevel.SetParent(nil);
    Exit;
  end;
  ParentWI := TWaylandWindowInfo(APopupParent.Handle);
  if (ParentWI = nil) or (ParentWI.Toplevel = nil) then Exit;
  WI.Toplevel.SetParent(ParentWI.Toplevel);
end;

class procedure TCDWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
var
  WI, ParentWI: TWaylandWindowInfo;
  ParentForm: TCustomForm;
begin
  { Wayland's analogue of the win32 / gtk "set transient for" hint is
    xdg_toplevel.set_parent: tells the compositor this toplevel is a
    child of another, so it stacks above the parent, fades / dims the
    parent on capable shells, and returns focus to the parent on
    close. The LCL itself runs the modal loop in TForm.ShowModal; this
    method only wires the compositor transient-parent hint. }
  if ACustomForm.Handle = 0 then Exit;
  WI := TWaylandWindowInfo(ACustomForm.Handle);
  if (WI = nil) or (WI.Toplevel = nil) then Exit;

  ParentForm := ACustomForm.GetRealPopupParent;
  if (ParentForm = nil) or (ParentForm.Handle = 0) then Exit;
  ParentWI := TWaylandWindowInfo(ParentForm.Handle);
  if (ParentWI = nil) or (ParentWI.Toplevel = nil) then Exit;

  WI.Toplevel.SetParent(ParentWI.Toplevel);
end;

{ Tear down the Wayland-side state of a popup form so the surface
  vanishes from the screen. Used for both LCL Hide (Self.Close ->
  caHide -> SetVisible(False) -> ShowHide) and grab-dismiss via
  popup_done. The LCL TCustomForm survives so caching schemes like
  TCDWSPopupMenu.Popup's CDPopUpMenus list keep working; only the
  Wayland objects are dropped. The next Show on this same handle
  would re-allocate them, but customdrawnwsmenus.pas creates a fresh
  TCDPopUpMenuForm each time so that path doesn't actually exercise. }
class procedure TCDWSCustomForm.WLTeardownPopup(WI: TWaylandWindowInfo);
begin
  if WI = nil then Exit;
  if (WI.Popup = nil) and not WI.PopupPending then Exit;

  { Tooltip-safety invariant (rule (2)): if the popup we're tearing
    down is the parent of the active tooltip, dismiss the tooltip
    first. Otherwise our xdg_popup.destroy on WI would land while it
    still has a live child popup -> "destroy popup with children"
    protocol error. The .Hide call recurses through ShowHide ->
    WLTeardownPopup for the tooltip; FWlActiveTooltip is cleared at
    the bottom of that recursive call. }
  if (CDWidgetset.FWlActiveTooltip <> nil)
     and (CDWidgetset.FWlActiveTooltip <> WI)
     and (CDWidgetset.FWlActiveTooltip.ParentWI = WI) then
  begin
    if CDWidgetset.FWlActiveTooltip.LCLForm <> nil then
      CDWidgetset.FWlActiveTooltip.LCLForm.Hide;
  end;

  if CDWidgetset.FWlPointerFocus  = WI then CDWidgetset.FWlPointerFocus  := nil;
  if CDWidgetset.FWlKeyboardFocus = WI then CDWidgetset.FWlKeyboardFocus := nil;
  if WI.PoolData <> nil then
  begin
    FpMUnMap(WI.PoolData, WI.PoolSize);
    WI.PoolData := nil;
    WI.PoolSize := 0;
  end;
  if WI.PoolBuffer <> nil then begin WI.PoolBuffer.DestroyRequest; WI.PoolBuffer := nil; end;
  if WI.Pool      <> nil then begin WI.Pool.DestroyRequest;       WI.Pool := nil; end;
  if WI.Popup     <> nil then begin WI.Popup.DestroyRequest;      WI.Popup := nil; end;
  if WI.XdgSurf   <> nil then begin WI.XdgSurf.DestroyRequest;    WI.XdgSurf := nil; end;
  if WI.Surface   <> nil then begin WI.Surface.DestroyRequest;    WI.Surface := nil; end;
  WI.HasConfigure := False;
  WI.PendingDraw  := False;
  WI.PopupPending := False;
  WI.ParentWI     := nil;
  if CDWidgetset.FWlActiveTooltip = WI then
    CDWidgetset.FWlActiveTooltip := nil;
  CDWidgetset.FWlDisplay.Connection.Flush;
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  WI: TWaylandWindowInfo;
  WSet: TCDWidgetSet;
  ParentWI: TWaylandWindowInfo;
  Anchor: TRect;
begin
  if AWinControl.Handle = 0 then Exit;
  WI := TWaylandWindowInfo(AWinControl.Handle);
  WSet := CDWidgetset;

  if WLIsPopupClass(WI.LCLForm) and not AWinControl.Visible then
  begin
    { Hide path for popup-class forms: tear down Wayland objects so the
      surface vanishes immediately. Without this the dropdown stays on
      screen until the popup role is torn down -- Self.Close from HandleItemClick lands here
      with Visible=False, but customdrawn's default ShowHide is a
      no-op. }
    WLTeardownPopup(WI);
    Exit;
  end;

  { Re-show after a previous teardown: WLTeardownPopup destroyed the
    wl_surface / xdg_surface so the popup could vanish, but the LCL
    handle survives. THintWindow specifically gets reused across many
    hover cycles, so we have to recreate the role-less surface stack
    and mark the popup pending again -- the deferred create block
    below then attaches a fresh xdg_popup with the new bounds. }
  if WLIsPopupClass(WI.LCLForm) and AWinControl.Visible
     and (WI.Surface = nil) then
  begin
    WI.Surface := WSet.FWlCompositor.CreateSurface;
    WI.XdgSurf := WSet.FXdgWmBase.GetXdgSurface(WI.Surface);
    WI.XdgSurf.OnConfigure := @WSet.WLHandleSurfaceConfigure;
    WI.PopupPending := True;
  end;

  { Deferred xdg_popup creation. By the time TCDWSPopupMenu.Popup
    reaches here, the popup form's Left / Top / Width / Height +
    child items are all settled (DoCreateHandle ran early during
    Canvas access for TextHeight measurement, when only Left / Top
    were known). Attach the popup role with the now-correct bounds
    and issue the first commit so the compositor sends configure. }
  if WI.PopupPending and AWinControl.Visible then
  begin
    ParentWI := WSet.WLChoosePopupParent(WI.LCLForm);
    if ParentWI <> nil then
    begin
      WI.ParentWI := ParentWI;
      if not WSet.WLComputePopupAnchorRect(WI.LCLForm, ParentWI, Anchor) then
        Anchor := Bounds(0, 0, 1, 1);
      WSet.WLCreatePopupForWindow(WI, ParentWI, Anchor);
      WI.PopupPending := False;
      WI.Surface.Commit;
      WSet.FWlDisplay.Connection.Flush;
    end;
  end;

  WI.PendingDraw := True;
end;

class function TCDWSCustomForm.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  WI: TWaylandWindowInfo;
begin
  { Wayland has no request for reading another client's title from the
    compositor. For our own form, return the title cached by SetText;
    do not read Caption here because Caption routes back through this
    widgetset method once the handle exists. }
  if AWinControl.Handle = 0 then
    Exit(False);
  WI := TWaylandWindowInfo(AWinControl.Handle);
  AText := WI.Title;
  Result := True;
end;

class function TCDWSCustomForm.GetTextLen(const AWinControl: TWinControl;
  var ALength: Integer): Boolean;
var
  S: String;
begin
  Result := GetText(AWinControl, S);
  if Result then
    ALength := Length(S)
  else
    ALength := 0;
end;

class procedure TCDWSCustomForm.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  WI: TWaylandWindowInfo;
begin
  if AWinControl.Handle = 0 then Exit;
  WI := TWaylandWindowInfo(AWinControl.Handle);
  WI.Title := AText;
  if Assigned(WI.Toplevel) then
    WI.Toplevel.SetTitle(AText);
end;

class function TCDWSCustomForm.GetClientBounds(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
begin
  ARect := Bounds(0, 0, AWinControl.Width, AWinControl.Height);
  Result := True;
end;

class function TCDWSCustomForm.GetClientRect(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
begin
  ARect := Bounds(0, 0, AWinControl.Width, AWinControl.Height);
  Result := True;
end;
