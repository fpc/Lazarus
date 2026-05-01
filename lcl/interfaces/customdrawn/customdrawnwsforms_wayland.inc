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

{ Per-window callbacks. The xdgshell callbacks pass us the surface or
  toplevel that fired; we look up the matching WindowInfo from the
  widgetset's FWlWindowList. }

procedure WLHandleSurfaceConfigure(Sender: TXdgSurface; Serial: LongWord);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  Sender.AckConfigure(Serial);
  for i := 0 to CDWidgetset.FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(CDWidgetset.FWlWindowList[i]);
    if (WI <> nil) and (WI.XdgSurf = Sender) then
    begin
      WI.HasConfigure := True;
      WI.PendingDraw := True;
      Exit;
    end;
  end;
end;

procedure WLHandleToplevelConfigure(Sender: TXdgToplevel;
  W, H: LongInt; const States: array of LongWord);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  if (W <= 0) or (H <= 0) then Exit;
  for i := 0 to CDWidgetset.FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(CDWidgetset.FWlWindowList[i]);
    if (WI <> nil) and (WI.Toplevel = Sender) then
    begin
      if (WI.LCLForm <> nil) and
         ((WI.LCLForm.Width <> W) or (WI.LCLForm.Height <> H)) then
      begin
        WI.LCLForm.SetBounds(WI.LCLForm.Left, WI.LCLForm.Top, W, H);
        WI.PendingDraw := True;
      end;
      Exit;
    end;
  end;
end;

procedure WLHandleToplevelClose(Sender: TXdgToplevel);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  for i := 0 to CDWidgetset.FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(CDWidgetset.FWlWindowList[i]);
    if (WI <> nil) and (WI.Toplevel = Sender) then
    begin
      WI.ShouldClose := True;
      if WI.LCLForm <> nil then
        LCLSendCloseQueryMsg(WI.LCLForm);
      Exit;
    end;
  end;
end;


class function TCDWSCustomForm.DoCreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  WI: TWaylandWindowInfo;
  EnvToken: string;
begin
  WI := TWaylandWindowInfo.Create;
  WI.LCLForm := TCustomForm(AWinControl);

  WI.Surface  := CDWidgetset.FWlCompositor.CreateSurface;
  WI.XdgSurf  := CDWidgetset.FXdgWmBase.GetXdgSurface(WI.Surface);
  WI.XdgSurf.OnConfigure := @WLHandleSurfaceConfigure;
  WI.Toplevel := WI.XdgSurf.GetToplevel;
  WI.Toplevel.OnConfigure := @WLHandleToplevelConfigure;
  WI.Toplevel.OnClose     := @WLHandleToplevelClose;
  if AWinControl.Caption <> '' then
    WI.Toplevel.SetTitle(AWinControl.Caption);
  WI.Toplevel.SetAppId('org.lazarus.customdrawn');

  WI.PendingDraw := True;
  CDWidgetset.FWlWindowList.Add(WI);

  { Consume any launcher-supplied XDG_ACTIVATION_TOKEN exactly once.
    This is what tells the compositor "the new window the user just
    launched has arrived" -- without it many compositors keep showing
    the loading/busy cursor until a hard timeout expires. We unset
    the env var afterwards per the xdg-activation spec, so that any
    process this app spawns later doesn't inherit and re-use it. }
  if (CDWidgetset.FXdgActivation <> nil)
     and (CDWidgetset.FWlWindowList.Count = 1) then
  begin
    EnvToken := GetEnvironmentVariable('XDG_ACTIVATION_TOKEN');
    if EnvToken <> '' then
    begin
      CDWidgetset.FXdgActivation.Activate(EnvToken, WI.Surface);
      WLUnsetEnv('XDG_ACTIVATION_TOKEN');
    end;
  end;

  { Initial commit asks the compositor for the first xdg_surface.configure. }
  WI.Surface.Commit;
  CDWidgetset.FWlDisplay.Connection.Flush;

  Result := TLCLHandle(WI);
end;

class procedure TCDWSCustomForm.DoShowHide(const AWinControl: TWinControl);
begin
  { FIXME: stub. Wayland has no explicit show/hide -- visibility is
    implied by attaching/detaching a buffer. To actually "hide" we
    would need to attach nil and commit. Left as no-op for now. }
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
  Idx := CDWidgetset.FWlWindowList.IndexOf(WI);
  if Idx >= 0 then CDWidgetset.FWlWindowList.Delete(Idx);
  if WI.PoolData <> nil then FpMUnMap(WI.PoolData, WI.PoolSize);
  if WI.PoolBuffer <> nil then WI.PoolBuffer.DestroyRequest;
  if WI.Pool <> nil then WI.Pool.DestroyRequest;
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

class procedure TCDWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  { FIXME: stub. Should call xdg_toplevel.set_parent + run nested
    event loop until the modal is dismissed. }
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  WI: TWaylandWindowInfo;
begin
  if AWinControl.Handle = 0 then Exit;
  WI := TWaylandWindowInfo(AWinControl.Handle);
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
