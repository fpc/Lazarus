{%MainUnit customdrawnint.pas}

{ All TCDWidgetSet methods specific to the Wayland backend.

  Modelled on the Cocoa backend (smallest existing host). The Wayland
  protocol grunt-work lives in waylandwire / waylandcore / xdgshell. }

{ TCDWidgetSet }

procedure TCDWidgetSet.WLHandleGlobal(Name: LongWord;
  const Iface: AnsiString; Version: LongWord);
var
  V: LongWord;
begin
  if Iface = 'wl_compositor' then
  begin
    if Version > 4 then V := 4 else V := Version;
    FWlCompositor := TWaylandCompositor(
      FWlRegistry.BindRaw(Name, Iface, V, TWaylandCompositor));
  end
  else if Iface = 'wl_shm' then
  begin
    FWlShm := TWaylandShm(
      FWlRegistry.BindRaw(Name, Iface, 1, TWaylandShm));
  end
  else if Iface = 'xdg_wm_base' then
  begin
    if Version > 5 then V := 5 else V := Version;
    FXdgWmBase := TXdgWmBase(
      FWlRegistry.BindRaw(Name, Iface, V, TXdgWmBase));
    FXdgWmBase.OnPing := @WLHandlePing;
  end
  else if Iface = 'xdg_activation_v1' then
  begin
    FXdgActivation := TXdgActivationV1(
      FWlRegistry.BindRaw(Name, Iface, 1, TXdgActivationV1));
  end
  else if Iface = 'wl_seat' then
  begin
    { v1 is enough for non-frame'd pointer + key + modifiers events.
      v5+ adds frame/axis_source/etc. that we currently ignore. }
    FWlSeat := TWaylandSeat(
      FWlRegistry.BindRaw(Name, Iface, 1, TWaylandSeat));
    FWlSeat.OnCapabilities := @WLHandleSeatCaps;
  end
  else if Iface = 'wp_cursor_shape_manager_v1' then
  begin
    FWpCursorShapeMgr := TWpCursorShapeManagerV1(
      FWlRegistry.BindRaw(Name, Iface, 1, TWpCursorShapeManagerV1));
  end
  else if Iface = 'wl_data_device_manager' then
  begin
    { v3 has set_actions / dnd_drop_performed / dnd_finished / action,
      a destroy request, and source_actions / action on offers. v1 is
      enough for selection (clipboard); we cap at 3 to take whatever
      the compositor offers up to that. }
    if Version > 3 then V := 3 else V := Version;
    FWlDataDeviceManager := TWlDataDeviceManager(
      FWlRegistry.BindRaw(Name, Iface, V, TWlDataDeviceManager));
  end
  else if Iface = 'wl_output' then
  begin
    { Bind v2 minimum so we get the scale event (HiDPI). v3 adds release;
      v4 adds name / description (informational). The compositor can
      advertise more than one wl_output (one per monitor); each shows up
      as its own global so this branch fires per monitor. }
    if Version > 4 then V := 4
    else if Version < 2 then V := Version  { older compositors -- no scale }
    else V := Version;
    if FWlOutputs = nil then FWlOutputs := TFPList.Create;
    FWlOutputs.Add(FWlRegistry.BindRaw(Name, Iface, V, TWaylandOutput));
  end;
end;

procedure TCDWidgetSet.WLHandlePing(Serial: LongWord);
begin
  if Assigned(FXdgWmBase) then
    FXdgWmBase.Pong(Serial);
end;

procedure TCDWidgetSet.WLHandleSeatCaps(Sender: TWaylandSeat; Caps: LongWord);
begin
  if ((Caps and WL_SEAT_CAPABILITY_POINTER) <> 0)
     and (FWlPointer = nil) then
  begin
    FWlPointer := Sender.GetPointer;
    FWlPointer.OnEnter  := @WLHandlePointerEnter;
    FWlPointer.OnLeave  := @WLHandlePointerLeave;
    FWlPointer.OnMotion := @WLHandlePointerMotion;
    FWlPointer.OnButton := @WLHandlePointerButton;
    FWlPointer.OnAxis   := @WLHandlePointerAxis;
    if (FWpCursorShapeMgr <> nil) and (FWpCursorShapeDev = nil) then
      FWpCursorShapeDev := FWpCursorShapeMgr.GetPointer(FWlPointer);
  end;
  if ((Caps and WL_SEAT_CAPABILITY_KEYBOARD) <> 0)
     and (FWlKeyboard = nil) then
  begin
    FWlKeyboard := Sender.GetKeyboard;
    FWlKeyboard.OnKeymap    := @WLHandleKeymap;
    FWlKeyboard.OnEnter     := @WLHandleKeyboardEnter;
    FWlKeyboard.OnLeave     := @WLHandleKeyboardLeave;
    FWlKeyboard.OnKey       := @WLHandleKey;
    FWlKeyboard.OnModifiers := @WLHandleKeyMods;
  end;
end;

function TCDWidgetSet.WLFindWindowBySurface(Surface: TWaylandSurface): TWaylandWindowInfo;
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  Result := nil;
  if (FWlWindowList = nil) or (Surface = nil) then Exit;
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
    if (WI <> nil) and (WI.Surface = Surface) then Exit(WI);
  end;
end;
{ ------------------------------------------------------------------
  Input handling. Mouse + keyboard both arrive on a single wl_seat
  bound at registry-time. We never reach LCL with raw scancodes --
  we translate evdev codes to VK_* and (for printable keys) a
  hardcoded US-QWERTY layout. FIXME: a real implementation should
  mmap the wl_keyboard.keymap fd and feed it to xkbcommon for
  correct character translation under non-US layouts.
  ------------------------------------------------------------------ }

function TCDWidgetSet.WLCurrentShiftState: TShiftState;
begin
  Result := [];
  if FWlShiftHeld then Include(Result, ssShift);
  if FWlCtrlHeld  then Include(Result, ssCtrl);
  if FWlAltHeld   then Include(Result, ssAlt);
end;

procedure TCDWidgetSet.WLHandlePointerEnter(Sender: TWaylandPointer;
  Serial: LongWord; Surface: TWaylandSurface; X, Y: LongInt);
begin
  if WaylandTrace then Trace(' input',
    Format('pointer enter surface=%p at (%d,%d)', [Pointer(Surface), X, Y]));
  { Wayland requires the client to set the cursor on every pointer enter
    event; without it the cursor shape stays at whatever the previous
    surface owner left it on (manifests as wrong/sticky cursor inside
    our window). Use wp_cursor_shape_v1 if the compositor offers it
    (server provides a themed cursor) -- nothing to fall back to without
    a wl_shm cursor bitmap, and we don't ship one. }
  if FWpCursorShapeDev <> nil then
    FWpCursorShapeDev.SetShape(Serial, WP_CURSOR_SHAPE_DEFAULT);
  FWlPointerFocus := WLFindWindowBySurface(Surface);
  FWlPointerX := X;
  FWlPointerY := Y;
  if FWlPointerFocus <> nil then
    CallbackMouseMove(FWlPointerFocus, X, Y, WLCurrentShiftState);
end;

procedure TCDWidgetSet.WLHandlePointerLeave(Sender: TWaylandPointer;
  Serial: LongWord; Surface: TWaylandSurface);
begin
  FWlPointerFocus := nil;
end;

procedure TCDWidgetSet.WLHandlePointerMotion(Sender: TWaylandPointer;
  TimeMs: LongWord; X, Y: LongInt);
var
  WI: TWaylandWindowInfo;
begin
  FWlPointerX := X;
  FWlPointerY := Y;
  WI := FWlPointerFocus;
  if WI <> nil then
    CallbackMouseMove(WI, X, Y, WLCurrentShiftState);
end;

procedure TCDWidgetSet.WLHandlePointerButton(Sender: TWaylandPointer;
  Serial, TimeMs, Button, State: LongWord);
var
  WI: TWaylandWindowInfo;
  Btn: TMouseButton;
  Known: Boolean;
begin
  { Stash the serial so wl_data_device.set_selection (clipboard
    ownership) can use it -- the compositor requires the serial of a
    recent input event from this seat. }
  FWlLastInputSerial := Serial;
  if WaylandTrace then Trace(' input',
    Format('pointer button=$%x state=%d at (%d,%d) focus=%p',
      [Button, State, FWlPointerX, FWlPointerY, Pointer(FWlPointerFocus)]));
  WI := FWlPointerFocus;
  if (WI = nil) or (WI.LCLForm = nil) then Exit;
  Known := True;
  case Button of
    BTN_LEFT:   Btn := mbLeft;
    BTN_RIGHT:  Btn := mbRight;
    BTN_MIDDLE: Btn := mbMiddle;
  else
    Known := False;  { FIXME: side/extra/forward/back buttons not mapped. }
  end;
  if not Known then Exit;
  if State = WL_POINTER_BUTTON_PRESSED then
    CallbackMouseDown(WI, FWlPointerX, FWlPointerY, Btn, WLCurrentShiftState)
  else
    CallbackMouseUp(WI, FWlPointerX, FWlPointerY, Btn, WLCurrentShiftState);
end;

procedure TCDWidgetSet.WLHandlePointerAxis(Sender: TWaylandPointer;
  TimeMs, Axis: LongWord; Value: TWlFixed);
var
  WI: TWaylandWindowInfo;
  Delta: SmallInt;
  Target: TWinControl;
begin
  WI := FWlPointerFocus;
  if (WI = nil) or (WI.LCLForm = nil) then Exit;
  if Axis <> WL_POINTER_AXIS_VERTICAL_SCROLL then Exit;  { TODO: horizontal wheel / touchpad scroll }
  { Wayland axis value is wl_fixed in surface units (roughly pixel rows
    scrolled). Wayland convention: positive = scroll down; LCL/Win
    convention: positive = scroll up; hence the negation. We treat
    each axis event as one notch -- LCL's WHEEL_DELTA convention. }
  if Value > 0 then Delta := -120
  else if Value < 0 then Delta := 120
  else Exit;
  { Route to the control under the cursor (same pattern CallbackMouseDown
    uses). Sending to the form leaves wheel bubbling to LCL's default
    "scroll the focused control" path, which doesn't do the right
    thing if the cursor is over a non-focused listbox. }
  Target := FindControlWhichReceivedEvent(WI.LCLForm, WI.Children,
                                          FWlPointerX, FWlPointerY);
  if Target = nil then Target := WI.LCLForm;
  LCLSendMouseWheelMsg(Target, FWlPointerX, FWlPointerY, Delta,
                       WLCurrentShiftState);
end;

procedure TCDWidgetSet.WLHandleKeymap(Sender: TWaylandKeyboard;
  Format: LongWord; Fd: cint; Size: LongWord);
begin
  { FIXME: We close the fd without using the keymap. A real
    implementation would mmap it (Format=1 means xkb_v1) and feed
    the bytes to xkbcommon for layout-correct character translation
    and modifier state. For now we use a hardcoded US-QWERTY map. }
  if Fd >= 0 then FpClose(Fd);
end;

procedure TCDWidgetSet.WLHandleKeyboardEnter(Sender: TWaylandKeyboard;
  Serial: LongWord; Surface: TWaylandSurface);
begin
  FWlKeyboardFocus := WLFindWindowBySurface(Surface);
  if WaylandTrace then Trace(' input',
    Format('keyboard ENTER surface=%p focus=%p',
      [Pointer(Surface), Pointer(FWlKeyboardFocus)]));
end;

procedure TCDWidgetSet.WLHandleKeyboardLeave(Sender: TWaylandKeyboard;
  Serial: LongWord; Surface: TWaylandSurface);
begin
  FWlKeyboardFocus := nil;
  { Drop any held modifier state so we don't leak into the next focus. }
  FWlShiftHeld := False;
  FWlCtrlHeld  := False;
  FWlAltHeld   := False;
end;

{ Linux evdev keycode -> Windows VK_*. Covers the common keys for a
  US-QWERTY layout. FIXME: not exhaustive; numpad, media keys, and
  most non-US keys are missing. }
function WLEvdevToVK(Code: LongWord): Word;
begin
  case Code of
    1:   Result := VK_ESCAPE;
    2..10: Result := VK_1 + (Code - 2);                  { 1..9 }
    11:  Result := VK_0;
    12:  Result := VK_OEM_MINUS;
    13:  Result := VK_OEM_PLUS;
    14:  Result := VK_BACK;
    15:  Result := VK_TAB;
    16:  Result := VK_Q;  17: Result := VK_W;  18: Result := VK_E;
    19:  Result := VK_R;  20: Result := VK_T;  21: Result := VK_Y;
    22:  Result := VK_U;  23: Result := VK_I;  24: Result := VK_O;
    25:  Result := VK_P;
    26:  Result := VK_OEM_4;   { [ }
    27:  Result := VK_OEM_6;   { ] }
    28:  Result := VK_RETURN;
    29:  Result := VK_LCONTROL;
    30:  Result := VK_A;  31: Result := VK_S;  32: Result := VK_D;
    33:  Result := VK_F;  34: Result := VK_G;  35: Result := VK_H;
    36:  Result := VK_J;  37: Result := VK_K;  38: Result := VK_L;
    39:  Result := VK_OEM_1;   { ; }
    40:  Result := VK_OEM_7;   { ' }
    41:  Result := VK_OEM_3;   { ` }
    42:  Result := VK_LSHIFT;
    43:  Result := VK_OEM_5;   { \ }
    44:  Result := VK_Z;  45: Result := VK_X;  46: Result := VK_C;
    47:  Result := VK_V;  48: Result := VK_B;  49: Result := VK_N;
    50:  Result := VK_M;
    51:  Result := VK_OEM_COMMA;
    52:  Result := VK_OEM_PERIOD;
    53:  Result := VK_OEM_2;   { / }
    54:  Result := VK_RSHIFT;
    56:  Result := VK_LMENU;   { Left Alt }
    57:  Result := VK_SPACE;
    58:  Result := VK_CAPITAL;
    59..68: Result := VK_F1 + (Code - 59);   { F1..F10 }
    87:  Result := VK_F11;
    88:  Result := VK_F12;
    97:  Result := VK_RCONTROL;
    100: Result := VK_RMENU;   { Right Alt }
    102: Result := VK_HOME;
    103: Result := VK_UP;
    104: Result := VK_PRIOR;   { PageUp }
    105: Result := VK_LEFT;
    106: Result := VK_RIGHT;
    107: Result := VK_END;
    108: Result := VK_DOWN;
    109: Result := VK_NEXT;    { PageDown }
    110: Result := VK_INSERT;
    111: Result := VK_DELETE;
    125: Result := VK_LWIN;
    126: Result := VK_RWIN;
  else
    Result := 0;
  end;
end;

{ Convert evdev keycode to a printable ASCII character given current
  modifiers. US-QWERTY hardcoded. Returns 0 for non-printable keys.
  FIXME: ignores capslock; non-US layouts need keymap support. }
function WLEvdevToChar(Code: LongWord; Shifted: Boolean): Char;
const
  Lower: array[0..127] of Char = (
    {0..15}    #0, #27, '1','2','3','4','5','6','7','8','9','0','-','=', #8, #9,
    {16..31}   'q','w','e','r','t','y','u','i','o','p','[',']', #13, #0, 'a','s',
    {32..47}   'd','f','g','h','j','k','l',';',#39, '`', #0, '\','z','x','c','v',
    {48..63}   'b','n','m',',','.','/', #0, '*', #0,' ', #0, #0, #0, #0, #0, #0,
    {64..79}   #0, #0, #0, #0, #0, #0, #0, '7','8','9','-','4','5','6','+','1',
    {80..95}   '2','3','0','.', #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
    {96..111}  #13, #0, '/', #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
    {112..127} #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0
  );
  Upper: array[0..127] of Char = (
    {0..15}    #0, #27, '!','@','#','$','%','^','&','*','(',')','_','+', #8, #9,
    {16..31}   'Q','W','E','R','T','Y','U','I','O','P','{','}', #13, #0, 'A','S',
    {32..47}   'D','F','G','H','J','K','L',':','"','~', #0, '|','Z','X','C','V',
    {48..63}   'B','N','M','<','>','?', #0, '*', #0,' ', #0, #0, #0, #0, #0, #0,
    {64..79}   #0, #0, #0, #0, #0, #0, #0, '7','8','9','-','4','5','6','+','1',
    {80..95}   '2','3','0','.', #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
    {96..111}  #13, #0, '/', #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
    {112..127} #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0
  );
begin
  if Code > 127 then Exit(#0);
  if Shifted then Result := Upper[Code]
  else            Result := Lower[Code];
end;

procedure TCDWidgetSet.WLHandleKey(Sender: TWaylandKeyboard;
  Serial, TimeMs, Key, State: LongWord);
var
  WI: TWaylandWindowInfo;
  VK: Word;
  Ch: Char;
begin
  { Stash the serial; see comment in WLHandlePointerButton. }
  FWlLastInputSerial := Serial;
  if WaylandTrace then Trace(' input',
    Format('keyboard key evdev=%d state=%d focus=%p',
      [Key, State, Pointer(FWlKeyboardFocus)]));
  WI := FWlKeyboardFocus;
  if (WI = nil) or (WI.LCLForm = nil) then Exit;

  VK := WLEvdevToVK(Key);
  if VK = 0 then Exit;

  if State = WL_KEYBOARD_KEY_PRESSED then
  begin
    CallbackKeyDown(WI, VK);
    { Our hardcoded US-QWERTY keymap is modifier-agnostic: it returns
      the printable letter even with Ctrl/Alt held, which would land
      'c' / 'v' / 'x' in the edit on a clipboard chord. Skip the
      char delivery when Ctrl or Alt is held -- the chord still
      reaches OnKeyDown via CallbackKeyDown above. }
    if not (FWlCtrlHeld or FWlAltHeld) then
    begin
      Ch := WLEvdevToChar(Key, FWlShiftHeld);
      if Ch <> #0 then
        CallbackKeyChar(WI, 0, Ch);
    end;
  end
  else
    CallbackKeyUp(WI, VK);
end;

procedure TCDWidgetSet.WLHandleKeyMods(Sender: TWaylandKeyboard;
  Serial, ModsDepressed, ModsLatched, ModsLocked, Group: LongWord);
const
  { Standard XKB modifier-mask bit positions: Shift / Lock / Control /
    Mod1..Mod5 are the X11 modifier indices that XKB inherits and
    Wayland forwards verbatim in wl_keyboard.modifiers. }
  XKB_MOD_SHIFT = 1 shl 0;
  XKB_MOD_LOCK  = 1 shl 1;     { caps lock (X11 'Lock' index) }
  XKB_MOD_CTRL  = 1 shl 2;
  { Mod1..Mod5 are virtual indices the keymap re-binds; the names
    below are the bindings used by typical desktop keymaps (US, GB, DE, FR,
    dvorak, ...), but a custom xkb_compatibility section
    could put Num Lock on any of Mod2..Mod5 in principle. }
  XKB_MOD_MOD1  = 1 shl 3;     { almost always Alt }
  XKB_MOD_MOD2  = 1 shl 4;     { almost always Num Lock }
var
  Active: LongWord;
begin
  { Active modifiers = depressed (held physically) | latched (one-shot
    that fires on the next key, e.g. sticky-keys). Locked is the
    sticky toggle state (caps lock / num lock) and lives separately
    so it doesn't appear as ssShift / ssAlt during typing. }
  Active := ModsDepressed or ModsLatched;
  FWlShiftHeld  := (Active and XKB_MOD_SHIFT) <> 0;
  FWlCtrlHeld   := (Active and XKB_MOD_CTRL)  <> 0;
  FWlAltHeld    := (Active and XKB_MOD_MOD1)  <> 0;
  FWlCapsLocked := (ModsLocked and XKB_MOD_LOCK) <> 0;
  FWlNumLocked  := (ModsLocked and XKB_MOD_MOD2) <> 0;
end;

function TCDWidgetSet.GetAppHandle: TLCLHandle;
begin
  Result := TLCLHandle(FWlDisplay);
end;

procedure TCDWidgetSet.BackendCreate;
begin
  if Application.ApplicationType = atDefault then
    Application.ApplicationType := atDesktop;
  if Application.LayoutAdjustmentPolicy = lapDefault then
    Application.LayoutAdjustmentPolicy := lapFixedLayout;

  { Connect to the Wayland compositor }
  FWlDisplay := TWaylandDisplay.Create;
  FWlDisplay.Connection.Connect(WaylandSocketPath);

  FWlRegistry := FWlDisplay.GetRegistry;
  FWlRegistry.OnGlobal := @WLHandleGlobal;

  { Two roundtrips: the first delivers globals, the second ensures
    binds are processed by the compositor. }
  FWlDisplay.Roundtrip;
  FWlDisplay.Roundtrip;

  if FWlCompositor = nil then
    raise Exception.Create('Wayland: compositor does not advertise wl_compositor');
  if FWlShm = nil then
    raise Exception.Create('Wayland: compositor does not advertise wl_shm');
  if FXdgWmBase = nil then
    raise Exception.Create('Wayland: compositor does not advertise xdg_wm_base');

  { Open the per-seat data device for clipboard ops. Both globals are
    already bound by the registry roundtrip above; if either is missing
    the compositor doesn't support the clipboard protocol and we leave
    FWlDataDevice nil (clipboard ops will return False / no-op). }
  if (FWlDataDeviceManager <> nil) and (FWlSeat <> nil) then
    FWlDataDevice := FWlDataDeviceManager.GetDataDevice(FWlSeat);

  FWlWindowList := TFPList.Create;
  FWlTimerList  := TFPList.Create;

  { ScreenDC scratch surface. Wayland gives us no concept of "the screen
    DC", so we use a 1x1 placeholder; per-form drawing happens on the
    form's own canvas. }
  ScreenBitmapWidth := 1;
  ScreenBitmapHeight := 1;
  ScreenBitmapRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(ScreenBitmapWidth, ScreenBitmapHeight);
  ScreenBitmapRawImage.CreateData(True);
  ScreenImage := TLazIntfImage.Create(ScreenBitmapRawImage, True);
  ScreenDC := TLazCanvas.Create(ScreenImage);
  ScreenFormat := clfBGRA32;
end;

procedure TCDWidgetSet.BackendDestroy;
var
  i: Integer;
begin
  if WaylandTrace then Trace('  loop', 'BackendDestroy enter');
  if Assigned(ScreenDC) then ScreenDC.Free;
  if Assigned(ScreenImage) then ScreenImage.Free;
  if Assigned(FWlTimerList) then
  begin
    for i := 0 to FWlTimerList.Count - 1 do
      TWLTimer(FWlTimerList[i]).Free;
    FWlTimerList.Free;
  end;
  if Assigned(FWlWindowList) then FWlWindowList.Free;
  if Assigned(FWlDisplay) then FWlDisplay.Free;
  FWlDisplay := nil;
  FWlCompositor := nil;
  FWlShm := nil;
  FXdgWmBase := nil;
  FXdgActivation := nil;
  FWlRegistry := nil;
end;

procedure TCDWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  ScreenInfo.PixelsPerInchX := 96;
  ScreenInfo.PixelsPerInchY := 96;
  ScreenInfo.ColorDepth     := 32;
  GenericAppInit;
end;

procedure TCDWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  if WaylandTrace then Trace('  loop',
    Format('AppRun enter (Terminated=%d, FTerminating=%d)',
      [Ord(Application.Terminated), Ord(FTerminating)]));
  while not (FTerminating or Application.Terminated) do
  begin
    AppProcessMessages;
    if FTerminating or Application.Terminated then Break;
    AppWaitMessage;
  end;
  if WaylandTrace then Trace('  loop', 'AppRun exit');
end;

procedure TCDWidgetSet.AppProcessMessages;
begin
  if FWlDisplay = nil then Exit;
  if WaylandTrace then Trace('  loop', 'AppProcessMessages enter');
  FWlDisplay.Connection.Flush;
  if WaylandTrace then Trace('  loop', 'AppProcessMessages flushed');
  { DispatchOne(False) returns True iff it pulled a message off the
    in-memory buffer (or the socket, opportunistically). Loop until
    drained -- crucial because a single recvmsg() often returns
    several messages, and waking on poll alone would leave the
    rest buffered until the next compositor write. }
  while (not FTerminating) and FWlDisplay.DispatchOne(False) do
    ;
  if WaylandTrace then Trace('  loop', 'AppProcessMessages -> paint');
  WLPaintAllPending;
  if WaylandTrace then Trace('  loop', 'AppProcessMessages exit');
end;

procedure TCDWidgetSet.AppWaitMessage;
var
  PFds: array of TFDSet;  { unused }
  Fds: array of pollfd;
  i, R: Integer;
  Timer: TWLTimer;
  Buf: QWord;             { timerfd read returns 8 bytes = expiration count }
begin
  if FWlDisplay = nil then Exit;
  if WaylandTrace then Trace('  loop', 'AppWaitMessage enter');
  FWlDisplay.Connection.Flush;

  { Build the poll set: wayland socket first, then every active timer fd. }
  SetLength(Fds, 1 + FWlTimerList.Count);
  Fds[0].fd      := FWlDisplay.Connection.SocketFd;
  Fds[0].events  := POLLIN;
  Fds[0].revents := 0;
  for i := 0 to FWlTimerList.Count - 1 do
  begin
    Fds[1 + i].fd      := TWLTimer(FWlTimerList[i]).Fd;
    Fds[1 + i].events  := POLLIN;
    Fds[1 + i].revents := 0;
  end;

  R := FpPoll(@Fds[0], Length(Fds), -1);  { -1 = block forever }
  if R <= 0 then Exit;

  { Drain any expired timers first; their callbacks may queue Wayland
    requests we'd want flushed before we go back to waiting. }
  for i := 0 to FWlTimerList.Count - 1 do
  begin
    if (Fds[1 + i].revents and POLLIN) = 0 then continue;
    Timer := TWLTimer(FWlTimerList[i]);
    { Read the expiration count to clear the readable state. We
      ignore how many times it expired -- the LCL only wants one
      callback per wakeup. }
    FpRead(Timer.Fd, Buf, SizeOf(Buf));
    if Assigned(Timer.Func) then Timer.Func();
  end;

  { Then drain any compositor events. }
  if (Fds[0].revents and POLLIN) <> 0 then
    FWlDisplay.DispatchOne(True);

  WLPaintAllPending;
end;

procedure TCDWidgetSet.AppTerminate;
begin
  if FTerminating then Exit;
  FTerminating := True;
end;

procedure TCDWidgetSet.AppMinimize;
begin
  { FIXME: stub. Should call xdg_toplevel.set_minimized on each form. }
end;

procedure TCDWidgetSet.AppRestore;
begin
  { xdg-shell has no generic "restore from minimized" request. A client
    can unset maximized / fullscreen state, but de-minimizing and focus
    decisions belong to the compositor. }
end;

procedure TCDWidgetSet.AppBringToFront;
begin
  { TODO: wire xdg_activation_v1 if/when the LCL can provide an
    activation token from recent user input. Arbitrary focus stealing is
    intentionally not available on Wayland. }
end;

procedure TCDWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
  { Wayland has no per-window icon request in xdg-shell. Shells look up
    icons through app_id / desktop-entry metadata instead. }
end;

procedure TCDWidgetSet.AppSetTitle(const ATitle: string);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  if FWlWindowList = nil then Exit;
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
    if (WI <> nil) and Assigned(WI.Toplevel) then
    begin
      WI.Title := ATitle;
      WI.Toplevel.SetTitle(ATitle);
    end;
  end;
end;

procedure TCDWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
  { App-level visibility is not an xdg-shell client setting; per-form
    visibility is represented by creating/destroying or attaching buffers
    for the form's surface. }
end;

function TCDWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  { xdg-shell has no normal-window stay-on-top request. Layer-shell is
    for desktop components such as panels/overlays, not a drop-in
    replacement for application toplevels. }
  Result := False;
end;

function TCDWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  { See AppRemoveStayOnTopFlags. }
  Result := False;
end;

procedure TCDWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin
  { Taskbar visibility is compositor/shell policy, usually keyed from
    app_id / desktop-entry metadata; xdg-shell has no client knob for
    this. }
end;

function TCDWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): TLCLHandle;
var
  Fd: cint;
  Spec: TWLTimerSpec;
  Timer: TWLTimer;
begin
  { Linux timerfd. The fd is added to AppWaitMessage's poll set; when
    the kernel fires the timer the fd becomes readable, AppWaitMessage
    drains 8 bytes from it (the kernel-supplied expiration count) and
    invokes TimerFunc. CLOCK_MONOTONIC=1, CLOCK_REALTIME=0; we want
    monotonic so DST/clock-jumps don't shift LCL timers. }
  Fd := WLTimerfdCreate(1, TFD_CLOEXEC);
  if Fd < 0 then
  begin
    Result := 0;
    Exit;
  end;
  Spec.it_value_sec     :=  Interval div 1000;
  Spec.it_value_nsec    := (Interval mod 1000) * 1000000;
  Spec.it_interval_sec  :=  Spec.it_value_sec;
  Spec.it_interval_nsec :=  Spec.it_value_nsec;
  if WLTimerfdSettime(Fd, 0, Spec, nil) <> 0 then
  begin
    FpClose(Fd);
    Result := 0;
    Exit;
  end;
  Timer := TWLTimer.Create(Fd, Interval, TimerFunc);
  FWlTimerList.Add(Timer);
  Result := TLCLHandle(Timer);
end;

function TCDWidgetSet.DestroyTimer(TimerHandle: TLCLHandle): boolean;
var
  Timer: TWLTimer;
  Idx: Integer;
begin
  Result := False;
  if TimerHandle = 0 then Exit;
  Timer := TWLTimer(TimerHandle);
  Idx := FWlTimerList.IndexOf(Timer);
  if Idx >= 0 then
  begin
    FWlTimerList.Delete(Idx);
    Timer.Free;     { closes fd in destructor }
    Result := True;
  end;
end;

{ Internal helper: walk the form list, allocate/resize each pending
  window's wl_shm buffer, paint the form into it, and commit. }

procedure TCDWidgetSet.WLPaintAllPending;
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  if FWlWindowList = nil then
  begin
    if WaylandTrace then Trace('  paint', 'WLPaintAllPending: window list nil');
    Exit;
  end;
  if WaylandTrace then Trace('  paint',
    Format('WLPaintAllPending enter, count=%d', [FWlWindowList.Count]));
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
    if WaylandTrace then Trace('  paint',
      Format('  window %d: HasConfigure=%d PendingDraw=%d',
        [i, Ord((WI <> nil) and WI.HasConfigure),
            Ord((WI <> nil) and WI.PendingDraw)]));
    if (WI = nil) or (not WI.HasConfigure) or (not WI.PendingDraw) then continue;
    WLDrawWindow(WI);
    WI.PendingDraw := False;
  end;
  if Assigned(FWlDisplay) then FWlDisplay.Connection.Flush;
  if WaylandTrace then Trace('  paint', 'WLPaintAllPending exit');
end;

procedure TCDWidgetSet.WLDrawWindow(WI: TWaylandWindowInfo);
begin
  TCDWSCustomForm.WLPaintForm(WI);
end;

{ Per-window xdg_surface / xdg_toplevel callbacks. These walk
  FWlWindowList to find the per-form WindowInfo whose xdg_surface or
  xdg_toplevel matches Sender, then update its state. }

procedure TCDWidgetSet.WLHandleSurfaceConfigure(Sender: TXdgSurface;
  Serial: LongWord);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  Sender.AckConfigure(Serial);
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
    if (WI <> nil) and (WI.XdgSurf = Sender) then
    begin
      WI.HasConfigure := True;
      WI.PendingDraw := True;
      Exit;
    end;
  end;
end;

procedure TCDWidgetSet.WLHandleToplevelConfigure(Sender: TXdgToplevel;
  W, H: LongInt; const States: array of LongWord);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  if (W <= 0) or (H <= 0) then Exit;
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
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

procedure TCDWidgetSet.WLHandleToplevelClose(Sender: TXdgToplevel);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
    if (WI <> nil) and (WI.Toplevel = Sender) then
    begin
      WI.ShouldClose := True;
      if WI.LCLForm <> nil then
        LCLSendCloseQueryMsg(WI.LCLForm);
      Exit;
    end;
  end;
end;

{ ------------------------------------------------------------------
  xdg_popup support: dropdowns, menus, tooltips. The popup must be
  parented under another mapped xdg_surface; the compositor handles
  click-outside / Escape dismiss via xdg_popup.grab + popup_done, and
  positions the popup relative to an anchor rectangle in parent-
  surface-local coordinates so it never falls off-screen.
  ------------------------------------------------------------------ }

function TCDWidgetSet.WLChoosePopupParent(LCLForm: TCustomForm): TWaylandWindowInfo;
var
  i: Integer;
  Cand: TWaylandWindowInfo;
begin
  Result := nil;
  if FWlWindowList = nil then Exit;

  { 1. Honour LCL's PopupParent if the user set one explicitly. }
  if (LCLForm <> nil) and (LCLForm.PopupParent <> nil)
     and (LCLForm.PopupParent.Handle <> 0) then
  begin
    Cand := TWaylandWindowInfo(LCLForm.PopupParent.Handle);
    if (Cand <> nil) and (Cand.LCLForm <> LCLForm) then
      Exit(Cand);
  end;

  { 2. Pointer focus -- a click-triggered popup almost always wants
       this as its anchor surface. }
  if FWlPointerFocus <> nil then
    Exit(FWlPointerFocus);

  { 3. Keyboard focus -- keyboard-triggered popups (menu key, accel). }
  if FWlKeyboardFocus <> nil then
    Exit(FWlKeyboardFocus);

  { 4. Last resort: first non-popup, non-self window in the list. }
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    Cand := TWaylandWindowInfo(FWlWindowList[i]);
    if (Cand = nil) or (Cand.LCLForm = LCLForm) then Continue;
    if Cand.Toplevel <> nil then Exit(Cand);
  end;
end;

function TCDWidgetSet.WLComputePopupAnchorRect(LCLForm: TCustomForm;
  ParentWI: TWaylandWindowInfo; out R: TRect): Boolean;
begin
  { TCDPopUpMenuForm's Left/Top are set by the LCL to absolute "screen"
    coordinates derived from a TControl.ClientToScreen call. Customdrawn
    on Wayland has no global screen origin -- TCDWSCustomForm doesn't
    receive a position from the compositor (Wayland refuses to expose
    surface positions to clients). Empirically the LCL's "screen"
    coordinates inside a single-toplevel app collapse to parent-surface-
    local for the active toplevel, so subtracting the parent form's
    (Left, Top) gives the right anchor for the popup. The anchor is a
    1x1 rect at the desired anchor point; the BOTTOM_LEFT anchor
    constant + BOTTOM_RIGHT gravity then makes the popup grow from
    that point downward and to the right. }
  Result := False;
  if (LCLForm = nil) or (ParentWI = nil) or (ParentWI.LCLForm = nil) then Exit;
  R.Left   := LCLForm.Left - ParentWI.LCLForm.Left;
  R.Top    := LCLForm.Top  - ParentWI.LCLForm.Top;
  R.Right  := R.Left + 1;
  R.Bottom := R.Top  + 1;
  Result := True;
end;

procedure TCDWidgetSet.WLCreatePopupForWindow(WI, ParentWI: TWaylandWindowInfo;
  const Anchor: TRect);
var
  Pos: TXdgPositioner;
begin
  if (WI = nil) or (ParentWI = nil) or (FXdgWmBase = nil) then Exit;
  Pos := FXdgWmBase.CreatePositioner;
  Pos.SetSize(Max(1, WI.LCLForm.Width), Max(1, WI.LCLForm.Height));
  Pos.SetAnchorRect(Anchor.Left, Anchor.Top,
                    Max(1, Anchor.Right - Anchor.Left),
                    Max(1, Anchor.Bottom - Anchor.Top));
  Pos.SetAnchor (XDG_POSITIONER_ANCHOR_BOTTOM_LEFT);
  Pos.SetGravity(XDG_POSITIONER_GRAVITY_BOTTOM_RIGHT);
  Pos.SetConstraintAdjustment(
    XDG_POSITIONER_CONSTRAINT_FLIP_Y or
    XDG_POSITIONER_CONSTRAINT_SLIDE_X);

  WI.Popup := WI.XdgSurf.GetPopup(ParentWI.XdgSurf, Pos);
  WI.Popup.OnConfigure := @WLHandlePopupConfigure;
  WI.Popup.OnDone      := @WLHandlePopupDone;

  { Grab MUST be issued before the surface is committed (xdg-shell spec).
    The serial is whichever input event triggered the popup; for a
    combobox click that's the BTN_LEFT release recorded by
    WLHandlePointerButton. Without a recent input serial the popup
    still works -- it just won't auto-dismiss on click-outside, which
    is the right fallback for programmatic / timer-driven popups. }
  if (FWlSeat <> nil) and (FWlLastInputSerial <> 0) then
    WI.Popup.Grab(FWlSeat, FWlLastInputSerial);

  { Compositor took its own snapshot of the positioner state at
    GetPopup; we don't need our handle anymore. }
  Pos.DestroyRequest;
end;

procedure TCDWidgetSet.WLHandlePopupConfigure(Sender: TXdgPopup;
  X, Y, W, H: LongInt);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  if FWlWindowList = nil then Exit;
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
    if (WI <> nil) and (WI.Popup = Sender) then
    begin
      { If the compositor chose a different size (shouldn't happen
        without resize_x|y in our constraint mask, but defensive),
        push it to the LCL form so the layout follows. }
      if (W > 0) and (H > 0) and (WI.LCLForm <> nil)
         and ((WI.LCLForm.Width <> W) or (WI.LCLForm.Height <> H)) then
        WI.LCLForm.SetBounds(WI.LCLForm.Left, WI.LCLForm.Top, W, H);
      WI.PendingDraw := True;
      Exit;
    end;
  end;
end;

procedure TCDWidgetSet.WLHandlePopupDone(Sender: TXdgPopup);
var
  i: Integer;
  WI: TWaylandWindowInfo;
begin
  if FWlWindowList = nil then Exit;
  for i := 0 to FWlWindowList.Count - 1 do
  begin
    WI := TWaylandWindowInfo(FWlWindowList[i]);
    if (WI <> nil) and (WI.Popup = Sender) then
    begin
      WI.ShouldClose := True;
      { Hide rather than LCLSendCloseQueryMsg: CloseQuery only asks "may
        I close?", it doesn't actually act. Hide flips Visible to False
        which routes through ShowHide(visible=False) into
        WLTeardownPopup -- the same path Self.Close from
        HandleItemClick uses. }
      if WI.LCLForm <> nil then WI.LCLForm.Hide;
      Exit;
    end;
  end;
end;
