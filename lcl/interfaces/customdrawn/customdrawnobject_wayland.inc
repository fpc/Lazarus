{%MainUnit customdrawnint.pas}

{ All TCDWidgetSet methods specific to the Wayland backend.

  Modelled on the Cocoa backend (smallest existing host). The Wayland
  protocol grunt-work lives in waylandwire / waylandcore / xdgshell. }

{ TCDWidgetSet }

procedure WLHandleGlobal(Name: LongWord; const Iface: AnsiString; Version: LongWord);
var
  V: LongWord;
begin
  if Iface = 'wl_compositor' then
  begin
    if Version > 4 then V := 4 else V := Version;
    CDWidgetset.FWlCompositor := TWaylandCompositor(
      CDWidgetset.FWlRegistry.BindRaw(Name, Iface, V, TWaylandCompositor));
  end
  else if Iface = 'wl_shm' then
  begin
    CDWidgetset.FWlShm := TWaylandShm(
      CDWidgetset.FWlRegistry.BindRaw(Name, Iface, 1, TWaylandShm));
  end
  else if Iface = 'xdg_wm_base' then
  begin
    if Version > 5 then V := 5 else V := Version;
    CDWidgetset.FXdgWmBase := TXdgWmBase(
      CDWidgetset.FWlRegistry.BindRaw(Name, Iface, V, TXdgWmBase));
    CDWidgetset.FXdgWmBase.OnPing := @WLHandlePing;
  end
  else if Iface = 'xdg_activation_v1' then
  begin
    CDWidgetset.FXdgActivation := TXdgActivationV1(
      CDWidgetset.FWlRegistry.BindRaw(Name, Iface, 1, TXdgActivationV1));
  end
  else if Iface = 'wl_seat' then
  begin
    { v1 is enough for non-frame'd pointer + key + modifiers events.
      v5+ adds frame/axis_source/etc. that we currently ignore. }
    CDWidgetset.FWlSeat := TWaylandSeat(
      CDWidgetset.FWlRegistry.BindRaw(Name, Iface, 1, TWaylandSeat));
    CDWidgetset.FWlSeat.OnCapabilities := @WLHandleSeatCaps;
  end
  else if Iface = 'wp_cursor_shape_manager_v1' then
  begin
    CDWidgetset.FWpCursorShapeMgr := TWpCursorShapeManagerV1(
      CDWidgetset.FWlRegistry.BindRaw(Name, Iface, 1, TWpCursorShapeManagerV1));
  end;
end;

procedure WLHandlePing(Serial: LongWord);
begin
  if Assigned(CDWidgetset.FXdgWmBase) then
    CDWidgetset.FXdgWmBase.Pong(Serial);
end;

procedure WLHandleSeatCaps(Sender: TWaylandSeat; Caps: LongWord);
begin
  if ((Caps and WL_SEAT_CAPABILITY_POINTER) <> 0)
     and (CDWidgetset.FWlPointer = nil) then
  begin
    CDWidgetset.FWlPointer := Sender.GetPointer;
    CDWidgetset.FWlPointer.OnEnter  := @WLHandlePointerEnter;
    CDWidgetset.FWlPointer.OnLeave  := @WLHandlePointerLeave;
    CDWidgetset.FWlPointer.OnMotion := @WLHandlePointerMotion;
    CDWidgetset.FWlPointer.OnButton := @WLHandlePointerButton;
    CDWidgetset.FWlPointer.OnAxis   := @WLHandlePointerAxis;
    if (CDWidgetset.FWpCursorShapeMgr <> nil)
       and (CDWidgetset.FWpCursorShapeDev = nil) then
      CDWidgetset.FWpCursorShapeDev :=
        CDWidgetset.FWpCursorShapeMgr.GetPointer(CDWidgetset.FWlPointer);
  end;
  if ((Caps and WL_SEAT_CAPABILITY_KEYBOARD) <> 0)
     and (CDWidgetset.FWlKeyboard = nil) then
  begin
    CDWidgetset.FWlKeyboard := Sender.GetKeyboard;
    CDWidgetset.FWlKeyboard.OnKeymap    := @WLHandleKeymap;
    CDWidgetset.FWlKeyboard.OnEnter     := @WLHandleKeyboardEnter;
    CDWidgetset.FWlKeyboard.OnLeave     := @WLHandleKeyboardLeave;
    CDWidgetset.FWlKeyboard.OnKey       := @WLHandleKey;
    CDWidgetset.FWlKeyboard.OnModifiers := @WLHandleKeyMods;
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

function WLCurrentShiftState: TShiftState;
begin
  Result := [];
  if CDWidgetset.FWlShiftHeld then Include(Result, ssShift);
  if CDWidgetset.FWlCtrlHeld  then Include(Result, ssCtrl);
  if CDWidgetset.FWlAltHeld   then Include(Result, ssAlt);
end;

procedure WLHandlePointerEnter(Sender: TWaylandPointer; Serial: LongWord;
  Surface: TWaylandSurface; X, Y: LongInt);
begin
  if WaylandTrace then Trace(' input',
    Format('pointer enter surface=%p at (%d,%d)', [Pointer(Surface), X, Y]));
  { Wayland requires the client to set the cursor on every pointer enter
    event; without it the cursor shape stays at whatever the previous
    surface owner left it on (manifests as wrong/sticky cursor inside
    our window). Use wp_cursor_shape_v1 if the compositor offers it
    (server provides a themed cursor) -- nothing to fall back to without
    a wl_shm cursor bitmap, and we don't ship one. }
  if CDWidgetset.FWpCursorShapeDev <> nil then
    CDWidgetset.FWpCursorShapeDev.SetShape(Serial, WP_CURSOR_SHAPE_DEFAULT);
  CDWidgetset.FWlPointerFocus := CDWidgetset.WLFindWindowBySurface(Surface);
  CDWidgetset.FWlPointerX := X;
  CDWidgetset.FWlPointerY := Y;
  if CDWidgetset.FWlPointerFocus <> nil then
    CallbackMouseMove(CDWidgetset.FWlPointerFocus, X, Y, WLCurrentShiftState);
end;

procedure WLHandlePointerLeave(Sender: TWaylandPointer; Serial: LongWord;
  Surface: TWaylandSurface);
begin
  CDWidgetset.FWlPointerFocus := nil;
end;

procedure WLHandlePointerMotion(Sender: TWaylandPointer; TimeMs: LongWord;
  X, Y: LongInt);
var
  WI: TWaylandWindowInfo;
begin
  CDWidgetset.FWlPointerX := X;
  CDWidgetset.FWlPointerY := Y;
  WI := CDWidgetset.FWlPointerFocus;
  if WI <> nil then
    CallbackMouseMove(WI, X, Y, WLCurrentShiftState);
end;

procedure WLHandlePointerButton(Sender: TWaylandPointer;
  Serial, TimeMs, Button, State: LongWord);
var
  WI: TWaylandWindowInfo;
  Btn: TMouseButton;
  Known: Boolean;
begin
  if WaylandTrace then Trace(' input',
    Format('pointer button=$%x state=%d at (%d,%d) focus=%p',
      [Button, State, CDWidgetset.FWlPointerX, CDWidgetset.FWlPointerY,
       Pointer(CDWidgetset.FWlPointerFocus)]));
  WI := CDWidgetset.FWlPointerFocus;
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
    CallbackMouseDown(WI, CDWidgetset.FWlPointerX,
                      CDWidgetset.FWlPointerY, Btn, WLCurrentShiftState)
  else
    CallbackMouseUp(WI, CDWidgetset.FWlPointerX,
                    CDWidgetset.FWlPointerY, Btn, WLCurrentShiftState);
end;

procedure WLHandlePointerAxis(Sender: TWaylandPointer;
  TimeMs, Axis: LongWord; Value: TWlFixed);
var
  WI: TWaylandWindowInfo;
  Delta: SmallInt;
begin
  WI := CDWidgetset.FWlPointerFocus;
  if (WI = nil) or (WI.LCLForm = nil) then Exit;
  if Axis <> WL_POINTER_AXIS_VERTICAL_SCROLL then Exit;  { FIXME: ignore horizontal }
  { Wayland axis value is wl_fixed in surface units (roughly pixel rows
    scrolled). Wayland convention: positive = scroll down; LCL/Win
    convention: positive = scroll up; hence the negation. We treat
    each axis event as one notch -- LCL's WHEEL_DELTA convention. }
  if Value > 0 then Delta := -120
  else if Value < 0 then Delta := 120
  else Exit;
  LCLSendMouseWheelMsg(WI.LCLForm, CDWidgetset.FWlPointerX,
                       CDWidgetset.FWlPointerY, Delta, WLCurrentShiftState);
end;

procedure WLHandleKeymap(Sender: TWaylandKeyboard; Format: LongWord;
  Fd: cint; Size: LongWord);
begin
  { FIXME: We close the fd without using the keymap. A real
    implementation would mmap it (Format=1 means xkb_v1) and feed
    the bytes to xkbcommon for layout-correct character translation
    and modifier state. For now we use a hardcoded US-QWERTY map. }
  if Fd >= 0 then FpClose(Fd);
end;

procedure WLHandleKeyboardEnter(Sender: TWaylandKeyboard; Serial: LongWord;
  Surface: TWaylandSurface);
begin
  CDWidgetset.FWlKeyboardFocus := CDWidgetset.WLFindWindowBySurface(Surface);
  if WaylandTrace then Trace(' input',
    Format('keyboard ENTER surface=%p focus=%p',
      [Pointer(Surface), Pointer(CDWidgetset.FWlKeyboardFocus)]));
end;

procedure WLHandleKeyboardLeave(Sender: TWaylandKeyboard; Serial: LongWord;
  Surface: TWaylandSurface);
begin
  CDWidgetset.FWlKeyboardFocus := nil;
  { Drop any held modifier state so we don't leak into the next focus. }
  CDWidgetset.FWlShiftHeld := False;
  CDWidgetset.FWlCtrlHeld  := False;
  CDWidgetset.FWlAltHeld   := False;
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

procedure WLHandleKey(Sender: TWaylandKeyboard;
  Serial, TimeMs, Key, State: LongWord);
var
  WI: TWaylandWindowInfo;
  VK: Word;
  Ch: Char;
begin
  if WaylandTrace then Trace(' input',
    Format('keyboard key evdev=%d state=%d focus=%p',
      [Key, State, Pointer(CDWidgetset.FWlKeyboardFocus)]));
  WI := CDWidgetset.FWlKeyboardFocus;
  if (WI = nil) or (WI.LCLForm = nil) then Exit;

  { Track modifier keys ourselves -- easier than carrying every
    wl_keyboard.modifiers event's xkb mask through to the LCL. }
  case Key of
    42, 54: CDWidgetset.FWlShiftHeld := (State = WL_KEYBOARD_KEY_PRESSED);
    29, 97: CDWidgetset.FWlCtrlHeld  := (State = WL_KEYBOARD_KEY_PRESSED);
    56,100: CDWidgetset.FWlAltHeld   := (State = WL_KEYBOARD_KEY_PRESSED);
  end;

  VK := WLEvdevToVK(Key);
  if VK = 0 then Exit;

  if State = WL_KEYBOARD_KEY_PRESSED then
  begin
    CallbackKeyDown(WI, VK);
    Ch := WLEvdevToChar(Key, CDWidgetset.FWlShiftHeld);
    if Ch <> #0 then
      CallbackKeyChar(WI, 0, Ch);
  end
  else
    CallbackKeyUp(WI, VK);
end;

procedure WLHandleKeyMods(Sender: TWaylandKeyboard;
  Serial, ModsDepressed, ModsLatched, ModsLocked, Group: LongWord);
begin
  { FIXME: we already track modifiers from key events for correctness
    without xkb. The compositor's own xkb state in ModsDepressed is
    ignored here; once we wire xkbcommon in, it should be the
    authoritative source. }
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
