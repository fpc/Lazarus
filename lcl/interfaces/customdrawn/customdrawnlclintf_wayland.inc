{%MainUnit customdrawnint.pas}

{ Wayland-specific TCDWidgetSet lclintf-shim methods. Minimal stubs. }

function TCDWidgetSet.AskUser(const DialogCaption, DialogMessage: string;
  DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
  { FIXME: no native dialog -- logs to stderr and returns -1. Should
    pop up a CD-drawn modal toplevel with the configured buttons. }
  DebugLn('[WL AskUser] %s: %s', [DialogCaption, DialogMessage]);
  Result := -1;
end;

procedure TCDWidgetSet.HideVirtualKeyboard;
begin
  { FIXME: no virtual keyboard. Wayland has zwp_input_method_v2 / IME;
    the user's compositor does advertise zwp_input_method_manager_v2. }
end;

procedure TCDWidgetSet.ShowVirtualKeyboard;
begin
  { FIXME: see HideVirtualKeyboard. }
end;

function TCDWidgetSet.IsMobilePlatform: Boolean;
begin
  Result := False;
end;

function TCDWidgetSet.PromptUser(const DialogCaption: string;
  const DialogMessage: string; DialogType: LongInt; Buttons: PLongInt;
  ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt): LongInt;
begin
  { FIXME: see AskUser -- same story. Returns the escape result so the
    caller's "user cancelled" path runs. }
  DebugLn('[WL PromptUser] %s: %s', [DialogCaption, DialogMessage]);
  Result := EscapeResult;
end;

function TCDWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC;
  const ARect: TRect): Boolean;
begin
  { TODO: not implemented. Used by screen-grabbing code paths; on
    Wayland the equivalent is zwlr_screencopy_manager_v1 (advertised
    by the user's compositor) -- wire that in if/when this matters. }
  FillChar(ARawImage, SizeOf(ARawImage), 0);
  Result := False;
end;
