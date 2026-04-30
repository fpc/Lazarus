{%MainUnit customdrawnint.pas}

{ Wayland-specific TCDWidgetSet lclintf-shim methods. Minimal stubs. }

function TCDWidgetSet.AskUser(const DialogCaption, DialogMessage: string;
  DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
  { Defer to TWidgetSet.AskUser, which routes through
    QuestionDialogFunction (set to DefaultQuestionDialog by Dialogs'
    init). DefaultQuestionDialog builds an LCL modal form, so it works
    on customdrawn-Wayland once ShowModal and modal input gating work. }
  Result := inherited AskUser(DialogCaption, DialogMessage, DialogType,
    Buttons, HelpCtx);
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
  { Defer to TWidgetSet.PromptUser, which routes through
    PromptDialogFunction (set to DefaultPromptDialog by Dialogs'
    init). DefaultPromptDialog builds a TPromptDialog (a TForm
    subclass) and calls ShowModal -- which on customdrawn-Wayland
    sets the xdg_toplevel parent hint and the LCL's modal pump
    drives the message loop. The earlier override here was an
    EscapeResult-only stub that bypassed the LCL fallback entirely;
    keeping it made MessageDlg / ShowMessage etc. return the cancel
    result without ever showing anything. }
  Result := inherited PromptUser(DialogCaption, DialogMessage, DialogType,
    Buttons, ButtonCount, DefaultIndex, EscapeResult);
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
