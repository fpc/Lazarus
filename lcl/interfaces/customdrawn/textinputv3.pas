unit textinputv3;

{ Bindings for zwp_text_input_manager_v3 / zwp_text_input_v3 (interface
  version 1). This is the v3 text-input protocol available in common
  ibus/fcitx5 setups on KDE, Sway and hyprland. Lets the client
  tell the compositor "this surface has an active text-entry, deliver
  IME pre-edit and committed text here, show the OSK if you have one".

  Compositors may omit this protocol. GNOME (mutter) is a known case:
  it does not advertise zwp_text_input_manager_v3 and uses its own GTK
  IM bridge instead. When the manager global is absent, this object
  stays nil and the client falls back to the WLHandleKey US-QWERTY path.

  Protocol semantics that bite:

  - All requests are double-buffered. enable / disable / every set_*
    only takes effect on the next commit. Counter-intuitive but
    deliberate (the compositor sees a consistent state snapshot).

  - committed `enable` resets ALL state -- preedit, surrounding text,
    content type, cursor rectangle. Caller must re-set whatever it
    cares about after each enable.

  - The `done` event applies state in this strict order: clear old
    preedit, run delete_surrounding_text, insert commit_string,
    insert new preedit_string, place cursor inside preedit. Callers
    that need to follow IME semantics correctly must apply pending
    state per `done` rather than per individual event.

  - `done(serial)` carries the count of commit requests as the
    compositor saw them. If our local FCommitSerial is ahead, the
    `done` was based on an older snapshot; the spec says to apply
    the events anyway but re-send pending set_* state on the next
    commit. We expose FCommitSerial to let the host compare. }

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ctypes, waylandwire, waylandcore;

type
  TZwpTextInputManagerV3 = class;
  TZwpTextInputV3        = class;

  { text-input.enter / leave -- the seat's text-input focus moved to
    or away from a surface. Compositor sends leave before enter when
    focus changes. After leave, all state is invalidated and the
    client must not send requests until the next enter. }
  TTextInputEnterProc      = procedure(Sender: TZwpTextInputV3;
                                       Surface: TWaylandSurface) of object;
  TTextInputLeaveProc      = procedure(Sender: TZwpTextInputV3;
                                       Surface: TWaylandSurface) of object;

  { preedit_string -- IME's current composition; lives between the
    keystrokes and the commit. cursor_begin / cursor_end are byte
    offsets within Text; if both are -1 the cursor should be hidden.
    Buffered, applied on `done`. Empty Text means "clear preedit". }
  TTextInputPreeditProc    = procedure(Sender: TZwpTextInputV3;
                                       const Text: AnsiString;
                                       CursorBegin, CursorEnd: LongInt) of object;

  { commit_string -- text the IME wants inserted at the caret.
    Buffered, applied on `done` after the preedit clear and the
    delete_surrounding_text but before the new preedit. }
  TTextInputCommitProc     = procedure(Sender: TZwpTextInputV3;
                                       const Text: AnsiString) of object;

  { delete_surrounding_text -- IME wants N bytes before and M bytes
    after the caret removed. Both counts are uint; if a preedit is
    active the spec counts before_length from the preedit's start. }
  TTextInputDeleteProc     = procedure(Sender: TZwpTextInputV3;
                                       BeforeLength, AfterLength: LongWord) of object;

  { done(serial) -- apply the pending preedit / commit / delete state
    atomically. Serial is the count of commit requests as the
    compositor saw them. }
  TTextInputDoneProc       = procedure(Sender: TZwpTextInputV3;
                                       Serial: LongWord) of object;

  { content_hint bitfield (request 5 -- set_content_type). }
  TZwpTextInputV3ContentHint = set of (
    tihCompletion,
    tihSpellcheck,
    tihAutoCapitalization,
    tihLowercase,
    tihUppercase,
    tihTitlecase,
    tihHiddenText,
    tihSensitiveData,
    tihLatin,
    tihMultiline);

  { content_purpose enum (request 5 -- set_content_type). }
  TZwpTextInputV3ContentPurpose = (
    tipNormal     = 0,
    tipAlpha      = 1,
    tipDigits     = 2,
    tipNumber     = 3,
    tipPhone      = 4,
    tipUrl        = 5,
    tipEmail      = 6,
    tipName       = 7,
    tipPassword   = 8,
    tipPin        = 9,
    tipDate       = 10,
    tipTime       = 11,
    tipDateTime   = 12,
    tipTerminal   = 13);

  { change_cause enum (request 4 -- set_text_change_cause). }
  TZwpTextInputV3ChangeCause = (
    ticInputMethod = 0,
    ticOther       = 1);

  TZwpTextInputManagerV3 = class(TWaylandObject)
  public
    { request 0: destroy. }
    procedure DestroyRequest;
    { request 1: get_text_input(new_id, seat). One per seat. }
    function GetTextInput(Seat: TWaylandSeat): TZwpTextInputV3;
  end;

  TZwpTextInputV3 = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnEnter:                  TTextInputEnterProc;
    OnLeave:                  TTextInputLeaveProc;
    OnPreeditString:          TTextInputPreeditProc;
    OnCommitString:           TTextInputCommitProc;
    OnDeleteSurroundingText:  TTextInputDeleteProc;
    OnDone:                   TTextInputDoneProc;
    { Count of commit() requests we've sent. Compared against the
      serial in `done` events to detect races (compositor responding
      to an older state than we've already moved past). }
    FCommitSerial: LongWord;

    { request 0: destroy. Implicitly disables all text-input on the
      surface. }
    procedure DestroyRequest;

    { request 1: enable. Pending state, applied on commit. Resets
      all stored state on apply. }
    procedure Enable;

    { request 2: disable. Pending state, applied on commit. }
    procedure Disable;

    { request 3: set_surrounding_text(text, cursor, anchor). Cursor
      and anchor are byte offsets within Text. If no selection,
      cursor = anchor. Empty Text means "field doesn't support
      surrounding-text reporting"; sending it once is sticky. }
    procedure SetSurroundingText(const Text: AnsiString;
                                 Cursor, Anchor: LongInt);

    { request 4: set_text_change_cause(cause). Tells the IME
      whether the change came from itself (input_method) or from
      something else (other = user typed, app code, etc). }
    procedure SetTextChangeCause(Cause: TZwpTextInputV3ChangeCause);

    { request 5: set_content_type(hint, purpose). Lets the IME
      pick an appropriate input panel (numeric keypad for tipNumber,
      hide caps lock for tipPassword, etc). }
    procedure SetContentType(Hint: TZwpTextInputV3ContentHint;
                             Purpose: TZwpTextInputV3ContentPurpose);

    { request 6: set_cursor_rectangle(x, y, w, h). Rectangle in
      surface-local coords; the compositor uses it to position the
      candidate window so it doesn't cover the typed text. }
    procedure SetCursorRectangle(X, Y, Width, Height: LongInt);

    { request 7: commit. Applies all pending state. Increments
      FCommitSerial. }
    procedure Commit;
  end;

implementation

{ TZwpTextInputManagerV3 }

procedure TZwpTextInputManagerV3.DestroyRequest;
begin
  SendRequest(0);  { destroy }
end;

function TZwpTextInputManagerV3.GetTextInput(Seat: TWaylandSeat): TZwpTextInputV3;
var
  W: TWlWriter;
  NewIdValue: TWlObjectId;
begin
  NewIdValue := FDisplay.NewId;
  Result := TZwpTextInputV3.Create(FDisplay, NewIdValue);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(NewIdValue);
  W.WriteObject(Seat.Id);
  SendRequest(1, W);  { get_text_input }
end;

{ TZwpTextInputV3 }

procedure TZwpTextInputV3.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  SurfId: TWlObjectId;
  Surface: TWaylandSurface;
  Obj: TWaylandObject;
  Text: AnsiString;
  CursorBegin, CursorEnd: LongInt;
  BeforeLen, AfterLen, Serial: LongWord;
begin
  case Opcode of
    0:  { enter(object surface) }
      begin
        SurfId := Reader.ReadObject;
        Obj := FDisplay.Find(SurfId);
        if Obj is TWaylandSurface then Surface := TWaylandSurface(Obj)
        else Surface := nil;
        if Assigned(OnEnter) then OnEnter(Self, Surface);
      end;
    1:  { leave(object surface) }
      begin
        SurfId := Reader.ReadObject;
        Obj := FDisplay.Find(SurfId);
        if Obj is TWaylandSurface then Surface := TWaylandSurface(Obj)
        else Surface := nil;
        if Assigned(OnLeave) then OnLeave(Self, Surface);
      end;
    2:  { preedit_string(string?, int cursor_begin, int cursor_end) }
      begin
        Text := Reader.ReadString;
        CursorBegin := Reader.ReadInt;
        CursorEnd := Reader.ReadInt;
        if Assigned(OnPreeditString) then
          OnPreeditString(Self, Text, CursorBegin, CursorEnd);
      end;
    3:  { commit_string(string?) }
      begin
        Text := Reader.ReadString;
        if Assigned(OnCommitString) then OnCommitString(Self, Text);
      end;
    4:  { delete_surrounding_text(uint before_length, uint after_length) }
      begin
        BeforeLen := Reader.ReadUInt;
        AfterLen := Reader.ReadUInt;
        if Assigned(OnDeleteSurroundingText) then
          OnDeleteSurroundingText(Self, BeforeLen, AfterLen);
      end;
    5:  { done(uint serial) }
      begin
        Serial := Reader.ReadUInt;
        if Assigned(OnDone) then OnDone(Self, Serial);
      end;
  end;
end;

procedure TZwpTextInputV3.DestroyRequest;
begin
  SendRequest(0);  { destroy }
end;

procedure TZwpTextInputV3.Enable;
begin
  SendRequest(1);  { enable }
end;

procedure TZwpTextInputV3.Disable;
begin
  SendRequest(2);  { disable }
end;

procedure TZwpTextInputV3.SetSurroundingText(const Text: AnsiString;
  Cursor, Anchor: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteString(Text);
  W.WriteInt(Cursor);
  W.WriteInt(Anchor);
  SendRequest(3, W);  { set_surrounding_text }
end;

procedure TZwpTextInputV3.SetTextChangeCause(Cause: TZwpTextInputV3ChangeCause);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Ord(Cause));
  SendRequest(4, W);  { set_text_change_cause }
end;

procedure TZwpTextInputV3.SetContentType(Hint: TZwpTextInputV3ContentHint;
  Purpose: TZwpTextInputV3ContentPurpose);
var
  W: TWlWriter;
  HintBits: LongWord;
begin
  HintBits := 0;
  if tihCompletion         in Hint then HintBits := HintBits or $001;
  if tihSpellcheck         in Hint then HintBits := HintBits or $002;
  if tihAutoCapitalization in Hint then HintBits := HintBits or $004;
  if tihLowercase          in Hint then HintBits := HintBits or $008;
  if tihUppercase          in Hint then HintBits := HintBits or $010;
  if tihTitlecase          in Hint then HintBits := HintBits or $020;
  if tihHiddenText         in Hint then HintBits := HintBits or $040;
  if tihSensitiveData      in Hint then HintBits := HintBits or $080;
  if tihLatin              in Hint then HintBits := HintBits or $100;
  if tihMultiline          in Hint then HintBits := HintBits or $200;
  W.Reset;
  W.WriteUInt(HintBits);
  W.WriteUInt(Ord(Purpose));
  SendRequest(5, W);  { set_content_type }
end;

procedure TZwpTextInputV3.SetCursorRectangle(X, Y, Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X);
  W.WriteInt(Y);
  W.WriteInt(Width);
  W.WriteInt(Height);
  SendRequest(6, W);  { set_cursor_rectangle }
end;

procedure TZwpTextInputV3.Commit;
begin
  Inc(FCommitSerial);
  SendRequest(7);  { commit }
end;

end.
