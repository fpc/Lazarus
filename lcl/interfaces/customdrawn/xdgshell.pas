unit xdgshell;

{ Bindings for the stable xdg-shell protocol: xdg_wm_base,
  xdg_surface, xdg_toplevel, xdg_positioner, xdg_popup. Versions 1..5. }

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ctypes, waylandwire, waylandcore;

const
  { xdg_positioner.anchor }
  XDG_POSITIONER_ANCHOR_NONE         = 0;
  XDG_POSITIONER_ANCHOR_TOP          = 1;
  XDG_POSITIONER_ANCHOR_BOTTOM       = 2;
  XDG_POSITIONER_ANCHOR_LEFT         = 3;
  XDG_POSITIONER_ANCHOR_RIGHT        = 4;
  XDG_POSITIONER_ANCHOR_TOP_LEFT     = 5;
  XDG_POSITIONER_ANCHOR_BOTTOM_LEFT  = 6;
  XDG_POSITIONER_ANCHOR_TOP_RIGHT    = 7;
  XDG_POSITIONER_ANCHOR_BOTTOM_RIGHT = 8;

  { xdg_positioner.gravity (mirror of anchor's enum) }
  XDG_POSITIONER_GRAVITY_NONE         = 0;
  XDG_POSITIONER_GRAVITY_TOP          = 1;
  XDG_POSITIONER_GRAVITY_BOTTOM       = 2;
  XDG_POSITIONER_GRAVITY_LEFT         = 3;
  XDG_POSITIONER_GRAVITY_RIGHT        = 4;
  XDG_POSITIONER_GRAVITY_TOP_LEFT     = 5;
  XDG_POSITIONER_GRAVITY_BOTTOM_LEFT  = 6;
  XDG_POSITIONER_GRAVITY_TOP_RIGHT    = 7;
  XDG_POSITIONER_GRAVITY_BOTTOM_RIGHT = 8;

  { xdg_positioner.constraint_adjustment (bitfield) }
  XDG_POSITIONER_CONSTRAINT_NONE     = 0;
  XDG_POSITIONER_CONSTRAINT_SLIDE_X  = 1;
  XDG_POSITIONER_CONSTRAINT_SLIDE_Y  = 2;
  XDG_POSITIONER_CONSTRAINT_FLIP_X   = 4;
  XDG_POSITIONER_CONSTRAINT_FLIP_Y   = 8;
  XDG_POSITIONER_CONSTRAINT_RESIZE_X = 16;
  XDG_POSITIONER_CONSTRAINT_RESIZE_Y = 32;

type
  TXdgWmBase     = class;
  TXdgSurface    = class;
  TXdgToplevel   = class;
  TXdgPositioner = class;
  TXdgPopup      = class;

  TXdgPingProc                = procedure(Serial: LongWord) of object;
  TXdgConfigureProc           = procedure(Sender: TXdgSurface; Serial: LongWord) of object;
  TXdgToplevelConfigureProc   = procedure(Sender: TXdgToplevel;
                                          Width, Height: LongInt;
                                          const States: array of LongWord) of object;
  TXdgToplevelCloseProc       = procedure(Sender: TXdgToplevel) of object;
  TXdgPopupConfigureProc      = procedure(Sender: TXdgPopup;
                                          X, Y, Width, Height: LongInt) of object;
  TXdgPopupDoneProc           = procedure(Sender: TXdgPopup) of object;

  TXdgWmBase = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnPing: TXdgPingProc;
    function GetXdgSurface(Surface: TWaylandSurface): TXdgSurface;
    function CreatePositioner: TXdgPositioner;
    procedure Pong(Serial: LongWord);
    procedure DestroyRequest;
  end;

  TXdgSurface = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnConfigure: TXdgConfigureProc;
    function GetToplevel: TXdgToplevel;
    function GetPopup(Parent: TXdgSurface; Positioner: TXdgPositioner): TXdgPopup;
    procedure SetWindowGeometry(X, Y, Width, Height: LongInt);
    procedure AckConfigure(Serial: LongWord);
    procedure DestroyRequest;
  end;

  TXdgToplevel = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnConfigure: TXdgToplevelConfigureProc;
    OnClose:     TXdgToplevelCloseProc;
    procedure SetParent(Parent: TXdgToplevel);
    procedure SetTitle(const ATitle: AnsiString);
    procedure SetAppId(const AAppId: AnsiString);
    procedure SetMinSize(Width, Height: LongInt);
    procedure SetMaxSize(Width, Height: LongInt);
    procedure SetMaximized;
    procedure UnsetMaximized;
    procedure SetMinimized;
    procedure DestroyRequest;
  end;

  { TXdgPositioner -- carries the layout rules (anchor rect, gravity,
    constraint-adjustment) for an xdg_popup. The compositor takes a
    snapshot at xdg_surface.get_popup time, so a positioner can be
    safely destroyed right after that. }
  TXdgPositioner = class(TWaylandObject)
  public
    procedure SetSize(Width, Height: LongInt);
    procedure SetAnchorRect(X, Y, Width, Height: LongInt);
    procedure SetAnchor(Anchor: LongWord);
    procedure SetGravity(Gravity: LongWord);
    procedure SetConstraintAdjustment(Bits: LongWord);
    procedure SetOffset(X, Y: LongInt);
    procedure DestroyRequest;
  end;

  { TXdgPopup -- a transient surface (dropdown, menu, tooltip)
    parented to another xdg_surface. With Grab() the compositor
    auto-dismisses on outside-click / Escape and routes keyboard
    focus to the popup; popup_done fires when that happens. }
  TXdgPopup = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnConfigure: TXdgPopupConfigureProc;
    OnDone:      TXdgPopupDoneProc;
    procedure Grab(Seat: TWaylandSeat; Serial: LongWord);
    procedure DestroyRequest;
  end;

implementation

{ TXdgWmBase }

procedure TXdgWmBase.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Serial: LongWord;
begin
  if Opcode = 0 then
  begin
    Serial := Reader.ReadUInt;
    if Assigned(OnPing) then OnPing(Serial);
  end;
end;

function TXdgWmBase.GetXdgSurface(Surface: TWaylandSurface): TXdgSurface;
var
  W: TWlWriter;
  XId: TWlObjectId;
begin
  XId := FDisplay.NewId;
  Result := TXdgSurface.Create(FDisplay, XId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(XId);
  W.WriteObject(Surface.Id);
  SendRequest(2, W);
end;

function TXdgWmBase.CreatePositioner: TXdgPositioner;
var
  W: TWlWriter;
  PId: TWlObjectId;
begin
  PId := FDisplay.NewId;
  Result := TXdgPositioner.Create(FDisplay, PId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(PId);
  SendRequest(1, W);
end;

procedure TXdgWmBase.Pong(Serial: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Serial);
  SendRequest(3, W);
end;

procedure TXdgWmBase.DestroyRequest;
begin
  SendRequest(0);
end;

{ TXdgSurface }

procedure TXdgSurface.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Serial: LongWord;
begin
  if Opcode = 0 then
  begin
    Serial := Reader.ReadUInt;
    if Assigned(OnConfigure) then OnConfigure(Self, Serial);
  end;
end;

function TXdgSurface.GetToplevel: TXdgToplevel;
var
  W: TWlWriter;
  TopId: TWlObjectId;
begin
  TopId := FDisplay.NewId;
  Result := TXdgToplevel.Create(FDisplay, TopId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(TopId);
  SendRequest(1, W);
end;

function TXdgSurface.GetPopup(Parent: TXdgSurface;
  Positioner: TXdgPositioner): TXdgPopup;
var
  W: TWlWriter;
  PopId: TWlObjectId;
begin
  PopId := FDisplay.NewId;
  Result := TXdgPopup.Create(FDisplay, PopId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(PopId);
  if Parent = nil then W.WriteObject(0)
  else                 W.WriteObject(Parent.Id);
  W.WriteObject(Positioner.Id);
  SendRequest(2, W);
end;

procedure TXdgSurface.SetWindowGeometry(X, Y, Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X); W.WriteInt(Y);
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(3, W);
end;

procedure TXdgSurface.AckConfigure(Serial: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Serial);
  SendRequest(4, W);
end;

procedure TXdgSurface.DestroyRequest;
begin
  SendRequest(0);
end;

{ TXdgToplevel }

procedure TXdgToplevel.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  CW, CH: LongInt;
  ArrPtr: PByte;
  ArrSize, Count: LongWord;
  States: array of LongWord;
begin
  case Opcode of
    0: begin   { configure(width, height, states[]) }
      CW := Reader.ReadInt;
      CH := Reader.ReadInt;
      Reader.ReadArray(ArrPtr, ArrSize);
      Count := ArrSize div SizeOf(LongWord);
      SetLength(States, Count);
      if (Count > 0) and (ArrPtr <> nil) then
        Move(ArrPtr^, States[0], Count * SizeOf(LongWord));
      if Assigned(OnConfigure) then OnConfigure(Self, CW, CH, States);
    end;
    1: begin   { close }
      if Assigned(OnClose) then OnClose(Self);
    end;
  end;
end;

procedure TXdgToplevel.SetParent(Parent: TXdgToplevel);
var
  W: TWlWriter;
begin
  W.Reset;
  if Parent = nil then
    W.WriteObject(0)              { detach from any current parent }
  else
    W.WriteObject(Parent.Id);
  SendRequest(1, W);              { xdg_toplevel.set_parent }
end;

procedure TXdgToplevel.SetTitle(const ATitle: AnsiString);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteString(ATitle);
  SendRequest(2, W);
end;

procedure TXdgToplevel.SetAppId(const AAppId: AnsiString);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteString(AAppId);
  SendRequest(3, W);
end;

procedure TXdgToplevel.SetMaxSize(Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(7, W);
end;

procedure TXdgToplevel.SetMinSize(Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(8, W);
end;

procedure TXdgToplevel.SetMaximized;
begin
  SendRequest(9);
end;

procedure TXdgToplevel.UnsetMaximized;
begin
  SendRequest(10);
end;

procedure TXdgToplevel.SetMinimized;
begin
  SendRequest(13);
end;

procedure TXdgToplevel.DestroyRequest;
begin
  SendRequest(0);
end;

{ TXdgPositioner }

procedure TXdgPositioner.SetSize(Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(1, W);
end;

procedure TXdgPositioner.SetAnchorRect(X, Y, Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X); W.WriteInt(Y);
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(2, W);
end;

procedure TXdgPositioner.SetAnchor(Anchor: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Anchor);
  SendRequest(3, W);
end;

procedure TXdgPositioner.SetGravity(Gravity: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Gravity);
  SendRequest(4, W);
end;

procedure TXdgPositioner.SetConstraintAdjustment(Bits: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Bits);
  SendRequest(5, W);
end;

procedure TXdgPositioner.SetOffset(X, Y: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X); W.WriteInt(Y);
  SendRequest(6, W);
end;

procedure TXdgPositioner.DestroyRequest;
begin
  SendRequest(0);
end;

{ TXdgPopup }

procedure TXdgPopup.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  CX, CY, CW, CH: LongInt;
begin
  case Opcode of
    0: begin   { configure(x, y, width, height) }
      CX := Reader.ReadInt;
      CY := Reader.ReadInt;
      CW := Reader.ReadInt;
      CH := Reader.ReadInt;
      if Assigned(OnConfigure) then OnConfigure(Self, CX, CY, CW, CH);
    end;
    1: begin   { popup_done }
      if Assigned(OnDone) then OnDone(Self);
    end;
    { 2: repositioned -- token argument; ignore for now }
  end;
end;

procedure TXdgPopup.Grab(Seat: TWaylandSeat; Serial: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteObject(Seat.Id);
  W.WriteUInt(Serial);
  SendRequest(1, W);
end;

procedure TXdgPopup.DestroyRequest;
begin
  SendRequest(0);
end;

end.
