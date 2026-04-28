unit xdgshell;

{ Bindings for the stable xdg-shell protocol: xdg_wm_base,
  xdg_surface and xdg_toplevel. Versions 1..5. }

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ctypes, waylandwire, waylandcore;

type
  TXdgWmBase   = class;
  TXdgSurface  = class;
  TXdgToplevel = class;

  TXdgPingProc                = procedure(Serial: LongWord) of object;
  TXdgConfigureProc           = procedure(Sender: TXdgSurface; Serial: LongWord) of object;
  TXdgToplevelConfigureProc   = procedure(Sender: TXdgToplevel;
                                          Width, Height: LongInt;
                                          const States: array of LongWord) of object;
  TXdgToplevelCloseProc       = procedure(Sender: TXdgToplevel) of object;

  TXdgWmBase = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnPing: TXdgPingProc;
    function GetXdgSurface(Surface: TWaylandSurface): TXdgSurface;
    procedure Pong(Serial: LongWord);
    procedure DestroyRequest;
  end;

  TXdgSurface = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnConfigure: TXdgConfigureProc;
    function GetToplevel: TXdgToplevel;
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

end.
