unit xdgactivation;

{ Bindings for xdg_activation_v1 + xdg_activation_token_v1 (stable, version 1).
  Used to claim activation/focus for a newly-mapped toplevel -- either with
  a launcher-supplied XDG_ACTIVATION_TOKEN, or a token we mint ourselves. }

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ctypes, waylandwire, waylandcore;

type
  TXdgActivationV1     = class;
  TXdgActivationTokenV1 = class;

  TActivationDoneProc = procedure(const Token: AnsiString) of object;

  TXdgActivationV1 = class(TWaylandObject)
  public
    function GetActivationToken: TXdgActivationTokenV1;
    procedure Activate(const Token: AnsiString; Surface: TWaylandSurface);
    procedure DestroyRequest;
  end;

  TXdgActivationTokenV1 = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    Done:   Boolean;
    Token:  AnsiString;
    OnDone: TActivationDoneProc;
    procedure SetSerial(Serial: LongWord; Seat: TWaylandObject);
    procedure SetAppId(const AppId: AnsiString);
    procedure SetSurface(Surface: TWaylandSurface);
    procedure Commit;
    procedure DestroyRequest;
    function  WaitForDone(ADisplay: TWaylandDisplay): AnsiString;
  end;

implementation

{ TXdgActivationV1 }

function TXdgActivationV1.GetActivationToken: TXdgActivationTokenV1;
var
  W: TWlWriter;
  TokenId: TWlObjectId;
begin
  TokenId := FDisplay.NewId;
  Result := TXdgActivationTokenV1.Create(FDisplay, TokenId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(TokenId);
  SendRequest(1, W);  { get_activation_token }
end;

procedure TXdgActivationV1.Activate(const Token: AnsiString;
  Surface: TWaylandSurface);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteString(Token);
  W.WriteObject(Surface.Id);
  SendRequest(2, W);  { activate }
end;

procedure TXdgActivationV1.DestroyRequest;
begin
  SendRequest(0);
end;

{ TXdgActivationTokenV1 }

procedure TXdgActivationTokenV1.Dispatch(Opcode: Word; var Reader: TWlReader);
begin
  if Opcode = 0 then    { done(token) }
  begin
    Token := Reader.ReadString;
    Done := True;
    if Assigned(OnDone) then OnDone(Token);
  end;
end;

procedure TXdgActivationTokenV1.SetSerial(Serial: LongWord;
  Seat: TWaylandObject);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Serial);
  if Seat <> nil then
    W.WriteObject(Seat.Id)
  else
    W.WriteObject(0);
  SendRequest(0, W);  { set_serial }
end;

procedure TXdgActivationTokenV1.SetAppId(const AppId: AnsiString);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteString(AppId);
  SendRequest(1, W);  { set_app_id }
end;

procedure TXdgActivationTokenV1.SetSurface(Surface: TWaylandSurface);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteObject(Surface.Id);
  SendRequest(2, W);  { set_surface }
end;

procedure TXdgActivationTokenV1.Commit;
begin
  SendRequest(3);
end;

procedure TXdgActivationTokenV1.DestroyRequest;
begin
  SendRequest(4);
end;

function TXdgActivationTokenV1.WaitForDone(ADisplay: TWaylandDisplay): AnsiString;
begin
  while not Done do
    ADisplay.DispatchOne(True);
  Result := Token;
end;

end.
