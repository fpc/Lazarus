unit waylandcore;

{ Core Wayland protocol objects: wl_display, wl_registry, wl_callback,
  wl_compositor, wl_surface, wl_region, wl_shm, wl_shm_pool, wl_buffer.

  Each request method allocates the new object id locally, registers
  the object with the display, and queues the request bytes. Each
  event opcode dispatches into a virtual method on the receiving
  object, which fires user-supplied callback procedures. }

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ctypes, BaseUnix, waylandwire;

const
  { wl_shm pixel formats }
  WL_SHM_FORMAT_ARGB8888 = 0;
  WL_SHM_FORMAT_XRGB8888 = 1;

  { Wayland id-range split: client ids run 1..WL_SERVER_ID_BASE-1,
    server-allocated ids run WL_SERVER_ID_BASE..$FFFFFFFF. Used by
    TWaylandDisplay.Register / Find / Unregister to pick the right
    storage list. }
  WL_SERVER_ID_BASE = LongWord($FF000000);

type
  TWaylandObject       = class;
  TWaylandObjectClass  = class of TWaylandObject;
  TWaylandDisplay      = class;
  TWaylandRegistry     = class;
  TWaylandCallback     = class;
  TWaylandCompositor   = class;
  TWaylandSurface      = class;
  TWaylandRegion       = class;
  TWaylandShm          = class;
  TWaylandShmPool      = class;
  TWaylandBuffer       = class;
  TWaylandSeat         = class;
  TWaylandPointer      = class;
  TWaylandKeyboard     = class;
  TWaylandOutput       = class;

  { Callback types are method types ('of object') so the host can bind
    them to instance methods of its widget set / window class without
    routing through globals. Pure-function callers that don't need a
    receiver can adapt with a one-line wrapper method. }
  TGlobalProc          = procedure(Name: LongWord;
                                   const Iface: AnsiString;
                                   Version: LongWord) of object;
  TGlobalRemoveProc    = procedure(Name: LongWord) of object;
  TCallbackDoneProc    = procedure(CallbackData: LongWord) of object;
  TBufferReleaseProc   = procedure(Buffer: TWaylandBuffer) of object;
  TShmFormatProc       = procedure(Format: LongWord) of object;
  TDisplayErrorProc    = procedure(ObjectId: TWlObjectId;
                                   Code: LongWord;
                                   const Message: AnsiString) of object;

  { Seat capabilities bitmask. }
  TSeatCapsProc        = procedure(Sender: TWaylandSeat; Caps: LongWord) of object;

  { wl_output property bursts. The compositor sends geometry+mode (and
    on v2+ scale) one after the other and finishes with done; consumers
    should treat the snapshot as valid only after done fires. }
  TOutputDoneProc      = procedure(Sender: TWaylandOutput) of object;

  { Pointer events. Coordinates are wl_fixed (24.8); we deliver them
    pre-converted to integer pixels because LCL only deals in ints. }
  TPointerEnterProc    = procedure(Sender: TWaylandPointer; Serial: LongWord;
                                   Surface: TWaylandSurface; X, Y: LongInt) of object;
  TPointerLeaveProc    = procedure(Sender: TWaylandPointer; Serial: LongWord;
                                   Surface: TWaylandSurface) of object;
  TPointerMotionProc   = procedure(Sender: TWaylandPointer; TimeMs: LongWord;
                                   X, Y: LongInt) of object;
  TPointerButtonProc   = procedure(Sender: TWaylandPointer; Serial, TimeMs,
                                   Button, State: LongWord) of object;
  TPointerAxisProc     = procedure(Sender: TWaylandPointer; TimeMs, Axis: LongWord;
                                   Value: TWlFixed) of object;

  { Keyboard events. The keymap event hands us a file descriptor that
    must be mmapped to read the XKB keymap; we expose it raw and let
    the host decide. }
  TKeyboardKeymapProc  = procedure(Sender: TWaylandKeyboard; Format: LongWord;
                                   Fd: cint; Size: LongWord) of object;
  TKeyboardEnterProc   = procedure(Sender: TWaylandKeyboard; Serial: LongWord;
                                   Surface: TWaylandSurface) of object;
  TKeyboardLeaveProc   = procedure(Sender: TWaylandKeyboard; Serial: LongWord;
                                   Surface: TWaylandSurface) of object;
  TKeyboardKeyProc     = procedure(Sender: TWaylandKeyboard; Serial, TimeMs,
                                   Key, State: LongWord) of object;
  TKeyboardModsProc    = procedure(Sender: TWaylandKeyboard; Serial,
                                   ModsDepressed, ModsLatched, ModsLocked,
                                   Group: LongWord) of object;

  TWaylandObject = class
  protected
    FId:      TWlObjectId;
    FDisplay: TWaylandDisplay;
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); reintroduce; virtual;
    procedure SendRequest(Opcode: Word); overload;
    procedure SendRequest(Opcode: Word; const Writer: TWlWriter); overload;
    procedure SendRequest(Opcode: Word; const Writer: TWlWriter;
                          const Fds: array of cint); overload;
  public
    constructor Create(ADisplay: TWaylandDisplay; AId: TWlObjectId); virtual;
    property Id: TWlObjectId read FId;
    property Display: TWaylandDisplay read FDisplay;
  end;

  TWaylandDisplay = class(TWaylandObject)
  private
    FConn:           TWlConnection;
    { Wayland splits the 32-bit object-id range: client allocates
      1..WL_SERVER_ID_BASE-1, server allocates WL_SERVER_ID_BASE
      upwards (used when the server delivers a new_id to the client,
      e.g. via wl_data_device.data_offer). We keep two sparse lists. }
    FObjects:        TFPList;     { client ids: indexed by id }
    FServerObjects:  TFPList;     { server ids: indexed by id - WL_SERVER_ID_BASE }
    FNextId:         TWlObjectId;
    FFatalError:     AnsiString;
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnError: TDisplayErrorProc;
    constructor Create; reintroduce;
    destructor Destroy; override;
    function NewId: TWlObjectId;
    procedure Register(Obj: TWaylandObject);
    procedure Unregister(Obj: TWaylandObject);
    function Find(AId: TWlObjectId): TWaylandObject;
    function GetRegistry: TWaylandRegistry;
    function Sync: TWaylandCallback;
    procedure Roundtrip;
    function  DispatchOne(Block: Boolean): Boolean;
    property Connection: TWlConnection read FConn;
    property FatalError: AnsiString read FFatalError;
  end;

  TWaylandRegistry = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnGlobal:        TGlobalProc;
    OnGlobalRemove:  TGlobalRemoveProc;
    function BindRaw(Name: LongWord; const Iface: AnsiString;
      Version: LongWord; ObjClass: TWaylandObjectClass): TWaylandObject;
  end;

  TWaylandCallback = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    Done: Boolean;
    OnDone: TCallbackDoneProc;
  end;

  TWaylandCompositor = class(TWaylandObject)
  public
    function CreateSurface: TWaylandSurface;
    function CreateRegion: TWaylandRegion;
  end;

  TWaylandSurface = class(TWaylandObject)
  public
    procedure Attach(Buffer: TWaylandBuffer; X, Y: LongInt);
    procedure Damage(X, Y, Width, Height: LongInt);
    procedure DamageBuffer(X, Y, Width, Height: LongInt);
    procedure SetBufferScale(Scale: LongInt);
    procedure Frame(Callback: TWaylandCallback);
    function  RequestFrame: TWaylandCallback;
    procedure Commit;
    procedure DestroyRequest;
  end;

  TWaylandRegion = class(TWaylandObject)
  public
    procedure Add(X, Y, Width, Height: LongInt);
    procedure Subtract(X, Y, Width, Height: LongInt);
    procedure DestroyRequest;
  end;

  TWaylandShm = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnFormat: TShmFormatProc;
    function CreatePool(Fd: cint; Size: LongInt): TWaylandShmPool;
  end;

  TWaylandShmPool = class(TWaylandObject)
  public
    function CreateBuffer(Offset, Width, Height, Stride: LongInt;
                          Format: LongWord): TWaylandBuffer;
    procedure Resize(Size: LongInt);
    procedure DestroyRequest;
  end;

  TWaylandBuffer = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnRelease: TBufferReleaseProc;
    procedure DestroyRequest;
  end;

  { wl_seat. Handles capabilities and (optional) name events. After
    receiving a capabilities event the host may call GetPointer /
    GetKeyboard to retrieve the corresponding sub-objects. }
  TWaylandSeat = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    Capabilities: LongWord;
    Name: AnsiString;
    OnCapabilities: TSeatCapsProc;
    function GetPointer: TWaylandPointer;
    function GetKeyboard: TWaylandKeyboard;
    procedure Release;
  end;

  TWaylandPointer = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnEnter:  TPointerEnterProc;
    OnLeave:  TPointerLeaveProc;
    OnMotion: TPointerMotionProc;
    OnButton: TPointerButtonProc;
    OnAxis:   TPointerAxisProc;
    procedure SetCursor(Serial: LongWord; Surface: TWaylandSurface;
                        HotspotX, HotspotY: LongInt);
    procedure Release;
  end;

  TWaylandKeyboard = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnKeymap:    TKeyboardKeymapProc;
    OnEnter:     TKeyboardEnterProc;
    OnLeave:     TKeyboardLeaveProc;
    OnKey:       TKeyboardKeyProc;
    OnModifiers: TKeyboardModsProc;
    procedure Release;
  end;

  { wl_output. Snapshot of one monitor as advertised by the compositor.
    Properties arrive in a burst (geometry, mode, scale on v2+, name &
    description on v4+) and end with done. The fields below carry the
    most recent values; OnDone fires after each burst so callers can
    re-evaluate aggregates (DPI, total desktop bounds). v1 of the
    protocol omits scale -- in that case ScaleFactor stays 1. }
  TWaylandOutput = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    XPos, YPos:           LongInt;       { compositor-space origin }
    PhysicalWidthMm:      LongInt;       { 0 if compositor doesn't know }
    PhysicalHeightMm:     LongInt;
    Subpixel:             LongInt;
    Make, Model:          AnsiString;
    Transform:            LongInt;
    CurrentWidthPx:       LongInt;       { mode w/h are pixel counts pre-scale }
    CurrentHeightPx:      LongInt;
    RefreshMHz:           LongInt;       { hundredths of Hz, e.g. 60000 = 60Hz }
    ScaleFactor:          LongInt;       { integer; 1 unless explicitly sent }
    Name:                 AnsiString;    { v4+ }
    Description:          AnsiString;    { v4+ }
    OnDone:               TOutputDoneProc;
    constructor Create(ADisplay: TWaylandDisplay; AId: TWlObjectId); override;
  end;

implementation

{ TWaylandObject }

constructor TWaylandObject.Create(ADisplay: TWaylandDisplay; AId: TWlObjectId);
begin
  inherited Create;
  FDisplay := ADisplay;
  FId := AId;
end;

procedure TWaylandObject.Dispatch(Opcode: Word; var Reader: TWlReader);
begin
  { Default: ignore unknown events. }
end;

procedure TWaylandObject.SendRequest(Opcode: Word);
begin
  FDisplay.Connection.QueueRequest(FId, Opcode, nil, 0, []);
end;

procedure TWaylandObject.SendRequest(Opcode: Word; const Writer: TWlWriter);
begin
  SendRequest(Opcode, Writer, []);
end;

procedure TWaylandObject.SendRequest(Opcode: Word; const Writer: TWlWriter;
  const Fds: array of cint);
var
  Sz: SizeInt;
  Ptr: Pointer;
begin
  Sz := Length(Writer.Buf);
  if Sz > 0 then Ptr := @Writer.Buf[0] else Ptr := nil;
  FDisplay.Connection.QueueRequest(FId, Opcode, Ptr, Sz, Fds);
end;

{ TWaylandDisplay }

constructor TWaylandDisplay.Create;
begin
  FConn := TWlConnection.Create;
  FObjects := TFPList.Create;
  FServerObjects := TFPList.Create;
  FNextId := 2;
  inherited Create(Self, 1);
  Register(Self);
end;

destructor TWaylandDisplay.Destroy;
begin
  FConn.Free;
  FObjects.Free;
  FServerObjects.Free;
  inherited Destroy;
end;

function TWaylandDisplay.NewId: TWlObjectId;
begin
  Result := FNextId;
  Inc(FNextId);
end;

procedure TWaylandDisplay.Register(Obj: TWaylandObject);
var
  L: TFPList;
  Idx: PtrInt;
begin
  if Obj.Id >= WL_SERVER_ID_BASE then
  begin
    L := FServerObjects;
    Idx := PtrInt(Obj.Id - WL_SERVER_ID_BASE);
  end
  else
  begin
    L := FObjects;
    Idx := PtrInt(Obj.Id);
  end;
  while L.Count <= Idx do L.Add(nil);
  L[Idx] := Obj;
end;

procedure TWaylandDisplay.Unregister(Obj: TWaylandObject);
var
  L: TFPList;
  Idx: PtrInt;
begin
  if Obj = nil then Exit;
  if Obj.Id >= WL_SERVER_ID_BASE then
  begin
    L := FServerObjects;
    Idx := PtrInt(Obj.Id - WL_SERVER_ID_BASE);
  end
  else
  begin
    L := FObjects;
    Idx := PtrInt(Obj.Id);
  end;
  if (Idx >= 0) and (Idx < L.Count) then L[Idx] := nil;
end;

function TWaylandDisplay.Find(AId: TWlObjectId): TWaylandObject;
var
  L: TFPList;
  Idx: PtrInt;
begin
  if AId >= WL_SERVER_ID_BASE then
  begin
    L := FServerObjects;
    Idx := PtrInt(AId - WL_SERVER_ID_BASE);
  end
  else
  begin
    L := FObjects;
    Idx := PtrInt(AId);
  end;
  if (Idx >= 0) and (Idx < L.Count) then
    Result := TWaylandObject(L[Idx])
  else
    Result := nil;
end;

function TWaylandDisplay.GetRegistry: TWaylandRegistry;
var
  W: TWlWriter;
  RegId: TWlObjectId;
begin
  RegId := NewId;
  Result := TWaylandRegistry.Create(Self, RegId);
  Register(Result);
  W.Reset;
  W.WriteNewId(RegId);
  SendRequest(1, W);  { wl_display.get_registry }
end;

function TWaylandDisplay.Sync: TWaylandCallback;
var
  W: TWlWriter;
  CbId: TWlObjectId;
begin
  CbId := NewId;
  Result := TWaylandCallback.Create(Self, CbId);
  Register(Result);
  W.Reset;
  W.WriteNewId(CbId);
  SendRequest(0, W);  { wl_display.sync }
end;

procedure TWaylandDisplay.Roundtrip;
var
  Cb: TWaylandCallback;
begin
  Cb := Sync;
  try
    while not Cb.Done do
      DispatchOne(True);
  finally
    Unregister(Cb);
    Cb.Free;
  end;
end;

function TWaylandDisplay.DispatchOne(Block: Boolean): Boolean;
var
  Msg: TWlMessage;
  Obj: TWaylandObject;
  Reader: TWlReader;
begin
  Result := False;
  FConn.Flush;
  if not FConn.NextMessage(Msg, Block) then Exit;
  Result := True;
  Obj := Find(Msg.Sender);
  if Obj = nil then Exit;
  if Length(Msg.Payload) > 0 then
    Reader.Init(@Msg.Payload[0], Length(Msg.Payload))
  else
    Reader.Init(nil, 0);
  Obj.Dispatch(Msg.Opcode, Reader);
end;

procedure TWaylandDisplay.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  ObjId, Code: LongWord;
  Msg: AnsiString;
  DeletedId: LongWord;
begin
  case Opcode of
    0: begin   { error }
      ObjId := Reader.ReadObject;
      Code  := Reader.ReadUInt;
      Msg   := Reader.ReadString;
      FFatalError := Format(
        'wl_display.error: object=%u code=%u message=%s',
        [ObjId, Code, Msg]);
      if Assigned(OnError) then
        OnError(ObjId, Code, Msg);
      raise Exception.Create(FFatalError);
    end;
    1: begin   { delete_id -- server has reclaimed an id (always one
                  the client allocated). Drop the entry so a future
                  Register at the same id slot re-allocates fresh. }
      DeletedId := Reader.ReadUInt;
      if (DeletedId < WL_SERVER_ID_BASE)
         and (PtrInt(DeletedId) < FObjects.Count) then
        FObjects[PtrInt(DeletedId)] := nil;
    end;
  end;
end;

{ TWaylandRegistry }

procedure TWaylandRegistry.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Name, Version: LongWord;
  Iface: AnsiString;
begin
  case Opcode of
    0: begin   { global }
      Name    := Reader.ReadUInt;
      Iface   := Reader.ReadString;
      Version := Reader.ReadUInt;
      if Assigned(OnGlobal) then
        OnGlobal(Name, Iface, Version);
    end;
    1: begin   { global_remove }
      Name := Reader.ReadUInt;
      if Assigned(OnGlobalRemove) then
        OnGlobalRemove(Name);
    end;
  end;
end;

function TWaylandRegistry.BindRaw(Name: LongWord; const Iface: AnsiString;
  Version: LongWord; ObjClass: TWaylandObjectClass): TWaylandObject;
var
  W: TWlWriter;
  ObjId: TWlObjectId;
begin
  ObjId := FDisplay.NewId;
  Result := ObjClass.Create(FDisplay, ObjId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteUInt(Name);
  W.WriteNewIdGeneric(Iface, Version, ObjId);
  SendRequest(0, W);  { wl_registry.bind }
end;

{ TWaylandCallback }

procedure TWaylandCallback.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Data: LongWord;
begin
  if Opcode = 0 then  { done }
  begin
    Data := Reader.ReadUInt;
    Done := True;
    if Assigned(OnDone) then
      OnDone(Data);
  end;
end;

{ TWaylandCompositor }

function TWaylandCompositor.CreateSurface: TWaylandSurface;
var
  W: TWlWriter;
  SurfId: TWlObjectId;
begin
  SurfId := FDisplay.NewId;
  Result := TWaylandSurface.Create(FDisplay, SurfId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(SurfId);
  SendRequest(0, W);  { wl_compositor.create_surface }
end;

function TWaylandCompositor.CreateRegion: TWaylandRegion;
var
  W: TWlWriter;
  RegionId: TWlObjectId;
begin
  RegionId := FDisplay.NewId;
  Result := TWaylandRegion.Create(FDisplay, RegionId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(RegionId);
  SendRequest(1, W);  { wl_compositor.create_region }
end;

{ TWaylandSurface }

procedure TWaylandSurface.DestroyRequest;
begin
  SendRequest(0);
end;

procedure TWaylandSurface.Attach(Buffer: TWaylandBuffer; X, Y: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  if Assigned(Buffer) then
    W.WriteObject(Buffer.Id)
  else
    W.WriteObject(0);
  W.WriteInt(X);
  W.WriteInt(Y);
  SendRequest(1, W);
end;

procedure TWaylandSurface.Damage(X, Y, Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X); W.WriteInt(Y);
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(2, W);
end;

procedure TWaylandSurface.Frame(Callback: TWaylandCallback);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteNewId(Callback.Id);
  SendRequest(3, W);
end;

function TWaylandSurface.RequestFrame: TWaylandCallback;
var
  CbId: TWlObjectId;
begin
  CbId := FDisplay.NewId;
  Result := TWaylandCallback.Create(FDisplay, CbId);
  FDisplay.Register(Result);
  Frame(Result);
end;

procedure TWaylandSurface.Commit;
begin
  SendRequest(6);
end;

procedure TWaylandSurface.SetBufferScale(Scale: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(Scale);
  SendRequest(8, W);
end;

procedure TWaylandSurface.DamageBuffer(X, Y, Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X); W.WriteInt(Y);
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(9, W);
end;

{ TWaylandRegion }

procedure TWaylandRegion.DestroyRequest;
begin
  SendRequest(0);
end;

procedure TWaylandRegion.Add(X, Y, Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X); W.WriteInt(Y);
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(1, W);
end;

procedure TWaylandRegion.Subtract(X, Y, Width, Height: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(X); W.WriteInt(Y);
  W.WriteInt(Width); W.WriteInt(Height);
  SendRequest(2, W);
end;

{ TWaylandShm }

procedure TWaylandShm.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Format: LongWord;
begin
  if Opcode = 0 then
  begin
    Format := Reader.ReadUInt;
    if Assigned(OnFormat) then
      OnFormat(Format);
  end;
end;

function TWaylandShm.CreatePool(Fd: cint; Size: LongInt): TWaylandShmPool;
var
  W: TWlWriter;
  PoolId: TWlObjectId;
begin
  PoolId := FDisplay.NewId;
  Result := TWaylandShmPool.Create(FDisplay, PoolId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(PoolId);
  W.WriteInt(Size);
  SendRequest(0, W, [Fd]);
end;

{ TWaylandShmPool }

function TWaylandShmPool.CreateBuffer(Offset, Width, Height, Stride: LongInt;
  Format: LongWord): TWaylandBuffer;
var
  W: TWlWriter;
  BufId: TWlObjectId;
begin
  BufId := FDisplay.NewId;
  Result := TWaylandBuffer.Create(FDisplay, BufId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(BufId);
  W.WriteInt(Offset);
  W.WriteInt(Width);
  W.WriteInt(Height);
  W.WriteInt(Stride);
  W.WriteUInt(Format);
  SendRequest(0, W);
end;

procedure TWaylandShmPool.DestroyRequest;
begin
  SendRequest(1);
end;

procedure TWaylandShmPool.Resize(Size: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteInt(Size);
  SendRequest(2, W);
end;

{ TWaylandBuffer }

procedure TWaylandBuffer.Dispatch(Opcode: Word; var Reader: TWlReader);
begin
  if Opcode = 0 then
    if Assigned(OnRelease) then
      OnRelease(Self);
end;

procedure TWaylandBuffer.DestroyRequest;
begin
  SendRequest(0);
end;

{ TWaylandSeat }

procedure TWaylandSeat.Dispatch(Opcode: Word; var Reader: TWlReader);
begin
  case Opcode of
    0: begin   { capabilities(uint) }
      Capabilities := Reader.ReadUInt;
      if Assigned(OnCapabilities) then OnCapabilities(Self, Capabilities);
    end;
    1: begin   { name(string) }
      Name := Reader.ReadString;
    end;
  end;
end;

function TWaylandSeat.GetPointer: TWaylandPointer;
var
  W: TWlWriter;
  Id: TWlObjectId;
begin
  Id := FDisplay.NewId;
  Result := TWaylandPointer.Create(FDisplay, Id);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(Id);
  SendRequest(0, W);  { get_pointer }
end;

function TWaylandSeat.GetKeyboard: TWaylandKeyboard;
var
  W: TWlWriter;
  Id: TWlObjectId;
begin
  Id := FDisplay.NewId;
  Result := TWaylandKeyboard.Create(FDisplay, Id);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(Id);
  SendRequest(1, W);  { get_keyboard }
end;

procedure TWaylandSeat.Release;
begin
  SendRequest(3);  { release, requires version 5+ }
end;

{ TWaylandPointer }

procedure TWaylandPointer.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Serial, TimeMs, Button, State, Axis: LongWord;
  SurfId: TWlObjectId;
  XFix, YFix, ValueFix: TWlFixed;
  Surface: TWaylandSurface;
  Obj: TWaylandObject;
begin
  case Opcode of
    0: begin   { enter(serial, surface, sx_fixed, sy_fixed) }
      Serial := Reader.ReadUInt;
      SurfId := Reader.ReadObject;
      XFix   := Reader.ReadFixed;
      YFix   := Reader.ReadFixed;
      Obj := FDisplay.Find(SurfId);
      if Obj is TWaylandSurface then Surface := TWaylandSurface(Obj) else Surface := nil;
      if Assigned(OnEnter) then
        OnEnter(Self, Serial, Surface, XFix shr 8, YFix shr 8);
    end;
    1: begin   { leave(serial, surface) }
      Serial := Reader.ReadUInt;
      SurfId := Reader.ReadObject;
      Obj := FDisplay.Find(SurfId);
      if Obj is TWaylandSurface then Surface := TWaylandSurface(Obj) else Surface := nil;
      if Assigned(OnLeave) then OnLeave(Self, Serial, Surface);
    end;
    2: begin   { motion(time, sx_fixed, sy_fixed) }
      TimeMs := Reader.ReadUInt;
      XFix   := Reader.ReadFixed;
      YFix   := Reader.ReadFixed;
      if Assigned(OnMotion) then
        OnMotion(Self, TimeMs, XFix shr 8, YFix shr 8);
    end;
    3: begin   { button(serial, time, button, state) }
      Serial := Reader.ReadUInt;
      TimeMs := Reader.ReadUInt;
      Button := Reader.ReadUInt;
      State  := Reader.ReadUInt;
      if Assigned(OnButton) then OnButton(Self, Serial, TimeMs, Button, State);
    end;
    4: begin   { axis(time, axis, value_fixed) }
      TimeMs   := Reader.ReadUInt;
      Axis     := Reader.ReadUInt;
      ValueFix := Reader.ReadFixed;
      if Assigned(OnAxis) then OnAxis(Self, TimeMs, Axis, ValueFix);
    end;
    { Opcodes 5-10 are pointer "frame" / axis-source / axis-stop /
      axis-discrete / axis-value120 / axis-relative-direction (v5+).
      We bind v1 of wl_seat so the compositor won't emit them. }
  end;
end;

procedure TWaylandPointer.SetCursor(Serial: LongWord; Surface: TWaylandSurface;
  HotspotX, HotspotY: LongInt);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Serial);
  if Surface <> nil then W.WriteObject(Surface.Id) else W.WriteObject(0);
  W.WriteInt(HotspotX);
  W.WriteInt(HotspotY);
  SendRequest(0, W);
end;

procedure TWaylandPointer.Release;
begin
  SendRequest(1);  { release, requires version 3+ }
end;

{ TWaylandKeyboard }

procedure TWaylandKeyboard.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Format, Size, Serial, TimeMs, Key, State: LongWord;
  ModsDepressed, ModsLatched, ModsLocked, Group: LongWord;
  SurfId: TWlObjectId;
  Surface: TWaylandSurface;
  Obj: TWaylandObject;
  ArrPtr: PByte;
  ArrSize: LongWord;
  Fd: cint;
begin
  case Opcode of
    0: begin   { keymap(format, fd, size) }
      Format := Reader.ReadUInt;
      Fd     := FDisplay.Connection.PopFd;
      Size   := Reader.ReadUInt;
      if Assigned(OnKeymap) then OnKeymap(Self, Format, Fd, Size);
    end;
    1: begin   { enter(serial, surface, keys-array) }
      Serial := Reader.ReadUInt;
      SurfId := Reader.ReadObject;
      Reader.ReadArray(ArrPtr, ArrSize);   { currently-held keys; ignored }
      Obj := FDisplay.Find(SurfId);
      if Obj is TWaylandSurface then Surface := TWaylandSurface(Obj) else Surface := nil;
      if Assigned(OnEnter) then OnEnter(Self, Serial, Surface);
    end;
    2: begin   { leave(serial, surface) }
      Serial := Reader.ReadUInt;
      SurfId := Reader.ReadObject;
      Obj := FDisplay.Find(SurfId);
      if Obj is TWaylandSurface then Surface := TWaylandSurface(Obj) else Surface := nil;
      if Assigned(OnLeave) then OnLeave(Self, Serial, Surface);
    end;
    3: begin   { key(serial, time, key, state) }
      Serial := Reader.ReadUInt;
      TimeMs := Reader.ReadUInt;
      Key    := Reader.ReadUInt;
      State  := Reader.ReadUInt;
      if Assigned(OnKey) then OnKey(Self, Serial, TimeMs, Key, State);
    end;
    4: begin   { modifiers(serial, mods_depressed, mods_latched, mods_locked, group) }
      Serial        := Reader.ReadUInt;
      ModsDepressed := Reader.ReadUInt;
      ModsLatched   := Reader.ReadUInt;
      ModsLocked    := Reader.ReadUInt;
      Group         := Reader.ReadUInt;
      if Assigned(OnModifiers) then
        OnModifiers(Self, Serial, ModsDepressed, ModsLatched, ModsLocked, Group);
    end;
    { Opcode 5: repeat_info (v4+); we ignore. }
  end;
end;

procedure TWaylandKeyboard.Release;
begin
  SendRequest(0);  { release, requires version 3+ }
end;

{ TWaylandOutput }

constructor TWaylandOutput.Create(ADisplay: TWaylandDisplay; AId: TWlObjectId);
begin
  inherited Create(ADisplay, AId);
  { v1 of wl_output never sends a scale event, so consumers expect 1. }
  ScaleFactor := 1;
end;

procedure TWaylandOutput.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Flags: LongWord;
begin
  case Opcode of
    0: begin   { geometry(x, y, physical_w_mm, physical_h_mm, subpixel,
                  make, model, transform) }
      XPos             := Reader.ReadInt;
      YPos             := Reader.ReadInt;
      PhysicalWidthMm  := Reader.ReadInt;
      PhysicalHeightMm := Reader.ReadInt;
      Subpixel         := Reader.ReadInt;
      Make             := Reader.ReadString;
      Model            := Reader.ReadString;
      Transform        := Reader.ReadInt;
    end;
    1: begin   { mode(flags, width, height, refresh) }
      Flags := Reader.ReadUInt;
      { Bit 0 = current. The compositor may advertise alternate non-current
        modes too; we only care about the active one. }
      if (Flags and 1) <> 0 then
      begin
        CurrentWidthPx  := Reader.ReadInt;
        CurrentHeightPx := Reader.ReadInt;
        RefreshMHz      := Reader.ReadInt;
      end
      else
      begin
        Reader.ReadInt; Reader.ReadInt; Reader.ReadInt;
      end;
    end;
    2: begin   { done -- v2+; signals end of property burst }
      if Assigned(OnDone) then OnDone(Self);
    end;
    3: begin   { scale(factor) -- v2+ }
      ScaleFactor := Reader.ReadInt;
      if ScaleFactor < 1 then ScaleFactor := 1;
    end;
    4: begin   { name(string) -- v4+ }
      Name := Reader.ReadString;
    end;
    5: begin   { description(string) -- v4+ }
      Description := Reader.ReadString;
    end;
  end;
end;

end.
