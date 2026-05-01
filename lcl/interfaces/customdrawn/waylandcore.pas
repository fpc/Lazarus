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

  TGlobalProc          = procedure(Name: LongWord;
                                   const Iface: AnsiString;
                                   Version: LongWord);
  TGlobalRemoveProc    = procedure(Name: LongWord);
  TCallbackDoneProc    = procedure(CallbackData: LongWord);
  TBufferReleaseProc   = procedure(Buffer: TWaylandBuffer);
  TShmFormatProc       = procedure(Format: LongWord);
  TDisplayErrorProc    = procedure(ObjectId: TWlObjectId;
                                   Code: LongWord;
                                   const Message: AnsiString);

  { Seat capabilities bitmask. }
  TSeatCapsProc        = procedure(Sender: TWaylandSeat; Caps: LongWord);

  { Pointer events. Coordinates are wl_fixed (24.8); we deliver them
    pre-converted to integer pixels because LCL only deals in ints. }
  TPointerEnterProc    = procedure(Sender: TWaylandPointer; Serial: LongWord;
                                   Surface: TWaylandSurface; X, Y: LongInt);
  TPointerLeaveProc    = procedure(Sender: TWaylandPointer; Serial: LongWord;
                                   Surface: TWaylandSurface);
  TPointerMotionProc   = procedure(Sender: TWaylandPointer; TimeMs: LongWord;
                                   X, Y: LongInt);
  TPointerButtonProc   = procedure(Sender: TWaylandPointer; Serial, TimeMs,
                                   Button, State: LongWord);
  TPointerAxisProc     = procedure(Sender: TWaylandPointer; TimeMs, Axis: LongWord;
                                   Value: TWlFixed);

  { Keyboard events. The keymap event hands us a file descriptor that
    must be mmapped to read the XKB keymap; we expose it raw and let
    the host decide. }
  TKeyboardKeymapProc  = procedure(Sender: TWaylandKeyboard; Format: LongWord;
                                   Fd: cint; Size: LongWord);
  TKeyboardEnterProc   = procedure(Sender: TWaylandKeyboard; Serial: LongWord;
                                   Surface: TWaylandSurface);
  TKeyboardLeaveProc   = procedure(Sender: TWaylandKeyboard; Serial: LongWord;
                                   Surface: TWaylandSurface);
  TKeyboardKeyProc     = procedure(Sender: TWaylandKeyboard; Serial, TimeMs,
                                   Key, State: LongWord);
  TKeyboardModsProc    = procedure(Sender: TWaylandKeyboard; Serial,
                                   ModsDepressed, ModsLatched, ModsLocked,
                                   Group: LongWord);

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
    FConn:        TWlConnection;
    FObjects:     TFPList;        { sparse, indexed by object id }
    FNextId:      TWlObjectId;
    FFatalError:  AnsiString;
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
  FNextId := 2;
  inherited Create(Self, 1);
  Register(Self);
end;

destructor TWaylandDisplay.Destroy;
begin
  FConn.Free;
  FObjects.Free;
  inherited Destroy;
end;

function TWaylandDisplay.NewId: TWlObjectId;
begin
  Result := FNextId;
  Inc(FNextId);
end;

procedure TWaylandDisplay.Register(Obj: TWaylandObject);
begin
  while FObjects.Count <= Integer(Obj.Id) do
    FObjects.Add(nil);
  FObjects[Integer(Obj.Id)] := Obj;
end;

procedure TWaylandDisplay.Unregister(Obj: TWaylandObject);
begin
  if (Obj <> nil) and (Integer(Obj.Id) < FObjects.Count) then
    FObjects[Integer(Obj.Id)] := nil;
end;

function TWaylandDisplay.Find(AId: TWlObjectId): TWaylandObject;
begin
  if Integer(AId) < FObjects.Count then
    Result := TWaylandObject(FObjects[Integer(AId)])
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
    1: begin   { delete_id }
      DeletedId := Reader.ReadUInt;
      if Integer(DeletedId) < FObjects.Count then
        FObjects[Integer(DeletedId)] := nil;
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

end.
