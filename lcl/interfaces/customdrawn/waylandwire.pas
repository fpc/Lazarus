unit waylandwire;

{ Low-level Wayland wire protocol: AF_UNIX socket, framed messages,
  SCM_RIGHTS fd passing, message reader/writer helpers, and an
  anonymous-file allocator suitable for wl_shm.

  No libc. Everything goes through native syscalls:
    - socket, connect       -- via FPC's Sockets unit (fpSocket,
                               fpConnect). FPC's Sockets RTL on Linux
                               issues these via Do_SysCall internally;
                               on i386 it routes them through the
                               multiplexed socketcall(2). All native.
    - close, poll, ftruncate -- via FPC's BaseUnix unit (FpClose,
                               FpPoll, FpFtruncate). Same story --
                               FPC's wrappers, native syscalls, no
                               libc.
    - sendmsg, recvmsg      -- this unit issues these directly via
                               Do_SysCall(syscall_nr_sendmsg/_recvmsg)
                               because FPC's Sockets unit only exposes
                               fpSend/fpRecv (= sendto/recvfrom), which
                               can't carry SCM_RIGHTS ancillary data
                               and so can't pass file descriptors.
    - memfd_create          -- this unit issues it directly via
                               Do_SysCall, with a per-CPU Linux syscall
                               number we declare ourselves below.
                               memfd_create is a Linux 3.17 syscall;
                               whether FPC's own per-CPU sysnr.inc
                               declares it is FPC-version dependent
                               (FPC 3.2.2 x86_64 doesn't; aarch64 picks
                               it up via sysnr-gen.inc). Local
                               declaration keeps the call site portable. }

{$mode delphi}{$H+}{$modeswitch advancedrecords}{$pointermath on}
{ The iovec / msghdr / cmsghdr records below go straight onto the wire
  via sendmsg/recvmsg and have to match the Linux kernel ABI exactly
  (size_t-aligned fields, natural padding). FPC's default for non-i8086
  targets is natural-per-field alignment, which produces the same
  layout as C ABI for our field types -- but that's incidental.
  Setting C alignment explicitly states what we need. }
{$packrecords c}

{$ifndef LINUX}
  {$error waylandwire targets Linux: it uses Linux-specific syscall numbers and the wl_shm memfd_create path. Add a port for your OS here.}
{$endif}

interface

uses
  BaseUnix, Linux, Sockets, Syscall, Classes, SysUtils, ctypes;

const
  WL_HEADER_SIZE = 8;

var
  { Set to True to mirror libwayland's WAYLAND_DEBUG=client behaviour.
    Auto-enabled by the unit's initialization section if that env is set. }
  WaylandTrace: Boolean = False;

type
  TWlObjectId = LongWord;
  PWlObjectId = ^TWlObjectId;
  TWlFixed = LongInt;

  TWlMessage = record
    Sender: TWlObjectId;
    Opcode: Word;
    Payload: array of Byte;
  end;

  TWlReader = record
  public
    Buf: PByte;
    Len: SizeInt;
    Pos: SizeInt;
    procedure Init(ABuf: PByte; ALen: SizeInt); inline;
    function Eof: Boolean; inline;
    function ReadInt: LongInt; inline;
    function ReadUInt: LongWord; inline;
    function ReadFixed: TWlFixed; inline;
    function ReadObject: TWlObjectId; inline;
    function ReadNewId: TWlObjectId; inline;
    function ReadString: AnsiString;
    procedure ReadArray(out Data: PByte; out Size: LongWord);
  end;

  TWlWriter = record
  public
    Buf: array of Byte;
    procedure Reset; inline;
    procedure WriteRaw(const Data; Size: SizeInt);
    procedure WriteInt(V: LongInt); inline;
    procedure WriteUInt(V: LongWord); inline;
    procedure WriteFixed(V: TWlFixed); inline;
    procedure WriteObject(Id: TWlObjectId); inline;
    procedure WriteNewId(Id: TWlObjectId); inline;
    procedure WriteString(const S: AnsiString);
    procedure WriteNewIdGeneric(const Iface: AnsiString;
      Version: LongWord; Id: TWlObjectId);
  end;

  TWlConnection = class
  private
    FSock: cint;
    FOutBytes: array of Byte;
    FOutFds: array of cint;
    FInBytes: array of Byte;
    FInFill: SizeInt;
    FInFds: TFPList;
    procedure Recv;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const APath: AnsiString);
    procedure Disconnect;
    procedure Flush;
    function WaitForData(TimeoutMs: cint): Boolean;
    function NextMessage(out Msg: TWlMessage; Block: Boolean): Boolean;
    function PopFd: cint;
    procedure QueueRequest(Sender: TWlObjectId; Opcode: Word;
      Payload: Pointer; PayloadSize: SizeInt;
      const Fds: array of cint);
    property SocketFd: cint read FSock;
  end;

function WaylandSocketPath: AnsiString;
function AllocateAnonymousFile(Size: SizeInt): cint;
procedure Trace(const Tag, Body: AnsiString);

implementation

const
  AF_UNIX           = 1;
  SOCK_STREAM       = 1;
  SOCK_CLOEXEC      = $80000;
  SOL_SOCKET        = 1;
  SCM_RIGHTS        = 1;
  MSG_NOSIGNAL      = $4000;
  MSG_CMSG_CLOEXEC  = $40000000;
  POLLIN            = $0001;
  MFD_CLOEXEC       = $0001;

type
  Tiovec = record
    iov_base: Pointer;
    iov_len: csize_t;
  end;
  Piovec = ^Tiovec;

  Tmsghdr = record
    msg_name:       Pointer;
    msg_namelen:    cuint;
    msg_iov:        Piovec;
    msg_iovlen:     csize_t;
    msg_control:    Pointer;
    msg_controllen: csize_t;
    msg_flags:      cint;
  end;
  Pmsghdr = ^Tmsghdr;

  Tcmsghdr = record
    cmsg_len:   csize_t;
    cmsg_level: cint;
    cmsg_type:  cint;
  end;
  Pcmsghdr = ^Tcmsghdr;

  // pollfd / ppollfd come from BaseUnix.

  TSockAddrUn = record
    sun_family: cushort;
    sun_path:   array[0..107] of AnsiChar;
  end;

{ Linux memfd_create syscall number, gated on CPU. The LINUX-required
  guard at the top of the unit already rules out non-Linux OSes; here
  we pick the number for the kernel ABI on the target CPU.

  Whether FPC's own rtl/linux/<cpu>/sysnr.inc declares this constant
  depends on the FPC version Lazarus is being built with. Spot-checked
  against FPC 3.2.2:
    - x86_64 sysnr.inc            -- header "Syscall nrs for 2.4.18";
                                     no memfd_create.
    - arm sysnr.inc               -- no version header; no memfd_create.
    - aarch64 sysnr.inc           -- pulls sysnr-gen.inc, which has it.
  We declare it locally for the CPUs the rest of the unit can build on
  (the elseif list below mirrors which CPUs FPC's RTL has the other
  syscall_nr_* names this unit needs). Once FPC's per-CPU tables list
  memfd_create themselves, the local declaration becomes redundant.

  Numbers are from arch/<cpu>/include/uapi/asm/unistd*.h in the kernel
  tree (or the asm-generic table for CPUs that include it). The arm
  declaration uses syscall_nr_base to match FPC's arm convention --
  that constant is 0 on EABI (the modern ABI) and $900000 on the
  legacy OABI. }
const
{$if defined(CPUX86_64)}
  syscall_nr_memfd_create = 319;
{$elseif defined(CPUAARCH64)}
  syscall_nr_memfd_create = 279;
{$elseif defined(CPUARM)}
  syscall_nr_memfd_create = syscall_nr_base + 385;
{$else}
  {$error waylandwire: this CPU's FPC Linux RTL is not yet wired up here -- check that syscall_nr_sendmsg / _recvmsg / _socket are defined for it, then add a syscall_nr_memfd_create entry}
{$endif}

{ Socket-syscall ABI note (Linux): this unit issues sendmsg / recvmsg
  via Do_SysCall(syscall_nr_sendmsg / _recvmsg, ...). That requires
  FPC's rtl/linux/<cpu>/sysnr.inc to declare those direct numbers --
  spot-check against FPC 3.2.2: x86_64 declares them, arm declares
  them, aarch64 picks them up via sysnr-gen.inc.

  i386 doesn't: the Linux i386 ABI for sockets goes through the
  multiplexed socketcall(2) syscall (still libc-free, but with a
  different call shape -- Do_SysCall(syscall_nr_socketcall, sub_op, &args)).
  FPC's Sockets unit handles that transparently in its NEED_SOCKETCALL
  branch, which is why fpSocket / fpConnect work on i386 unmodified;
  we don't replicate it here because we'd need the same path for
  sendmsg / recvmsg, and FPC's Sockets doesn't expose those.
  powerpc64 in FPC 3.2.2 also doesn't declare the direct numbers.
  Both fall through to the error directive in the memfd_create CPU-gate above,
  so a build for those CPUs fails at the right spot with a clear
  message; nothing extra to do here. }

function CmsgAlign(L: SizeInt): SizeInt; inline;
begin
  Result := (L + SizeOf(csize_t) - 1) and (not (SizeOf(csize_t) - 1));
end;

var
  TraceT0: QWord = 0;

function MonotonicMicros: QWord;
var
  Ts: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @Ts);
  Result := QWord(Ts.tv_sec) * 1000000 + QWord(Ts.tv_nsec) div 1000;
end;

procedure Trace(const Tag, Body: AnsiString);
var
  US: QWord;
begin
  if not WaylandTrace then Exit;
  US := MonotonicMicros - TraceT0;
  WriteLn(StdErr, Format('[%4d.%06d] %s %s',
    [US div 1000000, US mod 1000000, Tag, Body]));
  Flush(StdErr);
end;

function WaylandSocketPath: AnsiString;
var
  RuntimeDir, Display: AnsiString;
begin
  Display := GetEnvironmentVariable('WAYLAND_DISPLAY');
  if Display = '' then
    Display := 'wayland-0';
  if (Length(Display) > 0) and (Display[1] = '/') then
    Exit(Display);
  RuntimeDir := GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if RuntimeDir = '' then
    raise Exception.Create('XDG_RUNTIME_DIR is not set');
  Result := RuntimeDir + '/' + Display;
end;

function AllocateAnonymousFile(Size: SizeInt): cint;
const
  MemfdName: array[0..14] of AnsiChar = 'wayland-buffer'#0;
begin
  Result := cint(Do_SysCall(syscall_nr_memfd_create,
                            TSysParam(@MemfdName[0]),
                            TSysParam(MFD_CLOEXEC)));
  if Result < 0 then
    raise Exception.CreateFmt('memfd_create failed (errno=%d)', [fpgeterrno]);
  if FpFtruncate(Result, Size) <> 0 then
  begin
    FpClose(Result);
    raise Exception.CreateFmt('ftruncate failed (errno=%d)', [fpgeterrno]);
  end;
end;

{ TWlReader }

procedure TWlReader.Init(ABuf: PByte; ALen: SizeInt);
begin
  Buf := ABuf;
  Len := ALen;
  Pos := 0;
end;

function TWlReader.Eof: Boolean;
begin
  Result := Pos >= Len;
end;

function TWlReader.ReadInt: LongInt;
begin
  if Pos + 4 > Len then
    raise Exception.Create('Wayland message underflow (int)');
  Move(Buf[Pos], Result, 4);
  Inc(Pos, 4);
end;

function TWlReader.ReadUInt: LongWord;
begin
  if Pos + 4 > Len then
    raise Exception.Create('Wayland message underflow (uint)');
  Move(Buf[Pos], Result, 4);
  Inc(Pos, 4);
end;

function TWlReader.ReadFixed: TWlFixed;
begin
  Result := ReadInt;
end;

function TWlReader.ReadObject: TWlObjectId;
begin
  Result := ReadUInt;
end;

function TWlReader.ReadNewId: TWlObjectId;
begin
  Result := ReadUInt;
end;

function TWlReader.ReadString: AnsiString;
var
  L, Padded: LongWord;
begin
  L := ReadUInt;
  if L = 0 then
    Exit('');
  Padded := (L + 3) and (not LongWord(3));
  if Pos + Padded > Len then
    raise Exception.Create('Wayland message underflow (string)');
  SetString(Result, PAnsiChar(@Buf[Pos]), L - 1);
  Inc(Pos, Padded);
end;

procedure TWlReader.ReadArray(out Data: PByte; out Size: LongWord);
var
  Padded: LongWord;
begin
  Size := ReadUInt;
  Padded := (Size + 3) and (not LongWord(3));
  if Pos + Padded > Len then
    raise Exception.Create('Wayland message underflow (array)');
  if Size > 0 then
    Data := @Buf[Pos]
  else
    Data := nil;
  Inc(Pos, Padded);
end;

{ TWlWriter }

procedure TWlWriter.Reset;
begin
  SetLength(Buf, 0);
end;

procedure TWlWriter.WriteRaw(const Data; Size: SizeInt);
var
  Old: SizeInt;
begin
  if Size <= 0 then Exit;
  Old := Length(Buf);
  SetLength(Buf, Old + Size);
  Move(Data, Buf[Old], Size);
end;

procedure TWlWriter.WriteInt(V: LongInt);
begin
  WriteRaw(V, 4);
end;

procedure TWlWriter.WriteUInt(V: LongWord);
begin
  WriteRaw(V, 4);
end;

procedure TWlWriter.WriteFixed(V: TWlFixed);
begin
  WriteRaw(V, 4);
end;

procedure TWlWriter.WriteObject(Id: TWlObjectId);
begin
  WriteUInt(Id);
end;

procedure TWlWriter.WriteNewId(Id: TWlObjectId);
begin
  WriteUInt(Id);
end;

procedure TWlWriter.WriteString(const S: AnsiString);
var
  L, Padded: LongWord;
  Old: SizeInt;
begin
  { Always send the empty string as length=1 plus a single null byte +
    padding, never as length=0. On the wire, length=0 is reserved for
    the "null string" case used by string args declared allow-null=true
    in the protocol XML; sending it for a regular non-null string arg
    (e.g. xdg_toplevel.set_title) earns a protocol error from the
    compositor. There's no API on TWlWriter to send the null variant
    yet -- add one if/when a request needs it. }
  L := LongWord(Length(S)) + 1;          { content length including '\0' }
  Padded := (L + 3) and (not LongWord(3));
  WriteUInt(L);
  Old := Length(Buf);
  SetLength(Buf, Old + Padded);
  FillChar(Buf[Old], Padded, 0);          { null terminator + padding }
  if Length(S) > 0 then
    Move(S[1], Buf[Old], Length(S));
end;

procedure TWlWriter.WriteNewIdGeneric(const Iface: AnsiString;
  Version: LongWord; Id: TWlObjectId);
begin
  WriteString(Iface);
  WriteUInt(Version);
  WriteNewId(Id);
end;

{ TWlConnection }

constructor TWlConnection.Create;
begin
  inherited Create;
  FSock := -1;
  FInFds := TFPList.Create;
end;

destructor TWlConnection.Destroy;
begin
  Disconnect;
  FInFds.Free;
  inherited Destroy;
end;

procedure TWlConnection.Connect(const APath: AnsiString);
var
  Addr: TSockAddrUn;
  PathLen: SizeInt;
begin
  FSock := fpSocket(AF_UNIX, SOCK_STREAM or SOCK_CLOEXEC, 0);
  if FSock < 0 then
    raise Exception.CreateFmt('socket() failed (errno=%d)', [SocketError]);
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sun_family := AF_UNIX;
  PathLen := Length(APath);
  if PathLen >= SizeOf(Addr.sun_path) then
    raise Exception.Create('Wayland socket path too long');
  Move(APath[1], Addr.sun_path[0], PathLen);
  if fpConnect(FSock, psockaddr(@Addr), SizeOf(Addr)) <> 0 then
    raise Exception.CreateFmt(
      'connect(%s) failed (errno=%d)', [APath, SocketError]);
end;

procedure TWlConnection.Disconnect;
var
  i: Integer;
begin
  if FSock >= 0 then
  begin
    FpClose(FSock);
    FSock := -1;
  end;
  if Assigned(FInFds) then
  begin
    for i := 0 to FInFds.Count - 1 do
      FpClose(cint(PtrUInt(FInFds[i])));
    FInFds.Clear;
  end;
end;

procedure TWlConnection.QueueRequest(Sender: TWlObjectId; Opcode: Word;
  Payload: Pointer; PayloadSize: SizeInt; const Fds: array of cint);
var
  Old, TotalLen: SizeInt;
  Header: array[0..1] of LongWord;
  i: Integer;
begin
  TotalLen := WL_HEADER_SIZE + PayloadSize;
  if TotalLen > $FFFF then
    raise Exception.Create('Wayland request too large for wire format');
  Header[0] := Sender;
  Header[1] := (LongWord(TotalLen) shl 16) or LongWord(Opcode);
  Old := Length(FOutBytes);
  SetLength(FOutBytes, Old + TotalLen);
  Move(Header[0], FOutBytes[Old], WL_HEADER_SIZE);
  if PayloadSize > 0 then
    Move(Payload^, FOutBytes[Old + WL_HEADER_SIZE], PayloadSize);
  for i := 0 to High(Fds) do
  begin
    SetLength(FOutFds, Length(FOutFds) + 1);
    FOutFds[High(FOutFds)] := Fds[i];
  end;
  if WaylandTrace then
    Trace('-> req',
      Format('obj=%u opcode=%u size=%u fds=%d',
        [Sender, Opcode, TotalLen, Length(Fds)]));
end;

procedure TWlConnection.Flush;
var
  Iov: Tiovec;
  Hdr: Tmsghdr;
  CMsgBuf: array[0..31] of QWord;  { 256 B, 8-byte aligned }
  CMsg: Pcmsghdr;
  N: clong;
  Sent, FdBytes, CMsgTotal: SizeInt;
  FdsAttached: Boolean;
begin
  if Length(FOutBytes) = 0 then Exit;
  Sent := 0;
  while Sent < Length(FOutBytes) do
  begin
    FdsAttached := False;
    Iov.iov_base := @FOutBytes[Sent];
    Iov.iov_len := Length(FOutBytes) - Sent;
    FillChar(Hdr, SizeOf(Hdr), 0);
    Hdr.msg_iov := @Iov;
    Hdr.msg_iovlen := 1;
    if Length(FOutFds) > 0 then
    begin
      FdBytes := Length(FOutFds) * SizeOf(cint);
      CMsgTotal := SizeOf(Tcmsghdr) + FdBytes;
      if CMsgTotal > SizeOf(CMsgBuf) then
        raise Exception.Create('Too many fds in single Wayland flush');
      CMsg := Pcmsghdr(@CMsgBuf[0]);
      CMsg^.cmsg_len := CMsgTotal;
      CMsg^.cmsg_level := SOL_SOCKET;
      CMsg^.cmsg_type := SCM_RIGHTS;
      Move(FOutFds[0], (PByte(CMsg) + SizeOf(Tcmsghdr))^, FdBytes);
      Hdr.msg_control := CMsg;
      Hdr.msg_controllen := CMsgTotal;
      FdsAttached := True;
    end;
    N := clong(Do_SysCall(syscall_nr_sendmsg,
                          TSysParam(FSock),
                          TSysParam(@Hdr),
                          TSysParam(MSG_NOSIGNAL)));
    if N < 0 then
      raise Exception.CreateFmt('sendmsg failed (errno=%d)', [fpgeterrno]);
    if N = 0 then
      raise Exception.Create('sendmsg returned 0');
    Inc(Sent, N);
    if FdsAttached then
      SetLength(FOutFds, 0);
  end;
  SetLength(FOutBytes, 0);
end;

function TWlConnection.WaitForData(TimeoutMs: cint): Boolean;
var
  Pfd: Tpollfd;
  R: cint;
begin
  Pfd.fd := FSock;
  Pfd.events := POLLIN;
  Pfd.revents := 0;
  R := FpPoll(@Pfd, 1, TimeoutMs);
  if R < 0 then
    raise Exception.CreateFmt('poll failed (errno=%d)', [fpgeterrno]);
  Result := (R > 0) and ((Pfd.revents and POLLIN) <> 0);
end;

procedure TWlConnection.Recv;
var
  Iov: Tiovec;
  Hdr: Tmsghdr;
  CMsgBuf: array[0..63] of QWord;  { 512 B, 8-byte aligned }
  CMsg: Pcmsghdr;
  N: clong;
  CapNeeded, FdBytes, FdCount, i: SizeInt;
  FdPtr: PByte;
  CMsgEnd: PtrUInt;
  FdVal: cint;
begin
  CapNeeded := FInFill + 4096;
  if Length(FInBytes) < CapNeeded then
    SetLength(FInBytes, CapNeeded);
  Iov.iov_base := @FInBytes[FInFill];
  Iov.iov_len := Length(FInBytes) - FInFill;
  FillChar(Hdr, SizeOf(Hdr), 0);
  Hdr.msg_iov := @Iov;
  Hdr.msg_iovlen := 1;
  Hdr.msg_control := @CMsgBuf[0];
  Hdr.msg_controllen := SizeOf(CMsgBuf);
  if WaylandTrace then Trace('  recv', 'blocking on recvmsg ...');
  N := clong(Do_SysCall(syscall_nr_recvmsg,
                        TSysParam(FSock),
                        TSysParam(@Hdr),
                        TSysParam(MSG_CMSG_CLOEXEC)));
  if N < 0 then
    raise Exception.CreateFmt('recvmsg failed (errno=%d)', [fpgeterrno]);
  if N = 0 then
    raise Exception.Create('Wayland connection closed by server');
  Inc(FInFill, N);
  if Hdr.msg_controllen >= SizeOf(Tcmsghdr) then
  begin
    CMsg := Pcmsghdr(@CMsgBuf[0]);
    CMsgEnd := PtrUInt(@CMsgBuf[0]) + Hdr.msg_controllen;
    while (PtrUInt(CMsg) + SizeOf(Tcmsghdr) <= CMsgEnd) and
          (CMsg^.cmsg_len >= SizeOf(Tcmsghdr)) and
          (PtrUInt(CMsg) + CMsg^.cmsg_len <= CMsgEnd) do
    begin
      if (CMsg^.cmsg_level = SOL_SOCKET) and
         (CMsg^.cmsg_type = SCM_RIGHTS) then
      begin
        FdBytes := CMsg^.cmsg_len - SizeOf(Tcmsghdr);
        FdCount := FdBytes div SizeOf(cint);
        FdPtr := PByte(CMsg) + SizeOf(Tcmsghdr);
        for i := 0 to FdCount - 1 do
        begin
          Move(FdPtr[i * SizeOf(cint)], FdVal, SizeOf(cint));
          FInFds.Add(Pointer(PtrInt(FdVal)));
        end;
      end;
      CMsg := Pcmsghdr(PByte(CMsg) + CmsgAlign(CMsg^.cmsg_len));
    end;
  end;
end;

function TWlConnection.NextMessage(out Msg: TWlMessage; Block: Boolean): Boolean;
var
  Header: array[0..1] of LongWord;
  TotalLen, PayloadLen: SizeInt;
begin
  Result := False;
  while True do
  begin
    if FInFill >= WL_HEADER_SIZE then
    begin
      Move(FInBytes[0], Header[0], WL_HEADER_SIZE);
      TotalLen := (Header[1] shr 16) and $FFFF;
      if TotalLen < WL_HEADER_SIZE then
        raise Exception.Create('Invalid Wayland message size');
      if FInFill >= TotalLen then
      begin
        PayloadLen := TotalLen - WL_HEADER_SIZE;
        Msg.Sender := Header[0];
        Msg.Opcode := Header[1] and $FFFF;
        SetLength(Msg.Payload, PayloadLen);
        if PayloadLen > 0 then
          Move(FInBytes[WL_HEADER_SIZE], Msg.Payload[0], PayloadLen);
        if FInFill > TotalLen then
          Move(FInBytes[TotalLen], FInBytes[0], FInFill - TotalLen);
        Dec(FInFill, TotalLen);
        if WaylandTrace then
          Trace('<- evt',
            Format('obj=%u opcode=%u size=%u',
              [Msg.Sender, Msg.Opcode, TotalLen]));
        Exit(True);
      end;
    end;
    if Block then
      Recv
    else
    begin
      { Non-blocking: only recv if poll says data is ready, otherwise
        bail out. Without this check, callers that loop on
        NextMessage(False) would loop without making progress whenever the socket has
        unread bytes that haven't yet formed a complete message -- or,
        worse, have already been consumed: poll keeps returning POLLIN
        on a drained socket only until something actually reads it. }
      if WaitForData(0) then
        Recv
      else
        Exit(False);
    end;
  end;
end;

function TWlConnection.PopFd: cint;
begin
  if FInFds.Count = 0 then
    raise Exception.Create('No fd available to pop');
  Result := cint(PtrUInt(FInFds[0]));
  FInFds.Delete(0);
end;

initialization
  TraceT0 := MonotonicMicros;
  WaylandTrace := Pos('client', GetEnvironmentVariable('WAYLAND_DEBUG')) > 0;

end.
