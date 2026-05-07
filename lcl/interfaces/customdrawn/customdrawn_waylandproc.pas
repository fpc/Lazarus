unit customdrawn_waylandproc;

{ Helper unit for the LCL CustomDrawn Wayland host: defines per-window
  state and a few utilities. Mirrors customdrawn_x11proc. }

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

{$ifndef LINUX}
  {$error customdrawn_waylandproc targets Linux: it uses Linux-specific timerfd syscalls and the kernel itimerspec ABI.}
{$endif}

{ Pin C alignment for TWLTimerSpec below -- the layout has to match the
  Linux kernel's struct itimerspec exactly because we hand it to
  timerfd_settime via Do_SysCall. }
{$packrecords c}

interface

uses
  Types, Classes, SysUtils, ctypes,
  BaseUnix, Unix, Linux, Syscall,
  fpimage, fpcanvas,
  IntfGraphics, lazcanvas,
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc,
  customdrawnproc,
  waylandwire, waylandcore, xdgshell;

const
  { wl_seat capabilities bitmask. }
  WL_SEAT_CAPABILITY_POINTER  = 1;
  WL_SEAT_CAPABILITY_KEYBOARD = 2;
  WL_SEAT_CAPABILITY_TOUCH    = 4;

  { Linux evdev pointer button codes (linux/input-event-codes.h). }
  BTN_LEFT   = $110;
  BTN_RIGHT  = $111;
  BTN_MIDDLE = $112;

  { wl_pointer button state. }
  WL_POINTER_BUTTON_RELEASED = 0;
  WL_POINTER_BUTTON_PRESSED  = 1;

  { wl_keyboard key state. }
  WL_KEYBOARD_KEY_RELEASED = 0;
  WL_KEYBOARD_KEY_PRESSED  = 1;

  { wl_pointer.axis. }
  WL_POINTER_AXIS_VERTICAL_SCROLL   = 0;
  WL_POINTER_AXIS_HORIZONTAL_SCROLL = 1;

  { timerfd_create / timerfd_settime flag constants. The syscall
    numbers themselves are CPU-gated below; FPC's RTL declares them
    for some CPUs and not others, so we declare locally for every CPU
    we build on so the call site stays uniform. }
  TFD_CLOEXEC                  = $80000;

const
{$if defined(CPUX86_64)}
  syscall_nr_timerfd_create  = 283;
  syscall_nr_timerfd_settime = 286;
{$elseif defined(CPUAARCH64)}
  syscall_nr_timerfd_create  = 85;
  syscall_nr_timerfd_settime = 86;
{$elseif defined(CPUARM)}
  syscall_nr_timerfd_create  = syscall_nr_base + 350;
  syscall_nr_timerfd_settime = syscall_nr_base + 353;
{$else}
  {$error customdrawn_waylandproc: define Linux syscall_nr_timerfd_create / _timerfd_settime for this CPU}
{$endif}

type
  TWLTimerSpec = record
    it_interval_sec:  clong;
    it_interval_nsec: clong;
    it_value_sec:     clong;
    it_value_nsec:    clong;
  end;

function WLTimerfdCreate(ClockId, Flags: cint): cint;
function WLTimerfdSettime(Fd, Flags: cint;
  const NewSpec: TWLTimerSpec; OldSpec: Pointer): cint;

type

  { TWaylandWindowInfo
    One per CustomDrawn form. Owns the Wayland surface, xdg_surface,
    xdg_toplevel and the wl_shm pool/buffer used to push pixels. }

  TWaylandWindowInfo = class(TCDForm)
  public
    Surface:        TWaylandSurface;
    XdgSurf:        TXdgSurface;
    Toplevel:       TXdgToplevel;       { non-nil iff this WI is a toplevel; mutually exclusive with Popup }
    Popup:          TXdgPopup;          { non-nil iff this WI is an xdg_popup }
    Title:          String;             { last title set through the widgetset; Wayland cannot query it back }
    PopupPending:   Boolean;            { popup-class form whose role assignment is deferred to DoShowHide }
    ParentWI:       TWaylandWindowInfo; { popup's anchor parent, retained for grab + position math }
    Pool:           TWaylandShmPool;
    PoolBuffer:     TWaylandBuffer;
    PoolData:       Pointer;
    PoolSize:       PtrInt;
    PoolWidth:      LongInt;
    PoolHeight:     LongInt;
    PoolStride:     LongInt;
    HasConfigure:   Boolean;
    PendingDraw:    Boolean;
    ShouldClose:    Boolean;
    LastConfigSerial: LongWord;
  end;

  { Per-timer record. Backed by a Linux timerfd; the host's poll loop
    treats each one as another fd and fires Func when it becomes
    readable. }
  TWLTimerCallback = procedure of object;

  TWLTimer = class
  public
    Fd:        cint;
    IntervalMs: LongInt;
    Func:      TWLTimerCallback;
    constructor Create(AFd: cint; AIntervalMs: LongInt; AFunc: TWLTimerCallback);
    destructor Destroy; override;
  end;

implementation

function WLTimerfdCreate(ClockId, Flags: cint): cint;
begin
  Result := cint(do_syscall(syscall_nr_timerfd_create,
                            TSysParam(ClockId), TSysParam(Flags)));
end;

function WLTimerfdSettime(Fd, Flags: cint;
  const NewSpec: TWLTimerSpec; OldSpec: Pointer): cint;
begin
  Result := cint(do_syscall(syscall_nr_timerfd_settime,
                            TSysParam(Fd), TSysParam(Flags),
                            TSysParam(@NewSpec), TSysParam(OldSpec)));
end;

constructor TWLTimer.Create(AFd: cint; AIntervalMs: LongInt;
  AFunc: TWLTimerCallback);
begin
  inherited Create;
  Fd := AFd;
  IntervalMs := AIntervalMs;
  Func := AFunc;
end;

destructor TWLTimer.Destroy;
begin
  if Fd >= 0 then FpClose(Fd);
  inherited Destroy;
end;

end.
