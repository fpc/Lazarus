{
  This unit contains all access functions to libc (unix like OS like
  Linux, Mac OS X, BSD), which are needed by the IDE, but are not provided
  by FPC units SysUtils, Unix and BaseUnix.

  Before adding stuff here always check if fpc units Unix or BaseUnix already
  contains corresponding functions.
  Always use types from the ctypes unit.
}
unit IDEMiniLibC;

{$mode objfpc}{$H+}

{$packrecords c}

interface

uses
  ctypes
  //,libc
  ;

const
  clib = 'c';
  InvalHandle = -1;
  ICANON    = $0000002;
  ECHO      = $0000008;
  VMIN = 6;
  VTIME = 5;
  TCSANOW = 0;
  F_DUPFD   = 0;
  F_GETFD   = 1;
  F_SETFD   = 2;
  F_GETFL   = 3;
  F_SETFL   = 4;
  {$ifdef cpusparc}
  O_NONBLOCK = $4000;
  {$else}
  O_NONBLOCK = &04000;
  {$endif}
  EINTR = 4;
  NCCS = 32;

type
  error_t = cint;
  tcflag_t = cuint;
  cc_t = cchar;
  speed_t = cuint;
  size_t = cuint;
  ssize_t = cint;

  Ptermios = ^termios;
  termios = record
    c_iflag : tcflag_t;
    c_oflag : tcflag_t;
    c_cflag : tcflag_t;
    c_lflag : tcflag_t;
    c_line : cc_t;
    c_cc : array[0..(NCCS)-1] of cc_t;
    c_ispeed : speed_t;
    c_ospeed : speed_t;
  end;

function tcgetattr(__fd:cint; __termios_p: Ptermios):cint;cdecl;external clib name 'tcgetattr';
function tcsetattr(__fd:cint; __optional_actions:cint; __termios_p: Ptermios):cint;cdecl;external clib name 'tcsetattr';
function __read(Handle: cint; var Buffer; Count: size_t): ssize_t; cdecl;external clib name 'read';
function __write(Handle: cint; const Buffer; Count: size_t): ssize_t; cdecl;external clib name 'write';
function __close(Handle: cint): cint; cdecl;external clib name 'close';
function getpt:cint;cdecl;external clib name 'getpt';
function grantpt(__fd:cint):cint;cdecl;external clib name 'grantpt';
function unlockpt(__fd:cint):cint;cdecl;external clib name 'unlockpt';
function ptsname_r(__fd:cint; __buf:Pchar; __buflen:size_t):cint;cdecl;external clib name 'ptsname_r';
function fcntl(Handle: cint; Command: cint; Arg: clong): cint; cdecl;external clib name 'fcntl';

implementation

end.

