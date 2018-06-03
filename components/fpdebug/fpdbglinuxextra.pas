unit FpDbgLinuxExtra;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  BaseUnix,
  SysUtils;

const
  PTRACE_TRACEME                               = 0;
  PTRACE_PEEKTEXT                              = 1;
  PTRACE_PEEKDATA                              = 2;
  PTRACE_PEEKUSR                               = 3;
  PTRACE_POKETEXT                              = 4;
  PTRACE_POKEDATA                              = 5;
  PTRACE_POKEUSR                               = 6;
  PTRACE_CONT                                  = 7;
  PTRACE_KILL                                  = 8;
  PTRACE_SINGLESTEP                            = 9;
{$ifdef linux}
  PTRACE_GETREGS                               = 12;
  PTRACE_SETREGS                               = 13;
  PTRACE_GETFPREGS                             = 14;
  PTRACE_SETFPREGS                             = 15;
  PTRACE_SETOPTIONS                            = $4200;
  PTRACE_GETEVENTMSG                           = $4201;
  PTRACE_GETREGSET                             = $4204;
  PTRACE_SETREGSET                             = $4205;

  PTRACE_EVENT_FORK                            = 1;
  PTRACE_EVENT_VFORK                           = 2;
  PTRACE_EVENT_CLONE                           = 3;
  PTRACE_EVENT_EXEC                            = 4;
  PTRACE_EVENT_VFORK_DONE                      = 5;
  PTRACE_EVENT_EXIT                            = 6;
  PTRACE_EVENT_SECCOMP                         = 7;
  PTRACE_EVENT_STOP                            = 128;

  PTRACE_O_TRACEFORK                           = 1 << PTRACE_EVENT_FORK;
  PTRACE_O_TRACEVFORK                          = 1 << PTRACE_EVENT_VFORK;
  PTRACE_O_TRACECLONE                          = 1 << PTRACE_EVENT_CLONE;
{$endif linux}
  PTRACE_ATTACH                                = 16;

  RIP                                          = 16;

function fpPTrace(ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer): PtrInt;

implementation

type
  // all platforms, cint=32-bit.
  // On platforms with off_t =64-bit, people should
  // use int64, and typecast all calls that don't
  // return off_t to cint.
{$ifdef cpux86_64}
  TSysResult = int64;
  TSysParam  = int64;
{$else}
  TSysResult = cint32;
  TSysParam  = cint32;
{$endif cpux86_64}

{$ifdef darwin}
Function ptrace(ptrace_request: cInt; pid: TPid; addr:pointer; data:pointer): cint; cdecl; external clib name 'ptrace';
{$endif darwin}
{$ifdef linux}
function Do_SysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; {$ifdef cpui386}register;{$endif} external name 'FPC_SYSCALL4';

const
{$ifdef cpux86_64}
  syscall_nr_ptrace                            = 101;
{$else}
  syscall_nr_ptrace                            = 26;
{$endif}

{$endif linux}

function fpPTrace(ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer): PtrInt;
{$ifdef linux}
var
  res : TSysResult;
  ret : PtrInt;
{$endif linux}
begin
{$ifdef darwin}
  result := ptrace(ptrace_request, pid, addr, data);
{$endif}
{$ifdef linux}
  if (ptrace_request > 0) and (ptrace_request < 4) then
    data := @ret;

  res := do_syscall(TSysParam(syscall_nr_ptrace), TSysParam(ptrace_request), TSysParam(pid), TSysParam(addr), TSysParam(data));
  if (res >= 0) and (ptrace_request > 0) and (ptrace_request < 4) then
    begin
    errno:=0;
    result := ret;
    end
  else
    result := res;
{$endif linux}
end;

end.

