unit FpDbgLinuxExtra;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  BaseUnix,
{$ifdef linux}{$ifNdef FPDEBUG_USE_LIBC}
  SysCall,
{$endif}{$endif}
  SysUtils;

{$ifdef darwin}
  {$DEFINE FPDEBUG_USE_LIBC}
{$else}
  {$IF not declared(Do_SysCall)}
    {$IFDEF SUNOS} {$DEFINE FPDEBUG_USE_LIBC} {$ENDIF}
  {$ENDIF}
{$endif}

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
  PTRACE_DETACH                                = 17;
  PTRACE_SEIZE                                 = $4206;

  RIP                                          = 16;

function fpPTrace(ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer): PtrInt;

implementation

{$ifdef FPDEBUG_USE_LIBC}
(* **   ptrace   ** *)

Function ptrace(ptrace_request: cInt; pid: TPid; addr:pointer; data:pointer): cint; cdecl; external clib name 'ptrace';

function fpPTrace(ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer): PtrInt; inline;
begin
  result := ptrace(ptrace_request, pid, addr, data);
end;

{$else} // FPDEBUG_USE_LIBC
{$IF not declared(Do_SysCall)}
(* **   ptrace not available   ** *)

function fpPTrace(ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer): PtrInt; inline;
begin
  raise Exception.Create('not supported');
end;

{$else} // not declared(Do_SysCall)
(* **   Use Do_SysCall   ** *)

const
{$ifdef cpux86_64}
  syscall_nr_ptrace                            = 101;
{$else}
  syscall_nr_ptrace                            = 26;
{$endif}

function fpPTrace(ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer): PtrInt;
var
  res : TSysResult;
  ret : PtrInt;
begin
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
end;

{$endif} // declared(Do_SysCall)
{$endif FPDEBUG_USE_LIBC}

end.

