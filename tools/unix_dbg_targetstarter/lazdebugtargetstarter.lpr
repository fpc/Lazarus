program LazDebugTargetStarter;
{$mode objfpc}{$H+}

uses
  SysUtils, LazDebuggerStarterUtils, BaseUnix, unix;

var
  FD: LongInt;
  s, n: string;
  i: integer;

begin
  if argc < 3 then
    exit;

  n := CreatePipe;
  if n = '' then
    exit;

  FD := OpenWritePipe(argv[1]);
  if FD = -1 then begin
    RemovePipe(n);
    exit;
  end;

  s := IntToStr(GetProcessID) + #13
       + n + #13;
  if WritePipe(FD, s) <= 0 then begin
    Fpclose(FD);
    RemovePipe(n);
    exit;
  end;
  ClosePipe(FD);

  FD := OpenReadPipe(n);
  if FD = -1 then begin
    RemovePipe(n);
    exit;
  end;

  s := ReadPipe(FD, 2 * 60 * 1000); // wait 2 minutes
  Fpclose(FD);
  RemovePipe(n);


  fpexecv(argv[2], @argv[2]); // name and argument of the target
end.

