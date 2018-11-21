program ArgVPrg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils
  { you can add units after this };

var
  i: Integer;
  p: PChar;
  s: String;
begin
  s := '';
  for i := 1 to argc - 1 do begin
    p := (argv+i)^;
    while p^ <> #0 do begin
      s := s + IntToHex(ord(p^), 2);
      inc(p);
    end;
    s := s + ' ';
  end;
  Freemem(GetMem(1));
end.

