program ArgVPrg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  Classes, sysutils
  { you can add units after this };

var
  i: Integer;
  p: PChar;
  S: String;
  {$IFDEF WINDOWS}
  w: LPWSTR;
  {$ENDIF}



begin
  {$IFDEF WINDOWS}
  w := GetCommandLineW;
  s := '';
  for i := 0 to strlen(w) - 1 do
    s := s + IntToHex(ord(w[i]), 4);
  {$ELSE}
  s := '';
  for i := 0 to argc - 1 do begin
    p := (argv+i)^;
    while p^ <> #0 do begin
      s := s + IntToHex(ord(p^), 2);
      inc(p);
    end;
    s := s + ' ';
  end;
  {$ENDIF}
  Freemem(GetMem(1));
  Freemem(GetMem(1)); // line 40 breakpoint
  Freemem(GetMem(1));
end.

