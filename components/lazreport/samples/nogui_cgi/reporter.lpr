program reporter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  {$ifdef CONSOLE_APP}
  conApp,
  {$endif}
  {$ifdef HTTP_SERVER}
  fpHttpApp,
  {$endif}
  {$ifdef CGI}
  fpCGI,
  {$endif}
  // LCL Units
  dbflaz, lazreportpdfexport,
  //
  UnitWeb;

{$R *.res}

begin
  {$IFNDEF CONSOLE_APP}
  Application.LegacyRouting := true;
  {$ENDIF}
  {$ifdef HTTP_SERVER}
  Application.Port := 8080;
  {$endif}
  Application.Title:='cgiproject1';
  Application.Initialize;
  Application.Run;
end.

