program lazarusfppkg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, fppkg_mainfrm, laz_pkgrepos,
  pkgDownload,
  fppkg_lpk,
  pkgfpmake,
  pkgfphttp;

{$R *.res}

begin
  Application.Title := 'Lazarus package manager';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFppkgForm, FppkgForm);
  Application.Run;
end.

