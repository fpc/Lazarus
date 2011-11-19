program lazarusfppkg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, fppkg_mainfrm, laz_pkgcommands, laz_pkgrepos, laz_pkghandler,
fppkg_lpk;

{$R *.res}

begin
  Application.Title:='Lazarus Package Manager';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFppkgForm, FppkgForm);
  Application.Run;
end.

