unit laz_pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  pkghandler;

implementation

uses
  pkgglobals, pkgoptions, pkgdownload, pkgrepos, laz_pkgrepos, pkgfpmake;

type
  { TCommandLazListPackages }

  TCommandLazListPackages = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

procedure TCommandLazListPackages.Execute;
begin
  Laz_ListPackages;
end;

initialization
  RegisterPkgHandler('laz_list',TCommandLazListPackages);
end.
