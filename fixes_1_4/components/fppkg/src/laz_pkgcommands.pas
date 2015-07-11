unit laz_pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler,laz_pkghandler;

implementation

uses
  zipper,
  pkgmessages, pkgglobals, pkgoptions, pkgdownload, pkgrepos, fprepos,
  laz_pkgrepos, pkgfpmake;

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
