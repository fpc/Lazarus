{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for FCL 1.0.1

   This file was generated on 29-12-18
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_FCL(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('fcl');
    P.Version:='1.0.1';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('fcl-process');
    P.Dependencies.Add('fcl-db');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('fcllaz.pas');
    t.Dependencies.AddUnit('LazarusPackageIntf');
    T := P.Targets.AddImplicitUnit('lazaruspackageintf.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('FCL.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_FCL('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
