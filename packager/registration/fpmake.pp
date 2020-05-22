{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for FCL 1.0.1

   This file was generated on 22-05-20
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
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('fcl');
    P.Version:='1.0.1-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus';
    P.License:='modified LGPL-2';
    P.Description:='The FCL - FreePascal Component Library provides the base classes for object pascal.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('fcl-process');
    D := P.Dependencies.Add('fcl-db');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('fcllaz.pas');
    D := T.Dependencies.AddUnit('LazarusPackageIntf');
    T := P.Targets.AddImplicitUnit('lazaruspackageintf.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('FCL.compiled');
    P.InstallFiles.Add('FCL.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_FCL('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
