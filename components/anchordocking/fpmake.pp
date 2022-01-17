{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for AnchorDocking 1.0

   This file was generated on 17.01.2022
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_AnchorDocking(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('anchordocking');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Mattias Gaertner mattias@freepascal.org';
    P.License:='modified LGPL-2 like LCL';
    P.Description:='Docking manager for LCL aplications';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('lcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('anchordockpkg.pas');
    t.Dependencies.AddUnit('anchordocking');
    t.Dependencies.AddUnit('anchordockstorage');
    t.Dependencies.AddUnit('anchordockstr');
    t.Dependencies.AddUnit('anchordockoptionsdlg');
    t.Dependencies.AddUnit('anchordockpanel');

    T:=P.Targets.AddUnit('anchordocking.pas');
    T:=P.Targets.AddUnit('anchordockstorage.pas');
    T:=P.Targets.AddUnit('anchordockstr.pas');
    T:=P.Targets.AddUnit('anchordockoptionsdlg.pas');
    T:=P.Targets.AddUnit('anchordockpanel.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('AnchorDocking.compiled');
    P.InstallFiles.Add('AnchorDocking.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_AnchorDocking('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
