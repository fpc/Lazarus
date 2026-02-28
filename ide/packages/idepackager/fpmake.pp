{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdePackager 1.0

   This file was generated on 28.02.2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IdePackager(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('idepackager');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='GPLv2';
    P.Description:='-- This package is part of the IDE --'#10'This package does not guarantee any particular interface/API. Files are maintained for the use by the IDE.'#10''#10'Files in this package are for the main configuration of the IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('ideutilspkg');
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
    T:=P.Targets.AddUnit('idepackager.pas');
    t.Dependencies.AddUnit('basepkgmanager');
    t.Dependencies.AddUnit('editablepackage');
    t.Dependencies.AddUnit('idepackagerstrconsts');
    t.Dependencies.AddUnit('packagedefs');
    t.Dependencies.AddUnit('packagelinks');
    t.Dependencies.AddUnit('packagesystem');
    t.Dependencies.AddUnit('pkgsysbasepkgs');

    T:=P.Targets.AddUnit('basepkgmanager.pas');
    T:=P.Targets.AddUnit('editablepackage.pas');
    T:=P.Targets.AddUnit('idepackagerstrconsts.pas');
    T:=P.Targets.AddUnit('packagedefs.pas');
    T:=P.Targets.AddUnit('packagelinks.pas');
    T:=P.Targets.AddUnit('packagesystem.pas');
    T:=P.Targets.AddUnit('pkgsysbasepkgs.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('idepackager.compiled');
    P.InstallFiles.Add('idepackager.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdePackager('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
