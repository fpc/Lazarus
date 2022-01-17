{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for AnchorDockingDsgn 1.0

   This file was generated on 17.01.2022
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_AnchorDockingDsgn(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('anchordockingdsgn');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Mattias Gaertner mattias@freepascal.org';
    P.License:='GPL2 as the IDE';
    P.Description:='Installs the anchor docking manager in the IDE';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('ideintf');
    D := P.Dependencies.Add('anchordocking');
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
    T:=P.Targets.AddUnit('anchordockingdsgn.pas');
    t.Dependencies.AddUnit('registeranchordocking');
    t.Dependencies.AddUnit('anchordesktopoptions');

    T:=P.Targets.AddUnit('registeranchordocking.pas');
    T:=P.Targets.AddUnit('anchordesktopoptions.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('AnchorDockingDsgn.compiled');
    P.InstallFiles.Add('AnchorDockingDsgn.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_AnchorDockingDsgn('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
