{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeProject 1.0

   This file was generated on 10.03.2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IdeProject(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('ideproject');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='GPLv2';
    P.Description:='-- This package is part of the IDE --'#10'This package does not guarantee any particular interface/API. Files are maintained for the use by the IDE.'#10''#10'Files in this package are for the main configuration of the IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('idepackager');
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
    T:=P.Targets.AddUnit('ideproject.pas');
    t.Dependencies.AddUnit('runparamoptions');
    t.Dependencies.AddUnit('projecticon');
    t.Dependencies.AddUnit('w32manifest');
    t.Dependencies.AddUnit('w32versioninfo');
    t.Dependencies.AddUnit('projectuserresources');
    t.Dependencies.AddUnit('ideprojectstrconsts');
    t.Dependencies.AddUnit('projectresources');
    t.Dependencies.AddUnit('project');
    t.Dependencies.AddUnit('projectdefs');

    T:=P.Targets.AddUnit('runparamoptions.pas');
    T:=P.Targets.AddUnit('projecticon.pas');
    T:=P.Targets.AddUnit('w32manifest.pas');
    T:=P.Targets.AddUnit('w32versioninfo.pas');
    T:=P.Targets.AddUnit('projectuserresources.pas');
    T:=P.Targets.AddUnit('ideprojectstrconsts.pas');
    T:=P.Targets.AddUnit('projectresources.pas');
    T:=P.Targets.AddUnit('project.pp');
    T:=P.Targets.AddUnit('projectdefs.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('ideproject.compiled');
    P.InstallFiles.Add('ideproject.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeProject('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
