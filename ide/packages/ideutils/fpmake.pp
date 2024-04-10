{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeUtilsPkg 1.0

   This file was generated on 11/04/2024
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IdeUtilsPkg(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('ideutilspkg');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='GPLv2';
    P.Description:='-- This package is part of the IDE --'#13#10'This package does not guarantee any particular interface/API. Files are maintained for the use by the IDE.'#13#10''#13#10'Files in this package are for the main configuration of the IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('ideintf');
    D := P.Dependencies.Add('codetools');
    D := P.Dependencies.Add('lclbase');
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
    T:=P.Targets.AddUnit('ideutilspkg.pas');
    t.Dependencies.AddUnit('dialogprocs');
    t.Dependencies.AddUnit('idetranslations');
    t.Dependencies.AddUnit('ideutilspkgstrconsts');

    T:=P.Targets.AddUnit('dialogprocs.pas');
    T:=P.Targets.AddUnit('idetranslations.pas');
    T:=P.Targets.AddUnit('ideutilspkgstrconsts.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('ideutilspkg.compiled');
    P.InstallFiles.Add('ideutilspkg.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeUtilsPkg('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
