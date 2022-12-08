{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeConfig 1.0

   This file was generated on 08/12/2022
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IdeConfig(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('ideconfig');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='GPLv2';
    P.Description:='-- This package is part of the IDE --'#13#10'This package does not guarantee any particular interface/API. Files are maintained for the use by the IDE.'#13#10''#13#10'Files in this package are for the main configuration of the IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('buildintf');
    D := P.Dependencies.Add('lclbase');
    D := P.Dependencies.Add('lazutils');
    D := P.Dependencies.Add('codetools');
    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.IncludePath.Add('include');
    P.IncludePath.Add('include/$(OS)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('ideconfig.pas');
    t.Dependencies.AddUnit('searchpathprocs');
    t.Dependencies.AddUnit('recentlistprocs');
    t.Dependencies.AddUnit('idexmlconfigprocs');
    t.Dependencies.AddUnit('lazconf');
    t.Dependencies.AddUnit('transfermacrosintf');
    t.Dependencies.AddUnit('ideoptiondefs');
    t.Dependencies.AddUnit('modematrixopts');

    T:=P.Targets.AddUnit('searchpathprocs.pas');
    T:=P.Targets.AddUnit('recentlistprocs.pas');
    T:=P.Targets.AddUnit('idexmlconfigprocs.pas');
    T:=P.Targets.AddUnit('lazconf.pp');
    T:=P.Targets.AddUnit('transfermacrosintf.pas');
    T:=P.Targets.AddUnit('ideoptiondefs.pas');
    T:=P.Targets.AddUnit('modematrixopts.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('IdeConfig.compiled');
    P.InstallFiles.Add('IdeConfig.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeConfig('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
