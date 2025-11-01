{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeConfig 1.0

   This file was generated on 01.11.2025
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
    P.Description:='-- This package is part of the IDE --'#10'This package does not guarantee any particular interface/API. Files are maintained for the use by the IDE.'#10''#10'Files in this package are for the main configuration of the IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('codetools');
    D := P.Dependencies.Add('debuggerintf');
    D := P.Dependencies.Add('buildintf');
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
    t.Dependencies.AddUnit('compiler');
    t.Dependencies.AddUnit('compileroptions');
    t.Dependencies.AddUnit('compoptsmodes');
    t.Dependencies.AddUnit('coolbaroptions');
    t.Dependencies.AddUnit('diffpatch');
    t.Dependencies.AddUnit('editdefinetree');
    t.Dependencies.AddUnit('editortoolbaroptions');
    t.Dependencies.AddUnit('environmentopts');
    t.Dependencies.AddUnit('etfpcmsgfilepool');
    t.Dependencies.AddUnit('etmakemsgparser');
    t.Dependencies.AddUnit('fppkghelper');
    t.Dependencies.AddUnit('idecmdline');
    t.Dependencies.AddUnit('ideconfigpcktest');
    t.Dependencies.AddUnit('ideconfstrconsts');
    t.Dependencies.AddUnit('ideguicmdline');
    t.Dependencies.AddUnit('ideoptiondefs');
    t.Dependencies.AddUnit('ideprocs');
    t.Dependencies.AddUnit('idexmlconfigprocs');
    t.Dependencies.AddUnit('lazconf');
    t.Dependencies.AddUnit('modematrixopts');
    t.Dependencies.AddUnit('parsedcompileropts');
    t.Dependencies.AddUnit('projectbuildmode');
    t.Dependencies.AddUnit('projpackcommon');
    t.Dependencies.AddUnit('recentlistprocs');
    t.Dependencies.AddUnit('searchpathprocs');
    t.Dependencies.AddUnit('toolbaroptionsbase');
    t.Dependencies.AddUnit('transfermacros');

    T:=P.Targets.AddUnit('compiler.pp');
    T:=P.Targets.AddUnit('compileroptions.pp');
    T:=P.Targets.AddUnit('compoptsmodes.pas');
    T:=P.Targets.AddUnit('coolbaroptions.pas');
    T:=P.Targets.AddUnit('diffpatch.pas');
    T:=P.Targets.AddUnit('editdefinetree.pas');
    T:=P.Targets.AddUnit('editortoolbaroptions.pas');
    T:=P.Targets.AddUnit('environmentopts.pp');
    T:=P.Targets.AddUnit('etfpcmsgfilepool.pas');
    T:=P.Targets.AddUnit('etmakemsgparser.pas');
    T:=P.Targets.AddUnit('fppkghelper.pas');
    T:=P.Targets.AddUnit('idecmdline.pas');
    T:=P.Targets.AddUnit('ideconfigpcktest.pas');
    T:=P.Targets.AddUnit('ideconfstrconsts.pas');
    T:=P.Targets.AddUnit('ideguicmdline.pas');
    T:=P.Targets.AddUnit('ideoptiondefs.pas');
    T:=P.Targets.AddUnit('ideprocs.pp');
    T:=P.Targets.AddUnit('idexmlconfigprocs.pas');
    T:=P.Targets.AddUnit('lazconf.pp');
    T:=P.Targets.AddUnit('modematrixopts.pas');
    T:=P.Targets.AddUnit('parsedcompileropts.pas');
    T:=P.Targets.AddUnit('projectbuildmode.pas');
    T:=P.Targets.AddUnit('projpackcommon.pas');
    T:=P.Targets.AddUnit('recentlistprocs.pas');
    T:=P.Targets.AddUnit('searchpathprocs.pas');
    T:=P.Targets.AddUnit('toolbaroptionsbase.pas');
    T:=P.Targets.AddUnit('transfermacros.pp');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('ideconfig.compiled');
    P.InstallFiles.Add('ideconfig.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeConfig('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
