{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for IdeHelpUtils 1.0

   This file was generated on 26/07/2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_IdeHelpUtils(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('idehelputils');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='GPLv2';
    P.Description:='-- This package is part of the IDE --'#13#10'This package does not guarantee any particular interface/API. Files are maintained for the use by the IDE.'#13#10''#13#10'Files in this package are for the main configuration of the IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('idepackager');
    D := P.Dependencies.Add('synedit');
    D := P.Dependencies.Add('ideintf');
    D := P.Dependencies.Add('lclbase');
    D := P.Dependencies.Add('ideconfig');
    D := P.Dependencies.Add('fcl');
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
    T:=P.Targets.AddUnit('idehelputils.pas');
    t.Dependencies.AddUnit('codehelp');
    t.Dependencies.AddUnit('idehelputilstrings');
    t.Dependencies.AddUnit('srcedithintfrm');
    t.Dependencies.AddUnit('fpdochints');
    t.Dependencies.AddUnit('fpdocselectinherited');
    t.Dependencies.AddUnit('fpdocselectlink');
    t.Dependencies.AddUnit('idecontexthelpedit');
    t.Dependencies.AddUnit('idewindowhelp');
    t.Dependencies.AddUnit('fpdoceditwindow');

    T:=P.Targets.AddUnit('codehelp.pas');
    T:=P.Targets.AddUnit('idehelputilstrings.pas');
    T:=P.Targets.AddUnit('srcedithintfrm.pas');
    T:=P.Targets.AddUnit('fpdochints.pas');
    T:=P.Targets.AddUnit('fpdocselectinherited.pas');
    T:=P.Targets.AddUnit('fpdocselectlink.pas');
    T:=P.Targets.AddUnit('idecontexthelpedit.pas');
    T:=P.Targets.AddUnit('idewindowhelp.pas');
    T:=P.Targets.AddUnit('fpdoceditwindow.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('idehelputils.compiled');
    P.InstallFiles.Add('idehelputils.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_IdeHelpUtils('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
