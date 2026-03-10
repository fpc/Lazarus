{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for DebuggerIntf 0.1

   This file was generated on 10.03.2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_DebuggerIntf(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('debuggerintf');
    P.Version:='0.1.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='GPL-2';
    P.Description:='DebuggerIntf'#10''#10'Provides an interface to add debuggers to the IDE';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('lazdebuggerintf');
    D := P.Dependencies.Add('lazutils');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-venibq');
    P.Options.Add('-vw-h-');
    P.Options.Add('-vm4046');
    P.UnitPath.Add('fcl-proc331');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('debuggerintf.pas');
    t.Dependencies.AddUnit('dbgintfbasetypes');
    t.Dependencies.AddUnit('dbgintfdebuggerbase');
    t.Dependencies.AddUnit('dbgintfmiscclasses');
    t.Dependencies.AddUnit('ideminilibc');
    t.Dependencies.AddUnit('dbgintfpseudoterminal');
    t.Dependencies.AddUnit('dbgintfcommonstrings');
    t.Dependencies.AddUnit('dbgintfprocess');
    t.Dependencies.AddUnit('pipes331');
    t.Dependencies.AddUnit('process331');

    T:=P.Targets.AddUnit('dbgintfbasetypes.pas');
    T:=P.Targets.AddUnit('dbgintfdebuggerbase.pp');
    T:=P.Targets.AddUnit('dbgintfmiscclasses.pas');
    T:=P.Targets.AddUnit('ideminilibc.pas');
    T:=P.Targets.AddUnit('dbgintfpseudoterminal.pas');
    T:=P.Targets.AddUnit('dbgintfcommonstrings.pas');
    T:=P.Targets.AddUnit('dbgintfprocess.pas');
    P.Targets.AddImplicitUnit('fcl-proc331/pipes331.pp');
    P.Targets.AddImplicitUnit('fcl-proc331/process331.pp');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('debuggerintf.compiled');
    P.InstallFiles.Add('debuggerintf.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_DebuggerIntf('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
