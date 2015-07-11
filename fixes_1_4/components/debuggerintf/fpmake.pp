{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for DebuggerIntf 0.1

   This file was generated on 02-01-2015
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

begin
  with Installer do
    begin
    P:=AddPAckage('debuggerintf');
    P.Version:='0.1';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('lclbase');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vw-');
    P.Options.Add('-vh-');
    P.Options.Add('-venibq');
    P.Options.Add('-vm4046');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('debuggerintf.pas');
    t.Dependencies.AddUnit('dbgintfbasetypes');
    t.Dependencies.AddUnit('dbgintfdebuggerbase');
    t.Dependencies.AddUnit('dbgintfmiscclasses');

    T:=P.Targets.AddUnit('dbgintfbasetypes.pas');
    T:=P.Targets.AddUnit('dbgintfdebuggerbase.pp');
    T:=P.Targets.AddUnit('dbgintfmiscclasses.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('DebuggerIntf.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_DebuggerIntf('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
