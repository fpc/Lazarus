{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazDebuggerGdbmi 0.1

   This file was generated on 01/11/2021
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazDebuggerGdbmi(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lazdebuggergdbmi');
    P.Version:='0.1.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team / Marc Weustink  / Martin Friebe';
    P.License:='GPL';
    P.Description:='Debugger for Lazarus IDE.'#13#10''#13#10'This debugger uses gdb and is based on gdb''s mi interface.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('cmdlinedebuggerbase');
    D := P.Dependencies.Add('debuggerintf');
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
    T:=P.Targets.AddUnit('lazdebuggergdbmi.pas');
    t.Dependencies.AddUnit('cmdlinedebugger');
    t.Dependencies.AddUnit('debugutils');
    t.Dependencies.AddUnit('gdbtypeinfo');
    t.Dependencies.AddUnit('gdbmimiscclasses');
    t.Dependencies.AddUnit('gdbmidebugger');
    t.Dependencies.AddUnit('gdbmidebuginstructions');
    t.Dependencies.AddUnit('gdbmiserverdebugger');
    t.Dependencies.AddUnit('sshgdbmidebugger');
    t.Dependencies.AddUnit('gdbmistringconstants');

    T:=P.Targets.AddUnit('cmdlinedebugger.pp');
    T:=P.Targets.AddUnit('debugutils.pp');
    T:=P.Targets.AddUnit('gdbtypeinfo.pp');
    T:=P.Targets.AddUnit('gdbmimiscclasses.pp');
    T:=P.Targets.AddUnit('gdbmidebugger.pp');
    T:=P.Targets.AddUnit('gdbmidebuginstructions.pp');
    T:=P.Targets.AddUnit('gdbmiserverdebugger.pas');
    T:=P.Targets.AddUnit('sshgdbmidebugger.pas');
    T:=P.Targets.AddUnit('gdbmistringconstants.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('LazDebuggerGdbmi.compiled');
    P.InstallFiles.Add('LazDebuggerGdbmi.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazDebuggerGdbmi('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
