{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazDebuggerGdbmi 0.1

   This file was generated on 23-12-2014
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazDebuggerGdbmi;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('lazdebuggergdbmi');
    P.Version:='0.1';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    P.Directory:='components/lazdebuggergdbmi';
{$endif ALLPACKAGES}

    P.Dependencies.Add('ideintf');
    P.Dependencies.Add('debuggerintf');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCL_PLATFORM)');
    P.Options.Add('-dNoCarbon');
    P.UnitPath.Add('../../packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../debuggerintf/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('../lazcontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('../ideintf/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('lazdebuggergdbmi.pas');
    t.Dependencies.AddUnit('cmdlinedebugger');
    t.Dependencies.AddUnit('debugutils');
    t.Dependencies.AddUnit('gdbtypeinfo');
    t.Dependencies.AddUnit('gdbmimiscclasses');
    t.Dependencies.AddUnit('ideminilibc');
    t.Dependencies.AddUnit('gdbmidebugger');
    t.Dependencies.AddUnit('gdbmidebuginstructions');
    t.Dependencies.AddUnit('gdbmiserverdebugger');
    t.Dependencies.AddUnit('sshgdbmidebugger');
    t.Dependencies.AddUnit('gdbmistringconstants');

    T:=P.Targets.AddUnit('cmdlinedebugger.pp');
    T:=P.Targets.AddUnit('debugutils.pp');
    T:=P.Targets.AddUnit('gdbtypeinfo.pp');
    T:=P.Targets.AddUnit('gdbmimiscclasses.pp');
    T:=P.Targets.AddUnit('ideminilibc.pas');
    T:=P.Targets.AddUnit('gdbmidebugger.pp');
    T:=P.Targets.AddUnit('gdbmidebuginstructions.pp');
    T:=P.Targets.AddUnit('gdbmiserverdebugger.pas');
    T:=P.Targets.AddUnit('sshgdbmidebugger.pas');
    T:=P.Targets.AddUnit('gdbmistringconstants.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('LazDebuggerGdbmi.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazDebuggerGdbmi;
  Installer.Run;
end.
{$endif ALLPACKAGES}
