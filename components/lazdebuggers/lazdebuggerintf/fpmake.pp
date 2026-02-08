{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazDebuggerIntf 0.0.1

   This file was generated on 08/02/2026
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazDebuggerIntf(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lazdebuggerintf');
    P.Version:='0.0.1-0';

    P.Directory:=ADirectory;

    P.Author:='Martin Friebe';
    P.License:='LGPL with linking exception'#13#10''#13#10'See LCL license for details.';
    P.Description:='An interface for integrating Debugger-Backends into the IDE';

    D := P.Dependencies.Add('lazutils');
    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-vm6058,5024');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('lazdebuggerintfpackage.pas');
    t.Dependencies.AddUnit('lazdebuggerintf');
    t.Dependencies.AddUnit('lazdebuggertemplate');
    t.Dependencies.AddUnit('lazdebuggerintfbasetypes');
    t.Dependencies.AddUnit('lazdebuggervalueconverter');
    t.Dependencies.AddUnit('dbgutilstypepatternlist');
    t.Dependencies.AddUnit('lazdebuggerutils');
    t.Dependencies.AddUnit('lazdebuggerintffloattypes');
    t.Dependencies.AddUnit('lazdebuggerintfexcludedroutines');
    t.Dependencies.AddUnit('lazdebuggerintfsynchronizedlist');

    T:=P.Targets.AddUnit('lazdebuggerintf.pas');
    T:=P.Targets.AddUnit('lazdebuggertemplate.pas');
    T:=P.Targets.AddUnit('lazdebuggerintfbasetypes.pas');
    T:=P.Targets.AddUnit('lazdebuggervalueconverter.pas');
    T:=P.Targets.AddUnit('dbgutilstypepatternlist.pas');
    T:=P.Targets.AddUnit('lazdebuggerutils.pas');
    T:=P.Targets.AddUnit('lazdebuggerintffloattypes.pas');
    T:=P.Targets.AddUnit('lazdebuggerintfexcludedroutines.pas');
    T:=P.Targets.AddUnit('lazdebuggerintfsynchronizedlist.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('lazdebuggerintf.compiled');
    P.InstallFiles.Add('lazdebuggerintf.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazDebuggerIntf('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
