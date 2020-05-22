{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for DebuggerIntf 0.1

   This file was generated on 22-05-20
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

    D := P.Dependencies.Add('lclbase');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-venibq');
    P.Options.Add('-vw-h-');
    P.Options.Add('-vm4046');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('debuggerintf.pas');
    D := T.Dependencies.AddUnit('DbgIntfBaseTypes');
    D := T.Dependencies.AddUnit('DbgIntfDebuggerBase');
    D := T.Dependencies.AddUnit('DbgIntfMiscClasses');
    D := T.Dependencies.AddUnit('IDEMiniLibC');
    D := T.Dependencies.AddUnit('DbgIntfPseudoTerminal');
    T := P.Targets.AddImplicitUnit('dbgintfbasetypes.pas');
    T := P.Targets.AddImplicitUnit('dbgintfdebuggerbase.pp');
    T := P.Targets.AddImplicitUnit('dbgintfmiscclasses.pas');
    T := P.Targets.AddImplicitUnit('ideminilibc.pas');
    T := P.Targets.AddImplicitUnit('dbgintfpseudoterminal.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('DebuggerIntf.compiled');
    P.InstallFiles.Add('DebuggerIntf.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_DebuggerIntf('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
