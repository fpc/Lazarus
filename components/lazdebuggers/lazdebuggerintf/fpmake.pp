{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for LazDebuggerIntf 0.0.1

   This file was generated on 27-03-22
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

    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('lazdebuggerintfpackage.pas');
    D := T.Dependencies.AddUnit('LazDebuggerIntf');
    D := T.Dependencies.AddUnit('LazDebuggerTemplate');
    D := T.Dependencies.AddUnit('LazDebuggerIntfBaseTypes');
    T := P.Targets.AddImplicitUnit('lazdebuggerintf.pas');
    T := P.Targets.AddImplicitUnit('lazdebuggertemplate.pas');
    T := P.Targets.AddImplicitUnit('lazdebuggerintfbasetypes.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('LazDebuggerIntf.compiled');
    P.InstallFiles.Add('LazDebuggerIntf.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazDebuggerIntf('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
