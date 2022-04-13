{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for Pas2JSDsgn 1.0.2

   This file was generated on 13.04.2022
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_Pas2JSDsgn(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('pas2jsdsgn');
    P.Version:='1.0.2-0';

    P.Directory:=ADirectory;

    P.Author:='Mattias Gaertner';
    P.License:='GPL-2';
    P.Description:='Adds a Lazarus project for pas2js browser applications.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('simplewebservergui');
    D := P.Dependencies.Add('codetools');
    D := P.Dependencies.Add('ideintf');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Schi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('pas2jsdsgn.pas');
    t.Dependencies.AddUnit('pjsdsgnregister');
    t.Dependencies.AddUnit('pjsdsgnoptsframe');
    t.Dependencies.AddUnit('frmpas2jsbrowserprojectoptions');
    t.Dependencies.AddUnit('pjsdsgnoptions');
    t.Dependencies.AddUnit('frmpas2jsnodejsprojectoptions');
    t.Dependencies.AddUnit('pjscontroller');
    t.Dependencies.AddUnit('strpas2jsdesign');
    t.Dependencies.AddUnit('pjsprojectoptions');
    t.Dependencies.AddUnit('frmpas2jsatompackagesettings');
    t.Dependencies.AddUnit('regpas2jsatom');
    t.Dependencies.AddUnit('regpas2jsvscode');
    t.Dependencies.AddUnit('frmpas2jsvscodeextensionsettings');
    t.Dependencies.AddUnit('frmhtmltoform');
    t.Dependencies.AddUnit('idehtml2class');
    t.Dependencies.AddUnit('frmdtstopas');
    t.Dependencies.AddUnit('idedtstopas');
    t.Dependencies.AddUnit('idehtmltools');

    T:=P.Targets.AddUnit('pjsdsgnregister.pas');
    T:=P.Targets.AddUnit('pjsdsgnoptsframe.pas');
    T:=P.Targets.AddUnit('frmpas2jsbrowserprojectoptions.pp');
    T:=P.Targets.AddUnit('pjsdsgnoptions.pas');
    T:=P.Targets.AddUnit('frmpas2jsnodejsprojectoptions.pp');
    T:=P.Targets.AddUnit('pjscontroller.pp');
    T:=P.Targets.AddUnit('strpas2jsdesign.pp');
    T:=P.Targets.AddUnit('pjsprojectoptions.pp');
    T:=P.Targets.AddUnit('frmpas2jsatompackagesettings.pas');
    T:=P.Targets.AddUnit('regpas2jsatom.pas');
    T:=P.Targets.AddUnit('regpas2jsvscode.pas');
    T:=P.Targets.AddUnit('frmpas2jsvscodeextensionsettings.pas');
    T:=P.Targets.AddUnit('frmhtmltoform.pas');
    T:=P.Targets.AddUnit('idehtml2class.pas');
    T:=P.Targets.AddUnit('frmdtstopas.pas');
    T:=P.Targets.AddUnit('idedtstopas.pas');
    T:=P.Targets.AddUnit('idehtmltools.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('Pas2JSDsgn.compiled');
    P.InstallFiles.Add('Pas2JSDsgn.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_Pas2JSDsgn('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
