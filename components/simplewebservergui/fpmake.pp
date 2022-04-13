{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for SimpleWebServerGUI 1.0

   This file was generated on 13.04.2022
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_SimpleWebServerGUI(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('simplewebservergui');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;


    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('codetools');
    D := P.Dependencies.Add('syneditdsgn');
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
    T:=P.Targets.AddUnit('simplewebservergui.pas');
    t.Dependencies.AddUnit('simplewebsrvwnd');
    t.Dependencies.AddUnit('simplewebsrvoptionsframe');
    t.Dependencies.AddUnit('simplewebsrvoptions');
    t.Dependencies.AddUnit('simplewebsrvutils');
    t.Dependencies.AddUnit('simplewebsrvcontroller');
    t.Dependencies.AddUnit('simplewebsrvstrconsts');
    t.Dependencies.AddUnit('simplewebsrvadd');

    T:=P.Targets.AddUnit('simplewebsrvwnd.pas');
    T:=P.Targets.AddUnit('simplewebsrvoptionsframe.pas');
    T:=P.Targets.AddUnit('simplewebsrvoptions.pas');
    T:=P.Targets.AddUnit('simplewebsrvutils.pas');
    T:=P.Targets.AddUnit('simplewebsrvcontroller.pas');
    T:=P.Targets.AddUnit('simplewebsrvstrconsts.pas');
    T:=P.Targets.AddUnit('simplewebsrvadd.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('SimpleWebServerGUI.compiled');
    P.InstallFiles.Add('SimpleWebServerGUI.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_SimpleWebServerGUI('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
