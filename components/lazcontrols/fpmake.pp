{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazControls 1.0.1

   This file was generated on 13-1-2016
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazControls(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('lazcontrols');
    P.Version:='1.0.1';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('lcl');
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
    T:=P.Targets.AddUnit('lazcontrols.pas');
    t.Dependencies.AddUnit('checkboxthemed');
    t.Dependencies.AddUnit('dividerbevel');
    t.Dependencies.AddUnit('extendednotebook');
    t.Dependencies.AddUnit('listfilteredit');
    t.Dependencies.AddUnit('listviewfilteredit');
    t.Dependencies.AddUnit('treefilteredit');
    t.Dependencies.AddUnit('shortpathedit');
    t.Dependencies.AddUnit('lvlgraphctrl');
    t.Dependencies.AddUnit('extendedtabcontrols');
    t.Dependencies.AddUnit('spinex');

    T:=P.Targets.AddUnit('checkboxthemed.pas');
    T:=P.Targets.AddUnit('dividerbevel.pas');
    T:=P.Targets.AddUnit('extendednotebook.pas');
    T:=P.Targets.AddUnit('listfilteredit.pas');
    T:=P.Targets.AddUnit('listviewfilteredit.pas');
    T:=P.Targets.AddUnit('treefilteredit.pas');
    T:=P.Targets.AddUnit('shortpathedit.pas');
    T:=P.Targets.AddUnit('lvlgraphctrl.pas');
    T:=P.Targets.AddUnit('extendedtabcontrols.pas');
    T:=P.Targets.AddUnit('spinex.pp');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('LazControls.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazControls('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
