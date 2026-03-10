{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazControls 1.0.1

   This file was generated on 10.03.2026
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
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lazcontrols');
    P.Version:='1.0.1-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='modified LGPL-2';
    P.Description:='Some extra LCL controls needed by the IDE.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('lcl');
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
    t.Dependencies.AddUnit('smallorderedseteditor');
    t.Dependencies.AddUnit('laznumedit');
    t.Dependencies.AddUnit('shiftstateselector');
    t.Dependencies.AddUnit('selectitemdialog');

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
    T:=P.Targets.AddUnit('smallorderedseteditor.pas');
    T:=P.Targets.AddUnit('laznumedit.pas');
    T:=P.Targets.AddUnit('shiftstateselector.pas');
    T:=P.Targets.AddUnit('selectitemdialog.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('lazcontrols.compiled');
    P.InstallFiles.Add('lazcontrols.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazControls('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
