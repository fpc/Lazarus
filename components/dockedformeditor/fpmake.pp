{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for DockedFormEditor 0.0

   This file was generated on 06/08/2024
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_DockedFormEditor(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('dockedformeditor');
    P.Version:='<none>';

    P.Directory:=ADirectory;


    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('lazcontroldsgn');
    D := P.Dependencies.Add('debuggerintf');
    D := P.Dependencies.Add('codetools');
    D := P.Dependencies.Add('ideintf');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-gv');
    P.Options.Add('-Xg');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.IncludePath.Add('source');
    P.UnitPath.Add('source');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('dockedformeditor.pas');
    t.Dependencies.AddUnit('dockedregister');
    t.Dependencies.AddUnit('dockedstrconsts');
    t.Dependencies.AddUnit('dockedformaccesses');
    t.Dependencies.AddUnit('dockedmainide');
    t.Dependencies.AddUnit('dockedresizer');
    t.Dependencies.AddUnit('dockedoptionside');
    t.Dependencies.AddUnit('dockedoptionsframe');
    t.Dependencies.AddUnit('dockedtools');
    t.Dependencies.AddUnit('dockeddesignform');
    t.Dependencies.AddUnit('dockedsourcepagecontrol');
    t.Dependencies.AddUnit('dockedsourcewindow');
    t.Dependencies.AddUnit('dockedanchordesigner');
    t.Dependencies.AddUnit('dockedbasicanchordesigner');
    t.Dependencies.AddUnit('dockedanchorcontrol');
    t.Dependencies.AddUnit('dockedgrip');
    t.Dependencies.AddUnit('dockedresizecontrol');
    t.Dependencies.AddUnit('dockedforminitialsetupframe');

    T:=P.Targets.AddUnit('source\dockedregister.pas');
    T:=P.Targets.AddUnit('dockedstrconsts.pas');
    T:=P.Targets.AddUnit('source\dockedformaccesses.pas');
    T:=P.Targets.AddUnit('source\dockedmainide.pas');
    T:=P.Targets.AddUnit('source\dockedresizer.pas');
    T:=P.Targets.AddUnit('source\dockedoptionside.pas');
    T:=P.Targets.AddUnit('source\dockedoptionsframe.pas');
    T:=P.Targets.AddUnit('source\dockedtools.pas');
    T:=P.Targets.AddUnit('source\dockeddesignform.pas');
    T:=P.Targets.AddUnit('source\dockedsourcepagecontrol.pas');
    T:=P.Targets.AddUnit('source\dockedsourcewindow.pas');
    T:=P.Targets.AddUnit('source\dockedanchordesigner.pas');
    T:=P.Targets.AddUnit('source\dockedbasicanchordesigner.pas');
    T:=P.Targets.AddUnit('source\dockedanchorcontrol.pas');
    T:=P.Targets.AddUnit('source\dockedgrip.pas');
    T:=P.Targets.AddUnit('source\dockedresizecontrol.pas');
    T:=P.Targets.AddUnit('dockedforminitialsetupframe.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('dockedformeditor.compiled');
    P.InstallFiles.Add('dockedformeditor.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_DockedFormEditor('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
