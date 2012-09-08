{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazControls 1.0.1

   This file was generated on 09/01/12
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazControls;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('lazcontrols');
    P.Version:='1.0.1';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    P.Directory:='components/lazcontrols/';
{$endif ALLPACKAGES}

    P.Dependencies.Add('lcl');
    P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-vewnhi');
    P.Options.Add('-l');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('lazcontrols.pas');
    t.Dependencies.AddUnit('dividerbevel');
    t.Dependencies.AddUnit('extendednotebook');
    t.Dependencies.AddUnit('listfilteredit');
    t.Dependencies.AddUnit('treefilteredit');
    t.Dependencies.AddUnit('shortpathedit');

    T:=P.Targets.AddUnit('dividerbevel.pas');
    T:=P.Targets.AddUnit('extendednotebook.pas');
    T:=P.Targets.AddUnit('listfilteredit.pas');
    T:=P.Targets.AddUnit('treefilteredit.pas');
    T:=P.Targets.AddUnit('shortpathedit.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('LazControls.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazControls;
  Installer.Run;
end.
{$endif ALLPACKAGES}
