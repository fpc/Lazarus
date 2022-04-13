{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazProjectGroups 0.7

   This file was generated on 13.04.2022
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazProjectGroups(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lazprojectgroups');
    P.Version:='0.7.0-0';

    P.Directory:=ADirectory;

    P.Author:='Mattias Gaertner, Michael Van Canneyt';
    P.License:='Same as IDEIntf.'#10'GPL-2.';
    P.Description:='IDE Add-on for grouping projects, packages and project groups.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('codetools');
    D := P.Dependencies.Add('ideintf');
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
    T:=P.Targets.AddUnit('lazprojectgroups.pas');
    t.Dependencies.AddUnit('projectgroup');
    t.Dependencies.AddUnit('projectgroupeditor');
    t.Dependencies.AddUnit('regprojectgroup');
    t.Dependencies.AddUnit('projectgroupstrconst');
    t.Dependencies.AddUnit('prjgrpoptionsfrm');
    t.Dependencies.AddUnit('prjgrpinfofrm');

    T:=P.Targets.AddUnit('projectgroup.pp');
    T:=P.Targets.AddUnit('projectgroupeditor.pas');
    T:=P.Targets.AddUnit('regprojectgroup.pp');
    T:=P.Targets.AddUnit('projectgroupstrconst.pas');
    T:=P.Targets.AddUnit('prjgrpoptionsfrm.pas');
    T:=P.Targets.AddUnit('prjgrpinfofrm.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('LazProjectGroups.compiled');
    P.InstallFiles.Add('LazProjectGroups.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazProjectGroups('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
