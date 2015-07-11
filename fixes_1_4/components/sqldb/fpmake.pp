{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for SQLDBLaz 1.0.1

   This file was generated on 23-12-2014
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_SQLDBLaz;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('sqldblaz');
    P.Version:='1.0.1';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    P.Directory:='components/sqldb';
{$endif ALLPACKAGES}

    P.Dependencies.Add('codetools');
    P.Dependencies.Add('synedit');
    P.Dependencies.Add('ideintf');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scgi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCL_PLATFORM)');
    P.Options.Add('-dNoCarbon');
    P.UnitPath.Add('../../packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../codetools/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('../lazcontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('../synedit/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('../ideintf/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('sqldblaz.pas');
    t.Dependencies.AddUnit('sqlstringspropertyeditordlg');

    P.Sources.AddSrc('registersqldb.pas');
    T:=P.Targets.AddUnit('sqlstringspropertyeditordlg.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('SQLDBLaz.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_SQLDBLaz;
  Installer.Run;
end.
{$endif ALLPACKAGES}
