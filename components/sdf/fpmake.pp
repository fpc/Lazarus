{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for SDFLaz 0.1.1

   This file was generated on 23-12-2014
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_SDFLaz;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('sdflaz');
    P.Version:='0.1.1';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    P.Directory:='components/sdf';
{$endif ALLPACKAGES}

    P.Dependencies.Add('lcl');
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
    P.UnitPath.Add('../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.UnitPath.Add('../../lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('sdflaz.pas');
    t.Dependencies.AddUnit('registersdf');

    T:=P.Targets.AddUnit('registersdf.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('SDFLaz.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_SDFLaz;
  Installer.Run;
end.
{$endif ALLPACKAGES}
