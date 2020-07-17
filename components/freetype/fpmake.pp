{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for freetypelaz 1.0

   This file was generated on 17.07.2020
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_freetypelaz(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('freetypelaz');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='David Turner, Robert Wilhelm and Werner Lemberg';
    P.License:='Modified LGPL-2 or FreeType License';
    P.Description:='FreeType font rendering.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('lazutils');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('freetypelaz.pas');
    D := T.Dependencies.AddUnit('EasyLazFreeType');
    D := T.Dependencies.AddUnit('LazFreeType');
    D := T.Dependencies.AddUnit('LazFreeTypeFontCollection');
    D := T.Dependencies.AddUnit('LazFreeTypeFPImageDrawer');
    D := T.Dependencies.AddUnit('TTCache');
    D := T.Dependencies.AddUnit('TTCalc');
    D := T.Dependencies.AddInclude('ttcalc1.inc');
    D := T.Dependencies.AddInclude('ttcalc2.inc');
    D := T.Dependencies.AddInclude('ttcalc3.inc');
    D := T.Dependencies.AddInclude('ttcalc4.inc');
    D := T.Dependencies.AddUnit('TTCMap');
    D := T.Dependencies.AddInclude('ttconfig.inc');
    D := T.Dependencies.AddUnit('TTDebug');
    D := T.Dependencies.AddUnit('TTError');
    D := T.Dependencies.AddUnit('TTFile');
    D := T.Dependencies.AddUnit('TTGLoad');
    D := T.Dependencies.AddUnit('TTInterp');
    D := T.Dependencies.AddUnit('TTKern');
    D := T.Dependencies.AddUnit('TTLoad');
    D := T.Dependencies.AddUnit('TTMemory');
    D := T.Dependencies.AddUnit('TTObjs');
    D := T.Dependencies.AddUnit('TTProfile');
    D := T.Dependencies.AddUnit('TTRASTER');
    D := T.Dependencies.AddInclude('ttraster_sweep.inc');
    D := T.Dependencies.AddUnit('TTTables');
    D := T.Dependencies.AddUnit('TTTypes');
    T := P.Targets.AddImplicitUnit('easylazfreetype.pas');
    T := P.Targets.AddImplicitUnit('lazfreetype.pas');
    T := P.Targets.AddImplicitUnit('lazfreetypefontcollection.pas');
    T := P.Targets.AddImplicitUnit('lazfreetypefpimagedrawer.pas');
    T := P.Targets.AddImplicitUnit('ttcache.pas');
    T := P.Targets.AddImplicitUnit('ttcalc.pas');
    T := P.Targets.AddImplicitUnit('ttcmap.pas');
    T := P.Targets.AddImplicitUnit('ttdebug.pas');
    T := P.Targets.AddImplicitUnit('tterror.pas');
    T := P.Targets.AddImplicitUnit('ttfile.pas');
    T := P.Targets.AddImplicitUnit('ttgload.pas');
    T := P.Targets.AddImplicitUnit('ttinterp.pas');
    T := P.Targets.AddImplicitUnit('ttkern.pas');
    T := P.Targets.AddImplicitUnit('ttload.pas');
    T := P.Targets.AddImplicitUnit('ttmemory.pas');
    T := P.Targets.AddImplicitUnit('ttobjs.pas');
    T := P.Targets.AddImplicitUnit('ttprofile.pas');
    T := P.Targets.AddImplicitUnit('ttraster.pas');
    T := P.Targets.AddImplicitUnit('tttables.pas');
    T := P.Targets.AddImplicitUnit('tttypes.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('freetypelaz.compiled');
    P.InstallFiles.Add('freetypelaz.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_freetypelaz('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
