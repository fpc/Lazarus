{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for freetype 1.0

   This file was generated on 14.07.2020
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_freetype(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('freetype');
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
    T:=P.Targets.AddUnit('freetype.pas');
    t.Dependencies.AddUnit('easylazfreetype');
    t.Dependencies.AddUnit('lazfreetype');
    t.Dependencies.AddUnit('lazfreetypefontcollection');
    t.Dependencies.AddUnit('lazfreetypefpimagedrawer');
    t.Dependencies.AddUnit('ttcache');
    t.Dependencies.AddUnit('ttcalc');
    t.Dependencies.AddUnit('ttcmap');
    t.Dependencies.AddUnit('ttdebug');
    t.Dependencies.AddUnit('tterror');
    t.Dependencies.AddUnit('ttfile');
    t.Dependencies.AddUnit('ttgload');
    t.Dependencies.AddUnit('ttinterp');
    t.Dependencies.AddUnit('ttkern');
    t.Dependencies.AddUnit('ttload');
    t.Dependencies.AddUnit('ttmemory');
    t.Dependencies.AddUnit('ttobjs');
    t.Dependencies.AddUnit('ttprofile');
    t.Dependencies.AddUnit('ttraster');
    t.Dependencies.AddUnit('tttables');
    t.Dependencies.AddUnit('tttypes');

    T:=P.Targets.AddUnit('easylazfreetype.pas');
    T:=P.Targets.AddUnit('lazfreetype.pas');
    T:=P.Targets.AddUnit('lazfreetypefontcollection.pas');
    T:=P.Targets.AddUnit('lazfreetypefpimagedrawer.pas');
    T:=P.Targets.AddUnit('ttcache.pas');
    T:=P.Targets.AddUnit('ttcalc.pas');
    T:=P.Targets.AddUnit('ttcmap.pas');
    T:=P.Targets.AddUnit('ttdebug.pas');
    T:=P.Targets.AddUnit('tterror.pas');
    T:=P.Targets.AddUnit('ttfile.pas');
    T:=P.Targets.AddUnit('ttgload.pas');
    T:=P.Targets.AddUnit('ttinterp.pas');
    T:=P.Targets.AddUnit('ttkern.pas');
    T:=P.Targets.AddUnit('ttload.pas');
    T:=P.Targets.AddUnit('ttmemory.pas');
    T:=P.Targets.AddUnit('ttobjs.pas');
    T:=P.Targets.AddUnit('ttprofile.pas');
    T:=P.Targets.AddUnit('ttraster.pas');
    T:=P.Targets.AddUnit('tttables.pas');
    T:=P.Targets.AddUnit('tttypes.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('freetype.compiled');
    P.InstallFiles.Add('freetype.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_freetype('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
