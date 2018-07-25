{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazUtils 1.0

   This file was generated on 14-10-16
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazUtils(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('lazutils');
    P.Version:='1.0';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('lazutils.pas');
    t.Dependencies.AddUnit('laz2_dom');
    t.Dependencies.AddUnit('laz2_xmlcfg');
    t.Dependencies.AddUnit('laz2_xmlread');
    t.Dependencies.AddUnit('laz2_xmlutils');
    t.Dependencies.AddUnit('laz2_xmlwrite');
    t.Dependencies.AddUnit('laz_dom');
    t.Dependencies.AddUnit('laz_xmlcfg');
    t.Dependencies.AddUnit('laz_xmlread');
    t.Dependencies.AddUnit('laz_xmlstreaming');
    t.Dependencies.AddUnit('laz_xmlwrite');
    t.Dependencies.AddUnit('lazfileutils');
    t.Dependencies.AddUnit('lazfilecache');
    t.Dependencies.AddUnit('lazutf8');
    t.Dependencies.AddUnit('lazdbglog');
    t.Dependencies.AddUnit('paswstring');
    t.Dependencies.AddUnit('fileutil');
    t.Dependencies.AddUnit('lazutf8classes');
    t.Dependencies.AddUnit('masks');
    t.Dependencies.AddUnit('lazutilsstrconsts');
    t.Dependencies.AddUnit('lconvencoding');
    t.Dependencies.AddUnit('lazutf16');
    t.Dependencies.AddUnit('lazsysutils');
    t.Dependencies.AddUnit('lazmethodlist');
    t.Dependencies.AddUnit('avglvltree');
    t.Dependencies.AddUnit('lazlogger');
    t.Dependencies.AddUnit('lazfreetype');
    t.Dependencies.AddUnit('ttcache');
    t.Dependencies.AddUnit('ttcalc');
    t.Dependencies.AddUnit('ttcmap');
    t.Dependencies.AddUnit('ttdebug');
    t.Dependencies.AddUnit('tterror');
    t.Dependencies.AddUnit('ttfile');
    t.Dependencies.AddUnit('ttgload');
    t.Dependencies.AddUnit('ttinterp');
    t.Dependencies.AddUnit('ttload');
    t.Dependencies.AddUnit('ttmemory');
    t.Dependencies.AddUnit('ttobjs');
    t.Dependencies.AddUnit('ttprofile');
    t.Dependencies.AddUnit('ttraster');
    t.Dependencies.AddUnit('tttables');
    t.Dependencies.AddUnit('tttypes');
    t.Dependencies.AddUnit('easylazfreetype');
    t.Dependencies.AddUnit('lazloggerbase');
    t.Dependencies.AddUnit('lazloggerdummy');
    t.Dependencies.AddUnit('lazclasses');
    t.Dependencies.AddUnit('lazfreetypefontcollection');
    t.Dependencies.AddUnit('lazconfigstorage');
    t.Dependencies.AddUnit('utf8process');
    t.Dependencies.AddUnit('laz2_xpath');
    t.Dependencies.AddUnit('lazloggerprofiling');
    t.Dependencies.AddUnit('fpcadds');
    t.Dependencies.AddUnit('lazutilities');
    t.Dependencies.AddUnit('lazfglhash');
    t.Dependencies.AddUnit('lcsvutils');
    t.Dependencies.AddUnit('lazcollections');
    t.Dependencies.AddUnit('lazlistclasses');
    t.Dependencies.AddUnit('lazfreetypefpimagedrawer');
    t.Dependencies.AddUnit('lookupstringlist');
    t.Dependencies.AddUnit('lazunicode');

    T:=P.Targets.AddUnit('laz2_dom.pas');
    T:=P.Targets.AddUnit('laz2_xmlcfg.pas');
    T:=P.Targets.AddUnit('laz2_xmlread.pas');
    T:=P.Targets.AddUnit('laz2_xmlutils.pas');
    T:=P.Targets.AddUnit('laz2_xmlwrite.pas');
    T:=P.Targets.AddUnit('laz_dom.pas');
    T:=P.Targets.AddUnit('laz_xmlcfg.pas');
    T:=P.Targets.AddUnit('laz_xmlread.pas');
    T:=P.Targets.AddUnit('laz_xmlstreaming.pas');
    T:=P.Targets.AddUnit('laz_xmlwrite.pas');
    T:=P.Targets.AddUnit('lazfileutils.pas');
    T:=P.Targets.AddUnit('lazfilecache.pas');
    T:=P.Targets.AddUnit('lazutf8.pas');
    T:=P.Targets.AddUnit('lazdbglog.pas');
    T:=P.Targets.AddUnit('paswstring.pas');
    T:=P.Targets.AddUnit('fileutil.pas');
    T:=P.Targets.AddUnit('lazutf8classes.pas');
    T:=P.Targets.AddUnit('masks.pas');
    T:=P.Targets.AddUnit('lazutilsstrconsts.pas');
    T:=P.Targets.AddUnit('lconvencoding.pas');
    T:=P.Targets.AddUnit('lazutf16.pas');
    T:=P.Targets.AddUnit('lazsysutils.pas');
    T:=P.Targets.AddUnit('lazmethodlist.pas');
    T:=P.Targets.AddUnit('avglvltree.pas');
    T:=P.Targets.AddUnit('lazlogger.pas');
    T:=P.Targets.AddUnit('lazfreetype.pas');
    T:=P.Targets.AddUnit('ttcache.pas');
    T:=P.Targets.AddUnit('ttcalc.pas');
    T:=P.Targets.AddUnit('ttcmap.pas');
    T:=P.Targets.AddUnit('ttdebug.pas');
    T:=P.Targets.AddUnit('tterror.pas');
    T:=P.Targets.AddUnit('ttfile.pas');
    T:=P.Targets.AddUnit('ttgload.pas');
    T:=P.Targets.AddUnit('ttinterp.pas');
    T:=P.Targets.AddUnit('ttload.pas');
    T:=P.Targets.AddUnit('ttmemory.pas');
    T:=P.Targets.AddUnit('ttobjs.pas');
    T:=P.Targets.AddUnit('ttprofile.pas');
    T:=P.Targets.AddUnit('ttraster.pas');
    T:=P.Targets.AddUnit('tttables.pas');
    T:=P.Targets.AddUnit('tttypes.pas');
    T:=P.Targets.AddUnit('easylazfreetype.pas');
    T:=P.Targets.AddUnit('lazloggerbase.pas');
    T:=P.Targets.AddUnit('lazloggerdummy.pas');
    T:=P.Targets.AddUnit('lazclasses.pas');
    T:=P.Targets.AddUnit('lazfreetypefontcollection.pas');
    T:=P.Targets.AddUnit('lazconfigstorage.pas');
    T:=P.Targets.AddUnit('utf8process.pp');
    T:=P.Targets.AddUnit('laz2_xpath.pas');
    T:=P.Targets.AddUnit('lazloggerprofiling.pas');
    T:=P.Targets.AddUnit('fpcadds.pas');
    T:=P.Targets.AddUnit('lazutilities.pas');
    T:=P.Targets.AddUnit('lazfglhash.pas');
    T:=P.Targets.AddUnit('lcsvutils.pas');
    T:=P.Targets.AddUnit('lazcollections.pas');
    T:=P.Targets.AddUnit('lazlistclasses.pas');
    T:=P.Targets.AddUnit('lazfreetypefpimagedrawer.pas');
    T:=P.Targets.AddUnit('lookupstringlist.pas');
    T:=P.Targets.AddUnit('lazunicode.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('LazUtils.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazUtils('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
