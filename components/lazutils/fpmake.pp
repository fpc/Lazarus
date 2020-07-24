{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazUtils 1.0

   This file was generated on 24.07.2020
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
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('lazutils');
    P.Version:='1.0.0-0';

    P.Directory:=ADirectory;

    P.Author:='Lazarus Team';
    P.License:='Modified LGPL-2';
    P.Description:='Useful units for Lazarus packages.';

    P.Flags.Add('LazarusDsgnPkg');

    D := P.Dependencies.Add('fcl-image');
    D := P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('lazutils.pas');
    t.Dependencies.AddUnit('avglvltree');
    t.Dependencies.AddUnit('dynamicarray');
    t.Dependencies.AddUnit('dynhasharray');
    t.Dependencies.AddUnit('dynqueue');
    t.Dependencies.AddUnit('extendedstrings');
    t.Dependencies.AddUnit('fileutil');
    t.Dependencies.AddUnit('fpcadds');
    t.Dependencies.AddUnit('laz2_dom');
    t.Dependencies.AddUnit('laz2_xmlcfg');
    t.Dependencies.AddUnit('laz2_xmlread');
    t.Dependencies.AddUnit('laz2_xmlutils');
    t.Dependencies.AddUnit('laz2_xmlwrite');
    t.Dependencies.AddUnit('laz2_xpath');
    t.Dependencies.AddUnit('laz_dom');
    t.Dependencies.AddUnit('laz_xmlcfg');
    t.Dependencies.AddUnit('laz_xmlread');
    t.Dependencies.AddUnit('laz_xmlstreaming');
    t.Dependencies.AddUnit('laz_xmlwrite');
    t.Dependencies.AddUnit('lazclasses');
    t.Dependencies.AddUnit('lazcollections');
    t.Dependencies.AddUnit('lazconfigstorage');
    t.Dependencies.AddUnit('lazdbglog');
    t.Dependencies.AddUnit('lazfglhash');
    t.Dependencies.AddUnit('lazfilecache');
    t.Dependencies.AddUnit('lazfileutils');
    t.Dependencies.AddUnit('lazlinkedlist');
    t.Dependencies.AddUnit('lazlistclasses');
    t.Dependencies.AddUnit('lazlogger');
    t.Dependencies.AddUnit('lazloggerbase');
    t.Dependencies.AddUnit('lazloggerdummy');
    t.Dependencies.AddUnit('lazloggerprofiling');
    t.Dependencies.AddUnit('lazmethodlist');
    t.Dependencies.AddUnit('lazunicode');
    t.Dependencies.AddUnit('lazutf16');
    t.Dependencies.AddUnit('lazutf8');
    t.Dependencies.AddUnit('lazutf8classes');
    t.Dependencies.AddUnit('lazsysutils');
    t.Dependencies.AddUnit('lazutilities');
    t.Dependencies.AddUnit('lazutilsstrconsts');
    t.Dependencies.AddUnit('lconvencoding');
    t.Dependencies.AddUnit('lcsvutils');
    t.Dependencies.AddUnit('lookupstringlist');
    t.Dependencies.AddUnit('maps');
    t.Dependencies.AddUnit('masks');
    t.Dependencies.AddUnit('paswstring');
    t.Dependencies.AddUnit('stringhashlist');
    t.Dependencies.AddUnit('textstrings');
    t.Dependencies.AddUnit('translations');
    t.Dependencies.AddUnit('utf8process');
    t.Dependencies.AddUnit('html2textrender');
    t.Dependencies.AddUnit('laz_avl_tree');
    t.Dependencies.AddUnit('compwriterpas');
    t.Dependencies.AddUnit('lazpasreadutil');
    t.Dependencies.AddUnit('integerlist');
    t.Dependencies.AddUnit('lazversion');
    t.Dependencies.AddUnit('uitypes');
    t.Dependencies.AddUnit('graphtype');
    t.Dependencies.AddUnit('laztracer');
    t.Dependencies.AddUnit('lazstringutils');
    t.Dependencies.AddUnit('lazutf8sysutils');
    t.Dependencies.AddUnit('objectlists');
    t.Dependencies.AddUnit('codepagesasian');
    t.Dependencies.AddUnit('codepagescommon');

    T:=P.Targets.AddUnit('avglvltree.pas');
    T:=P.Targets.AddUnit('dynamicarray.pas');
    T:=P.Targets.AddUnit('dynhasharray.pp');
    T:=P.Targets.AddUnit('dynqueue.pas');
    T:=P.Targets.AddUnit('extendedstrings.pas');
    T:=P.Targets.AddUnit('fileutil.pas');
    T:=P.Targets.AddUnit('fpcadds.pas');
    T:=P.Targets.AddUnit('laz2_dom.pas');
    T:=P.Targets.AddUnit('laz2_xmlcfg.pas');
    T:=P.Targets.AddUnit('laz2_xmlread.pas');
    T:=P.Targets.AddUnit('laz2_xmlutils.pas');
    T:=P.Targets.AddUnit('laz2_xmlwrite.pas');
    T:=P.Targets.AddUnit('laz2_xpath.pas');
    T:=P.Targets.AddUnit('laz_dom.pas');
    T:=P.Targets.AddUnit('laz_xmlcfg.pas');
    T:=P.Targets.AddUnit('laz_xmlread.pas');
    T:=P.Targets.AddUnit('laz_xmlstreaming.pas');
    T:=P.Targets.AddUnit('laz_xmlwrite.pas');
    T:=P.Targets.AddUnit('lazclasses.pas');
    T:=P.Targets.AddUnit('lazcollections.pas');
    T:=P.Targets.AddUnit('lazconfigstorage.pas');
    T:=P.Targets.AddUnit('lazdbglog.pas');
    T:=P.Targets.AddUnit('lazfglhash.pas');
    T:=P.Targets.AddUnit('lazfilecache.pas');
    T:=P.Targets.AddUnit('lazfileutils.pas');
    T:=P.Targets.AddUnit('lazlinkedlist.pas');
    T:=P.Targets.AddUnit('lazlistclasses.pas');
    T:=P.Targets.AddUnit('lazlogger.pas');
    T:=P.Targets.AddUnit('lazloggerbase.pas');
    T:=P.Targets.AddUnit('lazloggerdummy.pas');
    T:=P.Targets.AddUnit('lazloggerprofiling.pas');
    T:=P.Targets.AddUnit('lazmethodlist.pas');
    T:=P.Targets.AddUnit('lazunicode.pas');
    T:=P.Targets.AddUnit('lazutf16.pas');
    T:=P.Targets.AddUnit('lazutf8.pas');
    T:=P.Targets.AddUnit('lazutf8classes.pas');
    T:=P.Targets.AddUnit('lazsysutils.pas');
    T:=P.Targets.AddUnit('lazutilities.pas');
    T:=P.Targets.AddUnit('lazutilsstrconsts.pas');
    T:=P.Targets.AddUnit('lconvencoding.pas');
    T:=P.Targets.AddUnit('lcsvutils.pas');
    T:=P.Targets.AddUnit('lookupstringlist.pas');
    T:=P.Targets.AddUnit('maps.pp');
    T:=P.Targets.AddUnit('masks.pas');
    T:=P.Targets.AddUnit('paswstring.pas');
    T:=P.Targets.AddUnit('stringhashlist.pas');
    T:=P.Targets.AddUnit('textstrings.pas');
    T:=P.Targets.AddUnit('translations.pas');
    T:=P.Targets.AddUnit('utf8process.pp');
    T:=P.Targets.AddUnit('html2textrender.pas');
    T:=P.Targets.AddUnit('laz_avl_tree.pp');
    T:=P.Targets.AddUnit('compwriterpas.pas');
    T:=P.Targets.AddUnit('lazpasreadutil.pas');
    T:=P.Targets.AddUnit('integerlist.pas');
    T:=P.Targets.AddUnit('lazversion.pas');
    T:=P.Targets.AddUnit('uitypes.pas');
    T:=P.Targets.AddUnit('graphtype.pp');
    T:=P.Targets.AddUnit('laztracer.pas');
    T:=P.Targets.AddUnit('lazstringutils.pas');
    T:=P.Targets.AddUnit('lazutf8sysutils.pas');
    T:=P.Targets.AddUnit('objectlists.pas');
    T:=P.Targets.AddUnit('codepagesasian.pas');
    T:=P.Targets.AddUnit('codepagescommon.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.Sources.AddSrc('LazUtils.compiled');
    P.InstallFiles.Add('LazUtils.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazUtils('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
