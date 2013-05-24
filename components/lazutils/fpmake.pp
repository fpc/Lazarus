{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazUtils 1.0

   This file was generated on 08/21/12
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_LazUtils;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('lazutils');
    P.Version:='1.0';

{$ifdef ALLPACKAGES}
    P.Directory:='components/lazutils/';
{$endif ALLPACKAGES}
    P.SupportBuildModes := [bmBuildUnit];

    P.Dependencies.Add('fcl');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-image');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-vewnhi');
    P.Options.Add('-l');
    P.Options.Add('-Fu../../packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('lazutils.pas');
    t.Dependencies.AddUnit('lazutilsstrconsts');
    t.Dependencies.AddUnit('lazlogger');
    t.Dependencies.AddUnit('lazutf8');
    t.Dependencies.AddUnit('lazutf8sysutils');
    t.Dependencies.AddUnit('lazutf8classes');
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
    t.Dependencies.AddUnit('lazdbglog');
    t.Dependencies.AddUnit('paswstring');
    t.Dependencies.AddUnit('fileutil');
    t.Dependencies.AddUnit('lazconfigstorage');
    t.Dependencies.AddUnit('masks');
    t.Dependencies.AddUnit('lconvencoding');
    t.Dependencies.AddUnit('lazutf16');
    t.Dependencies.AddUnit('lazmethodlist');
    t.Dependencies.AddUnit('avglvltree');
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
    T:=P.Targets.AddUnit('lazconfigstorage.pas');
    T:=P.Targets.AddUnit('masks.pas');
    T:=P.Targets.AddUnit('lazutilsstrconsts.pas');
    T:=P.Targets.AddUnit('lconvencoding.pas');
    T:=P.Targets.AddUnit('lazutf16.pas');
    T:=P.Targets.AddUnit('lazutf8sysutils.pas');
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

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_LazUtils;
  Installer.Run;
end.
{$endif ALLPACKAGES}
