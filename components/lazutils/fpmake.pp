{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for LazUtils 1.0

   This file was generated on 22-05-20
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
    D := T.Dependencies.AddInclude('LazLoggerImpl.inc');
    D := T.Dependencies.AddInclude('LazLoggerIntf.inc');
    D := T.Dependencies.AddInclude('asiancodepagefunctions.inc');
    D := T.Dependencies.AddInclude('asiancodepages.inc');
    D := T.Dependencies.AddUnit('AvgLvlTree');
    D := T.Dependencies.AddUnit('DynamicArray');
    D := T.Dependencies.AddUnit('DynHashArray');
    D := T.Dependencies.AddUnit('DynQueue');
    D := T.Dependencies.AddUnit('EasyLazFreeType');
    D := T.Dependencies.AddUnit('ExtendedStrings');
    D := T.Dependencies.AddUnit('FileUtil');
    D := T.Dependencies.AddInclude('fileutil.inc');
    D := T.Dependencies.AddUnit('FPCAdds');
    D := T.Dependencies.AddUnit('Laz2_DOM');
    D := T.Dependencies.AddInclude('laz2_names.inc');
    D := T.Dependencies.AddUnit('Laz2_XMLCfg');
    D := T.Dependencies.AddUnit('laz2_XMLRead');
    D := T.Dependencies.AddUnit('laz2_xmlutils');
    D := T.Dependencies.AddUnit('laz2_XMLWrite');
    D := T.Dependencies.AddUnit('laz2_xpath');
    D := T.Dependencies.AddUnit('Laz_DOM');
    D := T.Dependencies.AddUnit('Laz_XMLCfg');
    D := T.Dependencies.AddUnit('Laz_XMLRead');
    D := T.Dependencies.AddUnit('Laz_XMLStreaming');
    D := T.Dependencies.AddUnit('Laz_XMLWrite');
    D := T.Dependencies.AddUnit('LazClasses');
    D := T.Dependencies.AddUnit('lazCollections');
    D := T.Dependencies.AddUnit('LazConfigStorage');
    D := T.Dependencies.AddUnit('LazDbgLog');
    D := T.Dependencies.AddUnit('lazfglhash');
    D := T.Dependencies.AddUnit('LazFileCache');
    D := T.Dependencies.AddUnit('LazFileUtils');
    D := T.Dependencies.AddInclude('lazfileutils.inc');
    D := T.Dependencies.AddUnit('LazFreeType');
    D := T.Dependencies.AddUnit('LazFreeTypeFontCollection');
    D := T.Dependencies.AddUnit('LazFreeTypeFPImageDrawer');
    D := T.Dependencies.AddUnit('LazLinkedList');
    D := T.Dependencies.AddUnit('LazListClasses');
    D := T.Dependencies.AddUnit('LazLogger');
    D := T.Dependencies.AddUnit('LazLoggerBase');
    D := T.Dependencies.AddUnit('LazLoggerDummy');
    D := T.Dependencies.AddUnit('LazLoggerProfiling');
    D := T.Dependencies.AddUnit('LazMethodList');
    D := T.Dependencies.AddUnit('LazUnicode');
    D := T.Dependencies.AddUnit('LazUTF16');
    D := T.Dependencies.AddUnit('LazUTF8');
    D := T.Dependencies.AddUnit('LazUTF8Classes');
    D := T.Dependencies.AddUnit('LazSysUtils');
    D := T.Dependencies.AddUnit('LazUtilities');
    D := T.Dependencies.AddInclude('lazutils_defines.inc');
    D := T.Dependencies.AddUnit('LazUtilsStrConsts');
    D := T.Dependencies.AddUnit('LConvEncoding');
    D := T.Dependencies.AddUnit('lcsvutils');
    D := T.Dependencies.AddUnit('LookupStringList');
    D := T.Dependencies.AddUnit('Maps');
    D := T.Dependencies.AddUnit('Masks');
    D := T.Dependencies.AddUnit('PasWString');
    D := T.Dependencies.AddUnit('StringHashList');
    D := T.Dependencies.AddUnit('TextStrings');
    D := T.Dependencies.AddUnit('Translations');
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
    D := T.Dependencies.AddUnit('TTLoad');
    D := T.Dependencies.AddUnit('TTMemory');
    D := T.Dependencies.AddUnit('TTObjs');
    D := T.Dependencies.AddUnit('TTProfile');
    D := T.Dependencies.AddUnit('TTRASTER');
    D := T.Dependencies.AddInclude('ttraster_sweep.inc');
    D := T.Dependencies.AddUnit('TTTables');
    D := T.Dependencies.AddUnit('TTTypes');
    D := T.Dependencies.AddInclude('unixfileutil.inc');
    D := T.Dependencies.AddInclude('unixlazfileutils.inc');
    D := T.Dependencies.AddInclude('unixlazutf8.inc');
    D := T.Dependencies.AddUnit('UTF8Process');
    D := T.Dependencies.AddInclude('winfileutil.inc');
    D := T.Dependencies.AddInclude('winlazfileutils.inc');
    D := T.Dependencies.AddInclude('winlazutf8.inc');
    D := T.Dependencies.AddUnit('HTML2TextRender');
    D := T.Dependencies.AddUnit('Laz_AVL_Tree');
    D := T.Dependencies.AddUnit('CompWriterPas');
    D := T.Dependencies.AddUnit('LazPasReadUtil');
    D := T.Dependencies.AddUnit('IntegerList');
    D := T.Dependencies.AddUnit('LazVersion');
    D := T.Dependencies.AddUnit('UITypes');
    D := T.Dependencies.AddUnit('GraphType');
    D := T.Dependencies.AddUnit('LazTracer');
    D := T.Dependencies.AddUnit('LazStringUtils');
    D := T.Dependencies.AddUnit('LazUTF8SysUtils');
    D := T.Dependencies.AddUnit('ObjectLists');
    D := T.Dependencies.AddUnit('TTKern');
    T := P.Targets.AddImplicitUnit('avglvltree.pas');
    T := P.Targets.AddImplicitUnit('dynamicarray.pas');
    T := P.Targets.AddImplicitUnit('dynhasharray.pp');
    T := P.Targets.AddImplicitUnit('dynqueue.pas');
    T := P.Targets.AddImplicitUnit('easylazfreetype.pas');
    T := P.Targets.AddImplicitUnit('extendedstrings.pas');
    T := P.Targets.AddImplicitUnit('fileutil.pas');
    T := P.Targets.AddImplicitUnit('fpcadds.pas');
    T := P.Targets.AddImplicitUnit('laz2_dom.pas');
    T := P.Targets.AddImplicitUnit('laz2_xmlcfg.pas');
    T := P.Targets.AddImplicitUnit('laz2_xmlread.pas');
    T := P.Targets.AddImplicitUnit('laz2_xmlutils.pas');
    T := P.Targets.AddImplicitUnit('laz2_xmlwrite.pas');
    T := P.Targets.AddImplicitUnit('laz2_xpath.pas');
    T := P.Targets.AddImplicitUnit('laz_dom.pas');
    T := P.Targets.AddImplicitUnit('laz_xmlcfg.pas');
    T := P.Targets.AddImplicitUnit('laz_xmlread.pas');
    T := P.Targets.AddImplicitUnit('laz_xmlstreaming.pas');
    T := P.Targets.AddImplicitUnit('laz_xmlwrite.pas');
    T := P.Targets.AddImplicitUnit('lazclasses.pas');
    T := P.Targets.AddImplicitUnit('lazcollections.pas');
    T := P.Targets.AddImplicitUnit('lazconfigstorage.pas');
    T := P.Targets.AddImplicitUnit('lazdbglog.pas');
    T := P.Targets.AddImplicitUnit('lazfglhash.pas');
    T := P.Targets.AddImplicitUnit('lazfilecache.pas');
    T := P.Targets.AddImplicitUnit('lazfileutils.pas');
    T := P.Targets.AddImplicitUnit('lazfreetype.pas');
    T := P.Targets.AddImplicitUnit('lazfreetypefontcollection.pas');
    T := P.Targets.AddImplicitUnit('lazfreetypefpimagedrawer.pas');
    T := P.Targets.AddImplicitUnit('lazlinkedlist.pas');
    T := P.Targets.AddImplicitUnit('lazlistclasses.pas');
    T := P.Targets.AddImplicitUnit('lazlogger.pas');
    T := P.Targets.AddImplicitUnit('lazloggerbase.pas');
    T := P.Targets.AddImplicitUnit('lazloggerdummy.pas');
    T := P.Targets.AddImplicitUnit('lazloggerprofiling.pas');
    T := P.Targets.AddImplicitUnit('lazmethodlist.pas');
    T := P.Targets.AddImplicitUnit('lazunicode.pas');
    T := P.Targets.AddImplicitUnit('lazutf16.pas');
    T := P.Targets.AddImplicitUnit('lazutf8.pas');
    T := P.Targets.AddImplicitUnit('lazutf8classes.pas');
    T := P.Targets.AddImplicitUnit('lazsysutils.pas');
    T := P.Targets.AddImplicitUnit('lazutilities.pas');
    T := P.Targets.AddImplicitUnit('lazutilsstrconsts.pas');
    T := P.Targets.AddImplicitUnit('lconvencoding.pas');
    T := P.Targets.AddImplicitUnit('lcsvutils.pas');
    T := P.Targets.AddImplicitUnit('lookupstringlist.pas');
    T := P.Targets.AddImplicitUnit('maps.pp');
    T := P.Targets.AddImplicitUnit('masks.pas');
    T := P.Targets.AddImplicitUnit('paswstring.pas');
    T := P.Targets.AddImplicitUnit('stringhashlist.pas');
    T := P.Targets.AddImplicitUnit('textstrings.pas');
    T := P.Targets.AddImplicitUnit('translations.pas');
    T := P.Targets.AddImplicitUnit('ttcache.pas');
    T := P.Targets.AddImplicitUnit('ttcalc.pas');
    T := P.Targets.AddImplicitUnit('ttcmap.pas');
    T := P.Targets.AddImplicitUnit('ttdebug.pas');
    T := P.Targets.AddImplicitUnit('tterror.pas');
    T := P.Targets.AddImplicitUnit('ttfile.pas');
    T := P.Targets.AddImplicitUnit('ttgload.pas');
    T := P.Targets.AddImplicitUnit('ttinterp.pas');
    T := P.Targets.AddImplicitUnit('ttload.pas');
    T := P.Targets.AddImplicitUnit('ttmemory.pas');
    T := P.Targets.AddImplicitUnit('ttobjs.pas');
    T := P.Targets.AddImplicitUnit('ttprofile.pas');
    T := P.Targets.AddImplicitUnit('ttraster.pas');
    T := P.Targets.AddImplicitUnit('tttables.pas');
    T := P.Targets.AddImplicitUnit('tttypes.pas');
    T := P.Targets.AddImplicitUnit('utf8process.pp');
    T := P.Targets.AddImplicitUnit('html2textrender.pas');
    T := P.Targets.AddImplicitUnit('laz_avl_tree.pp');
    T := P.Targets.AddImplicitUnit('compwriterpas.pas');
    T := P.Targets.AddImplicitUnit('lazpasreadutil.pas');
    T := P.Targets.AddImplicitUnit('integerlist.pas');
    T := P.Targets.AddImplicitUnit('lazversion.pas');
    T := P.Targets.AddImplicitUnit('uitypes.pas');
    T := P.Targets.AddImplicitUnit('graphtype.pp');
    T := P.Targets.AddImplicitUnit('laztracer.pas');
    T := P.Targets.AddImplicitUnit('lazstringutils.pas');
    T := P.Targets.AddImplicitUnit('lazutf8sysutils.pas');
    T := P.Targets.AddImplicitUnit('objectlists.pas');
    T := P.Targets.AddImplicitUnit('ttkern.pas');

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
