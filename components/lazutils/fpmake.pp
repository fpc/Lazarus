{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for LazUtils 1.0

   This file was generated on 29-12-18
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

    P.Dependencies.Add('fcl-image');
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
    t.Dependencies.AddUnit('AvgLvlTree');
    t.Dependencies.AddUnit('DynamicArray');
    t.Dependencies.AddUnit('DynHashArray');
    t.Dependencies.AddUnit('DynQueue');
    t.Dependencies.AddUnit('EasyLazFreeType');
    t.Dependencies.AddUnit('ExtendedStrings');
    t.Dependencies.AddUnit('FileUtil');
    t.Dependencies.AddUnit('FPCAdds');
    t.Dependencies.AddUnit('Laz2_DOM');
    t.Dependencies.AddUnit('Laz2_XMLCfg');
    t.Dependencies.AddUnit('laz2_XMLRead');
    t.Dependencies.AddUnit('laz2_xmlutils');
    t.Dependencies.AddUnit('laz2_XMLWrite');
    t.Dependencies.AddUnit('laz2_xpath');
    t.Dependencies.AddUnit('Laz_DOM');
    t.Dependencies.AddUnit('Laz_XMLCfg');
    t.Dependencies.AddUnit('Laz_XMLRead');
    t.Dependencies.AddUnit('Laz_XMLStreaming');
    t.Dependencies.AddUnit('Laz_XMLWrite');
    t.Dependencies.AddUnit('LazClasses');
    t.Dependencies.AddUnit('lazCollections');
    t.Dependencies.AddUnit('LazConfigStorage');
    t.Dependencies.AddUnit('LazDbgLog');
    t.Dependencies.AddUnit('lazfglhash');
    t.Dependencies.AddUnit('LazFileCache');
    t.Dependencies.AddUnit('LazFileUtils');
    t.Dependencies.AddUnit('LazFreeType');
    t.Dependencies.AddUnit('LazFreeTypeFontCollection');
    t.Dependencies.AddUnit('LazFreeTypeFPImageDrawer');
    t.Dependencies.AddUnit('LazLinkedList');
    t.Dependencies.AddUnit('LazListClasses');
    t.Dependencies.AddUnit('LazLogger');
    t.Dependencies.AddUnit('LazLoggerBase');
    t.Dependencies.AddUnit('LazLoggerDummy');
    t.Dependencies.AddUnit('LazLoggerProfiling');
    t.Dependencies.AddUnit('LazMethodList');
    t.Dependencies.AddUnit('LazUnicode');
    t.Dependencies.AddUnit('LazUTF16');
    t.Dependencies.AddUnit('LazUTF8');
    t.Dependencies.AddUnit('LazUTF8Classes');
    t.Dependencies.AddUnit('LazSysUtils');
    t.Dependencies.AddUnit('LazUtilities');
    t.Dependencies.AddUnit('LazUtilsStrConsts');
    t.Dependencies.AddUnit('LConvEncoding');
    t.Dependencies.AddUnit('lcsvutils');
    t.Dependencies.AddUnit('LookupStringList');
    t.Dependencies.AddUnit('Maps');
    t.Dependencies.AddUnit('Masks');
    t.Dependencies.AddUnit('PasWString');
    t.Dependencies.AddUnit('StringHashList');
    t.Dependencies.AddUnit('TextStrings');
    t.Dependencies.AddUnit('Translations');
    t.Dependencies.AddUnit('TTCache');
    t.Dependencies.AddUnit('TTCalc');
    t.Dependencies.AddUnit('TTCMap');
    t.Dependencies.AddUnit('TTDebug');
    t.Dependencies.AddUnit('TTError');
    t.Dependencies.AddUnit('TTFile');
    t.Dependencies.AddUnit('TTGLoad');
    t.Dependencies.AddUnit('TTInterp');
    t.Dependencies.AddUnit('TTLoad');
    t.Dependencies.AddUnit('TTMemory');
    t.Dependencies.AddUnit('TTObjs');
    t.Dependencies.AddUnit('TTProfile');
    t.Dependencies.AddUnit('TTRASTER');
    t.Dependencies.AddUnit('TTTables');
    t.Dependencies.AddUnit('TTTypes');
    t.Dependencies.AddUnit('UTF8Process');
    t.Dependencies.AddUnit('HTML2TextRender');
    t.Dependencies.AddUnit('Laz_AVL_Tree');
    t.Dependencies.AddUnit('CompWriterPas');
    t.Dependencies.AddUnit('LazPasReadUtil');
    t.Dependencies.AddUnit('IntegerList');
    t.Dependencies.AddUnit('LazVersion');
    t.Dependencies.AddUnit('UITypes');
    t.Dependencies.AddUnit('GraphType');
    t.Dependencies.AddUnit('LazTracer');
    t.Dependencies.AddUnit('LazStringUtils');
    t.Dependencies.AddUnit('LazUTF8SysUtils');
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
