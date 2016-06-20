{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  Laz2_DOM, Laz2_XMLCfg, laz2_XMLRead, laz2_xmlutils, laz2_XMLWrite, Laz_DOM,
  Laz_XMLCfg, Laz_XMLRead, Laz_XMLStreaming, Laz_XMLWrite, LazFileUtils,
  LazFileCache, LazUTF8, LazDbgLog, PasWString, FileUtil, LazUTF8Classes,
  Masks, LazUtilsStrConsts, LConvEncoding, LazUTF16, LazUTF8SysUtils,
  LazMethodList, AvgLvlTree, LazLogger, LazFreeType, TTCache, TTCalc, TTCMap,
  TTDebug, TTError, TTFile, TTGLoad, TTInterp, TTLoad, TTMemory, TTObjs,
  TTProfile, TTRASTER, TTTables, TTTypes, EasyLazFreeType, LazLoggerBase,
  LazLoggerDummy, LazClasses, LazFreeTypeFontCollection, LazConfigStorage,
  UTF8Process, laz2_xpath, LazLoggerProfiling, FPCAdds, LazUtilities,
  lazfglhash, lcsvutils, lazCollections, LazListClasses,
  LazFreeTypeFPImageDrawer, LookupStringList, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UTF8Process', @UTF8Process.Register);
end;

initialization
  RegisterPackage('LazUtils', @Register);
end.
