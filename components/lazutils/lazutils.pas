{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazUtils;

interface

uses
  Laz2_DOM, Laz2_XMLCfg, laz2_XMLRead, laz2_xmlutils, laz2_XMLWrite, Laz_DOM, Laz_XMLCfg, 
  Laz_XMLRead, Laz_XMLStreaming, Laz_XMLWrite, LazFileUtils, LazFileCache, LazUTF8, 
  LazDbgLog, paswstring, FileUtil, LazUTF8Classes, Masks, LazUtilsStrConsts, LConvEncoding, 
  lazutf16, lazutf8sysutils, LazMethodList, AvgLvlTree, LazLogger, LazFreeType, TTCache, 
  TTCalc, TTCMap, TTDebug, TTError, TTFile, TTGLoad, TTInterp, TTLoad, TTMemory, TTObjs, 
  TTProfile, TTRASTER, TTTables, TTTypes, EasyLazFreeType, LazLoggerBase, LazLoggerDummy, 
  LazClasses, LazFreeTypeFontCollection, LazConfigStorage, UTF8Process, laz2_xpath, 
  DictionaryStringList,  LazLoggerProfiling, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UTF8Process', @UTF8Process.Register);
end;

initialization
  RegisterPackage('LazUtils', @Register);
end.
