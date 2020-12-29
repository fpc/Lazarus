{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  AvgLvlTree, CodepagesAsian, CodepagesCommon, CompWriterPas, DynamicArray, 
  DynHashArray, DynQueue, ExtendedStrings, FileReferenceList, FileUtil, 
  FPCAdds, GraphMath, GraphType, HTML2TextRender, IntegerList, Laz2_DOM, 
  Laz2_XMLCfg, laz2_XMLRead, laz2_xmlutils, laz2_XMLWrite, laz2_xpath, 
  Laz_AVL_Tree, Laz_DOM, Laz_XMLCfg, Laz_XMLRead, Laz_XMLStreaming, 
  Laz_XMLWrite, LazClasses, lazCollections, LazConfigStorage, LazDbgLog, 
  lazfglhash, LazFileCache, LazFileUtils, LazLinkedList, LazListClasses, 
  LazLogger, LazLoggerBase, LazLoggerDummy, LazLoggerProfiling, LazMethodList, 
  LazPasReadUtil, LazStringUtils, LazSysUtils, LazTracer, LazUnicode, 
  LazUTF16, LazUTF8, LazUTF8Classes, LazUTF8SysUtils, LazUtilities, 
  LazUtilsStrConsts, LazVersion, LConvEncoding, lcsvutils, LookupStringList, 
  Maps, Masks, ObjectLists, StringHashList, TextStrings, Translations, 
  UITypes, UTF8Process, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazUtils', @Register);
end.
