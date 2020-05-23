{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpdebug;

{$warn 5023 off : no warning about unused units}
interface

uses
  FpDbgClasses, FpDbgDisasX86, FpDbgDwarf, FpDbgDwarfConst, FpDbgLoader, FpDbgPETypes, FpDbgSymbols, 
  FpDbgUtil, FpImgReaderWinPE, FpImgReaderElf, FpImgReaderElfTypes, FpImgReaderBase, FpPascalParser, 
  macho, FpImgReaderMachoFile, FpImgReaderMacho, FpPascalBuilder, FpDbgInfo, FpdMemoryTools, 
  FpErrorMessages, FPDbgController, FpDbgDwarfVerbosePrinter, FpDbgDwarfDataClasses, 
  FpDbgDwarfFreePascal, fpDbgSymTableContext, fpDbgSymTable, FpDbgAvrClasses, FpDbgDisasAvr, 
  FpDbgRsp, FpDbgCommon, FpImgReaderWinPETypes, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fpdebug', @Register);
end.
