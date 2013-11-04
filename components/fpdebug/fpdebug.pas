{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpdebug;

interface

uses
  FpDbgClasses, FpDbgDisasX86, FpDbgDwarf, FpDbgDwarfConst, FpDbgLoader, FpDbgPETypes, 
  FpDbgSymbols, FpDbgUtil, FpDbgWinExtra, FpImgReaderWinPE, FpImgReaderElf, 
  FpImgReaderElfTypes, FpImgReaderBase, FpPascalParser, macho, FpImgReaderMachoFile, 
  FpImgReaderMacho, FpPascalBuilder, FpDbgInfo, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fpdebug', @Register);
end.
