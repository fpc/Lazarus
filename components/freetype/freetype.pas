{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit freetype;

{$warn 5023 off : no warning about unused units}
interface

uses
  EasyLazFreeType, LazFreeType, LazFreeTypeFontCollection, 
  LazFreeTypeFPImageDrawer, TTCache, TTCalc, TTCMap, TTDebug, TTError, TTFile, 
  TTGLoad, TTInterp, TTKern, TTLoad, TTMemory, TTObjs, TTProfile, TTRASTER, 
  TTTables, TTTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('freetype', @Register);
end.
