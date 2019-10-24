{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PoChecker;

{$warn 5023 off : no warning about unused units}
interface

uses
  ResultDlg, PoFamilies, pocheckermain, pocheckerconsts, GraphStat, 
  PoCheckerSettings, PoCheckerXMLConfig, PoCheckerMemoDlg, PoFamilyLists, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pocheckermain', @pocheckermain.Register);
end;

initialization
  RegisterPackage('PoChecker', @Register);
end.
