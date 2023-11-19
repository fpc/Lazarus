{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpcunitide;

{$warn 5023 off : no warning about unused units}
interface

uses
  FPCUnitLazIDEIntf, strtestcaseopts, testcaseopts, fraTestInsightOpts, 
  frmtestinsight, RegTestInsight, testinsightcontroller, testinsightserver, 
  frmConsoleOpts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FPCUnitLazIDEIntf', @FPCUnitLazIDEIntf.Register);
  RegisterUnit('RegTestInsight', @RegTestInsight.Register);
end;

initialization
  RegisterPackage('fpcunitide', @Register);
end.
