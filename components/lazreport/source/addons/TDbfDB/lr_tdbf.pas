{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LR_TDbf;

{$warn 5023 off : no warning about unused units}
interface

uses
  lrTDbfData, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lrTDbfData', @lrTDbfData.Register);
end;

initialization
  RegisterPackage('LR_TDbf', @Register);
end.
