{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazplotfunction;

{$warn 5023 off : no warning about unused units}
interface

uses
  regplotpanel, exprplotpanel, plotpanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regplotpanel', @regplotpanel.Register);
end;

initialization
  RegisterPackage('lazplotfunction', @Register);
end.
