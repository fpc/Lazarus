{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NotLCLDesigner;

{$warn 5023 off : no warning about unused units}
interface

uses
  MyWidgetDesigner, MyWidgetSet, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MyWidgetDesigner', @MyWidgetDesigner.Register);
end;

initialization
  RegisterPackage('NotLCLDesigner', @Register);
end.
