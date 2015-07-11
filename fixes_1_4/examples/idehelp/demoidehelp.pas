{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DemoIDEHelp;

interface

uses
  MyIDEHelp, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MyIDEHelp', @MyIDEHelp.Register);
end;

initialization
  RegisterPackage('DemoIDEHelp', @Register);
end.
