{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazgoogleapis;

interface

uses
  reggoogleapi, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reggoogleapi', @reggoogleapi.Register);
end;

initialization
  RegisterPackage('lazgoogleapis', @Register);
end.
