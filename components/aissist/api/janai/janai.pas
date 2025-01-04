{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit janai;

{$warn 5023 off : no warning about unused units}
interface

uses
  janai_v1, janaiprotocol, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('janai', @Register);
end.
