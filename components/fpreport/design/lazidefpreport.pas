{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazidefpreport;

{$warn 5023 off : no warning about unused units}
interface

uses
  lazideregfpreport, frmideselectreportdata, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lazideregfpreport', @lazideregfpreport.Register);
end;

initialization
  RegisterPackage('lazidefpreport', @Register);
end.
