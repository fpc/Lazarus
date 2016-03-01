{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit printers4lazide;

{$warn 5023 off : no warning about unused units}
interface

uses
  ideprinting, SourcePrinter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ideprinting', @ideprinting.Register);
end;

initialization
  RegisterPackage('printers4lazide', @Register);
end.
