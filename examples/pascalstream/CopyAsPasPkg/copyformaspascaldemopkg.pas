{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CopyFormAsPascalDemoPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  CopyAsPasDemoUnit1, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CopyAsPasDemoUnit1', @CopyAsPasDemoUnit1.Register);
end;

initialization
  RegisterPackage('CopyFormAsPascalDemoPkg', @Register);
end.
