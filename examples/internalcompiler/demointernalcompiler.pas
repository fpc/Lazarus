{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DemoInternalCompiler;

{$warn 5023 off : no warning about unused units}
interface

uses
  DemoInternalCompilerReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DemoInternalCompilerReg', @DemoInternalCompilerReg.Register);
end;

initialization
  RegisterPackage('DemoInternalCompiler', @Register);
end.
