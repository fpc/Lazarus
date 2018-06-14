{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDebuggerFpLldb;

{$warn 5023 off : no warning about unused units}
interface

uses
  FpLldbDebugger, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FpLldbDebugger', @FpLldbDebugger.Register);
end;

initialization
  RegisterPackage('LazDebuggerFpLldb', @Register);
end.
