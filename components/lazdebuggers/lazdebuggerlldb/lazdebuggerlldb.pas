{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDebuggerLldb;

{$warn 5023 off : no warning about unused units}
interface

uses
  LldbDebugger, LldbInstructions, LldbHelper, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LldbDebugger', @LldbDebugger.Register);
end;

initialization
  RegisterPackage('LazDebuggerLldb', @Register);
end.
