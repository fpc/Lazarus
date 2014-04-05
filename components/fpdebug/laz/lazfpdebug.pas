{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazFpDebug;

interface

uses
  FpDebugDebugger, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FpDebugDebugger', @FpDebugDebugger.Register);
end;

initialization
  RegisterPackage('LazFpDebug', @Register);
end.
