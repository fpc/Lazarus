{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDebuggerFPDServer;

interface

uses
  FPDServerDebugger, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FPDServerDebugger', @FPDServerDebugger.Register);
end;

initialization
  RegisterPackage('LazDebuggerFPDServer', @Register);
end.
