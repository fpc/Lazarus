{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdeLazLogger;

interface

uses
  idelogger, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('idelogger', @idelogger.Register);
end;

initialization
  RegisterPackage('IdeLazLogger', @Register);
end.
