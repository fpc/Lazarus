{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazdebuggerfpremote;

{$warn 5023 off : no warning about unused units}
interface

uses
  RemoteDebugger, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RemoteDebugger', @RemoteDebugger.Register);
end;

initialization
  RegisterPackage('lazdebuggerfpremote', @Register);
end.
