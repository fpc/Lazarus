{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazdaemon;

{$warn 5023 off : no warning about unused units}
interface

uses
  RegLazDaemon, daemonapp, lazdaemonapp, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegLazDaemon', @RegLazDaemon.Register);
end;

initialization
  RegisterPackage('lazdaemon', @Register);
end.
