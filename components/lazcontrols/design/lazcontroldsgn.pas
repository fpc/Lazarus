{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazControlDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  RegisterLazControls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterLazControls', @RegisterLazControls.Register);
end;

initialization
  RegisterPackage('LazControlDsgn', @Register);
end.
