{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazchatctrl;

{$warn 5023 off : no warning about unused units}
interface

uses
  chatcontrol, regchatctrls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regchatctrls', @regchatctrls.Register);
end;

initialization
  RegisterPackage('lazchatctrl', @Register);
end.
