{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Pas2JSDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  PJSDsgnRegister, PJSDsgnOptsFrame, frmpas2jsbrowserprojectoptions, 
  PJSDsgnOptions, frmpas2jsnodejsprojectoptions, pjscontroller, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PJSDsgnRegister', @PJSDsgnRegister.Register);
end;

initialization
  RegisterPackage('Pas2JSDsgn', @Register);
end.
