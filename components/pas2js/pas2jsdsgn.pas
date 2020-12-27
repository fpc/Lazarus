{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Pas2JSDsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  PJSDsgnRegister, PJSDsgnOptsFrame, frmpas2jsbrowserprojectoptions, 
  PJSDsgnOptions, frmpas2jsnodejsprojectoptions, pjscontroller, 
  frmpas2jswebservers, strpas2jsdesign, pjsprojectoptions, 
  frmPas2jsAtomPackageSettings, regpas2jsatom, regpas2jsvscode, 
  frmPas2jsVSCodeExtensionSettings, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PJSDsgnRegister', @PJSDsgnRegister.Register);
  RegisterUnit('regpas2jsatom', @regpas2jsatom.Register);
  RegisterUnit('regpas2jsvscode', @regpas2jsvscode.Register);
end;

initialization
  RegisterPackage('Pas2JSDsgn', @Register);
end.
