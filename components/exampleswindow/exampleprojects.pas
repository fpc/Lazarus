{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit exampleprojects;

{$warn 5023 off : no warning about unused units}
interface

uses
    uIntf, uConst, uLaz_Examples, uexampledata, exwinsettings, 
    LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uIntf', @uIntf.Register);
end;

initialization
  RegisterPackage('exampleprojects', @Register);
end.
