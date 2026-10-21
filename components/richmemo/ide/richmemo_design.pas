{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit richmemo_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  RtfEditPropDialog, richmemoregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('richmemoregister', @richmemoregister.Register);
end;

initialization
  RegisterPackage('richmemo_design', @Register);
end.
