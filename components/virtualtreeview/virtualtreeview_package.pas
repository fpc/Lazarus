{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit virtualtreeview_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  VirtualTrees, VTHeaderPopup, registervirtualtreeview, VTGraphics, 
  VTIDEEditors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registervirtualtreeview', @registervirtualtreeview.Register);
end;

initialization
  RegisterPackage('virtualtreeview_package', @Register);
end.
