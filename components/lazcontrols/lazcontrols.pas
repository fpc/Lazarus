{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  CheckBoxThemed, DividerBevel, ExtendedNotebook, ListFilterEdit, 
  ListViewFilterEdit, TreeFilterEdit, ShortPathEdit, LvlGraphCtrl, 
  ExtendedTabControls, SpinEx, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazControls', @Register);
end.
