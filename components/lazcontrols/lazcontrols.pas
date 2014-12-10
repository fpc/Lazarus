{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazControls;

interface

uses
  CheckBoxThemed, DividerBevel, ExtendedNotebook, ListFilterEdit, 
  ListViewFilterEdit, TreeFilterEdit, ShortPathEdit, LvlGraphCtrl, 
  ExtendedTabControls, LazarusPackageIntf;

implementation

{$R *.res}

procedure Register;
begin
  RegisterUnit('CheckBoxThemed', @CheckBoxThemed.Register);
  RegisterUnit('DividerBevel', @DividerBevel.Register);
  RegisterUnit('ExtendedNotebook', @ExtendedNotebook.Register);
  RegisterUnit('ListFilterEdit', @ListFilterEdit.Register);
  RegisterUnit('ListViewFilterEdit', @ListViewFilterEdit.Register);
  RegisterUnit('TreeFilterEdit', @TreeFilterEdit.Register);
  RegisterUnit('ShortPathEdit', @ShortPathEdit.Register);
  RegisterUnit('LvlGraphCtrl', @LvlGraphCtrl.Register);
end;

initialization
  RegisterPackage('LazControls', @Register);
end.
