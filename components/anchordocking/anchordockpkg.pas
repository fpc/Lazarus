{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit anchordockpkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  AnchorDocking, AnchorDockStorage, AnchorDockStr, AnchorDockOptionsDlg, 
  AnchorDockPanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AnchorDockPanel', @AnchorDockPanel.Register);
end;

initialization
  RegisterPackage('AnchorDocking', @Register);
end.
