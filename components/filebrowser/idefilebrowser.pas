{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IDEFileBrowser;

{$warn 5023 off : no warning about unused units}
interface

uses
  frmFileBrowser, RegIDEFileBrowser, frmConfigFileBrowser, CtrlFileBrowser, 
  FileBrowserTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegIDEFileBrowser', @RegIDEFileBrowser.Register);
end;

initialization
  RegisterPackage('IDEFileBrowser', @Register);
end.
