{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit h2paswizard; 

interface

uses
    H2PasConvert, H2PasDlg, H2PasStrConsts, IDETextConvListEdit, 
  IDETextConvListAdd, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('H2PasDlg', @H2PasDlg.Register); 
end; 

initialization
  RegisterPackage('H2PasWizard', @Register); 
end.
