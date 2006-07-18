{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit H2PasWizard; 

interface

uses
  H2PasDlg, H2PasStrConsts, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('H2PasDlg', @H2PasDlg.Register); 
end; 

initialization
  RegisterPackage('H2PasWizard', @Register); 
end.
