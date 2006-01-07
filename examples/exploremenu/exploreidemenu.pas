{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit exploreidemenu; 

interface

uses
  frmexploremenu, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('frmexploremenu', @frmexploremenu.Register); 
end; 

initialization
  RegisterPackage('exploreidemenu', @Register); 
end.
