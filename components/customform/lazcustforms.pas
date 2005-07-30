{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit LazCustForms; 

interface

uses
  custforms, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('custforms', @custforms.Register); 
end; 

initialization
  RegisterPackage('LazCustForms', @Register); 
end.
