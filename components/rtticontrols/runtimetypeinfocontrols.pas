{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package RunTimeTypeInfoControls 0.1.
 }

unit RunTimeTypeInfoControls; 

interface

uses
  RTTICtrls, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RTTICtrls', @RTTICtrls.Register); 
end; 

initialization
  RegisterPackage('RunTimeTypeInfoControls', @Register)
end.
