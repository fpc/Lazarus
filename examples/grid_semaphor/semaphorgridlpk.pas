{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package SemaphorGridLPK 1.0.
 }

unit SemaphorGridLPK; 

interface

uses
  SemaphorGrids, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('SemaphorGrids', @SemaphorGrids.Register); 
end; 

initialization
  RegisterPackage('SemaphorGridLPK', @Register); 
end.
