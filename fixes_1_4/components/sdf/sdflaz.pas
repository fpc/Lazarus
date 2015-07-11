{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SDFLaz; 

interface

uses
  RegisterSDF, sdfdata, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('RegisterSDF', @RegisterSDF.Register); 
end; 

initialization
  RegisterPackage('SDFLaz', @Register); 
end.
