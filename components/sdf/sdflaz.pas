{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package SDFLaz 0.1.1.
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
  RegisterPackage('SDFLaz', @Register)
end.
