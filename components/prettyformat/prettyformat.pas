{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit prettyformat; 

interface

uses
  PtoPu, pfidesource, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('pfidesource', @pfidesource.Register); 
end; 

initialization
  RegisterPackage('prettyformat', @Register); 
end.
