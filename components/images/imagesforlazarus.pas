{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit ImagesForLazarus; 

interface

uses
  lazpng, lazpnm, lazjpg, lazbmp, laztga, lazxpm, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('lazpnm', @lazpnm.Register); 
  RegisterUnit('lazjpg', @lazjpg.Register); 
  RegisterUnit('laztga', @laztga.Register); 
end; 

initialization
  RegisterPackage('ImagesForLazarus', @Register); 
end.
