{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ImagesForLazarus; 

interface

uses
  LazPNG, LazPNM, LazJPG, LazBMP, LazTGA, LazXPM, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LazTGA', @LazTGA.Register); 
end; 

initialization
  RegisterPackage('ImagesForLazarus', @Register); 
end.
