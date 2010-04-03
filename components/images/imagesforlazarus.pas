{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit imagesforlazarus; 

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
