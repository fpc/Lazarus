{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit popupnotifierlaz; 

interface

uses
  popupnotifier, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('popupnotifier', @popupnotifier.Register); 
end; 

initialization
  RegisterPackage('popupnotifierlaz', @Register); 
end.
