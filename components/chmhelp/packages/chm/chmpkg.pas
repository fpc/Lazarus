{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit chmpkg; 

interface

uses
  chmbase, chmreader, paslzx, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('chmpkg', @Register); 
end.
