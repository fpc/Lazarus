{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit lazparadox; 

interface

uses
  regparadox, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('regparadox', @regparadox.Register); 
end; 

initialization
  RegisterPackage('lazparadox', @Register); 
end.
