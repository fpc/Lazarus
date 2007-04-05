{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit jumpto; 

interface

uses
  jumpto_impl, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('jumpto_impl', @jumpto_impl.Register); 
end; 

initialization
  RegisterPackage('jumpto', @Register); 
end.
