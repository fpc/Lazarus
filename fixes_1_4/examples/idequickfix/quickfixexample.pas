{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit QuickFixExample; 

interface

uses
  QuickFixDemo1, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('QuickFixDemo1', @QuickFixDemo1.Register); 
end; 

initialization
  RegisterPackage('QuickFixExample', @Register); 
end.
