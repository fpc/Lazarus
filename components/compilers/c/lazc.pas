{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit LazC; 

interface

uses
  LazCUtil, LazCStrConsts, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('LazCUtil', @LazCUtil.Register); 
end; 

initialization
  RegisterPackage('LazC', @Register); 
end.
