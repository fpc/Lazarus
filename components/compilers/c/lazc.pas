{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lazc; 

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
