{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LCL; 

interface

uses
  LCLIntfCompile, AllLCLIntfUnits, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LCLIntfCompile', @LCLIntfCompile.Register); 
end; 

initialization
  RegisterPackage('LCL', @Register); 
end.
