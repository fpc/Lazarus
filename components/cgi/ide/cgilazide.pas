{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit CGILazIDE; 

interface

uses
  CGILazIDEIntf, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('CGILazIDEIntf', @CGILazIDEIntf.Register); 
end; 

initialization
  RegisterPackage('CGILazIDE', @Register); 
end.
