{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit JPEGForLazarus; 

interface

uses
  LazJPEG, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('JPEGForLazarus', @Register); 
end.
