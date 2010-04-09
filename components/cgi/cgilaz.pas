{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit cgiLaz; 

interface

uses
  cgiModules, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('cgiLaz', @Register); 
end.
