{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package ibconnectionlaz 0.9.5.
 }

unit ibconnectionlaz; 

interface

uses
  ibconnection, registeribconnection, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registeribconnection', @registeribconnection.Register); 
end; 

initialization
  RegisterPackage('ibconnectionlaz', @Register); 
end.
