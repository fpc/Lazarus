{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package pqconnectionlaz 0.9.5.
 }

unit pqconnectionlaz; 

interface

uses
  pqconnection, registerpqconnection, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registerpqconnection', @registerpqconnection.Register); 
end; 

initialization
  RegisterPackage('pqconnectionlaz', @Register); 
end.
