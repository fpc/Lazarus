{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package Sqldblaz 0.9.5.
 }

unit Sqldblaz; 

interface

uses
  registersqldb, sqldb, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registersqldb', @registersqldb.Register); 
end; 

initialization
  RegisterPackage('Sqldblaz', @Register); 
end.
