{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package MemDSLaz 0.1.1.
}

unit MemDSLaz; 

interface

uses
  registermemds, memds, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registermemds', @registermemds.Register); 
end; 

initialization
  RegisterPackage('MemDSLaz', @Register)
end.
