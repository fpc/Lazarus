{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package DBFLaz 0.1.1.
}

unit DBFLaz; 

interface

uses
  RegisterDBF, dbf, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RegisterDBF', @RegisterDBF.Register); 
end; 

initialization
  RegisterPackage('DBFLaz', @Register)
end.
