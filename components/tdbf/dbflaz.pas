{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DBFLaz; 

interface

uses
  RegisterDBF, Dbf, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('RegisterDBF', @RegisterDBF.Register); 
end; 

initialization
  RegisterPackage('DBFLaz', @Register); 
end.
