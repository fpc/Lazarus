{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MemDSLaz; 

interface

uses
  memds, frmSelectDataset, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('frmSelectDataset', @frmSelectDataset.Register); 
end; 

initialization
  RegisterPackage('MemDSLaz', @Register); 
end.
