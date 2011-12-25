{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PoChecker; 

interface

uses
  ResultDlg, PoFamilies, pocheckermain, SimplePoFiles, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('pocheckermain', @pocheckermain.Register); 
end; 

initialization
  RegisterPackage('PoChecker', @Register); 
end.
