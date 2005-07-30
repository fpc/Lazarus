{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit appforms; 

interface

uses
  AppForm, dbappform, regappforms, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('appforms', @Register); 
end.
