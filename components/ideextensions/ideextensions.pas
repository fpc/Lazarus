{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ideextensions; 

interface

uses
  IdeGroupBox, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('IdeGroupBox', @IdeGroupBox.Register); 
end; 

initialization
  RegisterPackage('IdeExtensions', @Register); 
end.
