{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ideextensions; 

interface

uses
  DividerBevel, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DividerBevel', @DividerBevel.Register); 
end; 

initialization
  RegisterPackage('IdeExtensions', @Register); 
end.
