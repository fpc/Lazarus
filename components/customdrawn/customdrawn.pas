{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit customdrawn; 

interface

uses
  customdrawnextras, customdrawnutils, customdrawncontrols, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('customdrawnextras', @customdrawnextras.Register); 
end; 

initialization
  RegisterPackage('customdrawn', @Register); 
end.
