{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit Printer4Lazarus; 

interface

uses
  printersdlgs, osprinters, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('printersdlgs', @printersdlgs.Register); 
end; 

initialization
  RegisterPackage('Printer4Lazarus', @Register); 
end.
