{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lazplotfunction; 

interface

uses
regplotpanel, exprplotpanel, plotpanel, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('regplotpanel', @regplotpanel.Register); 
end; 

initialization
  RegisterPackage('lazplotfunction', @Register); 
end.
