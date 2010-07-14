{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ChmHelpPkg; 

interface

uses
  LazChmHelp, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LazChmHelp', @LazChmHelp.Register); 
end; 

initialization
  RegisterPackage('ChmHelpPkg', @Register); 
end.
