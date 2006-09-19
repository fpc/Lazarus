{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit DemoPackageWithHelp; 

interface

uses
  PkgHelpDemoUnit1, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('PkgHelpDemoUnit1', @PkgHelpDemoUnit1.Register); 
end; 

initialization
  RegisterPackage('DemoPackageWithHelp', @Register); 
end.
