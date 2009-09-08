{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit NotLCLDesigner; 

interface

uses
  MyWidgetDesigner, MyWidgetSet, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('MyWidgetDesigner', @MyWidgetDesigner.Register); 
end; 

initialization
  RegisterPackage('NotLCLDesigner', @Register); 
end.
