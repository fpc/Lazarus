{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit turbopoweriprodsgn; 

interface

uses
  IPIDEHTMLControl, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('IPIDEHTMLControl', @IPIDEHTMLControl.Register); 
end; 

initialization
  RegisterPackage('TurboPowerIProDsgn', @Register); 
end.
