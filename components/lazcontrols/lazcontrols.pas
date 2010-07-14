{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazControls; 

interface

uses
  DividerBevel, ExtendedNotebook, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DividerBevel', @DividerBevel.Register); 
  RegisterUnit('ExtendedNotebook', @ExtendedNotebook.Register); 
end; 

initialization
  RegisterPackage('LazControls', @Register); 
end.
