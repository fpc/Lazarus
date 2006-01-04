{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit SynUni; 

interface

uses
  synunidesigner, synunihighlighter, synunireg, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('synunihighlighter', @synunihighlighter.Register); 
  RegisterUnit('synunireg', @synunireg.Register); 
end; 

initialization
  RegisterPackage('SynUni', @Register); 
end.
