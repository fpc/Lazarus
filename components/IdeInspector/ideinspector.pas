{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdeInspector; 

interface

uses
  MainInspector, IdeInspectKeyGrapper, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('MainInspector', @MainInspector.Register); 
end; 

initialization
  RegisterPackage('IdeInspector', @Register); 
end.
