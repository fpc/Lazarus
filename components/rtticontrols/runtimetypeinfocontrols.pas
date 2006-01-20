{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit RunTimeTypeInfoControls; 

interface

uses
  RTTICtrls, RTTIGrids, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RTTICtrls', @RTTICtrls.Register); 
  RegisterUnit('RTTIGrids', @RTTIGrids.Register); 
end; 

initialization
  RegisterPackage('RunTimeTypeInfoControls', @Register); 
end.
