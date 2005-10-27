{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit prettyformat; 

interface

uses
  PtoPu, pfidesource, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('pfidesource', @pfidesource.Register); 
end; 

initialization
  RegisterPackage('prettyformat', @Register); 
end.
