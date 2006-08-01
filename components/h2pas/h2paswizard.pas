{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit H2PasWizard; 

interface

uses
  H2PasConvert, H2PasDlg, H2PasStrConsts, IDETextConvListEdit, 
    IDETextConvListAdd, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('H2PasDlg', @H2PasDlg.Register); 
end; 

initialization
  RegisterPackage('H2PasWizard', @Register); 
end.
