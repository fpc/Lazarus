{ Este arquivo foi automaticamente criado pelo Lazarus. Não edite!
  Este fonte é usado apenas para compilar e instalar o pacote.
 }

unit lazdbexport; 

interface

uses
    regdbexport, fpdataexporter, frmSelectExportFormat, frmexportprogress, 
  frmBaseConfigExport, sdb_consts, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('regdbexport', @regdbexport.Register); 
end; 

initialization
  RegisterPackage('lazdbexport', @Register); 
end.
