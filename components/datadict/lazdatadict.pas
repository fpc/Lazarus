{ Este arquivo foi automaticamente criado pelo Lazarus. Não edite!
  Este fonte é usado apenas para compilar e instalar o pacote.
 }

unit lazdatadict; 

interface

uses
    frmconfprojdatadict, idedatadict, reglazdatadict, frmconfdatadict, 
  frmSelectCodeGenerator, fpcodegenerator, frmBaseConfigCodeGenerator, 
  frmgeneratedcode, ldd_consts, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('reglazdatadict', @reglazdatadict.Register); 
end; 

initialization
  RegisterPackage('lazdatadict', @Register); 
end.
