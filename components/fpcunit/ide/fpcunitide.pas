{ Este arquivo foi automaticamente criado pelo Lazarus. Não edite!
  Este fonte é usado apenas para compilar e instalar o pacote.
 }

unit fpcunitide; 

interface

uses
  FPCUnitLazIDEIntf, strtestcaseopts, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FPCUnitLazIDEIntf', @FPCUnitLazIDEIntf.Register); 
end; 

initialization
  RegisterPackage('fpcunitide', @Register); 
end.
