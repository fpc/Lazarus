{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit EasyDockMgr; 

interface

uses
    EasyDockSite, fDockBook, fFloatingSite, fElasticSite, uMiniRestore, 
  fPageFrame, uMakeSite, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('fElasticSite', @fElasticSite.Register); 
end; 

initialization
  RegisterPackage('EasyDockMgr', @Register); 
end.
