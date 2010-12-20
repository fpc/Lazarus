{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit EasyDockMgr; 

interface

uses
    EasyDockSite, fDockBook, fFloatingSite, uMiniRestore, uMakeSite, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('EasyDockMgr', @Register); 
end.
