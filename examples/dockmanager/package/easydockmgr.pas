{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit EasyDockMgr; 

interface

uses
  EasyDockSite, fDockBook, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('EasyDockMgr', @Register); 
end.
