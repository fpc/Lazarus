{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit EasyDockMgrDsgn; 

interface

uses
  RegisterEasyDockMgr, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('RegisterEasyDockMgr', @RegisterEasyDockMgr.Register); 
end; 

initialization
  RegisterPackage('EasyDockMgrDsgn', @Register); 
end.
