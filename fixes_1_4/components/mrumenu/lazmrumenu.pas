{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazmrumenu; 

interface

uses
  reglazmru, mrumanager, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('reglazmru', @reglazmru.Register); 
end; 

initialization
  RegisterPackage('lazmrumenu', @Register); 
end.
