{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit externhelp; 

interface

uses
  ExternHelpFrm, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ExternHelpFrm', @ExternHelpFrm.Register); 
end; 

initialization
  RegisterPackage('ExternHelp', @Register); 
end.
