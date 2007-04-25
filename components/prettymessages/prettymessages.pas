{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit PrettyMessages; 

interface

uses
  HideFPCHints, PrettyMsgOptionsDlg, PrettyMsgOptions, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('HideFPCHints', @HideFPCHints.Register); 
end; 

initialization
  RegisterPackage('PrettyMessages', @Register); 
end.
