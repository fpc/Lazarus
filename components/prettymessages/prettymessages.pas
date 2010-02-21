{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit prettymessages; 

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
