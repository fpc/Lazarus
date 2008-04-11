{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit messagecomposerpkg; 

interface

uses
  MessageComposer, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('MessageComposer', @MessageComposer.Register); 
end; 

initialization
  RegisterPackage('messagecomposerpkg', @Register); 
end.
