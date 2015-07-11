{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit JumpToImplementation; 

interface

uses
  CodeToolsExample1, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('CodeToolsExample1', @CodeToolsExample1.Register); 
end; 

initialization
  RegisterPackage('JumpToImplementation', @Register); 
end.
