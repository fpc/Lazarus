{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package InterbaseLaz 1.0.
 }

unit InterbaseLaz; 

interface

uses
  RegisterInterbase, Interbase, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RegisterInterbase', @RegisterInterbase.Register); 
end; 

initialization
  RegisterPackage('InterbaseLaz', @Register)
end.
