{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit SynEditDesign; 

interface

uses
SynPropertyEditObjectList, SynEditDesignRegister, SynDesignStringConstants, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('SynEditDesignRegister', @SynEditDesignRegister.Register); 
end; 

initialization
  RegisterPackage('SynEditDesign', @Register); 
end.
