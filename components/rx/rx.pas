{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package rx 1.0.
 }

unit rx; 

interface

uses
  AppUtils, MRUList, Placement, StrHolder, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('MRUList', @MRUList.Register); 
  RegisterUnit('StrHolder', @StrHolder.Register); 
end; 

initialization
  RegisterPackage('rx', @Register)
end.
