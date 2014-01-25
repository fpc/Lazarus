{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazActiveX;

interface

uses
  activexcontainer, lazactivexreg, ImportTypelib, activexstrconsts, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lazactivexreg', @lazactivexreg.Register);
end;

initialization
  RegisterPackage('LazActiveX', @Register);
end.
