{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit xmlresource;

interface

uses
  xmlresourcefile, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('xmlresourcefile', @xmlresourcefile.Register);
end;

initialization
  RegisterPackage('xmlresource', @Register);
end.
{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit xmlresource;

interface

uses
  xmlresourcefile, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('xmlresourcefile', @xmlresourcefile.Register);
end;

initialization
  RegisterPackage('xmlresource', @Register);
end.
