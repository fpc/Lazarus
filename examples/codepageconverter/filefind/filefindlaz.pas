{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit filefindlaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  FileFind, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FileFind', @FileFind.Register);
end;

initialization
  RegisterPackage('filefindlaz', @Register);
end.
