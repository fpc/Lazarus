{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit allsyneditdsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  SynDesignStringConstants, SynEditLazDsgn, SynPropertyEditObjectList, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SynEditLazDsgn', @SynEditLazDsgn.Register);
end;

initialization
  RegisterPackage('SynEditDsgn', @Register);
end.
