{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazsqldbrest;

{$warn 5023 off : no warning about unused units}
interface

uses
  reglazsqldbrest, frmsqldbrestdispatchini, frmsqldbrestselectconn, reslazsqldbrest, frmsqldbrestselecttables, 
  dlgeditsqldbrestschema, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reglazsqldbrest', @reglazsqldbrest.Register);
end;

initialization
  RegisterPackage('lazsqldbrest', @Register);
end.
