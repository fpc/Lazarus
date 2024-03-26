{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Lazarus.FCL;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazarusPackageIntf, Data.Db, System.Process, System.SimpleIpc, Fcl.EventLog;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('FCL_UnicodeRTL', @Register);
end.
