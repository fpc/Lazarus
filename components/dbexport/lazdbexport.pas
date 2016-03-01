{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazdbexport;

{$warn 5023 off : no warning about unused units}
interface

uses
  regdbexport, fpdataexporter, frmSelectExportFormat, frmexportprogress, 
  frmBaseConfigExport, sdb_consts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regdbexport', @regdbexport.Register);
end;

initialization
  RegisterPackage('lazdbexport', @Register);
end.
