{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SQLDBLaz; 

interface

uses
  registersqldb, sqldb, SQLStringsPropertyEditorDlg, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('registersqldb', @registersqldb.Register); 
end; 

initialization
  RegisterPackage('SQLDBLaz', @Register); 
end.
