{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit Sqldblaz; 

interface

uses
  registersqldb, sqldb, ibconnection, pqconnection, mysql4conn, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registersqldb', @registersqldb.Register); 
end; 

initialization
  RegisterPackage('Sqldblaz', @Register); 
end.
