{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package mysql4connlaz 0.9.5.
 }

unit mysql4connlaz; 

interface

uses
  mysql4conn, registermysql4conn, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registermysql4conn', @registermysql4conn.Register); 
end; 

initialization
  RegisterPackage('mysql4connlaz', @Register); 
end.
