{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package MySQLLaz 0.0.
}

unit MySQLLaz; 

interface

uses
  RegisterMySQL, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RegisterMySQL', @RegisterMySQL.Register); 
end; 

initialization
  RegisterPackage('MySQLLaz', @Register)
end.
