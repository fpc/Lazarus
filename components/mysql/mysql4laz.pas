{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package MySQL4Laz 0.1.1.
 }

unit MySQL4Laz; 

interface

uses
  RegisterMySQL, MySQLDB4, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RegisterMySQL', @RegisterMySQL.Register); 
end; 

initialization
  RegisterPackage('MySQL4Laz', @Register); 
end.
