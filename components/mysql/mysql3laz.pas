{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package MySQL3Laz 0.1.1.
 }

unit MySQL3Laz; 

interface

uses
  RegisterMySQL, MySQLDB3, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RegisterMySQL', @RegisterMySQL.Register); 
end; 

initialization
  RegisterPackage('MySQL3Laz', @Register); 
end.
