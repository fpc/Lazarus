{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package SQLiteLaz 1.0.1.
}

unit SQLiteLaz; 

interface

uses
  SQLiteDataset, SQLiteResources, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('SQLiteDataset', @SQLiteDataset.Register); 
end; 

initialization
  RegisterPackage('SQLiteLaz', @Register)
end.
