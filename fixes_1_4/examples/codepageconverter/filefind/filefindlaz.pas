{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package filefindlaz 1.0.2.
 }

unit filefindlaz; 

interface

uses
  FileFind, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('FileFind', @FileFind.Register); 
end; 

initialization
  RegisterPackage('filefindlaz', @Register); 
end.
