{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sparta_Generics;

interface

uses
  Generics.Collections, Generics.Defaults, Generics.Hashes, Generics.Helpers, 
  Generics.MemoryExpanders, Generics.Strings, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('sparta_Generics', @Register);
end.
