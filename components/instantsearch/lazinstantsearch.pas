{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazinstantsearch;

{$warn 5023 off : no warning about unused units}
interface

uses
  idemcindexer, reginstantsearch, frmInstantSearch, instantsearchstrings, 
  ideinstantsearch, frainstantsearchoptions, frmSourceTree, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reginstantsearch', @reginstantsearch.Register);
end;

initialization
  RegisterPackage('lazinstantsearch', @Register);
end.
