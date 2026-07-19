{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IdeSynEdit;

{$warn 5023 off : no warning about unused units}
interface

uses
  etSrcEditMarks, SourceMarks, SourceSynEditor, IdeSynEditStrings, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IdeSynEdit', @Register);
end.
