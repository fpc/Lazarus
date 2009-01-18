unit SynEditDesignRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SynGutter, SynGutterChanges,SynGutterCodeFolding, SynGutterLineNumber,
  SynGutterMarks, SynPropertyEditObjectList, SynEdit, SynDesignStringConstants,
  LazarusPackageIntf, LResources, PropEdits;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(ClassTypeInfo(TSynGutterPartList), nil,
    '', TSynPropertyEditGutterPartList);

  RegisterGutterPartClass(TSynGutterLineNumber, syndsLineNumbers);
  RegisterGutterPartClass(TSynGutterCodeFolding, syndsCodeFolding);
  RegisterGutterPartClass(TSynGutterChanges, syndsChangeMarker);
  RegisterGutterPartClass(TSynGutterMarks, syndsBookmarks);
  RegisterGutterPartClass(TSynGutterSeparator, syndsSeparator);
end;

end.

