unit fpDbgSymTable;

{$mode objfpc}{$H+}

interface

uses
  DbgIntfBaseTypes,
  fgl, Classes;

type
  TfpSymbolList= specialize TFPGMap<String, TDBGPtr>;

implementation

end.

