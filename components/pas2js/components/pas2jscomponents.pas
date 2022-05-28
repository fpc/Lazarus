{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pas2jscomponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  regpas2jscomponents, frmHTMLActionsEditor, htmleventnames, 
  strpas2jscomponents, stub.htmlfragment, stub.restdataset, stub.htmlactions, 
  pas2jsrestutils, pas2jsrestcmd, frmRestData, stub.webwidget, Stub.JS, 
  stub.web, stub.bootstrapwidgets, frmpas2jsedithtml, p2jselementactions, 
  Stub.Data.HTMLActions, frmselecthtmlactions, stub.jsondataset, 
  stub.bootstraptablewidget, stub.dbwebwidget, stub.dbhtmlwidgets, 
  stub.htmlwidgets, stub.bulmawidgets, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regpas2jscomponents', @regpas2jscomponents.Register);
end;

initialization
  RegisterPackage('pas2jscomponents', @Register);
end.
