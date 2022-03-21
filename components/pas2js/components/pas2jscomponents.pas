{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pas2jscomponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  regpas2jscomponents, frmHTMLActionsEditor, htmlactions, htmleventnames, 
  strpas2jscomponents, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regpas2jscomponents', @regpas2jscomponents.Register);
end;

initialization
  RegisterPackage('pas2jscomponents', @Register);
end.
