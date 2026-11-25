{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazmarkdown;

{$warn 5023 off : no warning about unused units}
interface

uses
  Markdown.CanvasRender, Markdown.Control, Markdown.RegIDE, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Markdown.RegIDE', @Markdown.RegIDE.Register);
end;

initialization
  RegisterPackage('lazmarkdown', @Register);
end.
