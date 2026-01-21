{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazmarkdown;

{$warn 5023 off : no warning about unused units}
interface

uses
  markdown.canvasrender, markdown.control, markdown.regide, MarkDown.Elements, Markdown.FPDocRender, Markdown.HTMLEntities, 
  Markdown.HtmlRender, MarkDown.InlineText, MarkDown.LatexRender, MarkDown.Line, MarkDown.Parser, Markdown.Processors, 
  MarkDown.Render, MarkDown.Scanner, MarkDown.Utils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('markdown.regide', @markdown.regide.Register);
end;

initialization
  RegisterPackage('lazmarkdown', @Register);
end.
