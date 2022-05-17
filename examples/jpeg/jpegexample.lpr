program JPEGExample;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, MainForm;

begin
  Application.Initialize;
  Application.CreateForm(TJPEGExampleForm, JPEGExampleForm);
  Application.Run;
end.

