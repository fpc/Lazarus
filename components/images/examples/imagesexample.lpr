program ImagesExample;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, MainForm;

begin
  Application.Initialize;
  Application.CreateForm(TImagesExampleForm, ImagesExampleForm);
  Application.Run;
end.

