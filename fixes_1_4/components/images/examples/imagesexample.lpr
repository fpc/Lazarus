program ImagesExample;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, MainForm;

begin
  Application.Title:='Images Example';
  Application.Initialize;
  Application.CreateForm(TImagesExampleForm, ImagesExampleForm);
  Application.Run;
end.

