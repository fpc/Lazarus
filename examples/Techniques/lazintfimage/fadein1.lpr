program FadeIn1;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, MainUnit1;

begin
  Application.Title:='Fade In - Example for TLazIntfImage';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

