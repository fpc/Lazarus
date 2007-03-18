program OIExample;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, MainUnit;

begin
  Application.Title:='Example for the Object Inspector';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

