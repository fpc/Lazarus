program ExampleProject1;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, Example1;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

