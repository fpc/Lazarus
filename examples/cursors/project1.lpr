program project1;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1;

begin
  Application.Title:='Test cursors';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

