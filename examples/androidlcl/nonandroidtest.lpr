program nonandroidtest;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, secondform, customdrawndrawers, customdrawn_android;

{$R *.res}

begin
  DefaultStyle := dsAndroid;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

