PROGRAM holyday;

{$mode objfpc}{$H+}

USES
  Interfaces,
  Forms, Main, about;

BEGIN
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
END.

