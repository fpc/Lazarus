program htmlwithcss;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, htmlwithcssfrm;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

