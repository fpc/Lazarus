program htmlwithcss;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, htmlwithcssfrm;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

