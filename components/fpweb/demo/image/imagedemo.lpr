program imagedemo;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, wmimage;

begin
  Application.Title:='cgiproject1';
  Application.Initialize;
  Application.CreateForm(TFPWebModule1, FPWebModule1);
  Application.Run;
end.

